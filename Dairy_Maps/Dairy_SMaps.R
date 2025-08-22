# ============================
# Virginia Dairy Dashboard
# ============================

# ---- Libraries ----
library(shiny)
library(tigris)
library(sf)
library(leaflet)
library(dplyr)
library(ggplot2)
library(readxl)
library(lubridate)
library(scales)
library(rnassqs)
library(plotly)
library(stringr)
library(httr)
library(jsonlite)
library(tidyr)

options(tigris_use_cache = TRUE)

# ---- NASS API ----
nassqs_auth(key = "6644F8BA-CCCE-3CEE-BCE7-5BA5E83CA7E8")

# ---- Transportation data (safe download) ----
temp_file <- tempfile(fileext = ".xlsx")
try({
  download.file(
    "https://raw.githubusercontent.com/yuetong233/Dairy_dspg/main/data/miles_processed.xlsx",
    temp_file, mode = "wb", quiet = TRUE
  )
}, silent = TRUE)
transport_df <- tryCatch(
  readxl::read_excel(temp_file),
  error = function(e) tibble()
)

# ---- VA counties ----
va_counties <- counties(state = "VA", cb = TRUE, class = "sf") %>%
  st_transform(4326)
va_counties$NAME <- toupper(va_counties$NAME)

# Target counties
target_counties <- toupper(c(
  "SHENANDOAH","ROCKINGHAM","AUGUSTA","WARREN",
  "PAGE","FREDERICK","CLARKE","ROCKBRIDGE",
  "PITTSYLVANIA","FRANKLIN"
))
target_va_counties <- va_counties %>% filter(NAME %in% target_counties)

# ---- Milk production data (safe download) ----
temp_milk <- tempfile(fileext = ".xlsx")
try({
  download.file(
    "https://github.com/yuetong233/Dairy_dspg/raw/refs/heads/main/data/VA_Milk_Production.xlsx",
    temp_milk, mode = "wb", quiet = TRUE
  )
}, silent = TRUE)

milk_data <- tryCatch({
  readxl::read_excel(temp_milk) %>%
    rename(`MILK (lbs)` = POUNDS_OF_MILK, PRODUCERS = NUMBER_OF_PRODUCERS) %>%
    mutate(
      COUNTY = toupper(COUNTY),
      DATE = parse_date_time(paste(YEAR, MONTH, "1"), orders = "Y b d"),
      YEAR = as.numeric(YEAR),
      MONTH_NAME = factor(month.name[month(DATE)], levels = month.name, ordered = TRUE),
      MONTH_NUM = month(DATE)
    )
}, error = function(e) tibble(
  COUNTY = character(), YEAR = numeric(), `MILK (lbs)` = numeric(),
  DATE = as.Date(character()), MONTH_NAME = factor(), MONTH_NUM = numeric()
))

# ---- Roads shapefile (robust handling) ----
url_roads <- "https://github.com/yuetong233/Dairy_dspg/raw/main/tl_2023_51_prisecroads_final.zip"
temp_roads <- tempfile(fileext = ".zip")
roads <- NULL
try({
  download.file(url_roads, temp_roads, mode = "wb", quiet = TRUE)
  unzip_dir <- tempdir()
  unzip(temp_roads, exdir = unzip_dir)
  shp_paths <- list.files(unzip_dir, pattern = "\\.shp$", full.names = TRUE, recursive = TRUE)
  if (length(shp_paths) > 0) {
    roads <- st_read(dsn = shp_paths[1], quiet = TRUE)
  }
}, silent = TRUE)

# Filter & accessibility scores (graceful fallback)
primary_secondary_roads <- tryCatch({
  if (!is.null(roads) && nrow(roads) > 0) {
    roads %>% filter(RTTYP %in% c("P","S")) %>% st_transform(4326)
  } else NULL
}, error = function(e) NULL)

if (!is.null(primary_secondary_roads)) {
  roads_with_county <- tryCatch(
    st_join(primary_secondary_roads, target_va_counties["NAME"], left = FALSE),
    error = function(e) NULL
  )
} else {
  roads_with_county <- NULL
}

if (!is.null(roads_with_county) && nrow(roads_with_county) > 0) {
  roads_with_county$length_m <- as.numeric(st_length(roads_with_county))
  road_lengths <- roads_with_county %>%
    group_by(NAME) %>%
    summarise(total_road_length_km = sum(length_m, na.rm = TRUE) / 1000, .groups = "drop")
  target_va_counties$area_km2 <- as.numeric(st_area(target_va_counties) / 1e6)
  
  accessibility_scores <- road_lengths %>%
    left_join(st_drop_geometry(target_va_counties)[, c("NAME","area_km2")], by = "NAME") %>%
    mutate(road_density = total_road_length_km / area_km2) %>%
    arrange(desc(road_density)) %>%
    mutate(score = rescale(road_density, to = c(0, 100)))
  accessibility_scores_df <- accessibility_scores %>%
    st_set_geometry(NULL) %>%
    group_by(NAME) %>%
    summarise(across(everything(), ~ mean(.x, na.rm = TRUE)), .groups = "drop")
} else {
  # Fallback: neutral score if roads missing
  accessibility_scores_df <- tibble(NAME = target_counties, score = 50)
}

# ---- Dairy cow counts from NASS (helper) ----
fetch_dairy_cows <- function(years) {
  milk_cows_all <- lapply(years, function(y) {
    tryCatch({
      df <- nassqs(list(
        sector_desc = "ANIMALS & PRODUCTS",
        group_desc = "LIVESTOCK",
        commodity_desc = "CATTLE",
        class_desc = "COWS, MILK",
        statisticcat_desc = "INVENTORY",
        unit_desc = "HEAD",
        agg_level_desc = "COUNTY",
        state_alpha = "VA",
        source_desc = "SURVEY",
        year = as.character(y)
      ))
      if (!is.null(df) && is.data.frame(df)) df <- df %>% mutate(TYPE = "Milk Cows")
      df
    }, error = function(e) NULL)
  })
  do.call(rbind, Filter(Negate(is.null), milk_cows_all))
}

# ---- Weather / THI data ----
va_counties_thi <- target_va_counties
va_counties_thi$NAME <- str_to_title(target_va_counties$NAME)
va_counties_thi$centroid <- st_centroid(va_counties_thi$geometry)
coords <- st_coordinates(va_counties_thi$centroid)
va_counties_thi$lat <- coords[, 2]
va_counties_thi$lon <- coords[, 1]

full_data_hour <- list()
for (i in seq_len(nrow(va_counties_thi))) {
  lat <- va_counties_thi$lat[i]
  lon <- va_counties_thi$lon[i]
  name <- va_counties_thi$NAME[i]
  res <- tryCatch(
    GET(
      "https://api.open-meteo.com/v1/forecast",
      query = list(
        latitude = lat, longitude = lon,
        hourly = "temperature_2m,relative_humidity_2m",
        timezone = "America/New_York",
        past_days = 2, forecast_days = 3,
        wind_speed_unit = "mph",
        temperature_unit = "fahrenheit",
        precipitation_unit = "inch"
      )
    ),
    error = function(e) NULL
  )
  if (!is.null(res) && status_code(res) == 200) {
    data <- fromJSON(content(res, as = "text"))
    if (!is.null(data$hourly) && all(c("time","temperature_2m","relative_humidity_2m") %in% names(data$hourly))) {
      hourly <- data$hourly
      df <- data.frame(
        time = as.POSIXct(hourly$time, format = "%Y-%m-%dT%H:%M", tz = "America/New_York"),
        temperature_2m = hourly$temperature_2m,
        relative_humidity_2m = hourly$relative_humidity_2m,
        county = name
      )
      full_data_hour[[name]] <- df
    }
  }
}
weather_data_hour <- bind_rows(full_data_hour) %>%
  mutate(thi_mean_hour = temperature_2m - (0.55 - 0.0055 * relative_humidity_2m) * (temperature_2m - 58))

# ============================
# UI
# ============================
ui <- fluidPage(
  tags$head(
    tags$title("Interactive Map Index: Virginia Dairy Processing Industry")),
  # Theme & styles (Maroon & Orange)
  tags$style(HTML("
  :root{
    --vt-maroon:#861F41;
    --vt-orange:#E5751F;
    --map-h:520px;
  }
  body{ color:var(--vt-maroon); }
  a{ color:var(--vt-orange); }
  .app-header { 
    text-align:center; 
    font-weight:700; 
    font-size:28px; 
    color:var(--vt-maroon); 
    margin:10px 0 20px 0;
    position: relative;
  }
  .app-title { font-weight:700; font-size: 28px; margin: 0; color: var(--vt-maroon); }

  /* Dashboard button styling */
  .dashboard-button {
    position: absolute;
    top: 0;
    right: 0;
    z-index: 1000;
  }
  .dashboard-btn {
    background: var(--vt-orange);
    border: 2px solid var(--vt-orange);
    color: white;
    padding: 8px 16px;
    font-size: 14px;
    font-weight: 600;
    text-decoration: none;
    border-radius: 8px;
    transition: all 0.3s ease;
    display: inline-flex;
    align-items: center;
    gap: 6px;
  }
  .dashboard-btn:hover {
    background: var(--vt-maroon);
    border-color: var(--vt-maroon);
    color: white;
    text-decoration: none;
    transform: translateY(-1px);
    box-shadow: 0 4px 8px rgba(134,31,65,0.3);
  }
  .dashboard-btn:focus {
    outline: none;
    box-shadow: 0 0 0 3px rgba(229,117,31,0.4);
  }

  /* Tabset styling */
  .nav-tabs>li>a{ color:var(--vt-maroon) !important; border-radius:10px 10px 0 0; }
  .nav-tabs>li.active>a, .nav-tabs>li.active>a:focus, .nav-tabs>li.active>a:hover{
    color:#fff !important; background:var(--vt-maroon) !important; border-color:var(--vt-maroon) !important;
  }
  .nav-tabs>li>a:hover{ color:#fff !important; background:var(--vt-orange) !important; border-color:var(--vt-orange) !important; }
  .nav-tabs{ border-bottom:none !important; }
  .tab-content{ background:transparent !important; border:none !important; padding:14px 16px 18px 16px; }

  /* Section text */
  .section-title{ font-size:20px; font-weight:600; margin:12px 0 4px 0; color:var(--vt-maroon); }
  .section-desc{ font-size:14px; color:var(--vt-orange); margin:6px 0 14px 0; }

  /* Sidebar info box */
  .info-box{ background:transparent !important; border:none !important; padding:0 !important; }

  /* Leaflet sizing */
  #suitability_map, #thi_plotly{ height: var(--map-h); }
"))
  ,
  
  # Header with title and dashboard button
  div(class="app-header",
      h1("Interactive Map Index: Virginia Dairy Processing Industry", class = "app-title"),
      div(class = "dashboard-button",
          tags$a(
            href = "https://juliabrady.shinyapps.io/dairy_processing_facility_optimization/",
            target = "_blank",
            class = "dashboard-btn",
            tags$i(class = "fa fa-external-link"),
            "View Dashboard"
          )
      )
  ),
  
  tabsetPanel(
    tabPanel(
      "Sustainability Score Map",
      br(),
      fluidRow(
        column(
          width = 12,
          leafletOutput("suitability_map", height = "520px")
        )
      ),
      div(class="section-title",h3("Sustainability Score", style = "color:var(--vt-maroon); font-weight:700; text-align:left; margin-top:25px;")),
      div(
        class="section-desc",
        p("This map shows a", strong("composite sustainability score"), "for selected Virginia counties.",
          "In simple terms, it highlights which counties are better suited for dairy operations based on",
          strong("milk productivity, number of cows per county,"), "and", strong("accessibility index"), 
          "based on proximity to highways, drive times, and fuel prices.",
          style = "text-align:justify; color:#E5751F;"),
        p("Darker red shades indicate higher scores (more suitable), while lighter/yellow shades represent lower scores (less suitable).",
          "By clicking on a county, users can drill down to see details of its underlying score.",
          style = "text-align:justify; color:#E5751F;")
      )
    ),
    
    tabPanel(
      "THI Charts",
      br(),
      sidebarLayout(
        sidebarPanel(
          width = 3,
          div(class="info-box",
              selectInput(
                "selected_county_thi", "Choose a County:",
                choices = sort(unique(weather_data_hour$county)),
                selected = if (nrow(weather_data_hour)>0) sort(unique(weather_data_hour$county))[1] else NULL
              ),
              helpText("THI above ~68 can stress dairy cows and reduce milk yield. Green shading indicates the optimal range (below 68).")
          )
        ),
        mainPanel(
          width = 9,
          plotlyOutput("thi_line_plot", height = "460px"),
          div(class="section-title", h3("THI Charts", style = "color:var(--vt-maroon); font-weight:700; text-align:left; margin-top:25px;")),
          div(
            class="section-desc",
            p("This chart displays the", strong("recent and forecasted Temperature-Humidity Index (THI) values"),
              "for a selected county, showing how heat and humidity may affect dairy cow comfort and milk production.",
              "The", strong("green shaded zone"), "indicates the optimal range for cows (THI below ~68).",
              "When values rise above this threshold, cows experience heat stress, which can reduce milk yield.",
              style = "text-align:justify; color:#E5751F;"
            ),
            p("The THI is calculated", strong("hourly"), "from Open-Meteo temperature and humidity data at the county centroid.",
              "The line colors distinguish", strong("historical observations (blue)"), "from", strong("forecasted values (orange)"),
              ". Users can choose any county from the dropdown menu to monitor both current and upcoming conditions relevant to dairy productivity.",
              style = "text-align:justify; color:#E5751F;"
            )
          )
        )
      )
    )
  )
)

# ============================
# SERVER
# ============================
server <- function(input, output, session) {
  
  # Years to fetch for cow inventory
  years_to_fetch <- reactive({
    current_year <- as.numeric(format(Sys.Date(), "%Y"))
    seq(current_year - 9, current_year)
  })
  
  dairy_cows_data <- reactive({
    fetch_dairy_cows(years_to_fetch())
  })
  
  dairy_cows_va <- reactive({
    df <- dairy_cows_data()
    req(df)
    df %>%
      filter(toupper(county_name) %in% target_counties) %>%
      mutate(
        COUNTY = toupper(county_name),
        YEAR = as.numeric(year),
        COWS = suppressWarnings(as.numeric(Value))
      ) %>%
      group_by(COUNTY, YEAR) %>%
      summarise(COWS = sum(COWS, na.rm = TRUE), .groups = "drop") %>%
      filter(!is.na(COWS))
  })
  
  composite_data <- reactive({
    # Efficiency (use latest full year available; fall back to max)
    eff_year <- if (nrow(milk_data) > 0) max(milk_data$YEAR, na.rm = TRUE) else NA
    efficiency_df <- if (!is.na(eff_year)) {
      milk_data %>%
        filter(YEAR == eff_year, COUNTY %in% target_counties) %>%
        left_join(dairy_cows_va() %>% filter(YEAR == eff_year),
                  by = c("COUNTY","YEAR")) %>%
        filter(!is.na(COWS), COWS > 0, !is.na(`MILK (lbs)`)) %>%
        mutate(milk_efficiency = `MILK (lbs)` / (COWS * 30)) %>%
        group_by(COUNTY) %>%
        summarise(
          milk_efficiency = mean(milk_efficiency, na.rm = TRUE),
          total_milk = sum(`MILK (lbs)`, na.rm = TRUE),
          total_cows = mean(COWS, na.rm = TRUE),
          .groups = "drop"
        )
    } else {
      tibble(COUNTY = character(), milk_efficiency = numeric(),
             total_milk = numeric(), total_cows = numeric())
    }
    
    county_names_df <- st_drop_geometry(target_va_counties)[, c("NAME"), drop = FALSE]
    
    out <- county_names_df %>%
      left_join(accessibility_scores_df %>% select(NAME, score), by = "NAME") %>%
      left_join(efficiency_df, by = c("NAME" = "COUNTY")) %>%
      mutate(
        efficiency_score = rescale(milk_efficiency, to = c(0, 100), na.rm = TRUE),
        inventory_score  = rescale(total_cows, to = c(0, 100), na.rm = TRUE),
        score = replace_na(score, 0),
        composite_index = round(0.4 * efficiency_score + 0.3 * inventory_score + 0.3 * score, 1)
      )
    out
  })
  
  merged_suitability <- reactive({
    target_va_counties %>% left_join(composite_data(), by = "NAME")
  })
  
  # ---- Map ----
  output$suitability_map <- renderLeaflet({
    data <- merged_suitability()
    pal_suit <- colorNumeric(palette = "YlOrRd",
                             domain = data$composite_index,
                             na.color = "transparent")
    
    leaflet(data) %>%
      addProviderTiles("CartoDB.Positron") %>%
      setView(lng = -79.0, lat = 37.5, zoom = 7) %>%
      addPolygons(
        fillColor = ~ifelse(is.na(composite_index), "#bbbbbb", pal_suit(composite_index)),
        color = "#888888", weight = 1, fillOpacity = 0.7,
        label = ~lapply(paste0(
          "<strong>", NAME, "</strong><br>",
          "Composite Score: <b>", ifelse(is.na(composite_index),"NA", composite_index), "</b><br>",
          "Efficiency: ", ifelse(is.na(milk_efficiency),"NA", round(milk_efficiency, 1)), " lbs/cow/month<br>",
          "Inventory: ", ifelse(is.na(total_cows),"NA", formatC(total_cows, format = "d", big.mark = ",")), " cows<br>",
          "Accessibility Score: ", ifelse(is.na(score),"NA", round(score, 1))
        ), htmltools::HTML)
      ) %>%
      addLegend(
        pal = pal_suit, values = data$composite_index,
        title = "Composite<br>Score", position = "topleft"
      )
  })
  
  # ---- THI Plot ----
  output$thi_line_plot <- renderPlotly({
    req(nrow(weather_data_hour) > 0)
    req(input$selected_county_thi)
    
    thi_data <- weather_data_hour %>% filter(county == input$selected_county_thi)
    req(nrow(thi_data) > 0)
    
    thi_data$color <- ifelse(thi_data$time > Sys.time(), "Forecast", "Historical")
    thi_data <- thi_data[!is.na(thi_data$color), ]
    
    # dummy for legend
    dummy_df <- data.frame(x = c(Sys.time(), Sys.time()), y = c(68, 68))
    
    p <- ggplot(thi_data, aes(x = time)) +
      annotate("rect",
               xmin = min(thi_data$time), xmax = max(thi_data$time),
               ymin = min(thi_data$thi_mean_hour, na.rm = TRUE), ymax = 68,
               fill = "#00a000", alpha = 0.18) +
      annotate("segment",
               x = min(thi_data$time), xend = max(thi_data$time),
               y = 68, yend = 68, color = "#00a000", alpha = 0.5, linewidth = 0.5) +
      geom_line(aes(y = thi_mean_hour, color = color)) +
      geom_point(aes(y = thi_mean_hour, color = color,
                     text = paste0(format(time, "%b %d, %Y %I:%M %p"),
                                   "<br>THI: ", round(thi_mean_hour, 1))), size = 0.8) +
      geom_line(data = dummy_df, aes(x = x, y = y, color = "Optimal Range"), size = 0.5, alpha = 0.7) +
      scale_x_datetime(date_labels = "%m/%d", date_breaks = "1 day") +
      labs(title = paste("Recent THI for", input$selected_county_thi),
           x = "Time", y = "THI", color = "") +
      scale_color_manual(values = c(
        "Forecast" = "#E5751F",
        "Historical" = "steelblue2",
        "Optimal Range" = "#00a000"
      ), na.translate = FALSE) +
      theme_minimal()
    
    ggplotly(p, tooltip = "text") %>%
      layout(margin = list(t = 60))
  })
}

# ---- Run App ----
shinyApp(ui, server)