# ========================================================================
# Virginia Childcare Cost & Female Labor Force Participation â€" TABBED UI
# ========================================================================

rm(list = ls()); cat("\014")

library(shiny)
library(tidyverse)
library(readxl)
library(tigris)
library(leaflet)
library(sf)
library(plotly)
library(viridisLite)
library(scales)

options(tigris_use_cache = TRUE)

# ---------------- Data ----------------
# --- Paths: read from the app directory (shinyapps.io working dir is the app root)
data_file <- file.path("Childcare_Data", "Cleaned_Virginia_Womens_Bureau.xlsx")

# (Optional but safer) specify column types so you don't need as.numeric() later
# Adjust names/order to match your sheet exactly
RUdata <- read_excel(
  path = data_file,
  na = c("NA", "n/a", "", "-"),
  # If you know column types, uncomment and tailor:
  # col_types = c(
  #   COUNTY_FIPS_CODE = "text",
  #   MCINFANT = "numeric",
  #   MCTODDLER = "numeric",
  #   MCPRESCHOOL = "numeric",
  #   FLFPR_20to64_UNDER6 = "numeric",
  #   Urban = "numeric",
  #   .default = "guess"
  # )
) %>%
  mutate(
    FIPS = str_pad(as.character(COUNTY_FIPS_CODE), 5, pad = "0"),
    # Remove these if you set col_types above
    MCINFANT = as.numeric(MCINFANT),
    MCTODDLER = as.numeric(MCTODDLER),
    MCPRESCHOOL = as.numeric(MCPRESCHOOL),
    FLFPR_20to64_UNDER6 = as.numeric(FLFPR_20to64_UNDER6),
    Urban = as.numeric(Urban)
  )

# --- Maps: download VA counties as sf (tigris fetches from Census servers)
options(tigris_use_cache = TRUE)  # cache on server between runs when possible
va_map <- counties(state = "VA", cb = TRUE, class = "sf") %>%
  mutate(FIPS = str_pad(as.character(GEOID), 5, pad = "0"))

# ---------------- UI ----------------
ui <- fluidPage(
  tags$head(tags$title("Virginia Childcare Cost & Female Labor Force Interactive Maps")),
  tags$head(
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
  }
  .app-title { font-weight:700; font-size: 28px; margin: 0; color: var(--vt-maroon); }

  /* Dashboard button hover styling */
  .dashboard-btn:hover {
    background: var(--vt-maroon) !important;
    border-color: var(--vt-maroon) !important;
    color: white !important;
    text-decoration: none;
    transform: translateY(-1px);
    box-shadow: 0 4px 8px rgba(134,31,65,0.3);
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

  /* Sidebar grey layer */
  .well, .sidebarPanel, .panel.sidebarPanel {
    background-color: #f0f0f0 !important;
    border-radius: 10px;
    padding: 12px;
    box-shadow: inset 0 0 4px rgba(0,0,0,0.08);
  }

  /* Map sizing */
  #childcare_map, #flfpr_map, #childcare_trend_plot, #flfpr_trend_plot{ height: var(--map-h); }
  
  /* Select input styling */
  select.form-control, .selectize-input {
    border: 1px solid var(--vt-orange) !important;
    box-shadow: none !important;
  }
  select.form-control:focus, .selectize-input.focus {
    border-color: var(--vt-orange) !important;
    outline: none !important;
    box-shadow: 0 0 4px var(--vt-orange) !important;
  }
  .selectize-dropdown .active {
    background-color: var(--vt-orange) !important;
    color: #fff !important;
  }

  /* Toggle title styling */
  .toggle-title { text-align: left; font-weight: 600; color: var(--vt-maroon); margin: 6px 0 8px 0; }

"))
    
  ),
  
  # Header with title only
  div(class="app-header",
      h1("Virginia Childcare Cost & Female Labor Force Interactive Maps", class = "app-title")
  ),
  
  tabsetPanel(
    # Childcare Cost Tab
    tabPanel("Childcare Cost",
             sidebarLayout(
               sidebarPanel(
                 width = 3,
                 selectInput("childcare_view_mode", "Choose View Mode:",
                             choices = c("Single Year View" = "year", "Trend Over Time" = "trend"),
                             selected = "year"),
                 
                 conditionalPanel(
                   condition = "input.childcare_view_mode == 'year'",
                   selectInput("childcare_year", "Select Year:",
                               choices = sort(unique(RUdata$STUDYYEAR[RUdata$STUDYYEAR != 2008])))
                 ),
                 
                 conditionalPanel(
                   condition = "input.childcare_view_mode == 'year'",
                   selectInput("cost_type", "Select Childcare Type:",
                               choices = c("Infant Care" = "MCINFANT",
                                           "Toddler Care" = "MCTODDLER",
                                           "Preschool Care" = "MCPRESCHOOL"),
                               selected = "MCINFANT")
                 ),
                 
                 conditionalPanel(
                   condition = "input.childcare_view_mode == 'trend'",
                   checkboxGroupInput("trend_cost_types", "Select Childcare Types:",
                                      choices = c("Infant Care" = "MCINFANT",
                                                  "Toddler Care" = "MCTODDLER",
                                                  "Preschool Care" = "MCPRESCHOOL"),
                                      selected = c("MCINFANT", "MCTODDLER", "MCPRESCHOOL"))
                 ),
                 
                 radioButtons("childcare_urban_filter", "Show Counties:",
                              choices = c("All" = "all", "Urban Only" = "urban", "Rural Only" = "rural"),
                              selected = "all"),
                 
                 conditionalPanel(
                   condition = "input.childcare_view_mode == 'trend'",
                   selectizeInput("childcare_county_picker", "Select Counties for Trend:", choices = NULL, multiple = TRUE)
                 ),
                 
                 conditionalPanel(
                   condition = "input.childcare_view_mode == 'trend'",
                   actionButton("clear_childcare_county", "Clear County Selection", class = "btn btn-warning")
                 ),
                 
                 # Data source information
                 hr(style = "margin: 30px 0 15px 0; border-color: #ccc;"),
                 div(class = "toggle-title", "Data Source:"),
                 p("These interactive maps are developed by the DSPG Childcare program.", 
                   style = "font-size: 13px; color: #555; margin-bottom: 10px;"),
                 p("For additional analysis and insights, please visit the original dashboard:",
                   style = "font-size: 13px; color: #555; margin-bottom: 12px;"),
                 tags$a(
                   href = "https://yuetong233.shinyapps.io/childcare_costs_virginia/",
                   target = "_blank",
                   class = "dashboard-btn",
                   style = "background: var(--vt-orange); border: 2px solid var(--vt-orange); color: white; padding: 8px 16px; font-size: 12px; font-weight: 600; text-decoration: none; border-radius: 6px; display: inline-flex; align-items: center; gap: 6px; transition: all 0.3s ease;",
                   tags$i(class = "fa fa-external-link", style = "font-size: 11px;"),
                   "View Dashboard"
                 )
               ),
               
               mainPanel(
                 leafletOutput("childcare_map", height = "520px"),
                 br(),
                 h3("Childcare Cost", 
                    style = "color:var(--vt-maroon); font-weight:700; text-align:left; margin-top:25px;"),
                 p("This map shows the weekly median price of full-time childcare across Virginia counties.",
                   "The map highlights regional disparities, where urban areas tend to face higher childcare costs compared to rural areas.",
                   style = "text-align:justify; color:#E5751F;"),
                 p("Users can filter by year, childcare type, and whether to view urban, rural, or all counties.", 
                   "Lighter colors indicate higher childcare costs, with the legend on the right showing price ranges.",
                   style = "text-align:justify; color:#E5751F;"),
                 p(strong("In 'Trend Over Time' view mode, select counties to compare trends."),
                   style = "text-align:justify; color:#E5751F;"),
                 br(),
                 conditionalPanel(
                   condition = "input.childcare_view_mode == 'trend' && output.show_childcare_trend",
                   h4("Childcare Cost Trend for Selected Counties",
                      style = "text-align:center; font-weight:bold;"),
                   plotlyOutput("childcare_trend_plot", height = "400px"),
                   div(
                     style = "text-align: justify; margin-top:15px;",
                     p("This visualization shows the ", strong("weekly full-time median childcare costs over time"),
                       "for the counties selected by the user. Costs are displayed separately for", strong("Infant Care, Preschool, and Toddler Care."),
                       "Each line represents a county, allowing users to compare how childcare prices have changed across different regions from 2010 to 2022.",
                       style = "text-align:justify; color:#E5751F;"),
                     p("The chart highlights both the overall upward trend in childcare costs and regional disparities, ",
                       "with urban areas (e.g., Alexandria City) generally experiencing significantly higher prices than rural areas (e.g., Amelia or Appomattox County).",
                       style = "text-align:justify; color:#E5751F;")
                   )
                 ),
                 
                 conditionalPanel(
                   condition = "input.childcare_view_mode == 'trend' && !output.show_childcare_trend",
                   div(
                     style = "text-align: center; margin-top: 50px; padding: 20px; background-color: #f8f9fa; border-radius: 8px; border: 1px solid #dee2e6;",
                     h4("No Counties Selected", style = "color: var(--vt-maroon); margin-bottom: 15px;"),
                     p("Please select counties from the dropdown menu or click on the map to view childcare cost trends.",
                       style = "color: #6c757d; font-size: 14px;")
                   )
                 )
               )
             )
    ),
    
    # Female Labor Force Participation Tab
    tabPanel("Female Labor Force Participation",
             sidebarLayout(
               sidebarPanel(
                 width = 3,
                 selectInput("flfpr_view_mode", "Choose View Mode:",
                             choices = c("Single Year View" = "year", "Trend Over Time" = "trend"),
                             selected = "year"),
                 
                 conditionalPanel(
                   condition = "input.flfpr_view_mode == 'year'",
                   selectInput("flfpr_year", "Select Year:",
                               choices = sort(unique(RUdata$STUDYYEAR[RUdata$STUDYYEAR != 2008])))
                 ),
                 
                 radioButtons("flfpr_urban_filter", "Show Counties:",
                              choices = c("All" = "all", "Urban Only" = "urban", "Rural Only" = "rural"),
                              selected = "all"),
                 
                 conditionalPanel(
                   condition = "input.flfpr_view_mode == 'trend'",
                   selectizeInput("flfpr_county_picker", "Select Counties for Trend:", choices = NULL, multiple = TRUE)
                 ),
                 
                 conditionalPanel(
                   condition = "input.flfpr_view_mode == 'trend'",
                   actionButton("clear_flfpr_county", "Clear County Selection", class = "btn btn-warning")
                 ),
                 
                 # Data source information
                 hr(style = "margin: 30px 0 15px 0; border-color: #ccc;"),
                 div(class = "toggle-title", "Data Source:"),
                 p("These interactive maps are developed by the DSPG Childcare program.", 
                   style = "font-size: 13px; color: #555; margin-bottom: 10px;"),
                 p("For additional analysis and insights, please visit the original dashboard:",
                   style = "font-size: 13px; color: #555; margin-bottom: 12px;"),
                 tags$a(
                   href = "https://yuetong233.shinyapps.io/childcare_costs_virginia/",
                   target = "_blank",
                   class = "dashboard-btn",
                   style = "background: var(--vt-orange); border: 2px solid var(--vt-orange); color: white; padding: 8px 16px; font-size: 12px; font-weight: 600; text-decoration: none; border-radius: 6px; display: inline-flex; align-items: center; gap: 6px; transition: all 0.3s ease;",
                   tags$i(class = "fa fa-external-link", style = "font-size: 11px;"),
                   "View Dashboard"
                 )
               ),
               
               mainPanel(
                 leafletOutput("flfpr_map", height = "520px"),
                 br(),
                 h3("Female Labor Force Participation", 
                    style = "color:var(--vt-maroon); font-weight:700; text-align:left; margin-top:25px;"),
                 p("This map presents the", strong("Female Labor Force Participation Rate (FLFPR)"), 
                   "for mothers with children under six years old across Virginia counties. ",
                   "The map allows users to explore how women's workforce participation varies geographically and may relate to childcare costs.",
                   style = "text-align:justify; color:#E5751F;"),
                 p("Users can select the year and filter by urban, rural, or all counties.", 
                   "Lighter green shades represent higher participation rates, while purple areas represent lower rates.",
                   style = "text-align:justify; color:#E5751F;"),
                 p(strong("In 'Trend Over Time' view mode, select counties to compare trends."),
                   style = "text-align:justify; color:#E5751F;"),
                 br(),
                 conditionalPanel(
                   condition = "input.flfpr_view_mode == 'trend' && output.show_flfpr_trend",
                   h4("FLFPR Trend for Selected Counties",
                      style = "text-align:center; font-weight:bold;"),
                   plotlyOutput("flfpr_trend_plot", height = "400px"),
                   div(
                     style = "text-align: justify; margin-top:15px;",
                     p("This visualization shows the female labor force participation rate (FLFPR) trends over time for the selected counties. ",
                       "Each line represents a county, allowing users to compare how FLFPR with children under six years old have changed across different regions from 2009 to 2022.",
                       style = "text-align:justify; color:#E5751F;"),
                     p("Use the controls on the left to select different counties or adjust the view mode. ",
                       "Trends are based on ACS data and are expressed as percentages of the working-age population.",
                       style = "text-align:justify; color:#E5751F;")
                   )
                 ),
                 
                 conditionalPanel(
                   condition = "input.flfpr_view_mode == 'trend' && !output.show_flfpr_trend",
                   div(
                     style = "text-align: center; margin-top: 50px; padding: 20px; background-color: #f8f9fa; border-radius: 8px; border: 1px solid #dee2e6;",
                     h4("No Counties Selected", style = "color: var(--vt-maroon); margin-bottom: 15px;"),
                     p("Please select counties from the dropdown menu or click on the map to view FLFPR trends.",
                       style = "color: #6c757d; font-size: 14px;")
                   )
                 )
               )
             )
    )
  )
)

# ---------------- Server ----------------
server <- function(input, output, session) {
  # Reactive values for selected counties in each tab
  selected_childcare_counties <- reactiveVal(character(0))
  selected_flfpr_counties <- reactiveVal(character(0))
  
  # Reactive value to control trend plot visibility
  output$show_childcare_trend <- reactive({
    length(selected_childcare_counties()) > 0
  })
  outputOptions(output, "show_childcare_trend", suspendWhenHidden = FALSE)
  
  output$show_flfpr_trend <- reactive({
    length(selected_flfpr_counties()) > 0
  })
  outputOptions(output, "show_flfpr_trend", suspendWhenHidden = FALSE)
  
  # Update county choices for both tabs
  observe({
    county_names <- sort(unique(RUdata$COUNTY_NAME))
    updateSelectizeInput(session, "childcare_county_picker", choices = county_names)
    updateSelectizeInput(session, "flfpr_county_picker", choices = county_names)
  })
  
  # Handle county selection for childcare tab
  observeEvent(input$childcare_county_picker, {
    selected_childcare_counties(input$childcare_county_picker)
  })
  
  observeEvent(input$childcare_map_shape_click, {
    if (input$childcare_view_mode == "trend") {
      clicked_fips <- input$childcare_map_shape_click$id
      clicked_name <- RUdata %>% filter(FIPS == clicked_fips) %>% pull(COUNTY_NAME) %>% unique()
      if (length(clicked_name) == 1) {
        updated <- union(selected_childcare_counties(), clicked_name)
        selected_childcare_counties(updated)
        updateSelectizeInput(session, "childcare_county_picker", selected = updated)
      }
    }
  })
  
  observeEvent(input$clear_childcare_county, {
    selected_childcare_counties(character(0))
    updateSelectizeInput(session, "childcare_county_picker", selected = character(0))
  })
  
  # Handle county selection for FLFPR tab
  observeEvent(input$flfpr_county_picker, {
    selected_flfpr_counties(input$flfpr_county_picker)
  })
  
  observeEvent(input$flfpr_map_shape_click, {
    if (input$flfpr_view_mode == "trend") {
      clicked_fips <- input$flfpr_map_shape_click$id
      clicked_name <- RUdata %>% filter(FIPS == clicked_fips) %>% pull(COUNTY_NAME) %>% unique()
      if (length(clicked_name) == 1) {
        updated <- union(selected_flfpr_counties(), clicked_name)
        selected_flfpr_counties(updated)
        updateSelectizeInput(session, "flfpr_county_picker", selected = updated)
      }
    }
  })
  
  observeEvent(input$clear_flfpr_county, {
    selected_flfpr_counties(character(0))
    updateSelectizeInput(session, "flfpr_county_picker", selected = character(0))
  })
  
  # Filtered data for childcare tab
  filtered_childcare_data <- reactive({
    if (input$childcare_view_mode == "year") {
      req(input$childcare_year)
      RUdata %>% filter(STUDYYEAR == input$childcare_year)
    } else {
      RUdata %>%
        group_by(FIPS) %>%
        summarise(across(c(MCINFANT, MCTODDLER, MCPRESCHOOL, FLFPR_20to64_UNDER6, Urban),
                         mean, na.rm = TRUE),
                  .groups = "drop")
    }
  })
  
  # Filtered data for FLFPR tab
  filtered_flfpr_data <- reactive({
    if (input$flfpr_view_mode == "year") {
      req(input$flfpr_year)
      RUdata %>% filter(STUDYYEAR == input$flfpr_year)
    } else {
      RUdata %>%
        group_by(FIPS) %>%
        summarise(across(c(MCINFANT, MCTODDLER, MCPRESCHOOL, FLFPR_20to64_UNDER6, Urban),
                         mean, na.rm = TRUE),
                  .groups = "drop")
    }
  })
  
  # Childcare map
  output$childcare_map <- renderLeaflet({
    map_data <- left_join(va_map, filtered_childcare_data(), by = "FIPS")
    
    var <- if (input$childcare_view_mode == "year") input$cost_type else input$trend_cost_types[1]
    title <- paste(
      ifelse(input$childcare_view_mode == "trend", "Avg. Weekly Median Price", "Weekly Median Price"),
      "-", gsub("MC", "", var), "(USD)"
    )
    palette_values <- map_data[[var]]
    label_value <- paste0("Cost: $", ifelse(is.na(palette_values), "No data", round(palette_values, 2)))
    pal <- colorNumeric(palette = plasma(100), domain = palette_values, na.color = "#f0f0f0")
    
    # Urban filter styling
    map_data$fill_color <- ifelse(
      input$childcare_urban_filter == "all" |
        (input$childcare_urban_filter == "urban" & map_data$Urban == 1) |
        (input$childcare_urban_filter == "rural" & map_data$Urban == 0),
      pal(palette_values),
      "#d3d3d3"
    )
    map_data$fill_opacity <- ifelse(
      input$childcare_urban_filter == "all" |
        (input$childcare_urban_filter == "urban" & map_data$Urban == 1) |
        (input$childcare_urban_filter == "rural" & map_data$Urban == 0),
      0.7, 0.3
    )
    
    label_text <- paste0(
      map_data$NAME, "<br>", label_value,
      "<br>Type: ", ifelse(map_data$Urban == 1, "Urban", "Rural")
    )
    
    leaflet(map_data) %>%
      addProviderTiles("CartoDB.Positron") %>%
      setView(lng = -79.0, lat = 37.5, zoom = 7) %>%
      addPolygons(
        fillColor = ~fill_color,
        fillOpacity = ~fill_opacity,
        weight = 1,
        color = "white",
        label = lapply(label_text, HTML),
        highlightOptions = highlightOptions(color = "black", weight = 2, bringToFront = TRUE),
        layerId = ~FIPS
      ) %>%
      addLegend(
        position = "topright",
        pal = pal,
        values = palette_values,
        title = HTML(title),
        labFormat = labelFormat(prefix = "$"),
        opacity = 1
      )
  })
  
  # FLFPR map
  output$flfpr_map <- renderLeaflet({
    map_data <- left_join(va_map, filtered_flfpr_data(), by = "FIPS")
    
    var <- "FLFPR_20to64_UNDER6"
    title <- ifelse(input$flfpr_view_mode == "trend",
                    "Avg. FLFPR of Mothers (Under 6)",
                    "FLFPR of Mothers (Under 6)")
    palette_values <- map_data[[var]]
    label_value <- paste0("FLFPR: ", ifelse(is.na(palette_values), "No data", round(palette_values, 1)), "%")
    pal <- colorNumeric(palette = viridis(100), domain = palette_values, na.color = "#f0f0f0")
    
    # Urban filter styling
    map_data$fill_color <- ifelse(
      input$flfpr_urban_filter == "all" |
        (input$flfpr_urban_filter == "urban" & map_data$Urban == 1) |
        (input$flfpr_urban_filter == "rural" & map_data$Urban == 0),
      pal(palette_values),
      "#d3d3d3"
    )
    map_data$fill_opacity <- ifelse(
      input$flfpr_urban_filter == "all" |
        (input$flfpr_urban_filter == "urban" & map_data$Urban == 1) |
        (input$flfpr_urban_filter == "rural" & map_data$Urban == 0),
      0.7, 0.3
    )
    
    label_text <- paste0(
      map_data$NAME, "<br>", label_value,
      "<br>Type: ", ifelse(map_data$Urban == 1, "Urban", "Rural")
    )
    
    leaflet(map_data) %>%
      addProviderTiles("CartoDB.Positron") %>%
      setView(lng = -79.0, lat = 37.5, zoom = 7) %>%
      addPolygons(
        fillColor = ~fill_color,
        fillOpacity = ~fill_opacity,
        weight = 1,
        color = "white",
        label = lapply(label_text, HTML),
        highlightOptions = highlightOptions(color = "black", weight = 2, bringToFront = TRUE),
        layerId = ~FIPS
      ) %>%
      addLegend(
        position = "topright",
        pal = pal,
        values = palette_values,
        title = HTML(title),
        labFormat = labelFormat(suffix = "%"),
        opacity = 1
      )
  })
  
  # Childcare trend plot
  output$childcare_trend_plot <- renderPlotly({
    locs <- selected_childcare_counties()
    req(length(locs) > 0)
    county_data <- RUdata %>% filter(COUNTY_NAME %in% locs)
    
    num_colors <- length(locs)
    palette_fn <- function(n) viridis(n)
    county_colors <- palette_fn(num_colors)
    names(county_colors) <- locs
    
    req(length(input$trend_cost_types) > 0)
    
    cost_data <- county_data %>%
      filter(!STUDYYEAR %in% c(2008, 2021)) %>%
      select(c("COUNTY_NAME", "STUDYYEAR", input$trend_cost_types)) %>%
      pivot_longer(cols = all_of(input$trend_cost_types), names_to = "Type", values_to = "Cost") %>%
      mutate(Type = recode(Type,
                           MCINFANT = "Infant",
                           MCTODDLER = "Toddler",
                           MCPRESCHOOL = "Preschool")) %>%
      drop_na()
    
    gg <- ggplot(cost_data, aes(x = STUDYYEAR, y = Cost, color = COUNTY_NAME, group = COUNTY_NAME)) +
      geom_line(size = 1.1) +
      geom_point(size = 2) +
      scale_color_manual(values = county_colors) +
      labs(
        title = "Weekly Full-Time Median Childcare Cost Trends",
        subtitle = "Aggregated across providers in each county",
        x = "Year", y = "Cost (USD)", color = "County"
      ) +
      theme_minimal(base_family = "Times New Roman")
    
    if (length(unique(cost_data$Type)) > 1) {
      gg <- gg + facet_wrap(~ Type, scales = "free_y")
    } else {
      gg <- gg + ggtitle(paste0("Childcare Cost Trend - ", unique(cost_data$Type)))
    }
    
    ggplotly(gg, tooltip = c("x", "y", "color"))
  })
  
  # FLFPR trend plot
  output$flfpr_trend_plot <- renderPlotly({
    locs <- selected_flfpr_counties()
    req(length(locs) > 0)
    county_data <- RUdata %>% filter(COUNTY_NAME %in% locs)
    
    num_colors <- length(locs)
    palette_fn <- function(n) viridis(n)
    county_colors <- palette_fn(num_colors)
    names(county_colors) <- locs
    
    flfpr_data <- county_data %>%
      filter(STUDYYEAR != 2008) %>%
      select(COUNTY_NAME, STUDYYEAR, FLFPR_20to64_UNDER6) %>%
      drop_na()
    
    gg <- ggplot(flfpr_data, aes(x = STUDYYEAR, y = FLFPR_20to64_UNDER6, color = COUNTY_NAME)) +
      geom_line(size = 1.2) +
      geom_point(size = 2) +
      scale_color_manual(values = county_colors) +
      labs(
        title = "FLFPR (Under 6) Trends",
        x = "Year", y = "Participation Rate (%)", color = "County"
      ) +
      theme_minimal(base_family = "Times New Roman")
    
    ggplotly(gg, tooltip = c("x", "y", "color"))
  })
}

# ---------------- Run ----------------
shinyApp(ui = ui, server = server)