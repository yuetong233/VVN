# ===================================================================
# Interactive Solar Facilities Map - Updated Version
# ===================================================================

# 1. LOAD LIBRARIES
library(shiny)
library(leaflet) 
library(sf)
library(tigris)
library(tidyverse)
library(dplyr)
library(shinyWidgets)  # for radioGroupButtons

# 2. GLOBAL CONSTANTS
TARGET_CRS <- 4326

# Base data path 
data_path <- "Solar_Data"

# 3. LOAD RAW DATA

# Load parcel data
tryCatch({
  final_data <- readRDS(file.path(data_path, "final_parcel_data.rds"))
}, error = function(e) {
  final_data <- NULL
})

# Load transmission lines - SIMPLIFIED VERSION
tryCatch({
  transmission_file <- file.path(data_path, "Transmission_Lines/Transmission_Lines.shp")
  
  if(file.exists(transmission_file)) {
    # Read the shapefile
    transmission_lines_raw <- st_read(transmission_file, quiet = TRUE)
    
    # Transform to WGS84
    transmission_lines_raw <- st_transform(transmission_lines_raw, crs = TARGET_CRS)
    
  } else {
    transmission_lines_raw <- NULL
  }
}, error = function(e) {
  transmission_lines_raw <- NULL
})

# Load solar facility data
tryCatch({
  solar_data_raw <- read_csv(file.path(data_path, "solar_facility.csv"), show_col_types = FALSE)
}, error = function(e) {
  solar_data_raw <- NULL
})

# Load VA counties
tryCatch({
  suppressMessages({
    options(tigris_use_cache = TRUE)
    va_counties_raw <- counties(state = "VA", cb = TRUE)
  })
}, error = function(e) {
  va_counties_raw <- NULL
})

# Load county level data
tryCatch({
  county_level_data_raw <- read_csv(file.path(data_path, "county_level_merged_data.csv"), show_col_types = FALSE)
}, error = function(e) {
  county_level_data_raw <- NULL
})

# Load water features
tryCatch({
  water_features_raw <- read_csv(file.path(data_path, "virginia_water_features.csv"), show_col_types = FALSE)
}, error = function(e) {
  water_features_raw <- NULL
})

# Load VA cities
tryCatch({
  cities_file <- file.path(data_path, "VA_Cities/VA_Cities.shp")
  
  if(file.exists(cities_file)) {
    VA_cities_raw <- st_read(cities_file, quiet = TRUE)
    
    # Transform to WGS84
    VA_cities_raw <- st_transform(VA_cities_raw, crs = TARGET_CRS)
  } else {
    VA_cities_raw <- NULL
  }
}, error = function(e) {
  VA_cities_raw <- NULL
})

# 4. PROCESS DATA

# Process solar facility map data
if(!is.null(solar_data_raw)) {
  map_data <- solar_data_raw %>%
    select(
      Facility_Name = p_name,
      County = County,
      Latitude = ylat,
      Longitude = xlong,
      Capacity_MW = p_cap_ac,
      Commission_Year = Year
    ) %>%
    drop_na(Latitude, Longitude)
} else {
  map_data <- data.frame(
    Facility_Name = c("Solar Farm A", "Solar Farm B", "Solar Farm C"),
    County = c("Albemarle", "Augusta", "Bedford"),
    Latitude = c(37.8, 38.1, 37.3),
    Longitude = c(-78.5, -79.2, -79.8),
    Capacity_MW = c(50, 75, 100),
    Commission_Year = c(2018, 2019, 2020)
  )
}

# Process county shapefile
if(!is.null(va_counties_raw)) {
  va_counties <- va_counties_raw %>%
    st_transform(crs = TARGET_CRS) %>%
    mutate(county_clean = str_to_title(NAME))
  
  va_state_border <- st_union(va_counties)
} else {
  va_counties <- NULL
  va_state_border <- NULL
}

# Process county level data
if(!is.null(county_level_data_raw)) {
  county_level_data <- county_level_data_raw %>%
    mutate(
      across(c(Price_Per_Acre, CornYield, SoyYield, HousingAge, Population, VacantUnits), as.numeric),
      county_clean = str_to_title(gsub(" County", "", County))
    )
  
  county_year_summary <- county_level_data_raw %>%
    group_by(County, Year) %>%
    summarise(
      Price_Per_Acre = mean(Price_Per_Acre, na.rm = TRUE),
      CornYield = mean(CornYield, na.rm = TRUE),
      SoyYield = mean(SoyYield, na.rm = TRUE),
      HousingAge = mean(HousingAge, na.rm = TRUE),
      Population = mean(Population, na.rm = TRUE),
      TotalHousingUnits = mean(TotalHousingUnits, na.rm = TRUE),
      OccupiedUnits = mean(OccupiedUnits, na.rm = TRUE),
      VacantUnits = mean(VacantUnits, na.rm = TRUE),
      Treated = max(Treated, na.rm = TRUE),
      Post = max(Post, na.rm = TRUE),
      .groups = "drop"
    )
} else {
  county_level_data <- NULL
  county_year_summary <- data.frame(
    County = rep(c("Albemarle", "Augusta", "Bedford"), each = 3),
    Year = rep(c(2018, 2019, 2020), 3),
    Price_Per_Acre = runif(9, 3000, 8000),
    CornYield = runif(9, 120, 180),
    SoyYield = runif(9, 40, 60),
    HousingAge = runif(9, 15, 45),
    Population = runif(9, 50000, 200000),
    TotalHousingUnits = runif(9, 20000, 80000),
    OccupiedUnits = runif(9, 18000, 72000),
    VacantUnits = runif(9, 2000, 8000),
    Treated = 1,
    Post = c(0, 1, 1)
  )
}

# Process parcel data
if(!is.null(final_data)) {
  tryCatch({
    final_data <- final_data %>%
      mutate(across(c(sales_price, assess, per_acre, Acres), ~as.numeric(as.character(.x))))
    
    if(!is.na(st_crs(final_data))) {
      final_data <- final_data %>% st_transform(crs = TARGET_CRS)
    } else {
      st_crs(final_data) <- TARGET_CRS
    }
  }, error = function(e) {
    final_data <- NULL
  })
}

# Process transmission lines - SIMPLIFIED AND FIXED
if(!is.null(transmission_lines_raw)) {
  tryCatch({
    # Filter to Virginia if state border is available
    if(!is.null(va_state_border)) {
      # Make geometries valid if needed
      if(any(!st_is_valid(transmission_lines_raw))) {
        transmission_lines_raw <- st_make_valid(transmission_lines_raw)
      }
      
      # Try to filter to Virginia using intersection
      lines_intersect <- st_intersects(transmission_lines_raw, va_state_border, sparse = FALSE)
      lines_va <- transmission_lines_raw[as.vector(lines_intersect), ]
      
      # If filtering resulted in too few lines, use all lines
      if(nrow(lines_va) < 10) {
        lines_va <- transmission_lines_raw
      }
    } else {
      lines_va <- transmission_lines_raw
    }
    
    # Sample if too many lines for performance
    if(nrow(lines_va) > 1000) {
      lines_va <- lines_va[sample(nrow(lines_va), 1000), ]
    }
    
  }, error = function(e) {
    lines_va <- transmission_lines_raw
  })
} else {
  lines_va <- NULL
}

# Process cities
if(!is.null(VA_cities_raw)) {
  tryCatch({
    VA_cities <- VA_cities_raw %>%
      mutate(POP_2010 = ifelse(POP_2010 < 0, NA, POP_2010)) %>%
      filter(!is.na(POP_2010)) %>%
      slice_max(order_by = POP_2010, n = 300)
  }, error = function(e) {
    VA_cities <- VA_cities_raw
  })
} else {
  VA_cities <- NULL
}

# Process water features
if(!is.null(water_features_raw)) {
  tryCatch({
    water_features <- water_features_raw %>%
      st_as_sf(coords = c("INTPTLON", "INTPTLAT"), crs = TARGET_CRS) %>%
      slice_max(order_by = FULLNAME, n = 500)
  }, error = function(e) {
    water_features <- NULL
  })
} else {
  water_features <- NULL
}

# ===================================================================
# SHINY UI
# ===================================================================
ui <- fluidPage(
  tags$head(tags$title("Virginia Solar Facilities Interactive Map")),
  
  # THEME + LAYOUT TWEAKS
  tags$head(
    tags$style(HTML("
    :root{
      --map-h: 520px; 
      --vt-maroon: #861F41;   /* VT Maroon */
      --vt-orange: #E5751F;   /* VT Orange */
    }
    body { color: var(--vt-maroon); }
    .app-header { 
      text-align:center; 
      font-weight:700; 
      font-size:28px; 
      color:var(--vt-maroon); 
      margin: 10px 0 20px 0;
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

    /* Sidebar styling */
    .controls-wrapper, .well, .col-sm-3 {
      overflow-y: auto;
    }

    .controls-box { background: transparent !important; border: none !important; box-shadow: none !important; padding: 0 !important; }

    label { color: var(--vt-maroon); }

    .family-chooser .btn-group-vertical>.btn { text-align:left; }
    .family-chooser .btn {
      border-radius:10px !important;
      margin-bottom:8px;
      border:1px solid var(--vt-maroon);
      color:var(--vt-maroon);
      background:white;
    }
    .family-chooser .btn:hover { background:rgba(229,117,31,.08); color:var(--vt-maroon); }
    .family-chooser .btn.active, .family-chooser .btn:focus.active {
      background:var(--vt-orange) !important; 
      color:white !important; 
      border-color:var(--vt-orange) !important;
      box-shadow:0 0 0 2px rgba(229,117,31,.25) inset;
    }

    .toggle-title { text-align: left; font-weight: 600; color: var(--vt-maroon); margin: 6px 0 8px 0; }
    .toggle-items .checkbox, .toggle-items .checkbox label { text-align: left; }

    .leaflet-control-layers-list label,
    .leaflet-control-layers-base label,
    .leaflet-control-layers-overlays label {
      text-align: left !important;
    }
  "))
  )
  ,
  
  # Header with title only
  div(class="app-header",
      h1("Interactive Solar Facilities Map", class = "app-title")
  ),
  
  sidebarLayout(
    sidebarPanel(
      width = 3,
      div(class="controls-wrapper controls-box",
          div(
            radioGroupButtons(
              inputId   = "map_level",
              label     = "Select Geographic Level:",
              choices   = c("County Level", "Parcel Level"),
              selected  = "County Level",
              direction = "vertical",
              size      = "sm",
              status    = "primary",
              checkIcon = list(yes = icon("check")),
              width     = "100%"
            ),
            class = "family-chooser"
          ),
          
          conditionalPanel(
            condition = "input.map_level == 'County Level'",
            selectInput(
              "county_year", "Select Year:",
              choices  = if(!is.null(county_year_summary)) sort(unique(county_year_summary$Year), decreasing = TRUE) else c(2018, 2019, 2020),
              selected = if(!is.null(county_year_summary)) max(county_year_summary$Year, na.rm = TRUE) else 2020
            ),
            helpText("Click on a county to view its land value and demographic indicators.")
          ),
          
          conditionalPanel(
            condition = "input.map_level == 'Parcel Level'",
            selectInput("parcel_locality", "Filter by Locality:", choices = NULL)
          ),
          
          div(class = "toggle-title", "Toggle Map Layers:"),
          div(class = "toggle-items",
              checkboxInput("show_transmission", "Show Transmission Lines", value = TRUE),
              checkboxInput("show_cities",       "Show Urban Centers",    value = FALSE),
              checkboxInput("show_water",        "Show Water Features",   value = FALSE)
          ),
          
          # Data source information
          hr(style = "margin: 30px 0 15px 0; border-color: #ccc;"),
          div(class = "toggle-title", "Data Source:"),
          p("These interactive maps are developed by the DSPG Solar program.", 
            style = "font-size: 13px; color: #555; margin-bottom: 10px;"),
          p("For additional analysis and insights, please visit the original dashboard:",
            style = "font-size: 13px; color: #555; margin-bottom: 12px;"),
          tags$a(
            href = "https://solar-farm-vs-land-values-dspg.shinyapps.io/ShinyApp/",
            target = "_blank",
            class = "dashboard-btn",
            style = "background: var(--vt-orange); border: 2px solid var(--vt-orange); color: white; padding: 8px 16px; font-size: 12px; font-weight: 600; text-decoration: none; border-radius: 6px; display: inline-flex; align-items: center; gap: 6px; transition: all 0.3s ease;",
            tags$i(class = "fa fa-external-link", style = "font-size: 11px;"),
            "View Dashboard"
          )
      )
    ),
    
    mainPanel(
      width = 9,
      div(
        
        leafletOutput("interactive_map", height = "520px"),
        
        h3("Solar Facilities Map", 
           style = "color:var(--vt-maroon); font-weight:700; text-align:left; margin-top:25px;"),
        
        p("This map displays the", strong("location and distribution of utility-scale solar"),
          "facilities across Virginia. Each yellow point represents a solar site, overlaid on county boundaries.",
          "Users can explore the map by switching between", strong("county-level "), "and",
          strong("parcel-level views"), ", selecting a year of interest, and clicking on a county to see related land value and demographic indicators.",
          style = "text-align:justify; color:#E5751F;"),
        p("The map also includes optional", strong("geographic context layers"), "that can be toggled on or off.",
          style = "text-align:justify; color:#E5751F;"),
        p("Together, these layers provide insight into how proximity to infrastructure and geographic features relates to solar facility and potential land value impacts.",
          "The basemap style (street, satellite, topographic, or dark) can also be customized for different viewing needs.",
          style = "text-align:justify; color:#E5751F;")
      )
    )
    
  )
)


# ===================================================================
# SHINY SERVER
# ===================================================================

server <- function(input, output, session) {
  
  # Update locality dropdown
  observe({
    if(!is.null(final_data) && "LOCALITY" %in% names(final_data)) {
      updateSelectInput(session, "parcel_locality",
                        choices = c("All", sort(unique(final_data$LOCALITY))),
                        selected = "All"
      )
    } else {
      updateSelectInput(session, "parcel_locality",
                        choices = c("All"),
                        selected = "All"
      )
    }
  })
  
  # Reactive filtered parcel data
  filtered_parcel_data <- reactive({
    if(is.null(final_data)) return(NULL)
    
    req(input$parcel_locality)
    data_to_filter <- if (input$parcel_locality == "All") {
      final_data
    } else {
      final_data %>% filter(LOCALITY == input$parcel_locality)
    }
    
    if (nrow(data_to_filter) > 5000) {
      showNotification("Displaying a random sample of 5,000 parcels for performance.", type = "warning", duration = 6)
      data_to_filter <- sample_n(data_to_filter, 5000)
    }
    
    return(data_to_filter)
  })
  
  # Render Interactive Map - INITIAL SETUP
  output$interactive_map <- renderLeaflet({
    # Base map setup
    map <- leaflet() %>%
      addProviderTiles(providers$CartoDB.Positron, group = "Street Map") %>%
      addProviderTiles(providers$Esri.WorldImagery, group = "Satellite") %>%
      addProviderTiles(providers$Esri.WorldTopoMap, group = "Topographic") %>%
      addProviderTiles(providers$CartoDB.DarkMatter, group = "Dark") %>%
      setView(lng = -79.0, lat = 37.5, zoom = 7)%>%
      addLayersControl(
        baseGroups = c("Street Map", "Satellite", "Topographic", "Dark"),
        options = layersControlOptions(collapsed = FALSE)
      )
    
    # Add state border
    if(!is.null(va_state_border)) {
      map <- map %>% 
        addPolylines(
          data = va_state_border, 
          color = "#00008B", 
          weight = 2.5, 
          opacity = 1,
          group = "State Border"
        )
    }
    
    # Add solar facilities
    if(!is.null(map_data)) {
      map <- map %>%
        addCircleMarkers(
          data = map_data,
          lng = ~Longitude,
          lat = ~Latitude,
          radius = 5,
          color = "gold",
          stroke = TRUE,
          fillOpacity = 0.8,
          group = "Solar Facilities",
          popup = ~paste(
            "<b>Facility:</b>", Facility_Name, "<br>",
            "<b>County:</b>", County, "<br>",
            "<b>Capacity:</b>", Capacity_MW, "MW<br>",
            "<b>Year:</b>", Commission_Year
          )
        )
    }
    
    return(map)
  })
  
  # Observer for adding/removing overlay layers based on checkboxes
  observe({
    proxy <- leafletProxy("interactive_map")
    
    # Transmission Lines
    if(input$show_transmission && !is.null(lines_va) && nrow(lines_va) > 0) {
      proxy %>%
        clearGroup("Transmission Lines") %>%
        addPolylines(
          data = lines_va,
          color = "green",
          weight = 2,
          opacity = 0.7,
          group = "Transmission Lines",
          popup = ~paste0(
            "<b>Transmission Line</b><br>",
            ifelse("VOLTAGE" %in% names(lines_va), paste("Voltage:", VOLTAGE), ""),
            ifelse("OWNER" %in% names(lines_va), paste("<br>Owner:", OWNER), "")
          )
        )
    } else {
      proxy %>% clearGroup("Transmission Lines")
    }
    
    # Cities
    if(input$show_cities && !is.null(VA_cities) && nrow(VA_cities) > 0) {
      proxy %>%
        clearGroup("Urban Centers") %>%
        addCircleMarkers(
          data = VA_cities,
          lng = ~st_coordinates(geometry)[,1],
          lat = ~st_coordinates(geometry)[,2],
          radius = 4,
          color = "red",
          stroke = FALSE,
          fillOpacity = 0.6,
          group = "Urban Centers",
          popup = ~paste0("<b>City:</b> ", NAME)
        )
    } else {
      proxy %>% clearGroup("Urban Centers")
    }
    
    # Water Features
    if(input$show_water && !is.null(water_features) && nrow(water_features) > 0) {
      proxy %>%
        clearGroup("Water Features") %>%
        addCircleMarkers(
          data = water_features,
          lng = ~st_coordinates(geometry)[,1],
          lat = ~st_coordinates(geometry)[,2],
          radius = 3,
          color = "blue",
          stroke = FALSE,
          fillOpacity = 0.5,
          group = "Water Features",
          popup = ~paste0("<b>Waterbody:</b> ", FULLNAME)
        )
    } else {
      proxy %>% clearGroup("Water Features")
    }
  })
  
  # Observer for County / Parcel Level rendering
  observe({
    proxy <- leafletProxy("interactive_map") %>%
      clearGroup("Counties") %>%
      clearGroup("Parcels")
    
    if (input$map_level == "County Level" && !is.null(va_counties) && !is.null(county_year_summary)) {
      selected_year_data <- county_year_summary %>% filter(Year == input$county_year)
      counties_to_show <- left_join(va_counties, selected_year_data, by = c("NAME" = "County"))
      
      # First remove all overlay groups to ensure counties are at the bottom
      proxy %>%
        clearGroup("Transmission Lines") %>%
        clearGroup("Urban Centers") %>%
        clearGroup("Water Features") %>%
        clearGroup("Solar Facilities")
      
      # Add counties first (so they're at the bottom)
      proxy %>%
        addPolygons(
          data = counties_to_show,
          fillColor = "white",
          fillOpacity = 0.6,
          weight = 1.5,
          color = "#444444",
          group = "Counties",
          popup = ~paste0(
            "<b>County:</b> ", NAME, "<br>",
            "<b>Year:</b> ", Year, "<br>",
            "<b>Price per Acre ($):</b> ", round(as.numeric(Price_Per_Acre), 0), "<br>",
            "<b>Corn Yield:</b> ", round(as.numeric(CornYield), 1), " bu/acre<br>",
            "<b>Population:</b> ", round(as.numeric(Population), 0), "<br>",
            "<b>Treated:</b> ", ifelse(as.numeric(Treated) == 1, "Yes (Has solar facility)", "No")
          )
        ) %>%
        flyTo(lng = -79.0, lat = 37.5, zoom = 7)
      
      # Re-add other layers on top of counties
      # Re-add transmission lines if checkbox is checked
      if(input$show_transmission && !is.null(lines_va) && nrow(lines_va) > 0) {
        proxy %>%
          addPolylines(
            data = lines_va,
            color = "green",
            weight = 2,
            opacity = 0.7,
            group = "Transmission Lines",
            popup = ~paste0(
              "<b>Transmission Line</b><br>",
              ifelse("VOLTAGE" %in% names(lines_va), paste("Voltage:", VOLTAGE), ""),
              ifelse("OWNER" %in% names(lines_va), paste("<br>Owner:", OWNER), "")
            )
          )
      }
      
      # Re-add cities if checkbox is checked
      if(input$show_cities && !is.null(VA_cities) && nrow(VA_cities) > 0) {
        proxy %>%
          addCircleMarkers(
            data = VA_cities,
            lng = ~st_coordinates(geometry)[,1],
            lat = ~st_coordinates(geometry)[,2],
            radius = 4,
            color = "red",
            stroke = FALSE,
            fillOpacity = 0.6,
            group = "Urban Centers",
            popup = ~paste0("<b>City:</b> ", NAME)
          )
      }
      
      # Re-add water features if checkbox is checked
      if(input$show_water && !is.null(water_features) && nrow(water_features) > 0) {
        proxy %>%
          addCircleMarkers(
            data = water_features,
            lng = ~st_coordinates(geometry)[,1],
            lat = ~st_coordinates(geometry)[,2],
            radius = 3,
            color = "blue",
            stroke = FALSE,
            fillOpacity = 0.5,
            group = "Water Features",
            popup = ~paste0("<b>Waterbody:</b> ", FULLNAME)
          )
      }
      
      # Always add solar facilities on top
      if(!is.null(map_data)) {
        proxy %>%
          addCircleMarkers(
            data = map_data,
            lng = ~Longitude,
            lat = ~Latitude,
            radius = 5,
            color = "gold",
            stroke = TRUE,
            fillOpacity = 0.8,
            group = "Solar Facilities",
            popup = ~paste(
              "<b>Facility:</b>", Facility_Name, "<br>",
              "<b>County:</b>", County, "<br>",
              "<b>Capacity:</b>", Capacity_MW, "MW<br>",
              "<b>Year:</b>", Commission_Year
            )
          )
      }
    }
    
    if (input$map_level == "Parcel Level") {
      df <- filtered_parcel_data()
      if (!is.null(df) && nrow(df) > 0) {
        
        proxy %>%
          addPolygons(
            data = df,
            fillColor = "#D2691E",
            fillOpacity = 0.6,
            color = "darkgray",
            weight = 0.5,
            group = "Parcels",
            label = ~paste0("Parcel ID: ", PARCELID),
            popup = ~paste0(
              "<b>Parcel ID:</b> ", PARCELID, "<br>",
              "<b>Acres:</b> ", ifelse(is.na(Acres), "NA", round(as.numeric(Acres), 2)), "<br>",
              "<b>Price per Acre ($):</b> ", ifelse(is.na(per_acre), "NA", round(as.numeric(per_acre), 2))
            )
          )
        
        # Zoom logic
        if (input$parcel_locality != "All" && !is.null(va_counties)) {
          county_shape <- va_counties %>% filter(NAME == input$parcel_locality)
          if (nrow(county_shape) > 0) {
            proxy %>% addPolylines(data = county_shape, color = "black", weight = 3, opacity = 1)
            bbox <- st_bbox(county_shape)
            proxy %>% flyToBounds(bbox$xmin, bbox$ymin, bbox$xmax, bbox$ymax)
          }
        } else if(!is.null(va_state_border)) {
          proxy %>% flyTo(lng = -79.0, lat = 37.5, zoom = 7)
        }
      }
    }
  })
}

# ===================================================================
# RUN THE APPLICATION
# ===================================================================

shinyApp(ui = ui, server = server)