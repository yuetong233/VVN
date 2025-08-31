# Location: Maps/Map.R

library(tidyverse)
library(tigris)
library(sf)
library(leaflet)
library(RColorBrewer)
library(viridis)
library(readr)
library(shiny)
library(htmltools)
library(shinyWidgets)   # for radioGroupButtons

# Enable caching for tigris to speed up repeated calls
options(tigris_use_cache = TRUE)



# --- Set Working Directory to Maps folder ---
# Make path handling more robust
get_data_path <- function() {
  # Try different potential paths
  possible_paths <- c(
    "/Users/wheeinner/Library/Mobile Documents/com~apple~CloudDocs/DSPG/VVN/Cost_of_Living_Maps/CoL_Data/",
    "./CoL_Data/",
    "../CoL_Data/",
    "./Data/",
    "../Data/"
  )
  
  for (path in possible_paths) {
    if (dir.exists(path)) {
      cat("Using data path:", path, "\n")
      return(path)
    }
  }
  
  # Fallback - return first path and let user know
  cat("WARNING: Could not find data directory. Using default path.\n")
  cat("Please ensure data files are in:", possible_paths[1], "\n")
  return(possible_paths[1])
}

data_path <- get_data_path()

# --- Load Virginia Counties Spatial Data ---
load_county_data <- function() {
  tryCatch({
    va_counties <- counties(state = "VA", cb = TRUE, class = "sf")
    cat("Successfully loaded", nrow(va_counties), "Virginia counties\n")
    return(va_counties)
  }, error = function(e) {
    cat("Error loading county data:", e$message, "\n")
    cat("Check internet connection or tigris package installation\n")
    stop("Cannot proceed without county boundary data")
  })
}

va_counties <- load_county_data()
virginia_county_names <- sort(unique(va_counties$NAME))

# --- Define Structure Lists ---
family_structures_list <- c(
  "1 Adult: 19–64 Years",
  "2 Adults: 19–64 Years", 
  "1 Adult + 1 Child",
  "2 Adults + 2 Children",
  "1 Adult: 65+",
  "2 Adults: 65+"
)

cost_variables_list <- c(
  "Housing", "Food", "Transportation", "Taxes", "Healthcare",
  "Childcare", "Technology", "Elder Care", "Miscellaneous"
)

# --- Robust Data Loading Function ---
safe_read_csv <- function(file_path, description = "") {
  full_path <- paste0(data_path, file_path)
  
  if (!file.exists(full_path)) {
    cat("WARNING: File not found:", full_path, "\n")
    # Return empty tibble with basic structure
    return(tibble(County = character(), `2 Adults + 2 Children` = numeric()))
  }
  
  tryCatch({
    data <- read_csv(full_path, show_col_types = FALSE)
    cat("✓ Loaded", description, "- Rows:", nrow(data), "Cols:", ncol(data), "\n")
    return(data)
  }, error = function(e) {
    cat("✗ Error loading", description, ":", e$message, "\n")
    # Return empty tibble with basic structure
    return(tibble(County = character(), `2 Adults + 2 Children` = numeric()))
  })
}

# --- Load All Data Files with Better Error Handling ---
cat("Loading data files...\n")

# Minimum Cost Data
min_elder_care_raw <- safe_read_csv("minimum_elder_care_cost.csv", "minimum elder care")
min_transportation_raw <- safe_read_csv("minimum_transportation_data.csv", "minimum transportation")
min_technology_raw <- safe_read_csv("minimum_technology_costs.csv", "minimum technology")
min_food_raw <- safe_read_csv("final_minimum_food_data.csv", "minimum food")
min_tax_raw <- safe_read_csv("minimum_tax_cost.csv", "minimum tax")
min_childcare_raw <- safe_read_csv("childcare_minimum_cost.csv", "minimum childcare")
min_housing_raw <- safe_read_csv("minimum_housing_cost.csv", "minimum housing")
min_healthcare_raw <- safe_read_csv("minimum_healthcare_cost.csv", "minimum healthcare")

# Average Cost Data
avg_elder_care_raw <- safe_read_csv("average_elder_care_cost.csv", "average elder care")
avg_transportation_raw <- safe_read_csv("average_transportation_data.csv", "average transportation")
avg_technology_raw <- safe_read_csv("average_technology_costs.csv", "average technology")
avg_food_raw <- safe_read_csv("final_average_food_data.csv", "average food")
avg_tax_raw <- safe_read_csv("average_tax_cost.csv", "average tax")
avg_childcare_raw <- safe_read_csv("childcare_average_cost.csv", "average childcare")
avg_housing_raw <- safe_read_csv("average_housing_cost.csv", "average housing")
avg_healthcare_raw <- safe_read_csv("average_healthcare_cost.csv", "average healthcare")

# Wage/Income Data (optional)
average_wage <- safe_read_csv("average_wage.csv", "average wage")
minimum_wage <- safe_read_csv("minimum_wage.csv", "minimum wage")

cat("Data loading complete.\n\n")

# --- Improved Data Processing Functions ---
standardize_cols <- function(df) {
  if (nrow(df) == 0) return(df)
  
  # Define more comprehensive renaming patterns
  rename_patterns <- list(
    # Match various formats for each family structure
    "1.*Adult.*19.*64|1.*Adult.*Working|Working.*Adult" = "1 Adult: 19–64 Years",
    "2.*Adults.*19.*64|2.*Adults.*Working|Two.*Adults.*Working" = "2 Adults: 19–64 Years",
    "1.*Adult.*1.*Child|Adult.*Child|Single.*Parent" = "1 Adult + 1 Child", 
    "2.*Adults.*2.*Children|Family.*4|Typical.*Family" = "2 Adults + 2 Children",
    "1.*Adult.*65|Senior.*Individual|Elder.*Individual" = "1 Adult: 65+",
    "2.*Adults.*65|Senior.*Couple|Elder.*Couple" = "2 Adults: 65+"
  )
  
  # Apply renaming patterns
  result <- df
  for (pattern in names(rename_patterns)) {
    matching_cols <- grep(pattern, names(result), value = TRUE, ignore.case = TRUE)
    if (length(matching_cols) > 0) {
      old_name <- matching_cols[1]
      new_name <- rename_patterns[[pattern]]
      names(result)[names(result) == old_name] <- new_name
      cat("Renamed column:", old_name, "->", new_name, "\n")
    }
  }
  
  # Ensure County column exists and is properly formatted
  if ("County" %in% names(result)) {
    result <- result %>% mutate(County = as.character(trimws(County)))
  } else if ("NAME" %in% names(result)) {
    result <- result %>% rename(County = NAME) %>% mutate(County = as.character(trimws(County)))
  }
  
  return(result)
}

process_data <- function(df, data_name = "unknown") {
  cat("Processing", data_name, "data...\n")
  
  if (nrow(df) == 0) {
    cat("Warning:", data_name, "has no data\n")
    return(df)
  }
  
  # Standardize column names
  df_std <- standardize_cols(df)
  
  # Convert family structure columns to numeric
  for (col in intersect(family_structures_list, names(df_std))) {
    df_std[[col]] <- as.numeric(df_std[[col]])
  }
  
  # Add Total Monthly Cost if not present
  if (!"Total Monthly Cost" %in% names(df_std)) {
    if ("2 Adults + 2 Children" %in% names(df_std)) {
      df_std <- df_std %>% mutate(`Total Monthly Cost` = `2 Adults + 2 Children`)
    } else {
      df_std <- df_std %>% mutate(`Total Monthly Cost` = 0)
    }
  }
  
  # Final numeric conversion
  all_cols_to_check <- c(family_structures_list, "Total Monthly Cost")
  present_cols <- intersect(all_cols_to_check, names(df_std))
  
  if (length(present_cols) > 0) {
    df_std <- df_std %>% mutate(across(all_of(present_cols), as.numeric))
  }
  
  cat("✓", data_name, "processed successfully - columns:", paste(names(df_std), collapse = ", "), "\n")
  return(df_std)
}

# --- Process All Data with Better Logging ---
cat("Processing all datasets...\n")

min_elder_care_data <- process_data(min_elder_care_raw, "minimum elder care")
min_transportation_data <- process_data(min_transportation_raw, "minimum transportation")
min_technology_data <- process_data(min_technology_raw, "minimum technology")
min_food_data <- process_data(min_food_raw, "minimum food")
min_tax_data <- process_data(min_tax_raw, "minimum tax")
min_childcare_data <- process_data(min_childcare_raw, "minimum childcare")
min_housing_data <- process_data(min_housing_raw, "minimum housing")
min_healthcare_data <- process_data(min_healthcare_raw, "minimum healthcare")

avg_elder_care_data <- process_data(avg_elder_care_raw, "average elder care")
avg_transportation_data <- process_data(avg_transportation_raw, "average transportation")
avg_technology_data <- process_data(avg_technology_raw, "average technology")
avg_food_data <- process_data(avg_food_raw, "average food")
avg_tax_data <- process_data(avg_tax_raw, "average tax")
avg_childcare_data <- process_data(avg_childcare_raw, "average childcare")
avg_housing_data <- process_data(avg_housing_raw, "average housing")
avg_healthcare_data <- process_data(avg_healthcare_raw, "average healthcare")

cat("Data processing complete.\n\n")

# --- Improved Helper Function ---
safe_pivot_longer <- function(data, cost_var, type_val) {
  if (nrow(data) == 0) {
    cat("Warning: No data for", cost_var, type_val, "\n")
    return(tibble(
      County = character(),
      FamilyStructure = character(),
      Cost = numeric(),
      CostVariable = character(),
      Type = character()
    ))
  }
  
  # Get columns that exist in both the data and family_structures_list
  available_cols <- intersect(names(data), family_structures_list)
  
  if (length(available_cols) == 0) {
    cat("Warning: No matching family structure columns for", cost_var, type_val, "\n")
    return(tibble(
      County = character(),
      FamilyStructure = character(), 
      Cost = numeric(),
      CostVariable = character(),
      Type = character()
    ))
  }
  
  cat("Pivoting", cost_var, type_val, "- using columns:", paste(available_cols, collapse = ", "), "\n")
  
  tryCatch({
    result <- data %>% 
      pivot_longer(
        cols = all_of(available_cols), 
        names_to = "FamilyStructure", 
        values_to = "Cost"
      ) %>% 
      mutate(
        CostVariable = cost_var, 
        Type = type_val,
        Cost = as.numeric(Cost)
      ) %>%
      filter(!is.na(Cost), is.finite(Cost))
    
    cat("✓ Successfully pivoted", nrow(result), "rows for", cost_var, type_val, "\n")
    return(result)
  }, error = function(e) {
    cat("✗ Error pivoting", cost_var, type_val, ":", e$message, "\n")
    return(tibble(
      County = character(),
      FamilyStructure = character(),
      Cost = numeric(),
      CostVariable = character(),
      Type = character()
    ))
  })
}

# --- Create Unified Data Source with Better Error Handling ---
cat("Creating unified dataset...\n")

all_costs_long_for_table_raw <- bind_rows(
  safe_pivot_longer(min_elder_care_data, "Elder Care", "min"),
  safe_pivot_longer(avg_elder_care_data, "Elder Care", "avg"),
  safe_pivot_longer(min_transportation_data, "Transportation", "min"),
  safe_pivot_longer(avg_transportation_data, "Transportation", "avg"),
  safe_pivot_longer(min_technology_data, "Technology", "min"),
  safe_pivot_longer(avg_technology_data, "Technology", "avg"),
  safe_pivot_longer(min_food_data, "Food", "min"),
  safe_pivot_longer(avg_food_data, "Food", "avg"),
  safe_pivot_longer(min_tax_data, "Taxes", "min"),
  safe_pivot_longer(avg_tax_data, "Taxes", "avg"),
  safe_pivot_longer(min_childcare_data, "Childcare", "min"),
  safe_pivot_longer(avg_childcare_data, "Childcare", "avg"),
  safe_pivot_longer(min_housing_data, "Housing", "min"),
  safe_pivot_longer(avg_housing_data, "Housing", "avg"),
  safe_pivot_longer(min_healthcare_data, "Healthcare", "min"),
  safe_pivot_longer(avg_healthcare_data, "Healthcare", "avg")
)

cat("Raw unified dataset has", nrow(all_costs_long_for_table_raw), "rows\n")

# Process the unified dataset
if (nrow(all_costs_long_for_table_raw) > 0) {
  all_costs_long_for_table <- all_costs_long_for_table_raw %>%
    # Remove duplicates and aggregate
    group_by(County, FamilyStructure, CostVariable, Type) %>%
    summarise(Cost = mean(Cost, na.rm = TRUE), .groups = 'drop') %>%
    # Calculate subtotals for miscellaneous
    group_by(County, FamilyStructure, Type) %>%
    mutate(subtotal_for_misc = sum(Cost[CostVariable != 'Taxes'], na.rm = TRUE)) %>%
    ungroup() %>%
    # Add miscellaneous costs
    bind_rows(
      distinct(., County, FamilyStructure, Type, subtotal_for_misc) %>%
        filter(subtotal_for_misc > 0) %>%
        mutate(
          CostVariable = "Miscellaneous", 
          Cost = subtotal_for_misc * 0.10
        ) %>%
        select(-subtotal_for_misc)
    ) %>%
    # Clean up
    select(-subtotal_for_misc) %>%
    filter(!is.na(Cost), is.finite(Cost), Cost > 0)
  
  cat("✓ Final unified dataset has", nrow(all_costs_long_for_table), "rows\n")
} else {
  cat("ERROR: No data available for mapping\n")
  stop("Cannot continue without valid cost data")
}

# --- Prepare Map Data ---
total_costs_for_map <- all_costs_long_for_table %>%
  group_by(NAME = County, Type, FamilyStructure) %>%
  summarise(TotalCost = sum(Cost, na.rm = TRUE), .groups = 'drop') %>%
  filter(TotalCost > 0)

cat("Total costs calculated for", nrow(total_costs_for_map), "county-family-type combinations\n")

# Join with spatial data
va_map_data_full <- va_counties %>%
  left_join(total_costs_for_map, by = "NAME") %>%
  filter(!is.na(Type), !is.na(FamilyStructure))

cat("Map data joined - final dataset has", nrow(va_map_data_full), "rows\n")



# --- Improved Map Creation Function ---
create_cost_map <- function(cost_type = "min", family_structure = "2 Adults + 2 Children") {
  cat("Creating map for:", cost_type, "-", family_structure, "\n")
  
  # Filter data
  map_data <- va_map_data_full %>%
    filter(Type == cost_type, FamilyStructure == family_structure) %>%
    filter(!is.na(TotalCost), is.finite(TotalCost), TotalCost > 0)
  
  if (nrow(map_data) == 0) {
    cat("ERROR: No valid map data for", cost_type, family_structure, "\n")
    return(NULL)
  }
  
  cat("Map data filtered to", nrow(map_data), "counties\n")
  
  # Create popup content with better error handling
  popup_content <- sapply(map_data$NAME, function(county_name) {
    tryCatch({
      county_data <- all_costs_long_for_table %>%
        filter(
          County == county_name,
          FamilyStructure == family_structure,
          Type == cost_type
        )
      
      if (nrow(county_data) == 0) {
        return(paste0("<strong>", htmlEscape(county_name), "</strong><br>No data available"))
      }
      
      total_cost <- sum(county_data$Cost, na.rm = TRUE)
      
      popup_df <- county_data %>%
        filter(Cost > 0) %>%
        mutate(Percentage = if (total_cost > 0) (Cost / total_cost) * 100 else 0) %>%
        mutate(CostVariable = factor(CostVariable, levels = cost_variables_list)) %>%
        arrange(CostVariable)
      
      if (nrow(popup_df) == 0) {
        return(paste0("<strong>", htmlEscape(county_name), "</strong><br>No valid cost data"))
      }
      
      breakdown_lines <- sprintf(
        "%s: $%s (%s%%)",
        popup_df$CostVariable,
        format(round(popup_df$Cost), nsmall = 0, big.mark = ","),
        round(popup_df$Percentage)
      )
      
      breakdown_html <- paste0(
        "<strong>County: </strong>", htmlEscape(county_name), "<br><br>",
        paste(breakdown_lines, collapse = "<br>"),
        "<br><br><strong>Total Monthly Cost (", cost_type, "):</strong> $",
        format(round(total_cost), nsmall = 0, big.mark = ",")
      )
      
      return(breakdown_html)
    }, error = function(e) {
      cat("Error creating popup for", county_name, ":", e$message, "\n")
      return(paste0("<strong>", htmlEscape(county_name), "</strong><br>Error loading data"))
    })
  }, USE.NAMES = FALSE)
  
  # Set up color palette
  costs <- map_data$TotalCost[is.finite(map_data$TotalCost)]
  
  if (length(costs) < 2) {
    cat("WARNING: Insufficient data for color mapping\n")
    return(NULL)
  }
  
  # Create bins
  bins <- unique(quantile(costs, probs = seq(0, 1, length.out = 6), na.rm = TRUE))
  if (length(bins) < 2) {
    bins <- c(min(costs, na.rm = TRUE), max(costs, na.rm = TRUE))
  }
  
  # Set color palette
  if (cost_type == "min") {
    pal <- colorBin(palette = "plasma", domain = map_data$TotalCost, 
                    bins = bins, na.color = "#bdbdbd", reverse = TRUE)
  } else {
    pal <- colorBin(palette = "viridis", domain = map_data$TotalCost, 
                    bins = bins, na.color = "#bdbdbd", reverse = TRUE)
  }
  
  # Create leaflet map
  map <- leaflet(map_data) %>%
    addProviderTiles("CartoDB.Positron") %>%
    setView(lng = -79.0, lat = 37.5, zoom = 7) %>%
    addPolygons(
      fillColor = ~pal(TotalCost),
      weight = 1,
      color = "white",
      fillOpacity = 0.8,
      popup = popup_content,
      label = ~NAME,
      labelOptions = labelOptions(
        style = list("font-weight" = "normal", padding = "3px 8px"),
        textsize = "15px",
        direction = "auto"
      ),
      highlightOptions = highlightOptions(
        weight = 2,
        color = "#666",
        bringToFront = TRUE
      )
    ) %>%
    addLegend(
      pal = pal,
      values = ~TotalCost,
      title = paste("Total Monthly Cost<br>(", 
                    ifelse(cost_type == "min", "Minimum", "Average"), ")"),
      na.label = "No Data",
      opacity = 1
    )
  
  cat("✓ Map created successfully\n")
  return(map)
}



# --- Shiny Server Logic with Enhanced Error Handling ---
shiny_server_logic <- function(input, output, session) {
  
  # Reactive data with validation
  min_map_data_filtered <- reactive({ 
    req(input$family_structure_map_min)
    
    validate(
      need(input$family_structure_map_min %in% family_structures_list, 
           "Invalid family structure selection")
    )
    
    filtered_data <- va_map_data_full %>% 
      filter(Type == "min", FamilyStructure == input$family_structure_map_min)
    
    validate(
      need(nrow(filtered_data) > 0, 
           "No data available for selected family structure")
    )
    
    cat("Min map filter - Selected:", input$family_structure_map_min, 
        "Rows:", nrow(filtered_data), "\n")
    
    return(filtered_data)
  })
  
  avg_map_data_filtered <- reactive({ 
    req(input$family_structure_map_avg)
    
    validate(
      need(input$family_structure_map_avg %in% family_structures_list, 
           "Invalid family structure selection")
    )
    
    filtered_data <- va_map_data_full %>% 
      filter(Type == "avg", FamilyStructure == input$family_structure_map_avg)
    
    validate(
      need(nrow(filtered_data) > 0, 
           "No data available for selected family structure")
    )
    
    cat("Avg map filter - Selected:", input$family_structure_map_avg, 
        "Rows:", nrow(filtered_data), "\n")
    
    return(filtered_data)
  })
  
  # Minimum Cost Map Output
  output$min_map <- renderLeaflet({
    req(input$family_structure_map_min)
    
    tryCatch({
      map_result <- create_cost_map(
        cost_type = "min",
        family_structure = input$family_structure_map_min
      )
      
      if (is.null(map_result)) {
        validate("Unable to create map with current data")
      }
      
      return(map_result)
    }, error = function(e) {
      cat("Error in min_map rendering:", e$message, "\n")
      validate(paste("Map rendering error:", e$message))
    })
  })
  
  # Average Cost Map Output
  output$avg_map <- renderLeaflet({
    req(input$family_structure_map_avg)
    
    tryCatch({
      map_result <- create_cost_map(
        cost_type = "avg",
        family_structure = input$family_structure_map_avg
      )
      
      if (is.null(map_result)) {
        validate("Unable to create map with current data")
      }
      
      return(map_result)
    }, error = function(e) {
      cat("Error in avg_map rendering:", e$message, "\n")
      validate(paste("Map rendering error:", e$message))
    })
  })
}

# --- Improved Shiny UI ---
create_shiny_ui <- function() {
  fluidPage(
    tags$head(
      tags$title("Virginia Cost of Living Interactive Maps"),
      tags$style(HTML("
        :root{
          --vt-maroon: #861F41;   /* VT Maroon */
          --vt-orange: #E5751F;   /* VT Orange */
        }
        body { color: var(--vt-maroon); }
        .app-header { 
          text-align:center; 
          margin: 10px 0 20px 0; 
        }
        .app-title { font-weight:700; font-size: 28px; margin: 0; color: var(--vt-maroon); }
        .section-title { font-size: 22px; font-weight: 600; margin: 10px 0 6px 0; color: var(--vt-maroon); }
        .section-desc { font-size: 14px; color: var(--vt-orange); margin: 8px 0 18px 0; }
        .controls-box { background: #fff7f0; border: 1px solid rgba(134,31,65,.15); border-radius: 10px; padding: 12px; }
        label, .nav-tabs>li>a { color: var(--vt-maroon) !important; }
        a { color: var(--vt-orange); }
        
        /* Dashboard button hover styling */
        .dashboard-btn:hover {
          background: var(--vt-maroon) !important;
          border-color: var(--vt-maroon) !important;
          color: white !important;
          text-decoration: none;
          transform: translateY(-1px);
          box-shadow: 0 4px 8px rgba(134,31,65,0.3);
        }
        
        /* Center the controls column to the middle of the map */
        .map-row { display:flex; align-items:center; }
        .map-col { display:flex; flex-direction:column; }
        .map-holder { width:100%; }
        .controls-col { display:flex; align-items:center; }
        .controls-inner { width:100%; }

        /* shinyWidgets radio buttons themed */
        .family-chooser .btn-group-vertical>.btn { text-align:left; }
        .family-chooser .btn {
          border-radius: 10px !important; 
          margin-bottom: 8px;
          border: 1px solid var(--vt-maroon);
          color: var(--vt-maroon);
          background: white;
        }
        .family-chooser .btn:hover { 
          background: rgba(229,117,31,.08);
          color: var(--vt-maroon);
        }
        .family-chooser .btn.active, 
        .family-chooser .btn:focus.active {
          background: var(--vt-orange) !important; 
          color: white !important; 
          border-color: var(--vt-orange) !important;
          box-shadow: 0 0 0 2px rgba(229,117,31,.25) inset;
        }
        
        /* Tab pill & generic buttons recolor to maroon/orange */
        .nav-tabs>li.active>a, .nav-tabs>li.active>a:focus, .nav-tabs>li.active>a:hover {
          color:#fff !important; background: var(--vt-maroon) !important; border-color: var(--vt-maroon) !important;
        }
        .nav-tabs>li>a:hover { color:#fff !important; background: var(--vt-orange) !important; border-color: var(--vt-orange) !important; }
        .btn, .btn-default { border-color: var(--vt-maroon); color: var(--vt-maroon); }
        .btn:hover { background: rgba(134,31,65,.06); }
        .btn-primary, .btn-success { background: var(--vt-maroon); border-color: var(--vt-maroon); }
        .btn-primary:hover, .btn-success:hover { background: var(--vt-orange); border-color: var(--vt-orange); }
        
        /* Toggle title styling */
        .toggle-title { text-align: left; font-weight: 600; color: var(--vt-maroon); margin: 6px 0 8px 0; }
      "))
    ),
    
    # Header with title only
    div(class = "app-header",
        h1("Virginia Cost of Living Interactive Maps", class = "app-title")
    ),
    
    tabsetPanel(
      # ----------------------- TAB 1 -----------------------
      tabPanel(
        "Minimum Cost Maps",
        fluidRow(
          class = "map-row",
          column(
            width = 3, class = "controls-col",
            div(
              class = "controls-inner controls-box",
              div(style = "text-align:center;",
                  tags$label("Select Family Structure:", `for` = "family_structure_map_min")
              ),
              radioGroupButtons(
                inputId   = "family_structure_map_min",
                choices   = family_structures_list,
                selected  = if (length(family_structures_list) >= 4) family_structures_list[4] else family_structures_list[1],
                direction = "vertical",
                justified = FALSE,
                size      = "sm",
                status    = "primary",
                checkIcon = list(yes = icon("check")),
                width     = "100%"
              ),
              
              # Data source information
              hr(style = "margin: 30px 0 15px 0; border-color: #ccc;"),
              div(class = "toggle-title", "Data Source:"),
              p("These interactive maps are developed by the DSPG Cost of Living program.", 
                style = "font-size: 13px; color: #555; margin-bottom: 10px;"),
              p("For additional analysis and insights, please visit the original dashboard:",
                style = "font-size: 13px; color: #555; margin-bottom: 12px;"),
              tags$a(
                href = "https://cost.shinyapps.io/col_app/",
                target = "_blank",
                class = "dashboard-btn",
                style = "background: var(--vt-orange); border: 2px solid var(--vt-orange); color: white; padding: 8px 16px; font-size: 12px; font-weight: 600; text-decoration: none; border-radius: 6px; display: inline-flex; align-items: center; gap: 6px; transition: all 0.3s ease;",
                tags$i(class = "fa fa-external-link", style = "font-size: 11px;"),
                "View Dashboard"
              ),
              class = "family-chooser"
            )
          ),
          column(
            width = 9, class = "map-col",
            # Map
            div(class = "map-holder", leafletOutput("min_map", height = 520)),
            # Title + description
            div(class = "section-title", "Minimum Cost Map"),
            div(
              class = "section-desc",
              style = "text-align: justify;",
              p(HTML(paste0(
                "This map shows the <strong>lowest monthly cost of living</strong> for each Virginia county, based on essentials like housing, food, childcare, healthcare, and transportation. ",
                "It represents a <strong>minimum cost</strong>—just enough to meet basic needs. ",
                "Estimates come from HUD, USDA, Census, and other public data, with income-based adjustments. ",
                "For counties lacking specific inputs, healthcare/elder-care values are imputed with income-based multipliers (county median vs. state median). ",
                "Values are expressed in current USD."
              ))),
              p(
                "Users can select one of six family structures, and view how the minimum cost varies by household type and location. ",
                "Darker colors indicate higher cost, making it easy to compare counties."
              )
            )
          )
        )
      ),
      
      # ----------------------- TAB 2 -----------------------
      tabPanel(
        "Average Cost Maps",
        fluidRow(
          class = "map-row",
          column(
            width = 3, class = "controls-col",
            div(
              class = "controls-inner controls-box",
              div(style = "text-align:center;",
                  tags$label("Select Family Structure:", `for` = "family_structure_map_avg")
              ),
              radioGroupButtons(
                inputId   = "family_structure_map_avg",
                choices   = family_structures_list,
                selected  = if (length(family_structures_list) >= 4) family_structures_list[4] else family_structures_list[1],
                direction = "vertical",
                justified = FALSE,
                size      = "sm",
                status    = "primary",
                checkIcon = list(yes = icon("check")),
                width     = "100%"
              ),
              
              # Data source information
              hr(style = "margin: 30px 0 15px 0; border-color: #ccc;"),
              div(class = "toggle-title", "Data Source:"),
              p("These interactive maps are developed by the DSPG Cost of Living program.", 
                style = "font-size: 13px; color: #555; margin-bottom: 10px;"),
              p("For additional analysis and insights, please visit the original dashboard:",
                style = "font-size: 13px; color: #555; margin-bottom: 12px;"),
              tags$a(
                href = "https://cost.shinyapps.io/col_app/",
                target = "_blank",
                class = "dashboard-btn",
                style = "background: var(--vt-orange); border: 2px solid var(--vt-orange); color: white; padding: 8px 16px; font-size: 12px; font-weight: 600; text-decoration: none; border-radius: 6px; display: inline-flex; align-items: center; gap: 6px; transition: all 0.3s ease;",
                tags$i(class = "fa fa-external-link", style = "font-size: 11px;"),
                "View Dashboard"
              ),
              class = "family-chooser"
            )
          ),
          column(
            width = 9, class = "map-col",
            # Map
            div(class = "map-holder", leafletOutput("avg_map", height = 520)),
            # Title + description
            div(class = "section-title", "Average Cost Map"),
            div(
              class = "section-desc",
              style = "text-align: justify;",
              p(
                "This map shows the ", strong("average monthly cost of living"),
                " in each Virginia county, reflecting what households typically spend for a ",
                strong("modest but sustainable lifestyle"), ". ",
                "Costs cover the same categories as the minimum map but use ",
                strong("typical expenditure patterns"),
                " instead of bare minimums. Data sources include HUD, USDA, and Census ACS, ",
                "with adjustments to reflect local conditions."
              ),
              p(
                "Users can select one of six family structures, and view how the minimum cost varies by household type and location. ",
                "Darker shading marks higher-cost counties, with urban and suburban areas generally the most expensive."
              )
            )
          )
        )
      )
    )
  )
}


# --- Complete Shiny Application ---
create_complete_shiny_app <- function() {
  
  # Enhanced server function
  server <- function(input, output, session) {
    # Call the main server logic
    shiny_server_logic(input, output, session)
  }
  
  # Create UI
  ui <- create_shiny_ui()
  
  # Return complete app
  list(ui = ui, server = server)
}

# --- Run the Shiny Application ---
app <- create_complete_shiny_app()
shinyApp(ui = app$ui, server = app$server)