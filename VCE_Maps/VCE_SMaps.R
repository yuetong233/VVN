# ========================================================================
# Virginia Cooperative Extension Interactive Maps - TABBED UI
# ========================================================================

# === LIBRARIES FOR INTERACTIVE MAPS ===
library(shiny)
library(shinyWidgets)
library(bslib)
library(dplyr)
library(readr)
library(leaflet)
library(sf)
library(tigris)
library(tidycensus)
library(readxl)
library(htmltools)
library(stringr)
library(tidyr)
library(viridisLite)
library(scales)

# === DATA PATH CONFIGURATION ===
DATA_PATH <- "VCE_data"

get_data_path <- function(filename) {
  return(file.path(DATA_PATH, filename))
}

# === HELPER FUNCTION ===
clean_county <- function(x) {
  x %>%
    tolower() %>%
    gsub(" county, virginia| city, virginia", "", .) %>%
    gsub(" county| city", "", .) %>%
    trimws()
}

# === UI ===
ui <- fluidPage(
  tags$head(tags$title("Virginia Cooperative Extension Interactive Maps")),
  tags$head(
    tags$style(HTML("
  :root{
    --vt-maroon:#861F41;
    --vt-orange:#E5751F;
    --map-h:600px;
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

  /* Select view button styling */
  .btn-outline-primary {
    border-color: var(--vt-maroon) !important;
    color: var(--vt-maroon) !important;
    background-color: transparent !important;
  }
  .btn-outline-primary:hover,
  .btn-outline-primary:focus,
  .btn-outline-primary:active,
  .btn-outline-primary.active {
    background-color: var(--vt-orange) !important;
    border-color: var(--vt-orange) !important;
    color: white !important;
  }

  /* Tabset styling */
  .nav-tabs>li>a{ color:var(--vt-maroon) !important; border-radius:10px 10px 0 0; }
  .nav-tabs>li.active>a, .nav-tabs>li.active>a:focus, .nav-tabs>li.active>a:hover{
    color:#fff !important; background:var(--vt-maroon) !important; border-color:var(--vt-maroon) !important;
  }
  .nav-tabs>li>a:hover{ color:#fff !important; background:var(--vt-orange) !important; border-color:var(--vt-orange) !important; }
  .nav-tabs{ border-bottom:none !important; }
  .tab-content{ background:transparent !important; border:none !important; padding:14px 16px 18px 16px; }

  /* Sub-panel styling */
  .sub-panel .nav-pills>li>a { 
    color:var(--vt-maroon) !important; 
    background:transparent !important;
    border: 1px solid var(--vt-maroon) !important;
    margin: 2px;
    border-radius: 6px;
  }
  .sub-panel .nav-pills>li.active>a, .sub-panel .nav-pills>li.active>a:focus {
    color:#fff !important; 
    background:var(--vt-maroon) !important;
  }
  .sub-panel .nav-pills>li>a:hover {
    color:#fff !important; 
    background:var(--vt-orange) !important;
    border-color:var(--vt-orange) !important;
  }

  /* Section text */
  .section-title{ font-size:20px; font-weight:600; margin:12px 0 4px 0; color:var(--vt-maroon); }
  .section-desc{ font-size:14px; color:var(--vt-orange); margin:6px 0 14px 0; }

  /* Sidebar styling */
  .well, .sidebarPanel, .panel.sidebarPanel {
    background-color: #f0f0f0 !important;
    border-radius: 10px;
    padding: 12px;
    box-shadow: inset 0 0 4px rgba(0,0,0,0.08);
  }

  /* Map sizing */
  #fourh_participant_map, #fourh_volunteer_map, #fourh_ratio_map,
  #vce_participant_map, #vce_volunteer_map, #vce_ratio_map { 
    height: var(--map-h); 
  }
  
  /* Select input styling */
  select.form-control, .selectize-input {
    border: 1px solid var(--vt-maroon) !important;
    box-shadow: none !important;
  }
  select.form-control:focus, .selectize-input.focus {
    border-color: var(--vt-maroon) !important;
    outline: none !important;
    box-shadow: 0 0 4px var(--vt-maroon) !important;
  }
  .selectize-dropdown .active {
    background-color: var(--vt-maroon) !important;
    color: #fff !important;
  }

"))
  ),
  
  # Header with title
  div(class="app-header",
      h1("Virginia Cooperative Extension Interactive Maps", class = "app-title")
  ),
  
  tabsetPanel(
    # 4-H Tab
    tabPanel("4-H Programs",
             sidebarLayout(
               sidebarPanel(
                 width = 3,
                 # View selection buttons
                 h5(
                   strong("Select View:"),
                   style = "color:var(--vt-maroon); margin-bottom:15px;"
                 ),
                 div(style = "margin-bottom:20px;",
                     actionButton("fourh_participant_btn", "Participant County", 
                                  class = "btn btn-outline-primary active", 
                                  style = "width:100%; margin-bottom:5px;"),
                     actionButton("fourh_volunteer_btn", "Volunteer County", 
                                  class = "btn btn-outline-primary", 
                                  style = "width:100%; margin-bottom:5px;"),
                     actionButton("fourh_ratio_btn", "Participants vs Volunteers", 
                                  class = "btn btn-outline-primary", 
                                  style = "width:100%;")
                 ),
                 
                 # Conditional panels for different group selectors
                 conditionalPanel(
                   condition = "input.fourh_view_type == 'participant'",
                   selectInput("fourh_participant_group", "Select Group:",
                               choices = c(
                                 "Hispanic" = "eHispanic",
                                 "Not Hispanic" = "eNotHispanic",
                                 "Ethnicity Not Provided" = "eNotProvided",
                                 "Prefer Not to State Ethnicity" = "ePreferNotToState",
                                 "White" = "rWhite",
                                 "Black or African American" = "rBlack",
                                 "American Indian or Alaskan Native" = "rIndianAlaskan",
                                 "Native Hawaiian or Pacific Islander" = "rHawaiianIslander",
                                 "Asian" = "rAsian",
                                 "Two or More Races" = "rMoreThanOne",
                                 "Race Undetermined" = "rUndetermined"
                               ))
                 ),
                 
                 conditionalPanel(
                   condition = "input.fourh_view_type == 'volunteer'",
                   selectInput("fourh_volunteer_group", "Select Group:",
                               choices = c("Hispanic", "Non-Hispanic",
                                           "White", "Black", "Asian",
                                           "Undetermined"))
                 ),
                 
                 conditionalPanel(
                   condition = "input.fourh_view_type == 'ratio'",
                   h5("Volunteer-to-Participant Ratio", 
                      style = "color:var(--vt-maroon); margin-top:10px;"),
                   p("Shows volunteers as a percentage of participants by county.",
                     style = "color:#666; font-size:13px;")
                 ),
                 
                 # Data source information
                 hr(style = "margin: 30px 0 15px 0; border-color: #ccc;"),
                 div(class = "toggle-title", strong("Data Source:")),
                 p("These interactive maps are developed by the DSPG VCE program.", 
                   style = "font-size: 13px; color: #555; margin-bottom: 10px;"),
                 p("For additional analysis and insights, please visit the original dashboard:",
                   style = "font-size: 13px; color: #555; margin-bottom: 12px;"),
                 tags$a(
                   href = "https://vcedemographics.shinyapps.io/dashboard_take_2_july_28/",
                   target = "_blank",
                   class = "dashboard-btn",
                   style = "background: var(--vt-orange); border: 2px solid var(--vt-orange); color: white; padding: 8px 16px; font-size: 12px; font-weight: 600; text-decoration: none; border-radius: 6px; display: inline-flex; align-items: center; gap: 6px; transition: all 0.3s ease;",
                   tags$i(class = "fa fa-external-link", style = "font-size: 11px;"),
                   "View Dashboard"
                 ),
                 
                 # Hidden input to track view type
                 tags$script(HTML("
                   $(document).on('click', '#fourh_participant_btn', function() {
                     Shiny.setInputValue('fourh_view_type', 'participant');
                     $('.btn-outline-primary').removeClass('active');
                     $('#fourh_participant_btn').addClass('active');
                   });
                   $(document).on('click', '#fourh_volunteer_btn', function() {
                     Shiny.setInputValue('fourh_view_type', 'volunteer');
                     $('.btn-outline-primary').removeClass('active');
                     $('#fourh_volunteer_btn').addClass('active');
                   });
                   $(document).on('click', '#fourh_ratio_btn', function() {
                     Shiny.setInputValue('fourh_view_type', 'ratio');
                     $('.btn-outline-primary').removeClass('active');
                     $('#fourh_ratio_btn').addClass('active');
                   });
                   
                   // Set initial state
                   $(document).ready(function() {
                     Shiny.setInputValue('fourh_view_type', 'participant');
                   });
                 "))
               ),
               
               mainPanel(
                 conditionalPanel(
                   condition = "input.fourh_view_type == 'participant' || !input.fourh_view_type",
                   leafletOutput("fourh_participant_map", height = "600px"),
                   br(),
                   h3("4-H Participation by County", 
                      style = "color:var(--vt-maroon); font-weight:700; text-align:left; margin-top:25px;"),
                   p("This map shows 4-H participation counts across Virginia counties by demographic group.",
                     "Darker colors indicate higher participation numbers.",
                     style = "text-align:justify; color:#E5751F;"),
                   p("Users can select different racial and ethnic groups to explore participation patterns.",
                     style = "text-align:justify; color:#E5751F;")
                 ),
                 
                 conditionalPanel(
                   condition = "input.fourh_view_type == 'volunteer'",
                   leafletOutput("fourh_volunteer_map", height = "600px"),
                   br(),
                   h3("4-H Volunteers by County", 
                      style = "color:var(--vt-maroon); font-weight:700; text-align:left; margin-top:25px;"),
                   p("This map displays 4-H volunteer demographics across Virginia counties.",
                     style = "text-align:justify; color:#E5751F;"),
                   p("Users can filter by demographic group to see volunteer distribution patterns.",
                     "The visualization helps identify areas with strong volunteer engagement.",
                     style = "text-align:justify; color:#E5751F;")
                 ),
                 
                 conditionalPanel(
                   condition = "input.fourh_view_type == 'ratio'",
                   leafletOutput("fourh_ratio_map", height = "600px"),
                   br(),
                   h3("4-H Volunteers as % of Participants", 
                      style = "color:var(--vt-maroon); font-weight:700; text-align:left; margin-top:25px;"),
                   p("This map compares volunteer engagement relative to participant numbers across counties.",
                     "Higher percentages indicate counties with more volunteer support per participant.",
                     style = "text-align:justify; color:#E5751F;"),
                   p("This metric helps identify areas that may need additional volunteer recruitment.",
                     style = "text-align:justify; color:#E5751F;")
                 )
               )
             )
    ),
    
    # VCE Tab
    tabPanel("VCE Programs",
             sidebarLayout(
               sidebarPanel(
                 width = 3,
                 # View selection buttons
                 h5(
                   strong("Select View:"),
                   style = "color:var(--vt-maroon); margin-bottom:15px;"
                 ),
                 div(style = "margin-bottom:20px;",
                     actionButton("vce_participant_btn", "Participant County", 
                                  class = "btn btn-outline-primary active", 
                                  style = "width:100%; margin-bottom:5px;"),
                     actionButton("vce_volunteer_btn", "Volunteer County", 
                                  class = "btn btn-outline-primary", 
                                  style = "width:100%; margin-bottom:5px;"),
                     actionButton("vce_ratio_btn", "Participants vs Volunteers", 
                                  class = "btn btn-outline-primary", 
                                  style = "width:100%;")
                 ),
                 
                 # Conditional panels for different group selectors
                 conditionalPanel(
                   condition = "input.vce_view_type == 'participant'",
                   selectInput("vce_participant_group", "Select Group:",
                               choices = c(
                                 "All Participants" = "participants_total",
                                 "Hispanic" = "participants_ethnicity_hispanic",
                                 "White" = "participants_race_white",
                                 "Black" = "participants_race_black",
                                 "Asian" = "participants_race_asian"
                               ))
                 ),
                 
                 conditionalPanel(
                   condition = "input.vce_view_type == 'volunteer'",
                   selectInput("vce_volunteer_group", "Select Group:",
                               choices = c(
                                 "All Volunteers" = "all",
                                 "Hispanic" = "eth_hispanic",
                                 "White" = "race_white",
                                 "Black" = "race_black",
                                 "Asian" = "race_asian"
                               ))
                 ),
                 
                 conditionalPanel(
                   condition = "input.vce_view_type == 'ratio'",
                   h5("Volunteer-to-Participant Ratio", 
                      style = "color:var(--vt-maroon); margin-top:10px;"),
                   p("Shows volunteers as a percentage of participants by county.",
                     style = "color:#666; font-size:13px;")
                 ),
                 
                 # Data source information panel
                 hr(style = "margin: 30px 0 15px 0; border-color: #ccc;"),
                 div(class = "toggle-title", strong("Data Source:")),
                 p("These interactive maps are developed by the DSPG VCE program.", 
                   style = "font-size: 13px; color: #555; margin-bottom: 10px;"),
                 p("For additional analysis and insights, please visit the original dashboard:",
                   style = "font-size: 13px; color: #555; margin-bottom: 12px;"),
                 tags$a(
                   href = "https://vcedemographics.shinyapps.io/dashboard_take_2_july_28/",
                   target = "_blank",
                   class = "dashboard-btn",
                   style = "background: var(--vt-orange); border: 2px solid var(--vt-orange); color: white; padding: 8px 16px; font-size: 12px; font-weight: 600; text-decoration: none; border-radius: 6px; display: inline-flex; align-items: center; gap: 6px; transition: all 0.3s ease;",
                   tags$i(class = "fa fa-external-link", style = "font-size: 11px;"),
                   "View Dashboard"
                   
                 ),
                 
                 # Hidden input to track view type
                 tags$script(HTML("
                   $(document).on('click', '#vce_participant_btn', function() {
                     Shiny.setInputValue('vce_view_type', 'participant');
                     $('.btn-outline-primary').removeClass('active');
                     $('#vce_participant_btn').addClass('active');
                   });
                   $(document).on('click', '#vce_volunteer_btn', function() {
                     Shiny.setInputValue('vce_view_type', 'volunteer');
                     $('.btn-outline-primary').removeClass('active');
                     $('#vce_volunteer_btn').addClass('active');
                   });
                   $(document).on('click', '#vce_ratio_btn', function() {
                     Shiny.setInputValue('vce_view_type', 'ratio');
                     $('.btn-outline-primary').removeClass('active');
                     $('#vce_ratio_btn').addClass('active');
                   });
                   
                   // Set initial state
                   $(document).ready(function() {
                     Shiny.setInputValue('vce_view_type', 'participant');
                   });
                 "))
               ),
               
               mainPanel(
                 conditionalPanel(
                   condition = "input.vce_view_type == 'participant' || !input.vce_view_type",
                   leafletOutput("vce_participant_map", height = "600px"),
                   br(),
                   h3("VCE Participation by County", 
                      style = "color:var(--vt-maroon); font-weight:700; text-align:left; margin-top:25px;"),
                   p("This map shows Virginia Cooperative Extension program participation across counties.",
                     "Counties are shaded based on participant counts, making it easy to compare engagement levels statewide.",
                     style = "text-align:justify; color:#E5751F;"),
                   p("Users can view total participation or filter by specific demographic groups.",
                     style = "text-align:justify; color:#E5751F;")
                 ),
                 
                 conditionalPanel(
                   condition = "input.vce_view_type == 'volunteer'",
                   leafletOutput("vce_volunteer_map", height = "600px"),
                   br(),
                   h3("VCE Volunteers by County", 
                      style = "color:var(--vt-maroon); font-weight:700; text-align:left; margin-top:25px;"),
                   p("This map displays VCE volunteer engagement across Virginia counties by demographic group.",
                     "Volunteers are essential to VCE program delivery and community outreach.",
                     "The shading highlights where volunteer activity is concentrated.",
                     style = "text-align:justify; color:#E5751F;"),
                   p("The map helps identify volunteer distribution patterns and potential recruitment areas.",
                     "Users can filter by demographic group to examine which communities are most engaged in volunteering.",
                     style = "text-align:justify; color:#E5751F;")
                 ),
                 
                 conditionalPanel(
                   condition = "input.vce_view_type == 'ratio'",
                   leafletOutput("vce_ratio_map", height = "600px"),
                   br(),
                   h3("VCE Volunteers as % of Participants", 
                      style = "color:var(--vt-maroon); font-weight:700; text-align:left; margin-top:25px;"),
                   p("This map shows the ratio of volunteers to participants across VCE programs.",
                     "Counties with higher ratios may have stronger volunteer support networks.",
                     style = "text-align:justify; color:#E5751F;"),
                   p("This analysis helps inform volunteer recruitment and program support strategies.",
                     style = "text-align:justify; color:#E5751F;")
                 )
               )
             )
    )
  )
)

# === SERVER ===
server <- function(input, output, session) {
  
  # Load census API key and get VA counties shapefile
  census_api_key("6ee5ecd73ef70e9464ee5509dec0cdd4a3fa86c7", install = TRUE, overwrite = TRUE)
  va_counties <- counties("VA", cb = TRUE, year = 2023) %>%
    st_transform(4326) %>%
    mutate(County = clean_county(NAME))
  
  # === 4-H PARTICIPATION DATA ===
  fourh_data <- reactive({
    read_csv(get_data_path("annualprogreport.csv")) %>%
      mutate(
        County = CountyArea %>%
          tolower() %>%
          trimws(),
        County = case_when(
          County == "fairfax" & grepl("city", CountyArea, ignore.case = TRUE) ~ "fairfax city",
          TRUE ~ County
        ),
        Total = rowSums(select(., starts_with("e"), starts_with("r")), na.rm = TRUE)
      )
  })
  
  # === 4-H PARTICIPANT MAP ===
  output$fourh_participant_map <- renderLeaflet({
    req(input$fourh_participant_group)
    df <- left_join(va_counties, fourh_data(), by = "County") %>% st_as_sf()
    selected_values <- df[[input$fourh_participant_group]]
    selected_values[is.na(selected_values)] <- 0
    total_values <- df$Total
    total_values[is.na(total_values)] <- 0
    pal <- colorBin("Purples", domain = selected_values, bins = 5, na.color = "#f0f0f0")
    df <- df %>%
      mutate(label_4h = paste0(
        "<strong>", toupper(County), "</strong><br>",
        "Selected Group: ", selected_values, "<br>",
        "Total Participants: ", total_values
      ))
    leaflet(df) %>%
      addProviderTiles("CartoDB.Positron") %>%
      setView(lng = -79.0, lat = 37.5, zoom = 7) %>%
      addPolygons(
        fillColor = ~pal(selected_values),
        color = "black", weight = 1, fillOpacity = 0.7,
        label = lapply(df$label_4h, htmltools::HTML),
        highlightOptions = highlightOptions(weight = 2, color = "#666",
                                            fillOpacity = 0.9, bringToFront = TRUE)
      ) %>%
      addLegend("bottomright", pal = pal, values = selected_values,
                title = "4-H Participants", opacity = 1)
  })
  
  # === 4-H VOLUNTEER DATA ===
  volunteer_demo_df <- read_excel(get_data_path("4H_Volunteers.xlsx")) %>%
    mutate(CountyOrCity = str_to_title(trimws(CountyOrCity)))
  
  expand_demo_data <- function(df, column, pattern_list) {
    for (pattern in pattern_list) {
      colname <- gsub(" ", "_", pattern)
      regex <- paste0(pattern, ":\\s*(\\d+)")
      df[[colname]] <- as.numeric(str_match(df[[column]], regex)[, 2])
      df[[colname]][is.na(df[[colname]])] <- 0
    }
    return(df)
  }
  
  ethnicity_list <- c("Hispanic", "Non-Hispanic")
  race_list <- c("White", "Black", "Asian", "Undetermined")
  
  volunteer_demo_df <- expand_demo_data(volunteer_demo_df, "Ethnicity", ethnicity_list)
  volunteer_demo_df <- expand_demo_data(volunteer_demo_df, "Race", race_list)
  
  va_counties_map <- counties(state = "VA", cb = TRUE, year = 2023) %>%
    st_transform(4326) %>%
    mutate(CountyOrCity = str_to_title(gsub(" County, Virginia| City, Virginia", "", NAME)))
  
  volunteer_demo_map_data <- left_join(va_counties_map, volunteer_demo_df, by = "CountyOrCity") %>%
    st_as_sf()
  
  # === 4-H VOLUNTEER MAP ===
  output$fourh_volunteer_map <- renderLeaflet({
    req(input$fourh_volunteer_group)
    column <- gsub(" ", "_", input$fourh_volunteer_group)
    data <- volunteer_demo_map_data
    data$selected_group <- data[[column]]
    data$selected_group[is.na(data$selected_group)] <- 0
    data <- data %>%
      mutate(label_filtered = paste0(
        "<strong>", toupper(CountyOrCity), "</strong><br>",
        input$fourh_volunteer_group, ": ", selected_group, "<br>",
        "Total Volunteers: ", `4-H Volunteers`
      ))
    
    pal <- colorBin("YlGnBu", domain = data$selected_group, bins = 5, na.color = "#f0f0f0")
    leaflet(data) %>%
      addProviderTiles("CartoDB.Positron") %>%
      setView(lng = -79.0, lat = 37.5, zoom = 7) %>%
      addPolygons(
        fillColor = ~pal(selected_group),
        color = "black", weight = 1,
        fillOpacity = 0.7,
        label = lapply(data$label_filtered, htmltools::HTML),
        highlightOptions = highlightOptions(weight = 2, color = "#666",
                                            fillOpacity = 0.9, bringToFront = TRUE)
      ) %>%
      addLegend("bottomright", pal = pal, values = data$selected_group,
                title = input$fourh_volunteer_group, opacity = 1)
  })
  
  # === 4-H VOLUNTEERS VS PARTICIPANTS MAP ===
  output$fourh_ratio_map <- renderLeaflet({
    volunteers_df <- volunteer_demo_df %>%
      transmute(
        County = tolower(CountyOrCity),
        Volunteers = `4-H Volunteers`
      )
    
    participants_df <- fourh_data() %>%
      transmute(
        County = tolower(County),
        Participants = Total
      )
    
    combined <- left_join(volunteers_df, participants_df, by = "County") %>%
      mutate(
        RatioVP = ifelse(Participants > 0,
                         round(Volunteers / Participants * 100, 2), NA)
      )
    
    data <- left_join(va_counties, combined, by = "County") %>%
      st_as_sf() %>%
      mutate(label_vp = paste0(
        "<strong>", toupper(County), "</strong><br>",
        "Volunteers: ", Volunteers, "<br>",
        "Participants: ", Participants, "<br>",
        "Volunteers as % of Participants: ", RatioVP, "%"
      ))
    
    pal <- colorBin("YlOrRd", domain = data$RatioVP,
                    bins = 5, na.color = "#f0f0f0")
    
    leaflet(data) %>%
      addProviderTiles("CartoDB.Positron") %>%
      setView(lng = -79.0, lat = 37.5, zoom = 7) %>%
      addPolygons(
        fillColor = ~pal(RatioVP),
        color = "black", weight = 1,
        fillOpacity = 0.7,
        label = lapply(data$label_vp, htmltools::HTML),
        highlightOptions = highlightOptions(weight = 2, color = "#666",
                                            fillOpacity = 0.9, bringToFront = TRUE)
      ) %>%
      addLegend("bottomright", pal = pal,
                values = data$RatioVP,
                title = "% Volunteers of Participants",
                opacity = 1)
  })
  
  # === VCE VOLUNTEER DATA ===
  vce_volunteer_data <- read_excel(get_data_path("VCE_VolunteerCounty_Data.xlsx")) %>%
    filter(HoursWorked > 0, !is.na(CountyOrCity)) %>%
    mutate(
      County = tolower(trimws(CountyOrCity)),
      Race = `CF - Demographic Information - Race`,
      Ethnicity = `CF - Demographic Information - Ethnicity`
    )
  
  # === VCE VOLUNTEER MAP ===
  output$vce_volunteer_map <- renderLeaflet({
    data <- vce_volunteer_data
    
    switch(input$vce_volunteer_group,
           "eth_hispanic" = data <- data %>% filter(Ethnicity == "Hispanic or Latino/a/x"),
           "race_white" = data <- data %>% filter(Race == "05. White"),
           "race_black" = data <- data %>% filter(Race == "03. Black or African American"),
           "race_asian" = data <- data %>% filter(Race == "02. Asian"),
           "all" = data <- data
    )
    
    total_counts <- vce_volunteer_data %>%
      count(County, name = "TotalVolunteers")
    
    group_counts <- data %>%
      count(County, name = "GroupVolunteers")
    
    merged_data <- left_join(va_counties, total_counts, by = "County") %>%
      left_join(group_counts, by = "County") %>%
      replace_na(list(GroupVolunteers = 0)) %>%
      st_as_sf() %>%
      mutate(label_vce = paste0(
        "<strong>", toupper(County), "</strong><br>",
        "Selected Group Volunteers: ", GroupVolunteers, "<br>",
        "Total Volunteers: ", TotalVolunteers
      ))
    
    pal <- colorBin("OrRd", domain = merged_data$GroupVolunteers, bins = 5, na.color = "#f0f0f0")
    
    leaflet(merged_data) %>%
      addProviderTiles("CartoDB.Positron") %>%
      setView(lng = -79.0, lat = 37.5, zoom = 7) %>%
      addPolygons(
        fillColor = ~pal(GroupVolunteers),
        color = "black", weight = 1,
        fillOpacity = 0.7,
        label = lapply(merged_data$label_vce, htmltools::HTML),
        highlightOptions = highlightOptions(weight = 2, color = "#666",
                                            fillOpacity = 0.9, bringToFront = TRUE)
      ) %>%
      addLegend("bottomright", pal = pal,
                values = merged_data$GroupVolunteers,
                title = "Volunteers (Selected Group)",
                opacity = 1)
  })
  
  # === VCE PARTICIPATION DATA ===
  participant_data <- read_excel(get_data_path("vce_particip_demographics.xlsx")) %>%
    mutate(County = tolower(trimws(site_county)))
  
  # === VCE PARTICIPANT MAP ===
  output$vce_participant_map <- renderLeaflet({
    # Total participants per county
    total_counts <- participant_data %>%
      group_by(County) %>%
      summarise(TotalParticipants = sum(participants_total, na.rm = TRUE), .groups = 'drop')
    
    # Filtered group counts based on selection
    group_counts <- participant_data %>%
      mutate(GroupCount = case_when(
        input$vce_participant_group == "participants_ethnicity_hispanic" ~ participants_ethnicity_hispanic,
        input$vce_participant_group == "participants_race_white" ~ participants_race_white,
        input$vce_participant_group == "participants_race_black" ~ participants_race_black,
        input$vce_participant_group == "participants_race_asian" ~ participants_race_asian,
        input$vce_participant_group == "participants_total" ~ participants_total,
        TRUE ~ 0
      )) %>%
      group_by(County) %>%
      summarise(GroupParticipants = sum(GroupCount, na.rm = TRUE), .groups = 'drop')
    
    # Merge with spatial data
    merged <- left_join(va_counties, total_counts, by = "County") %>%
      left_join(group_counts, by = "County") %>%
      replace_na(list(GroupParticipants = 0, TotalParticipants = 0)) %>%
      st_as_sf() %>%
      mutate(label_participant = paste0(
        "<strong>", toupper(County), "</strong><br>",
        "Selected Group Participants: ", GroupParticipants, "<br>",
        "Total Participants: ", TotalParticipants
      ))
    
    max_value <- max(merged$GroupParticipants, na.rm = TRUE)
    if(max_value == 0 || is.infinite(max_value)) {
      max_value <- 1
      bins <- c(0, 1)
    } else {
      bins <- c(0, round(max_value/5), round(2*max_value/5), round(3*max_value/5), round(4*max_value/5), max_value)
      bins <- unique(bins[bins >= 0])
    }
    
    pal <- colorBin("YlOrRd", domain = merged$GroupParticipants, bins = bins, na.color = "#f0f0f0")
    
    leaflet(merged) %>%
      addProviderTiles("CartoDB.Positron") %>%
      setView(lng = -79.0, lat = 37.5, zoom = 7) %>%
      addPolygons(
        fillColor = ~pal(GroupParticipants),
        color = "black", weight = 1,
        fillOpacity = 0.7,
        label = lapply(merged$label_participant, htmltools::HTML),
        highlightOptions = highlightOptions(weight = 2, color = "#666",
                                            fillOpacity = 0.9, bringToFront = TRUE)
      ) %>%
      addLegend("bottomright", pal = pal,
                values = merged$GroupParticipants,
                title = "Participants (Selected Group)",
                opacity = 1)
  })
  
  # === VCE VOLUNTEERS VS PARTICIPANTS MAP ===
  output$vce_ratio_map <- renderLeaflet({
    tryCatch({
      pears_bi_df <- read_excel(get_data_path("PEARS_BI_with_NewVolunteers.xlsx")) %>%
        mutate(
          County = tolower(County),
          County = gsub(" county", "", County),
          County = gsub(" city", " (city)", County),
          County = trimws(County),
          Volunteers = NewVolunteers,
          volunteers_pct_participants = ifelse(Participants > 0,
                                               round(Volunteers / Participants * 100, 2),
                                               NA)
        )
      
      merged <- left_join(va_counties, pears_bi_df, by = "County") %>%
        st_as_sf() %>%
        mutate(label_vce = paste0(
          "<strong>", toupper(County), "</strong><br>",
          "Volunteers: ", ifelse(is.na(Volunteers), 0, Volunteers), "<br>",
          "Participants: ", ifelse(is.na(Participants), 0, Participants), "<br>",
          "Volunteers as % of Participants: ", ifelse(is.na(volunteers_pct_participants), "N/A", paste0(volunteers_pct_participants, "%"))
        ))
      
      valid_values <- merged$volunteers_pct_participants[!is.na(merged$volunteers_pct_participants) & merged$volunteers_pct_participants > 0]
      
      if(length(valid_values) == 0) {
        pal <- colorBin("YlOrRd", domain = c(0, 1), bins = 2, na.color = "#d9d9d9")
      } else {
        pal <- colorBin("YlOrRd", domain = merged$volunteers_pct_participants,
                        bins = 5, na.color = "#d9d9d9")
      }
      
      leaflet(merged) %>%
        addProviderTiles("CartoDB.Positron") %>%
        setView(lng = -79.0, lat = 37.5, zoom = 7) %>%
        addPolygons(
          fillColor = ~pal(volunteers_pct_participants),
          color = "black", weight = 1, fillOpacity = 0.7,
          label = lapply(merged$label_vce, htmltools::HTML),
          highlightOptions = highlightOptions(weight = 2, color = "#666",
                                              fillOpacity = 0.9, bringToFront = TRUE)
        ) %>%
        addLegend("bottomright", pal = pal,
                  values = merged$volunteers_pct_participants,
                  title = "% Volunteers of Participants",
                  opacity = 1)
      
    }, error = function(e) {
      # Fallback map if data loading fails
      leaflet(va_counties) %>%
        addProviderTiles("CartoDB.Positron") %>%
        setView(lng = -79.0, lat = 37.5, zoom = 7) %>%
        addPolygons(
          fillColor = "#d9d9d9",
          color = "black", weight = 1, fillOpacity = 0.3,
          label = "No data available"
        )
    })
  })
}

# === RUN APP ===
shinyApp(ui = ui, server = server)