library(shiny)
library(readr)
library(bslib)
library(leaflet)
library(dplyr)
library(DT)
library(sf)
library(tigris)
library(shinythemes)
library(ggplot2)

# Load your data
data = readRDS("ITA_FIPS.rds")

# Load shapefiles
counties_sf = counties(cb = TRUE, resolution = "20m", class = "sf", year = 2020)%>%
  st_transform(crs = 4326) %>%
  st_simplify(dTolerance = 500)

# UI
ui = fluidPage(theme = shinytheme("sandstone"),
               titlePanel("Workplace Injuries by State (2023)"),
               sidebarLayout(
                 sidebarPanel(
                   width = 3,
                   h4("Filter Options"),
                   selectInput("state", 
                               "Select State to start:",
                               choices = c("", sort(unique(data$STATE))),
                               selected = ""),
                   selectInput("county", 
                               "Select County (optional):",
                               choices = c("All Counties" = "all"),
                               selected = "all"),
                   selectInput("industry", "Filter by Industry (optional):", 
                               choices = c("All Industries" = "all"),
                               selected = "all"),
                   hr(),
                   a("See full US dashboard", href = "https://019ab75a-5c50-98f0-9bf7-4dfef07c5131.share.connect.posit.cloud/", target = "_blank"),
                   br(),
                   wellPanel(
                     strong("Rate Calculation per Establishment:"),
                     "(Count of injuries and illnesses X 200,000) / Employee hours worked reported to ITA = Incidence Rate"
                   ),
                   br(),
                   wellPanel(
                     strong("Data Source:"),
                     "This dashboard is based on the 2023 OSHA Injury Tracking Application (ITA) data from large employers (100+ employees).",
                     a(" View data", href = "https://www.osha.gov/Establishment-Specific-Injury-and-Illness-Data", target = "_blank")
                   ),
                   br(),
                   p(em("For questions, contact Alia Jamil (ajamil@gwu.edu)"))
                   ),
                 
                 mainPanel(
                   tabsetPanel(
                     tabPanel("County-level Map", 
                              leafletOutput("injury_map", height = "700px")),
                     tabPanel("Injury Details",
                              DTOutput("narrative_table"),
                              br(),
                              helpText("Download this data and more detailed information, including all injury narratives, occupation, and company name."),
                              downloadButton("download_narratives", "Download Full Data", class = "btn-primary"))))
               ))
#server
server <- function(input, output, session) {
  
  #county choices 
  observeEvent(input$state, {
    if (!is.null(input$state)) {
      choices = data %>%
        filter(STATE == input$state) %>%
        distinct(COUNTYNAME) %>%
        arrange(COUNTYNAME) %>%
        pull(COUNTYNAME)
    } else {choices = character(0)}
    updateSelectInput(
      session, "county",
      choices = c("All Counties" = "all", as.list(choices)),
      selected = "all"
    )
  })
  
  observeEvent({
    input$county
    input$state
  }, {

    if (!is.null(input$county) && input$county != "all") {
      choices2 = data %>%
        filter(COUNTYNAME == input$county) %>%
        distinct(naics_title_2digits) %>%
        arrange(naics_title_2digits) %>%
        pull(naics_title_2digits)
    } 
    else {
      choices2 = data %>%
        filter(STATE == input$state) %>%
        distinct(naics_title_2digits) %>%
        arrange(naics_title_2digits) %>%
        pull(naics_title_2digits)
    }
    
      updateSelectInput(
        session, "industry",
        choices = c("All Industries" = "all", as.list(choices2)),
        selected = "all"
      )
  })
  
  
  # Reactive data filtering
  filtered_data <- reactive({
    filtered = data
    
    if (!is.null(input$state) && input$state != "all") {
      filtered <- filtered %>% filter(STATE == input$state)
    }
    
    if (!is.null(input$county) && input$county != "all") {
      filtered <- filtered %>% filter(COUNTYNAME == input$county)
    }
    
    if (!is.null(input$industry) && input$industry != "all") {
      filtered <- filtered %>% filter(naics_title_2digits == input$industry)
    }
    
    filtered = filtered %>%
      group_by(establishment_name) %>%
      mutate(total_injuries = n(),
             incidence_calc = ifelse(total_hours_worked > 0, (total_injuries * 200000) / total_hours_worked, NA),
             incidence = round(incidence_calc, digits = 2)) %>%
      ungroup()
    
    
    
    return(filtered)
  })
  
  search_filter = reactive ({
    filtered = filtered_data()
    
    search_term = input$narrative_table_search
    
    if (!is.null(search_term) && search_term != "") {
      search <- tolower(search_term)
      
      search_filtered = filtered %>%
        filter(
          grepl(search, tolower(NEW_NAR_WHAT_HAPPENED)) |
            grepl(search, tolower(NEW_NAR_INJURY_ILLNESS)) |
            grepl(search, tolower(NEW_INCIDENT_DESCRIPTION)) |
            grepl(search, tolower(NEW_NAR_OBJECT_SUBSTANCE)) |
            grepl(search, tolower(NEW_NAR_BEFORE_INCIDENT)) |
            grepl(search, tolower(NEW_INCIDENT_LOCATION)) |
            grepl(search, tolower(zip_code)) |
            grepl(search, tolower(establishment_name))|
            grepl(search, tolower(company_name))
        )
    }
    
    return(search_filtered)
  })
  
  map_data <- reactive({
    agg_data = data
    if(!is.null(input$industry) && input$industry != "all") {
      agg_data = agg_data %>%
        filter(naics_title_2digits == input$industry)}
    
    agg_data = agg_data %>%
      filter(STATE == input$state)%>%
      group_by(GEOID, COUNTYNAME, STATE) %>%
      summarise(
        total_injuries = n(),
        .groups = "drop"
      )
      
    
    filtered_counties = counties_sf
    if (!is.null(input$state) && input$state != "all") {
      filtered_counties = counties_sf %>%
        filter(STUSPS == input$state)
    }
    
    joined_data = filtered_counties %>%
      left_join(agg_data, by = "GEOID")
    return(joined_data)
  })
  
  output$injury_map = renderLeaflet({
    req(input$state)
    map_df = map_data()
    
    # Color palette
    pal = colorNumeric(
      palette = "magma",
      domain = map_df$total_injuries,
      reverse = TRUE,
      na.color = "transparent"
    )
    
    # County-level labels
    labels = sprintf(
      "<strong>%s County</strong><br/>
        Total Injuries: %g<br/>",
      map_df$NAME,
      map_df$total_injuries
    ) %>% lapply(htmltools::HTML)
    
    
    map  = leaflet(map_df) %>%
      addTiles() %>%
      addProviderTiles("Esri.WorldGrayCanvas") %>%
      addPolygons(
        fillColor = ~pal(total_injuries),
        weight = ifelse(map_df$COUNTYNAME == input$county, 3, 1),
        opacity = 1,
        color = "#FFFFFF",
        fillOpacity = ifelse(map_df$COUNTYNAME == input$county, 1, 0.5),
        highlightOptions = highlightOptions(
          weight = 1.5,
          fillOpacity = 0.75,
          bringToFront = TRUE
        ),
        label = labels,
        labelOptions = labelOptions(
          style = list("font-weight" = "normal", padding = "3px 8px"),
          textsize = "15px",
          direction = "auto"
        )) %>%
      addLegend(
        pal = pal, 
        values = ~total_injuries,
        opacity = 0.7, 
        title = "Injury Count<br/>",
        position = "bottomright"
      )
    
    return(map)
    
  })
  
  # Narrative table + download
  output$narrative_table <- renderDT({
    
    filtered_nar = filtered_data()
    
    narratives <- filtered_nar %>%
      select(`Narrative Description` = NEW_NAR_WHAT_HAPPENED, 
             `County` = COUNTYNAME,
             `State` = STATE,
             `Zip Code` = zip_code,
             `Industry` = naics_title_2digits,
             `Establishment` = establishment_name,
             `Company` = company_name,
             `Establishment Incidence Rate (per 100 FTE)` = incidence)
    
    datatable(narratives, 
              options = list(pageLength = 10, scrollY = "400px"), 
              rownames = FALSE)
  }, server = TRUE)
  
  output$download_narratives <- downloadHandler(
    filename = function() {
      paste0("injuries", "_", gsub(" ", " ", input$industry),"_", gsub(" ", " ", input$county), ".csv")
    },
    content = function(file) {
      download_data = search_filter()
      
      narratives <- download_data %>%
        select(
          `Before Incident` = NEW_NAR_BEFORE_INCIDENT,
          `What Happened` = NEW_NAR_WHAT_HAPPENED,
          `Injury/Illness` = NEW_NAR_INJURY_ILLNESS,
          `Object/Substance` = NEW_NAR_OBJECT_SUBSTANCE,
          `Incident Description` = NEW_INCIDENT_DESCRIPTION,
          `Incident Location` = NEW_INCIDENT_LOCATION,
          `Occupation` = soc_description,
          `Address` = arcgis_address,
          `County` = COUNTYNAME,
          `State` = STATE,
          `Zip Code` = zip_code,
          `Industry` = naics_title_2digits,
          `Detailed Industry` = naics_title_6digits,
          `Establishment` = establishment_name,
          `Company` = company_name,
          `Establishment Incidence Rate (per 100 FTE)` = incidence
        )
      
      write.csv(narratives, file, row.names = FALSE)
    }
  )}

shinyApp(ui = ui, server = server)
