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
data = read_csv("ITA_FIPS.csv")

# Load shapefiles
counties_sf = counties(cb = TRUE, resolution = "20m", class = "sf", year = 2020)%>%
  st_transform(crs = 4326) %>%
  st_simplify(dTolerance = 500)

# UI
ui = fluidPage(theme = shinytheme("sandstone"),
  titlePanel("County-Level Map of Workplace Injuries (2023)"),
  sidebarLayout(
    sidebarPanel(
    width = 3,
    h4("Click a County to start"),
    selectInput("industry", "Filter by Industry (optional):", 
                  choices = c("All Industries" = "all", sort(unique(data$naics_title_2digits))),
                  selected = "all"),
    hr(),
    wellPanel(
      a("See full US dashboard", href = "https://ajamil.shinyapps.io/US_dashboard/", target = "_blank")
    ),
    wellPanel(
      strong("Rate Calculation per Establishment:"),
      "(Count of injuries and illnesses X 200,000) / Employee hours worked reported to ITA = Incidence Rate"
    ),
    br(),
    wellPanel(
      strong("Data Source:"),
      "This dashboard is based on the 2023 OSHA Injury Tracking Application (ITA) data from large employers (100+ employees).",
      a(" View data", href = "https://www.osha.gov/Establishment-Specific-Injury-and-Illness-Data", target = "_blank")
    )),
  
    mainPanel(
      tabsetPanel(
        tabPanel("County-level Map", 
                 leafletOutput("injury_map", height = "700px")),
        tabPanel("Injury Details",
                 DTOutput("narrative_table"),
                 br(),
                 helpText("Download this data and more detailed information, including all injury narratives, occupation, and company name."),
                 downloadButton("download_narratives", "Download Detailed Data", class = "btn-primary"))))
  ))

#server
server <- function(input, output, session) {
  
  labels = reactive({ 
    map_df = map_data()
    sprintf(
      "<strong>%s County,</strong><br/>
      <strong>%s</strong><br/>
        Total Injuries: %g",
      map_df$NAME,
      map_df$STATE_NAME,
      map_df$total_injuries
    ) %>% lapply(htmltools::HTML)})
  
  
  map_data <- reactive({
    agg_data = data
    if(!is.null(input$industry) && input$industry != "all") {
      agg_data = agg_data %>%
        filter(naics_title_2digits == input$industry)}
    
      agg_data = agg_data %>%
        group_by(GEOID, COUNTYNAME, STATE) %>%
        summarise(
          total_injuries = n(),
          .groups = "drop"
        )
      
    joined_data = counties_sf %>%
        left_join(agg_data, by = "GEOID")
      return(joined_data)
    })
  
  selectedcounty = reactiveVal(NULL)

  output$injury_map = renderLeaflet({
    map_df = map_data()
    
    popup_text = sprintf(
      "<strong>%s County,</strong><br/>
    <strong>%s</strong><br/>
    Total Injuries: %g",
      map_df$NAME,
      map_df$STATE_NAME,
      map_df$total_injuries
    )
    
    # Color palette
    pal = pal = colorNumeric(
      palette = "magma",
      domain = log1p(map_df$total_injuries),
      na.color = "transparent",
      reverse = TRUE
    )
    
    
    map  = leaflet(map_df) %>%
      setView(-98.7, 39.8, zoom = 4) %>%
      addTiles() %>%
      addProviderTiles("Esri.WorldGrayCanvas") %>%
      addPolygons(
        layerId = ~GEOID,
        fillColor = ~pal(log1p(total_injuries)),
        weight = 1,
        opacity = 1,
        color = "#FFFFFF",
        fillOpacity = 0.5,
        highlightOptions = highlightOptions(
          weight = 1.5,
          fillOpacity = 1,
          bringToFront = TRUE
        ),
        popup = popup_text,
        label = labels(),
        labelOptions = labelOptions(
          style = list("font-weight" = "normal", padding = "3px 8px"),
          textsize = "15px",
          direction = "auto"
        )) %>%
      addLegend(
        pal = pal, 
        values = ~log1p(total_injuries),
        opacity = 0.7, 
        title = "Injury Count",
        position = "bottomright",
        labFormat = labelFormat(transform = function(x) round(exp(x) - 1))
      )
    
    return(map)
    
  })
  

 
  observeEvent(input$injury_map_shape_click, {
    tryCatch({
      click <- input$injury_map_shape_click
      clicked_geoid <- click$id
      
      selectedcounty(clicked_geoid)
      
    }, error = function(e) {
      cat("Click error:", e$message, "\n")
    })
  })
  
  
  filtered_data = reactive({
    filtered = data
    
    
    if (!is.null(selectedcounty()) && selectedcounty() != ""){
      filtered <- filtered %>% filter(GEOID == selectedcounty())
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
             `Establishment Incidence Rate (per 100 FTE)` = incidence)
    
    datatable(narratives, 
              options = list(pageLength = 10, scrollY = "400px"), 
              rownames = FALSE)
  })
  
  output$download_narratives <- downloadHandler(
    filename = function() {
      paste0("injuries", gsub(" ", "_", input$industry),"_", gsub(" ", " ", input$county), ".csv")
    },
    content = function(file) {
      download_data = filtered_data()
      
      narratives <- download_data %>%
        select(`Before Incident` = NEW_NAR_BEFORE_INCIDENT,
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
              `Establishment Incidence Rate (per 100 FTE)` = incidence)
      
      write.csv(narratives, file, row.names = FALSE)
    }
  )}

shinyApp(ui = ui, server = server)
