library(shiny)
library(readr)
library(leaflet)
library(dplyr)
library(DT)
library(sf)
library(tigris)
library(shinythemes)


# Load your data
data = bind_rows(
  readRDS("ITA_FIPS_23.rds"),
  readRDS("ITA_FIPS_24.rds")
)

counties_sf = counties(cb = TRUE, resolution = "20m", class = "sf", year = 2020)%>%
  st_transform(crs = 4326) %>%
  st_simplify(dTolerance = 500)

counties_subset = counties_sf [, c(5,7:8)]
data = data %>%
  left_join(counties_subset, by = "GEOID")



# UI
ui = fluidPage(theme = shinytheme("sandstone"),
               titlePanel("OSHA ITA Case Detail Data (2023 and 2024)"),
      
               fluidRow(
                 column(3,
                        selectizeInput(
                          "industry",
                          "Select Sector(s)",
                          choices = c(sort(unique(data$naics_title_2digits))),
                          multiple = TRUE,
                          options = list(placeholder = "All Sectors")
                        )
                 ),
                 column(3,
                        selectizeInput(
                          "state",
                          "Select State(s)",
                          choices = c(sort(unique(data$STUSPS))),
                          multiple = TRUE,
                          options = list(placeholder = "Entire country")
                        )
                 ),
                 column(3,
                        selectizeInput(
                          "year",
                          "Select Year",
                          choices = c(sort(unique(data$year_filing_for))),
                          multiple = TRUE,
                          options = list(placeholder = "2023 and 2024")
                        )
                 ),
                 column(3,
                        p(strong("Click a County to select")))
               ),
              
               hr(),
               div(class = "text-center",
                   actionButton(
                     "clear_filters",
                     "Clear All Filters",
                     icon = icon("eraser"),
                     class = "btn-danger"
                   )
               ),
               br(),
    
                 mainPanel(width = 12,
                   uiOutput("active_filters"),
                   tabsetPanel(
                     tabPanel("Map and Table", 
                              leafletOutput("injury_map", height = "700px"),
                              br(),
                              helpText("Note(s): Table reflects 2023 and 2024 data; Map is 2023 only. Search feature filters the table only. Download this data and more detailed information, including all injury narratives, below."),
                              DTOutput("narrative_table"),
                              br(),
                              downloadButton("download_narratives", "Download Detailed Data", class = "btn-primary"),
                              hr(),
                              p(strong("Data Source:")),
                              p("This dashboard is based on the 2023 and 2024 OSHA Injury Tracking Application (ITA) data from large employers (100+ employees).",
                                a("The ITA Case Detail Dataset can be found here", href = "https://www.osha.gov/Establishment-Specific-Injury-and-Illness-Data", target = "_blank")),
                              br(),
                              p(em("For questions, contact Alia Jamil (ajamil@gwu.edu)"))),
                     tabPanel("Data Information",
                              fluidRow(
                                column(
                                  width = 8,
                                  h3("Data Information"),
                                  p("The data used in this dashboard is OSHA Injury Tracking Application data recorded on Form 300 and Form 301 
                                  from large employers (100+ employees) in 2023 and 2024. It is limited to what employers report.
Mining, industries exempt from routine OSHA record-keeping, low-hazard industries, commuting injuries,
federal agencies, state and local government in states with no OSHA plans, most occupational fatalities,
and businesses closed before the electronic reporting deadline are not required to report."
                                    ),
                                  p("Injury counts are the total injuries per geographic area. Establishment rates are calculated using: (Count
of injuries and illnesses X 200,000) / Employee hours worked reported to ITA = Incidence Rate. Rates are
based on reported data and may inaccurately represent injury burden."
                                  ),
                                  p("Data will download in CSV format, which can be opened and further explored in Excel, R, or other
software. An establishment is a single workplace. A company can have several establishments. Company names may not be standardized across the dataset."
                                  ),
                                  p("Currently, the map does not reflect 2024 data and is a work-in-progress. The table includes both years.")
                                )
                              )
                     ))))


#server
server = function(input, output, session) {
  
  selectedcounty = reactiveVal(character(0))
  
  observeEvent(input$injury_map_shape_click, {
    clicked_geoid = input$injury_map_shape_click$id
    current = selectedcounty()
    
    if (clicked_geoid %in% current) {
      selectedcounty(setdiff(current, clicked_geoid))
    } else {
      selectedcounty(c(current, clicked_geoid))
    }
  })
  
  county_name = reactive({
    req(selectedcounty())
    
    data %>%
      filter(GEOID %in% selectedcounty()) %>%
      distinct(NAMELSAD) %>%
      pull(NAMELSAD)
  })
  
  observeEvent(input$clear_filters, {
    selectedcounty(character(0))
    updateSelectizeInput(session, "industry", selected = character(0))
    updateSelectizeInput(session, "state", selected = character(0))
    #add year
  })
  
  output$active_filters = renderUI({
    tags$div(
      style = "background:#f8f9fa; padding:10px; border-radius:5px;",
      strong("Current Filters: "),
      tags$ul(
        if (length(selectedcounty()))
          tags$li(paste("County:", paste(county_name(), collapse = ", "))),
        if (length(input$industry))
          tags$li(paste("Sector:", paste(input$industry, collapse = ", "))),
        if (length(input$state))
          tags$li(paste("State:", paste(input$state, collapse = ", "))),
        if (length(input$year))
          tags$li(paste("Year:", paste(input$year, collapse = ", ")))
      )
    )
  })
  
  labels = reactive({ 
    map_df = map_data()
    sprintf(
      "<strong>%s</strong><br/>
      <strong>%s</strong><br/>
        Total Injuries: %g",
      map_df$NAMELSAD,
      map_df$STATE_NAME,
      map_df$total_injuries
    ) %>% lapply(htmltools::HTML)})
  
  
  map_data = reactive({
    agg_data = data
    if(!is.null(input$industry) && length(input$industry) > 0) {
      agg_data = agg_data %>%
        filter(naics_title_2digits %in% input$industry)}
    if (!is.null(input$state) && length(input$state)>0) {
      agg_data = agg_data %>% filter(STUSPS %in% input$state)
    }
    if (!is.null(input$year) && length(input$year)>0) {
      agg_data = agg_data %>% filter(year_filing_for %in% input$year)
    }
    
    agg_data = agg_data %>%
      group_by(GEOID) %>%
      summarise(
        total_injuries = n(),
        .groups = "drop"
      )
    
    joined_data = left_join(counties_sf, agg_data, by = "GEOID")
    
    return(joined_data)
  })
  
  
  output$injury_map = renderLeaflet({
    map_df = map_data()
    
    popup_text = labels()
    
    # Color palette
    pal = colorNumeric(
      palette = "magma",
      domain = log1p(map_df$total_injuries),
      na.color = "transparent",
      reverse = TRUE
    )
    
    map  = leaflet(map_df) %>%
      setView(-98.7, 39.8, zoom = 4) %>%
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
  
  
  filtered_data = reactive({
    filtered = data
    
    
    if (!is.null(selectedcounty()) && length(selectedcounty()) > 0){
      filtered = filtered %>% 
        filter(GEOID %in% selectedcounty())
    }
    
    if (!is.null(input$state) && length(input$state) > 0) {
      filtered = filtered %>% filter(STUSPS %in% input$state)
    }
    
    if (!is.null(input$industry) && length(input$industry) > 0) {
      filtered = filtered %>%
        filter(naics_title_2digits %in% input$industry)
    }
    
    if (!is.null(input$year) && length(input$year) > 0) {
      filtered = filtered %>%
        filter(year_filing_for %in% input$year)
    }
    
    filtered$naics_code = as.character(filtered$naics_code)
    
    filtered = filtered %>%
      group_by(establishment_name) %>%
      mutate(total_injuries = n(),
             incidence_calc = ifelse(total_hours_worked > 0, (total_injuries * 200000) / total_hours_worked, NA),
             incidence = round(incidence_calc, digits = 2)) %>%
      ungroup()
    
    
    return(filtered)
  })
  
  
  # Narrative table + download
  output$narrative_table = renderDT({
    
    filtered_nar = filtered_data()
    
    narratives = filtered_nar %>%
      select(`Year` = year_filing_for,
              `State` = STUSPS,
             `County` = NAMELSAD,
             `Zip Code` = zip_code,
             `Sector` = naics_title_2digits,
             `NAICS Code` = naics_code,
             `Company` = company_name,
             `Establishment` = establishment_name,
             `Narrative Description` = NEW_NAR_WHAT_HAPPENED,
             `Nature of Injury` = nature_title_pred,
             `Establishment Incidence Rate (per 100 FTE)` = incidence
             )
    
    datatable(narratives,
              filter = "top",
              options = list(pageLength = 10, scrollY = "400px"), 
              rownames = FALSE)
  }, server = TRUE)
  
  output$download_narratives = downloadHandler(
    filename = function() {
      paste0("OSHA_ITA_injuries.csv")

    },
    content = function(file) {
      filtered_rows = input$narrative_table_rows_all
      
      narratives = filtered_data() %>%
        select(
          `Year` = year_filing_for,
          `County` = NAMELSAD,
          `State` = STUSPS,
          `Zip Code` = zip_code,
          `Sector` = naics_title_2digits,
          `NAICS Code` = naics_code,
          `Establishment ID` = establishment_id,
          `Establishment` = establishment_name,
          `Company` = company_name,
          `Occupation` = soc_description,
          `Before Incident` = NEW_NAR_BEFORE_INCIDENT,
          `What Happened` = NEW_NAR_WHAT_HAPPENED,
          `Injury/Illness` = NEW_NAR_INJURY_ILLNESS,
          `Object/Substance` = NEW_NAR_OBJECT_SUBSTANCE,
          `Incident Description` = NEW_INCIDENT_DESCRIPTION,
          `Incident Location` = NEW_INCIDENT_LOCATION,
          `Nature of Injury` = nature_title_pred,
          `Part of Body` = part_title_pred,
          `Event or Exposure` = event_title_pred,
          `Source` = source_title_pred,
          `Establishment Incidence Rate (per 100 FTE)` = incidence)
      
      write.csv(narratives[filtered_rows, ], file, row.names = FALSE)
    })
  } 

shinyApp(ui = ui, server = server)