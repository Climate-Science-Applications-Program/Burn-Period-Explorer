#
# Web app to download RAWS and plot Burn Period time series plots 
# for stations in the U.S.
#
# 
#

library(shiny)
library(leaflet)
library(dplyr)
library(plotly)
library(bslib)

#####
# Custom CSS for spinning wheel
spinner_css <- "
#spinner {
  border: 16px solid #f3f3f3;
  border-radius: 50%;
  border-top: 16px solid #3498db;
  width: 60px;
  height: 60px;
  -webkit-animation: spin 2s linear infinite;
  animation: spin 2s linear infinite;
  margin: auto;
}

.modal-body {
  display: flex;
  flex-direction: column;
  justify-content: center;
  align-items: center;
  height: 200px;
}

@-webkit-keyframes spin {
  0% { -webkit-transform: rotate(0deg); }
  100% { -webkit-transform: rotate(360deg); }
}

@keyframes spin {
  0% { transform: rotate(0deg); }
  100% { transform: rotate(360deg); }
}
"
#####

source("getRAWSdata.R")
source("calcBP.R")
source("generateBPplot.R")
source("climoBPplots.R")

# Load location data from the CSV file
locations <- read.csv("RAWSfw13list-gapfilled.csv")

# Rename columns for easier use
locations <- locations %>%
  rename(lat = LatDegrees, lng = LonDegrees, name = Name, elevation = Elevation)

# Define the UI
ui <- fluidPage(
  # set theme
  theme = bs_theme(bootswatch = "journal"),
  # Add the logo at the top of the page
  tags$div(
    style = "display: flex; align-items: center; justify-content: space-between; margin-bottom: 20px;",
    tags$h1("Wildfire Burn Period Explorer"),
    tags$img(src = "UAlogo.jpg", height = "60px")  # Adjust the height as needed
  ),
  # Footer with logo
  tags$div(
    style = "position: fixed; bottom: 0; width: 100%; text-align: center; padding: 10px; background-color: #f8f9fa;",
    tags$img(src = "BP_app_logos.png", height = "70px") # Adjust src to point to your logo file and set the desired height
  ),
  # add css for spinnger
  tags$head(tags$style(HTML(spinner_css))),  # Include the custom spinner CSS
  # CSS to make the map take full width
  #tags$style(type = "text/css", "#map {height: 700px;}"),
  # set page title
  # titlePanel("Wildfire Burn Period Plot Generator"),
  
  # Add tabs to the app
  tabsetPanel(
    id = "tabs",
    # Tab 1: Map
    tabPanel("Map", 
             sidebarLayout(
               sidebarPanel(
                 selectizeInput("selected_location", 
                                "Choose a location and then click Download Data:", 
                                choices = locations$name,
                                options = list(placeholder = 'Type or select a location')),
                 br(),
                 actionButton("execute_btn", "Download Data"),  # <-- New: Add action button
                 br(),
                 br(),
                 p("(Downloading large dataset: This will take several minutes.)")
               ),
               mainPanel(
                 leafletOutput("map", width = "100%", height = "500px")
               )
             )
    ),
    
    # Tab 2: Plot
    tabPanel("Plots",
             sidebarLayout(
               sidebarPanel(
                 selectizeInput("selected_year", 
                                "Choose a year:", 
                                choices = seq(2000,as.numeric(format(Sys.Date(),"%Y")),1),
                                options = list(placeholder = 'Type or select a year'),
                                selected = as.numeric(format(Sys.Date(),"%Y"))),
                 br(),
                 radioButtons( 
                   inputId = "bpThreshold", 
                   label = "Relative Humidity Threshold", 
                   choices = list( 
                     "10%" = 10, 
                     "15%" = 15,
                     "20%" = 20,
                     "25%" = 25,
                     "30%" = 30
                   ),
                   selected = 20
                 ),
               ),
               mainPanel(
                 plotlyOutput("plot"),
                 p("Climatology represents daily smoothed mean (dark grey line) and range of values between 5th and 95th percentiles (light grey bars)."),
                 br(),
                 plotlyOutput("plot3"),
                 br(),
                 plotlyOutput("plot2")
               )
             )
    ),
    
    # Tab 3: About
    tabPanel("About", 
             fluidRow(
               column(12, 
                      h5("About the Explorer"),
                      p("This app helps visualize and explore Burn Period (hours/day with relative humidity below threshold) values at weather stations across the U.S. By using hourly RAWS (Remote Automatic Weather Station) data, you can explore burn periods based on different relative humidity thresholds."),
                      h5("Data Source"),
                      p("Data is downloaded from both the gap-filled RAWS data at DRI-CEFA (2000-2022, https://cefa.dri.edu/raws/) and WIMS (2023-present, https://www.wildfire.gov/application/wims), which provides a long and complete period of record to allow for climatological analyses of Burn Period values."),
                      h5("How to Use the App"),
                      tags$ul(
                        tags$li(tags$b("Interactive Map"), ": Use the interactive map to explore different station locations. Click on a station or use the dropdown list to select a specific site."),
                        tags$li(tags$b("Downloading Data"), ": After selecting a station, click \"Download Data\" to fetch the historical data for the selected station. This will take several minutes."),
                        tags$li(tags$b("Generating Plots"), ": In the 'Plots' tab, choose a year and relative humidity threshold to generate burn period time series plots. These plots help visualize the burn period and relative humidity trends for the selected location and year.")
                      ),
                      h5("Features"),
                      tags$ul(
                        tags$li(tags$b("Burn Period Analysis"), ": Generate burn period plots based on weather data from selected RAWS stations."),
                        tags$li(tags$b("Interactive Plots"), ": Created using Plotly, these plots allow you to zoom and explore data points in detail and download and save plots."),
                        tags$li(tags$b("Relative Humidity Threshold"), ": Adjust the relative humidity (RH) threshold to see its impact on burn periods.")
                      ),
                      h5("Use Cases"),
                      p("This app can be used by wildfire managers, researchers, and decision-makers to assess Burn Periods across different regions. It is also helpful for assessing historical conditions and the evolution of fire seasons in past years."),
                      h5("Acknowledgments"),
                      p("We acknowledge the Program for Climate, Ecosystem and Fire Applications at the Desert Research Institute for providing the gap-filled RAWS data critical in calculating Burn Period climatologies."),
                      h5("Contact Information"),
                      p("If you have questions or feedback, please contact Mike Crimmins (crimmins@arizona.edu)."),
                      h5("App Code"),
                      tags$a(href='https://github.com/Climate-Science-Applications-Program/Burn-Period-Explorer', 'https://github.com/Climate-Science-Applications-Program/Burn-Period-Explorer')
               )
             )
    )
  )
)

# Define the server logic
server <- function(input, output, session) {
  
  # Create a reactive value to store the selected station ID
  selected_stationID <- reactiveVal(NULL)
  
  # Render Leaflet map with markers
  output$map <- renderLeaflet({
    leaflet() %>%
      addTiles() %>%
      setView(lng = -114.3275, lat = 34.130833, zoom = 6) %>%
      addCircleMarkers(data = locations,
                       lng = ~lng, lat = ~lat,
                       layerId = ~name,
                       radius = 3,
                       color = "red",
                       fillOpacity = 0.5)
  })
  
  # Update map and show popup when a location is selected from the dropdown
  observeEvent(input$selected_location, {
    loc <- filter(locations, name == input$selected_location)
    if (nrow(loc) > 0) {
      selected_stationID(loc$StationID)  # Update reactive variable with selected StationID
      leafletProxy("map") %>%
        clearPopups() %>%
        setView(lng = loc$lng, lat = loc$lat, zoom = 6) %>%
        addPopups(lng = loc$lng, lat = loc$lat,
                  popup = paste0("<b>", loc$name, "</b><br>Elevation: ", loc$elevation, " feet",
                                 "<br>ID: ", loc$StationID))
    }
  })
  
  # Update dropdown and show popup when a map marker is clicked
  observeEvent(input$map_marker_click, {
    click <- input$map_marker_click
    updateSelectizeInput(session, "selected_location", selected = click$id)
    
    loc <- filter(locations, name == click$id)
    if (nrow(loc) > 0) {
      selected_stationID(loc$StationID)  # Update reactive variable with clicked StationID
      leafletProxy("map") %>%
        clearPopups() %>%
        addPopups(lng = loc$lng, lat = loc$lat,
                  popup = paste0("<b>", loc$name, "</b><br>Elevation: ", loc$elevation, " meters"))
    }
  })
  
  # Example: Generate a plot based on selected station data (you can modify as needed)
  # output$plot <- renderPlot({
  #   hist(locations$elevation, breaks = 20, col = "lightblue",
  #        main = "Distribution of Station Elevations",
  #        xlab = "Elevation (feet)")
  # })
  
  # Example: Print the selected station ID to the console 
  observe({
    station_id <- selected_stationID()
    if (!is.null(station_id)) {
      print(paste("Selected Station ID:", station_id))
    }
  })
  
  # Show a modal message while the script is running
  observeEvent(input$execute_btn, {
    
    ##### DOWNLOADING DATA MODALS #####
    # showModal(modalDialog(
    #   title = "Please wait...",
    #   "Downloading RAWS data. This will take several minutes.",
    #   footer = NULL,
    #   easyClose = FALSE
    # ))
    
    showModal(modalDialog(
      title = "Please wait...",
      div(class = "modal-body",
          tags$div(id = "spinner"),  # Spinner element
          tags$p("Downloading RAWS data -- This will take a few minutes")   # Message below the spinner
      ),
      footer = NULL,
      easyClose = FALSE
    ))
    
    # Use progress bar (indeterminate)
    #withProgress(message = 'Downloading data...', value = NULL, {
    # run download script
    station_id <- selected_stationID()
    rawsData<-get_RAWS_rh(station_id)
    #})
    # Once the script completes, remove the initial modal
    removeModal()
    
    # Show a completion message after the script finishes
    # showModal(modalDialog(
    #   title = "Success!",
    #   "Data downloaded -- close to view plot",
    #   easyClose = TRUE,
    #   footer = NULL
    #   #footer = modalButton("Close")
    # ))
    
    showModal(modalDialog(
      title = "Success!",
      "Data downloaded -- close to view plot",
      footer = tagList(
        actionButton("close_modal", "Close")
      )
    ))
    observeEvent(input$close_modal, {
      removeModal()
      updateTabsetPanel(session, "tabs", selected = "Plots")
    })
    #####
    
    
    # ##### INTERACTIVE BP PLOTTING #####
    # output$plot <- renderPlotly({
    #   selYr<-input$selected_year
    #   bpT<-input$bpThreshold
    #   # generate the plots
    #   bpData<-calc_bp(rawsData,bpT) # make threshold selection interactive
    #   bpPlots<-make_BP_plots(bpData, selYr)
    #   bpPlots[[2]]
    # })
    # #####
    # 
    # ##### INTERACTIVE BP PLOTTING #####
    # output$plot2 <- renderPlotly({
    #   bpT2<-input$bpThreshold
    #   bpData<-calc_bp(rawsData,bpT2) # make threshold selection interactive
    #   # generate the plots
    #   climPlot<-make_climo_plots(rawsData,bpData,as.numeric(bpT2))
    #   climPlot
    # })
    # #####
    
    observe({
      # monitor for changes in UI   
      selYr<-input$selected_year
      bpT<-input$bpThreshold
      # generate the plots
      bpData<-calc_bp(rawsData,bpT) # make threshold selection interactive
      bpPlots<-make_BP_plots(bpData, selYr)
      climPlots<-make_climo_plots(rawsData,bpData,as.numeric(bpT))
      
      # render BP plot
      output$plot <- renderPlotly({
        bpPlots[[2]]
      })
      # render RH plot
      output$plot2 <- renderPlotly({
        climPlots[[1]]
      })
      # render RH plot
      output$plot3 <- renderPlotly({
        climPlots[[2]]
      })
    })
  })
  
}

# Run the application 
shinyApp(ui = ui, server = server)
