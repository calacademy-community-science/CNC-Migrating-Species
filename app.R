# This app shows a select number of species and their global GBIF occurrences

# Install required packages if not already installed
# install.packages(c("shiny", "leaflet", "arrow", "dplyr"))

library(shiny)
library(leaflet)
library(dplyr)
library(duckdb)
library(terra)
library(viridis)
library(dbplyr)
library(tictoc)
library(tidyr)

con <- dbConnect(duckdb())
con |> dbExecute("
CREATE VIEW occ AS
SELECT *
FROM PARQUET_SCAN('occurrences.parquet');")

# Define UI
ui <- fluidPage(
  titlePanel("Target Migrating Species Map"),
  
  sidebarLayout(
    sidebarPanel(
      checkboxGroupInput("selected_species", "Select Species:",
                         choices = NULL, 
                         selected = NULL, 
                         inline = FALSE
      ),
      actionButton("submitBtn", "Update species"), 
      hr(),
      radioButtons("data_source", "Data Source:",
                   choices = list("All GBIF" = "all", "Only iNaturalist" = "iNaturalist"),
                   selected = "all"
      ),
      # hr(),
      # sliderInput("coord_uncertainty", "Maximum Coordinate Uncertainty (m):",
      #             # Can't do more than a max of about 750
      #             min = 50, max = 500, value = 300),
      width = 3
    ),
    mainPanel(
      leafletOutput("occurrencesMap", height = 600)
    )
  )
)

# Define server logic
server <- function(input, output, session) {
  occurrences <- con |>
    tbl("occ") |>
    mutate(
      latitude = round(decimallatitude, 1),
      longitude = round(decimallongitude, 1)
    ) |>
    filter(coordinateuncertaintyinmeters < 300 |
             is.na(coordinateuncertaintyinmeters),
           !is.na(decimallatitude),
           !is.na(decimallongitude)) |>
    count(longitude, latitude, species,
          institutioncode, coordinateuncertaintyinmeters) |>
    # drop_na() |> 
    mutate(n = log(n))# |>
    # collect() |>
    # na.omit()
  
  # Eventually use duckdb for filtering, but reactive doens't work with sql clearly
  # con |> dbDisconnect(shutdown = T)
  
  species_list <- c(
    "Tiger Shark" = "Galeocerdo cuvier",
    "Leatherback Turtle" = "Dermochelys coriacea",
    "Monarch Butterfly" = "Danaus plexippus",
    "Lesser Flamingo" = "Phoeniconaias minor",
    "Osprey" = "Pandion haliaetus",
    "Common swift" = "Apus apus"
  )
  
  updateCheckboxGroupInput(session, "selected_species",
                           choices = species_list,
                           selected = species_list
  )
  
  selected_species <- reactiveVal(species_list)
  
  # Render the leaflet map immediately when the app starts
  output$occurrencesMap <- renderLeaflet({
    leaflet() |>
      addProviderTiles("CartoDB.Positron")
  })
  

  
  # Update selected species when submit button is clicked
  observeEvent(input$submitBtn, {
    print("should not be running")
    selected_species(input$selected_species)
  })
  
  filtered_occurrences <- reactive({
    tic()
    req(selected_species(), input$data_source)
    print('filtering')
    
    # can do duckdb sql filtering when rective value is assigned to variable
    specs <- selected_species()
    # filtered_data <- occurrences |>
    #   filter(species %in% specs) |>
    #   # collect() |>
    #   na.omit()
    # 
    # if (input$data_source == "iNaturalist") {
    #   filtered_data <- filtered_data |> filter(institutioncode == "iNaturalist")
    # }
    
    # Attempt to use duckdb filtering instead, but it seems slower
    if (input$data_source == "iNaturalist") {
      filtered_data <- occurrences |>
        filter(species %in% specs,
               institutioncode == "iNaturalist") |>
        collect() #|> na.omit()
    } else {
      filtered_data <- occurrences |>
        filter(species %in% specs) |>
        collect() #|> na.omit()
    }
    
    
    print('finished filtering')
    toc()
    filtered_data 
    
  })
  
  rast_data <- reactive({
    req(filtered_occurrences())
    print('making rast')
    # occ <- filtered_occurrences()
    # print('hm')
    # occrast <- rast(occ, crs = "epsg:4326")
    # print('projecting')
    # # specraster <- terra::project(occrast, "epsg:3857")
    # specraster <- occrast
    # print("hmmm")
    specraster <- filtered_occurrences() |>
      rast(crs = "epsg:4326")
    
    print('finished rast')
    specraster
  })
  
  pal_domain <- reactive({
    req(rast_data())
    print('making pal_domain')
    rast_data()[["n"]] |> values() |> 
      as.integer() |> unique()
  })
  
  pal <- reactive({
    req(pal_domain())
    print('making pal')
    colorNumeric(palette = viridis(1000), domain = pal_domain(), na.color = NA)
  })
  
  res_in_km <- reactive({
    req(rast_data())
    print('making res_in_km')
    (res(rast_data())[1] |> round(-3)) / 1000
  })
  
  # Function to update the map
  observe({
    req(rast_data(), pal_domain(), res_in_km(), pal())
    print("mapping proxy")
    print(paste(object.size(rast_data()[["n"]]) / (1024^2), "MB"))
    # print(rast_data())
    # print(pal_domain())
    # print(res_in_km())
    # print(pal())
    leafletProxy("occurrencesMap") |> 
      clearImages() |> 
      clearControls() |>
      addRasterImage(rast_data()[["n"]], colors = pal()) |> 
      addLegend(pal = pal(), values = pal_domain(), opacity = 0.8,
                title = sprintf("Count / %s", 
                                "12km"
                                # res_in_km()
                                ),
                position = "bottomright")
  })
}

# Run the app
shinyApp(ui, server)

