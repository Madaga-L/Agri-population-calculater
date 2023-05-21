# Load required packages
library(shiny)
library(leaflet)
library(sf)
library(dplyr)
library(tidyr)
library(readr)
library(raster)
library(terra)

# Load GADM data
gadm_path <- "H:\\CIMMYT Shiny project\\Data\\afr_g2014_2013_0.shp"
gadm_sf <- st_read(gadm_path)

# Load FAO Farming Systems data
fao_path <- "H:\\CIMMYT Shiny project\\Data\\Farm_sys.tif"
fao_raster <- raster(fao_path)

# Load WorldPop population data
wp_path <- "H:\\CIMMYT Shiny project\\Data\\AFR_PPP_2020_adj_v2.tif"
wp_data <- raster(wp_path)

# Load SPAM data
spam_path <- "H:\\CIMMYT Shiny project\\Data\\spam_dataset.tif"
spam_data <- raster(spam_path)

# Define UI for app
ui <- fluidPage(
  titlePanel("Population within agricultural extent"),
  sidebarLayout(
    sidebarPanel(
      selectInput("country", "Select a country:", choices = unique(gadm_sf$ADM0_NAME)),
      conditionalPanel(
        condition = "input.country != null",
        selectInput("farming_system", "Select a farming system:",
                    choices = unique(fao_raster$DESCRIPTIO[fao_raster$DESCRIPTIO == input$country]))
      )
    ),
    mainPanel(
      leafletOutput("map"),
      textOutput("population")
    )
  )
)

# Define server logic
server <- function(input, output) {
  
  # Filter GADM and FAO data based on user selection
  selected_gadm <- reactive({
    gadm_sf %>%
      filter(NAME_0 == input$country)
  })
  
  selected_fao <- reactive({
    fao_sf %>%
      filter(ISO == input$country, FARM_SYS == input$farming_system)
  })
  
  # Create leaflet map
  output$map <- renderLeaflet({
    leaflet() %>%
      addTiles() %>%
      addPolygons(data = selected_gadm(), fillColor = "lightblue", fillOpacity = 0.5) %>%
      addPolygons(data = selected_fao(), fillColor = "orange", fillOpacity = 0.5)
  })
  
  # Calculate population within agricultural extent
  output$population <- renderText({
    if (!is.null(input$country) & !is.null(input$farming_system)) {
      # Filter SPAM data based on user selection
      spam_data_sel <- spam_data %>%
        filter(ISO3 == input$country, farm_sys == input$farming_system)
      
      # Calculate total agricultural area
      total_area <- sum(spam_data_sel$area)
      
      # Calculate population within agricultural extent
      wp_data_sel <- wp_data %>%
        filter(iso3 == input$country) %>%
        left_join(spam_data_sel, by = c("lon" = "lon", "lat" = "lat")) %>%
        filter(!is.na(area)) %>%
        summarise(population = sum(wp) * sum(area) / total_area)
      
      # Return formatted population estimate
      paste("Population within agricultural extent:", round(wp_data_sel$population))
    }
  })
}

# Run the app
shinyApp(ui = ui, server = server)
