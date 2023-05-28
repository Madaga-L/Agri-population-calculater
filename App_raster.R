library(shiny)
library(shinythemes)
library(sf)
library(dplyr)
library(rgdal)
library(terra)
library(raster)
library(leaflet)
library(leaflet.extras)

# Set working directory
setwd("H:\\CIMMYT Shiny project\\Data_2")

# Preprocess and store transformed spatial datasets
gadm_sf <- readRDS("gadm_sf.rds")
fao_sf <- readRDS("fao_sf.rds")
worldpop_pop <- readRDS("worldpop_pop.rds")
agext <- readRDS("agext.rds")

#Define the UI
ui <- bootstrapPage(
  navbarPage(
    theme = shinytheme("flatly"), collapsible = TRUE, id = "nav",
    HTML('<a style="text-decoration:none;cursor:default;color:#FFFFFF;" class="active" href="#">Agri population mapper</a>'),
    windowTitle = "Agricultural population calculator",
    tabPanel("Calculate Population",
             fluidRow(
               column(
                 width = 4,
                 selectInput(
                   inputId = "country",
                   label = tags$span("Select Country", 
                                     style = "cursor: help;", 
                                     title = "Select the country for which you want to calculate agricultural population. The data available includes information on the farming system and the population density of different administrative levels."),
                   choices = unique(gadm_sf$ADM0_NAME)
                 )
               ),
               column(
                 width = 4,
                 selectInput(
                   inputId = "farming_system",
                   label = tags$span("Select Farming System", 
                                     style = "cursor: help;", 
                                     title = "Select the farming system for which you want to calculate agricultural population. The available options depend on the country selected."),
                   choices = NULL
                 )
               ),
               column(
                 width = 4,
                 actionButton(
                   inputId = "calculate",
                   label = "Calculate Population"
                 )
               )
             ),
             hr(),
             h3("Results"),
             tableOutput(outputId = "result_table"),
             leafletOutput(outputId = "result_map")
    ),
    
    tabPanel("Data",
             numericInput(inputId = "maxrows", label = "Rows to show", value = 25),
             "This tool uses data provided by ",
             tags$a(href = "https://gadm.org/data.html", "GADM"),
             ", ",
             tags$a(href = "http://www.fao.org/geonetwork/srv/en/main.home", "FAO"),
             ", ",
             tags$a(href = "https://www.worldpop.org/", "WorldPop"),
             ", and ",
             tags$a(href = "https://data.apps.fao.org/catalog/iso/59f7a5ef-2be4-43ee-9600-a6a9e9ff562a", "SPAM"),
             "."
    ),
    
    tabPanel("About this site",
             tags$div(
               tags$h4("What is the Agricultural Population Mapper?"),
               "The Agricultural Population Mapper is a tool that allows you to estimate the population living in rural areas and engaged in different farming systems in a given country.",
               tags$br(), tags$br(),
               tags$h4("How can I use it?"),
               "To use the Agricultural Population Mapper, simply select the country and farming system you're interested in and click the 'Calculate Population' button. The tool will return a table and a map showing the population and population density of different administrative levels within the selected country and farming system.",
               tags$br(), tags$br(),
               tags$h4("Code"),
               "Code and input data used to generate this Shiny mapping tool are available on ",
               tags$a(href="https://github.com/Madaga-L/Agri-population-calculator", "Github."),
               tags$br(),tags$br(),
               tags$h4("Acknowledgements"),
               "This app was developed by CIMMYT as part of Ex Ante Evaluation Support to Prospective Agronomy Interventions."
             )
    )
  )
)

# Define Server
server <- function(input, output, session) {
  
  # Precompute and store the raster operations
  observeEvent(input$calculate, {
    country_name <- input$country
    farming_system_name <- input$farming_system
    farming_system_geom <- fao_sf[fao_sf$DESCRIPTIO == farming_system_name & fao_sf$ADM0_NAME == country_name, ]
    
    country_geom <- gadm_sf[gadm_sf$ADM0_NAME == country_name, ]
    farming_system_geom <- fao_sf[fao_sf$DESCRIPTIO == farming_system_name & fao_sf$ADM0_NAME == country_name, ]
    intersection_extent <- st_intersection(country_geom, farming_system_geom)
    
    ## Create a new raster object rr using the same resolution and extent as the worldpop_pop raster, but based on the geometry of the farming_system_geom.
    rr <- terra::rast(intersection_extent, resolution = res(worldpop_pop), ext = ext(worldpop_pop))
    
    ## Rasterize the farming system geometry to be used in masking
    farming_system_raster <- terra::rasterize(intersection_extent, field = "gridcode", rr, fun = "sum", overwrite = TRUE)
    
    # Load computed raster datasets
    worldpop_pop_mask <- terra::mask(worldpop_pop, farming_system_raster)
    spam_yield_resampled <- terra::resample(agext, rr)
    spam_yield_mask <- terra::mask(spam_yield_resampled, farming_system_raster)
    
    # Downsample the raster to a lower resolution
    downsampled <- aggregate(worldpop_pop_mask, fact = 10)  # Adjust the factor according to your needs
    
    # Set CRS for the downsampled raster
    terra::crs(downsampled) <- "+proj=longlat +datum=WGS84"
    
    # Display precomputed values in the result table
    output$result_table <- renderTable({
      total_population_sum <- sum(as.vector(worldpop_pop_mask), na.rm = TRUE)
      mean_population <- mean(as.vector(worldpop_pop_mask), na.rm = TRUE, FUN = mean)
      mean_yield <- mean(as.vector(spam_yield_mask), na.rm = TRUE, FUN = mean)
      
      data.frame("Total_Population" = total_population_sum,
                 "mean_Population" = mean_population,
                 "mean_SPAM_Yield" = mean_yield)
    })
    
    # Display precomputed raster in the result map
    output$result_map <- renderLeaflet({
      # Create color palette
      pal0 <- colorNumeric(c("RdYlBu"), na.omit(terra::values(downsampled)),
                           na.color = "transparent")
      
      # Create leaflet map
      leaflet() %>% 
        addTiles() %>% 
        addRasterImage(downsampled, colors = pal0, opacity = 0.9) %>%
        addLegend(pal = pal0, values = terra::values(downsampled), 
                  title = "Population",
                  position = "bottomright")
    })
  })
  
  # Update farming system choices based on selected country
  observeEvent(input$country, {
    country_name <- input$country
    farming_system_choices <- unique(fao_sf$DESCRIPTIO[fao_sf$ADM0_NAME == country_name])
    updateSelectInput(session, "farming_system", choices = farming_system_choices)
  })
}

# Run the app
shinyApp(ui = ui, server = server)
