library(shiny)
library(shinythemes)
library(sf)
library(raster)
library(dplyr)
library(httr)
library(terra)
library(leaflet)
library(reshape2)
library(maptools)
library(spatstat)
library(gstat)
library(leaflet.extras)
library(rgdal)

setwd("H:\\CIMMYT Shiny project\\Data_2")

# Load country boundaries from GADM
gadm_sf <- st_read("Africa_countries.shp") %>% st_make_valid()

# Load farming system boundaries from FAO
fao_sf <- st_read("fao_gadm_intersect.shp") %>% st_make_valid()

# Load WorldPop population dataset
wp_path <- "H:\\CIMMYT Shiny project\\Data_2\\AFR_PPP_2020_adj_v2.tif"
worldpop_pop <- raster::brick(wp_path)

# Define the reclassification matrix
reclass_mat <- matrix(c(0, 10, 1,
                        10, 50, 2,
                        50, 100, 3,
                        100, 500, 4,
                        500, 1000, 5,
                        1000, 5000, 6,
                        5000, 10000, 7,
                        10000, 50000, 8,
                        50000, 100000, 9,
                        100000, Inf, 10), ncol = 3, byrow = TRUE)

# Reclassify the raster
worldpop_pop_reclass <- raster::reclassify(worldpop_pop, reclass_mat)

# create a new plotting device with larger size
dev.new(width = 8, height = 6)

# plot the raster with smaller margins
plot(worldpop_pop_reclass, 
     main="Africa Population", 
     col=rev(terrain.colors(10)), 
     legend=TRUE, 
     legend.args=list(at=seq(0,100000,10000), 
                      labels=prettyNum(seq(0,100000,10000), big.mark=","),
                      text="Population (thousands)"),
     mar=c(2,2,2,2) # Set smaller margins
)


# Load SPAM dataset
spam_path <- "H:\\CIMMYT Shiny project\\Data_2\\spam2010V2r0_global_Y_MAIZ_A.tif"
spam_yield <- raster::brick(spam_path)

reclass_mat_spam <- matrix(c(-Inf, 0, 0,
                             0, 1000, 1,
                             1000, 2000, 2,
                             2000, 4000, 3,
                             4000, 8000, 4,
                             8000, 16000, 5,
                             16000, Inf, 6), ncol = 3, byrow = TRUE)


# Reclassify the raster
spam_yield_reclass <- raster::reclassify(spam_yield, reclass_mat_spam)


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
                   label = "Select Country",
                   choices = unique(gadm_sf$ADM0_NAME)
                 )
               ),
               column(
                 width = 4,
                 selectInput(
                   inputId = "farming_system",
                   label = "Select Farming System",
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
             "Adapted from data provided by ",
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
               tags$h4("Background"), 
               "CGIAR is a global research partnership for a food-secure future. ",
               "CGIAR science is dedicated to reducing poverty, enhancing food and nutrition security, and improving natural resources and ecosystem services. ",
               "Its research is carried out by 15 CGIAR centers, including CIMMYT. ", 
               "Through CGIAR Research Programs (CRPs), Centers and partners work on integrated research programs, drawing on the expertise of other Centers and a multitude of partners.", 
               tags$br(),tags$br(),
               tags$h4("CIMMYT CRPs"), 
               "The CGIAR Research Program on Maize (MAIZE) focuses on increasing production for 900 million poor consumers in Africa, South Asia, and Latin America. Overarching goals include doubling maize productivity and increasing incomes and livelihood opportunities from sustainable, maize-based farming systems.",
               tags$br(),tags$br(),
               "The CGIAR Research Program on Wheat (WHEAT) couples advanced science with field-level research and extension in lower- and middle-income countries to raise the productivity, production and affordable availability of wheat agri-food systems for 2.5 billion resource-poor consumers in 89 countries.",
               tags$br(),tags$br(),
               tags$h4("Code"),
               "Code and input data used to generate this Shiny mapping tool are available on ",
               tags$a(href="https://github.com/Madaga-L/Rshiny-Agri-population-calculator", "Github."),
               tags$br(),tags$br(),
               tags$h4("Acknowledgements"),
               "This app was developed by CIMMYT as part of Ex Ante Evaluation Support to Prospective Agronomy Interventions."
             )
    )
  )
)

# Define Server
server <- function(input, output, session) {
  
  # Update farming system choices based on selected country
  observeEvent(input$country, {
    country_name <- input$country
    farming_system_choices <- unique(fao_sf$DESCRIPTIO[fao_sf$ADM0_NAME == country_name])
    updateSelectInput(session, "farming_system", choices = farming_system_choices)
  })
  
  # Calculate agricultural population and yield based on user selections
  observeEvent(input$calculate, {
    country_name <- input$country
    farming_system_name <- input$farming_system
    farming_system_geom <- fao_sf[fao_sf$DESCRIPTIO == farming_system_name & fao_sf$ADM0_NAME == country_name, ]
    
    country_geom <- gadm_sf[gadm_sf$ADM0_NAME == country_name, ]
    country_extent <- st_bbox(country_geom)
    farming_system_extent <- st_bbox(farming_system_geom)
    intersection_extent <- st_intersection(country_geom, farming_system_geom) %>%
      st_bbox()
    
    worldpop_pop_crop <- raster::crop(worldpop_pop_reclass, intersection_extent)
    spam_yield_crop <- raster::crop(spam_yield_reclass, intersection_extent)
    
    total_population <- sum(as.vector(values(worldpop_pop_crop)), na.rm = TRUE)
    total_yield <- sum(as.vector(values(spam_yield_crop)), na.rm = TRUE)
    
    output$result_table <- renderTable({
      data.frame("Total Population density within Agricultural Area" = total_population,
                 "Maize Yield within Agricultural Area(kg/ha)" = total_yield)
    })
    pal0 <- colorNumeric(c("RdYlBu"), values(worldpop_pop_crop),
                         na.color = "transparent")
    
    output$result_map <- renderLeaflet({
      farming_system_geom %>% 
        st_transform(crs = 4326) %>% 
        leaflet() %>% 
        addProviderTiles("CartoDB.Positron") %>% 
        addPolygons() %>% 
        addRasterImage(worldpop_pop_crop, colors = pal0,opacity = 0.4) %>%
        addLegend(pal = pal0, values = values(worldpop_pop_crop), 
                  title = "Population(thousands)",
                  position = "bottomright")
    })
  })
}

# Run the app
shinyApp(ui = ui, server = server)
