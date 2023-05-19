library(shiny)
library(shinythemes)
library(sf)
library(dplyr)
library(rgdal)
library(terra)
library(leaflet)
library(leaflet.extras)

# Read Africa countries shapefile and transform to WGS84
gadm_sf <- st_read("Africa_countries.shp") %>%
  st_transform("+init=EPSG:4326") %>%
  st_make_valid()

# Load farming system boundaries from FAO and transform to WGS84
fao_sf <- st_read("fao_gadm_intersect.shp") %>%
  st_transform("+init=EPSG:4326") %>%
  st_make_valid()

# Load WorldPop population dataset and transform to WGS84
wp_path <- "AFR_PPP_2020_adj_v2.tif"
worldpop_pop <- terra::rast(wp_path)

# Set the CRS of fao_sf to match worldpop_pop
fao_sf <- st_transform(fao_sf, st_crs(worldpop_pop))

# Set the CRS of fao_sf to match worldpop_pop
gadm_sf <- st_transform(gadm_sf, st_crs(worldpop_pop))

#load spam dataset
# physical area from SPAM 2010
mylist <- list.files(pattern="._A.tif$")

mylist 

r <- rast(mylist)

# visualize
plot(r[[1]], type="interval", breaks=c(0,1,10,50,100,500,1000, Inf), ext=ext(gadm_sf))


# crop extent to gadm_sf
spam_yield <- terra::crop(r, gadm_sf)

# sum crop area for all 42 crops
agext <- sum(spam_yield)


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