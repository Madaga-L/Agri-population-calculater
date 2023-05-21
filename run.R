library(shiny)
library(shinythemes)
library(sf)
library(dplyr)
library(rgdal)
library(terra)
library(raster)
library(leaflet)
library(leaflet.extras)

port <- Sys.getenv('PORT')

shiny::runApp(
  appDir = getwd(),
  host = '0.0.0.0',
  port = as.numeric(port)
)