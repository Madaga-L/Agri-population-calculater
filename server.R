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
    farming_system_geom <- fao_sf[fao_sf$DESCRIPTIO == farming_system_name & fao_sf$ADM0_NAME == country_name, ]
    intersection_extent <- st_intersection(country_geom, farming_system_geom)
    
    
    #mask population based on farming system selected
    worldpop_pop_mask <- terra::mask(worldpop_pop, intersection_extent)
    
    spam_yield_mask <- terra::mask(agext, intersection_extent) 
    
    total_population_sum <- sum(as.vector(worldpop_pop_mask), na.rm = TRUE)
    mean_population <- mean(as.vector(worldpop_pop_mask), na.rm = TRUE, FUN = mean)
    mean_yield <- mean(as.vector(spam_yield_mask), na.rm = TRUE, FUN = mean)
    
    output$result_table <- renderTable({
      data.frame("Total_Population" = total_population_sum,
                 "mean_Population" = mean_population,
                 "mean_SPAM_Yield" = mean_yield)
    })
    
    # convert SpatRaster to rasterLayer object
    worldpop_pop_mask_raster <- raster(worldpop_pop_mask)
    
    pal0 <- colorNumeric(c("RdYlBu"), na.omit(values(worldpop_pop_mask_raster)),
                         na.color = "transparent")
    
    
    output$result_map <- renderLeaflet({
      leaflet() %>% 
        addProviderTiles("CartoDB.Positron") %>% 
        addRasterImage(worldpop_pop_mask_raster, colors = pal0, opacity = 0.9) %>%
        addLegend(pal = pal0, values = values(worldpop_pop_mask_raster), 
                  title = "Population",
                  position = "bottomright")
    })
  })
}
