library(shiny)
library(dplyr)
library(leaflet)

# the main idea of the server code I borrowed from here: https://rstudio.github.io/leaflet/shiny.html
# follow the link for more detailed comments

shinyServer(function(input, output, session) {
   
    # import data
    riversStats <- read.csv("riversStats.csv", stringsAsFactors = FALSE)
    
    # filter df
    filteredData <- reactive({
      filter(riversStats, valueType == input$valueType & substance == input$pollutant) })
    
    # define output$map and basemap
    output$map <- renderLeaflet({
      # create a leaflet basemap
      m <- leaflet(riversStats) %>% addProviderTiles(providers$Stamen.TonerLite) %>%
        fitBounds(~min(lon), ~min(lat), ~max(lon), ~max(lat)) })
    
    observe({
      # create a pallet
      domain <- filter(riversStats, valueType == input$valueType & substance == input$pollutant)[7]
      domain <- domain$value
      pal <- colorNumeric(pal = c("#12c2e9", "#f64f59"), domain = domain)
      
      # create layer of points
      leafletProxy("map", data = filteredData()) %>%
        clearShapes() %>%
        addCircles(radius = 5000, weight = 0,
                   fillColor = ~pal(value), fillOpacity = 0.9, popup = ~paste(value))})
 
    
    observe({
      proxy <- leafletProxy("map", data = filteredData())
      # clear legend
      proxy %>% clearControls()
      # calculate domain
      domain <- filter(riversStats, valueType == input$valueType & substance == input$pollutant)[7]
      domain <- domain$value
      # define pallete 
      pal <- colorNumeric(pal = c("#12c2e9", "#f64f59"), domain = domain)
      proxy %>% addLegend(position = "bottomright", pal = pal, values = ~domain) })
    
    })
