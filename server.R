#
# This is the server logic of a Shiny web application. You can run the 
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#

library(dplyr)
library(leaflet)
library(shiny)
library(shinydashboard)
library(ggplot2)
library(DT)
library(rgeos)
library(geosphere)
library(stringr)
library(rsconnect)

# London postcode centroids XY info - each point has long/lat info 
ldn_centroids <- read.csv("./Data/ldn_post_centroids.csv") 
ldn_centroids <- ldn_centroids %>%
  select(X.1, X, Y, objectid, pcd, pcd2, pcds) %>%
  mutate(pcd_nospace = str_replace_all(pcd, " ", ""))


# London restaurants XY info - each restaurant has long/lat info
ldn_rest <- read.csv("./Data/restaurants_ldn_geocoded.csv")
ldn_rest <- ldn_rest %>% 
  select(X, Name, Line.1, Line.2, Town, County, Postcode, full_add, longitude, latitude)

# Create icon for postcode - in leaflet library
pc_icon <- awesomeIcons(
  icon = 'user-circle',
  iconColor = 'white',
  library = 'fa',
  markerColor = 'red'
)

shinyServer(function(input, output) {
  
  ### Part 1: Produce the data table
  
  # Get the XY coordinate for the input postcode
  x <- reactive({
    ip <- toupper(input$text)
    post <- ldn_centroids %>%
      filter(pcd == ip | pcd_nospace == ip) ## accommodate entering postcode with or without space
    return(post)
  })
  
  # Get a df of distance between input postcode and all restaurants
  distance <- reactive({
    dist <- data.frame(c(distm(c(x()[1,2], x()[1,3]), ldn_rest[,9:10]))) # dist between input postcode and ldn_rest
    colnames(dist) <- "dist"
    return(dist)
  }) 
  
  # Merge list of distances with the restaurants, filter if less than the input (num) km
  data_input <- reactive({ # Note: reactive functions are used to return a value somewhere later in the code 
    distance() %>%
      mutate(sn = 1:n()) %>%
      left_join(ldn_rest, by=c("sn"="X")) %>%
      filter(dist <= input$num*1000) # "num" is a variable created in the ui server 
  })
  
  
  # Produce list from a datatable
  data_input_ordered <- reactive({
    data_input() %>% 
      select(Name, full_add, dist) %>% 
      mutate_if(is.numeric, ~round(., 0)) %>% ## round numbers to nearest whole number
      arrange(dist) %>% ## arrange restaurants in ascending order of distance
      rename("Restaurant" = Name, "Address" = full_add, "Approx distance from postcode (m)" = dist)
  })
  output$`Restaurants list` <- renderDataTable(data_input_ordered())
  
  # Part 2: Produce a map
  
  # Create a default map of London, without the reactive aspect, if postcode value is empty
  output$`London map` <- renderLeaflet(
    leaflet() %>% 
      setView(-0.12766, 51.50731, zoom = 12) %>% ## Default image of London
      addTiles() 
  )  
  
  # Create a reactive function + add observe to control the reactive function on the map
  
  # Add restaurant points
  observe({ # Note: observe monitors all changes in the reactive function, so we use observe
    leafletProxy("London map", data = data_input()) %>%
      clearGroup(group = "one") %>%
      addMarkers(~longitude, ~latitude, 
                 group = "one",
                 popup = ~as.character(paste("<b style=color:DodgerBlue;>", Name, "</b>", 
                                             "<br/>", Line.1, sep = "", Line.2,
                                             "<br/>", Postcode,
                                             "<br/>", Town))) 
    
  })
  

  # Add postcode marker point
  observe({ # Note: observeEvent is a defined trigger, so use it when an text changes
    leafletProxy("London map", data = x()) %>%
      clearGroup(group = "two") %>%
      setView(lng = x()[1,2], lat = x()[1,3], zoom = 16-2*input$num) %>% ## Zoom based on the most recent distance entered
      addAwesomeMarkers(~X, ~Y, 
                        group = "two",
                        icon = pc_icon, 
                        popup = ~as.character(paste("<b style=color:rgb(103, 77, 128);>", "Postcode: ", "</b>", pcd)))
    
  })
  
  
})

