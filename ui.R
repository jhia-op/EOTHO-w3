#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
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

# Create a dashboard instead of normal UI

ui <- dashboardPage(
  skin = "red",
  dashboardHeader(title = "Eat Out to Help Out Restaurant Finder"),
  dashboardSidebar(
    textInput("text", 
              label = "Postcode", 
              placeholder = "Enter Postcode"), ## Place to enter postcode - "text" is input id, for server reference
    numericInput("num", label = "Distance (km)", 
                 value = 0.5, min = 0.5, max = 5, step = 0.5) ## Place to enter distance
  ),
  dashboardBody(
    fluidRow(box(width = 12, leafletOutput(outputId = "London map"))), ## Output a map of the zoomed in map, width = 12 means 12 columns
    fluidRow(box(width = 12, dataTableOutput(outputId = "Restaurants list"))) ## Output of restaurants 
  )
  
)
