#
# This is the server logic of a Shiny web application. You can run the
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(leaflet)
library(geojsonio)

shinyServer(function(input, output) {
  
  # # Reading in the dataset
  # shiny_data <- read.csv("imp_gender_race_tx.csv", header = T)
   shiny_data <- read.csv("C:/Users/kulra/Contacts/Desktop/heartmortality-prediction/imp_gender_race_tx.csv", header = T)
   

  # Function to extract latitude and longitude
  extract_coordinates <- function(coord_string) {
    coord_string <- gsub("[\\(\\)]", "", coord_string)  # Remove parentheses
    coords <- unlist(strsplit(coord_string, ", "))  # Split into latitude and longitude
    return(as.numeric(coords))
  }
  
  # Apply function to extract coordinates
  Coordinates <-  sapply(shiny_data$LatLong, extract_coordinates)
  #odd values- latitudes
  latitudes <- Coordinates[seq(1, length(Coordinates), 2)]
  #even values - longitudes
  longitudes <- Coordinates[c(FALSE, TRUE)]
  shiny_data$latitude = latitudes
  shiny_data$longitude = longitudes
  

  
  output$county_map <- renderLeaflet({
    filtered_data <- shiny_data
    
    # Apply filters based on user input
    if (input$genderInput != "All") {
      filtered_data <- filtered_data[filtered_data$Gender == input$genderInput, ]
    }
    if (input$raceInput != "All") {
      filtered_data <- filtered_data[filtered_data$Race_Ethnicity == input$raceInput, ]
    }
    
    # Create leaflet map
    
    m <- leaflet() %>%
      addTiles() %>%  # Add default OpenStreetMap map tiles
      addMarkers(lat=31.81858 , lng=-95.65393, popup="Palestine, TX")
    m  # Print the map
    
  })
})
