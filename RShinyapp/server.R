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

shinyServer(function(input, output) {
  
  # # Reading in the dataset
  # shiny_data <- read.csv("imp_gender_race_tx.csv", header = T)

  # Function to extract latitude and longitude
  extract_coordinates <- function(coord_string) {
    coord_string <- gsub("[\\(\\)]", "", coord_string)  # Remove parentheses
    coords <- unlist(strsplit(coord_string, ", "))  # Split into latitude and longitude
    return(as.numeric(coords))
  }
  
  # Apply function to extract coordinates
  shiny_data$Coordinates <- sapply(shiny_data$LatLong, extract_coordinates)
  
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
    leaflet() %>%
      addTiles() %>%
      addCircleMarkers(
        data = filtered_data,
        lat = ~Coordinates[1],
        lng = ~Coordinates[2],
        popup = paste("County: ", filtered_data$County, "<br>",
                      "Death Count: ", filtered_data$DeathCount),
        label = ~paste("County: ", County),
        radius = ~sqrt(DeathCount) * 2  # Adjust marker size based on DeathCount
      )
    # You can customize the markers, popups, tooltips based on your dataset
  })
})