## R Shiny App

#setwd("C:/Users/holly/OneDrive/Documents/R/656")
setwd("C:/Users/kulra/Contacts/Desktop/heartmortality-prediction/RShinyapp")

library(shiny)
library(leaflet)
library(geojsonio)
library(sf)
library(dplyr)
library(htmlwidgets)
library(raster)
library(RColorBrewer)


## load data

shiny_data <- read.csv("imp_gender_race_tx.csv")

  # Function to extract latitude and longitude
  extract_coordinates <- function(coord_string) {
    coord_string <- gsub("[\\(\\)]", "", coord_string)  # Remove parentheses
    coords <- as.numeric(unlist(strsplit(coord_string, ", ")))  # Split into latitude and longitude
    return(coords)
  }
  
  # Apply function to extract coordinates
  Coordinates <-  sapply(shiny_data$LatLong, extract_coordinates)
  #odd values- latitudes
  latitudes <- Coordinates[seq(1, length(Coordinates), 2)]
  #even values - longitudes
  longitudes <- Coordinates[c(FALSE, TRUE)]
  shiny_data$latitude = latitudes
  shiny_data$longitude = longitudes

  # Load GeoJSON data
  geojson_data <- st_read("TexasCounties.geojson")
  
  
  
  ### Create an asymmetric color range
  
  ## Make vector of colors for values which are smaller (250 colors)
  rc1 <- colorRampPalette(colors = c("white", "red"), space = "Lab")(150)
  
  ## Make vector of colors for values which are much larger than 50 (50 colors)
  rc2 <- colorRampPalette(colors = c("red", "dark red"), space = "Lab")(100)
  
  ## Combine the two color palettes
  rampcols <- c(rc1, rc2)
  
  mypal <- colorNumeric(palette = rampcols, domain = shiny_data$DeathCount)
  
  ## If you want to preview the color range, run the following code
  #min deathcounts is 50.5, max deathcounts is 1053
  previewColors(colorNumeric(palette = rampcols, domain = NULL), values = 50:1100)
  
  

ui <- fluidPage(

    # Application title
    titlePanel("Heart Disease Mortality Map of Texas Adults 35+"),

    sidebarLayout(
      sidebarPanel(
        selectInput("genderInput", "Select Gender:",
                    choices = c("Male", "Female"), selected = "All",
			  multiple = F),
        selectInput("raceInput", "Select Race/Ethnicity:",
                    choices = c("White", "Black", "Hispanic"), selected = "All",
			  multiple = F),
	  selectInput("countyInput", "Select Texas County:",
			  choices = unique(shiny_data$County), selected = "All",
			  multiple = F)
      ),

      mainPanel(
        leafletOutput("county_map")
      )
    )
)

server <- shinyServer(function(input, output) {
  
    county_map = reactive({
	filtered_data <- shiny_data %>%
	  filter(County == input$countyInput) %>%
	  filter(Gender ==  input$genderInput | input$genderInput == "All") %>%
	  filter(Race_Ethnicity == input$raceInput | input$raceInput == "All")
	return(filtered_data) 
  })
  
   output$county_map <- renderLeaflet({
    selected_county <- county_map()$County[1]  # Assuming there's only one selected county

    leaflet() %>%
      addTiles() %>%
      setView(lat = mean(shiny_data$latitude), lng = mean(shiny_data$longitude), zoom = 6) %>%
      addPolygons(data = geojson_data,fillOpacity = 0.5,
                  fillColor = ~mypal(shiny_data$DeathCount),
                    #fillColor = "none",
                  color = "black",
                  weight = 1) %>%
      addMarkers(lat = shiny_data$latitude[shiny_data$County == selected_county],
                 lng = shiny_data$longitude[shiny_data$County == selected_county],
                 label = as.character(county_map()$DeathCount),
                 labelOptions = labelOptions(noHide = TRUE))%>%
      addLegend(position = "bottomright", pal = mypal, values = shiny_data$DeathCount,
                title = "Death Count\n per 100,000 value",
                opacity = 1)

  })

   
   
})

shinyApp(ui = ui, server = server)