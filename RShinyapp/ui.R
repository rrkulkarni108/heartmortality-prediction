##User Interface

library(shiny)
library(leaflet)

fluidPage(

    # Application title
    titlePanel("Heart Disease Mortality Map of Texas in Adults 35+"),


    sidebarLayout(
      sidebarPanel(
        selectInput("genderInput", "Select Gender:",
                    choices = c("All", "Male", "Female"), selected = "All"),
        selectInput("raceInput", "Select Race/Ethnicity:",
                    choices = c("All", "White", "Black", "Hispanic"), selected = "All")
      ),

      mainPanel(
        leafletOutput("county_map")
      )
    )
)
