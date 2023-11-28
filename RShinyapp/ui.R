##User Interface

library(shiny)
library(leaflet)

ui <- fluidPage(

    # Application title
    titlePanel("Heart Disease Mortality Map of Texas in Adults 35+"),


    sidebarLayout(
      sidebarPanel(
        br(),
        selectInput("genderInput", "Select Gender:",
                    choices = c("Male", "Female"), selected = "All"),
        br(),
        selectInput("raceInput", "Select Race/Ethnicity:",
                    choices = c("White", "Black", "Hispanic"), selected = "All"),
        br(),
    	  selectInput("countyInput", "Select Texas County:",
    			  choices = 
    			    #choices = 
    			    unique(imp_gender_race_tx$County), selected = "All")
          ),

      mainPanel(
        leafletOutput("county_map")
      )
    )
)
