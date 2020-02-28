library(shiny)
library(shinydashboard)
library(ggplot2)
library(lubridate)
library(DT)
library(jpeg)
library(grid)
library(leaflet)
library(scales)

#note: the data file to be read here needs to be processed by out Python script first.

#read in datafile
data <- read.csv(file = 'cleaned_hurricane_data.csv', sep = ",", header = TRUE)


#SHINY DASHBOARD

# Create the shiny dashboard
ui <- dashboardPage(
  dashboardHeader(title = "Hurricane Data Analysis"),
  dashboardSidebar(disable = FALSE, collapsed = FALSE

  #input                   
                   
  ),
  
  #Body
  dashboardBody(
    
      #put fluidRows and columns here

  ))


server <- function(input, output) {
  
  # increase the default font size
  theme_set(theme_grey(base_size = 18))
  
  
  #GET REACTIVE DATA FROM INPUT:
  #(from P1):
  
            # calculate the values one time and re-use them in multiple charts to speed things up
            # <- reactive({subset(filtered, filtered$username == input$Picker)}) #contatins just the data about selected picker
            
            
            # observeEvent(input$Picker, {
            #   print(paste0("You have chosen: ", input$Picker))
            # })
  
  
  
  #PLOT THE DATA:
  
  #insert data components here
  
}

shinyApp(ui = ui, server = server)

