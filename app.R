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

  # < INPUT FROM USER >:                  
                   
  ),
  
  #Body
  dashboardBody(
    
    # APPLICATION LAYOUT: ---- insert layout components here: ------------------------------------------------------
    fluidRow(
      
      #left column
      column(4,
             
              # < LEAFLET >:
             
               box(title = "Hurricane Map", solidHeader = TRUE, status = "primary", width = 12,
                   leafletOutput("leaf", height = 600)
               ),
             
             
             
             # < TABLE OF HURRICANES SINCE 2005 >:
             
             
             
      ),
      
      #middle coulumn
      column(4,

              # < sEARCH BY DAY >:
             
      ),
      
      
      
      #right column (tables)  - amber this is the first part
      column(4,
             
             # < BAR CHART BY YEAR >:
             
             
             # < BAR CHART BY MAX STRENGTH >:
             
             
             # < ABOUT >:
      )
      
      
      
    ) #end major fluidRow

    # application layout above   ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
  ))


server <- function(input, output) {
  
  # increase the default font size
  theme_set(theme_grey(base_size = 18))
  
  
  #REACTIVE DATA HERE
  
  
  #PLOT THE DATA: ---- insert data components here (in any order): -------------------------------------------
  
  
  # add a leaflet map of the atlantic
  output$leaf <- renderLeaflet({
    map <- leaflet()
    map <- addTiles(map)
    map <- setView(map, lng = -35.947, lat = 26.121, zoom = 2)
    map
  })
  
  
  
  
  #amber: this is the other part
  
  
  
  #data components above  ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
  
}

shinyApp(ui = ui, server = server)