library(shiny)
library(shinydashboard)
library(ggplot2)
library(lubridate)
library(DT)
library(jpeg)
library(grid)
library(leaflet)
library(scales)
library(dplyr)

#note: the data file to be read here needs to be processed by out Python script first.

#read in datafile
data <- read.csv(file = 'cleaned_hurricane_data.csv', sep = ",", header = TRUE)
data$date <- ymd(data$date)

#getting data from 2018 and onwards
data1year <- data[year(data$date)==2018,]  # | year(data$date)<=2011,]
#data1year <- data1year[seq(1, nrow(data1year), 2), ]  #get every other point

#getting data from 2005 and onwards
data2 <- data[year(data$date)>=2005,]

#getting code and name of hurricanes, saving max windspeed of hurrican from 2005 and onwards
data3 <- data2 %>% group_by(hur_code,hur_name) %>% summarize(max_speed =max(max_speed))
data3 <- data3[order(data3$hur_name,decreasing = FALSE),]

#getting top 10 hurrican speed
data4 <- data3[order(data3$max_speed, decreasing = TRUE),]
data4 <- data4[1:10,]

#range of hurrican data
data5 <- range(year(data2$date))

#SHINY DASHBOARD

# Create the shiny dashboard
ui <- dashboardPage(
  dashboardHeader(title = "Hurricane Data Analysis"),
  dashboardSidebar(disable = FALSE, collapsed = FALSE,
                   
                   # < INPUT FROM USER >:
                   
                   selectInput("hurrYear","Hurrican By Year",append("All",seq(data5[1],data5[2],by=1) )),
                   selectInput("hurrName","Hurrican Name",append("All",as.character(data3$hur_code)) ),
                   selectInput("hurrTop","Hurrican Top 10",append("All",as.character(data4$hur_code)) )
  ),
  
  #Body
  dashboardBody(
    
    # APPLICATION LAYOUT: ---- insert layout components here: ------------------------------------------------------
    fluidRow(
      
      #left column
      column(8,
             # < LEAFLET >:
             box(title = "Hurricane Map", solidHeader = TRUE, status = "primary", width = 12,
                 leafletOutput("leaf", height = 1000)
             ),
             box(title="Total Trash picked up by Tag", solidHeader = TRUE, status="primary", width=12,
                 dataTableOutput("atlanticData", height=400)
             )
             # < TABLE OF HURRICANES SINCE 2005 >:
      ),
      #middle coulumn
      column(2,
             # < sEARCH BY DAY >:
      ),
      #right column (tables)  - amber this is the first part
      column(2,
             
             # < BAR CHART BY YEAR >:
             # < BAR CHART BY MAX STRENGTH >:
             # < ABOUT >:
      )
      
      
      
    ) #end major fluidRow
    
    # application layout above   ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
  ))


server <- function(input, output) {
  
  # increase the default font size
  theme_set(theme_grey(base_size = 18) )
  tableOne = data2;
  hurrYearR <- reactive(
    if(input$hurrYear == "All"){
      tableOne = data2 
    }
  )
  hurrNameR <- reactive(
    if(input$hurrName == "All"){
      tableOne = data2 
    }
  )
  hurrTopR <- reactive(
    if(input$hurrTop == "All"){
      tableOne = data2 
    }
  )
  
  #REACTIVE DATA HERE
  
  
  #PLOT THE DATA: ---- insert data components here (in any order): -------------------------------------------
  
  
  
  # Create a continuous palette function (from leaflet documentation)
  pal <- colorNumeric(
    palette = "Reds",
    domain = data1year$max_speed)
  
  square <- function(x){
    return(x*x)
  }
  
  # add a leaflet map of the atlantic
  output$leaf <- renderLeaflet({
    map <- leaflet()
    map <- addTiles(map)
    map <- setView(map, lng = -35.947, lat = 39.121, zoom = 3)
    map <- addCircles(map, 
                      lng = data1year$lon, lat = data1year$lat, 
                      color = pal(data1year$max_speed), 
                      weight = data1year$max_speed / 5,    #1->5   2->20   3->40
                      label = paste("(", data1year$lat, ",", data1year$lon, ")")) #concat
    map
  })
  
  
  output$atlanticData <- renderDT(
    tableOne
  )
  
  
  
  
  
  #amber: this is the other part
  
  
  
  #data components above  ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
  
}

shinyApp(ui = ui, server = server)