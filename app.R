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
library(plyr)

#note: the data file to be read here needs to be processed by our Python script first.

#read in datafile
data <- read.csv(file = 'cleaned_hurricane_data.csv', sep = ",", header = TRUE)
data$date <- ymd(data$date)

#getting data from 2018 and onwards
data1year <- data[year(data$date) == 2018,] # | year(data$date)<=2011,]
#data1year <- data1year[seq(1, nrow(data1year), 2), ]  #get every other point

#getting data from 2005 and onwards
data2 <- data[year(data$date) >= 2005,]

#select columns
#data2 <- data2[c(0:2, 4:11)]

#getting code and name of hurricanes, saving max windspeed of hurricane from 2005 and onwards
#data3 <- data2 %>% group_by(hur_code, hur_name) %>% summarize(max_speed = max(max_speed))
dataT <- data2[,c(1,2,10)]
data3 <- aggregate(. ~hur_code+hur_name, dataT, max)
data3 <- data3[order(data3$hur_name, decreasing = FALSE),]


#getting top 10 hurricane speed
data4 <- data3[order(data3$max_speed, decreasing = TRUE),]
data4 <- data4[1:10,]

#range of hurricane data
data5 <- range(year(data2$date))

#SHINY DASHBOARD

# Create the shiny dashboard
ui <- dashboardPage(
  dashboardHeader(title = "Hurricane Data Analysis"),
  dashboardSidebar(disable = FALSE, collapsed = FALSE,



  # < INPUT FROM USER >:
  
    selectInput("hurrYear","Hurricane By Year",append("All",seq(data5[1],data5[2],by=1)), selected=2018),
    selectInput("hurrName","Hurricane Name",append("All",as.character(data3$hur_name)) ),
    selectInput("hurrTop","Hurricane Top 10",append("All",as.character(data4$hur_code)) )
    #checkboxInput("hurrTop10", "Hurricane Top 10", value = FALSE, width = NULL)

  ),

#Body
  dashboardBody(

# APPLICATION LAYOUT: ---- insert layout components here: ------------------------------------------------------
    fluidRow(
    
      
      
      #left column
      column(10,
              # < LEAFLET >:
               box(title = "Hurricane Map", solidHeader = TRUE, status = "primary", width = 12,
                   leafletOutput("leaf", height = 600)
               ),
             
             # < TABLE OF HURRICANES SINCE 2005 >:
               box(title="Hurricane List", solidHeader = TRUE, status="primary", width=12,
                   dataTableOutput("hurrTable", height=400)
               )
             
      ),
      
      #middle coulumn
      column(1,
              # < sEARCH BY DAY >:
      ),
      #right column (tables)  - amber this is the first part
      column(1,

             
             # < BAR CHART BY YEAR >:
             # < BAR CHART BY MAX STRENGTH >:
             # < ABOUT >:

      )



    ) #end major fluidRow

# application layout above   ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
  ))
#added a comment here for no reason

server <- function(input, output) {


  #data1year <- data[year(data$date)==2018 | year(data$date)<=2011,]


  # increase the default font size

  theme_set(theme_grey(base_size = 18) )
  filter <- reactive(
    if(input$hurrYear == "All" & input$hurrName == "All"){
      dataCol = data2[c(0:2, 4:11)] #select only some columns
      dataCol
    }
    else if (input$hurrName == "All"){  #filter by year:
      dataCol = data2[c(0:2, 4:11)] #select only some columns
      dataCol[year(dataCol$date)==input$hurrYear,]
    }
    else if (input$hurrYear == "All"){  #filter by name:
      dataCol = data2[c(0:2, 4:11)] #select only some columns
      dataCol[dataCol$hur_name==input$hurrName,]
    }
    else{
      dataCol = data2[c(0:2, 4:11)] #select only some columns
      dataCol[year(dataCol$date)==input$hurrYear & dataCol$hur_name==input$hurrName,]
    }
  )

 

  
  hurrTopR <- reactive(
    if(input$hurrTop == "All"){
       data2 

    }
  )

  #REACTIVE DATA HERE


  #PLOT THE DATA: ---- insert data components here (in any order): -------------------------------------------



  # Create a continuous palette function (from leaflet documentation)
  pal <- colorNumeric(
    palette = "Reds",
    domain = data1year$max_speed)

  square <- function(x) {
    return(x * x)
  }

  # add a leaflet map of the atlantic
  output$leaf <- renderLeaflet({
    
    reactData <- filter()   #filter data
    
    map <- leaflet()
    map <- addTiles(map)
    map <- setView(map, lng = -35.9, lat = 39.1, zoom = 3)
    map <- addCircles(map, 
                      lng = reactData$lon, lat = reactData$lat, 
                      color = pal(reactData$max_speed), 
                      weight = reactData$max_speed / 4,    #1->5   2->20   3->40
                      label = paste(reactData$hur_name, ": ", reactData$max_speed, " knots, ", reactData$min_pressure, " millibars, on",
                                    month(reactData$date), "/", day(reactData$date), "/", year(reactData$date), " @ ", reactData$time

                                    ), #concat
                      labelOptions = labelOptions(textOnly = TRUE, direction = "top")
    )
    # map <-   addPolylines(
    #                   map,
    #                   data = reactData,
    #                   lng = reactData$lon, 
    #                   lat = reactData$lat,
    #                   weight = 3,
    #                   opacity = 3
    #) 
    map
  })
  
    
    output$hurrTable <- DT::renderDataTable(
      DT::datatable({
        filter()
      },
      options = list(searching = TRUE, pageLength = 10, lengthChange = FALSE
      ), rownames = FALSE
      )
    )
    
   # output$value <- renderText({ input$somevalue })
    
   
  
  
  
  

  #amber: this is the other part



  #data components above  ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

}

shinyApp(ui = ui, server = server)