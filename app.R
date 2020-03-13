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

#marks data with A to identify it with Atlantic
data <- read.csv(file = 'AtlanticHurrica-cleaned.csv', sep = ",", header = TRUE)
data$date <- ymd(data$date)
data$type <- "A"

#marks data2 with N to identify it with NorthEast
data2 <- read.csv(file="NortheastAndNor-cleaned.csv", sep=",", header=TRUE)
data2$date <- ymd(data2$date)
data2$type <- "N"

#combines both data tables into one
data <- rbind(data,data2)
#data[is.na(data$hur_name),]="UNNAMED"

#getting data from 2005 and onwards
data2005 <- data #[year(data$date) >= 2005,]

#select only some columns
dataCol2005 = data2005[c(0:2, 4:11)] 

#getting code and name of hurricanes, saving max windspeed of hurricane from 2005 and onwards
dataT <- data2005[,c(1,2,10)]
hurMaxSpeed <- aggregate(. ~hur_code+hur_name, dataT, max)
hurMaxSpeed <- hurMaxSpeed[order(hurMaxSpeed$hur_name, decreasing = FALSE),]

#add a category column to hurMaxSpeed:
hurMaxSpeed$category[hurMaxSpeed$max_speed <= 33] <- 'TD'
hurMaxSpeed$category[hurMaxSpeed$max_speed >= 34 & hurMaxSpeed$max_speed <= 63] <- 'TS'
hurMaxSpeed$category[hurMaxSpeed$max_speed >= 64 & hurMaxSpeed$max_speed <= 82] <- '1'
hurMaxSpeed$category[hurMaxSpeed$max_speed >= 83 & hurMaxSpeed$max_speed <= 95] <- '2'
hurMaxSpeed$category[hurMaxSpeed$max_speed >= 96 & hurMaxSpeed$max_speed <= 112] <- '3'
hurMaxSpeed$category[hurMaxSpeed$max_speed >= 113 & hurMaxSpeed$max_speed <= 136] <- '4'
hurMaxSpeed$category[hurMaxSpeed$max_speed >= 137] <- '5'

#getting top 10 hurricane speed
hurTop10 <- hurMaxSpeed[order(hurMaxSpeed$max_speed, decreasing = TRUE),]
hurTop10 <- hurTop10[1:10,]

#range of hurricane data
dataRange2005 <- range(year(data2005$date))

#SHINY DASHBOARD
# Create the shiny dashboard
ui <- dashboardPage(
  dashboardHeader(title = "Hurricane Data Analysis"),
  dashboardSidebar(disable = FALSE, collapsed = FALSE,
                   
                   # < INPUT FROM USER >:
                   selectInput("hurrYear","Hurricane By Year",append(c("","All"),seq(dataRange2005[1],dataRange2005[2],by=1)), selected=2018),
                   selectInput("hurrName","Hurricane Name",append("All",as.character(hurMaxSpeed$hur_name)), selected="All"),
                   selectInput("hurrTop","Hurricane Top 10",append(c("","All"),as.character(hurTop10$hur_code)), selected=""),
                   #checkboxInput("hurrTop10", "Hurricane Top 10", value = FALSE, width = NULL),
                   checkboxInput("landfallCheckbox", "Hurricanes Making Landfall", value = FALSE, width = NULL),
                   
                   radioButtons("hurrSpan", "Filter Year Range", 
                                choices = c("Show Hurricanes Since 2005" = "span2005",
                                            "Show All Hurricanes" = "spanAll"),
                                selected = NULL, inline = FALSE, width = NULL)
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
             box(title="Hurricane List", background = "black", solidHeader = TRUE, status="primary", width=12,
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

server <- function(input, output, session) {
  
  
  # increase the default font size
  theme_set(theme_grey(base_size = 18) )
  
  
  #filter by year and name:
  filter <- reactive(
    #user selected hurricane, show hurricane on map
    if(input$hurrTop != "All" & input$hurrTop != ""){
      updateSelectInput(session,"hurrYear",choices=append("All",seq(dataRange2005[1],dataRange2005[2],by=1)), selected="")
      updateSelectInput(session,"hurrName",choices=append("All",as.character(hurMaxSpeed$hur_name)), selected="")
      dataCol2005[dataCol2005$hur_code==input$hurrTop,]
    }
    #user selected All, show all hurricane
    else if(input$hurrTop == "All" & input$hurrTop!=""){
      updateSelectInput(session,"hurrYear",choices=append("All",seq(dataRange2005[1],dataRange2005[2],by=1)), selected="")
      updateSelectInput(session,"hurrName",choices=append("All",as.character(hurMaxSpeed$hur_name)), selected="")
      dataCol2005[dataCol2005$hur_code %in% hurTop10$hur_code,]
    }
    #user selected hurrYear = All and hurrName = "All"
    else if(input$hurrYear == "All" & input$hurrName == "All"){
      updateSelectInput(session,"hurrTop",choices=append("All",as.character(hurTop10$hur_code)), selected="")
      dataCol2005
    }
    #user selected hurrName = All, hurrYear to a year
    else if (input$hurrName == "All" & input$hurrYear != "All"){  #filter by year:
      updateSelectInput(session,"hurrTop",choices=append("All",as.character(hurTop10$hur_code)), selected="")
      
      dataYear <- dataCol2005[year(dataCol2005$date)==input$hurrYear,]
      #updateSelectInput(session,"hurrName",choices=append("All",as.character(dataYear$hur_name)), selected="All")
      dataYear
    }
    #user selected hurrName to a name, hurrYear = All
    else if (input$hurrName != "All" & input$hurrYear == "All"){  #filter by name:
      updateSelectInput(session,"hurrTop",choices=append("All",as.character(hurTop10$hur_code)), selected="")
      #updateSelectInput(session,"hurrName",choices=append("All",as.character(dataCol2005$hur_name)), selected="All")
      dataCol2005[dataCol2005$hur_name==input$hurrName,]
    }
    #user selected hurrName to a name, hurrYear to a year
    else{
      updateSelectInput(session,"hurrTop",choices=append("All",as.character(hurTop10$hur_code)), selected="")
      dataCol2005[year(dataCol2005$date)==input$hurrYear & dataCol2005$hur_name==input$hurrName,]
    }
  )
  
  
  #filterByLandfall:
  filterByLandfall <- reactive(
    if(input$landfallCheckbox == TRUE){
      df[df$record_id == 'L',] #return only records with landfall
    }
    else{
      df   #return all records
    }
  )
  
  #creates table for lineGraph of max wind speed
  filterLineMaxWind <- reactive(
    #get max windspeed per day for specified year
    if(input$hurrYear != "All" || input$hurrYear != ""){
      
    }
    #get max windspeed for all years
    else{
      
    }
  )
  
  observeEvent(input$landfallCheckbox,
               filterByLandfall())
  
  #top10 list
  hurrTopR <- reactive(
    if(input$hurrTop == "All"){
      data2005 
      
    }
  )
  #REACTIVE DATA HERE
  
  #PLOT THE DATA: ---- insert data components here (in any order): -------------------------------------------
  square <- function(x) {
    return(x * x)
  }
  
  # add a leaflet map of the atlantic
  output$leaf <- renderLeaflet({
    
    #get reactive data:
    reactData <- filter()   #filter data
    
    # Create a continuous palette function (from leaflet documentation)
    pal <- colorNumeric(
    palette = "Reds",
      domain = reactData$max_speed)
    
    map <- leaflet()
    map <- addTiles(map)
    map <- setView(map, lng = -35.9, lat = 39.1, zoom = 3)
    map <- addCircles(map, 
                      lng = reactData$lon, lat = reactData$lat, 
                      color = pal(reactData$max_speed), 
                      weight = reactData$max_speed / 4,    #1->5   2->20   3->40
                      label = paste(reactData$hur_name, ": ", reactData$max_speed, " knots, ", reactData$min_pressure, " millibars, on",
                                    month(reactData$date), "/", day(reactData$date), "/", year(reactData$date), " @ ", reactData$time
                                    
                      ),
                      labelOptions = labelOptions(textOnly = TRUE, direction = "top")
    )
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