#--app.R file for Project 2 of CS 424 - Group 9-------
#--Amber Little------*********************************
#--Charly Sandoval---*********************************
#--Matt Jankowski----*********************************
#-----------------------------------------------------

library(shiny)
library(shinydashboard)
library(ggplot2)
library(lubridate)
library(DT)
library(jpeg)
library(grid)
library(leaflet)
library(scales)
library(hashmap)
library(plyr)
#library(dplyr)
library(devtools)        #for theme
library(dashboardthemes) #for theme
library(hflights)
library(repurrrsive)
library(tidyverse)
library(RColorBrewer)

#IMPORTANT: app.R needs "dark_theme_mod.R" in the same directory to run well with the dark theme:
source("dark_theme_mod.R") #connect



#note: the data file to be read here is first processed by our Python script.
#READ IN THE DATA FILES:

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

#add a column. 'L' in record_id will be 50 in landfall - Matt
data$landfall[data$record_id == 'L'] <- 50
data$landfall[data$record_id != 'L'] <- 0

#getting data from 2005 and onwards
data2005 <- data #[year(data$date) >= 2005,]

#Data for graphs that begins at the year 2005
Ambersdata2005 <- (data[year(data$date) >= 2005,])


#getting code and name of hurricanes, saving max windspeed of hurricane from 2005 and onwards
dataT <- data2005[,c(1,2,10)]
hurMaxSpeed <- aggregate(. ~hur_code+hur_name, dataT, max)
hurMaxSpeed <- hurMaxSpeed[order(hurMaxSpeed$hur_name, decreasing = FALSE),]

#barChartDataing Max Speed - Amber  
dataQ <- Ambersdata2005[,c(1,2,10,4)] 
hurMaxSpeed2 <- aggregate(. ~hur_code+hur_name+year(date), dataQ, max)[0:4]
hurMaxSpeed2 <- hurMaxSpeed2[order(hurMaxSpeed2$hur_name, decreasing = FALSE),]

#filters by whether the hurricane made landfall or not - Matt
dataL <- data2005[,c(1,2,25)]
hurLandfall <- aggregate(. ~hur_code+hur_name, dataL, max)
#data$landfall <- NULL #(deallocate)


#add a category column to hurMaxSpeed:
hurMaxSpeed$category[hurMaxSpeed$max_speed <= 33] <- 'TD'
hurMaxSpeed$category[hurMaxSpeed$max_speed >= 34 & hurMaxSpeed$max_speed <= 63] <- 'TS'
hurMaxSpeed$category[hurMaxSpeed$max_speed >= 64 & hurMaxSpeed$max_speed <= 82] <- '1'
hurMaxSpeed$category[hurMaxSpeed$max_speed >= 83  & hurMaxSpeed$max_speed <= 95] <- '2'
hurMaxSpeed$category[hurMaxSpeed$max_speed >= 96 & hurMaxSpeed$max_speed <= 112] <- '3'
hurMaxSpeed$category[hurMaxSpeed$max_speed >= 113 & hurMaxSpeed$max_speed <= 136] <- '4'
hurMaxSpeed$category[hurMaxSpeed$max_speed >= 137] <- '5'

#add a category column to hurMaxSpeed2 (For Ambers Graphs)
hurMaxSpeed2$category[hurMaxSpeed2$max_speed <= 33] <- 'TD'
hurMaxSpeed2$category[hurMaxSpeed2$max_speed >= 34 & hurMaxSpeed2$max_speed <= 63] <- 'TS'
hurMaxSpeed2$category[hurMaxSpeed2$max_speed >= 64 & hurMaxSpeed2$max_speed <= 82] <- '1'
hurMaxSpeed2$category[hurMaxSpeed2$max_speed >= 83 & hurMaxSpeed2$max_speed <= 95] <- '2'
hurMaxSpeed2$category[hurMaxSpeed2$max_speed >= 96 & hurMaxSpeed2$max_speed <= 112] <- '3'
hurMaxSpeed2$category[hurMaxSpeed2$max_speed >= 113 & hurMaxSpeed2$max_speed <= 136] <- '4'
hurMaxSpeed2$category[hurMaxSpeed2$max_speed >= 137] <- '5'


barChartData <- hurMaxSpeed2
col_name <- paste("column" , 1:5, sep = "")
names(barChartData) <- col_name


#replace column from a number to a meaningful word or blank (For Matt's filter)
hurLandfall$landfall[hurLandfall$landfall == 50] <- 'landfall'
hurLandfall$landfall[hurLandfall$landfall == 0]  <- '-'


#select only some columns
dataCol2005 = data2005[c(0:2, 4:11)]

#make a join by hur_code -> adds landfall column to dataCol2005
dataCol2005 = merge(hurLandfall[c(1,3)], dataCol2005, by="hur_code")


#remove all hurricanes except the 5 favs:
ourHurricanes = dataCol2005[dataCol2005$hur_code == 'AL092017' |
                            dataCol2005$hur_code == 'AL112017' |
                            dataCol2005$hur_code == 'AL152017' |
                            dataCol2005$hur_code == 'AL062018' |
                            dataCol2005$hur_code == 'AL142018',]



#getting top 10 hurricanes by max speed
hurTop10 <- hurMaxSpeed[order(hurMaxSpeed$max_speed, decreasing = TRUE),]
hurTop10 <- hurTop10[1:10,]

#range of hurricane data
dataRange2005 <- range(year(data2005$date))

#Create a mapping between names and renderings
mapRenderingsList <- c('Open Topo Map', 'Toner Background', 'World Terrain')
mapSourcesList <- c(providers$OpenTopoMap, providers$Stamen.TonerBackground, providers$Esri.WorldTerrain)
mapRenderings <- hashmap(mapRenderingsList, mapSourcesList)



#SHINY DASHBOARD:
# Create the shiny dashboard
ui <- dashboardPage(
  
  # skin = "red", #use the custom skin theme
  
  #Header
  dashboardHeader(title = "Hurricane Data Analysis"),

  #Sidebar
  dashboardSidebar(disable = FALSE, collapsed = FALSE,
                   
    #INPUT FROM USER:
    selectInput("hurrYear","Hurricane By Year",append(c("","All"),seq(dataRange2005[1],dataRange2005[2],by=1)), selected=2018),
    selectInput("hurrName","Hurricane Name",append("All",as.character(hurMaxSpeed$hur_name)), selected="All"),
    selectInput("hurrTop","Hurricane Top 10",append(c("","All"),as.character(hurTop10$hur_code)), selected=""),
                 
    #checkboxInput("hurrTop10", "Hurricane Top 10", value = FALSE, width = NULL),
    checkboxInput("landfallCheckbox", "Hurricanes Making Landfall", value = FALSE, width = NULL),
    radioButtons("hurrSpan", "Filter Year Range", 
                choices = c("Show Hurricanes Since 2005" = "span2005",
                            "Show All Hurricanes" = "spanAll"),
                selected = NULL, inline = FALSE, width = NULL),
    selectInput(inputId="mapRender",  #choose map style
               label="Map Rendering",
               choices=mapRenderingsList),
    checkboxInput("ourHurCheckbox", "5 interesting hurricanes", value = FALSE, width = NULL)
    
  ),
  
  #Body
  dashboardBody(
   
    dark_theme_mod,  ### changing theme

    # APPLICATION LAYOUT: ---- insert layout components here: ------------------------------------------------------
    fluidRow(
      
      #left column
      column(12,
             # < LEAFLET >:
             box(title = "Hurricane Map", solidHeader = TRUE, status = "primary", width = 12,
                 leafletOutput("leaf", height = 600)
             ),
             # < TABLE OF HURRICANES SINCE 2005 >:
             box(title="Hurricane List", background = "black", solidHeader = TRUE, status="primary", width=12,
                 dataTableOutput("hurrTable", height=400)
             ),
             # < MAX SPEED LINE GRAPH >:
             box(title="Hurricane Max Wind Speed",solidHeader = TRUE, status="primary",width=12,
               plotOutput("maxWindSpeed", height=600)
             ),
             
             # #HURRICANES PER YEAR
             box(title = "Number of Hurricanes Per Year Since 2005", solidHeader = TRUE, status = "primary", width= 12,
                plotOutput("bargraph1", height = 600)
                ), 
             
             # # HURRICANES AND THEIR MAX STRENGTH
             box(title = "Hurricane Max Speed", solidHeader = TRUE, status = "primary", width= 12,
                 plotOutput("bargraph2", height = 600)
             ),
             
             # # HURRICANES AND THEIR MAX STRENGTH
             box(title = "Hurricane Max Category", solidHeader = TRUE, status = "primary", width= 12,
                 plotOutput("bargraph3", height = 600)
             ),
      
            # STACKED BAR CHART
            box(title = "Hurricanes & Their Strength", solidHeader = TRUE, status = "primary", width= 12,
                plotOutput("bargraph4", height = 800)
            )
  
             )  #, #End column
      
      # #middle coulumn
      # column(1,
      #        # < SEARCH BY DAY >:
      # ),
      # 
      # #right column (tables)  - amber this is the first part
      # column(1,
      #        # < BAR CHART BY YEAR >:
      #        # < BAR CHART BY MAX STRENGTH >:
      #        # < ABOUT >:
      #        
      # )      
      
    ) #end major fluidRow
    
    # application layout above   ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
  ) # End dash board body
  
  ) # End dashboard page


# SERVER SIDE:
server <- function(input, output, session) {
  
  dataCommon = data.frame()

  
  #REACTIVE FUNCTIONS:
  
  #filter by year and name:
  filter <- reactive(
    
    if(input$ourHurCheckbox == TRUE){
      ourHurricanes
    }

    
    #THIS IS THE PART SAME AS BEFORE:
    else if(input$landfallCheckbox == FALSE){
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
      
    } #end checkbox FALSE
    
    
    #THIS IS THE PART WHICH WAS DOUBLED & EXTENDED:
    else{ #TRUE
            #user selected hurricane, show hurricane on map
            if(input$hurrTop != "All" & input$hurrTop != ""){
              updateSelectInput(session,"hurrYear",choices=append("All",seq(dataRange2005[1],dataRange2005[2],by=1)), selected="")
              updateSelectInput(session,"hurrName",choices=append("All",as.character(hurMaxSpeed$hur_name)), selected="")
              dataCol2005[dataCol2005$hur_code==input$hurrTop & dataCol2005$landfall == 'landfall',]
            }
            #user selected All, show all hurricane
            else if(input$hurrTop == "All" & input$hurrTop!=""){
              updateSelectInput(session,"hurrYear",choices=append("All",seq(dataRange2005[1],dataRange2005[2],by=1)), selected="")
              updateSelectInput(session,"hurrName",choices=append("All",as.character(hurMaxSpeed$hur_name)), selected="")
              dataCol2005[dataCol2005$hur_code %in% hurTop10$hur_code & dataCol2005$landfall == 'landfall',]
            }
            #user selected hurrYear = All and hurrName = "All"
            else if(input$hurrYear == "All" & input$hurrName == "All"){
              updateSelectInput(session,"hurrTop",choices=append("All",as.character(hurTop10$hur_code)), selected="")
              dataCol2005[dataCol2005$landfall == 'landfall',]
            }
            #user selected hurrName = All, hurrYear to a year
            else if (input$hurrName == "All" & input$hurrYear != "All"){  #filter by year:
              updateSelectInput(session,"hurrTop",choices=append("All",as.character(hurTop10$hur_code)), selected="")
              dataYear <- dataCol2005[year(dataCol2005$date)==input$hurrYear & dataCol2005$landfall == 'landfall',]
              #updateSelectInput(session,"hurrName",choices=append("All",as.character(dataYear$hur_name)), selected="All")
              dataYear
            }
            #user selected hurrName to a name, hurrYear = All
            else if (input$hurrName != "All" & input$hurrYear == "All"){  #filter by name:
              updateSelectInput(session,"hurrTop",choices=append("All",as.character(hurTop10$hur_code)), selected="")
              #updateSelectInput(session,"hurrName",choices=append("All",as.character(dataCol2005$hur_name)), selected="All")
              dataCol2005[dataCol2005$hur_name==input$hurrName & dataCol2005$landfall == 'landfall',]
            }
            #user selected hurrName to a name, hurrYear to a year
            else{
              updateSelectInput(session,"hurrTop",choices=append("All",as.character(hurTop10$hur_code)), selected="")
              dataCol2005[year(dataCol2005$date)==input$hurrYear & dataCol2005$hur_name==input$hurrName & dataCol2005$landfall == 'landfall',]
            }
    }
    
  )
  


  #creates table for lineGraph of max wind speed
  filterWindA <- reactive(
    #get max windspeed per day for specified year
    if(input$hurrYear != "All" ){
      maxAtlantic <- data2005[data2005$type=="A" & year(data2005$date)==input$hurrYear,]
      maxAtlantic <- maxAtlantic[,c("date","max_speed")]
      maxAtlantic$day <- day(maxAtlantic$date)
      maxAtlantic$month <- month(maxAtlantic$date)
      maxAtlantic$year <- year(maxAtlantic$date)
      maxAtlantic$date <- NULL
      maxASpeed <- aggregate(. ~year+month+day,maxAtlantic,max)
      maxASpeed$date <- paste(maxASpeed$year,maxASpeed$month,maxASpeed$day,sep="-") %>% ymd()
      maxASpeed
    }
    #get max windspeed for all years
    else{
      maxAtlantic <- data2005[data2005$type=="A",]
      maxAtlantic <- maxAtlantic[,c("date","max_speed")]
      maxAtlantic$date <- year(maxAtlantic$date)
      maxASpeed <- aggregate(. ~date,maxAtlantic,max)
      maxASpeed
    }
  )
  
  filterWindN <- reactive(
    #get max windspeed per day for specified year
    if(input$hurrYear != "All"){
      maxNorth <- data2005[data2005$type=="N" & year(data2005$date)==input$hurrYear,]
      maxNorth <- maxNorth[,c("date","max_speed")]
      maxNorth$day <- day(maxNorth$date)
      maxNorth$month <- month(maxNorth$date)
      maxNorth$year <- year(maxNorth$date)
      maxNorth$date <- NULL
      if(nrow(maxNorth)==0 ){
        maxNSpeed <- data.frame(date = as.Date(character()),
                                max_speed=integer())
      }else{
        maxNSpeed <- aggregate(. ~year+month+day,maxNorth, max)
        maxNSpeed$date <- paste(maxNSpeed$year,maxNSpeed$month,maxNSpeed$day,sep="-") %>% ymd()
      }
      maxNSpeed
    }
    #get max windspeed for all years
    else{
      maxNorth <- data2005[data2005$type=="N" ,]
      maxNorth <- maxNorth[,c("date","max_speed")]
      maxNorth$date <- year(maxNorth$date)
      maxNSpeed <- aggregate(. ~date,maxNorth, max)
      maxNSpeed
    }
  )

  
  #top10 list
  hurrTopR <- reactive(
    if(input$hurrTop == "All"){
      data2005 
    }
  )
    
  #amber: this is the other part
  #PLOT THE DATA: ---- insert data components here (in any order): -------------------------------------------
  
  # Number of Hurricanes per year since 2005
  output$bargraph1 <- renderPlot({
    ggplot(data = barChartData, aes(x = column3)) +
             geom_bar(stat="bin", colour="black", fill="#DD8888",) +
            xlab("Year") + ylab("Number of Hurricanes") + 
            theme(text = element_text(size = 25)) }
  ) # End bargraph1
  
  # Hurricanes with respect to their max speed 
  output$bargraph2 <- renderPlot({
    ggplot(data = barChartData, aes(x = column4)) +
      geom_bar(stat="bin", colour="black", fill="#DD8888",) +
      xlab("Max Speed") + ylab("Number of Hurricanes") + 
      theme(text = element_text(size = 25)) }
  ) # End bargraph2
  
  # Hurricanes with respect to their max category 
  output$bargraph3 <- renderPlot({
    ggplot(data = barChartData, aes(x = column5)) +
      geom_bar(stat="count", colour="black", fill="#DD8888",) +
      xlab("Max Category") + ylab("Number of Hurricanes") + 
      
      theme(text = element_text(size = 24)) 
    }
  ) # End bargraph3
  
  # Stacked Bar Graph Displaying Num of Hurricane Including Categories since 2005
  output$bargraph4 <- renderPlot({
(ggplot(data = barChartData, aes(x = column3, fill = column5)) +
     geom_bar(position ="stack", stat = "count")
     + xlab("Year") + ylab("Number of Hurricanes") + 
      scale_fill_brewer(palette = 10) +
      theme(text = element_text(size = 25))
 ) # end ggplot
    
    }
  ) # End bargraph4


  square <- function(x) {  #square of a number
    return(x * x)
  }
  
  
  # add a leaflet map
  output$leaf <- renderLeaflet({
    
    #get reactive data:
    reactData <- filter()   #filter data
   # reactData <- filterByLandfall(reactData)
    
    # Create a continuous palette function (from leaflet documentation)
    pal <- colorNumeric(
      palette = "Reds",
      domain = reactData$max_speed)
    
    #map object
    map <- leaflet(data = reactData) %>%
          addTiles() %>%
          addProviderTiles(mapRenderings[[input$mapRender]]) %>%
          setView(lng = -75.9, lat = 39.1, zoom = 3) %>%
          addCircles(  
              lng = reactData$lon, lat = reactData$lat, 
              color = pal(reactData$max_speed), radius = 2,
              weight = reactData$max_speed / 3.5,    #1->5   2->20   3->40
              label = paste(reactData$hur_name, ": ", reactData$max_speed, " knots, ", reactData$min_pressure, " mbar, on",
                    month(reactData$date), "/", day(reactData$date), "/", year(reactData$date), " @ ", reactData$time      
              ),
              labelOptions = labelOptions(textOnly = TRUE, direction = "top",
                    style = list(
                      "color" = "black",
                      # "font-style" = "italic",
                      "box-shadow" = "3px 3px 3px rgba(0,0,0,1)",
                      "font-size" = "10px",
                      "background-color" =  "white"
                    )
              )
          ) 
    map   #run map
  })
  
  #add a table
  output$hurrTable <- DT::renderDataTable(
    DT::datatable({
      filter() #use filtered data
      },
      options = list(searching = TRUE, pageLength = 10, lengthChange = FALSE
      ), rownames = FALSE
    )
  )
  
  #add line graph for max wind speed
  output$maxWindSpeed <-renderPlot({
    maxASpeed <- filterWindA()
    maxNSpeed <- filterWindN()
    if(nrow(maxNSpeed) ==0){
      graph = ggplot() +
        geom_line(data=maxASpeed, aes(x=date,y=max_speed, colour="Atlantic")) +
        scale_colour_manual("",
                            breaks = c("Atlantic"),
                            values = c("red"))+
        xlab("Dates")+
        ylab("Max Wind Speed")+
        ggtitle(paste("Wind Speed for Year ",input$hurrYear))
      graph
    }
    else{
      graph = ggplot() +
        geom_line(data=maxASpeed, aes(x=date,y=max_speed, colour="Atlantic")) +
        geom_line(data=maxNSpeed, aes(x=date,y=max_speed, colour="North")) +
        scale_colour_manual("",
                            breaks = c("Atlantic","North"),
                            values = c("red","blue"))+
        xlab("Dates")+
        ylab("Max Wind Speed")+
        ggtitle(paste("Wind Speed for Year ",input$hurrYear))
      graph
    }
  }) # End line graph for max wind speed
  
  
  # output$value <- renderText({ input$somevalue })     #text output
  
  #data components above  ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
  
}
shinyApp(ui = ui, server = server)

