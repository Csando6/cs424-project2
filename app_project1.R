# Matt Jankowski
# Project 1 - R and Shiny

#libraries to include

library(shiny)
library(shinydashboard)
library(ggplot2)
library(lubridate)
library(DT)
library(jpeg)
library(grid)
library(leaflet)
library(scales)



#FILE READING

#get the data in
rawdata <- read.table(file = "https://www.evl.uic.edu/aej/424/litterati%20challenge-65.csv", sep = ",", header = TRUE)
rawdata


#DATE MANIPULATION

#some of the locations are not in Forest Park, so let's get rid of those data points
filtered <- subset(rawdata, lat > 41.84 & lon < -87.80 & lon > -87.83)

#date manipulation
newDate <- ymd_hms(filtered$litterTimestamp)   #ex: 2020-01-07 22:14:20 UTC
newDateChicago <- with_tz(newDate, tzone = "America/Chicago") #convert to CST
date <- date(newDateChicago)                          #only hours
hour <- hour(newDateChicago)
weekday <- weekdays(newDateChicago)

filtered$date <- date       #add new rows to dataframe
filtered$hour <- hour
filtered$weekday <- weekday
filtered$litterTimestamp <- NULL   #remove old data from table


# get top 10 pickers:
pickerFreq <- as.data.frame(table(filtered$username))
top10pickers <- head(pickerFreq[order(-pickerFreq$Freq),], 10)

# top10tags <- TODO



#SHINY DASHBOARD

# Create the shiny dashboard
ui <- dashboardPage(
  dashboardHeader(title = "Litterati Forest Park Challenge - Data Analysis"),
  dashboardSidebar(disable = FALSE, collapsed = FALSE,
                   
  #Sidebar input                 
  selectInput("Picker", "Top 10 Pickers", top10pickers$Var1, selected = 'julieta'),
  selectInput("Tag", "Top 10 Tags", 
              c('plastic', 'paper', 'wrapper', 'bag', 'can', 'candy', 'cigarrette', 'cup', 'soda', 'jar'), 
              selected = 'plastic')
  ),
  
  #Body
  dashboardBody(
    fluidRow(
      
      #left column
      column(3,
             fluidRow(
               infoBoxOutput("progressBox", width = 12)
             ),
             fluidRow(
               box(title = "Map of Litter in Forest Park", solidHeader = TRUE, status = "primary", width = 12,
                   leafletOutput("leaf", height = 600)
               )
             ),
             textInput("About", "About: Written by Matt Jankowski; libraries: shiny, ggplot, lubridate; data from litterati.com", placeholder = "Thank you for enjoying!")

      ),
      
      #middle coulumn (bar graphs)
      column(6,
             fluidRow(
               box( title = "Litter Picked by Date", solidHeader = TRUE, status = "primary", width = 12,
                    plotOutput("bar1", height = 300)
               )
             ), 
             fluidRow(
               box( title = "Litter Picked by Day of the Week", solidHeader = TRUE, status = "primary", width = 12,
                    plotOutput("bar2", height = 300)
               )
             ),             
             fluidRow(
               box( title = "Litter Picked by Hour of Day", solidHeader = TRUE, status = "primary", width = 12,
                    plotOutput("bar3", height = 300)
               )
             )
             # ,
             # fluidRow(
             #   box( title = "Litter Picked by Category (Tag)", solidHeader = TRUE, status = "primary", width = 12,
             #        plotOutput("bar4", height = 300)
             #   )
             # )
      ),
      
      #right column (tables)
      column(3,
             fluidRow(
               box(title = "Litter Picked by Date", solidHeader = TRUE, status = "primary", width = 12,
                   dataTableOutput("table1", height = 300)
               )
             ),
             fluidRow(
               box(title = "Litter Picked by Day of the Week", solidHeader = TRUE, status = "primary", width = 12,
                   dataTableOutput("table2", height = 300)
               )
             ),
             fluidRow(
               box(title = "Litter Picked by Hour of Day", solidHeader = TRUE, status = "primary", width = 12,
                   dataTableOutput("table3", height = 300)
               )
             )
             # ,
             # fluidRow(
             #   box(title = "Litter Picked by Category (Tag)", solidHeader = TRUE, status = "primary", width = 12,
             #       dataTableOutput("table4", height = 300)
             #   )
             # )
      )
    )
  ))


server <- function(input, output) {
  
  # increase the default font size
  theme_set(theme_grey(base_size = 18))
  
  
  #GET REACTIVE DATA FROM INPUT:
  
  # calculate the values one time and re-use them in multiple charts to speed things up
  justOnePickerReactive <- reactive({subset(filtered, filtered$username == input$Picker)}) #contatins just the data about selected picker
  
  
  # observeEvent(input$Picker, {
  #   print(paste0("You have chosen: ", input$Picker))
  # })

  
  #PLOT THE DATA:
  
  #info box
  output$progressBox <- renderInfoBox({
    justOnePicker <- justOnePickerReactive()
    numberPerUser = nrow(justOnePicker)
    
    infoBox(
      input$Picker, paste0(numberPerUser, " pieces of litter"), icon = icon("leaf"),
      color = "green"
    )
  })
  
  

  
  #bar graphs
  output$bar1 <- renderPlot({
    justOnePicker <- justOnePickerReactive()
    ggplot(justOnePicker, aes(x=date)) +
      labs(x="Date", y = "Number of Litter Picked") + 
      geom_bar(stat="count", width=0.7, fill="steelblue") +
      scale_x_date(date_breaks = "1 month", date_labels =  "%b %Y", expand = c(0, 0)) +
      theme(axis.text.x=element_text(angle=55, hjust=1))
  })
  
  output$bar2 <- renderPlot({
    justOnePicker <- justOnePickerReactive()
    justOnePicker$weekday <- factor(justOnePicker$weekday, weekdays(as.Date('1970-01-03') + 1:7))
    ggplot(justOnePicker, aes(x=weekday)) + 
      labs(x="Weekday", y = "Number of Litter Picked") + 
      geom_bar(stat="count", width=0.7, fill="steelblue")
  })
  
  output$bar3 <- renderPlot({
    justOnePicker <- justOnePickerReactive()
    ggplot(justOnePicker, aes(x=hour)) + 
      labs(x="Hour", y = "Number of Litter Picked") + 
      geom_bar(stat="count", width=0.7, fill="steelblue") +
      scale_x_continuous(breaks = 0:24)
  })
  
  #add a leaflet map of Forest Park
  output$leaf <- renderLeaflet({
    justOnePicker <- justOnePickerReactive()
    map <- leaflet()
    map <- addTiles(map)
    map <- setView(map, lng = -87.813, lat = 41.869, zoom = 14)
    map <- addCircles(map, lng = justOnePicker$lon, lat = justOnePicker$lat, color = "blue")
    map
  })

  #tables
  output$table1 <- DT::renderDataTable(
    
      DT::datatable({
        justOnePicker <- justOnePickerReactive()
        dateFreq <- as.data.frame(table(justOnePicker$date, dnn = list("Date")), responseName = "Count")
      },
      options = list(searching = FALSE, pageLength = 4, lengthChange = FALSE
      ), rownames = FALSE
      )
  )
  
  output$table2 <- DT::renderDataTable(
    DT::datatable({
      justOnePicker <- justOnePickerReactive()
      justOnePicker$weekday <- factor(justOnePicker$weekday, weekdays(as.Date('1970-01-03') + 1:7))
      weekdayFreq <- as.data.frame(table(justOnePicker$weekday, dnn = list("Weekday")), responseName = "Count")
    },
    options = list(searching = FALSE, pageLength = 7, lengthChange = FALSE, info = FALSE, bPaginate = FALSE
    ), rownames = FALSE
    )
  )
  
  output$table3 <- DT::renderDataTable(
    DT::datatable({
      justOnePicker <- justOnePickerReactive()
      hourFreq <- as.data.frame(table(justOnePicker$hour, dnn = list("Hour")), responseName = "Count")
    },
    options = list(searching = FALSE, pageLength = 5, lengthChange = FALSE
    ), rownames = FALSE
    )
  )
  
}

shinyApp(ui = ui, server = server)
