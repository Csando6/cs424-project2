#+
#--app.R file for Project 2 of CS 424 - Group 9-------
#-----------------------------------------------------
#--Amber Little------*********************************
#--Charly Sandoval---*********************************
#--Matt Jankowski----*********************************
#-----------------------------------------------------

library(shiny) #in server
library(shinydashboard) #in server
library(ggplot2) #in server
library(lubridate) #in server
library(DT) #in server
#library(jpeg) #in server
#library(grid) 
library(leaflet) #in server
library(scales) # in server
library(hashmap) # in server
library(plyr) #in server
library(devtools) #in server
library(roxygen2)
library(rversions)
#library(dashboardthemes) #in server
#library(hflights) #XXXX
#library(repurrrsive)#XXXX
#library(tidyverse)# in server
library(RColorBrewer)# in server

#IMPORTANT: app.R needs "dark_theme_mod.R" in the same directory to run well with the dark theme:
#source("dark_theme_mod.R") #connect

#NOTE: the data file to be read here is first processed by our Python script.
#READ IN THE DATA FILES:


#marks data with A to identify it with Atlantic
data <- read.csv(file = 'AtlanticHurrica-cleaned.csv', sep = ",", header = TRUE)
data$date <- ymd(data$date)
data$type <- "A" #Atlantic


#marks data2 with N to identify it with NorthEast
data2 <- read.csv(file="NortheastAndNor-cleaned.csv", sep=",", header=TRUE)
data2$date <- ymd(data2$date)
data2$type <- "N" #Pacific


#combines both data tables into one
data <- rbind(data,data2)
data$min_pressure[data$min_pressure == -999] <- NA
#data[is.na(data$hur_name),]="UNNAMED"


#add a column. 'L' in record_id will be 50 in landfall - Matt
data$landfall[data$record_id == 'L'] <- 50
data$landfall[data$record_id != 'L'] <- 0


#Data for graphs that begins at the year 2005
data2005 <- (data[year(data$date) >= 2005,])


#getting code and name of hurricanes, saving max windspeed of hurricane from 2005 and onwards
dataT <- data[,c(1,2,10)]
hurMaxSpeed <- aggregate(. ~hur_code+hur_name, dataT, max)
hurMaxSpeed <- hurMaxSpeed[order(hurMaxSpeed$hur_name, decreasing = FALSE),]


#by year:
dataY <- data[c(1,2,4)]
dataY$date <- year(dataY$date)
hurYear <- aggregate(. ~hur_code+hur_name, dataY, max)


#getting code and name of hurricanes, saving min pressure of hurricane from 2005 and onwards - Matt
dataP <- data[,c(1,2,11)]
hurMinPressure <- aggregate(. ~hur_code+hur_name, dataP, min)
#hurMaxSpeed <- hurMaxSpeed[order(hurMaxSpeed$hur_name, decreasing = FALSE),]


#barChartDataing Max Speed - Amber  
dataQ <- data2005[,c(1,2,10,4)] 
hurMaxSpeed2 <- aggregate(. ~hur_code+hur_name+year(date), dataQ, max)[0:4]
hurMaxSpeed2 <- hurMaxSpeed2[order(hurMaxSpeed2$hur_name, decreasing = FALSE),]


#filters by whether the hurricane made landfall or not - Matt
#add the type in there as well - it will be useful later on
dataL <- data[,c(1,2,25,24)]
hurLandfall <- aggregate(. ~hur_code+hur_name+type, dataL, max)
hurLandfall <- hurLandfall[, c(1,2,4,3)] #reorder columns


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
hurLandfall$landfall[hurLandfall$landfall == 50] <- 'yes'
hurLandfall$landfall[hurLandfall$landfall == 0]  <- '-'


#-Unused but available: A combination of the above tables, in which 1 hurricane is in 1 line,
# along with all helpful columns. -Matt
merg0 = merge(hurYear[c(1:3)], hurMaxSpeed[c(1,3:4)], by="hur_code")
merg1 = merge(merg0, hurMinPressure[c(1,3)], by="hur_code")
merg2 = merge(merg1, hurLandfall[c(1,3,4)], by="hur_code")
classifiedHurricanes <- with(merg2,  merg2[order(hur_name),])


#select only some columns
dataCol = data[c(0:2, 4:11, 24)]


#make a join by hur_code -> adds landfall column to dataCol
dataCol = merge(hurLandfall[c(1,3)], dataCol, by="hur_code")


#same for 2005+ only:
#select only some columns
dataCol2005 = data2005[c(0:2, 4:11, 24)]


#make a join by hur_code -> adds landfall column to dataCol
dataCol2005 = merge(hurLandfall[c(1,3)], dataCol2005, by="hur_code")


#remove all hurricanes except the 5 favs:
ourHurricanes = dataCol[dataCol$hur_code == 'AL092017' |
                        dataCol$hur_code == 'AL112017' |
                        dataCol$hur_code == 'AL152017' |
                        dataCol$hur_code == 'AL062018' |
                        dataCol$hur_code == 'AL142018',]

#getting top 10 hurricanes by max speed
hurTop10 <- hurMaxSpeed[order(hurMaxSpeed$max_speed, decreasing = TRUE),]
hurTop10 <- hurTop10[1:10,]

#range of hurricane data
dataRange2005 <- range(year(data$date))

#Create a mapping between names and renderings
#use this website to find more renderings: http://leaflet-extras.github.io/leaflet-providers/preview/index.html
mapRenderingsList <- c('Open Topo Map', 'Toner Background', 'Watercolor')
mapSourcesList <- c(providers$OpenTopoMap, providers$Stamen.TonerBackground, providers$Stamen.Watercolor)
mapRenderings <- hashmap(mapRenderingsList, mapSourcesList)


#SHINY DASHBOARD:

ui <- dashboardPage(
  
  #Header
  dashboardHeader(title = "Hurricane Data Analysis"),
  
  #Sidebar
  dashboardSidebar(disable = FALSE, collapsed = FALSE,
                   
     #INPUT COMPONENTS FROM USER:
     selectInput("hurrYear","Hurricanes By Year",append(c("","All"),seq(dataRange2005[1],dataRange2005[2],by=1)), selected=2018),
     selectInput("hurrName","Hurricanes By Name",append("All",as.character(hurMaxSpeed$hur_name)), selected="All"),
     selectInput("hurrTop","Top 10 Hurricanes",append(c("","All"),as.character(hurTop10$hur_code)), selected=""),
     dateInput("hurrDate", "Hurricanes By Date",value="", format="mm/dd/yyyy"),
     
     checkboxInput("atlanticCheckbox", "Atlantic Hurricanes", value = TRUE, width = NULL),
     checkboxInput("pacificCheckbox", "Pacific Hurricanes", value = TRUE, width = NULL),
     checkboxInput("checkbox2005", "Only Since 2005", value = FALSE, width = NULL),
     checkboxInput("landfallCheckbox", "Only Making Landfall", value = FALSE, width = NULL),
     
     selectInput(inputId="mapRender",  #choose map style
                 label="Map Rendering",
                 choices=mapRenderingsList),
     checkboxInput("ourHurCheckbox", "Our 5 Favorites", value = FALSE, width = NULL)
  ),
  
  
  #Body - is made of tabs, which house individual components
  dashboardBody(
    
    #dark_theme_mod,  ### changing theme to dark
    
    # APPLICATION LAYOUT: ----layout components here: ------------------------------------------------------
    fluidRow(
     
      #central column:
      column(12,
             
             tabsetPanel(
               
               tabPanel ( "Map & List" ,
                          #project summary
                          box(headerPanel(title="Hurricane Analysis"), width = 9, height = 125, textOutput("explanation")),
                          
                          infoBoxOutput("progressBox", width = 3), #will show hur count
                          
                          # < LEAFLET >:
                          box(title = "Hurricane Map", background = "black", solidHeader = TRUE, status = "primary", width = 7,
                              leafletOutput("leaf", height = 600)
                          ),
                          # < TABLE OF HURRICANES SINCE 2005 >:
                          box(title="Hurricane List", background = "black", solidHeader = TRUE, status="primary", width=5,
                              dataTableOutput("hurrTable", height=600)
                          ),
               ), # End tab1
               # -------------------------------------------------------------------------------------------------------------------------------------------- #
               
               tabPanel("Intensity Plots", 
                        # < MAX SPEED LINE GRAPH >:
                        box(title="Hurricane Max Wind Speed", background = "black", solidHeader = TRUE, status="primary",width=12,
                            plotOutput("maxWindSpeed", height=600)
                        ),
                        # < MAX SPEED LINE GRAPH >:
                        box(title="Hurricane Min Pressure", background = "black", solidHeader = TRUE, status="primary",width=12,
                            plotOutput("minPressure", height=600)
                        ),
                        
               ), # End tab2
               # -------------------------------------------------------------------------------------------------------------------------------------------- #
              
               tabPanel("Numerical Graphs",
                        
                        # # HURRICANES AND THEIR MAX STRENGTH
                        box(title = "Number of Hurricanes Per Category", background = "black", solidHeader = TRUE, status = "primary", width= 12,
                            plotOutput("bargraph3", height = 600)
                        ),
                        
                        # #HURRICANES PER YEAR
                        box(title = "Number of Hurricanes Per Year", background = "black", solidHeader = TRUE, status = "primary", width= 12,
                            plotOutput("bargraph1", height = 600)
                        ), 
                        
                        # STACKED BAR CHART
                        box(title = "Hurricanes & Their Strength", background = "black", solidHeader = TRUE, status = "primary", width= 12,
                            plotOutput("bargraph4", height = 800)
                        )
               ),   # End Tab3
               
               tabPanel ( "About" , 
                          
                          h2("Against The Wind"),
                          
                          h3("Developed By: Amber Little, Charly Sandoval, and Matt Jankowski"),
                          
                          h4("Project 2 in CS 424 (Data Analytics / Visualization) at the University of Illinois at Chicago Spring 2020"),
                          
                          h5("________________________________________________________________________________________________________________________________________________________________________________________________________________________________________________________________________________________________________________________________________________"),
                          
                          h3("5 Favorite Hurricanes"),
                          h3(""),
                          h4("These 5 hurricanes are from 2017 and 2018. Most of us remember these hurricanes and the destruction they caused on the US and Carribean islands."),
                          h4("In order to investigate them, toggle the -Our 5 Favorites- filter to explore the specs and stats of some of the most powerful hurricanes of modern times."),
                          h3(""),
                          h5("AL092017 -	HARVEY		- 2017"),
                          h5("AL112017 -	IRMA	    - 2017"),
                          h5("AL152017 -	MARIA		  - 2017"),
                          h5("AL062018 -	FLORENCE	- 2018"),
                          h5("AL142018 -	MICHAEL		- 2018"),
                          
                          h5("________________________________________________________________________________________________________________________________________________________________________________________________________________________________________________________________________________________________________________________________________________"),
                          h3("How Hurricanes are Categorized: Saffir-Simpson Hurricane Wind Scale"),
                          
                          
                          h4("Tropical Depression (TD); Non-Hurricane"),
                          h5("A tropical cyclone with maximum sustained winds of 38 mph (33 knots) or less."),
                          
                          h4("Tropical Storm (TS); Non-Hurricane"),
                          h5("A tropical cyclone with maximum sustained winds of 39 to 73 mph (34 to 63 knots)."),
                          
                          h4("Category 1: 74-95 mph Sustained Winds; Very dangerous winds will produce some damage"),
                          h5("Well-constructed frame homes could have damage to roof, shingles, vinyl siding and gutters. Extensive damage to power lines and poles likely will result in power outages that could last a few to several days. "),
                          
                          h4("Category 2: 96-110 mph Sustained Winds; Extremely dangerous winds will cause extensive damage"),
                          h5(" Well-constructed frame homes could sustain major roof and siding damage. Near-total power loss is expected with outages that could last from several days to weeks."),
                          
                          h4("Category 3: 111-129 mph Sustained Winds - MAJOR; Devastating damage will occur"),
                          h5(" Well-built framed homes may incur major damage or removal of roof decking and gable ends. Electricity and water will be unavailable for several days to weeks after the storm passes."),
                          
                          h4("Category 4: 130-156 mph  Sustained Winds - MAJOR; Catastrophic damage will occur"),
                          h5("Well-built framed homes can sustain severe damage with loss of most of the roof structure and/or some exterior walls. Power outages will last weeks to possibly months. Most of the area will be uninhabitable for weeks or months."),
                          
                          h4("Category 5: 157 mph or higher Sustained Winds - MAJOR; Catastrophic damage will occur"),
                          h5("A high percentage of framed homes will be destroyed, with total roof failure and wall collapse. Power outages will last for weeks to possibly months. Most of the area will be uninhabitable for weeks or months."),
                          
                          h4("Source: National Hurricane Center and Central Pacific Hurricane Center, https://www.nhc.noaa.gov/aboutsshws.php"),
                          h5("________________________________________________________________________________________________________________________________________________________________________________________________________________________________________________________________________________________________________________________________________________"),
                          
                          h5("Note: This project implements an interactive display/analysis of a dataset of hurricane data dating back to 1851 in the Atlantic
                            and Pacific regions. Modern technology has allowed us to gather data about a hurricane's path, collect its increase/decrease in speeds as it moves, 
                            and can even predict its behavior! With this being said, there is limited information for hurricanes dating a few decades back. You will find that some of these hurricanes are listed as
                            'UNNAMED' in addition to displaying minimal information about its path, max-speed, etc. Keep this in mind while analyzing hurricane records over the years"),
                          
                          h5("* Hurricanes are measured in knots/hour. Knot is a unit of speed equal to one nautical mile per hour, exactly 1.852 km/h (1.151 mph)."),
                          
                          h5("* Libraries Used: shiny, shinydashboard, ggplot2, lubridate, DT, leaflet, plyr, devtools,
                               dashboardthemes, hflights, repurrrsive, tidyverse, RColorBrewer"),
                          
                          h5("* Data Source: NHC Data Archive -> http://www.nhc.noaa.gov/data/#hurdat"),
                          
                          h5("* Created using R, RStudio, Shiny, Python, [insert theme credit here]")
               ) # End tabsetPanel
             ) 
      ) # End tabsetPanel
    ) # End tabsetPanel
  ) # End dash board body
  
  
  
  # -------------------------------------------------------------------------------------------------------------------------------------------- #
  
  
) # End dashboard page


#................................................................................................................................................ #
# SERVER SIDE:
server <- function(input, output, session) {
  
  dataCommon = data.frame()
  
  
  #REACTIVE FUNCTIONS:
  
  #filter by year and name:
  filter <- reactive(
    
    #IF 5-Hurricane option selected
    if(input$ourHurCheckbox == TRUE){
      ourHurricanes
    }
    
    #IF 5-Hurricane option selected AND Landfall
    #this is independent from the other filters
    else if(input$ourHurCheckbox == TRUE && input$landfallCheckbox == TRUE){
      ourHurricanes[ourHurricanes$landfall == 'yes',]
    }
    else if(!is_empty(input$hurrDate)){
      #print(input$hurrDate)
      dateVal <- ymd(input$hurrDate)
      #print(dateVal)
      dataCol[dataCol$date==input$hurrDate,]
    }
    
    #the rest is independent from the above
    else if (input$pacificCheckbox == FALSE){
      if (input$atlanticCheckbox == FALSE){
        dataCol[dataCol$type=='N/A',] #if neither atlantic nor pacific true -> show none
      }
      #DUPLICATED FOR ATLANTIC OCEAN:
      
      else{ #atlantic == TRUE
        
        #THIS PART IS ORIGINAL:
        if(input$landfallCheckbox == FALSE & input$checkbox2005 == FALSE){
          #user selected hurricane, show hurricane on map
          if(input$hurrTop != "All" & input$hurrTop != ""){
            updateSelectInput(session,"hurrYear",choices=append("All",seq(dataRange2005[1],dataRange2005[2],by=1)), selected="")
            updateSelectInput(session,"hurrName",choices=append("All",as.character(hurMaxSpeed$hur_name)), selected="")
            dataCol[dataCol$hur_code==input$hurrTop & dataCol$type=='A',]
          }
          #user selected All, show all hurricane    
          else if(input$hurrTop == "All" & input$hurrTop!=""){
            updateSelectInput(session,"hurrYear",choices=append("All",seq(dataRange2005[1],dataRange2005[2],by=1)), selected="")
            updateSelectInput(session,"hurrName",choices=append("All",as.character(hurMaxSpeed$hur_name)), selected="")
            dataCol[dataCol$hur_code %in% hurTop10$hur_code & dataCol$type=='A',]
          }
          #user selected hurrYear = All and hurrName = "All"
          else if(input$hurrYear == "All" & input$hurrName == "All"){
            updateSelectInput(session,"hurrTop",choices=append("All",as.character(hurTop10$hur_code)), selected="")
            dataCol[dataCol$type=='A',]
          }
          #user selected hurrName = All, hurrYear to a year
          else if (input$hurrName == "All" & input$hurrYear != "All"){  #filter by year:
            updateSelectInput(session,"hurrTop",choices=append("All",as.character(hurTop10$hur_code)), selected="")
            dataYear <- dataCol[year(dataCol$date)==input$hurrYear & dataCol$type=='A',]
            #updateSelectInput(session,"hurrName",choices=append("All",as.character(dataYear$hur_name)), selected="All")
            dataYear
          }
          #user selected hurrName to a name, hurrYear = All
          else if (input$hurrName != "All" & input$hurrYear == "All"){  #filter by name:
            updateSelectInput(session,"hurrTop",choices=append("All",as.character(hurTop10$hur_code)), selected="")
            #updateSelectInput(session,"hurrName",choices=append("All",as.character(dataCol$hur_name)), selected="All")
            dataCol[dataCol$hur_name==input$hurrName & dataCol$type=='A',]
          }
          #user selected hurrName to a name, hurrYear to a year
          else{
            updateSelectInput(session,"hurrTop",choices=append("All",as.character(hurTop10$hur_code)), selected="")
            dataCol[year(dataCol$date)==input$hurrYear & dataCol$hur_name==input$hurrName & dataCol$type=='A',]
          }
          
        } #end checkbox FALSE
        
        
        #THIS PART WAS REPLICATED & EXTENDED FROM THE ABOVE:
        else if(input$landfallCheckbox == TRUE & input$checkbox2005 == FALSE){ #TRUE
          #user selected hurricane, show hurricane on map
          if(input$hurrTop != "All" & input$hurrTop != ""){
            updateSelectInput(session,"hurrYear",choices=append("All",seq(dataRange2005[1],dataRange2005[2],by=1)), selected="")
            updateSelectInput(session,"hurrName",choices=append("All",as.character(hurMaxSpeed$hur_name)), selected="")
            dataCol[dataCol$hur_code==input$hurrTop & dataCol$landfall == 'yes' & dataCol$type=='A',]
          }
          #user selected All, show all hurricane
          else if(input$hurrTop == "All" & input$hurrTop!=""){
            updateSelectInput(session,"hurrYear",choices=append("All",seq(dataRange2005[1],dataRange2005[2],by=1)), selected="")
            updateSelectInput(session,"hurrName",choices=append("All",as.character(hurMaxSpeed$hur_name)), selected="")
            dataCol[dataCol$hur_code %in% hurTop10$hur_code & dataCol$landfall == 'yes' & dataCol$type=='A',]
          }
          #user selected hurrYear = All and hurrName = "All"
          else if(input$hurrYear == "All" & input$hurrName == "All"){
            updateSelectInput(session,"hurrTop",choices=append("All",as.character(hurTop10$hur_code)), selected="")
            dataCol[dataCol$landfall == 'yes' & dataCol$type=='A',]
          }
          #user selected hurrName = All, hurrYear to a year
          else if (input$hurrName == "All" & input$hurrYear != "All"){  #filter by year:
            updateSelectInput(session,"hurrTop",choices=append("All",as.character(hurTop10$hur_code)), selected="")
            dataYear <- dataCol[year(dataCol$date)==input$hurrYear & dataCol$landfall == 'yes' & dataCol$type=='A',]
            #updateSelectInput(session,"hurrName",choices=append("All",as.character(dataYear$hur_name)), selected="All")
            dataYear
          }
          #user selected hurrName to a name, hurrYear = All
          else if (input$hurrName != "All" & input$hurrYear == "All"){  #filter by name:
            updateSelectInput(session,"hurrTop",choices=append("All",as.character(hurTop10$hur_code)), selected="")
            #updateSelectInput(session,"hurrName",choices=append("All",as.character(dataCol$hur_name)), selected="All")
            dataCol[dataCol$hur_name==input$hurrName & dataCol$landfall == 'yes' & dataCol$type=='A',]
          }
          #user selected hurrName to a name, hurrYear to a year
          else{
            updateSelectInput(session,"hurrTop",choices=append("All",as.character(hurTop10$hur_code)), selected="")
            dataCol[year(dataCol$date)==input$hurrYear & dataCol$hur_name==input$hurrName & dataCol$landfall == 'yes' & dataCol$type=='A',]
          }
        }
        
        
        # DUPLICATED FOR 2005 / ALL FILTER: (The 2005 checkbox is true:)
        
        #THIS PART IS ORIGINAL:
        else if(input$landfallCheckbox == FALSE){
          #user selected hurricane, show hurricane on map
          if(input$hurrTop != "All" & input$hurrTop != ""){
            updateSelectInput(session,"hurrYear",choices=append("All",seq(dataRange2005[1],dataRange2005[2],by=1)), selected="")
            updateSelectInput(session,"hurrName",choices=append("All",as.character(hurMaxSpeed$hur_name)), selected="")
            dataCol2005[dataCol2005$hur_code==input$hurrTop & dataCol2005$type=='A',]
          }
          #user selected All, show all hurricane    
          else if(input$hurrTop == "All" & input$hurrTop!=""){
            updateSelectInput(session,"hurrYear",choices=append("All",seq(dataRange2005[1],dataRange2005[2],by=1)), selected="")
            updateSelectInput(session,"hurrName",choices=append("All",as.character(hurMaxSpeed$hur_name)), selected="")
            dataCol2005[dataCol2005$hur_code %in% hurTop10$hur_code & dataCol2005$type=='A',]
          }
          #user selected hurrYear = All and hurrName = "All"
          else if(input$hurrYear == "All" & input$hurrName == "All"){
            updateSelectInput(session,"hurrTop",choices=append("All",as.character(hurTop10$hur_code)), selected="")
            dataCol2005[dataCol2005$type=='A',]
          }
          #user selected hurrName = All, hurrYear to a year
          else if (input$hurrName == "All" & input$hurrYear != "All"){  #filter by year:
            updateSelectInput(session,"hurrTop",choices=append("All",as.character(hurTop10$hur_code)), selected="")
            dataYear <- dataCol2005[year(dataCol2005$date)==input$hurrYear & dataCol2005$type=='A',]
            #updateSelectInput(session,"hurrName",choices=append("All",as.character(dataYear$hur_name)), selected="All")
            dataYear
          }
          #user selected hurrName to a name, hurrYear = All
          else if (input$hurrName != "All" & input$hurrYear == "All"){  #filter by name:
            updateSelectInput(session,"hurrTop",choices=append("All",as.character(hurTop10$hur_code)), selected="")
            #updateSelectInput(session,"hurrName",choices=append("All",as.character(dataCol2005$hur_name)), selected="All")
            dataCol2005[dataCol2005$hur_name==input$hurrName & dataCol2005$type=='A',]
          }
          #user selected hurrName to a name, hurrYear to a year
          else{
            updateSelectInput(session,"hurrTop",choices=append("All",as.character(hurTop10$hur_code)), selected="")
            dataCol2005[year(dataCol2005$date)==input$hurrYear & dataCol2005$hur_name==input$hurrName & dataCol2005$type=='A',]
          }
          
        } #end checkbox FALSE
        
        
        else if (input$landfallCheckbox == TRUE){ #TRUE
          #user selected hurricane, show hurricane on map
          if(input$hurrTop != "All" & input$hurrTop != ""){
            updateSelectInput(session,"hurrYear",choices=append("All",seq(dataRange2005[1],dataRange2005[2],by=1)), selected="")
            updateSelectInput(session,"hurrName",choices=append("All",as.character(hurMaxSpeed$hur_name)), selected="")
            dataCol2005[dataCol2005$hur_code==input$hurrTop & dataCol2005$landfall == 'yes' & dataCol2005$type=='A',]
          }
          #user selected All, show all hurricane
          else if(input$hurrTop == "All" & input$hurrTop!=""){
            updateSelectInput(session,"hurrYear",choices=append("All",seq(dataRange2005[1],dataRange2005[2],by=1)), selected="")
            updateSelectInput(session,"hurrName",choices=append("All",as.character(hurMaxSpeed$hur_name)), selected="")
            dataCol2005[dataCol2005$hur_code %in% hurTop10$hur_code & dataCol2005$landfall == 'yes' & dataCol2005$type=='A',]
          }
          #user selected hurrYear = All and hurrName = "All"
          else if(input$hurrYear == "All" & input$hurrName == "All"){
            updateSelectInput(session,"hurrTop",choices=append("All",as.character(hurTop10$hur_code)), selected="")
            dataCol2005[dataCol2005$landfall == 'yes' & dataCol2005$type=='A',]
          }
          #user selected hurrName = All, hurrYear to a year
          else if (input$hurrName == "All" & input$hurrYear != "All"){  #filter by year:
            updateSelectInput(session,"hurrTop",choices=append("All",as.character(hurTop10$hur_code)), selected="")
            dataYear <- dataCol2005[year(dataCol2005$date)==input$hurrYear & dataCol2005$landfall == 'yes' & dataCol2005$type=='A',]
            #updateSelectInput(session,"hurrName",choices=append("All",as.character(dataYear$hur_name)), selected="All")
            dataYear
          }
          #user selected hurrName to a name, hurrYear = All
          else if (input$hurrName != "All" & input$hurrYear == "All"){  #filter by name:
            updateSelectInput(session,"hurrTop",choices=append("All",as.character(hurTop10$hur_code)), selected="")
            #updateSelectInput(session,"hurrName",choices=append("All",as.character(dataCol2005$hur_name)), selected="All")
            dataCol2005[dataCol2005$hur_name==input$hurrName & dataCol2005$landfall == 'yes' & dataCol2005$type=='A',]
          }
          #user selected hurrName to a name, hurrYear to a year
          else{
            updateSelectInput(session,"hurrTop",choices=append("All",as.character(hurTop10$hur_code)), selected="")
            dataCol2005[year(dataCol2005$date)==input$hurrYear & dataCol2005$hur_name==input$hurrName & dataCol2005$landfall == 'yes' & dataCol2005$type=='A',]
          }
        }
      }
    }
    
    #----- half point of filter()
    #---
    #-
    #DUPLICATED FOR PACIFIC HURRICANES:
    else{ #pacific = true
      
      
      if (input$atlanticCheckbox == FALSE){
        
        #THIS PART IS ORIGINAL:
        if(input$landfallCheckbox == FALSE & input$checkbox2005 == FALSE){
          #user selected hurricane, show hurricane on map
          if(input$hurrTop != "All" & input$hurrTop != ""){
            updateSelectInput(session,"hurrYear",choices=append("All",seq(dataRange2005[1],dataRange2005[2],by=1)), selected="")
            updateSelectInput(session,"hurrName",choices=append("All",as.character(hurMaxSpeed$hur_name)), selected="")
            dataCol[dataCol$hur_code==input$hurrTop & dataCol$type=='N',]
          }
          #user selected All, show all hurricane    
          else if(input$hurrTop == "All" & input$hurrTop!=""){
            updateSelectInput(session,"hurrYear",choices=append("All",seq(dataRange2005[1],dataRange2005[2],by=1)), selected="")
            updateSelectInput(session,"hurrName",choices=append("All",as.character(hurMaxSpeed$hur_name)), selected="")
            dataCol[dataCol$hur_code %in% hurTop10$hur_code & dataCol$type=='N',]
          }
          #user selected hurrYear = All and hurrName = "All"
          else if(input$hurrYear == "All" & input$hurrName == "All"){
            updateSelectInput(session,"hurrTop",choices=append("All",as.character(hurTop10$hur_code)), selected="")
            dataCol[dataCol$type=='N',]
          }
          #user selected hurrName = All, hurrYear to a year
          else if (input$hurrName == "All" & input$hurrYear != "All"){  #filter by year:
            updateSelectInput(session,"hurrTop",choices=append("All",as.character(hurTop10$hur_code)), selected="")
            dataYear <- dataCol[year(dataCol$date)==input$hurrYear & dataCol$type=='N',]
            #updateSelectInput(session,"hurrName",choices=append("All",as.character(dataYear$hur_name)), selected="All")
            dataYear
          }
          #user selected hurrName to a name, hurrYear = All
          else if (input$hurrName != "All" & input$hurrYear == "All"){  #filter by name:
            updateSelectInput(session,"hurrTop",choices=append("All",as.character(hurTop10$hur_code)), selected="")
            #updateSelectInput(session,"hurrName",choices=append("All",as.character(dataCol$hur_name)), selected="All")
            dataCol[dataCol$hur_name==input$hurrName & dataCol$type=='N',]
          }
          #user selected hurrName to a name, hurrYear to a year
          else{
            updateSelectInput(session,"hurrTop",choices=append("All",as.character(hurTop10$hur_code)), selected="")
            dataCol[year(dataCol$date)==input$hurrYear & dataCol$hur_name==input$hurrName & dataCol$type=='N',]
          }
          
        } #end checkbox FALSE
        
        
        #THIS PART WAS REPLICATED & EXTENDED FROM THE ABOVE:
        else if(input$landfallCheckbox == TRUE & input$checkbox2005 == FALSE){ #TRUE
          #user selected hurricane, show hurricane on map
          if(input$hurrTop != "All" & input$hurrTop != ""){
            updateSelectInput(session,"hurrYear",choices=append("All",seq(dataRange2005[1],dataRange2005[2],by=1)), selected="")
            updateSelectInput(session,"hurrName",choices=append("All",as.character(hurMaxSpeed$hur_name)), selected="")
            dataCol[dataCol$hur_code==input$hurrTop & dataCol$landfall == 'yes' & dataCol$type=='N',]
          }
          #user selected All, show all hurricane
          else if(input$hurrTop == "All" & input$hurrTop!=""){
            updateSelectInput(session,"hurrYear",choices=append("All",seq(dataRange2005[1],dataRange2005[2],by=1)), selected="")
            updateSelectInput(session,"hurrName",choices=append("All",as.character(hurMaxSpeed$hur_name)), selected="")
            dataCol[dataCol$hur_code %in% hurTop10$hur_code & dataCol$landfall == 'yes' & dataCol$type=='N',]
          }
          #user selected hurrYear = All and hurrName = "All"
          else if(input$hurrYear == "All" & input$hurrName == "All"){
            updateSelectInput(session,"hurrTop",choices=append("All",as.character(hurTop10$hur_code)), selected="")
            dataCol[dataCol$landfall == 'yes' & dataCol$type=='N',]
          }
          #user selected hurrName = All, hurrYear to a year
          else if (input$hurrName == "All" & input$hurrYear != "All"){  #filter by year:
            updateSelectInput(session,"hurrTop",choices=append("All",as.character(hurTop10$hur_code)), selected="")
            dataYear <- dataCol[year(dataCol$date)==input$hurrYear & dataCol$landfall == 'yes' & dataCol$type=='N',]
            #updateSelectInput(session,"hurrName",choices=append("All",as.character(dataYear$hur_name)), selected="All")
            dataYear
          }
          #user selected hurrName to a name, hurrYear = All
          else if (input$hurrName != "All" & input$hurrYear == "All"){  #filter by name:
            updateSelectInput(session,"hurrTop",choices=append("All",as.character(hurTop10$hur_code)), selected="")
            #updateSelectInput(session,"hurrName",choices=append("All",as.character(dataCol$hur_name)), selected="All")
            dataCol[dataCol$hur_name==input$hurrName & dataCol$landfall == 'yes' & dataCol$type=='N',]
          }
          #user selected hurrName to a name, hurrYear to a year
          else{
            updateSelectInput(session,"hurrTop",choices=append("All",as.character(hurTop10$hur_code)), selected="")
            dataCol[year(dataCol$date)==input$hurrYear & dataCol$hur_name==input$hurrName & dataCol$landfall == 'yes' & dataCol$type=='N',]
          }
        }
        
        
        # DUPLICATED FOR 2005 / ALL FILTER: (The 2005 checkbox is true:)
        
        #THIS PART IS ORIGINAL:
        else if(input$landfallCheckbox == FALSE){
          #user selected hurricane, show hurricane on map
          if(input$hurrTop != "All" & input$hurrTop != ""){
            updateSelectInput(session,"hurrYear",choices=append("All",seq(dataRange2005[1],dataRange2005[2],by=1)), selected="")
            updateSelectInput(session,"hurrName",choices=append("All",as.character(hurMaxSpeed$hur_name)), selected="")
            dataCol2005[dataCol2005$hur_code==input$hurrTop & dataCol2005$type=='N',]
          }
          #user selected All, show all hurricane    
          else if(input$hurrTop == "All" & input$hurrTop!=""){
            updateSelectInput(session,"hurrYear",choices=append("All",seq(dataRange2005[1],dataRange2005[2],by=1)), selected="")
            updateSelectInput(session,"hurrName",choices=append("All",as.character(hurMaxSpeed$hur_name)), selected="")
            dataCol2005[dataCol2005$hur_code %in% hurTop10$hur_code & dataCol2005$type=='N',]
          }
          #user selected hurrYear = All and hurrName = "All"
          else if(input$hurrYear == "All" & input$hurrName == "All"){
            updateSelectInput(session,"hurrTop",choices=append("All",as.character(hurTop10$hur_code)), selected="")
            dataCol2005[dataCol2005$type=='N',]
          }
          #user selected hurrName = All, hurrYear to a year
          else if (input$hurrName == "All" & input$hurrYear != "All"){  #filter by year:
            updateSelectInput(session,"hurrTop",choices=append("All",as.character(hurTop10$hur_code)), selected="")
            dataYear <- dataCol2005[year(dataCol2005$date)==input$hurrYear & dataCol2005$type=='N',]
            #updateSelectInput(session,"hurrName",choices=append("All",as.character(dataYear$hur_name)), selected="All")
            dataYear
          }
          #user selected hurrName to a name, hurrYear = All
          else if (input$hurrName != "All" & input$hurrYear == "All"){  #filter by name:
            updateSelectInput(session,"hurrTop",choices=append("All",as.character(hurTop10$hur_code)), selected="")
            #updateSelectInput(session,"hurrName",choices=append("All",as.character(dataCol2005$hur_name)), selected="All")
            dataCol2005[dataCol2005$hur_name==input$hurrName & dataCol2005$type=='N',]
          }
          #user selected hurrName to a name, hurrYear to a year
          else{
            updateSelectInput(session,"hurrTop",choices=append("All",as.character(hurTop10$hur_code)), selected="")
            dataCol2005[year(dataCol2005$date)==input$hurrYear & dataCol2005$hur_name==input$hurrName & dataCol2005$type=='N',]
          }
          
        } #end checkbox FALSE
        
        
        else if (input$landfallCheckbox == TRUE){ #TRUE
          #user selected hurricane, show hurricane on map
          if(input$hurrTop != "All" & input$hurrTop != ""){
            updateSelectInput(session,"hurrYear",choices=append("All",seq(dataRange2005[1],dataRange2005[2],by=1)), selected="")
            updateSelectInput(session,"hurrName",choices=append("All",as.character(hurMaxSpeed$hur_name)), selected="")
            dataCol2005[dataCol2005$hur_code==input$hurrTop & dataCol2005$landfall == 'yes' & dataCol2005$type=='N',]
          }
          #user selected All, show all hurricane
          else if(input$hurrTop == "All" & input$hurrTop!=""){
            updateSelectInput(session,"hurrYear",choices=append("All",seq(dataRange2005[1],dataRange2005[2],by=1)), selected="")
            updateSelectInput(session,"hurrName",choices=append("All",as.character(hurMaxSpeed$hur_name)), selected="")
            dataCol2005[dataCol2005$hur_code %in% hurTop10$hur_code & dataCol2005$landfall == 'yes' & dataCol2005$type=='N',]
          }
          #user selected hurrYear = All and hurrName = "All"
          else if(input$hurrYear == "All" & input$hurrName == "All"){
            updateSelectInput(session,"hurrTop",choices=append("All",as.character(hurTop10$hur_code)), selected="")
            dataCol2005[dataCol2005$landfall == 'yes' & dataCol2005$type=='N',]
          }
          #user selected hurrName = All, hurrYear to a year
          else if (input$hurrName == "All" & input$hurrYear != "All"){  #filter by year:
            updateSelectInput(session,"hurrTop",choices=append("All",as.character(hurTop10$hur_code)), selected="")
            dataYear <- dataCol2005[year(dataCol2005$date)==input$hurrYear & dataCol2005$landfall == 'yes' & dataCol2005$type=='N',]
            #updateSelectInput(session,"hurrName",choices=append("All",as.character(dataYear$hur_name)), selected="All")
            dataYear
          }
          #user selected hurrName to a name, hurrYear = All
          else if (input$hurrName != "All" & input$hurrYear == "All"){  #filter by name:
            updateSelectInput(session,"hurrTop",choices=append("All",as.character(hurTop10$hur_code)), selected="")
            #updateSelectInput(session,"hurrName",choices=append("All",as.character(dataCol2005$hur_name)), selected="All")
            dataCol2005[dataCol2005$hur_name==input$hurrName & dataCol2005$landfall == 'yes' & dataCol2005$type=='N',]
          }
          #user selected hurrName to a name, hurrYear to a year
          else{
            updateSelectInput(session,"hurrTop",choices=append("All",as.character(hurTop10$hur_code)), selected="")
            dataCol2005[year(dataCol2005$date)==input$hurrYear & dataCol2005$hur_name==input$hurrName & dataCol2005$landfall == 'yes' & dataCol2005$type=='N',]
          }
        }
      }
    
      
      #DUPLICATED FOR ATLANTIC OCEAN:
      
      else{ #atlantic == TRUE
        
        #THIS PART IS ORIGINAL:
        if(input$landfallCheckbox == FALSE & input$checkbox2005 == FALSE){
          #user selected hurricane, show hurricane on map
          if(input$hurrTop != "All" & input$hurrTop != ""){
            updateSelectInput(session,"hurrYear",choices=append("All",seq(dataRange2005[1],dataRange2005[2],by=1)), selected="")
            updateSelectInput(session,"hurrName",choices=append("All",as.character(hurMaxSpeed$hur_name)), selected="")
            dataCol[dataCol$hur_code==input$hurrTop & (dataCol$type=='A' | dataCol$type=='N'),]   #realized
          }
          #user selected All, show all hurricane    
          else if(input$hurrTop == "All" & input$hurrTop!=""){
            updateSelectInput(session,"hurrYear",choices=append("All",seq(dataRange2005[1],dataRange2005[2],by=1)), selected="")
            updateSelectInput(session,"hurrName",choices=append("All",as.character(hurMaxSpeed$hur_name)), selected="")
            dataCol[dataCol$hur_code %in% hurTop10$hur_code & (dataCol$type=='A' | dataCol$type=='N'),]
          }
          #user selected hurrYear = All and hurrName = "All"
          else if(input$hurrYear == "All" & input$hurrName == "All"){
            updateSelectInput(session,"hurrTop",choices=append("All",as.character(hurTop10$hur_code)), selected="")
            dataCol[dataCol$type=='A' | dataCol$type=='N',]
          }
          #user selected hurrName = All, hurrYear to a year
          else if (input$hurrName == "All" & input$hurrYear != "All"){  #filter by year:
            updateSelectInput(session,"hurrTop",choices=append("All",as.character(hurTop10$hur_code)), selected="")
            dataYear <- dataCol[year(dataCol$date)==input$hurrYear & (dataCol$type=='A' | dataCol$type=='N'),]
            #updateSelectInput(session,"hurrName",choices=append("All",as.character(dataYear$hur_name)), selected="All")
            dataYear
          }
          #user selected hurrName to a name, hurrYear = All
          else if (input$hurrName != "All" & input$hurrYear == "All"){  #filter by name:
            updateSelectInput(session,"hurrTop",choices=append("All",as.character(hurTop10$hur_code)), selected="")
            #updateSelectInput(session,"hurrName",choices=append("All",as.character(dataCol$hur_name)), selected="All")
            dataCol[dataCol$hur_name==input$hurrName & (dataCol$type=='A' | dataCol$type=='N'),]
          }
          #user selected hurrName to a name, hurrYear to a year
          else{
            updateSelectInput(session,"hurrTop",choices=append("All",as.character(hurTop10$hur_code)), selected="")
            dataCol[year(dataCol$date)==input$hurrYear & dataCol$hur_name==input$hurrName & (dataCol$type=='A' | dataCol$type=='N'),]
          }
          
        } #end checkbox FALSE
        
        
        #THIS PART WAS REPLICATED & EXTENDED FROM THE ABOVE:
        else if(input$landfallCheckbox == TRUE & input$checkbox2005 == FALSE){ #TRUE
          #user selected hurricane, show hurricane on map
          if(input$hurrTop != "All" & input$hurrTop != ""){
            updateSelectInput(session,"hurrYear",choices=append("All",seq(dataRange2005[1],dataRange2005[2],by=1)), selected="")
            updateSelectInput(session,"hurrName",choices=append("All",as.character(hurMaxSpeed$hur_name)), selected="")
            dataCol[dataCol$hur_code==input$hurrTop & dataCol$landfall == 'yes' & (dataCol$type=='A' | dataCol$type=='N'),]
          }
          #user selected All, show all hurricane
          else if(input$hurrTop == "All" & input$hurrTop!=""){
            updateSelectInput(session,"hurrYear",choices=append("All",seq(dataRange2005[1],dataRange2005[2],by=1)), selected="")
            updateSelectInput(session,"hurrName",choices=append("All",as.character(hurMaxSpeed$hur_name)), selected="")
            dataCol[dataCol$hur_code %in% hurTop10$hur_code & dataCol$landfall == 'yes' & (dataCol$type=='A' | dataCol$type=='N'),]
          }
          #user selected hurrYear = All and hurrName = "All"
          else if(input$hurrYear == "All" & input$hurrName == "All"){
            updateSelectInput(session,"hurrTop",choices=append("All",as.character(hurTop10$hur_code)), selected="")
            dataCol[dataCol$landfall == 'yes' & (dataCol$type=='A' | dataCol$type=='N'),]
          }
          #user selected hurrName = All, hurrYear to a year
          else if (input$hurrName == "All" & input$hurrYear != "All"){  #filter by year:
            updateSelectInput(session,"hurrTop",choices=append("All",as.character(hurTop10$hur_code)), selected="")
            dataYear <- dataCol[year(dataCol$date)==input$hurrYear & dataCol$landfall == 'yes' & (dataCol$type=='A' | dataCol$type=='N'),]
            #updateSelectInput(session,"hurrName",choices=append("All",as.character(dataYear$hur_name)), selected="All")
            dataYear
          }
          #user selected hurrName to a name, hurrYear = All
          else if (input$hurrName != "All" & input$hurrYear == "All"){  #filter by name:
            updateSelectInput(session,"hurrTop",choices=append("All",as.character(hurTop10$hur_code)), selected="")
            #updateSelectInput(session,"hurrName",choices=append("All",as.character(dataCol$hur_name)), selected="All")
            dataCol[dataCol$hur_name==input$hurrName & dataCol$landfall == 'yes' & (dataCol$type=='A' | dataCol$type=='N'),]
          }
          #user selected hurrName to a name, hurrYear to a year
          else{
            updateSelectInput(session,"hurrTop",choices=append("All",as.character(hurTop10$hur_code)), selected="")
            dataCol[year(dataCol$date)==input$hurrYear & dataCol$hur_name==input$hurrName & dataCol$landfall == 'yes' & (dataCol$type=='A' | dataCol$type=='N'),]
          }
        }
        
        
        # DUPLICATED FOR 2005 / ALL FILTER: (The 2005 checkbox is true:)
        
        #THIS PART IS ORIGINAL:
        else if(input$landfallCheckbox == FALSE){
          #user selected hurricane, show hurricane on map
          if(input$hurrTop != "All" & input$hurrTop != ""){
            updateSelectInput(session,"hurrYear",choices=append("All",seq(dataRange2005[1],dataRange2005[2],by=1)), selected="")
            updateSelectInput(session,"hurrName",choices=append("All",as.character(hurMaxSpeed$hur_name)), selected="")
            dataCol2005[dataCol2005$hur_code==input$hurrTop & (dataCol2005$type=='A' | dataCol2005$type=='N'),]
          }
          #user selected All, show all hurricane    
          else if(input$hurrTop == "All" & input$hurrTop!=""){
            updateSelectInput(session,"hurrYear",choices=append("All",seq(dataRange2005[1],dataRange2005[2],by=1)), selected="")
            updateSelectInput(session,"hurrName",choices=append("All",as.character(hurMaxSpeed$hur_name)), selected="")
            dataCol2005[dataCol2005$hur_code %in% hurTop10$hur_code & (dataCol2005$type=='A' | dataCol2005$type=='N'),]
          }
          #user selected hurrYear = All and hurrName = "All"
          else if(input$hurrYear == "All" & input$hurrName == "All"){
            updateSelectInput(session,"hurrTop",choices=append("All",as.character(hurTop10$hur_code)), selected="")
            dataCol2005[dataCol2005$type=='A' | dataCol2005$type=='N',]
          }
          #user selected hurrName = All, hurrYear to a year
          else if (input$hurrName == "All" & input$hurrYear != "All"){  #filter by year:
            updateSelectInput(session,"hurrTop",choices=append("All",as.character(hurTop10$hur_code)), selected="")
            dataYear <- dataCol2005[year(dataCol2005$date)==input$hurrYear & (dataCol2005$type=='A' | dataCol2005$type=='N'),]
            #updateSelectInput(session,"hurrName",choices=append("All",as.character(dataYear$hur_name)), selected="All")
            dataYear
          }
          #user selected hurrName to a name, hurrYear = All
          else if (input$hurrName != "All" & input$hurrYear == "All"){  #filter by name:
            updateSelectInput(session,"hurrTop",choices=append("All",as.character(hurTop10$hur_code)), selected="")
            #updateSelectInput(session,"hurrName",choices=append("All",as.character(dataCol2005$hur_name)), selected="All")
            dataCol2005[dataCol2005$hur_name==input$hurrName & (dataCol2005$type=='A' | dataCol2005$type=='N'),]
          }
          #user selected hurrName to a name, hurrYear to a year
          else{
            updateSelectInput(session,"hurrTop",choices=append("All",as.character(hurTop10$hur_code)), selected="")
            dataCol2005[year(dataCol2005$date)==input$hurrYear & dataCol2005$hur_name==input$hurrName & (dataCol2005$type=='A' | dataCol2005$type=='N'),]
          }
          
        } #end checkbox FALSE
        
        
        else if (input$landfallCheckbox == TRUE){ #TRUE
          #user selected hurricane, show hurricane on map
          if(input$hurrTop != "All" & input$hurrTop != ""){
            updateSelectInput(session,"hurrYear",choices=append("All",seq(dataRange2005[1],dataRange2005[2],by=1)), selected="")
            updateSelectInput(session,"hurrName",choices=append("All",as.character(hurMaxSpeed$hur_name)), selected="")
            dataCol2005[dataCol2005$hur_code==input$hurrTop & dataCol2005$landfall == 'yes' & (dataCol2005$type=='A' | dataCol2005$type=='N'),]
          }
          #user selected All, show all hurricane
          else if(input$hurrTop == "All" & input$hurrTop!=""){
            updateSelectInput(session,"hurrYear",choices=append("All",seq(dataRange2005[1],dataRange2005[2],by=1)), selected="")
            updateSelectInput(session,"hurrName",choices=append("All",as.character(hurMaxSpeed$hur_name)), selected="")
            dataCol2005[dataCol2005$hur_code %in% hurTop10$hur_code & dataCol2005$landfall == 'yes' & (dataCol2005$type=='A' | dataCol2005$type=='N'),]
          }
          #user selected hurrYear = All and hurrName = "All"
          else if(input$hurrYear == "All" & input$hurrName == "All"){
            updateSelectInput(session,"hurrTop",choices=append("All",as.character(hurTop10$hur_code)), selected="")
            dataCol2005[dataCol2005$landfall == 'yes' & (dataCol2005$type=='A' | dataCol2005$type=='N'),]
          }
          #user selected hurrName = All, hurrYear to a year
          else if (input$hurrName == "All" & input$hurrYear != "All"){  #filter by year:
            updateSelectInput(session,"hurrTop",choices=append("All",as.character(hurTop10$hur_code)), selected="")
            dataYear <- dataCol2005[year(dataCol2005$date)==input$hurrYear & dataCol2005$landfall == 'yes' & (dataCol2005$type=='A' | dataCol2005$type=='N'),]
            #updateSelectInput(session,"hurrName",choices=append("All",as.character(dataYear$hur_name)), selected="All")
            dataYear
          }
          #user selected hurrName to a name, hurrYear = All
          else if (input$hurrName != "All" & input$hurrYear == "All"){  #filter by name:
            updateSelectInput(session,"hurrTop",choices=append("All",as.character(hurTop10$hur_code)), selected="")
            #updateSelectInput(session,"hurrName",choices=append("All",as.character(dataCol2005$hur_name)), selected="All")
            dataCol2005[dataCol2005$hur_name==input$hurrName & dataCol2005$landfall == 'yes' & (dataCol2005$type=='A' | dataCol2005$type=='N'),]
          }
          #user selected hurrName to a name, hurrYear to a year
          else{
            updateSelectInput(session,"hurrTop",choices=append("All",as.character(hurTop10$hur_code)), selected="")
            dataCol2005[year(dataCol2005$date)==input$hurrYear & dataCol2005$hur_name==input$hurrName & dataCol2005$landfall == 'yes' & (dataCol2005$type=='A' | dataCol2005$type=='N'),]
          }
        }
      }
    }
    
  ) #end filter reactive function
  
  
  #Reactive functions for line plots:
  
  #creates table for lineGraph of max wind speed
  filterWindA <- reactive(
    #get max windspeed per day for specified year
    if(input$hurrYear != "All" ){
      maxAtlantic <- data[data$type=="A" & year(data$date)==input$hurrYear,]
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
      maxAtlantic <- data[data$type=="A",]
      maxAtlantic <- maxAtlantic[,c("date","max_speed")]
      maxAtlantic$date <- year(maxAtlantic$date)
      maxASpeed <- aggregate(. ~date,maxAtlantic,max)
      maxASpeed
    }
  )
  
  filterWindN <- reactive(
    #get max windspeed per day for specified year
    if(input$hurrYear != "All"){
      maxNorth <- data[data$type=="N" & year(data$date)==input$hurrYear,]
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
      maxNorth <- data[data$type=="N" ,]
      maxNorth <- maxNorth[,c("date","max_speed")]
      maxNorth$date <- year(maxNorth$date)
      maxNSpeed <- aggregate(. ~date,maxNorth, max)
      maxNSpeed
    }
  )
  
  filterMinA <- reactive(
    if(input$hurrYear != "All"){
      minAtlantic <- data[data$type =="A" & year(data$date)==input$hurrYear,]
      minAtlantic <- minAtlantic[,c("date","min_pressure")]
      minAtlantic$day <- day(minAtlantic$date)
      minAtlantic$month <- month(minAtlantic$date)
      minAtlantic$year <- year(minAtlantic$date)
      minASpeed <-aggregate(. ~year+month+day,minAtlantic, max)
      minASpeed$date <- paste(minASpeed$year,minASpeed$month,minASpeed$day,sep="-") %>% ymd()
      minASpeed
    }
    else{
      minAtlantic <- data[data$type=="A",]
      minAtlantic <- minAtlantic[,c("date","min_pressure")]
      minAtlantic$date <- year(minAtlantic$date)
      minASpeed <- aggregate(. ~date, minAtlantic, max)
      minASpeed
    }
  )
  
  filterMinN <- reactive(
    if(input$hurrYear != "All"){
      minNorth <- data[data$type=="N" & year(data$date)==input$hurrYear, ]
      minNorth <- minNorth[,c("date","min_pressure")]
      minNorth$day <- day(minNorth$date)
      minNorth$month <- month(minNorth$date)
      minNorth$year <- year(minNorth$date)
      if(nrow(minNorth)==0 ){
        minNSpeed <- data.frame(date = as.Date(character()),
                                min_pressure=integer())
      }else{
        minNSpeed <- aggregate(. ~year+month+day, minNorth, max)
        minNSpeed$date <- paste(minNSpeed$year,minNSpeed$month,minNSpeed$day,sep="-") %>% ymd()
      }
      minNSpeed
    }
    else{
      minNorth <- data[data$type=='N',]
      minNorth <- minNorth[,c("date","min_pressure")]
      minNorth$date <- year(minNorth$date)
      minNSpeed <- aggregate(. ~date, minNorth, max)
      minNSpeed
    }
  )
  
  # #top10 list
  # hurrTopR <- reactive(
  #   if(input$hurrTop == "All"){
  #     data 
  #   }
  # )
  
  
  data$year <-year(data$date)
  data$year <- as.character(data$year)
  
  
  #reactive function used in bar charts
  #filters only by specific hurrYear and by 2005+
  userReactive <- reactive(
    if (input$checkbox2005 == TRUE){
      if(input$hurrYear != "All"){
        subset(classifiedHurricanes, classifiedHurricanes$date == input$hurrYear & classifiedHurricanes$date >= 2005)
      }
      else
        subset(classifiedHurricanes, classifiedHurricanes$date >= 2005)
    }
    else{
      if(input$hurrYear != "All"){
        subset(classifiedHurricanes, classifiedHurricanes$date == input$hurrYear)
      }
      else
        classifiedHurricanes
    }
  )
  
  
  # BACKEND LAYER: ---- backend components here: ------------------------------------------------------
  
  #BAR CHARTS:
  
  # Hurricanes with respect to their max category 
  output$bargraph3 <- renderPlot({
    
    hurPerYearReactive <- userReactive()
    
    #reorder bar chart bars
    positions <- c("TD", "TS", "1", "2", "3", "4", "5")
    
    ggplot(data = hurPerYearReactive, aes(x = category, fill=type)) +
      geom_bar(stat="count", position = position_dodge(preserve = 'single')) +
      xlab("Category") + ylab("Number of Hurricanes") + 
      scale_fill_manual(values=c('navy', '#e4283B'), name="Ocean", labels = c("Atlantic", "Pacific")) +
      scale_x_discrete(limits = positions) +
      theme(text = element_text(size = 18)) +
      scale_y_continuous(breaks= pretty_breaks())
  }) # End bargraph3
  
  
  
  # Number of Hurricanes per year
  output$bargraph1 <- renderPlot({
    hurPerYearReactive <- userReactive()
    
    #get min and max year for chart
    min_year <- min(hurPerYearReactive$date)
    if (min_year==1851 | min_year==1852){min_year=1850}   #subtract 1 year to show proper decades (1860, 1870)
    max_year <- max(hurPerYearReactive$date)
    
    ggplot(hurPerYearReactive, aes(x = date, fill=type, width=0.3)) +
      xlab("Year") + ylab("Number of Hurricanes") +
      geom_bar(width=0.3) +
      scale_fill_manual(values=c('navy', '#e4283B'), name="Ocean", labels = c("Atlantic", "Pacific")) +
      scale_x_continuous(breaks = seq(min_year, max_year, by=10)) +
      theme(text = element_text(size = 18))  +
      scale_y_continuous(breaks= pretty_breaks())
  }) # End bargraph1
  
  
  
  # Stacked Bar Graph Displaying Num of Hurricane Including Categories since 2005
  output$bargraph4 <- renderPlot({
    
    hurPerYearReactive <- userReactive()
    #reassign the level of category column:
    hurPerYearReactive$category <- factor(hurPerYearReactive$category, 
                                          levels = rev(c("TD", "TS", "1", "2", "3", "4", "5")))
    
    #get min and max year for chart
    min_year <- min(hurPerYearReactive$date)
    if (min_year==1851 | min_year==1852){min_year=1850}   #subtract 1 year to show proper decades (1860, 1870)
    max_year <- max(hurPerYearReactive$date)
    
    (ggplot(data = hurPerYearReactive, aes(x = date, fill = category)) +
        geom_bar(position ="stack", stat = "count") + 
        xlab("Year") + ylab("Number of Hurricanes") + 
        scale_fill_manual(values=rev(c('#488f31', '#6dac78', '#b7cf8d', '#fff1af', '#f5bb78', '#e98058', '#de425b')), name="Category") + #color theme
        scale_x_continuous(breaks = seq(min_year, max_year, by=10)) + #labels on x axis
        theme(text = element_text(size = 18)) +
        scale_y_continuous(breaks= pretty_breaks())                      #size of text
    ) # end ggplot
    
  }) # End bargraph4
  
  
  
  # add a leaflet map
  output$leaf <- renderLeaflet({
    
    #get reactive data:
    reactData <- filter() #filter data
    
    # Create a continuous palette function (from leaflet documentation)
    pal <- colorNumeric(
      palette = "Reds",
      domain = reactData$max_speed)
    
    #map object
    map <- leaflet(data = reactData) %>%
      addTiles() %>%
      addProviderTiles(mapRenderings[[input$mapRender]]) %>%
      setView(lng = -90, lat = 39.1, zoom = 3) %>%
      addCircles(  
        lng = reactData$lon, lat = reactData$lat, 
        color = pal(reactData$max_speed), 
        radius = 2,
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
      reactData <- filter() #use filtered data
      a1 <- aggregate(. ~hur_code+hur_name+landfall+year(date), reactData[,c(1:3,10,4)], max)[0:5]
      a2 <- aggregate(. ~hur_code+type, reactData[,c(1,11,12)], min)
      
      combined <- merge(a1, a2, by="hur_code")
      
      #add a category column to combined:
      combined$category[combined$max_speed <= 33] <- 'TD'
      combined$category[combined$max_speed >= 34 & combined$max_speed <= 63] <- 'TS'
      combined$category[combined$max_speed >= 64 & combined$max_speed <= 82] <- '1'
      combined$category[combined$max_speed >= 83  & combined$max_speed <= 95] <- '2'
      combined$category[combined$max_speed >= 96 & combined$max_speed <= 112] <- '3'
      combined$category[combined$max_speed >= 113 & combined$max_speed <= 136] <- '4'
      combined$category[combined$max_speed >= 137] <- '5'
      
      #change name of A and N
      combined$type[combined$type == 'A'] <- 'Atlantic'
      combined$type[combined$type == 'N'] <- 'Pacific'
      
      #rename columns
      columns <- c("Codename", "Name","Landfall", "Year", "Max Speed", "Ocean", "Min Pressure", "Category")
      colnames(combined) <- columns
      
      #reorder table
      combined <- combined[, c(1,2,4,6,5,7,3,8)]
      combined[2:8]  #run datatable
      
    },
    options = list(searching = TRUE, pageLength = 12, lengthChange = FALSE
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
        ggtitle(paste("Wind Speed for Year ",input$hurrYear))+
        theme(text = element_text(size = 18)) 
      graph
    }
    else{
      graph = ggplot() +
        geom_line(data=maxASpeed, aes(x=date,y=max_speed, colour="Atlantic")) +
        geom_line(data=maxNSpeed, aes(x=date,y=max_speed, colour="Pacific")) +
        scale_colour_manual("",
                            breaks = c("Atlantic","Pacific"),
                            values = c("red","navy"))+
        xlab("Dates")+
        ylab("Max Wind Speed")+
        ggtitle(paste("Wind Speed for Year ",input$hurrYear))+
        theme(text = element_text(size = 18)) 
      graph
    }
  }) # End line graph for max wind speed
  
  output$minPressure <-renderPlot({
    minASpeed <-filterMinA()
    minNSpeed <- filterMinN()
    if(nrow(minNSpeed) == 0 ){
      graph = ggplot() +
        geom_line(data=minASpeed, aes(x=date,y=min_pressure, colour="Atlantic")) +
        scale_colour_manual("",
                            breaks = c("Atlantic"),
                            values = c("red"))+
        xlab("Dates")+
        ylab("Max Wind Speed")+
        ggtitle(paste("Min Pressure for Year ",input$hurrYear))+
        theme(text = element_text(size = 18)) 
      graph
    }
    
    else{
      graph = ggplot() +
        geom_line(data=minASpeed, aes(x=date,y=min_pressure, colour="Atlantic")) +
        geom_line(data=minNSpeed, aes(x=date,y=min_pressure, colour="Pacific")) +
        scale_colour_manual("",
                            breaks = c("Atlantic","Pacific"),
                            values = c("red","navy"))+
        xlab("Dates")+
        ylab("Max Wind Speed")+
        ggtitle(paste("Min Pressure for Year ",input$hurrYear))+
        theme(text = element_text(size = 18)) 
      graph
    }
  })
  
  
  output$explanation <- renderText(
    print("The Pacific and Atlantic Oceans are expansive hubs of hurricane activity. Each
          year, numerous hurricanes form over the oceans and move, finally dying out as
          they reach land. Our team used data since 1851 to the present to visualize and
          analyze these movements.")
  )
  
  #info box
  output$progressBox <- renderInfoBox({
    numberData = 0   #number to display on progressbar
    reactData <- filter()
    if (nrow(reactData) > 0){
      aggregated <- aggregate(. ~hur_code+hur_name, reactData[,c(1,3,10)], min) #aggregate to show actual amount of hurs.
      numberData = nrow(aggregated)  #number of hurricanes shown on map
    }  
    
    infoBox(
      "Count", paste("Displaying ", numberData, " hurricanes"), icon = icon("chart-bar"),
      color = "black"
    )
    
  })
  
  
  #data components above  ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
  
}#end server block

shinyApp(ui = ui, server = server)  #use ui and server in shiny app