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
dataT <- data2005[,c("hur_code","hur_name","max_speed")]
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


#aggregate by day, saving max wind speed
input <-NULL
input$hurYear <- 2018

maxAtlantic <- data2005[data2005$type=="A" & year(data2005$date)==input$hurYear,]
maxAtlantic <- maxAtlantic[,c("date","max_speed")]
maxAtlantic$day <- day(maxAtlantic$date)
maxAtlantic$month <- month(maxAtlantic$date)
maxAtlantic$year <- year(maxAtlantic$date)
maxAtlantic$date <- NULL
maxASpeed <- aggregate(. ~year+month+day,maxAtlantic,max)
maxASpeed$date <- paste(maxASpeed$year,maxASpeed$month,maxASpeed$day,sep="-") %>% ymd()

maxNorth <- data2005[data2005$type=="N" & year(data2005$date)==input$hurYear,]
maxNorth <- maxNorth[,c("date","max_speed")]
maxNorth$day <- day(maxNorth$date)
maxNorth$month <- month(maxNorth$date)
maxNorth$year <- year(maxNorth$date)
maxNorth$date <- NULL
maxNSpeed <- aggregate(. ~year+month+day,maxNorth, max)
maxNSpeed$date <- paste(maxNSpeed$year,maxNSpeed$month,maxNSpeed$day,sep="-") %>% ymd()

graph = ggplot() +
  geom_line(data=maxASpeed, aes(x=date,y=max_speed, colour="Atlantic")) +
  geom_line(data=maxNSpeed, aes(x=date,y=max_speed, colour="North")) +
  scale_colour_manual("",
                     breaks = c("Atlantic","North"),
                     values = c("red","blue"))+
  xlab("Dates")+
  ylab("Max Wind Speed")+
  ggtitle(paste("Wind Speed for Year ",input$hurYear))

print(graph)



