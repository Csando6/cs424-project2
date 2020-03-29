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
data <- read.csv(file = 'AtlanticHurrica-cleaned.csv', sep = ",", header = TRUE)
data$date <- ymd(data$date)
data$type <- "A"

#marks data2 with N to identify it with NorthEast
data2 <- read.csv(file="NortheastAndNor-cleaned.csv", sep=",", header=TRUE)
data2$date <- ymd(data2$date)
data2$type <- "N"

#combines both data tables into one
data <- rbind(data,data2)
data2005 <- data
data2005$min_pressure[data2005$min_pressure == -999] <- NA

#data2005Temp <- data2005[data2005$hur_code == "EP052013" ,]

input <- NULL
input$hurrYear <- "All"

maxAtlantic <- data2005[data2005$type=="A",]
maxAtlantic <- maxAtlantic[,c("date","max_speed")]
maxAtlantic$date <- year(maxAtlantic$date)
maxASpeed <- aggregate(. ~date,maxAtlantic,max)
maxASpeed

maxNorth <- data2005[data2005$type=="N" ,]
maxNorth <- maxNorth[,c("date","max_speed")]
maxNorth$date <- year(maxNorth$date)
maxNSpeed <- aggregate(. ~date,maxNorth, max)
maxNSpeed

minAtlantic <- data2005[data2005$type=="A",]
minAtlantic <- minAtlantic[,c("date","min_pressure")]
minAtlantic$date <- year(minAtlantic$date)
minASpeed <- aggregate(. ~date, minAtlantic, max)

minNorth <- data2005[data2005$type=='N',]
minNorth <- minNorth[,c("date","min_pressure")]
minNorth$date <- year(minNorth$date)
minNSpeed <- aggregate(. ~date, minNorth, max)

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


graph = ggplot() +
  geom_line(data=minASpeed, aes(x=date,y=min_pressure, colour="Atlantic")) +
  geom_line(data=minNSpeed, aes(x=date,y=min_pressure, colour="North")) +
  scale_colour_manual("",
                      breaks = c("Atlantic","North"),
                      values = c("red","blue"))+
  xlab("Dates")+
  ylab("Max Wind Speed")+
  ggtitle(paste("Wind Speed for Year ",input$hurrYear))
graph






