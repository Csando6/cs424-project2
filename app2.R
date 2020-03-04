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

data2 <- data[year(data$date)>2005,]
#data2 <- year(data$date)

data5 <- range(year(data2$date))
#data3 <- select(hur_code, hur_name)
#data3 <- aggregate(data2, by=list(hurr_code=data2$hur_code, hurr_name=data2$hur_name) )
data3 <- data2 %>% group_by(hur_code,hur_name) %>% summarize(max(max_speed))
data3 <- data3[order(data3$hur_name,decreasing = FALSE),]

data3 <- data2 %>% group_by(hur_code,hur_name) %>% summarize(max_speed =max(max_speed))


data3 <- data2 %>% group_by(hur_code,hur_name, max_speed) %>% filter(max_speed == max(max_speed)) 
data3 <- data3[order(data3$hur_name,decreasing = FALSE),]


temp1 <- year(data2$date)
temp2 <- data2[year(data2$date)==2010,]

