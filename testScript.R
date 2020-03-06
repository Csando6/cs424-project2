
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
data1year <- data[year(data$date) == 2018,] # | year(data$date)<=2011,]
#data1year <- data1year[seq(1, nrow(data1year), 2), ]  #get every other point

#getting data from 2005 and onwards
data2 <- data[year(data$date) >= 2005,]

#select columns
#data2 <- data2[c(0:2, 4:11)]

#getting code and name of hurricanes, saving max windspeed of hurricane from 2005 and onwards
data3 <- data2 %>% group_by(hur_code, hur_name) %>% summarize()#max_speed = max(max_speed))
data3 <- data3[order(data3$hur_name, decreasing = FALSE),]

#getting top 10 hurrican speed
data4 <- data3[order(data3$max_speed, decreasing = TRUE),]
data4 <- data4[1:10,]

#range of hurrican data
data5 <- range(year(data2$date))

#yearOfHurricanes <-year(data$date)

# hurrByYear <- data2 %>% group_by(hur_code, hur_name, year(date)) %>% summarize(max_speed =max(max_speed))


