library(shiny)
library(shinydashboard)
library(ggplot2)
library(lubridate)
library(DT)
library(jpeg)
library(grid)
library(leaflet)
library(scales)
# 
# rawdata <- read.table(file = "https://www.evl.uic.edu/aej/424/litterati%20challenge-65.csv", sep = ",", header = TRUE)
data <- read.csv(file = 'cleaned_hurricane_data.csv', sep = ",", header = TRUE)
# 
# head(data)
# data

