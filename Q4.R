library(ggplot2)
library(shiny)
library(dplyr)
library(htmltools)
library(leaflet)
library(scales)
library(datasets)

data <- read.csv("assignment-02-data-formated.csv",header=T)  
#reading the given datset into data

data[,"value"] <- as.numeric(sub("%", "",data$"value",fixed=TRUE))  
#converting the value column to numeric type and substituting $ with ""

#Q4.

leaflet(data = data) %>% addTiles() %>%
  addMarkers(~longitude, ~latitude, popup = ~as.character(location))
#using leaflet to plot sites with popup option as sitenames
# The symbol %>% acts as a pipe opeartor and takes outpt of 
#previous operator as input for next operator 

