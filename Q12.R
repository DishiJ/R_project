library(ggplot2)
library(shiny)
library(dplyr)
library(htmltools)
library(leaflet)
library(scales)
library(datasets)

#Q1.

data <- read.csv("assignment-02-data-formated.csv",header=T)  
                                #reading the given datset into data
                                                                
data[,"value"] <- as.numeric(sub("%", "",data$"value",fixed=TRUE))  
              #converting the value column to numeric type and substituting $ with ""


#Q2.

ggplot(data,aes(x=year,y=value)) +  
  geom_point() +
  facet_grid(coralType~reorder(location,latitude)) +   
                          #creating facets based on location and ordering by latitude
  geom_smooth(method="lm",se=FALSE) +                      #line smoothing
  labs(y="Bleaching Value in Percentage", x="Year",      
       title = "BLEACHING OF CORALS OVER THE YEARS IN EACH LOCATION") + 
                                         #labels for X,Y axis and title of plot
  theme(plot.title = element_text(hjust = 0.5))             #to get title in center 


