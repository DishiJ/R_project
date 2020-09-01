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

ui <- shinyUI(fluidPage( 
  
  # Application title
  headerPanel("LOCATION WISE BLEACHING OF CORALS OVER THE YEARS WITH LOCATIONS OF SITES"),

  sidebarLayout(
    sidebarPanel(
      div(style="display: inline-block;vertical-align:top;width: 200px;",
                              #setting the style of the selectinput
          selectInput("variable", "Coraltype:", 
                                      #variable storing input of coralType
                      c("Blue Corals" = "Blue Corals", 
                        "Hard Corals" = "Hard Corals",
                        "Sea Fans" = "Sea Fans",
                        "Sea Pens" = "Sea Pens",
                        "Soft Corals" = "Soft Corals")
          )),
      
      div(style="display: inline-block;vertical-align:top;width: 200px;",
                          #setting the style of the selectinput
          selectInput("variable2", "SmootherType:", 
                                   #variable2 storing input of method of smoothing
                      c("Linear Model" = "lm", 
                        "Generalised Linear Model" = "glm",
                        "Generalised Additive Model" = "gam",
                        "Local Regression(loess)" = "loess")
          ))
      
    ),
    
    
    mainPanel(
      h3(textOutput("caption"),align="center"),
            #caption will store caption for main plot
      plotOutput("coralplot",width="100%"),
            #coralplot to represent main plot while connecting with server.R
      h3(textOutput("caption2"),align="center"),
            #caption2 will store caption for leaflet map
      leafletOutput("locationmap")
            #locationmap to represent location map while connecting with server.R
    
    )
  )
))


server <- shinyServer(function(input, output) {
  
  
  output$caption <- reactiveText(function() {
    paste(input$variable,"bleaching value at different sites over the years, 
          with smoothing method as",input$variable2)
  })        #Caption of output plot using input variable
  
  output$coralplot <- renderPlot({
    # check for the input variable
    if (input$variable == "Blue Corals") {
      
      coraldata <- data[data$coralType == "blue corals",]
          #selecting only blue corals in coraltype and creating new data as coraldata
    }
    
    else if(input$variable == "Hard Corals") {
      coraldata <- data[data$coralType == "hard corals",]
          #selecting only hard corals in coraltype and creating new data as coraldata
    }
    
    else if(input$variable == "Sea Fans") {
      coraldata <- data[data$coralType == "sea fans",]
          #selecting only sea fans in coraltype and creating new data as coraldata
    }
    
    else if(input$variable == "Sea Pens") {
      coraldata <- data[data$coralType == "sea pens",]
          #selecting only sea pens in coraltype and creating new data as coraldata
    }
    
    else if(input$variable == "Soft Corals") {
      coraldata <- data[data$coralType == "soft corals",]
          #selecting only soft corals in coraltype and creating new data as coraldata
    }
    
    f_graph <- ggplot(coraldata, aes(year, value,color=coralType)) + 
      geom_point() + facet_grid(~reorder(location,latitude)) + 
      geom_smooth(method=input$variable2) +
      labs(y="Bleaching Value in Percentage", x="Year")
    
                    #using new data coraldata to plot the graph and 
                    #using variable2 to selet the method of smoothing
    print(f_graph)
    
  })
  
  output$caption2 <- reactiveText(function() {
    paste("Location of the sites")
  })        #Caption of output plot of sites
  
 
    output$locationmap <- renderLeaflet({
    leaflet(data = data) %>% addTiles() %>%  
      addMarkers(~longitude, ~latitude, label = ~htmlEscape(location),
                  labelOptions = labelOptions(noHide = T,textsize = "10px",
                                  direction = "right",style = list("color" = "red")))
    #adding text labels for each site as visual indicators
    
  })
  
})

shinyApp(ui,server)
