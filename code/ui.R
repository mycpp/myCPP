#Jessica
# Justin
# This is the user-interface definition of a Shiny web application.
# You can find out more about building applications with Shiny here:
#
# http://shiny.rstudio.com
#

library(shiny)
library(plotly)
library(leaflet)
library(maps)

shinyUI(fluidPage(#theme = "bootstrap.css",
  
  # Application title
  titlePanel("CPP Test Tool"),
  
  # Sidebar with a slider input for number of bins
  fluidRow(
    column(4,
           sliderInput("Coal",
                       "Coal Usage",
                       min = -20,
                       max = 20,
                       value = 0),
           sliderInput("NGCC",
                       "NGCC Usage",
                       min = -20,
                       max = 20,
                       value = 0),
           selectizeInput("stateInput", #inputID
                          label = "State", #label
                          choices = NULL,
                          selected = "Alabama",
                          multiple = FALSE,
                          options = list(placeholder = 'select a state name') #maxOptions = 5, 
           )
    ),
    ###
    column(4, align ="center",
           h3(htmlOutput("dispOldEnergy")),
           h3(htmlOutput("dispNewEnergy"))
    ),
    column(4, align = "center",
           h3(htmlOutput("dispEff"))
    )
  ),
  
  fluidRow(
    # Show a plot of the generated distribution
    column(4,
           ""# actionButton("go", "Plot")
    ),

    column(8,
           tabsetPanel(type = "tabs", 
                       id = "tabset1",
                       tabPanel("Rate", value = "Rate", plotlyOutput("ratePlotly")),
                       tabPanel("Mass", value = "Mass", plotlyOutput("massPlotly")),#, plotlyOutput("massPlot"))
                       tabPanel("State Energy Map", value = "State Energy Map", leafletOutput("Statemap"))
           )
           #             h1(radioButtons("radio", 
           #                             label = "Emissions",
           #                             choices = list (
           #                               "Rate" = 1,
           #                               "Mass" = 2
           #                             ),
           #                             selected = 1)),
    )
  )#,

))
