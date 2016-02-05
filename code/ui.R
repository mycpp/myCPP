# This is the user-interface definition of a Shiny web application.
# You can find out more about building applications with Shiny here:
#
# http://shiny.rstudio.com
#

library(shiny)
library(plotly)
library(leaflet)
library(maps)


shinyUI(fluidPage(theme = "bootstrap.css",
                  # Google Analytics
                  tags$head(includeScript("www/google-analytics.js")),
                  # Application title
                  HTML('<img src="logo.svg" style = "max-height:130px" width = "100%"/>'),
                  fluidRow(column(12, align ="center",
                                  h3("A Clean Power Plan Evaluation Tool")
                  )),
                  fluidRow(column(12, align ="center",
                                  h4("Click", HTML('<a href="https://github.com/mycpp/myCPP/wiki/User-Guide-for-myCPP" onclick="ga(\'send\', \'event\', \'click\', \'View the Wiki\', \'Wiki\', 1)">here</a>'), "for a guide of how to use this tool")
                  )),
                  
                  # Sidebar with a slider input for number of bins
                  
                  ###
                  fluidRow(
                    selectizeInput("stateInput", #inputID
                                   label = "State", #label
                                   choices = NULL,
                                   selected = "Alabama",
                                   multiple = FALSE,
                                   options = list(placeholder = 'select a state name'))
                  ),
                  fluidRow(column(1),
                           column(7,tabsetPanel(type = "tabs",
                                                id = "tabset2",
                                                tabPanel("Generation (MWh) Map", leafletOutput("Genmap")),
                                                tabPanel("Carbon Emissions (tons of CO2) Map", leafletOutput("Carbonmap"))
                           )
                           )),
                  fluidRow(column(2),
                           column(4, align ="center",
                                  h3(htmlOutput("dispDemandMet"))
                           )
                  ),
                  fluidRow(column(1),
                           # Show a plot of the generated distribution
                           column(7,
                                  tabsetPanel(type = "tabs",
                                              id = "tabset1",
                                              tabPanel("Rate", value = "Rate", h3(htmlOutput("dispRate"),plotlyOutput("ratePlotly",height="150px"))),
                                              tabPanel("Mass", value = "Mass", h3(htmlOutput("dispMass"),plotlyOutput("massPlotly",height="150px")))
                                  ))
                  ),
                  fluidRow(column(1),
                           column(8,
                                  tabsetPanel(type = "tabs",
                                              id = "tabset0",
                                              tabPanel("Plant Modifications", value = "Plant Modifications",
                                                       sliderInput("CoalHeatRateImp",
                                                                   "Coal Heat Rate Improvement (%)",
                                                                   min = 0,
                                                                   max = 8,
                                                                   value = 0,
                                                                   step = 0.1,
                                                                   width = '1000px'),
                                                       uiOutput(outputId="NGCCSlider"),
                                                       sliderInput("NuclearUprates",
                                                                   "Nuclear Capacity Uprates (%)",
                                                                   min = 0,
                                                                   max = 20,
                                                                   value = 0,
                                                                   step = 1,
                                                                   width = '1000px')
                                              ),
                                              tabPanel("Capacity Additions", value = "Capacity Additions",
                                                       sliderInput("CoalCapacity",
                                                                   "Increase Coal Capacity (MW)",
                                                                   min = 0,
                                                                   max = 4000,
                                                                   value = 0,
                                                                   step = 100,
                                                                   width = '1000px'),
                                                       sliderInput("NGCCCapacity",
                                                                   "Increase NGCC Capacity (MW)",
                                                                   min = 0,
                                                                   max = 4000,
                                                                   value = 0,
                                                                   step = 100,
                                                                   width = '1000px'),
                                                       sliderInput("NuclearCapacity",
                                                                   "Increase Nuclear Capacity (MW)",
                                                                   min = 0,
                                                                   max = 4000,
                                                                   value = 0,
                                                                   step = 100,
                                                                   width = '1000px'),
                                                       sliderInput("HydroCapacity",
                                                                   "Increase Hydroelectric Capacity (MW)",
                                                                   min = 0,
                                                                   max = 4000,
                                                                   value = 0,
                                                                   step = 10,
                                                                   width = '1000px'),
                                                       sliderInput("WindCapacity",
                                                                   "Increase Wind Capacity (MW)",
                                                                   min = 0,
                                                                   max = 500,
                                                                   value = 0,
                                                                   step = 1,
                                                                   width = '1000px'),
                                                       sliderInput("SolarCapacity",
                                                                   "Increase Solar Capacity (MW)",
                                                                   min = 0,
                                                                   max = 300,
                                                                   value = 0,
                                                                   step = 1,
                                                                   width = '1000px'),
                                                       sliderInput("BiomassCapacity",
                                                                   "Increase Biomass Capacity (MW)",
                                                                   min = 0,
                                                                   max = 1000,
                                                                   value = 0,
                                                                   step = 10,
                                                                   width = '1000px'),
                                                       sliderInput("GeothermalCapacity",
                                                                   "Increase Geothermal Capacity (MW)",
                                                                   min = 0,
                                                                   max = 1000,
                                                                   value = 0,
                                                                   step = 10,
                                                                   width = '1000px')
                                              ),
                                              tabPanel("Capacity Reductions", value = "Capacity Additions",
                                                       sliderInput("CoalReduction",
                                                                   "Decrease Coal Capacity (MW)",
                                                                   min = -10000,
                                                                   max = 0,
                                                                   value = 0,
                                                                   step = 100,
                                                                   width = '1000px'),
                                                       sliderInput("NGCCReduction",
                                                                   "Decrease NGCC Capacity (MW)",
                                                                   min = -4000,
                                                                   max = 0,
                                                                   value = 0,
                                                                   step = 100,
                                                                   width = '1000px'),
                                                       sliderInput("NuclearReduction",
                                                                   "Decrease Nuclear Capacity (MW)",
                                                                   min = -4000,
                                                                   max = 0,
                                                                   value = 0,
                                                                   step = 100,
                                                                   width = '1000px'),
                                                       sliderInput("HydroReduction",
                                                                   "Decrease Hydroelectric Capacity (MW)",
                                                                   min = -4000,
                                                                   max = 0,
                                                                   value = 0,
                                                                   step = 100,
                                                                   width = '1000px'),
                                                       sliderInput("WindReduction",
                                                                   "Decrease Wind Capacity (MW)",
                                                                   min = -500,
                                                                   max = 0,
                                                                   value = 0,
                                                                   step = 1,
                                                                   width = '1000px'),
                                                       sliderInput("SolarReduction",
                                                                   "Decrease Solar Capacity (MW)",
                                                                   min = -300,
                                                                   max = 0,
                                                                   value = 0,
                                                                   step = 1,
                                                                   width = '1000px'),
                                                       sliderInput("BiomassReduction",
                                                                   "Decrease Biomass Capacity (MW)",
                                                                   min = -1000,
                                                                   max = 0,
                                                                   value = 0,
                                                                   step = 10,
                                                                   width = '1000px'),
                                                       sliderInput("GeothermalReduction",
                                                                   "Decrease Geothermal Capacity (MW)",
                                                                   min = -1000,
                                                                   max = 0,
                                                                   value = 0,
                                                                   step = 10,
                                                                   width = '1000px')
                                              )
                                              
                                  )
                           )
                  ),
                  HTML('<img src="bc-banner-bottom.svg", style = "max-height:100px" width = "100%"/>'),
                  HTML('<p align="center"><a href="mailto:bccppt@gmail.com" onclick="ga(\'send\', \'event\', \'click\', \'Send an Email\', \'Email\', 1)">Have feedback that you would like to give us? Drop us a line here!</a></p>')
)
)