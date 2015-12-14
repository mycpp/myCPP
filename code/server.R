
# This is the server logic for a Shiny web application.
# You can find out more about building applications with Shiny here:
#
# http://shiny.rstudio.com
#

library(shiny)
library(ggplot2)
library(plotly)
library(leaflet)
#some code here

generationData = read.csv("data/statedata.csv", #"https://docs.google.com/spreadsheets/d/1ZbDI31sSKatBoEVKo70TV_A4VwCBHK4pIoCWXB7yfx0/pub?gid=192701245&single=true&output=csv", 
                          header = TRUE) #read csv file
generationDataCleaned = generationData[!(is.null(generationData$Name) | generationData$Name==""), ]

#as.numeric(gsub(",","", generationDataCleaned$Coal.Steam.Electric.Generation..MWh.)) just got rid of , on data entry
statenames = as.character(generationDataCleaned$Name) 
row.names(generationDataCleaned) = statenames

#set default
    state = "Alabama"
    pctCoal = 0 
    pctNGCC = 0

shinyServer(function(input, output, session) {

  updateSelectizeInput(session,
                       'stateInput',
                       choices = statenames, #must be a character vector!!! http://shiny.rstudio.com/articles/selectize.html
                       selected = "Alabama",
                       server = TRUE)

# reactive expression -------------  
  result <- reactive({
    state = input$stateInput
    pctCoal = input$Coal / 100
    pctNGCC = input$NGCC / 100
    
    if(state == "") {
      #handle onload
      print("it was blank!")
      state = "Alabama"
      pctCoal = 0 
      pctNGCC = 0
      }
    baseCoal_Energy = generationDataCleaned[state, "Coal.Steam.Electric.Generation..MWh."]
    baseNGCC_Energy = generationDataCleaned[state, "NGCC.Electric.Generation..MWh."]
    
    baseEnergy = sum(baseCoal_Energy,
                      baseNGCC_Energy
                     )
    
    newEnergy = sum((1 + pctCoal) * baseCoal_Energy,
                     (1 + pctNGCC) * baseNGCC_Energy
                    )
    
    Energy_Frame <- c(baseEnergy, newEnergy)
    
    print(Energy_Frame)
    
## Emissions (mass) calculated by Rate -----------
    
    baseCoal_CO2_Rate = generationDataCleaned[state, "Coal.Steam.Emission.Rate..lb.MWh."]
    baseNGCC_CO2_Rate = generationDataCleaned[state, "NGCC.Emission.Rate..lb.MWh."]
    
    baseCO2_Rate = sum((baseCoal_CO2_Rate / 2000 * baseCoal_Energy) , #convert lbs to tons
                        (baseNGCC_CO2_Rate / 2000 * baseNGCC_Energy)   #convert lbs to tons
                        )
    
    newCO2_Rate = sum(((1 + pctCoal) * baseCoal_CO2_Rate / 2000 * baseCoal_Energy) , #convert lbs to tons
                      ((1 + pctNGCC) * baseNGCC_CO2_Rate / 2000 * baseNGCC_Energy)   #convert lbs to tons
    )
    
    CO2_Rate_Frame <- c(baseCO2_Rate, newCO2_Rate) 
    
## Emissions (mass) calculated by Mass -----------
    
    baseCoal_CO2_Mass = generationDataCleaned[state, "Coal.Steam.Carbon.Dioxide.Emissions..tons."]
    baseNGCC_CO2_Mass = generationDataCleaned[state, "NGCC.Carbon.Dioxide.Emissions..tons."]
    
    
    baseCO2_Mass = sum(baseCoal_CO2_Mass,
                        baseNGCC_CO2_Mass
                        )
    newCO2_Mass = sum((1 + pctCoal) * baseCoal_CO2_Mass,
                      (1 + pctNGCC) * baseNGCC_CO2_Mass
    )

    CO2_Mass_Frame <- c(baseCO2_Mass, newCO2_Mass) 
    
### result ----------    
    
    name_Frame <- c("Base", "New")
    
    result <- data.frame(name_Frame, Energy_Frame, CO2_Rate_Frame, CO2_Mass_Frame)
    
    colnames(result) <- c("Name", "Energy", "Rate", "Mass")
    result

  })
  
# render -----  

  output$dispNewEnergy <- renderUI({
    totalEnergy = result()[2,2]
    str1 <- paste("Your Annual Generation is")
    str2 <- paste(format(totalEnergy, big.mark=",", scientific = FALSE), " Mwh")
    HTML(paste(str1,str2, sep = '<br/>'))
  })
  output$dispOldEnergy <- renderUI({
    totalEnergy = result()[1,2]
    str1 <- paste("Your Annual Generation was")
    str2 <- paste(format(totalEnergy, big.mark=",", scientific = FALSE), " Mwh")
    HTML(paste(str1,str2, sep = '<br/>'))
  })
  
  output$dispEff <- renderUI({
    efficiency = mean(1-result()[2,3]/result()[1,3],1-result()[2,4]/result()[1,4])
    effperc = format(efficiency * 100, digits = 1)
    str3 <- paste("Your Plan Efficiency is")
    str4 <- paste(effperc,"%")
    HTML(paste(str3,str4, sep = '<br/>'))
    #outputOptions(output, "massPlot", suspendWhenHidden = FALSE)
  })
  
  output$ratePlotly <- renderPlotly({
    r <- plot_ly(result(),
                 type = "bar",       # all "bar" attributes: https://plot.ly/r/reference/#bar
                 x = Name,               # more about bar's "x": /r/reference/#bar-x
                 y = Rate,            # more about bar's "y": /r/reference/#bar-y
                 name = "CO2 Emissions (tons)",  # more about bar's "name": /r/reference/#bar-name
                 marker = list(          # marker is a named list, valid keys: /r/reference/#bar-marker
                   color=c("#1b9e77","#d95f02")     # more about marker's "color" attribute: /r/reference/#bar-marker-color
                 ))
    r <- layout(r,              # all of layout's properties: /r/reference/#layout
                xaxis = list(           # layout's xaxis is a named list. List of valid keys: /r/reference/#layout-xaxis
                  title = ""     # xaxis's title: /r/reference/#layout-xaxis-title
                ),
                yaxis = list(           # layout's yaxis is a named list. List of valid keys: /r/reference/#layout-yaxis
                  title = "CO2 Emissions (tons)"      # yaxis's title: /r/reference/#layout-yaxis-title
                )
                
    )
    
  })
  
#     output$ratePlot <- renderPlot({ #ly
#       gg <- ggplot(result(), aes(x = Name, y = Rate, fill = Name)) +
#         theme(
#           legend.position = "none",
#           axis.title.x = element_blank(),
#           axis.title.y = element_text(size = 16),
#           axis.text = element_text(size = 14)) +
#         geom_bar(stat = "identity") + 
#         scale_fill_brewer(type = "qual", palette = 1) +
#         ylab("CO2 Emissions (tons)")
#       gg
#       #p <- ggplotly(gg)
#       #p
#     })
    
    output$massPlotly <- renderPlotly({
      p <- plot_ly(result(),
                   type = "bar",       # all "bar" attributes: https://plot.ly/r/reference/#bar
                   x = Name,               # more about bar's "x": /r/reference/#bar-x
                   y = Mass,            # more about bar's "y": /r/reference/#bar-y
                   name = "CO2 Emissions (tons)",  # more about bar's "name": /r/reference/#bar-name
                   marker = list(          # marker is a named list, valid keys: /r/reference/#bar-marker
                     color=c("#1b9e77","#d95f02")     # more about marker's "color" attribute: /r/reference/#bar-marker-color
                   ))
      p <- layout(p,              # all of layout's properties: /r/reference/#layout
                  xaxis = list(           # layout's xaxis is a named list. List of valid keys: /r/reference/#layout-xaxis
                    title = ""     # xaxis's title: /r/reference/#layout-xaxis-title
                  ),
                  yaxis = list(           # layout's yaxis is a named list. List of valid keys: /r/reference/#layout-yaxis
                    title = "CO2 Emissions (tons)"      # yaxis's title: /r/reference/#layout-yaxis-title
                  )
                  
      )
      
    })
    
    
#     output$massPlot <- renderPlot({
#       gg2 <- ggplot(result(), aes(x = Name, y = Mass, fill = Name)) +
#         theme(
#           legend.position = "none",
#           axis.title.x = element_blank(),
#           axis.title.y = element_text(size = 16),
#           axis.text = element_text(size = 14)) +
#         geom_bar(stat = "identity") + 
#         scale_fill_brewer(type = "qual", palette = 1) +
#         ylab("CO2 Emissions (tons)")
#       gg2
#     })
  

})
