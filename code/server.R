
# This is the server logic for a Shiny web application.
# You can find out more about building applications with Shiny here:
#
# http://shiny.rstudio.com
# added comments 01/06/2016

## Libraries ----
library(shiny)
library(plotly)
library(leaflet)
library(maps)

## Generation Data ----
### denotes functions used to call data from csv files
generationData = read.csv("data/statedata.csv", #"https://docs.google.com/spreadsheets/d/1ZbDI31sSKatBoEVKo70TV_A4VwCBHK4pIoCWXB7yfx0/pub?gid=192701245&single=true&output=csv", 
                          header = TRUE) #read csv file
generationDataCleaned = generationData[!(is.null(generationData$Name) | generationData$Name==""), ]

stateNameCSV = read.csv("data/StateNames.csv",
                        header = TRUE)

### convert generation data to vectors with charcters for manipulation
statenames = as.character(stateNameCSV$State) 
row.names(generationDataCleaned) = as.character(generationDataCleaned$Name)

### Plant Location Data
geodata <- read.csv("data/plantgeodata.csv")

# reorder FuelSimplified factor to amount of plants rather than alphabetically, this puts the best colors for the most important data.
FuelSimplifiedtable <- table(geodata$FuelSimplified)
FuelSimplifiedfactors <- factor(geodata$FuelSimplified,
                                levels = names(FuelSimplifiedtable[order(FuelSimplifiedtable, decreasing = TRUE)]))

# Reactive ----
shinyServer(function(input, output, session) {
  
  updateSelectizeInput(session,
                       'stateInput',
                       choices = statenames, #must be a character vector!!! http://shiny.rstudio.com/articles/selectize.html
                       selected = "Alabama",
                       server = TRUE)
  
  # reactive expression -------------  
  ### denote the reactive inputs used in code (i.e. "input$somevariable here") 
  result <- reactive({
    state = input$stateInput
    pctCoal = input$Coal / 100
    pctNGCC = input$NGCC / 100
    ## Handle Onload ----
    ### default display when nothing selected
    if(state == "") {
      state = "Alabama"
      pctCoal = 0 
      pctNGCC = 0
    } 
    ## Base Energy ----
    ### denotes variable used for calculations, varies with state
    ### and calculates data for slider bars
    baseCoal_Energy = generationDataCleaned[state, "Coal.Steam.Electric.Generation..MWh."]
    baseNGCC_Energy = generationDataCleaned[state, "NGCC.Electric.Generation..MWh."]
    
    baseEnergy = sum(baseCoal_Energy,
                     baseNGCC_Energy
    )
    ## New Energy ----
    newEnergy = sum((1 + pctCoal) * baseCoal_Energy,
                    (1 + pctNGCC) * baseNGCC_Energy
    )
    ## Energy Frame ----
    ### concatenate "group" data into a one column vector "base" and "new" values
    Energy_Frame <- c(baseEnergy, newEnergy)
    
    print(Energy_Frame)
    
    ## Leaflet Maps --------- 
    ### read https://rstudio.github.io/leaflet/ for syntax details
    
    ## Handle Hawaii and Alaska abstraction ----
    ### probably not really important, http://www.r-bloggers.com/mapping-capabilities-in-r/
    ### in Maps these states were named differently, added code to handle correctly with our designations
    if (state == "Hawaii") {
      mapStates <- map('world', region = c("USA:Hawaii"))
    } else if(state == "Alaska") {
      mapStates <- map('world', region = c("USA:Alaska"))
    } else                      {
      ## 48 States and DC for a list: map('state', names = TRUE, plot = FALSE)
      mapStates <- map('state', region = c(state))
    }
    stateCode <- state


    # set the color palette which is by Fuel Type https://rstudio.github.io/leaflet/colors.html
    pal <- colorFactor(
      palette = "Paired", 
      domain = FuelSimplifiedfactors
    )

    your.map1 <- leaflet(data = mapStates) %>%
      addProviderTiles("Stamen.TonerLite") %>%
      addPolylines(data=mapStates, fill=FALSE, smoothFactor=FALSE, color="#000", weight = 3, opacity = 0.9) %>%
      
      addCircleMarkers(data=geodata[((geodata$State==state)),], lng= ~Lon, lat = ~Lat, 
                       color=~pal(factor(FuelSimplified,
                                         levels = names(FuelSimplifiedtable[order(FuelSimplifiedtable, decreasing = TRUE)]))), #reordered
                       stroke=FALSE, 
                       popup=paste(sep = "<br/>",
                                   paste0("<i>",geodata[((geodata$State==state)),]$Name,"</i>"),
                                   paste0("<b>",geodata[((geodata$State==state)),]$FuelSimplified,"</b>"),
                                   paste0("Category: ", geodata[((geodata$State==state)),]$Category)), 
                       fillOpacity=0.85, radius=~sqrt((Generation/6000)/3.14159)) %>%
      addLegend("bottomright",       # add Legend
                pal = pal,
                values = FuelSimplifiedfactors,
                title = "&nbsp;&nbsp;&nbsp;&nbsp;Plant Type&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;", #ghetto way to extend the width of the legend, seems to be no workaround at the moment to increasing the legend width
                opacity = 0.90)

    output$Genmap <- renderLeaflet(your.map1)
    
    your.map2 <- leaflet(data = mapStates) %>%
      addProviderTiles("Stamen.TonerLite") %>%
      addPolylines(data=mapStates, fill=FALSE, smoothFactor=FALSE, color="#000", weight = 3, opacity = 0.9) %>%
      
      addCircleMarkers(data=geodata[((geodata$State==state)),], lng= ~Lon, lat = ~Lat, 
                       color=~pal(factor(FuelSimplified,
                                         levels = names(FuelSimplifiedtable[order(FuelSimplifiedtable, decreasing = TRUE)]))), #reordered
                       stroke=FALSE, 
                       popup=paste(sep = "<br/>",
                                   paste0("<i>",geodata[((geodata$State==state)),]$Name,"</i>"),
                                   paste0("<b>",geodata[((geodata$State==state)),]$FuelSimplified,"</b>"),
                                   paste0("Category: ", geodata[((geodata$State==state)),]$Category)), 
                       fillOpacity=0.85, radius=~sqrt((CarbonDioxide/6000)/3.14159)) %>%
      addLegend("bottomright",       # add Legend
                pal = pal,
                values = FuelSimplifiedfactors,
                title = "&nbsp;&nbsp;&nbsp;&nbsp;Plant Type&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;", #ghetto way to extend the width of the legend, seems to be no workaround at the moment to increasing the legend width
                opacity = 0.90)
    
    output$Carbonmap <- renderLeaflet(your.map2)
    
    
    ## Emissions (mass) calculated by Rate -----------
    ### denotes variable used for calculations, varies with state
    ### and performs calculations 
    baseCoal_CO2_Rate = generationDataCleaned[state, "Coal.Steam.Emission.Rate..lb.MWh."]
    baseNGCC_CO2_Rate = generationDataCleaned[state, "NGCC.Emission.Rate..lb.MWh."]
    
    baseCO2_Rate = sum((baseCoal_CO2_Rate / 2000 * baseCoal_Energy) , #convert lbs to tons
                       (baseNGCC_CO2_Rate / 2000 * baseNGCC_Energy)   #convert lbs to tons
    )
    
    newCO2_Rate = sum(((1 + pctCoal) * baseCoal_CO2_Rate / 2000 * baseCoal_Energy) , #convert lbs to tons
                      ((1 + pctNGCC) * baseNGCC_CO2_Rate / 2000 * baseNGCC_Energy)   #convert lbs to tons
    )
    ### concatenate "group" data into a one column vector "base" and "new" values
    CO2_Rate_Frame <- c(baseCO2_Rate, newCO2_Rate) 
    
    ## Emissions (mass) calculated by Mass -----------
    ### denotes variable used for calculations, varies with state
    ### and performs calculations     
    baseCoal_CO2_Mass = generationDataCleaned[state, "Coal.Steam.Carbon.Dioxide.Emissions..tons."]
    baseNGCC_CO2_Mass = generationDataCleaned[state, "NGCC.Carbon.Dioxide.Emissions..tons."]
    
    
    baseCO2_Mass = sum(baseCoal_CO2_Mass,
                       baseNGCC_CO2_Mass
    )
    newCO2_Mass = sum((1 + pctCoal) * baseCoal_CO2_Mass,
                      (1 + pctNGCC) * baseNGCC_CO2_Mass
    )
    ### concatenate "group" data into a one column vector "base" and "new" values
    CO2_Mass_Frame <- c(baseCO2_Mass, newCO2_Mass) 
    
    ### result ----------
    ### concatenate "group" data into a one column vector "base" and "new" values
    name_Frame <- c("Base", "New")
    
    ### converts the column vectors of equal length into a data table
    result <- data.frame(name_Frame, Energy_Frame, CO2_Rate_Frame, CO2_Mass_Frame)
    ### denotes the column header for the "results" table
    colnames(result) <- c("Name", "Energy", "Rate", "Mass")
    result
    
    ## capacity factor (%)
    ### denotes variable used for calculations, varies with state
    ### and performs calculations 
    #### unable to determine reliable capacity factor data from loaded csv files.... will load eia data file using Plant ID (AKA "ORIS code")
    
    
  })
  
  # render -----
  ### denote the reactive outputs used in shiny code (i.e. "output$somevariable here")
  ### to diplay for webpage i.e. renderUI (somescript_here)
  output$dispNewEnergy <- renderUI({
    ### get data from "result" table and prep for HTML display
    totalEnergy = result()[2,2]
    str1 <- paste("Your Annual Generation is")
    str2 <- paste(format(totalEnergy, big.mark=",", scientific = FALSE), " Mwh")
    HTML(paste(str1,str2, sep = '<br/>'))
  })
  output$dispOldEnergy <- renderUI({
    ### get data from "result" table and prep for HTML display
    totalEnergy = result()[1,2]
    str1 <- paste("Your Annual Generation was")
    str2 <- paste(format(totalEnergy, big.mark=",", scientific = FALSE), " Mwh")
    HTML(paste(str1,str2, sep = '<br/>'))
  })
  output$dispEff <- renderUI({
    ### get data from "result" table and prep for HTML display
    efficiency = mean(1-result()[2,3]/result()[1,3],1-result()[2,4]/result()[1,4])
    effperc = format(efficiency * 100, digits = 1)
    str3 <- paste("Your Plan Efficiency is")
    str4 <- paste(effperc,"%")
    HTML(paste(str3,str4, sep = '<br/>'))
    #outputOptions(output, "massPlot", suspendWhenHidden = FALSE)
  })
  
  ### to diplay for webpage i.e. renderPlotly (somescript_here converts Plotly to r for display)
  ### uses the "result" table created to pull data to plot figure using plotly
  ### note variable calls
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
  
})