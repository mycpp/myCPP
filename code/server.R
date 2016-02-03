
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

### Plant capacity factor data


# Reactive ----
shinyServer(function(input, output, session) {
  
  updateSelectizeInput(session,
                       'stateInput',
                       choices = statenames, #must be a character vector!!! http://shiny.rstudio.com/articles/selectize.html
                       selected = "Alabama",
                       server = TRUE)
  
  output$NGCCSlider <- renderUI({
    sliderInput("NGCCCap",
                "NGCC Average Capacity Factor (%)",
                min = 0,
                max = 90,
                value = generationDataCleaned[input$stateInput, "NGCCcapFactor"] * 100,
                step = 1,
                width = '1000px')
    
  })
  output$BCLogo <- renderImage({outfile <- tempfile(fileext='BCLogo.png')})
  

  # reactive expression -------------  
  ### denote the reactive inputs used in code (i.e. "input$somevariable here") 
  result <- reactive({
    state = input$stateInput
    pctCoal = input$CoalHeatRateImp / 100
    pctNGCC = (input$NGCCCap)/100
    pctUprates = (input$NuclearUprates)/100
    CoalAdditions = input$CoalCapacity
    NGCCAdditions = input$NGCCCapacity
    nuclearAdditions = input$NuclearCapacity
    windAdditions = input$WindCapacity
    solarAdditions = input$SolarCapacity
    hydroAdditions = input$HydroCapacity
    biomassAdditions = input$BiomassCapacity
    geothermalAdditions = input$GeothermalCapacity
    CoalSubtractions = input$CoalReduction
    NGCCSubtractions = input$NGCCReduction
    nuclearSubtractions = input$NuclearReduction
    windSubtractions = input$WindReduction
    solarSubtractions = input$SolarReduction
    hydroSubtractions = input$HydroReduction
    biomassSubtractions = input$BiomassReduction
    geothermalSubtractions = input$GeothermalReduction
      
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
    baseEnergy = generationDataCleaned[state, "Demand2030"]
    baseCoal_Energy = generationDataCleaned[state, "TotalCoal"]
    baseNGCC_Energy = generationDataCleaned[state, "TotalNGCC"]
    baseNuclear_Energy = generationDataCleaned[state, "TotalNuclear"]
    baseHydro_Energy = generationDataCleaned[state, "TotalHydro"]
    baseBiomass_Energy = generationDataCleaned[state, "TotalBiomass"]
    baseSolar_Energy = generationDataCleaned[state, "TotalSolar"]
    baseWind_Energy = generationDataCleaned[state, "TotalWind"]
    baseGeothermal_Energy = generationDataCleaned[state, "TotalGeothermal"]
    
    ## New Energy ----
    newCoal_Energy = (1+pctCoal)*baseCoal_Energy+(CoalAdditions*0.422*365.25*24)+(CoalSubtractions*0.422*365.25*24)
    newNGCC_Energy = (1+(pctNGCC-generationDataCleaned[state,"NGCCcapFactor"])/(generationDataCleaned[state,"NGCCcapFactor"]))*baseNGCC_Energy+(NGCCAdditions*pctNGCC*365.25*24)+((NGCCSubtractions*pctNGCC*365.25*24))
    newNuclear_Energy = (1+pctUprates)*baseNuclear_Energy+(nuclearAdditions*0.850*365.25*24)
    newHydro_Energy = baseHydro_Energy+(hydroAdditions*0.369*365.25*24)
    newBiomass_Energy = baseBiomass_Energy+(biomassAdditions*0.514*365.25*24)
    newWind_Energy = baseWind_Energy+(windAdditions*0.274*365.25*24)
    newSolar_Energy = baseSolar_Energy+(solarAdditions*0.163*365.25*24)
    newGeothermal_Energy = baseGeothermal_Energy+(geothermalAdditions*0.472*365.25*24)
    
    newEnergy = sum(newCoal_Energy,newNGCC_Energy,newNuclear_Energy,newHydro_Energy,newBiomass_Energy,newWind_Energy,newSolar_Energy,newGeothermal_Energy,nuclearSubtractions*0.850*365.25*24,hydroSubtractions*0.369*365.25*24,biomassSubtractions*0.514*365.25*24,windSubtractions*0.274*365.25*24,solarSubtractions*0.163*365.25*24,geothermalSubtractions*0.472*365.25*24)
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
      palette(), 
      domain = geodata$FuelSimplified
    )
    
    
    your.map1 <- leaflet(data = mapStates) %>%
      addProviderTiles("Stamen.TonerLite") %>%
      addPolylines(data=mapStates, fill=FALSE, smoothFactor=FALSE, color="#000", weight = 3, opacity = 0.9) %>%
      
      addCircles(data=geodata[((geodata$State==state)),], lng= ~Lon, lat = ~Lat, color=~pal(FuelSimplified), stroke=FALSE, 
                       popup=paste(sep = "<br/>",
                                   paste0("<i>",geodata[((geodata$State==state)),]$Name,"</i>"),
                                   paste0("<b>",geodata[((geodata$State==state)),]$FuelSimplified,"</b>"),
                                   paste0("Category: ", geodata[((geodata$State==state)),]$Category)), 
                       fillOpacity=0.8, radius=~sqrt((Generation*500)/3.14159)) %>%
      addLegend("bottomright",       # add Legend
                pal = pal,
                values = geodata$FuelSimplified,
                title = "Plant Type",
                opacity = 0.90)
    
    
    output$Genmap <- renderLeaflet(your.map1)
    
    your.map2 <- leaflet(data = mapStates) %>%
      addProviderTiles("Stamen.TonerLite") %>%
      addPolylines(data=mapStates, fill=FALSE, smoothFactor=FALSE, color="#000", weight = 3, opacity = 0.9) %>%
      
      addCircles(data=geodata[((geodata$State==state)),], lng= ~Lon, lat = ~Lat, color=~pal(FuelSimplified), stroke=FALSE, 
                       popup=paste(sep = "<br/>",
                                   paste0("<i>",geodata[((geodata$State==state)),]$Name,"</i>"),
                                   paste0("<b>",geodata[((geodata$State==state)),]$FuelSimplified,"</b>"),
                                   paste0("Category: ", geodata[((geodata$State==state)),]$Category)), 
                       fillOpacity=0.8, radius=~sqrt((CarbonDioxide*500)/3.14159)) %>%
      addLegend("bottomright",             # add Legend
                pal = pal,
                values = geodata$FuelSimplified,
                title = "Plant Type",
                opacity = 0.90)
    output$Carbonmap <- renderLeaflet(your.map2)
    ## Emissions (mass) calculated by Rate -----------
    ### denotes variable used for calculations, varies with state
    ### and performs calculations 
    baseCoal_CO2_Mass = generationDataCleaned[state, "CoalCO2Mass"]*2000
    baseNGCC_CO2_Mass = generationDataCleaned[state, "NGCCCO2Mass"]*2000
    baseCoal_CO2_Rate = generationDataCleaned[state, "CoalCO2Rate"]
    baseNGCC_CO2_Rate = generationDataCleaned[state, "NGCCCO2Rate"]
    
    baseCO2_Rate = (baseCoal_CO2_Mass+baseNGCC_CO2_Mass)/(baseCoal_Energy+baseNGCC_Energy)
    newCO2_Rate = ((newCoal_Energy-(pctCoal)*baseCoal_Energy-baseCoal_Energy)*baseCoal_CO2_Rate+(newNGCC_Energy-baseNGCC_Energy)*baseNGCC_CO2_Rate+baseCoal_CO2_Mass+baseNGCC_CO2_Mass)/(newCoal_Energy+newNGCC_Energy+(newNuclear_Energy-baseNuclear_Energy)+(newHydro_Energy-baseHydro_Energy)+(newBiomass_Energy-baseBiomass_Energy)+(newWind_Energy-baseWind_Energy)+(newSolar_Energy-baseSolar_Energy)+(newGeothermal_Energy-baseGeothermal_Energy))
    rateGoal = generationDataCleaned[state, "RateGoals"]
    
    ### concatenate "group" data into a one column vector "base" and "new" values
    CO2_Rate_Frame <- c(baseCO2_Rate, newCO2_Rate) 
    
    ## Emissions (mass) calculated by Mass -----------
    ### denotes variable used for calculations, varies with state
    ### and performs calculations     
    
    
    baseCO2_Mass = sum(baseCoal_CO2_Mass,
                       baseNGCC_CO2_Mass
    )
    newCO2_Mass = (newCoal_Energy-(pctCoal)*baseCoal_Energy-baseCoal_Energy)*baseCoal_CO2_Rate+(newNGCC_Energy-baseNGCC_Energy)*baseNGCC_CO2_Rate+baseCoal_CO2_Mass+baseNGCC_CO2_Mass
    
    massGoal = generationDataCleaned[state, "MassGoals"]
    ### concatenate "group" data into a one column vector "base" and "new" values
    CO2_Mass_Frame <- c(baseCO2_Mass/2000, newCO2_Mass/2000) 
    
    ### result ----------
    goals_Frame <- c(rateGoal, massGoal)
    ### concatenate "group" data into a one column vector "base" and "new" values
    name_Frame <- c("(Base) 2012", "Your Plan")
    ### converts the column vectors of equal length into a data table
    result <- data.frame(name_Frame, Energy_Frame, CO2_Rate_Frame, CO2_Mass_Frame, goals_Frame)
    ### denotes the column header for the "results" table
    colnames(result) <- c("Name", "Energy", "Rate", "Mass", "Goals")
    
    result
    print(result)

    
    ### denotes variable used for calculations, varies with state
    ### and performs calculations 
    #### unable to determine reliable capacity factor data from loaded csv files.... will load eia data file using Plant ID (AKA "ORIS code")
    
    
  })
  
  # render -----
  ### denote the reactive outputs used in shiny code (i.e. "output$somevariable here")
  ### to diplay for webpage i.e. renderUI (somescript_here)
  output$dispDemandMet <- renderUI({
    ### get data from "result" table and prep for HTML display
    demandMet = result()[2,2]/result()[1,2]
    demandMet = format(demandMet*100,digits=3)
    str1 <- paste("2030 Consumer Demand Met")
    str2 <- paste(format(demandMet, big.mark=",", scientific = FALSE), "%")
    HTML(paste(str1,str2, sep = '<br/>'))
  })
  output$dispRate <- renderUI({
    ### get data from "result" table and prep for HTML display
    rate = result()[1,5]/result()[2,3]
    rateperc = format(rate * 100, digits = 1)
    str3 <- paste("Rate-Based CPP Compliance")
    str4 <- paste(rateperc,"%")
    HTML(paste(str3,str4))  # sep = '<br/>'
    #outputOptions(output, "massPlot", suspendWhenHidden = FALSE)
  })
  output$dispMass <- renderUI({
    ### get data from "result" table and prep for HTML display
    mass = result()[2,5]/(result()[2,4])
    massperc = format(mass * 100, digits = 1)
    str3 <- paste("Mass-Based CPP Compliance")
    str4 <- paste(massperc,"%")
    HTML(paste(str3,str4)) #sep = '<br/>'
    #outputOptions(output, "massPlot", suspendWhenHidden = FALSE)
  })
  ### to diplay for webpage i.e. renderPlotly (somescript_here converts Plotly to r for display)
  ### uses the "result" table created to pull data to plot figure using plotly
  ### note variable calls
  output$ratePlotly <- renderPlotly({
    r <- plot_ly(result(),
                 type = "bar",       # all "bar" attributes: https://plot.ly/r/reference/#bar
                 orientation = "h",
                 x = Rate,               # more about bar's "x": /r/reference/#bar-x
                 y = Name,                # more about bar's "y": /r/reference/#bar-y
                 name = "lbsCO2/MWh",
                 opacity = 0.7,    # more about bar's "name": /r/reference/#bar-name
                 marker = list(         # marker is a named list, valid keys: /r/reference/#bar-marker
                   color=c("517C96","FF8200")     # more about marker's "color" attribute: /r/reference/#bar-marker-color
                 ))
    r <- add_trace(x = c(result()[1,5],result()[1,5]), type = "line",
                   marker = list(
                     color="red",
                     size = 0
                   ),
                   line = list(          # marker is a named list, valid keys: /r/reference/#bar-marker
                     color="red",
                     width = 6
                     
                   ),
                   name = "Goal"
    )
    r <- layout(r,              # all of layout's properties: /r/reference/#layout
                xaxis = list(           # layout's xaxis is a named list. List of valid keys: /r/reference/#layout-xaxis
                  title = ""     # xaxis's title: /r/reference/#layout-xaxis-title
                ),
                yaxis = list(           # layout's yaxis is a named list. List of valid keys: /r/reference/#layout-yaxis
                  title = "lbsCO2/MWh"      # yaxis's title: /r/reference/#layout-yaxis-title
                ),
                margin = list(
                  l = 120
                )
                
    )
    
  })
 
  output$massPlotly <- renderPlotly({
    p <- plot_ly(result(),
                 type = "bar",       # all "bar" attributes: https://plot.ly/r/reference/#bar
                 orientation = "h",
                 x = Mass,               # more about bar's "x": /r/reference/#bar-x
                 y = Name,            # more about bar's "y": /r/reference/#bar-y
                 name = "tons of CO2",  # more about bar's "name": /r/reference/#bar-name
                 opacity =0.7,
                 marker = list(          # marker is a named list, valid keys: /r/reference/#bar-marker
                   color=c("517C96","FF8200")     # more about marker's "color" attribute: /r/reference/#bar-marker-color
                 ))
    p <- add_trace(x = c(result()[2,5],result()[2,5]), type = "line",
                   marker = list(
                     color="red",
                     size = 0
                   ),
                   line = list(          # marker is a named list, valid keys: /r/reference/#bar-marker
                     color="red",
                     width = 6
                     
                   ),
                   name = "Goal"
    )
    p <- layout(p,              # all of layout's properties: /r/reference/#layout
                xaxis = list(           # layout's xaxis is a named list. List of valid keys: /r/reference/#layout-xaxis
                  title = ""     # xaxis's title: /r/reference/#layout-xaxis-title
                ),
                yaxis = list(           # layout's yaxis is a named list. List of valid keys: /r/reference/#layout-yaxis
                  title = "tons of CO2"      # yaxis's title: /r/reference/#layout-yaxis-title
                ),
                margin = list(
                  l = 120
                )
                
    )
    
  })
  
})