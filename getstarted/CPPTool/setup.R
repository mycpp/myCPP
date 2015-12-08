install.packages("shiny")
install.packages("plotly")
# help(signup, package = 'plotly') #if you don't have a username
# i will use my username and api key for this exercise:
Sys.setenv("plotly_username" = "AlexPawlowski")
Sys.setenv("plotly_api_key" = "e9m20t66t6")

colnames(generationData) = generationData[1,]
rm(generationDataCleaned)

#   output$ratePlot <- renderPlotly({
#       gg <- ggplot(result(), aes(x = Name, y = CO2_Rate, fill = Name)) + 
#         theme_minimal() +
#         geom_bar(stat = "identity") + 
#         scale_fill_brewer(type = "qual", palette = 1)
#     p <- ggplotly(gg)
#     p
#   })
#   output$massPlot <- renderPlotly({
#     gg2 <- ggplot(result(), aes(x = Name, y = CO2_Mass, fill = Name)) + 
#       theme_minimal() +
#       geom_bar(stat = "identity") + 
#       scale_fill_brewer(type = "qual", palette = 1)
#     p2 <- ggplotly(gg2)
#     p2
#   })
#  output$distPlot <- renderPlotly({
#     
#     # generate bins based on input$Goal from ui.R
#     x    <- faithful[, 2]
#     #bins <- seq(min(x), max(x), length.out = input$Goal + 1)
#     
#     # size of the bins depend on the input 'bins'
#     size <- (max(x) - min(x)) / input$Goal
#     
#     # draw the histogram with the specified number of bins
#     #hist(x, breaks = bins, col = 'darkgray', border = 'white')
#     
#     p <- plot_ly(faithful, x = x, autobinx = F, type = "histogram", 
#                  xbins = list(start = min(x), end = max(x), size = size))
#   })