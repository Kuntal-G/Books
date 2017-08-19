auto <- read.csv("auto-mpg.csv")
shinyServer(function(input, output) {
  output$outputString <- renderText(paste("mpg ~", input$x))
  output$plot <- renderPlot(
    plot(as.formula(paste("mpg ~",input$x)),data=auto,col=input$color)    
  )
})