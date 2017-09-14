auto <- read.csv("auto-mpg.csv")
shinyServer(function(input, output) {
  output$outputString <- renderText(paste("mpg ~", input$x))
  output$plot <- renderPlot(
    plot(as.formula(paste("mpg ~",input$x)),data=auto,col=input$color)
  )  
  # Generate a summary of the data
  output$summary <- renderPrint({
    summary(auto)
  })
  
  # Generate an HTML table view of the data
  output$table <- renderTable({
    data.frame(x=auto)
  })
  
  # Generate an HTML table view of the data
  output$datatable <- renderDataTable({
    auto
  }, options = list(aLengthMenu = c(5, 25, 50), iDisplayLength = 5))
})