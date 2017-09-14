
auto <- read.csv("auto-mpg.csv")

shinyServer(function(input, output) {


output$plot <- renderPlot({
    if (input$plotType == "hist") {
        hist(mtcars$mpg)
    }
    else {
        plot(mtcars[,input$xaxis], mtcars$mpg, xlab=input$xaxis, ylab="mpg")
    }
})

#output$plot <- renderPlot({
#  hist(mtcars$mpg)
#})
})

