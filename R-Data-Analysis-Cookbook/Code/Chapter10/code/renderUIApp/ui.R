library(shiny)

shinyUI(fluidPage(
  
  titlePanel("renderUI Example"),
  
  sidebarPanel(
    selectInput("dataset", "Dataset", c("rock", "mtcars")),
    uiOutput("var")
         ),
  mainPanel(
      plotOutput("plot")
    )  
))
