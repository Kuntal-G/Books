library(shiny)

shinyUI(fluidPage(
  
titlePanel("Conditional Panel"),

sidebarPanel(
selectInput(
"plotType", "Plot Type",
c("Scatter Plot" = "scatter",
Histogram = "hist")),
conditionalPanel(condition="input.plotType != 'hist'",
selectInput("xaxis","X Axis Variable",
choices = c(Weight="wt", Cylinders="cyl", "Horse Power"="hp"))
)),
mainPanel(
plotOutput("plot")
)

))
