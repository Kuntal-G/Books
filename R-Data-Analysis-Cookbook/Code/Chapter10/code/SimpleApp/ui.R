library(shiny)
 
shinyUI(fluidPage(   
  titlePanel("Simple Shiny Application"),    
  sidebarLayout(     
    sidebarPanel(
      p("Create plots using the auto data"),
      selectInput("x", "Select X axis", 
        choices = c("weight","cylinders","acceleration")),
      br(),
      radioButtons("color","Select Color scheme",
                    choices=c("gray","blue","green","red"))
    ),
        mainPanel(
          h4(textOutput("outputString")),
          plotOutput("plot"))   ) 
  ))      