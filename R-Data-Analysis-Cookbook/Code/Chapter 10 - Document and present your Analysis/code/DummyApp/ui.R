library(shiny)
shinyUI(pageWithSidebar(   
  headerPanel("Dummy Application"),   
  sidebarPanel(     h3('Sidebar text')   ),   
  mainPanel(       h3('Main Panel text')   ) ))
