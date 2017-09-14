1. Generating reports of your data analysis with R markdown and KnitR

install.packages("knitr")
install.packages(“rmarkdown”)

rmarkdown::render("introduction.Rmd","pdf_document”)
rmarkdown::render(“introduction.Rmd”,”all”)

2. Creating interactive web applications with Shiny

install.packages("shiny")