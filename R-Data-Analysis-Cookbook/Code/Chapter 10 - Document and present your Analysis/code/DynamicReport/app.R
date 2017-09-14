ui = fluidPage(
  selectInput("select", "Select X axis",
              choices = c("weight","cylinders","acceleration")),
  downloadButton("report", "Generate report")
)

server = function(input, output) {
  output$report <- downloadHandler(
    # For PDF output, change filename to "report.pdf"
    filename = "report.html",
    content = function(file) {
      
     report <- file.path("report.Rmd")
     
      
      # Set up parameters to pass to Rmd document
      params <- list(n = input$select)
      
      # Knit the document, passing in the `params` list, and eval it in a
      # child of the global environment 
      rmarkdown::render(report, output_file = file,
                        params = params,
                        envir = new.env(parent = globalenv())
      )
    }
  )
}


shinyApp(ui = ui, server = server)
