# Module server function
dwServer <- function(id) {
  #dwServer.R
  library(tidyr)
  library(tidyverse)
  library(skimr)
  library(CoolBeans)

  moduleServer(
    id,
    function(input, output, session){
    #Reactive expression to read the uploaded data
    original_data <- reactive({
     req(input$data)
     tryCatch({
       if (tools::file_ext(input$data$datapath) == "rds") {
         readRDS(input$data$datapath)
       } else {
         read.csv(input$data$datapath)
       }
     }, error = function(e) {
       showNotification(paste("Error:", e$message), type = "error")
       return(NULL)
     })
   })

  observeEvent(input$summary, {
    output$preview1 <- renderPrint({
    skim_without_charts(original_data(),c(1:input$columns))
  })})


  ###Pre-analitical step###
   return(original_data)


})}
