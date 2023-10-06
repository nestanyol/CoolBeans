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
      
      # Reactive value to store the transformed data
      prep_data <- reactiveVal()
      
      ###Pre-analytical step###
      #eventReactive(input$run, { #doesn't give output
      observeEvent(input$run, {
        
        ncols <- as.numeric(unlist(strsplit(input$ncols,",")))
        #data <- original_data()[,c(ncols[1],ncols[2],ncols[3]:ncol(original_data()))]
        data <- original_data()[,c(ncols[1],ncols[2],ncols[3]:30)]
        observe({prep_data(preprocess_data(data, input$id, input$target, 
                                  cutoff_met = input$na_cutoff/100, cutoff_subj = input$na_cutoff/100)) 
        })
        #check if something is happening
        output$preview2 <- renderPrint({
          skim_without_charts(prep_data(),c(1:10))
        })
      
      })
      
      return(prep_data)
      
    })}