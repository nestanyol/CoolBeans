# Module server function
smServer <- function(id, df) {
  #dwServer.R
  library(tidyr)
  library(tidyverse)
  library(skimr)
  library(CoolBeans)

  moduleServer(
    id,
    function(input, output, session){
      # #Reactive expression to read the uploaded data
      # original_data <- reactive({
      #   req(input$data)
      #   tryCatch({
      #     if (tools::file_ext(input$data$datapath) == "rds") {
      #       readRDS(input$data$datapath)
      #     } else {
      #       read.csv(input$data$datapath)
      #     }
      #   }, error = function(e) {
      #     showNotification(paste("Error:", e$message), type = "error")
      #     return(NULL)
      #   })
      # })


      # Reactive value to store the transformed data
      single_metabolites <- reactiveVal()
      single_metabolites_corrected <- reactiveVal()
      

      ###Pre-analytical step###
      #eventReactive(input$run, { #doesn't give output
      observeEvent(input$run, {
        observe({single_metabolites(sing_met(data = df(), start_met = input$smet, confounders = input$confounders))
        })
        
        #check if something is happening
        output$preview1 <- renderPrint({
          head(single_metabolites(), 10)
          })
          
        
        observe({single_metabolites_corrected(sing_met(data = df(), start_met = input$smet, confounders = input$confounders, correction = input$correction_method))
        })
        #single_metabolites_corrected()$p.value <- p.adjust(single_metabolites_corrected()$p.value, method = input$correction_method)
        #check if something is happening
        output$preview2 <- renderPrint({
          #input$confounders
          head(single_metabolites_corrected(), 10)
          })
          

      })

      return(single_metabolites)

    })}
