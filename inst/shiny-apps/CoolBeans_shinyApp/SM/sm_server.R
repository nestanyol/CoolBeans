# Module server function
smServer <- function(id) {
  #dwServer.R
  library(tidyr)
  library(tidyverse)
  library(skimr)
  library(CoolBeans)

  moduleServer(
    id,
    function(input, output, session){
      #Reactive expression to read the uploaded data
      input_data <- reactive({
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
          skim_without_charts(input_data(),c(1:input$columns))
        })})

      # Reactive value to store the transformed data
      single_metabolites <- reactiveVal()

      ###Pre-analytical step###
      #eventReactive(input$run, { #doesn't give output
      observeEvent(input$run, {
        observe({single_metabolites(sing_met(data = input_data(), start_met = input$smet, confounders = input$confounders))
        #observe({single_metabolites(sing_met(data = input_data(), start_met = input$smet, confounders = c("age", "sex")))

          })
        #check if something is happening
        output$preview2 <- renderPrint({
          #cat(input$confounders)
          head(single_metabolites(), 5)
        })

      })

      return(single_metabolites)

    })}
