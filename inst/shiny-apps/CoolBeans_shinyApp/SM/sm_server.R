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
      data_filtered <- reactiveVal()
      
      ###Pre-analytical step###
      #eventReactive(input$run, { #doesn't give output
      observeEvent(input$run, {
        observe({single_metabolites(sing_met(data = df(), exposure_feature = "target", start_met = input$smet, threshold = input$pvalue, confounders = input$confounders, correction = input$correction_method))
        })
        
        #check output
        output$preview1 <- renderPrint({
          head(single_metabolites(), 10)

          #cat("Number of filtered metabolites", nrow(single_metabolites()))
          })
        #check if something is happening
        output$preview2 <- renderPrint({
          cat("Number of filtered metabolites", nrow(single_metabolites()))
        })
        
        output$plot1 <- renderPlot({
          met_pvalue <- single_metabolites() %>%
            ggplot2::ggplot(aes(x = yvar, y = p.value_corrected)) +
            geom_segment( aes(x=yvar, xend=yvar, y=0, yend=p.value_corrected), color="black") +
            geom_point( color="gray", size=2, alpha=0.6) +
            theme_light() +
            coord_flip() +
            labs(x = "Metabolites", y = "p-value") +
            theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 0.5))
            #theme(axis.text.y = element_text(size = 10))
          
          met_pvalue
        }, res = 96)
          
        #filter dataset based on metabolites with high p-value
        #observeEvent(input$select, {
        
        # observe({single_metabolites_corrected(sing_met(data = df(), exposure_feature = "target", start_met = input$smet, confounders = input$confounders, correction = input$correction_method))
        # })
        # #single_metabolites_corrected()$p.value <- p.adjust(single_metabolites_corrected()$p.value, method = input$correction_method)
        # #check if something is happening
        # output$preview2 <- renderPrint({
        #   #input$confounders
        #   head(single_metabolites_corrected(), 10)
        #   })
        
        #data_ML 
        observe({data_filtered(df()%>%
                select(id, target, single_metabolites()$yvar))
        })
      
      })
      
      return(data_filtered)    


    })
  }
