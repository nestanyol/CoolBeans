# Module server function
smServer <- function(id, df, name) {
  #dwServer.R
  library(tidyr)
  library(tidyverse)
  library(skimr)
  library(future)
  library(CoolBeans)

  moduleServer(
    id,
    function(input, output, session){
      #Reactive expression to read the uploaded data
      loaded_data <- reactive({
        req(input$data)
        tryCatch({
          if (tools::file_ext(input$data$datapath) == "rds") {
            readRDS(input$data$datapath)
          } else {
            read.csv(input$data$datapath, sep="\t")
          }
        }, error = function(e) {
          showNotification(paste("Error:", e$message), type = "error")
          return(NULL)
        })
      })

      # Reactive value to store the transformed data
      data_to_use <- reactiveVal()
      single_metabolites <- reactiveVal()
      data_filtered <- reactiveVal()
      file_name <- reactiveVal()
      
      ###Preprocessed data###
      observeEvent(input$load,{
        observe({file_name(as.character(input$data))})
        observe({data_to_use(loaded_data())})
        
        #check output
        output$preview1 <- renderPrint({
          dim(data_to_use())
          #cat("Number of filtered metabolites", nrow(single_metabolites()))
        })

        output$preview2 <- renderPrint({
          file_name()[1]
          #cat("Number of filtered metabolites", nrow(single_metabolites()))
        })
      })
      
      ###Pre-analytical step###
      #eventReactive(input$run, { #doesn't give output
      observeEvent(input$run,{
        if(input$use){
          data_to_use(df())
          file_name(name())
        }
        
        observe({single_metabolites(sing_met_analysis(data = data_to_use(), exposure_feature = input$target, start_metabolites = input$smet, threshold = input$pvalue, covariates = input$covariates, correction = input$correction_method))
        
        })
        
        #check output
        output$preview1 <- renderPrint({
          head(single_metabolites(), 10)

          #cat("Number of filtered metabolites", nrow(single_metabolites()))
          })
        #check if something is happening
        output$preview2 <- renderPrint({
          #file_name()[1]
          cat("Number of filtered metabolites", nrow(single_metabolites()))
        })
        
        output$plot1 <- renderPlot({
          if(nrow(single_metabolites()) >= 10){
            top_metabolites <- single_metabolites()[1:10,]
          } else {
            top_metabolites <- single_metabolites()
          }
          
          met_pvalue <- top_metabolites %>%
            ggplot2::ggplot(aes(x = yvar, y = p.value_corrected)) +
            geom_segment(aes(x=yvar, xend=yvar, y=0, yend=p.value_corrected), color="black", linewidth = 1) +
            geom_point( color="gray", size=3, alpha=0.6) +
            theme_light() +
            coord_flip() +
            labs(x = "Metabolites", y = "p-value") +
            theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 0.5))
            #theme(axis.text.y = element_text(size = 10))
          
          met_pvalue
        }, res = 96)
        
        output$download <- downloadHandler(
          filename = function() {
            #Name for files
            file <- file_name()[1]
            paste0(substr(file, 1, nchar(file)-4), "_single_metabolite_analysis.csv")
          },
          content = function(file) {
            vroom::vroom_write(single_metabolites(), file)
          }
        )
          
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
        
        # #data_ML 
        observe({data_filtered(data_to_use()%>%
                select(id, target, single_metabolites()$yvar))
        })
        
      
      })
      
      return(list(singlemetabolites = single_metabolites, datafiltered = data_filtered, filename = file_name))    


    })
  }
