# Module server function
dwServer <- function(id) {
  #dwServer.R
  library(tidyr)
  library(tidyverse)
  library(ggplot2)
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
      
      observeEvent(input$plot_raw, {
        # output$preview1 <- renderPrint({
        #   skim_without_charts(original_data(),c(1:input$columns))
        # })
        output$plot1 <- renderPlot({
          raw_data_boxplots <- original_data()[,c(1:20)] %>%
            tidyr::gather(key = "HN", value = "value", starts_with("HN"))
          
          boxplot <- raw_data_boxplots %>%
            ggplot2::ggplot(aes(x = HN, y = value)) +
            ggplot2::geom_boxplot() +
            labs(x = "Metabolites", y = "Intensity") +
            theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1))
          
          boxplot
        }, res = 96)
      })
      
      # Reactive value to store the transformed data
      prep_data <- reactiveVal()
      
      ###Pre-analytical step###
      #eventReactive(input$run, { #doesn't give output
      observeEvent(input$run, {
        
        #ncols <- as.numeric(unlist(strsplit(input$ncols,",")))
        #data <- original_data()[,c(ncols[1],ncols[2],ncols[3]:ncol(original_data()))]
        data <- original_data()
        
        # observe({prep_data(preprocess_data(data, input$id, input$target, 
        #                                    cutoff_met = input$na_cutoff/100, cutoff_subj = input$na_cutoff/100)) 
        # })
        
        observe({prep_data(preprocess_data_all(data, input$id, input$target, input$ncols,
                                           cutoff_met = input$na_cutoff/100, cutoff_subj = input$na_cutoff/100,
                                           imputation = input$imputation_method))
        })
        
        #check if something is happening
        # output$preview2 <- renderPrint({
        #   skim_without_charts(prep_data(),c(1:10))
        # })
        
        output$plot2 <- renderPlot({
          prep_data_boxplots <- prep_data()[,c(1:20)] %>%
            tidyr::gather(key = "HN", value = "value", starts_with("HN"))
          
          boxplot_prep <- prep_data_boxplots %>%
            ggplot2::ggplot(aes(x = HN, y = value)) +
            ggplot2::geom_boxplot() +
            labs(x = "Metabolites", y = "Intensity") +
            theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1))
          
          boxplot_prep
        }, res = 96)
        
      })
      
      return(prep_data)
      
    })}