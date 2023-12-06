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
        #   head(original_data(), input$ncols)
        # })
        
        output$plot1 <- renderPlot({
          raw_data_boxplots <- original_data()[,c(input$ncols:(input$ncols+5))] %>% #change for select based on input vector
            tidyr::gather(key = "metabolites", value = "value", starts_with(input$key_plot))

          boxplot <- raw_data_boxplots %>%
            ggplot2::ggplot(aes(x = metabolites, y = value)) +
            ggplot2::geom_boxplot() +
            labs(x = "Metabolites", y = "Intensity") +
            theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1))

          boxplot
        }, res = 96)
      })
      
      # Reactive value to store the transformed data
      prep_data <- reactiveVal()
      file_name <- reactiveVal()
      id <- reactiveVal()
      target <- reactiveVal()
      start_met <- reactiveVal()
      cutoff_colums <- reactiveVal()
      cutoff_rows <- reactiveVal()
      imputation <- reactiveVal()
      
      
      ###Pre-analytical step###
      #eventReactive(input$run, { #doesn't give output
      observeEvent(input$run, {
        
        #ncols <- as.numeric(unlist(strsplit(input$ncols,",")))
        #data <- original_data()[,c(ncols[1],ncols[2],ncols[3]:ncol(original_data()))]
        data <- original_data()
        observe(file_name({as.character(input$data)}))
        
        # observe({prep_data(preprocess_data(data, input$id, input$target, 
        #                                    cutoff_met = input$na_cutoff/100, cutoff_subj = input$na_cutoff/100)) 
        # })
        
        observe({id(input$id)})
        observe({target(input$target)})
        observe({start_met(input$ncols)})
        observe({cutoff_colums(input$na_cutoffcol)})
        observe({cutoff_rows(input$na_cutoffrows)})
        observe({imputation(input$imputation_method)})
        
        
        observe({prep_data(preprocessing(data, id(), target(), start_metabolites = start_met(),
                                         cutoff_columns = cutoff_colums()/100, cutoff_rows = cutoff_rows()/100,
                                         imputation = imputation())) #extra slider to choose different cutoffs from rows and columns.
        })
        
        # extra step optional for outliers
        #check if something is happening
        # output$preview2 <- renderPrint({
        #   skim_without_charts(prep_data(),c(1:10))
        # })
        
        output$plot2 <- renderPlot({
          prep_data_boxplots <- prep_data()[,c(input$ncols:(input$ncols+5))] %>%
            tidyr::gather(key = "metabolites", value = "value", starts_with(input$key_plot))
          
          boxplot_prep <- prep_data_boxplots %>%
            ggplot2::ggplot(aes(x = metabolites, y = value)) +
            ggplot2::geom_boxplot() +
            labs(x = "Metabolites", y = "Intensity") +
            theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1))
          
          boxplot_prep
        }, res = 96)
        
        output$download <- downloadHandler(
          filename = function() {
            file <- as.character(input$data)
            paste0(substr(file, 1, nchar(file)-4), "_preprocessed.csv")
          },
          content = function(file) {
            vroom::vroom_write(prep_data(), file)
          }
        )
        
      })
      
      return(list(raw_data = original_data, preprocessed_data = prep_data, filename = file_name, idcol = id, 
                  targetcol = target, startmet = start_met, nacolumns = cutoff_colums, narows = cutoff_rows,
                  imput = imputation))
      
    })}