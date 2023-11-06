mlServer <- function(id, df) {

  library(tidyr)
  library(tidyverse)
  library(skimr)
  library(CoolBeans)

  moduleServer(
    id,
  function(input, output, session) {

    observeEvent(input$run_split,{
        # Split data
        data_split <- split_data(df(), 'target', input$split/100)

        #split data
        train_data <- data_split$train_data
        test_data <- data_split$test_data

        #check and drop NAs in target column, otherwise ML won't run
        train_data <- train_data %>% drop_na('target')
        test_data <- test_data %>% drop_na('target')
        
        # output$output_model <- renderPrint({
        #   skim_without_charts(train_data,c(1:10))
        # })
   
        observeEvent(input$run_train, {
        #   output$output_model <- renderPrint({
        #     skim_without_charts(df(),c(1:10))
        # })
      
        if (input$model_type == "Linear Regression") {
          #train model
          model <- train_model(train_data)
          #test model
          results <- test_model(model, test_data)

          output$output_model <- renderPrint({
           model
          })
          
          output$feature_imp <- renderPrint({
            results
          })
          
          } else if (input$model_type == "Random Forest") {
            if(input$algorithm == 'regression'){
              
              #train model with regression option
              model <- train_model_rf(train_data, type=1)
              #test model
              results <- test_rf_regression(model, test_data)
              
              output$output_model <- renderPrint({
                model
              })
              
              output$feature_imp <- renderPrint({
                results
              })
      
            } else if(input$algorithm == 'classification'){
              
              #train model with regression option
              model <- train_model_rf(train_data, type=2)
              #test model
              results <- test_rf_classification(model, test_data)
              
              output$output_model <- renderPrint({
                model
              })
              
              output$feature_imp <- renderPrint({
                results
              })
      
            }
          }
        })
          })

  }
  )
  }
