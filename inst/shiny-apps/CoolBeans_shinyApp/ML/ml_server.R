mlServer <- function(id, df, name) {

  library(tidyr)
  library(tidyverse)
  library(skimr)
  library(CoolBeans)

  moduleServer(
    id,
  function(input, output, session) {
    
    # Reactive value to store the results
    results <- reactiveVal()
    coefficients <- reactiveVal()

    observeEvent(input$run_split,{
        # Split data
        data_split <- splitting(df(), 'target', input$split/100)

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
          model <- training_lr(train_data)
          #test model
          observe({results(testing_lr(model, test_data))})
          output$output_model <- renderPrint({
           model
          })

          output$feature_imp <- renderPrint({
            results()[-1]
          })

          } else if (input$model_type == "Random Forest") {
            if(input$algorithm == 'regression'){

              #train model with regression option
              model <- training_rf(train_data, type=1)
              #test model
              observe({results(testing_rf_regression(model, test_data))})

              output$output_model <- renderPrint({
                model
              })

              output$feature_imp <- renderPrint({
                results()[-1]
              })

            } else if(input$algorithm == 'classification'){

              #train model with regression option
              model <- training_rf(train_data, type=2)
              #test model
              observe({results(testing_rf_classification(model, test_data))})

              output$output_model <- renderPrint({
                model
              })

              output$feature_imp <- renderPrint({
                results()[-1]
              })

            }
          } else if (input$model_type == "K-Nearest Neighbor") {
            if(input$algorithm == 'regression'){

              #train model with regression option
              model <- training_knn(train_data, type=1)
              #test model
              observe({results(testing_knn_regression(model, test_data))})

              output$output_model <- renderPrint({
                model
              })

              output$feature_imp <- renderPrint({
                results()[-1]
              })

            } else if(input$algorithm == 'classification'){

              #train model with regression option
              model <- training_knn(train_data, type=2)
              #test model
              observe({results(testing_knn_classification(model, test_data))})

              output$output_model <- renderPrint({
                model
              })

              output$feature_imp <- renderPrint({
                results()[-1]
              })

            }
          }
        })
          
        #Save coefficients for RMarkdown
        observe({coefficients(results()[2])})
        })
    
    output$download <- downloadHandler(
      filename = function() {
        file <- name()[1]
        paste0(substr(file, 1, nchar(file)-4), "_predictionsML.csv")
      },
      content = function(file) {
        vroom::vroom_write(results()$prediction, file)
      }
    )

    return(model_results = coefficients)
  })
  }
