mlServer <- function(id, df_train, df_test, name) {

  library(tidyr)
  library(tidyverse)
  library(skimr)
  library(CoolBeans)

  moduleServer(
    id,
  function(input, output, session) {
    
    # Reactive value to store the results
    results <- reactiveVal()
    model <- reactiveVal()
  


    observeEvent(input$run_train,{
      train_data <- df_train()
      test_data <- df_test()
    #   output$output_model <- renderPrint({
    #     skim_without_charts(df(),c(1:10))
    # })

    if (input$model_type == "Linear Regression") {
          #train model
          observe({model(training_lr(train_data))})
          #test model
          observe({results(testing_lr(model(), test_data))})
          output$output_model <- renderPrint({
           model()
          })

          output$feature_imp <- renderPrint({
            results()[-1]
          })} else if (input$model_type == "Random Forest") {
            if(input$algorithm == 'regression'){

              #train model with regression option
              observe({model(training_rf(train_data, type=1))})
              #model <- training_rf(train_data, type=1)
              #test model
              observe({results(testing_rf_regression(model(), test_data))})

              output$output_model <- renderPrint({
                model()
              })

              output$feature_imp <- renderPrint({
                results()[-1]
              })

            } else if(input$algorithm == 'classification'){

              #train model with regression option
              observe({model(training_rf(train_data, type=2))})
              #model <- training_rf(train_data, type=2)
              #test model
              observe({results(testing_rf_classification(model(), test_data))})

              output$output_model <- renderPrint({
                model()
              })

              output$feature_imp <- renderPrint({
                results()[-1]
              })

            }
          } else if (input$model_type == "K-Nearest Neighbor") {
            if(input$algorithm == 'regression'){

              #train model with regression option
              observe({model(training_knn(train_data, type=1))})
              #model <- training_knn(train_data, type=1)
              #test model
              observe({results(testing_knn_regression(model(), test_data))})

              output$output_model <- renderPrint({
                model()
              })

              output$feature_imp <- renderPrint({
                results()[-1]
              })

            } else if(input$algorithm == 'classification'){

              #train model with regression option
              observe({model(training_knn(train_data, type=2))})
              #model <- training_knn(train_data, type=2)
              #test model
              observe({results(testing_knn_classification(model(), test_data))})

              output$output_model <- renderPrint({
                model()
              })

              output$feature_imp <- renderPrint({
                results()[-1]
              })

            }
          }
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

    return(list(model_trainig = model, model_testing = results))
  
})
  
}