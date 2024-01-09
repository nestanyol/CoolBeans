mlsServer <- function(id, df_train, df_test, name, residual_met) {

  library(tidyr)
  library(tidyverse)
  library(skimr)
  library(CoolBeans)

  moduleServer(
    id,
  function(input, output, session) {
    
    # Reactive value to store the results
    n_folds <- reactiveVal()
    n_repeats <- reactiveVal()
    l_tune <- reactiveVal()
    model <- reactiveVal()
    coeff <- reactiveVal()
    results <- reactiveVal()
    
    

    observeEvent(input$run_train_cv,{
      train_data <- df_train()
      test_data <- df_test()
      
      observe({n_folds(input$nfolds)})
      observe({n_repeats(input$nrepeats)})
      observe({l_tune(input$ltune)})
      
    #   output$output_model <- renderPrint({
    #     head(train_data)
    # })

    if (input$model_type == "Linear Regression") {
          #train model
          observe({model(crossvalidation_model(train_data, model = "glmnet",
                                               nfolds=n_folds(), nrepeats=n_repeats(), ltune=l_tune()))})
      
          #coefficients
          observe({coeff(stats::coef(model()$finalModel, model()$bestTune$lambda))})
          
          #score
          features <- colnames(residual_met)
          
          #test model
          observe({results(crossvalidation_eval(model(), test_data, type = input$algorithm))})
          
          #outputs
          
          output$output_model <- renderPrint({
           model()
          })
          
          output$coefficients <- renderPrint({
            cat("Coefficients")
            head(coeff())
          })
          
          output$plot1 <- renderPlot({
            plot(model())
          })
          
          output$output_eval <- renderPrint({
            results()
          })

          # output$feature_imp <- renderPrint({
          #   results()[-1]
          # })
          } else if (input$model_type == "Random Forest") {
          # train model
            observe({model(crossvalidation_model(train_data, model = "ranger",
                                                 nfolds=n_folds(), nrepeats=n_repeats(), ltune=l_tune()))})
            #coefficients
            observe({coeff(stats::coef(model()$finalModel, model()$bestTune$lambda))})
            
            
            #test model
            observe({results(crossvalidation_eval(model(), test_data, type = input$algorithm))})
            
            #outputs
            
            output$output_model <- renderPrint({
              model()
            })
            
            output$coefficients <- renderPrint({
              cat("Coefficients")
              head(coeff())
            })
            
            output$plot1 <- renderPlot({
              plot(model())
            })
            
            output$output_eval <- renderPrint({
              results()
            })

          }
      
        })
  
          
    output$download <- downloadHandler(
      filename = function() {
        file <- name()[1]
        paste0(substr(file, 1, nchar(file)-4), "_model.rds")
      },
      content = function(file) {
        #vroom::vroom_write(model(), file)
        saveRDS(model(), file)
      }
    )

    return(list(nfolds = n_folds, nrepeats = n_repeats, ltune = l_tune,
                model_cv = model, eval_cv = results))
  
})
  
}