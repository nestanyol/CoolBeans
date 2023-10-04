mlServer <- function(id, df) {

  library(tidyr)
  library(tidyverse)
  library(skimr)
  library(CoolBeans)

  moduleServer(
    id,
  function(input, output, session) {
    observeEvent(input$train_model, {
      output$output_model <- renderPrint({
        print(c(1:10))
      })
      # Split data
      data_split <- split_data(df(), 'target', input$split/100)

      #split data
      train_data <- data_split$train_data
      test_data <- data_split$test_data

      #check and drop NAs in target column, otherwise ML won't run
      train_data <- train_data %>% drop_na('target')
      test_data <- test_data %>% drop_na('target')


      if (input$model_type == "Linear Regression") {
        model <- train_model(train_data)
        #output$output_model <- renderPrint({
        #  model
        #})
        } else if (input$model_type == "Random Forest") {
          if(input$algorithm == 'regression'){

          } else if(input$algorithm == 'classification'){

          }
        }
  })

  }
)
}
