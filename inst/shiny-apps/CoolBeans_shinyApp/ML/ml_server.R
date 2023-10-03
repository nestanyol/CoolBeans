mlServer <- function(id) {
  
  moduleServer(
    id, 
  function(input, output, session) {
  
  observeEvent(input$learning_type, {
    if (input$learning_type == "Supervised Learning") {
      output$dynamic_hyperparameters_supervised <- renderUI({
        if (input$supervised_model_type == "Linear Regression") {
          numericInput(session$ns("reg_param"), "Regularization Parameter:", value = 0.01, min = 0, step = 0.01)
        } else if (input$supervised_model_type == "Random Forest") {
          list(
            numericInput(session$ns("n_trees"), "Number of Trees:", value = 100, min = 1),
            numericInput(session$ns("mtry"), "Number of Variables Randomly Sampled:", value = 3, min = 1)
          )
        } 
        #      else if (input$supervised_model_type == "SVM") {
        #       list(
        #         selectInput(session$ns("kernel"), "Kernel:", choices = c("linear", "polynomial", "radial", "sigmoid")),
        #         numericInput(session$ns("cost"), "Cost:", value = 1, min = 0.1)
        #       )
        #     } else if (input$supervised_model_type == "Neural Network") {
        #       list(
        #         numericInput(session$ns("hidden_units"), "Number of Hidden Units:", value = 10, min = 1),
        #         numericInput(session$ns("learning_rate"), "Learning Rate:", value = 0.01, min = 0.001, step = 0.001)
        #       )
        #     } else if (input$supervised_model_type == "k-NN") {
        #       numericInput(session$ns("neighbors"), "Number of Neighbors:", value = 5, min = 1)
        #     }
        #   })
        # } else if (input$learning_type == "Unsupervised Learning") {
        #   output$dynamic_hyperparameters_unsupervised <- renderUI({
        #     if (input$unsupervised_model_type == "K-Means") {
        #       numericInput(session$ns("clusters"), "Number of Clusters:", value = 3, min = 1)
        #     }
      })
    }
  })
  
  }
)
}
