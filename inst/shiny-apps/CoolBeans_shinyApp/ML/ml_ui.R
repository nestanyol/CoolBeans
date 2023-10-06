mlUI <- function(id, label = 'machineLearning1') {
  ns <- NS(id)

  tagList(
    titlePanel("Machine Learning on Metabolomics Data"),

    sidebarLayout(
      sidebarPanel(
        
        wellPanel(
          # Training and Validation Split for Supervised Learning
          sliderInput(ns("split"), "Training Data Split (%):", min = 50, max = 90, value = 70, step = 5),

          # Train Model Button for Supervised Learning
          actionButton(ns("run_split"), "Split Data")

        ),

        wellPanel(
          # Model Selection for Supervised Learning
          selectInput(ns("model_type"), "Choose Model:",
                      #choices = c("Linear Regression", "Random Forest", "SVM", "Neural Network", "k-NN"),
                      choices = c("Linear Regression", "Random Forest"),
                      selected = "Linear Regression"),
          # Select regression or classification
          #h4("Select algorithm"),
          radioButtons(ns("algorithm"), "algorithm:",
                       choices = list("regression" = "regression",
                                      "classification" = "classification"),
                       selected = "regression"),

          # Training and Validation Split for Supervised Learning
          #sliderInput(ns("split"), "Training Data Split (%):", min = 50, max = 90, value = 70, step = 5),

          # Train Model Button for Supervised Learning
          actionButton(ns("run_train"), "Train Model")
        )
        ),

      mainPanel(
        # Model Evaluation Outputs
        tabsetPanel(
          tabPanel("Output model",
                   verbatimTextOutput(ns("output_model"))),
          tabPanel("Feature Importance",
                   verbatimTextOutput(ns("feature_imp")))
        )
      )
    )
  )
}
