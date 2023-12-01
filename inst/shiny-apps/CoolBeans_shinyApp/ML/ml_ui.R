mlUI <- function(id, label = 'machineLearning1') {
  ns <- NS(id)

  tagList(
    titlePanel("Machine learning"),

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
                      choices = c("Random Forest", "Linear Regression", "K-Nearest Neighbor"),
                      selected = "Random Forest"),
          # Select regression or classification
          #h4("Select algorithm"),
          prettyRadioButtons(ns("algorithm"), "algorithm:",
                             status = "default",
                       choices = list("regression" = "regression",
                                      "classification" = "classification"),
                       selected = "classification"),

          # Training and Validation Split for Supervised Learning
          #sliderInput(ns("split"), "Training Data Split (%):", min = 50, max = 90, value = 70, step = 5),

          # Train Model Button for Supervised Learning
          actionButton(ns("run_train"), "Train Model"),
          
          #Dowload preprocessed data
          downloadButton(ns("download"), "Download predictions .csv")
        ), 
        
        # wellPanel(
        #   downloadButton("report", "Generate report")
        #   )
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
