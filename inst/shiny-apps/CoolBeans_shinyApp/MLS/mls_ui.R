mlsUI <- function(id, label = 'multimetsignature1') {
  ns <- NS(id)

  tagList(
    titlePanel("Multi-metabolite signature"),

    sidebarLayout(
      sidebarPanel(
        

        wellPanel(
          # Model Selection for Supervised Learning
          selectInput(ns("model_type"), "Choose Model:",
                      #choices = c("Linear Regression", "Random Forest", "SVM", "Neural Network", "k-NN"),
                      choices = c("Linear Regression", "Random Forest"),
                      selected = "Linear Regression"),
          # Select regression or classification
          #h4("Select algorithm"),
          prettyRadioButtons(ns("algorithm"), "Problem type:",
                             status = "default",
                       choices = list("regression" = "regression",
                                      "classification" = "classification"),
                       selected = "regression"),

          # Training and Validation Split for Supervised Learning
          #sliderInput(ns("split"), "Training Data Split (%):", min = 50, max = 90, value = 70, step = 5),
          
          #Input for function
          numericInput(ns("nfolds"), "Enter number of folds", 2),
          numericInput(ns("nrepeats"), "Enter number of repeats", 2),
          numericInput(ns("ltune"), "Enter tune lenght", 5),
          
          
          # Train Model Button for Supervised Learning
          actionButton(ns("run_train_cv"), "Run Model with CV"),
          
          #Dowload preprocessed data
          downloadButton(ns("download"), "Download model .rds")
        ), 
        
        # wellPanel(
        #   downloadButton("report", "Generate report")
        #   )
        ),

      mainPanel(
        # Model Evaluation Outputs
        tabsetPanel(
          tabPanel("Model",
                   verbatimTextOutput(ns("output_model")),
                   verbatimTextOutput(ns("coefficients")),
                   #verbatimTextOutput(ns("score"))
                   ),
          tabPanel("Plot",
                   #verbatimTextOutput(ns("feature_imp"))
                   plotOutput(ns("plot1"))),
          tabPanel("Evaluation",
                   verbatimTextOutput(ns("output_eval")))
        )
      )
    )
  )
}
