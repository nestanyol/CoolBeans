mlsUI <- function(id, label = 'multimetsignature1') {
  ns <- NS(id)
  tagList(
    titlePanel("Machine learning (ML) with crossvalidation"),
    p("In this section you can test run a machine learning model with crossvalidation"),
    br(),
    
    sidebarLayout(
      sidebarPanel(
        wellPanel(
          # Model Selection for Supervised Learning
          selectInput(ns("model_type"), "Choose Model:",
                      choices = c("Linear Regression", "Random Forest"),
                      selected = "Linear Regression"),
          # Select regression or classification
          #h4("Select algorithm"),
          prettyRadioButtons(ns("algorithm"), "Problem type:",
                             status = "default",
                       choices = list("regression" = "regression",
                                      "classification" = "classification"),
                       selected = "regression"),
          #Input for function
          numericInput(ns("nfolds"), "Enter number of folds", 2),
          numericInput(ns("nrepeats"), "Enter number of repeats", 2),
          numericInput(ns("ltune"), "Enter tune lenght", 5),
          # Train Model Button for Supervised Learning
          actionButton(ns("run_train_cv"), "Run Model with CV"),
          #Dowload preprocessed data
          downloadButton(ns("download"), "Download model .rds")
        )
        ),

      mainPanel(
        # Model Evaluation Outputs
        tabsetPanel(
          tabPanel("Model",
                   verbatimTextOutput(ns("output_model")),
                   verbatimTextOutput(ns("coefficients")),
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
