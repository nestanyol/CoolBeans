mlUI <- function(id, label = 'machineLearning1') {
  ns <- NS(id)

  tagList(
    titlePanel("Testing machine learning (ML) models"),
    p("In this section you can test you can test a couple of machine learning models"),
    br(),

    sidebarLayout(
      sidebarPanel(
        

        wellPanel(
          # Model Selection for Supervised Learning
          selectInput(ns("model_type"), "Choose Model:",
                      #choices = c("Linear Regression", "Random Forest", "SVM", "Neural Network", "k-NN"),
                      choices = c("Linear Regression", "Random Forest", "K-Nearest Neighbor"),
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

          # Train Model Button for Supervised Learning
          actionButton(ns("run_train"), "Run Model"),
          
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
          tabPanel("Output training",
                   verbatimTextOutput(ns("output_model"))),
          tabPanel("Output testing",
                   verbatimTextOutput(ns("feature_imp")))
        )
      )
    )
  )
}
