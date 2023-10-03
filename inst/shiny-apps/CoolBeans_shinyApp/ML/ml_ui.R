mlUI <- function(id, label = 'ML') {
  ns <- NS(id)
  
  fluidPage(
    titlePanel("Machine Learning on Metabolomics Data"),
    
    sidebarLayout(
      sidebarPanel(
        tabsetPanel(
          id = ns("learning_type"),
          tabPanel("Supervised Learning",
                   # Model Selection for Supervised Learning
                   selectInput(ns("supervised_model_type"), "Choose Model:",
                               #choices = c("Linear Regression", "Random Forest", "SVM", "Neural Network", "k-NN"),
                               choices = c("Linear Regression", "Random Forest"),
                               selected = "Linear Regression"),
                   
                   # Dynamic Hyperparameters for Supervised Learning
                   #uiOutput(ns("dynamic_hyperparameters_supervised")),
                   
                   # Training and Validation Split for Supervised Learning
                   sliderInput(ns("train_split_supervised"), "Training Data Split (%):", min = 50, max = 90, value = 70, step = 5),
                   
                   # Train Model Button for Supervised Learning
                   actionButton(ns("train_model_supervised"), "Train Model")
          ),
          # tabPanel("Unsupervised Learning",
          #          # Model Selection for Unsupervised Learning
          #          selectInput(ns("unsupervised_model_type"), "Choose Model:",
          #                      choices = c("K-Means", "Hierarchical Clustering", "DBSCAN"),
          #                      selected = "K-Means"),
          # 
          #          # Dynamic Hyperparameters for Unsupervised Learning
          #          uiOutput(ns("dynamic_hyperparameters_unsupervised")),
          # 
          #          # Train Model Button for Unsupervised Learning
          #          actionButton(ns("train_model_unsupervised"), "Train Model")
          # )
        )
      ),
      
      mainPanel(
        # Model Evaluation Outputs
        tabsetPanel(
          tabPanel("Raw Data Summary", verbatimTextOutput(ns("raw_data_summary"))),
          tabPanel("Visualization", verbatimTextOutput(ns("vis")))
        )
      )
    )
  )
}
