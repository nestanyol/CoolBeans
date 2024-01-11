scoreUI <- function(id, label = 'score1') {
  ns <- NS(id)
  
  tagList(
    titlePanel("Multi-metabolite signature"),
    
    sidebarLayout(
      sidebarPanel(
        
        
        wellPanel(
          textInput(ns("target"), "Type exposure feature column", "target"),
          #Columns to select
          #textInput(ns("confounders"), "Enter the confounders list", "id, sex, etc"),
          selectizeInput(ns("covariates"),  "Enter the covariates list",
                         choices = NULL,
                         multiple =TRUE,
                         options = list(create = TRUE)),
          selectInput(ns("correction_method"), "Select correction method", choices = list("fdr",
                                                                                          "bonferroni")),
          #p-value
          numericInput(ns("pvalue"), "Insert p-value threshold after correction",0.01),
          # Calculate scores
          actionButton(ns("calculate"), "Calculate scores"),
          
          # Train Model Button for Supervised Learning
          actionButton(ns("Plot"), "Plot distribution"),
          
          #Dowload preprocessed data
          #downloadButton(ns("download"), "Download model .rds")
        ), 
        
        wellPanel(
          downloadButton("download", "Download scores .csv")
          )
      ),
      
      mainPanel(
        # Model Evaluation Outputs
        tabsetPanel(
          tabPanel("Preview scores",
                   verbatimTextOutput(ns("output_score"))),
          tabPanel("Scores distribution",
                   #verbatimTextOutput(ns("feature_imp"))
                   plotOutput(ns("plot1")))
        )
      )
    )
  )
}