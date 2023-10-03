dwUI <- function(id, label = 'DW') {
  ns <- NS(id)

  tagList(
   #fluidPage(
     titlePanel("Data Wrangling"),
     p("Here the data will be prepared for the ML step. Run will do the following: 1)remove duplicate columns, 2) remove columns/rows 
       with high Na number defined by the cutoff value, 3) data imputation using the mean, 4) log transformation and 5) normalization."),
 
     sidebarLayout(
       sidebarPanel(
         wellPanel(
           title = "Data Upload",
           fileInput(ns("data"), "Upload your dataset", accept = c(".rds", ".csv")),
           numericInput(ns("columns"), "# columns to include in summary",10),
           actionButton(ns("summary"), "Summary")
         ),
         
         wellPanel(
             title = "Pre-analitical step",
             h4("Do pre-analitical step"),
             #Insert column names for ID and target
             textInput(ns("id"), "id column", "Enter text"),
             textInput(ns("target"), "target column", "Enter text"),
             # columns cutoff
             sliderInput(ns("na_cutoff"), "Na cutoff (%):", min = 0, max = 100, value = 20, step = 10),
             #run the pre-analytical step
             actionButton(ns("run"), "Run")
             )
         
             # wellPanel(
             # title = "Data Splitting",
             # numericInput(ns("train_ratio"), "Training Set Ratio:", 0.7, min = 0.1, max = 0.9, step = 0.1),
             # actionButton(ns("split_data"), "Split"))
       ),
 
       mainPanel(
         # Model Evaluation Outputs
         tabsetPanel(
           tabPanel("Raw Data Summary", 
                    verbatimTextOutput(ns("preview1"))),
           tabPanel("Visualization", 
                    verbatimTextOutput(ns("vis")))
         )
       )
     )
   )
}
