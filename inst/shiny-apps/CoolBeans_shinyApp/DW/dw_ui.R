dwUI <- function(id, label = 'dataWrangling1') {
  ns <- NS(id)
  
  tagList(
    #fluidPage(
    titlePanel("Data Preprocessing"),
    #p("Here the data will be prepared for the ML step. Run will do the following: 1)remove duplicate columns, 2) remove columns/rows
       #with high Na number defined by the cutoff value, 3) data imputation with the option of 4 different methods, 4) log transformation and 5) normalization."),
    
    sidebarLayout(
      sidebarPanel(
        wellPanel(
          title = "Data Upload",
          fileInput(ns("data"), "Upload your dataset", accept = c(".rds", ".csv")),
          #numericInput(ns("columns"), "# columns to include in summary",10),
          #textInput(ns("key_plot"), "Key to use for plotting", "metabolite"),
          #Columns to select
          #varSelectInput(ns("namecols"), "Enter columns to plot:", data, multiple = TRUE),
          #textInput(ns("namecols"), "Enter columns to plot (comma delimited)", "metabolite_1, metabolite_2, metabolite_3"),
          selectizeInput(ns("namecols"),  "Enter the columns list",
                         choices = NULL,
                         multiple =TRUE,
                         options = list(create = TRUE)),
          actionButton(ns("plot_raw"), "Plot")
        ),
        
        wellPanel(
          title = "Pre-analitical step",
          h4("Do pre-analitical step"),
          #Insert column names for ID and target
          textInput(ns("id"), "Type id column", "id"),
          textInput(ns("target"), "Type target feature column", "exposure"),
          numericInput(ns("ncols"), "Enter first column with metabolites", 5),
          # columns cutoff
          sliderInput(ns("na_cutoffcol"), "Na cutoff columns (%):", min = 0, max = 100, value = 20, step = 10),
          # rows cutoff
          sliderInput(ns("na_cutoffrows"), "Na cutoff rows (%):", min = 0, max = 100, value = 20, step = 10),
          # select imputation method
          selectInput(ns("imputation_method"), "Select imputation method", choices = list("mean",
                                                                                          "median",
                                                                                          "knn",
                                                                                          "lower")),
          #run the pre-analytical step
          actionButton(ns("run"), "Run"),
          
          #Dowload preprocessed data
          downloadButton(ns("download"), "Download preprossed .csv")
        )
        
        # wellPanel(
        # title = "Data Splitting",
        # numericInput(ns("train_ratio"), "Training Set Ratio:", 0.7, min = 0.1, max = 0.9, step = 0.1),
        # actionButton(ns("split_data"), "Split"))
      ),
      
      mainPanel(
        # Model Evaluation Outputs
        tabsetPanel(
          tabPanel("Raw Data",
                   #verbatimTextOutput(ns("preview1"))
                   plotOutput(ns("plot1"))
                  
                   ),
          tabPanel("Data Pre-Analitical",
                   #verbatimTextOutput(ns("preview2")))
                   plotOutput(ns("plot2")))
                   
        )
      )
    )
  )
}