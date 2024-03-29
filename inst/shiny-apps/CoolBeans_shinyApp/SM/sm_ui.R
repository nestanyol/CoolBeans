smUI <- function(id, label = 'singMetabolite1') {
  ns <- NS(id)

  tagList(
    #fluidPage(
    titlePanel("Dimension reduction"),
    p("Single metabolite selection will reduce the number of total metabolites using the following formula."),
    strong("Metabolite ~ β0P + β1C + ... + Ԑ"),
    br(),
    br(),

    sidebarLayout(
      sidebarPanel(

        wellPanel(
          title = "Start from a preprocessed file",
          h4("Start from a preprocessed file"),
          fileInput(ns("data"), "Upload your dataset", accept = c(".rds", ".csv")),
          #Column number where metabolites start
          numericInput(ns("smet"), "Column where metabolites start",5),
          #textInput(ns("ncols"), "Enter first column with metabolites", "14"),
          h4("Or from previous step"),
          checkboxInput(ns("use"), "Use data from previous step", TRUE),
          actionButton(ns("load"), "Load")
        ),
        
        wellPanel(
          # Training and Validation Split for Supervised Learning
          sliderInput(ns("split"), "Training Data Split (%):", min = 50, max = 90, value = 70, step = 5),
          
          # Train Model Button for Supervised Learning
          actionButton(ns("run_split"), "Split Data")
          
        ),

        wellPanel(
          title = "Preprocessed data from shiny",
          #h4("Run"),
          #Box to check if analysis is done with preprocessed data from previous step
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
          actionButton(ns("run"), "Run"),
          
          #Dowload preprocessed data
          downloadButton(ns("download"), "Download metabolites .csv")
        ),

        # wellPanel(
        # title = "Data for machine learning model",
        # selectInput(ns("whichdata"), "Select which data to use in ML", choices = list("original",
        #                                                                                 "after single metabolite selection")),
        # actionButton(ns("select"), "Select"))
      ),

      mainPanel(
        # Model Evaluation Outputs
        tabsetPanel(
          tabPanel("Output",
                   verbatimTextOutput(ns("preview1")),
                   verbatimTextOutput(ns("preview2"))),
          tabPanel("P-value",
                   plotOutput(ns("plot1"), height = 500))
        )
      )
    )
  )
}
