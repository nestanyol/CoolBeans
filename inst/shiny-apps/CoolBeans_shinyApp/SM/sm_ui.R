smUI <- function(id, label = 'singMetabolite1') {
  ns <- NS(id)

  tagList(
    #fluidPage(
    titlePanel("Single metabolite analysis"),
    p(""),

    sidebarLayout(
      sidebarPanel(

        # wellPanel(
        #   title = "Data Upload",
        #   fileInput(ns("data"), "Upload your dataset", accept = c(".rds", ".csv")),
        #   numericInput(ns("columns"), "# columns to include in summary",10),
        #   actionButton(ns("summary"), "Summary")
        # ),

        wellPanel(
          title = "Single metabolite analysis",
          #h4("Do pre-analitical step"),
          #Column number where metabolites start
          numericInput(ns("smet"), "Column where metabolites start",14),
          #Columns to select
          #textInput(ns("confounders"), "Enter the confounders list", "id, sex, etc"),
          selectizeInput(ns("confounders"),  "Enter the confounders list",
                         choices = NULL,
                         multiple =TRUE,
                         options = list(create = TRUE)),
          selectInput(ns("correction_method"), "Select correction method", choices = list("fdr",
                                                                                          "bonferroni")),
          #p-value
          numericInput(ns("pvalue"), "Insert p-value threshold after correction",0.01),
          actionButton(ns("run"), "Run"),
          
          #Dowload preprocessed data
          downloadButton(ns("download"), "Download .csv")
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
          tabPanel("Preview Model Output",
                   verbatimTextOutput(ns("preview1")),
                   verbatimTextOutput(ns("preview2"))),
          tabPanel("P-value",
                   plotOutput(ns("plot1"), height = 500))
        )
      )
    )
  )
}
