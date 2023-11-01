smUI <- function(id, label = 'singMetabolite1') {
  ns <- NS(id)

  tagList(
    #fluidPage(
    titlePanel("Single metabolites analysis"),
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
          numericInput(ns("smet"), "Column where metabolites start",10),
          #Columns to select
          #textInput(ns("confounders"), "Enter the confounders list", "id, sex, etc"),
          selectizeInput(ns("confounders"),  "Enter the confounders list",
                         choices = NULL,
                         multiple =TRUE,
                         options = list(create = TRUE)),
          selectInput(ns("correction_method"), "Select correction method", choices = list("fdr",
                                                                                          "bonferroni")),
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
          tabPanel("Model Output Before Correction",
                   verbatimTextOutput(ns("preview1"))),
          tabPanel("Model Output After Correction",
                   verbatimTextOutput(ns("preview2")))
        )
      )
    )
  )
}
