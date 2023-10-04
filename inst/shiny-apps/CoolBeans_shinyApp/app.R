library(shinydashboard)
library(shiny)
options(shiny.maxRequestSize = 500*1024^2)

source("DW/dw_ui.R")
source("DW/dw_server.R")

source("ML/ml_ui.R")
source("ML/ml_server.R")


introUI <- function(id) {
  ns <- NS(id)

  fluidPage(

    titlePanel("Welcome to the Metabolomics Data Analysis Platform!"),

    p("Metabolomics, a branch of 'omics' science, focuses on the comprehensive analysis of metabolites in biological samples.
      This platform provides a comprehensive suite of tools tailored for metabolomics data analysis."),

    p("What you can do with this platform:"),

    tags$ul(
      tags$li("Preprocess your data to make it suitable for machine learning."),
      tags$li("Apply various machine learning algorithms to predict outcomes or classify samples."),
      tags$li("Understand the importance of different metabolites or features in your dataset.")
    ),

    p("To get started, upload your dataset and navigate through the tabs to access the different functionalities.
      Each tab provides specific tools and visualizations to guide you through the data analysis process."),

    h3("Need Help?"),

    p("If you encounter any issues or have questions about specific functionalities, please refer to the documentation or contact the support team."),

  )
}



ui <- dashboardPage(
  header = dashboardHeader(title = "Metabolomics Data Analysis"),
  sidebar = dashboardSidebar(
    sidebarMenu(
      menuItem("Introduction", tabName = "intro", icon = icon("info-circle")),
      menuItem("Data Wrangling", tabName = "dataWrangling", icon = icon("cogs")),
      menuItem("Machine Learning", tabName = "ml", icon = icon("robot"))#,
      #menuItem("Feature Importance", tabName = "feature_importance", icon = icon("list"))
    )
  ),
  body = dashboardBody(
    tabItems(
      tabItem(tabName = "intro", introUI("intro1")),
      tabItem(tabName = "dataWrangling", dwUI("dataWrangling1")),
      tabItem(tabName = "ml", mlUI("ml1"))#,
      #tabItem(tabName = "feature_importance", h2("Feature Importance & Interpretability"))
    )
  ),
  title = "Metabolomics Analysis"
)


server <- function(input, output, session) {
  prep_data <- dwServer(id = "dataWrangling1")
  mlServer(id = 'machineLearning1',
                 df = prep_data)
  #callModule(dwServer,"dataWrangling1")
  #callModule(mlServer, "ml1")
}


if (interactive()) {
  shinyApp(ui, server)
}
