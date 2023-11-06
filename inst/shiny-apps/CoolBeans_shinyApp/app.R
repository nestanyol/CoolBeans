library(shinydashboard)
library(shiny)
options(shiny.maxRequestSize = 900*1024^2)

source("DW/dw_ui.R")
source("DW/dw_server.R")

source("SM/sm_ui.R")
source("SM/sm_server.R")

source("ML/ml_ui.R")
source("ML/ml_server.R")


introUI <- function(id) {
  ns <- NS(id)

  fluidPage(

    titlePanel("Welcome to CoolBeans!"),

    p("High-throughput metabolomics approaches in human studies provide large datasets with complex correlation structures that reflect genetic, phenotypical, lifestyle and environmental influences. 
      At the same time, metabolomics data are strongly predictive of multiple disease outcomes and the multi-metabolite patterns (aka signatures) have proved instrumental in capturing the exposure 
      to complex lifestyles factors."),

    p("CoolBeans is a tool that leverages metabolomics data for multi-metabolite biomarker assessment."),

    tags$ul(
      tags$li("Data preprocessing"),
      tags$li("Dimension reduction"),
      tags$li("Multi-metabolite signature"),
      #tags$li("Understand the importance of different metabolites or features in your dataset.")
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
      menuItem("Metabolites selection", tabName = "singMetabolite", icon = icon("tachometer-alt")),
      menuItem("Machine Learning", tabName = "ml", icon = icon("robot"))#,
      #menuItem("Feature Importance", tabName = "feature_importance", icon = icon("list"))
    )
  ),
  body = dashboardBody(
    tabItems(
      tabItem(tabName = "intro", introUI("intro1")),
      tabItem(tabName = "dataWrangling", dwUI("dataWrangling1")),
      tabItem(tabName = "singMetabolite", smUI("singMetabolite1")),
      tabItem(tabName = "ml", mlUI("machineLearning1"))#,
      #tabItem(tabName = "feature_importance", h2("Feature Importance & Interpretability"))
    )
  ),
  title = "Metabolomics Analysis"
)


server <- function(input, output, session) {
  data_preprocessed <- dwServer(id = "dataWrangling1")
  #smServer(id = "singMetabolite1")
  data_filtered <- smServer(id = "singMetabolite1", df=data_preprocessed)
  mlServer(id = 'machineLearning1', df=data_filtered)
  #callModule(dwServer,"dataWrangling1")
  #callModule(mlServer, "ml1")
}


if (interactive()) {
  shinyApp(ui, server)
}
