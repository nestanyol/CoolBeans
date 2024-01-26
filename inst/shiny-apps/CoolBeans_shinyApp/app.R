library(shinydashboard)
library(shiny)
library(fresh)
library(shinyWidgets)

# Specify the application port, docker
# options(shiny.host = "0.0.0.0")
# options(shiny.port = 8180)

options(shiny.maxRequestSize = 900*1024^2)

source("DW/dw_ui.R")
source("DW/dw_server.R")

source("SM/sm_ui.R")
source("SM/sm_server.R")

source("ML/ml_ui.R")
source("ML/ml_server.R")

source("MLS/mls_ui.R")
source("MLS/mls_server.R")

source("score/score_ui.R")
source("score/score_server.R")

source("reports/rep_ui.R")
source("reports/rep_server.R")


# Create the theme to control appearance app
mytheme <- create_theme(
  bs_vars_color(
    gray_base = "#101719", #text in panel
    brand_primary = "#101719",
    brand_info = "#101719"
  ),
  bs_vars_input(
    color = "#101719",
    color_placeholder = "#101719"),
  
  bs_vars_button(
   primary_bg = "#101719"),
  
  bs_vars_wells(
    bg = "#FFF",
    border = "#101719"
  )
)


introUI <- function(id) {
  ns <- NS(id)

  fluidPage(

    titlePanel("Welcome to CoolBeans!"),

    p("High-throughput metabolomics approaches in human studies provide large datasets with complex correlation structures that reflect genetic, phenotypical, lifestyle and environmental influences. 
      At the same time, metabolomics data are strongly predictive of multiple disease outcomes and the multi-metabolite patterns (aka signatures) have proved instrumental in capturing the exposure 
      to complex lifestyles factors."),

    p("CoolBeans is a tool that leverages metabolomics data for multi-metabolite biomarker assessment. Here you can do:"),

    tags$ul(
      tags$li("Data preprocessing"),
      tags$li("Dimension reduction"),
      tags$li("Testing machine learning (ML) models"),
      tags$li("Machine learning (ML) with crossvalidation"),
      tags$li("Multi metabolite score calculation")
      #tags$li("Understand the importance of different metabolites or features in your dataset.")
    ),

    p("To get started, upload your dataset and navigate through the tabs to access the different functionalities.
      Each tab provides specific tools and visualizations to guide you through the data analysis process."),

    h3("Need Help?"),

    p("If you encounter any issues or have questions about specific functionalities, please refer to the documentation or contact the support team."),

  )
}



ui <- dashboardPage(

  skin = "black", #
  header = dashboardHeader(title = "Metabolomics Data Analysis"),

  sidebar = dashboardSidebar(
    sidebarMenu(
      menuItem("Introduction", tabName = "intro", icon = icon("info-circle")),
      menuItem("Data Preprocessing", tabName = "dataWrangling", icon = icon("cogs")),
      menuItem("Dimension reduction", tabName = "singMetabolite", icon = icon("tachometer-alt")),
      menuItem("Testing ML models", tabName = "ml", icon = icon("calendar")),
      menuItem("ML with crossvalidation", tabName = "mls", icon = icon("robot")),
      menuItem("Multi-metabolite score", tabName = "score", icon = icon("bell")),
      menuItem("Generate report", tabName = "report", icon = icon("list"))
    )
  ),
  body = dashboardBody(
    use_theme(mytheme), # <-- use the theme
    chooseSliderSkin("Modern", color = "#606161"), #change color of slider

    tabItems(
      tabItem(tabName = "intro", introUI("intro1")),
      tabItem(tabName = "dataWrangling", dwUI("dataWrangling1")),
      tabItem(tabName = "singMetabolite", smUI("singMetabolite1")),
      tabItem(tabName = "ml", mlUI("machineLearning1")),
      tabItem(tabName = "mls", mlsUI("multimetsignature1")),
      tabItem(tabName = "score", scoreUI("score1")),
      tabItem(tabName = "report", repUI("report1") )
    )
  ),
  title = "Metabolomics Analysis"
  #theme = "cyborg"
)


server <- function(input, output, session) {
  data_preprocessed <- dwServer(id = "dataWrangling1")
  data_filtered <- smServer(id = "singMetabolite1", df=data_preprocessed$preprocessed_data, name = data_preprocessed$filename, 
                            startmet = data_preprocessed$startmet)
  ml_output <- mlServer(id = 'machineLearning1', df_train=data_filtered$traindatafiltered, df_test=data_filtered$testdatafiltered, 
                        name = data_filtered$filename )
  mls_output <- mlsServer(id = 'multimetsignature1', df_train=data_filtered$traindatafiltered, df_test=data_filtered$testdatafiltered, 
                          name = data_filtered$filename,
                          residual_met = data_filtered$residualmet) 
  score_output <- scoreServer(id = 'score1', df=data_preprocessed$preprocessed_data, name = data_preprocessed$filename, 
                              coeff = mls_output$coefficients)
  repServer(id = "report1", rawdata = data_preprocessed$raw_data, prepdata = data_preprocessed$preprocessed_data,
            idcol=data_preprocessed$idcol, target=data_preprocessed$targetcol, namecol = data_preprocessed$namecol, 
            startmet=data_preprocessed$startmet, nacol=data_preprocessed$nacolumns, narow=data_preprocessed$narows, 
            imputmethod=data_preprocessed$imput, metdata = data_filtered$singlemetabolites, 
            results_training = ml_output$model_trainig, results_testing = ml_output$model_testing,
            nfolds = mls_output$nfolds, nrepeats = mls_output$nrepeats, ltune = mls_output$ltune,
            model_cv = mls_output$model_cv, coeff = mls_output$coefficients, results_cv = mls_output$eval_cv,
            scores = score_output)
}


if (interactive()) {
  shinyApp(ui, server)
}
