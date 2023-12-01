repUI <- function(id, label = 'report1') {
    ns <- NS(id)
    
    tabPanel("Report ",
             mainPanel(
               width=12,title="Reporting information", solidHeader = TRUE, status = "primary",collapsible = F,
               # # Set title of report
               fluidRow(
                 column(4,  HTML('Report title')),
                 column(8,textInput(ns("title"), placeholder='Report title',label=NULL))
               ),
               fluidRow(
                 column(4,  HTML('author')),
                 column(8,textInput(ns("author"), placeholder='Modeler name',label=NULL))
               ),
               # Start report rendering
               fluidRow(
                 hr(),
                 # column(6,radioButtons(ns('format'), 'Document format', c('PDF', 'HTML', 'Word'),
                 #                       inline = TRUE)),
                 column(6,  downloadButton(ns("report"), "Generate report",width='100%'))
                 
                 
               )
               
               
               
             )
             
    )
  }
  