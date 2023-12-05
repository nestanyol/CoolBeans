repUI <- function(id, label = 'report1') {
    ns <- NS(id)
    
    tabPanel("Report ",
             mainPanel(
               width=12,title="Reporting information", solidHeader = TRUE, status = "primary",collapsible = F,
               # # Set title of report
               fluidRow(
                 column(4,  HTML('Report title')),
                 column(8,textInput(ns("title"), placeholder='Report',label=NULL))
               ),
               fluidRow(
                 column(4,  HTML('author')),
                 column(8,textInput(ns("author"), placeholder='Name',label=NULL))
               ),
               
               # Some inputs
               fluidRow(
                 hr(),
                 numericInput(ns("ncols"), "Type column where metabolites start",5),
                 textInput(ns("key_plot"), "Key to use for plotting", "metabolite")),
               
               
                 
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
  