# Module server function
#repServer <- function(id, rawdata, prepdata, metdata, results){
repServer <- function(id, rawdata, prepdata, metdata){
    

  library(rmarkdown)
  
  moduleServer(
    id, 
    function(input, output, session){
    output$report <- downloadHandler(
      filename = function() {
        paste0("report", Sys.Date(), ".html")
      },
      content = function(file) {
        rmarkdown::render("reports/report.Rmd",
                          output_file = file, 
                          params = list(
                            title = input$title,
                            author = input$author,
                            fileraw = rawdata(),
                            fileprep = prepdata(),
                            filemetabolites = metdata()#,
                            #modelresults = results()
                          ),
                          envir = new.env())#,
                          #intermediates_dir = tempdir())
                          
      }
        )}
    )}


