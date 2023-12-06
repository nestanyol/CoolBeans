# Module server function
#repServer <- function(id, rawdata, prepdata, metdata, results){
repServer <- function(id, rawdata, prepdata, idcol, target, startmet, nacol, narow, imputmethod, 
                      metdata){
    

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
                            inputid = idcol(),
                            inputtarget = target(),
                            inputstartmet = startmet(),
                            inputnacol = nacol(),
                            inputnarow = narow(),
                            inputimputmethod = imputmethod(),
                            filemetabolites = metdata()#,
                            #modelresults = results()
                          ),
                          envir = new.env())#,
                          #intermediates_dir = tempdir())
                          
      }
        )}
    )}


