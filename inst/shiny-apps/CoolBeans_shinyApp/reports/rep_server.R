# Module server function
#repServer <- function(id, rawdata, prepdata, metdata, results){
repServer <- function(id, rawdata, prepdata, idcol, target, startmet, 
                      namecol, nacol, narow, imputmethod, 
                      metdata, results_training, results_testing,
                      nfolds, nrepeats, ltune, model_cv, coeff, 
                      results_cv, scores){
    

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
                            colplot = namecol(),
                            inputstartmet = startmet(),
                            inputnacol = nacol(),
                            inputnarow = narow(),
                            inputimputmethod = imputmethod(),
                            filemetabolites = metdata(),
                            modeltraining = results_training(),
                            modeltesting = results_testing(),
                            model_nfolds = nfolds(),
                            model_nrepeats = nrepeats(),
                            model_ltune = ltune(),
                            modelcv = model_cv(),
                            coeffs = coeff(),
                            resultscv = results_cv(),
                            scoresall = scores()
                          ),
                          envir = new.env())#,
                          #intermediates_dir = tempdir())
                          
      }
        )}
    )}


