# Module server function
repServer <- function(id, rawname, rawdata){

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
                            nameraw = rawname(),
                            fileraw = rawdata()
                          ),
                          envir = new.env())#,
                          #intermediates_dir = tempdir())
                          
      }
        )}
    )}
    #   filename = "report.html",
    #   # filename = function() {
    #   #   paste('My_report', Sys.Date(), sep = '.', switch(
    #   #     input$format, PDF = 'pdf', HTML = 'html', Word = 'docx'
    #   #   ))
    #   # },
    #   
    #   content = function(file) {
    #     #src <- normalizePath('report.Rmd')
    #     
    #     # withProgress(message = 'Report generating in progress',
    #     #              detail = 'This may take a while...', value = 0, {
    #     #                for (i in 1:10) {
    #     #                  incProgress(1/10)
    #     #                  Sys.sleep(0.40)
    #     #                }
    #     #                
    #     #              })
    #     # 
    #     # # Set up parameters to pass to Rmd document
    #     # params_for_rmd =  list(preprocessing = preprocess(),
    #     #                        #plot_2=graph(),
    #     #                        set_title=input$title,
    #     #                        set_author=input$author)
    #     # 
    #     # 
    #     # 
    #     # 
    #     # owd <- setwd(tempdir())
    #     # on.exit(setwd(owd))
    #     # file.copy(src, 'report.Rmd', overwrite = TRUE)
    #     # 
    #     # 
    #     # 
    #     # #library(rmarkdown)
    #     # out <- render('report.Rmd', switch(
    #     #   input$format,
    #     #   PDF = pdf_document(), HTML = html_document(), Word = word_document()
    #     # ))
    #     # file.rename(out, file)
    #     tempReport <- file.path(getwd(), "report.Rmd")
    #     file.copy("report.Rmd", tempReport, overwrite = TRUE)
    #     
    #     # Set up parameters to pass to Rmd document
    #     params <- list(preprocessing = preprocess(),
    #                    #                        #plot_2=graph(),
    #                    set_title=input$title,
    #                    set_author=input$author)
    #     
    #     # Knit the document, passing in the `params` list, and eval it in a
    #     # child of the global environment (this isolates the code in the document
    #     # from the code in this app).
    #     rmarkdown::render(tempReport, output_file = file,
    #                       params = params,
    #                       envir = new.env(parent = globalenv())
    #     )
    #   }
    # )

