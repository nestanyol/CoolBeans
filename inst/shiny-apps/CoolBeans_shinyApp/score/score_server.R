scoreServer <- function(id, df, name, startmet, coeff) {
  
  library(tidyr)
  library(tidyverse)
  library(skimr)
  library(CoolBeans)
  
  moduleServer(
    id,
    function(input, output, session) {
      
      # Reactive value to store the results
      sin_met <- reactiveVal()
      residual_met <- reactiveVal()
      ids <- reactiveVal()
      scores <- reactiveVal()

      
      observeEvent(input$calculate,{

          observe({sin_met(sing_met_analysis(data = df(), 
                                                      exposure_feature = input$target, 
                                                      start_metabolites = startmet(), threshold = input$pvalue, 
                                                      covariates = input$covariates, correction = input$correction_method))
            })
        
        observe({residual_met(sin_met()$residuals)
          })
        
        observe({ids(df()$id)
        }) 
        
        observe({scores(score_calculation(ids(), residual_met(), coeff()))})
        
        #scores_id <- cbind(scores(), id)
          
        output$output_score <- renderPrint({
          #head(residual_met())
          #print(coeff())
          head(scores())
        })
          
        
      })
      
      
      output$download <- downloadHandler(
        filename = function() {
          file <- name()[1]
          paste0(substr(file, 1, nchar(file)-4), "_model.rds")
        },
        content = function(file) {
          #vroom::vroom_write(model(), file)
          saveRDS(model(), file)
        }
      )
      
      # return(list(nfolds = n_folds, nrepeats = n_repeats, ltune = l_tune,
      #             model_cv = model, coefficients = coeff, eval_cv = results))
      
    })
  
}