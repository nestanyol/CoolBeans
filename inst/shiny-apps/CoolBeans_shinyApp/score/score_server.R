scoreServer <- function(id, df, name, residuals, coeff) {
#scoreServer <- function(id, df, name, startmet, coeff) {
  
  library(tidyr)
  library(tidyverse)
  library(skimr)
  library(CoolBeans)
  
  moduleServer(
    id,
    function(input, output, session) {
      
      # Reactive value to store the results
      #sin_met <- reactiveVal()
      #residual_met <- reactiveVal()
      ids <- reactiveVal()
      scores <- reactiveVal()

      
      observeEvent(input$calculate,{

          # observe({sin_met(sing_met_analysis(data = df(), 
          #                                             exposure_feature = input$target, 
          #                                             start_metabolites = startmet(), threshold = input$pvalue, 
          #                                             covariates = input$covariates, correction = input$correction_method))
          #  })
        
        # observe({residual_met(sin_met()$residuals)
        #   })
        
        observe({ids(df()$id)
        }) 
        
        observe({scores(score_calculation(ids(), residuals(), coeff()))})
        
        #scores_id <- cbind(scores(), id)
          
        output$output_score <- renderPrint({
          #head(residual_met())
          #print(coeff())
          head(scores())
        })
        
        output$plot1 <- renderPlot({
          
          den <- scores() %>%
            ggplot2::ggplot(aes(x = total)) +
            ggplot2::geom_density() +
            labs(x = "Total score", y = "density") +
            theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1))
          
          den
        }, res = 96)

        output$download <- downloadHandler(
          filename = function() {
            file <- name()[1]
            paste0(substr(file, 1, nchar(file)-4), "_scores.csv")
          },
          content = function(file) {
            vroom::vroom_write(scores(), file)
          }
        )
        
      })

      return(scores)
      
    })
  
}