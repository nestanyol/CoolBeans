#' Calculation of scores using residuals from lm and coefficients from the CV model
#'
#' @description The function calculates a score based on residuals and coefficients
#'
#' @param ids calculated from lm
#' @param residuals calculated from lm
#' @param coefficients calculated from ml with crossvalidation
#'
#' @return data frame with scores per sample
#' @export
#'
score_calculation <- function(ids, residuals, coefficients) {
  
  coefficients_df <- data.frame(metabolite = rownames(coefficients),
                                coefficient = as.numeric(coefficients))
  #remove non-metabolites from the df
  coefficients_filtered <- coefficients_df %>%
    filter(metabolite != "(Intercept)") %>%
    filter(metabolite != "id") %>%
    filter(coefficient != 0)
  
  features <- coefficients_filtered$metabolite
  
  score <- data.frame(matrix(NA,
                             nrow=nrow(residuals),
                             ncol=length(features)))


  for(i in c(1:length(features))){
    n <- which(coefficients_filtered$metabolite == features[i])
    val <- residuals %>%
      select(features[i]) %>%
      mutate(.*coefficients_filtered[n,2])

    score[,i] <- val
  }

  colnames(score) <- features
  
  scores <- cbind(ids, score, total = rowSums(score))
  
  scores
  
}
