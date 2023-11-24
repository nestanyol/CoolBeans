#' Analysis of single metabolites based on a target (and confounder/s)
#'
#' @description The function fits a linear model where the response variable are
#' each metabolite and the terms are the exposure feature and other features that
#' are defined as confounders.
#'
#' @param data data frame to use for analysis
#' @param exposure_feature variable of exposure name (e.g. diet score, group of exposure)
#' @param start_metabolites column number where metabolites start
#' @param confounders list of confounders
#' @param correction correction method to use
#'
#' @return A [tibble::tibble()].
#' @export
#'
sing_met_analysis <- function(data, exposure_feature, start_metabolites, confounders = NULL, threshold = 0.01, correction = NULL) {
  # Variables definition
  metabolite_columns <- colnames(data)[c(start_metabolites:ncol(data))]
  # defining the model
  features <- c(exposure_feature, confounders)

  output <- metabolite_columns %>%
    furrr::future_map(~ lm_singmet(.x, features, data = data)) %>%
    purrr::list_rbind(names_to = "model_id")

  #check if input is given or correction, regardless
  if (length(correction)) {
    output_filtered <- output %>%
      dplyr::mutate(p.value_corrected = stats::p.adjust(output$p.value, method = correction)) %>%
      dplyr::filter(stringr::str_detect(term, "target")) %>%
      dplyr::filter(p.value_corrected < threshold) %>%
      dplyr::arrange(p.value_corrected)
  } else {
    output_filtered <- output %>%
      dplyr::filter(stringr::str_detect(term, "target")) %>%
      dplyr::filter(p.value < threshold) %>%
      dplyr::arrange(p.value)
  }
  output_filtered
}

# Set up modelling function:
lm_singmet <- function(data, metabolite, y) {
  model_formula <- stats::reformulate(y, response = metabolite)
  results <- stats::lm(model_formula, data = data)
  broom::tidy(results) %>%
    dplyr::mutate(yvar = metabolite, .before = dplyr::everything())
}
