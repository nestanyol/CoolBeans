#' Analysis of single metabolites based on a target exposure feature (and covariate/s).
#'
#' @description The function concurrently fits a linear model with each metabolite feature in the data set, with the exposure feature
#' as the primary predictor and other features that are defined as covariates.
#'
#'
#' @param data data frame to use for analysis. It contains subject id, an exposure feature variable and metabolite variables; can also contain covariates.
#' @param exposure_feature variable of exposure name (e.g. diet score, group of exposure).
#' @param start_metabolites column index where metabolite features start.
#' @param covariates list of optional covariates.
#' @param threshold Threshold for significance in the results filtering.
#' @param correction Multiple testinc optional correction.
#'
#' @return A [tibble::tibble()].
#' @export
#'
sing_met_analysis <- function(data, exposure_feature, start_metabolites, covariates = NULL, threshold = 0.1, correction = NULL) {

  # Variables definition:
  metabolite_columns <- colnames(data)[c(start_metabolites:ncol(data))]
  features <- c(exposure_feature, covariates)

  # Model fitting to each metabolite column in parallel:
  output <- metabolite_columns %>%
    furrr::future_map(~ lm_singmet(.x, features, data = data)) %>%
    purrr::list_rbind(names_to = "model_id") # Combine results in a single dataframe

  # Results filtering:
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

# Modelling function definition:
lm_singmet <- function(data, metabolite, y) {
  model_formula <- stats::reformulate(y, response = metabolite)
  results <- stats::lm(model_formula, data = data)
  broom::tidy(results) %>%
    dplyr::mutate(yvar = metabolite, .before = dplyr::everything())
}
