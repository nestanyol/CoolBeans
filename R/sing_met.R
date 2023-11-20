#' sing_met
#'
#' @param data dataframe to use for analysis
#' @param exposure_feature variable of exposure name (e.g. diet score, group of exposure)
#' @param start_met column number where metabolites start
#' @param confounders list of confounders
#' @param correction correction method to use
#'
#' @return
#' @export
#'
#' @examples
sing_met <- function(data, exposure_feature, start_met, confounders, threshold = 0.1, correction = NULL) {
  # Variables definition
  metabolite_columns <- colnames(data)[c(start_met:ncol(data))]
  # defining the model
  features <- c(exposure_feature, confounders)

  # Set up parallel processing
  plan(multisession)

  output <- metabolite_columns %>%
    furrr::future_map(~ lm_singmet(.x, features, data = data)) %>%
    purrr::list_rbind(names_to = "model_id")

  # Reset the plan to sequential
  plan(sequential)

  if (length(correction)) {
    p.value_corrected <- stats::p.adjust(output$p.value, method = correction)
    output <- cbind(output, p.value_corrected)

    output_filtered <- output[stringr::str_detect(output$term, "target"), ] %>%
      dplyr::filter(p.value_corrected < threshold) %>%
      dplyr::arrange(p.value_corrected)
    return(output_filtered)
  } else {
    output_filtered <- output[stringr::str_detect(output$term, "target"), ] %>%
      dplyr::filter(p.value < threshold) %>%
      dplyr::arrange(p.value)
    return(output_filtered)
  }
}

# Set up modelling function:
lm_singmet <- function(data, metabolite, y) {
  model_formula <- stats::reformulate(features, response = metabolite)
  results <- stats::lm(model_formula, data = data)
  broom::tidy(results) %>%
    dplyr::mutate(yvar = metabolite, .before = dplyr::everything())
}
