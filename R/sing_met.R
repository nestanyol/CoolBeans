#' sing_met
#'
#' @param data dataframe to use for analysis
#' @param exposure_feature variable of exposure name (e.g. diet score, group of exposure)
#' @param start_met column number where metabolites start
#' @param confounders list of confounders
#'
#' @return
#' @export
#'
#' @examples


sing_met <- function(data, exposure_feature, start_met, confounders) {
  library(future)

  # Variables definition
  metabolite_columns <- colnames(data)[c(start_met:ncol(data))]

  # Merge data_main and data_confounders based on the ID column
  #merged_data <- merge(data, metadata, by = id)

  # Set up modelling function:
  lm_singmet <- function(data, exposure_feature, metabolite, confounders) {
    model_formula <- reformulate(c(exposure_feature, confounders), response = metabolite)
    results <- lm(model_formula, data = data)
    broom::tidy(results) %>%
      mutate(yvar = metabolite,.before = everything())
  }

  # Set up parallel processing
  plan(multisession)

  output <- metabolite_columns %>%
    furrr::future_map(~lm_singmet(.x, exposure_feature, confounders, data=data)) %>%
    list_rbind(names_to = "model_id")

  # Reset the plan to sequential
  plan(sequential)

  return(output)

}
