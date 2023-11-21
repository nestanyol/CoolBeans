#' Analysis of single metabolites based on a target (and confounder/s)
#'
#' @param data dataframe to use for analysis
#' @param exposure_feature variable of exposure name (e.g. diet score, group of exposure)
#' @param start_met column number where metabolites start
#' @param confounders list of confounders
#' @param correction correction method to use
#'
#' @return dataframe with the model results (and p-value corrected)
#' @export
#'
#' @examples


sing_met_analysis <- function(data, exposure_feature = "target", start_metabolites, confounders=NULL, threshold=0.1, correction=NULL) {

  # Variables definition
  metabolite_columns <- colnames(data)[c(start_metabolites:ncol(data))]
  #defining the model
  features <- c(exposure_feature, confounders)

  # Set up modelling function:
  lm_singmet <- function(data, metabolite, y) { #y takes the features input
    model_formula <- reformulate(y, response = metabolite)
    results <- lm(model_formula, data = data)
    broom::tidy(results) %>%
      mutate(yvar = metabolite,.before = everything())
  }

  # Set up parallel processing
  future::plan(multisession)

  output <- metabolite_columns %>%
    furrr::future_map(~lm_singmet(.x, y=features, data=data)) %>%
    list_rbind(names_to = "model_id")

  # Reset the plan to sequential
  future::plan(sequential)

  #check if input is given or correction, regardless
  if (length(correction)) {
    #correct p-value using user input for option
    p.value_corrected <- p.adjust(output$p.value, method = correction)
    output <- cbind(output, p.value_corrected)
  #filter based on threshold and arrange with higher on top
    output_filtered <- output[str_detect(output$term, "target"),] %>%
      filter(p.value_corrected < threshold) %>%
      arrange(p.value_corrected)
    #return(output_filtered)
  } else {
    #filter based on threshold and arrange with higher on top
    output_filtered <- output[str_detect(output$term, "target"),] %>%
      filter(p.value < threshold) %>%
      arrange(p.value)
    #return(output_filtered)
    }
}
