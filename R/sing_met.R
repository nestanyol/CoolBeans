
#' Title
#'
#' @param variables
#'
#' @return
#' @export
#'
#' @examples


# user defines confounder list in the script.
# data: contains id and metabolite columns
# metadata: contains id, diet score, confounders

sing_met <- function(data, metadata, confounders) {

  # Variables definition
  metabolite_cols <- setdiff(colnames(data), id)

  # Merge data_main and data_confounders based on the ID column
  merged_data <- merge(data, metadata, by = id)

  # Set up modelling function:
  lm_singmet <- function(data, metabolite, confounders) {
    model_formula <- reformulate(c(confounders, response = metabolite)
    results<- lm(model_formula, data = data)
    broom::tidy(results) %>%
      mutate(yvar=metabolite,.before=everything())
  }

  # Set up parallel processing
  plan(multisession)

  metabolite_columns %>%
    future_map(~lm_singmet(.x, confounders, data=data)) %>%
    list_rbind(names_to = "model_id")

  # Reset the plan to sequential
  plan(sequential)

  return()

}
