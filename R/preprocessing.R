#' Standard preprocessing
#'
#' @param raw_data dataframe with metabolomic data
#'
#' @return dataframe
#' @export
#'
preprocess_data <- function(raw_data) {
  # Remove noise data
  preprocessed_data_prep <- raw_data %>%
    # Remove duplicate rows
    dplyr::distinct() %>%
    # Remove columns (metabolites) that have 20% of values as missing
    janitor::remove_empty("cols", cutoff = 0.2) %>%
    # Remove rows (subjects) that have 20% of values as missing
    janitor::remove_empty("rows", cutoff = 0.2)

  # Create a recipe for preprocessing the data
  preprocess_recipe <- recipes::recipe(~., data = preprocessed_data_prep) %>%
    recipes::update_role(id, new_role = "id") %>%
    recipes::update_role(diet_score, new_role = "outcome") %>%
    # Imputation
    recipes::step_impute_mean(recipes::all_predictors()) %>%
    # log normalization
    recipes::step_log(recipes::all_predictors())

  # Apply the recipe to preprocess data
  preprocessed_data <- preprocess_recipe %>%
    recipes::prep(data = preprocessed_data_prep) %>%
    recipes::juice()

  return(preprocessed_data)
}
