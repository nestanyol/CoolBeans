#' Standard preprocessing
#'
#' @param raw_data dataframe with metabolomic data
#' @param id id of each row
#' @param target target to focus in the model
#' @param cutoff_met threshold to use to discard columns given n NaN
#' @param cutoff_subj threshold to use to discard rows given n NaN
#'
#' @return dataframe
#' @export
#'
#preprocess_data <- function(raw_data, cutoff_met = 0, cutoff_subj = 0) {
#modified definition to have the cutoffs intended in the comments
preprocess_data <- function(raw_data, id, target, cutoff_met = 0.2, cutoff_subj = 0.2) {
  # Remove noise data
  preprocessed_data_prep <- raw_data %>%
    # Remove duplicate rows
    dplyr::distinct() %>%
    # Remove columns (metabolites) that have 20% of values as missing
    janitor::remove_empty("cols", cutoff = cutoff_met) %>% # Optional step
    # Remove rows (subjects) that have 20% of values as missing
    janitor::remove_empty("rows", cutoff = cutoff_subj) # Optional step

  #some data sets migth have NaN in the target column, can we do something about it during this step?

  # Create a recipe for preprocessing the data
  preprocess_recipe <- recipes::recipe(~., data = preprocessed_data_prep) %>%
    #are we always expecting the same column names?
    recipes::update_role(id, new_role = "id") %>%
    recipes::update_role(target, new_role = "outcome") %>%
    # Imputation
    recipes::step_impute_median(recipes::all_predictors()) %>% #step_imputelower() min?
    # log transformation
    recipes::step_log(recipes::all_predictors()) %>%
    # normalization
    recipes::step_normalize(recipes::all_predictors())

  # Apply the recipe to preprocess data
  preprocessed_data <- preprocess_recipe %>%
    recipes::prep(data = preprocessed_data_prep) %>%
    recipes::juice()

  preprocessed_data <- preprocessed_data%>%rename_at(id, ~'id')
  preprocessed_data <- preprocessed_data%>%rename_at(target, ~'target')

  return(preprocessed_data)
}
