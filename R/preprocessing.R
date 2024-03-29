#' Standard data wrangling (pre-processing)
#'
#' @description This function performs a simple workflow of data wrangling.
#' Overall it will remove noise data, duplicate rows and columns/rows with NAs.
#' Next the data will be transformed using imputation, log transformation and
#' normalization. The function will also assign new column names to ID column
#' and target column which will be used later for the other functions.
#'
#' @param raw_data dataframe with metabolomic data
#' @param id id of each row
#' @param target target to focus in the model
#' @param start_metabolites column number where metabolites start
#' @param cutoff_columns threshold to use to discard columns given n NaN
#' @param cutoff_rows threshold to use to discard rows given n NaN
#' @param imputation method of choice, four available: median, +mean, lower, knn
#'
#' @return A [tibble::tibble()]. It changes name columns for column ID
#' and column target.
#' @export
#'
preprocessing <- function(raw_data, id, target, start_metabolites, cutoff_columns = 0.2, cutoff_rows = 0.2, imputation = "median") {

  # Remove noise data
  clean_data <- raw_data %>%
    # Remove rows with NaN in target column
    tidyr::drop_na(target) %>%
    # Remove duplicate rows
    dplyr::distinct() %>%
    # Remove columns (metabolites) that have 20% of values as missing
    janitor::remove_empty("cols", cutoff = cutoff_columns) %>% # Optional step
    # Remove rows (subjects) that have 20% of values as missing
    janitor::remove_empty("rows", cutoff = cutoff_rows) # Optional step

  # split data
  variables <- clean_data[, c(1:start_metabolites - 1)]
  metabolites <- clean_data[, c(start_metabolites:ncol(clean_data))]
  # get section to use for preprocessing
  col_selection <- variables %>% dplyr::select(id, target)
  preprocessed_data_prep <- cbind(col_selection, metabolites)
  # extra variables
  extra <- variables %>%
    dplyr::select(!id) %>%
    dplyr::select(!target)


  # Create a recipe for preprocessing the data
  preprocess_recipe <- recipes::recipe(~., data = preprocessed_data_prep) %>%
    # are we always expecting the same column names? #changed to be more general
    recipes::update_role(id, new_role = "id") %>%
    recipes::update_role(target, new_role = "outcome")


  ### Imputation ###
  # Give more options for imputation: median, +mean, lower, knn
  if (imputation == "knn") {
    preprocess_recipe <- preprocess_recipe %>%
      recipes::step_impute_knn(recipes::all_predictors())
  } else if (imputation == "lower") {
    preprocess_recipe <- preprocess_recipe %>%
      recipes::step_impute_lower(recipes::all_predictors())
  } else if (imputation == "mean") {
    preprocess_recipe <- preprocess_recipe %>%
      recipes::step_impute_mean(recipes::all_predictors())
  } else {
    preprocess_recipe <- preprocess_recipe %>%
      recipes::step_impute_median(recipes::all_predictors())
  }

  # continue with transformation and scaling
  # log transformation
  preprocess_recipe <- preprocess_recipe %>%
    recipes::step_log(recipes::all_predictors()) %>%
    # normalization
    recipes::step_normalize(recipes::all_predictors())

  # Apply the recipe to preprocess data
  preprocessed_data <- preprocess_recipe %>%
    recipes::prep(data = preprocessed_data_prep) %>%
    recipes::juice()

  # allows the downstream funtions to run with this initial decision
  preprocessed_data <- preprocessed_data %>% rename_at(id, ~"id")
  preprocessed_data <- preprocessed_data %>% rename_at(target, ~"target")

  # join all data
  preprocessed_data_all <- cbind(extra, preprocessed_data)

  # return(preprocessed_data)
}
