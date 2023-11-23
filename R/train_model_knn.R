#' Model training using k-nearest neighbor
#'
#'
#' @description The function trains a model using k-nearest neighbor. It is possible
#' to use two different algorithms: regression or classification. The number of clusters
#' will be defined based on the unique elements on the target column up to n = 10.
#'
#' @param train_data data to be used for training
#' @param type two options 1 for regression using linear regression and 2 for classification using random forest.
#'
#' @return A list of two elements a list and an stage class
#' @export
#'
train_model_knn <- function(train_data, type = 1) {
  # number of cluster k/neighbour
  n <- length(unique(train_data$target))
  if (n > 10) {
    n <- 10
  }

  # Create a recipe
  recipe <- recipes::recipe(target ~ ., data = train_data) %>%
    recipes::step_rm(id)

  # Create the model specification
  if (type == 1) {
    model_spec <- parsnip::nearest_neighbor(neighbors = n, weight_func = "triangular") %>%
      parsnip::set_engine("kknn") %>% # needs to be install
      parsnip::set_mode("regression")
  } else if (type == 2) {
    model_spec <- parsnip::nearest_neighbor(neighbors = n, weight_func = "triangular") %>%
      parsnip::set_engine("kknn") %>%
      parsnip::set_mode("classification")
  }

  # Train the model
  model <- workflows::workflow() %>%
    workflows::add_recipe(recipe) %>%
    workflows::add_model(model_spec) %>%
    parsnip::fit(data = train_data)

  return(model$fit)
}
