#' train_model_rf
#'
#' @description The function trains a model using random forest. It is possible
#' to use two different algorithms: regression or classification.
#'
#' @param train_data data to be used for training
#' @param type two options 1 for regression using linear regression and 2 for classification using random forest.
#'
#' @return model parameters
#' @export
#'
train_model_rf <- function(train_data, type = 1) {
  # Create a recipe
  recipe <- recipes::recipe(target ~ ., data = train_data) %>%
    recipes::step_rm(id)

  # Create the model specification
  if (type == 1) {
    model_spec <- parsnip::rand_forest() %>%
      parsnip::set_engine("ranger", importance = "impurity") %>%
      parsnip::set_mode("regression")
    # model_spec <- parsnip::linear_reg(penalty = 0.5, mixture = 0.5) %>%
    #  parsnip::set_engine("glmnet")
  } else if (type == 2) {
    model_spec <- parsnip::rand_forest() %>%
      parsnip::set_engine("ranger", importance = "impurity") %>%
      parsnip::set_mode("classification")
  }

  # Train the model
  model <- workflows::workflow() %>%
    workflows::add_recipe(recipe) %>%
    workflows::add_model(model_spec) %>%
    parsnip::fit(data = train_data)

  return(model$fit)
  # return(model)
}
