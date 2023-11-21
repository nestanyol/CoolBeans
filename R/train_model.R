#' train_model_lr
#'
#' @description The function trains a model using linear regression (elastic net)
#'
#'
#' @param train_data data to be used for training
#'
#' @return model parameters
#' @export
#'
train_model <- function(train_data) {
  # Create a recipe
  recipe <- recipes::recipe(target ~ ., data = train_data) %>%
    recipes::step_rm(id)

  # Create the model specification
  model_spec <- parsnip::linear_reg(penalty = 0.5, mixture = 0.5) %>%
    parsnip::set_engine("glmnet")

  # Train the model
  model <- workflows::workflow() %>%
    workflows::add_recipe(recipe) %>%
    workflows::add_model(model_spec) %>%
    parsnip::fit(data = train_data)

  return(model$fit)
}
