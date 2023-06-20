#' Title
#'
#' @param train_data
#'
#' @return model parameters
#' @export
#'
train_model <- function(train_data) {
  # Create a recipe
  recipe <- recipes::recipe(diet_score ~ ., data = train_data) %>%
    recipes::step_rm(id)

  # Create the model specification
  model_spec <- parsnip::linear_reg(penalty = 1, mixture = 0.5) %>%
    parsnip::set_engine("glmnet")

  # Train the model
  model <- workflows::workflow() %>%
    workflows::add_recipe(recipe) %>%
    workflows::add_model(model_spec) %>%
    workflows::fit(data = train_data)

  return(model$fit)
}
