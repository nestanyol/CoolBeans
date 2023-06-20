#' Title
#'
#' @param train_data
#'
#' @return model parameters
#' @export
#'
train_model <- function(train_data) {
  # Create a recipe
  recipe <- recipe(diet_score ~ ., data = train_data) %>%
    step_rm(id)

  # Create the model specification
  model_spec <- linear_reg(penalty = 1, mixture = 0.5) %>%
    set_engine("glmnet")

  # Train the model
  model <- workflow() %>%
    add_recipe(recipe) %>%
    add_model(model_spec) %>%
    fit(data = train_data)

  return(model$fit)
}
