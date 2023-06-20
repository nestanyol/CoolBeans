#' Title
#'
#' @param model
#' @param test_data
#'
#' @return model coefficients
#' @export
#'
test_model <- function(model, test_data) {

  model_fit <- model$fit

  # Make predictions on the test set
  predictions <- predict(model_fit, new_data = test_data) %>%
    as_tibble() %>%
    rename(predicted = .pred) %>%
    bind_cols(test_data)

  # Assess model performance
  rmse <- caret::RMSE(predictions$predicted, predictions$diet_score)
  # r_squared <- caret::R2(predictions$predicted, predictions$diet_score)

  # View the model coefficients
  model_coeffs <- broom::tidy(model_fit)

  # Print the evaluation metrics
  #cat("Root Mean Squared Error:", rmse, "\n")
  #cat("R-squared:", r_squared, "\n")

  return(list(model_coeffs = model_coeffs, rmse_val = rmse))

}
