#' Title
#'
#' @param model to be use with the data
#' @param test_data data to test the model
#'
#' @return model coefficients
#' @export
#'
test_model <- function(model, test_data) {

  model_fit <- model$fit

  # Make predictions on the test set
  predictions <- stats::predict(model_fit, new_data = test_data) %>%
    dplyr::as_tibble() %>%
    dplyr::rename(predicted = .pred) %>%
    dplyr::bind_cols(test_data)

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
