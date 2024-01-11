#' Model testing from linear regression model
#'
#' @description The function asses the performance of the trained model using
#' linear regression.
#'
#' @param model to be use with the data
#' @param test_data data to test the model
#'
#' @return A list of one [tibble::tibble()] and one numeric value
#' @export
#'
testing_lr <- function(model, test_data) {
  # model_fit <- trained_model$fit
  model_fit <- model$fit

  # Make predictions on the test set
  predictions <- stats::predict(model_fit, new_data = test_data) %>%
    dplyr::as_tibble() %>%
    dplyr::rename(predicted = .pred) %>%
    dplyr::bind_cols(test_data)


  # View the model coefficients
  model_coeffs <- broom::tidy(model_fit)
  #save(model_coeffs, file = "LRmodel_coefficients.RData")

  # Assess model performance
  rmse <- caret::RMSE(predictions$predicted, predictions$target)
  # r_squared <- caret::R2(predictions$predicted, predictions$diet_score)


  # Print the evaluation metrics
  # cat("Root Mean Squared Error:", rmse, "\n")
  # cat("R-squared:", r_squared, "\n")

  list(prediction = predictions, model_coefficients = model_coeffs, rmse_value = rmse)
  #list(prediction = predictions, model_coefficients = model_coeffs)

}
