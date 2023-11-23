#' Model testing from k-nearest neighbor model using regression algorithm
#'
#' @description The function asses the performance of the trained model using
#' random forest with regression algorithm.
#'
#'
#' @param model to be use with the data
#' @param test_data data to test the model
#'
#' @returnA list of one [tibble::tibble()] and two numeric values
#' @export
#'
testing_kknn_regression <- function(model, test_data) {
  # model_fit <- trained_model$fit
  model_fit <- model$fit

  # Make predictions on the test set
  predictions <- stats::predict(model_fit, new_data = test_data) %>%
    dplyr::as_tibble() %>%
    dplyr::rename(predicted = .pred) %>%
    dplyr::bind_cols(test_data)

  # Assess model performance
  rmse <- caret::RMSE(predictions$predicted, predictions$target)
  r2 <- caret::R2(predictions$predicted, predictions$target)

  # View the variable importance
  # model_importance <- vip::vi(model_fit)

  # return(list(model_importance = model_importance, r_squared = r2, RMSE = rmse))
  list(prediction = predictions, r_squared = r2, RMSE = rmse)
}
