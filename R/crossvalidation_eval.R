#' Model training using cross-validation and the model of choice
#'
#' @description The function trains a model using linear regression (elastic net)
#' and cross-validation
#'
#' @param model trained model to be used
#' @param test_data data to be used for testing
#' @param type proble type either regression or classification
#'
#' @return Output from training model
#' @export
#'
crossvalidation_eval <- function(model, test_data, type = "regression") {
  predictions <- predict(model, newdata = test_data)%>%
    dplyr::as_tibble() %>%
    dplyr::rename(predicted = value) %>%
    dplyr::bind_cols(test_data)

  if(type == "regression"){
    rmse <- caret::RMSE(predictions$predicted, predictions$target)
    list(df = predictions, RMSE = rmse)
  } else if (type == "classification") {
    cm <- caret::confusionMatrix(predictions$predicted, test_data$target)
    list(df = predictions, confussion_matrix = cm)
  }


}
