#' test_model_kknn_class
#'
#' @description The function asses the performance of the trained model using
#' k-nearest neighbors with classification algorithm.
#'
#'
#' @param model to be use with the data
#' @param test_data data to test the model
#'
#' @return model coefficients
#' @export
#'
test_kknn_classification <- function(model, test_data) {
  
  model_fit <- model$fit
  
  # Make predictions on the test set
  predictions <- bind_cols(stats::predict(model_fit, new_data = test_data),
                           stats::predict(model_fit, new_data = test_data, type = "prob")) %>%
    dplyr::as_tibble() %>%
    dplyr::rename(predicted = .pred_class) %>%
    dplyr::bind_cols(test_data)
  
  # Assess model performance
  #rocau <- roc_auc(predictions, diet_score, predicted)
  #acc <- accuracy(predictions, target, predicted)
  #confmat <- conf_mat(predictions, truth = target, estimate = predicted)
  cm <- confusionMatrix(predictions$predicted, test_data$target)
  
  # View the variable importance
  #model_importance <- vip::vi(model_fit)
  
  #return(list(model_importance = model_importance, accuracy = acc, conf_mat = confmat))
  return(list(prediction = predictions, conf_mat = cm))
  
  
}
