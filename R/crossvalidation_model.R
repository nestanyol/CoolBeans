#' Model training using cross-validation and the model of choice
#'
#' @description The function trains a model using linear regression (elastic net)
#' and cross-validation
#'
#'
#' @param train_data data to be used for training
#' @param model model to be used, default is elastic model
#' @param nfolds number of folds to be used
#' @param nrepeats number of repeats to be used
#' @param ltune tuneLength parameter during training
#'
#' @return Output from training model
#' @export
#'
crossvalidation_model <- function(train_data, model = "glmnet", nfolds=2, nrepeats=2, ltune=5) {
  #split data
  #train_data = data$train_data
  #test_data = data$test_data

  control <- caret::trainControl(method = "repeatedcv",
                          number = nfolds, #number of folds
                          repeats = nrepeats, #number of repeats
                          search = "random",
                          verboseIter = TRUE)

  # Training Elastic Net Regression model
  set.seed(42) #set.seed to make it reproducible (otherwise different result each time)
  selected_model <- caret::train(target~ .,
                         data = train_data,
                         method = model, #method to use
                         preProcess = c("center", "scale"),
                         tuneLength = ltune, # grid granularity
                         trControl = control)

  #coeff <- coef(selected_model$finalModel, selected_model$bestTune$lambda)

  #list(model = selected_model, coefficients =coeff)
}
