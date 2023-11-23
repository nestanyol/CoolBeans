#' Splits the prepossessed data set into train and test
#'
#' @description The function splits the data for model training, two datasets
#' are generated for training and testing. It is possible to define the percentage
#' of data wanted for training, default = 0.7.
#'
#' @param preprocessed_data data after pre-processing
#' @param target column
#' @param training_split data fraction to use for training
#'
#' @return A list of two [tibble::tibble()] for training and testing dataset.
#' @export
#'
splitting <- function(preprocessed_data, target, training_split = 0.7) {
  # Perform a random split using the `rsample` package
  split <- rsample::initial_split(
    preprocessed_data,
    prop = training_split,
    strata = target
  )

  train_data <- rsample::training(split)
  test_data <- rsample::testing(split)

  list(test_data = test_data, train_data = train_data)
}
