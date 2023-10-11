#' Split dataset
#'
#' @description The function splits the data for model training, two datasets
#' are generated for training and testing. It is possible to define the percetage
#' of data wanted for training, default = 0.7.
#'
#'
#' @param preprocessed_data data after pre-processing
#' @param training_split data fraction to use for training
#'
#' @return split dataframes
#' @export
#'
split_data <- function(preprocessed_data, target, training_split = 0.7) {
  # Perform a random split using the `rsample` package
  split <- rsample::initial_split(
    preprocessed_data,
    prop = training_split,
    strata = target#,
    #jitter = 0.05 parameter doesn't exist
  )

  train_data <- rsample::training(split)
  test_data <- rsample::testing(split)

  return(list(test_data = test_data, train_data = train_data))
}

