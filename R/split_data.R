#' Split dataset
#'
#' @param preprocessed_data
#' @param training_split
#'
#' @return split dataframes
#' @export
#'
split_data <- function(preprocessed_data, training_split = 0.7) {
  # Perform a random split using the `rsample` package
  split <- rsample::initial_split(
    preprocessed_data,
    prop = training_split,
    strata = "diet_score",
    jitter = 0.05
  )

  train_data <- rsample::training(split)
  test_data <- rsample::testing(split)

  return(list(test_data = test_data, train_data = train_data))
}
