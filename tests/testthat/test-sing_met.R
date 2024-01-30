library(dplyr)

test_that("Evaluates and checks number of columns", {
  "test if expected number of columns are retrieved"
  data <- sim_data |>
    preprocessing(id = "id", target = "exposure", start_metabolites = 5)
  split <- data |>
    splitting(target = "target")

  train_data <- split$train_data
  actual <- sing_met_analysis(data=data, train_data = train_data, exposure_feature = "target", start_metabolites = 5,covariates = c("age")) |>
    length()

  # We expect mean pvalue to have a specific value.
  expected <- 2

  expect_equal(actual, expected)
})
