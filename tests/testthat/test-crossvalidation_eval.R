library(dplyr)

test_that("Evaluates output of list size", {
  data <- sim_data |>
    preprocessing(id = "id", target = "exposure", start_metabolites = 5) |>
    select(id, target, starts_with("met")) |>
    #drop_na('target') |>
    splitting()

  trained_model <- crossvalidation_model(data$train_data)

  actual <- crossvalidation_eval(trained_model, data$test_data) |>
    length()

  # We expect a list of size 2.
  expected <- 2

  expect_equal(actual, expected)
})
