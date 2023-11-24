library(dplyr)

test_that("Check the expected rmse value from linear regression model", {
  #evaluates the model and evaluates the rmse value
  data <- sim_data |>
    preprocessing(id = "id", target = "exposure", start_metabolites = 4) |>
    select(id, target, starts_with("met")) |>
    splitting()

  trained_model <- training_lr(data$train_data)

  actual <- testing_lr(trained_model, data$test_data)

  # We expect an approximate rmse value.
  expected <- 1.5

  expect_equal(actual$rmse_value, expected, tolerance = 0.5)
})
