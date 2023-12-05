library(dplyr)

test_that("Check type of output", {
  #evaluates the model and evaluates the value importance of the top variable
  data <- sim_data |>
    preprocessing(id = "id", target = "sex", start_metabolites = 5) |>
    select(id, target, starts_with("met")) |>
    drop_na(target) |>
    splitting()

  trained_model <- training_rf(data$train_data, type = 2)

  tested_model <- testing_rf_classification(trained_model, data$test_data)

  #actual <- pull(actual$model_importance["Importance"][1,1])

  # We expect an approximate rmse value.
  #expected <- 45

  #expect_equal(actual, expected, tolerance = 5)
  expect_type(tested_model, "list")
})
