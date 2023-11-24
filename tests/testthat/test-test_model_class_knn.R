library(dplyr)

test_that("Check the expected value of the model importance from top variable in a random forest model using classification", {
  #evaluates the model and evaluates the value importance of the top variable
  data <- sim_data |>
    preprocessing(id = "id", target = "sex", start_metabolites = 5) |>
    select(id, target, starts_with("met")) |>
    drop_na(target) |>
    splitting()

  trained_model <- training_knn(data$train_data, type = 2)

  tested_model <- testing_knn_classification(trained_model, data$test_data)

  actual <- tested_model$conf_mat$overall[['Accuracy']]

  # We expect an approximate rmse value.
  expected <- 0.5

  expect_equal(actual, expected, tolerance = 0.05)
})
