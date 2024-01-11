test_that("checks outout of score calculation", {
  data <- sim_data |>
    preprocessing(id = "id", target = "exposure", start_metabolites = 5) |>
    select(id, target, starts_with("met")) |>
    #drop_na('target') |>
    splitting()

  train_data <- data$train_data
  model <- crossvalidation_model(train_data)

  #evals <- crossvalidation_eval(model, data$test_data)
  coefficients <- stats::coef(model$finalModel, model$bestTune$lambda)

  ids <- train_data$id

  residual_met <- train_data %>%
    select(starts_with("met"))

  actual <- score_calculation(ids, residual_met, coefficients) |>
    class()
  # We expect a list of size 2.
  expected <- "data.frame"

  expect_equal(actual, expected)
})
