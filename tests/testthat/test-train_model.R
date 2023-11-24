library(dplyr)

test_that("Check if function returns linear regression model", {
  #runs the model and evaluates list output
  actual <- sim_data |>
    preprocessing(id = "id", target = "exposure", start_metabolites = 4) |>
    select(id, target, starts_with("met")) |>
    training_lr()

  expect_type(actual, "list")
})
