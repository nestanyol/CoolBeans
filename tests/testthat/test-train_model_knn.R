library(dplyr)

test_that("Check if function returns knn model", {
  #runs the model and evaluates list output
  actual <- sim_data |>
    preprocessing(id = "id", target = "exposure", start_metabolites = 5) |>
    select(id, target, starts_with("met")) |>
    training_knn()

  expect_type(actual, "list")
})
