library(dplyr)

test_that("Check if function returns training output", {
  #runs the model and evaluates list output
  model <- sim_data |>
    preprocessing(id = "id", target = "exposure", start_metabolites = 5) |>
    select(id, target, starts_with("met")) |>
    crossvalidation_model()

  actual <- length(model)

    expect_gt(actual, 0)
})
