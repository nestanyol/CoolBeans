library(dplyr)

test_that("Evaluates and checks number of columns", {
  "test if expected number of columns are retrieved"
  actual <- sim_data |>
    preprocessing(id = "id", target = "exposure", start_metabolites = 5) |>
    sing_met_analysis(exposure_feature = "target", start_metabolites = 5,confounders = c("age")) |>
    ncol()

  # We expect mean pvalue to have a specific value.
  expected <- 7

  expect_equal(actual, expected)
})
