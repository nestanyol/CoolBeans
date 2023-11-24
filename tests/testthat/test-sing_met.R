library(dplyr)

test_that("Evaluates and compares pvalue", {
  "test if the p.value column gets the expected value"
  actual <- sim_data |>
    preprocessing(id = "id", target = "exposure", start_metabolites = 5) |>
    sing_met_analysis(exposure_feature = "target", start_metabolites = 5,confounders = c("age")) |>
    pull(p.value) |>
    mean()

  # We expect mean pvalue to have a specific value.
  expected <- 0.0002898679

  expect_equal(actual, expected)
})
