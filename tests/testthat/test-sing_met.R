library(dplyr)

test_that("Test if output dataframe has the expected seven columns", {
  "test if the otput as 7 columns"
  actual <- sim_data |>
    preprocessing(id = "id", target = "exposure", start_metabolites = 4) |>
    sing_met_analysis(exposure_feature = "target", start_metabolites = 4,confounders = c("age")) |>
    ncol()

  # We expect the standardization to have 12 zeroes.
  expected <- 7

  expect_equal(actual, expected)
})
