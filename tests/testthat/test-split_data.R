library(dplyr)

test_that("Evaluates output of two data frames", {
  actual <- sim_data |>
    preprocessing(id = "id", target = "exposure", start_metabolites = 5) |>
    splitting() |>
    length()

  # We expect the standardization to have 12 zeroes.
  expected <- 3

  expect_equal(actual, expected)
})
