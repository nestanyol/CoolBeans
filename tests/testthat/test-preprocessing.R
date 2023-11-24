library(dplyr)

test_that("Preprocessing zero-mean centers the data", {
  # Calculate the mean of all metabolite columns and only keep the vector of zeroes
  actual <- sim_data |>
    preprocessing(id = "id", target = "exposure", start_metabolites = 4) |>
    summarise(across(starts_with("meta"), ~round(mean(.x), digits = 0))) |>
    as.vector() |>
    unlist() |>
    set_names(NULL)

  # We expect the standardization to have 12 zeroes.
  expected <- rep(0, times = 12)

  expect_equal(actual, expected)
})
