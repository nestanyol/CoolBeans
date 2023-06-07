## code to prepare `simulated-data` dataset goes here

library(dplyr)
library(lavaan)
library(lavaanPlot)
library(simsurv)

# Create DAG for the simulation -------------------------------------------

dag_model <- "
# Network connections
metabolite_1 ~ 0.25*metabolite_2 + 0.25*metabolite_12
metabolite_2 ~ 0.25*metabolite_3 + 0.25*metabolite_7
metabolite_3 ~ 0.25*metabolite_4 + 0.25*metabolite_12
metabolite_4 ~ 0.25*metabolite_5 + 0.25*metabolite_12
metabolite_5 ~ 0.25*metabolite_6 + 0.25*metabolite_9
metabolite_6 ~ 0.25*metabolite_12
metabolite_7 ~ 0.25*metabolite_8
metabolite_8 ~ 0.25*metabolite_9 + 0.25*metabolite_10
metabolite_9 ~ 0.25*metabolite_10
metabolite_10 ~ 0.25*metabolite_11

# Classification dietary score
metabolite_1 ~ 0.2*diet_score
metabolite_8 ~ -0.5*diet_score
metabolite_10 ~ 0.3*diet_score
"

# Simulate data -----------------------------------------------------------


insert_random_missingness <- function(data) {
  purrr::map_df(data, ~ .[sample(
    c(TRUE, NA),
    prob = c(0.98, 0.02),
    size = length(.),
    replace = TRUE
  )])
}

sim_data <- as_tibble(simulateData(dag_model, sample.nobs = 2000)) %>%
  mutate(across(matches("metabolite_"), ~ . + (5 + abs(min(.))) * 1.1)) %>%
  mutate(id = 1:n(), diet_score = round(abs(runif(2000, min = 0, max = 50)))) %>%
  insert_random_missingness()

# Check if DAG created correctly

sim_model_fit <- sem(model = dag_model, data = sim_data)
summary(sim_model_fit)
lavaanPlot(model = sim_model_fit)


# Save dataset ------------------------------------------------------------

usethis::use_data(sim_data, overwrite = TRUE)
