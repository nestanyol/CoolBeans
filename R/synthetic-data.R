library(dplyr)
library(lavaan)
library(lavaanPlot)
library(simsurv)


# Build the DAG model to base simulation ----------------------------------

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

# Insert random missing values --------------------------------------------

insert_random_missingness <- function(data) {
  variables <- setdiff(names(data), c("id", "diet_score"))

  data[variables] <- purrr::map_df(data[variables], ~ .[sample(
    c(TRUE, NA),
    prob = c(0.98, 0.02),
    size = length(.),
    replace = TRUE
  )])
  data
}

# Simulate data -----------------------------------------------------------

sim_data <- as_tibble(simulateData(dag_model, sample.nobs = 2000)) %>%
  mutate(across(matches("metabolite_"), ~ . + (5 + abs(min(.))) * 1.1)) %>%
  mutate(id = 1:n()) %>%
  mutate(diet_score = sapply(1:n(), function(x) sample(0:50, 1))) %>%
  insert_random_missingness()


# Check if DAG was created correctly:
sim_model_fit <- lavaan::sem(model = dag_model, data = sim_data)
summary(sim_model_fit)
lavaan::lavaanPlot(model = sim_model_fit)

# Save data ---------------------------------------------------------------

usethis::use_data(sim_data, overwrite = TRUE) # Save dataset in the `data/` folder
