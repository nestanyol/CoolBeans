library(dplyr)
library(simsurv)


# Build the DAG model to base simulation ----------------------------------

dag_model <- "
# Exposure feature:
metabolite_1 ~ 0.2*exposure
metabolite_8 ~ -0.5*exposure
metabolite_10 ~ 0.3*exposure

# Confounders:
age ~ exposure + outcome_continuous

# Network connections:
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
"

# Insert random missing values --------------------------------------------

insert_random_missingness <- function(data) {
  variables <- dplyr::setdiff(names(data), c("id", "exposure"))

  data[variables] <- purrr::map_df(data[variables], ~ .[sample(
    c(TRUE, NA),
    prob = c(0.98, 0.02),
    size = length(.),
    replace = TRUE
  )])
  data
}

# Range -------------------------------------------------------------------

fun_range <- function(x) {                              # Create user-defined function
  (x - min(x)) / (max(x) - min(x))*10
}

# Simulate data -----------------------------------------------------------

sim_data <- as_tibble(simulateData(dag_model, sample.nobs = 2000)) %>%
  mutate(id = 1:n()) %>%
  mutate(age = age + 50) %>%
  mutate(across(matches("metabolite_"), ~ . + (5 + abs(min(.))) * 1.1)) %>%
  mutate(exposure = fun_range(exposure)) %>%
  insert_random_missingness()


# Check if DAG was created correctly:
# sim_model_fit <- lavaan::sem(model = dag_model, data = sim_data)
# summary(sim_model_fit)
# lavaan::lavaanPlot(model = sim_model_fit)

# Save data ---------------------------------------------------------------

usethis::use_data(sim_data, overwrite = TRUE) # Save dataset in the `data/` folder
