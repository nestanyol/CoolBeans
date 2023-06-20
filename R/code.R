# Load libraries:
library(tidyverse)
library(tidymodels)
library(recipes)
library(glmnet)
library(rsample)
library(caret)
source(here::here("R/preprocessing.R"))
source(here::here("R/split_data.R"))
source(here::here("R/train_model.R"))
source(here::here("R/test_model.R"))

# IMPORT DATASET ----------------------------------------------------------

# raw_data <- read_csv2(
#  here::here("data/metabolomics_data.csv"),
#  # Convert column names to `snake_case`
#  name_repair = snakecase::to_snake_case
# )

# Code for the simulated dataset.

load("data/sim_data.rda")
raw_data <- sim_data

# Data summary
summary(raw_data)

raw_data_boxplots <- raw_data %>%
  tidyr::gather(key = "metabolite", value = "value", starts_with("metabolite"))

boxplot <- raw_data_boxplots %>%
  ggplot2::ggplot(aes(x = metabolite, y = value)) +
  ggplot2::geom_boxplot() +
  labs(x = "Metabolites", y = "Intensity") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1))

ggsave(filename = "doc/images/raw_data_boxplots.png", plot = boxplot)

# DATA PREPROCESSING ------------------------------------------------------

prep_data <- preprocess_data(raw_data)

prep_data_boxplots <- prep_data %>%
  tidyr::gather(key = "metabolite", value = "value", starts_with("metabolite"))

boxplot_prep <- prep_data_boxplots %>%
  ggplot2::ggplot(aes(x = metabolite, y = value)) +
  ggplot2::geom_boxplot() +
  ggplot2::labs(x = "Metabolites", y = "Intensity") +
  ggplot2::theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1))

ggplot2::ggsave(filename = "doc/images/prep_data_boxplots.png", plot = boxplot_prep)

# SPLIT DATA --------------------------------------------------------------

data_split <- split_data(prep_data)
train_data <- data_split$train_data
test_data <- data_split$test_data

# TRAIN MODEL -------------------------------------------------------------

trained_model <- train_model(train_data)

# TEST MODEL --------------------------------------------------------------

results <- test_model(trained_model, test_data)
model_coeffs <- results$model_coeffs
rmse_val <- results$rmse_val
#r_squared_val <- results$r_squared_val

coefficients <- model_coeffs$estimate
feature_names <- names(coefficients)[-1]
coefficients_df <- data.frame(feature = feature_names, coefficient = coefficients[-1])
