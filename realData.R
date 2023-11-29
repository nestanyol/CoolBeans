#test
devtools::load_all()

#there is no score, isn't it?
raw_data <- readRDS("/Users/garciada/Library/CloudStorage/OneDrive-Chalmers/Documents/GitHub/CoolBeans/inst/extdata/MEDGICARB_peaktable_clustered.rds")
raw_data <- readRDS("/Users/danielaagarcia-soriano/Library/CloudStorage/OneDrive-Chalmers/Documents/GitHub/CoolBeans/inst/extdata/MEDGICARB_peaktable_clustered.rds")

#for subsetting
# raw_data <- raw_dataAll[,c(1,8,14:100)] #classification
# raw_data <- raw_dataAll[,c(1,13,14:100)] #regression
# raw_data <-  raw_dataAll[,c(1:5000)] #%>% as_tibble() #test
# hilic <- (c("HN", "HP"))
# raw_data <- raw_dataAll %>% as_tibble() %>%  select(-starts_with(hilic))

#check NA
# which(is.na(raw_data[,2]))
# #remove rows that have NA in target/label column
# raw_data <- raw_data %>% drop_na('group')


raw_data_boxplots <- raw_data[,c(14:23)] %>%
  tidyr::gather(key = "HN", value = "value", starts_with("HN"))

boxplot <- raw_data_boxplots %>%
  ggplot2::ggplot(aes(x = HN, y = value)) +
  ggplot2::geom_boxplot() +
  labs(x = "Metabolites", y = "Intensity") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1))

boxplot

#PRE-PROCESS --------------------------------------------------------------

prep_data <- preprocessing(raw_data,'combo','group', 14, imputation = "median") #classification
#prep_data <- preprocessing(raw_data,'combo','BMI', 14, imputation = "median") #regression

prep_data_boxplots <- prep_data[,c(14:23)] %>%
  tidyr::gather(key = "HN", value = "value", starts_with("HN"))

boxplot_prep <- prep_data_boxplots %>%
  ggplot2::ggplot(aes(x = HN, y = value)) +
  ggplot2::geom_boxplot() +
  ggplot2::labs(x = "Metabolites", y = "Intensity") +
  ggplot2::theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1))

boxplot_prep

# METABOLIC SIGNATURE ------------------------------------------------------

#sin_metabolites <- sing_met_analysis(data=prep_data,exposure_feature = "target", start_metabolites = 14) # no confounders and no correction
sin_metabolites <- sing_met_analysis(data=prep_data, exposure_feature = "target", start_metabolites = 14,confounders = c("sex", "age"), correction = "fdr")

met_pvalue <- sin_metabolites %>%
  ggplot2::ggplot(aes(x = yvar, y = p.value_corrected)) +
  geom_segment( aes(x=yvar, xend=yvar, y=0, yend=p.value_corrected), color="black") +
  geom_point( color="gray", size=2, alpha=0.6) +
  theme_light() +
  coord_flip() +
  labs(x = "Metabolites", y = "p-value") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1)) +
  theme(axis.text.y = element_text(size = 5))

met_pvalue

sin_metabolites_filtered <- sin_metabolites[str_detect(sin_metabolites$term, "target"),] %>%
  filter(p.value_corrected < 0.01) %>%
  arrange(p.value_corrected)

selected_metabolites_id <- sin_metabolites_filtered$yvar

selected_data <- prep_data %>%
  select(c(id, target, selected_metabolites_id))

# #Loop for feature-wise tidymodel
# fast_df <- NULL
# for (i in 14:ncol(raw_data)) { #load raw data
#   #for (i in 14:100) { #load raw data
#   d <- raw_data %>% #create data frame with only fasting data
#     filter(timepoint == "-15") %>%
#     select(all_of(c(2:5, 7:13, i))) %>%
#     #Select metadata but remove column hemolysis(column 6) and combo(column 1). This make all values unique and data frame can expand properly. Otherwise I have NAs.
#     pivot_wider( #Data wrangling. Pre and post-intervention data is on a column
#       names_from = occasion,
#       values_from = 12
#     ) %>% rename(pre_int = a, post_int = b)
#   fast_df <- rbind(fast_df, d)
# }
#
# sing_met_int(df=fast_df, eq=post_int ~ pre_int + batch + group + sex + age + BMI + site)

#WITHOUT METABOLIC SIGNATURE ------------------------------------------------------

selected_data <- prep_data[,12:ncol(prep_data)]

# SPLIT DATA --------------------------------------------------------------

data_split <- splitting(selected_data) #after single metabolite selection
train_data <- data_split$train_data
test_data <- data_split$test_data
#check NA
which(is.na(train_data$target))
which(is.na(test_data$target))
#which(is.na(train_data[,2]))
#which(is.na(test_data[,2]))
#remove rows that have NA in target/label column
train_data <- train_data %>% drop_na('target')
test_data <- test_data %>% drop_na('target')

# TRAIN MODEL -------------------------------------------------------------
## linear regression
trained_model <- train_model_lr(train_data) #regression

## random forest
trained_model <- train_model_rf(train_data, type=1) #regression
trained_model <- train_model_rf(train_data,type=2) #classification

## knn
#trained_model <- train_model_knn(train_data, type=1) #regression
#trained_model <- train_model_knn(train_data,type=2) #classification


# TEST MODEL --------------------------------------------------------------
## linear regression
results <- testing_lr(trained_model, test_data)


results <- test_rf_classification(trained_model, test_data)


# CROSS-VALIDATION --------------------------------------------------------------

# Create a recipe with folds
set.seed(345)
folds <- vfold_cv(train_data, v = 10)

# Create the model specification
model_spec <- parsnip::linear_reg(penalty = 0.5, mixture = 0.5) %>%
  parsnip::set_engine("glmnet")

# Train the model
model <- workflows::workflow() %>%
  workflows::add_model(model_spec) %>%
  workflows::add_formula(target ~ .)

set.seed(123)
model_cv <- model %>%
  fit_resamples(folds,
                control = control_resamples(save_pred = TRUE))

collect_metrics(model_cv)

collect_predictions(model_cv)


