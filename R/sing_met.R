
#' Title
#'
#' @param variables
#'
#' @return
#' @export
#'
#' @examples


sing_met <- function(variables) {


  # Model definitions
  metabolite_columns <- colnames(nhs1_lcd_df)[3:ncol(nhs1_lcd_df)]
  confounders_mod1 <- c("sex",  )

  # Set up modelling function:
  lm_singmet <- function(metabolite, data, confounders) {
    model_formula <- reformulate(c(colnames(data)[2]), confounders, response = metabolite)
    results<- lm(model_formula, data = data)
    broom::tidy(results) %>%
      mutate(yvar=metabolite,.before=everything())
  }

  # Set up parallel processing
  plan(multisession)
  metabolite_columns %>%
    future_map(~lm_singmet(.x, confounders, data=nhs1_lcd_df)) %>%
    list_rbind(names_to = "model_id")
  # Reset the plan to sequential
  plan(sequential)

}
