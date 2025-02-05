# 12.- models_commute_exposure

### Load Required Libraries
pacman::p_load(
  tidyverse, DataExplorer, skimr, haven, patchwork, hablar, rio, magrittr, ggridges, 
  viridis, hrbrthemes, gtsummary, janitor, flextable, kableExtra, corrplot, summarytools,
  foreign, psych, car, readstata13, SmartEDA, GGally, ggcorrplot, 
  naniar, mice, visdat, irr, rstatix, nlme, e1071, readxl, moments, ggpubr, dotwhisker
)

### Load Data
# Raw data
helix_data <- readxl::read_xlsx("Data/clean/helix_db_clean_2.xlsx")

# Imputed data
load("Data/imputed/imp.IQR_v2_21_04_2022.RData")
load("Data/imputed/imp.IQR_v2_27_04_2022.RData")
imputed_data <- imp.IQR_v2 

### Explore Data
imputed_data %>% 
  mice::complete("long") %>% 
  glimpse()

### Define Variables
## Exposures
exposures <- c("hs_no2_yr_hs_r_log_iqr", "hs_pm25_yr_hs_r_iqr")

## Outcomes
outcomes <- c("hs_hitrtse", "hs_tmta_responsetime", "hs_tmtb_responsetime", 
              "hs_correct_raven", "hs_sum_domhand", "hs_laterality")

## Confounders
confounders <- c("h_cohort", "e3_ses", "h_age", "h_edumc", "h_mbmi", "h_parity", 
                 "h_fage", "e3_edufc", "e3_fbmic", "hs_age_years", "e3_sex", "h_seabir")

### Regression Models
run_models <- function(outcome, exposure) {
  raw_model <- imputed_data %>% 
    with(glm(reformulate(exposure, outcome), family = gaussian)) %>% 
    mice::pool()
  
  adjusted_model <- imputed_data %>% 
    with(glm(reformulate(c(exposure, confounders), outcome), family = gaussian)) %>% 
    mice::pool()
  
  multipollutant_model <- imputed_data %>% 
    with(glm(reformulate(c(exposures, confounders), outcome), family = gaussian)) %>% 
    mice::pool()
  
  list(raw = raw_model, adjusted = adjusted_model, multipollutant = multipollutant_model)
}

# Run models for each outcome
models <- lapply(outcomes, function(outcome) {
  lapply(exposures, function(exposure) run_models(outcome, exposure))
})

### Summarize Models
extract_results <- function(model, label) {
  tidy(model, conf.int = TRUE) %>% 
    dotwhisker::relabel_predictors(setNames(label, exposures)) %>% 
    filter(term %in% label) %>% 
    mutate(model = label, outcome = outcome)
}

summary_results <- lapply(models, function(outcome_models) {
  lapply(outcome_models, function(exp_models) {
    lapply(names(exp_models), function(type) extract_results(exp_models[[type]], type))
  })
}) %>% unlist(recursive = FALSE) %>% bind_rows() %>% mutate(across(where(is.numeric), round, 2))

### Export Results
table_path <- "output/tables/models/commute/"
rio::export(summary_results, paste0(table_path, "summary_results.xlsx"))

### Create Forest Plots
forest_plot <- function(data, title, xlim) {
  dotwhisker::dwplot(data, dot_args = list(size = 3),
                     vline = geom_vline(xintercept = 0, colour = "black", linetype = 2)) +
    theme_classic(base_size = 13) +
    theme(legend.position = 'none') +
    scale_x_continuous(limits = xlim, breaks = seq(xlim[1], xlim[2], length.out = 3)) +
    facet_grid(. ~ title)
}

# Define limits for each outcome
plot_limits <- list(
  "Hit react time (se)" = c(-25, 25),
  "Task switching score" = c(-16000, 16000),
  "Task shifting score" = c(-0.2, 0.2),
  "CPM score" = c(-2, 2),
  "Sum trial dom hand" = c(-8, 8),
  "Laterality index" = c(-2, 2)
)

plots <- lapply(names(plot_limits), function(title) {
  forest_plot(summary_results %>% filter(outcome == title), title, plot_limits[[title]])
})

final_plot <- Reduce(`|`, plots) +
  plot_annotation(title = 'C. Exposure to air pollution at commute',
                  subtitle = expression("Effect estimate of air pollution at commute (" *beta * " (95% CI))")) &
  theme(plot.title = element_text(face = 'bold', size = 16),
        plot.subtitle = element_text(hjust = 0.5, size = 14),
        strip.text.x = element_text(size = 12))

ggsave("output/figures/fig2C_commute_forest_plot_adj_cohort.tiff", plot = final_plot, width = 12, height = 3, dpi = 400)









