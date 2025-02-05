# 12.- models_school_exposure

pacman::p_load(tidyverse, DataExplorer, skimr, haven, patchwork, hablar, rio, magrittr, ggridges, 
               viridis, hrbrthemes, gtsummary, janitor, flextable, kableExtra, corrplot, summarytools,
               foreign, psych, car, readstata13, SmartEDA, GGally, ggcorrplot, 
               naniar, mice, visdat, irr, rstatix, nlme, e1071, readxl, moments, ggpubr,
               lme4, broom.mixed) 

# Load data
helix_data <- readxl::read_xlsx("Data/clean/helix_db_clean_2.xlsx")

# Load imputed data
load("Data/imputed/imp.IQR_v2_21_04_2022.RData")
load("Data/imputed/imp.IQR_v2_27_04_2022.RData")
imputed_data <- imp.IQR_v2 

# Check variable names
glimpse(mice::complete(imputed_data, "long"))

#################
### Exposures ###
#################
# Nitrogen dioxide (NO2): hs_no2_yr_hs_s (at school) 
# Particulate matter 2.5 (PM2.5): hs_pm25_yr_hs_s (at school)

################
### Outcomes ###
################
# Attentional function: hs_hitrtse
# Cognitive flexibility: hs_tmta_responsetime, hs_tmtb_responsetime
# Non-verbal intelligence: hs_correct_raven
# Fine motor function: hs_sum_domhand, hs_laterality

####################
### Confounders ###
####################
# Maternal: h_age, h_edumc, h_mbmi, h_parity
# Paternal: h_fage, e3_edufc, e3_fbmic
# Children: hs_age_years, e3_sex, h_seabir
# Site: h_cohort, e3_ses

#########################################
# Regression Models - School Exposure  #
#########################################

# Attentional Function
model1_af_school_no2 <- imputed_data %>% 
  with(glm(hs_hitrtse ~ hs_no2_yr_hs_s_log_iqr + h_cohort, family = gaussian)) %>% 
  mice::pool()

model2_af_school_pm25 <- imputed_data %>% 
  with(glm(hs_hitrtse ~ hs_pm25_yr_hs_s_iqr + h_cohort, family = gaussian)) %>% 
  mice::pool()

model3_af_school_no2_adj <- imputed_data %>% 
  with(glm(hs_hitrtse ~ hs_no2_yr_hs_s_log_iqr + h_cohort + e3_ses + 
             h_age + h_edumc + h_mbmi + h_parity + h_fage + e3_edufc + e3_fbmic + 
             hs_age_years + e3_sex + h_seabir, family = gaussian)) %>% 
  mice::pool()

model4_af_school_pm25_adj <- imputed_data %>% 
  with(glm(hs_hitrtse ~ hs_pm25_yr_hs_s_iqr + h_cohort + e3_ses + 
             h_age + h_edumc + h_mbmi + h_parity + h_fage + e3_edufc + e3_fbmic + 
             hs_age_years + e3_sex + h_seabir, family = gaussian)) %>% 
  mice::pool()

model5_af_school_multipollutant <- imputed_data %>% 
  with(glm(hs_hitrtse ~ hs_no2_yr_hs_s_log_iqr + hs_pm25_yr_hs_s_iqr + h_cohort + e3_ses + 
             h_age + h_edumc + h_mbmi + h_parity + h_fage + e3_edufc + e3_fbmic + 
             hs_age_years + e3_sex + h_seabir, family = gaussian)) %>% 
  mice::pool()

# Summarize and Export Models
models_af_school <- list(model1_af_school_no2, model2_af_school_pm25, model3_af_school_no2_adj, 
                         model4_af_school_pm25_adj, model5_af_school_multipollutant) %>% 
  map(~ tidy(.x, conf.int = TRUE)) %>% 
  bind_rows(.id = "model") %>% 
  mutate(model = recode(model, `1` = "NO2", `2` = "PM2.5", `3` = "NO2 + C", `4` = "PM2.5 + C", `5` = "NO2 + PM2.5 + C"), 
         outcomes = "Hit react time (se)")

rio::export(models_af_school, "output/tables/models/school/models_af_school.xlsx")

# Forest Plot
library(dotwhisker)
af_school_exposure <- dwplot(models_af_school, dot_args = list(size = 3),
                             vline = geom_vline(xintercept = 0, colour = "black", linetype = 2)) +
  theme_classic(base_size = 13) +
  theme(legend.position = 'none') +
  scale_color_manual(values = rep("#1F968BFF", 6)) +
  facet_grid(. ~ "Hit react time (se)")

ggsave("output/figures/fig2B_school_forest_plot.tiff", width = 12, height = 3, dpi = 400)






















