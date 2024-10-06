# 12.- models_commute_exposure

pacman::p_load(tidyverse, DataExplorer, skimr, haven, patchwork, hablar, rio, magrittr, ggridges, 
               viridis, hrbrthemes, gtsummary, janitor, flextable, kableExtra, corrplot, summarytools,
               foreign, psych, car, readstata13, SmartEDA, GGally, ggcorrplot, 
               naniar, mice, visdat, irr, rstatix, nlme, e1071, readxl, moments, ggpubr) # packages for missing values


# load data not imputed
helix_data <- readxl::read_xlsx("Data/clean/helix_db_clean_2.xlsx")

# load data imputed and with exposures standardize by IQR
load("Data/imputed/imp.IQR_v2_21_04_2022.RData") # this is a mids object
load("Data/imputed/imp.IQR_v2_27_04_2022.RData")
# we name the imputed object from mice as imputed_data
imputed_data <- imp.IQR_v2

# to se the variable names
imputed_data %>% 
  mice::complete("long") %>% # we could select any of the 20 imputed datasets
  dplyr::glimpse()

#################
### codebook ###
################

#########################################################################

#################
### exposures ###
################

# (Nitrogen dioxide) :  hs_no2_yr_hs_r_log_iqr (at commute)

# (Particulate matter 2,5) : hs_pm25_yr_hs_r_iqr (at commute)

#########################################################################

################
### outcomes ###
###############

# (Attentional function): hs_hitrtse 

# (Cognitive flexibility): hs_tmta_responsetime , hs_tmtb_responsetime (task switching)

# (Non-verbal intelligence): hs_correct_raven 

# (Fine motor functon):  hs_sum_domhand , hs_laterality ,  


##########################################################################

####################
### confounders ###
###################

# maternal confounders #

# h_age (maternal age at recruitment) + h_edumc (maternal educational level) + h_mbmi (maternal pre-pregnancy bmi) + h_parity (maternal parity before index pregnancy)

# paternal confounders #

# h_fage (paternal age at inclusion) + e3_edufc (paternal educational level) + e3_fbmic (paternal bmi)

# children confounders #

# hs_age_years (child age) + e3_sex (child sex) + h_seabir (season at birth)

# site confounders : h_cohort (cohort name)  + e3_ses (socieconomic status of the parents)

##########################################################################

# check the multiple imputations 


#########################################
# regression models postnatal exposures #
########################################

########################
# Attentional function # 
########################

# microenvironment : commute # 
# outcome : hs_hitrtse #

# model 1 raw - attentional function - commute exposure to NO2 
model1_af_commute_no2 <- imputed_data %>% 
  with(glm(formula = hs_hitrtse ~ hs_no2_yr_hs_r_log_iqr + h_cohort, 
           family = gaussian))  %>% 
  mice::pool() 

# model 2 raw - attentional function - commute exposure to PM25
model2_af_commute_pm25 <- imputed_data %>% 
  with(glm(formula = hs_hitrtse ~ hs_pm25_yr_hs_r_iqr + h_cohort,  
           family = gaussian)) %>% 
  mice::pool() 

# model 3 adjusted - attentional function - commute exposure to NO2 
model3_af_commute_no2_adj <- imputed_data %>% 
  with(glm(formula = hs_hitrtse ~ hs_no2_yr_hs_r_log_iqr +
             h_cohort  + e3_ses + 
             h_age + h_edumc + h_mbmi + h_parity +
             h_fage + e3_edufc + e3_fbmic + 
             hs_age_years + e3_sex + h_seabir, 
           family = gaussian)) %>% 
  mice::pool()

# model 4 adjusted - attentional function - commute exposure to PM25
model4_af_commute_pm25_adj <- imputed_data %>% 
  with(glm(formula = hs_hitrtse ~ hs_pm25_yr_hs_r_iqr +
             h_cohort  + e3_ses + 
             h_age + h_edumc + h_mbmi + h_parity +
             h_fage + e3_edufc + e3_fbmic + 
             hs_age_years + e3_sex + h_seabir, 
           family = gaussian)) %>% 
  mice::pool() 

# model 5 multi-pollutant - attentional function - commute exposure to NO2 and PM25
model5_af_commute_multipollutant <- imputed_data %>% 
  with(glm(formula = hs_hitrtse ~ hs_no2_yr_hs_r_log_iqr + hs_pm25_yr_hs_r_iqr +
             h_cohort  + e3_ses + 
             h_age + h_edumc + h_mbmi + h_parity +
             h_fage + e3_edufc + e3_fbmic + 
             hs_age_years + e3_sex + h_seabir, 
           family = gaussian)) %>% 
  mice::pool()  


# summarize models attentional function
model1_af_commute_no2 <- tidy(model1_af_commute_no2, conf.int = TRUE) %>% 
  dotwhisker::relabel_predictors("hs_no2_yr_hs_r_log_iqr" = "NO2") %>% 
  filter(term == "NO2") %>% 
  mutate(model = "NO2",
         outcomes = "Hit react time (se)") 

model2_af_commute_pm25 <- tidy(model2_af_commute_pm25, conf.int = TRUE) %>% 
  dotwhisker::relabel_predictors("hs_pm25_yr_hs_r_iqr" = "PM2.5") %>% 
  filter(term == "PM2.5") %>% 
  mutate(model = "PM2.5",
         outcomes = "Hit react time (se)") 

model3_af_commute_no2_adj <- tidy(model3_af_commute_no2_adj, conf.int = TRUE) %>% 
  dotwhisker::relabel_predictors("hs_no2_yr_hs_r_log_iqr" = "NO2 + C") %>% 
  filter(term == "NO2 + C") %>% 
  mutate(model = "NO2 + C",
         outcomes = "Hit react time (se)")

model4_af_commute_pm25_adj <- tidy(model4_af_commute_pm25_adj, conf.int = TRUE) %>% 
  dotwhisker::relabel_predictors("hs_pm25_yr_hs_r_iqr" = "PM2.5 + C") %>% 
  filter(term == "PM2.5 + C") %>%  
  mutate(model = "PM2.5 + C",
         outcomes = "Hit react time (se)")

model5_af_commute_no2_multipollutant <- tidy(model5_af_commute_multipollutant, conf.int = TRUE) %>% 
  dotwhisker::relabel_predictors("hs_no2_yr_hs_r_log_iqr" = "NO2 + PM2.5 + C") %>% 
  filter(term == "NO2 + PM2.5 + C") %>% 
  mutate(model = "NO2 + PM2.5 + C",
         outcomes = "Hit react time (se)")

model6_af_commute_pm25_multipollutant <- tidy(model5_af_commute_multipollutant, conf.int = TRUE) %>% 
  dotwhisker::relabel_predictors("hs_pm25_yr_hs_r_iqr" = "PM2.5 + NO2 + C") %>%
  filter(term == "PM2.5 + NO2 + C") %>%
  mutate(model = "PM2.5 + NO2 + C",
         outcomes = "Hit react time (se)")

# join the plots 
six_models_af_commute <- rbind(model1_af_commute_no2, model2_af_commute_pm25,
                       model3_af_commute_no2_adj, model4_af_commute_pm25_adj,
                       model5_af_commute_no2_multipollutant, model6_af_commute_pm25_multipollutant) %>% 
  mutate(across(where(is.numeric), round, 2))   # to put this into a table



# single pollutant model non-adjusted by cohort 
rio::export(six_models_af_commute,"output/tables/models/commute/six_models_af_commute.xlsx") # export as xlsx

# single pollutant model adjusted by cohort 
rio::export(six_models_af_commute,"output/tables/models/commute/six_models_af_commute_adj_cohort.xlsx") # export as xlsx


# forest plot attentional function - postnatal exposure home 
af_commute_exposure <- dotwhisker::dwplot(six_models_af_commute, dot_args = list(size = 4) ,
                                       vline = geom_vline(xintercept = 0, colour = "black",linetype = 2))  +
  theme_classic(base_size = 13) + theme(legend.position = 'none') + 
  scale_color_manual(values = c("#39568CFF", # sixth
                                "#39568CFF", # fifth
                                "#39568CFF", # fourth
                                "#39568CFF", # third
                                "#EEAD0E", # second
                                "#EEAD0E")) + # first
  facet_grid(. ~ "Hit react time (se)") 

# forest plot attentional function - postnatal exposure at commute (single pollutant model adjusted by cohort)
af_commute_exposure <- dotwhisker::dwplot(six_models_af_commute, dot_args = list(size = 3) ,
                                          vline = geom_vline(xintercept = 0, colour = "black",linetype = 2))  +
  theme_classic(base_size = 13) + theme(legend.position = 'none') + 
  scale_color_manual(values = c("#39568CFF", # sixth
                                "#39568CFF", # fifth
                                "#39568CFF", # fourth
                                "#39568CFF", # third
                                "#39568CFF", # second
                                "#39568CFF")) + # first
  facet_grid(. ~ "Hit react time (se)") 


################################################################################################################################################################################################

###########################
# Cognitive flexibility # 
##########################

# microenvironment : commute # 
# outcome : hs_tmtb_responsetime (task switching) 

############################
### task switching score ### 
#############################

# model 1 raw - task switching - commute exposure to NO2 
model1_task_switching_commute_no2 <- imputed_data %>% 
  with(glm(formula = hs_tmtb_responsetime ~ hs_no2_yr_hs_r_log_iqr + h_cohort, 
           family = gaussian)) %>% 
  mice::pool() 


# model 2 raw - task switching - commute exposure to PM25
model2_task_switching_commute_pm25 <- imputed_data %>% 
  with(glm(formula = hs_tmtb_responsetime ~ hs_pm25_yr_hs_r_iqr + h_cohort,  
           family = gaussian)) %>% 
  mice::pool() 

# model 3 adjusted - task switching - commute exposure to NO2 
model3_task_switching_commute_no2_adj <- imputed_data %>% 
  with(glm(formula = hs_tmtb_responsetime ~ hs_no2_yr_hs_r_log_iqr +
             h_cohort  + e3_ses + 
             h_age + h_edumc + h_mbmi + h_parity +
             h_fage + e3_edufc + e3_fbmic + 
             hs_age_years + e3_sex + h_seabir, 
           family = gaussian)) %>% 
  mice::pool()

# model 4 adjusted - task switching - commute exposure to PM25
model4_task_switching_commute_pm25_adj <- imputed_data %>% 
  with(glm(formula = hs_tmtb_responsetime ~ hs_pm25_yr_hs_r_iqr +
             h_cohort  + e3_ses + 
             h_age + h_edumc + h_mbmi + h_parity +
             h_fage + e3_edufc + e3_fbmic + 
             hs_age_years + e3_sex + h_seabir, 
           family = gaussian)) %>% 
  mice::pool() 


# model 5 multi-pollutant - task switching - commute exposure to NO2 and PM25
model5_task_switching_commute_multipollutant <- imputed_data %>% 
  with(glm(formula = hs_tmtb_responsetime ~ hs_no2_yr_hs_r_log_iqr + hs_pm25_yr_hs_r_iqr +
             h_cohort  + e3_ses + 
             h_age + h_edumc + h_mbmi + h_parity +
             h_fage + e3_edufc + e3_fbmic + 
             hs_age_years + e3_sex + h_seabir, 
           family = gaussian)) %>% 
  mice::pool()  

# summarize models 
model1_task_switching_commute_no2 <- tidy(model1_task_switching_commute_no2, conf.int = TRUE) %>% 
  dotwhisker::relabel_predictors("hs_no2_yr_hs_r_log_iqr" = "NO2") %>% 
  filter(term == "NO2") %>% 
  mutate(model = "NO2",
         outcomes = "Task switching score") 

model2_task_switching_commute_pm25 <- tidy(model2_task_switching_commute_pm25, conf.int = TRUE) %>% 
  dotwhisker::relabel_predictors("hs_pm25_yr_hs_r_iqr" = "PM2.5") %>% 
  filter(term == "PM2.5") %>% 
  mutate(model = "PM2.5",
         outcomes = "Task switching score") 

model3_task_switching_commute_no2_adj <- tidy(model3_task_switching_commute_no2_adj, conf.int = TRUE) %>% 
  dotwhisker::relabel_predictors("hs_no2_yr_hs_r_log_iqr" = "NO2 + C") %>% 
  filter(term == "NO2 + C") %>% 
  mutate(model = "NO2 + C",
         outcomes = "Task switching score") 

model4_task_switching_commute_pm25_adj <- tidy(model4_task_switching_commute_pm25_adj, conf.int = TRUE) %>% 
  dotwhisker::relabel_predictors("hs_pm25_yr_hs_r_iqr" = "PM2.5 + C") %>% 
  filter(term == "PM2.5 + C") %>%  
  mutate(model = "PM2.5 + C",
         outcomes = "Task switching score") 

model5_task_switching_commute_no2_multipollutant <- tidy(model5_task_switching_commute_multipollutant, conf.int = TRUE) %>% 
  dotwhisker::relabel_predictors("hs_no2_yr_hs_r_log_iqr" = "NO2 + PM2.5 + C") %>% 
  filter(term == "NO2 + PM2.5 + C") %>% 
  mutate(model = "NO2 + PM2.5 + C",
         outcomes = "Task switching score") 

model6_task_switching_commute_pm25_multipollutant <- tidy(model5_task_switching_commute_multipollutant, conf.int = TRUE)  %>% 
  dotwhisker::relabel_predictors("hs_pm25_yr_hs_r_iqr" = "PM2.5 + NO2 + C") %>%
  filter(term == "PM2.5 + NO2 + C") %>%
  mutate(model = "PM2.5 + NO2 + C",
         outcomes = "Task switching score") 


# join the plots 
six_models_task_switching_commute <- rbind(model1_task_switching_commute_no2, model2_task_switching_commute_pm25,
                                   model3_task_switching_commute_no2_adj, model4_task_switching_commute_pm25_adj,
                                   model5_task_switching_commute_no2_multipollutant, model6_task_switching_commute_pm25_multipollutant) %>% 
  mutate(across(where(is.numeric), round, 2))   # to put this into a table


# single pollutant model non-adjusted by cohort
rio::export(six_models_task_switching_commute,"output/tables/models/commute/six_models_task_switching_commute.xlsx") # export as xlsx

# single pollutant model adjusted by cohort
rio::export(six_models_task_switching_commute,"output/tables/models/commute/six_models_task_switching_commute_adj_cohort.xlsx") # export as xlsx

# forest plot task switching score - postnatal exposure home
task_switching_commute_exposure <- dotwhisker::dwplot(six_models_task_switching_commute, dot_args = list(size = 4) ,
                                                   vline = geom_vline(xintercept = 0, colour = "black",linetype = 2))  +
  theme_classic(base_size = 13) + theme(legend.position = 'none') + 
  scale_color_manual(values = c("#39568CFF", # sixth
                                "#39568CFF", # fifth
                                "#39568CFF", # fourth
                                "#39568CFF", # third
                                "#EEAD0E", # second
                                "#39568CFF")) + # first
  facet_grid(. ~ "Task switching")

# forest plot task switching score - postnatal exposure home (single pollutant model adjusted by cohort)
task_switching_commute_exposure <- dotwhisker::dwplot(six_models_task_switching_commute, dot_args = list(size = 3) ,
                                                      vline = geom_vline(xintercept = 0, colour = "black",linetype = 2))  +
  theme_classic(base_size = 13) + theme(legend.position = 'none') + 
  scale_color_manual(values = c("#39568CFF", # sixth
                                "#39568CFF", # fifth
                                "#39568CFF", # fourth
                                "#39568CFF", # third
                                "#39568CFF", # second
                                "#39568CFF")) + # first
  facet_grid(. ~ "Task switching")

############################
### task shifting score #### 
#############################

# outcome : task_shifting_score 
# microenvironment : commute


# model 1 raw - task shifting - commute exposure to NO2 
model1_task_shifting_commute_no2 <- imputed_data %>% 
  with(glm(formula = task_shifting_score ~ hs_no2_yr_hs_r_log_iqr + h_cohort, 
           family = gaussian)) %>% 
  mice::pool() 


# model 2 raw - task shifting - commute exposure to PM25
model2_task_shifting_commute_pm25 <- imputed_data %>% 
  with(glm(formula = task_shifting_score ~ hs_pm25_yr_hs_r_iqr + h_cohort,  
           family = gaussian)) %>% 
  mice::pool() 

# model 3 adjusted - task shifting - commute exposure to NO2 
model3_task_shifting_commute_no2_adj <- imputed_data %>% 
  with(glm(formula = task_shifting_score ~ hs_no2_yr_hs_r_log_iqr +
             h_cohort  + e3_ses + 
             h_age + h_edumc + h_mbmi + h_parity +
             h_fage + e3_edufc + e3_fbmic + 
             hs_age_years + e3_sex + h_seabir, 
           family = gaussian)) %>% 
  mice::pool()

# model 4 adjusted - task shifting - commute exposure to PM25
model4_task_shifting_commute_pm25_adj <- imputed_data %>% 
  with(glm(formula = task_shifting_score ~ hs_pm25_yr_hs_r_iqr +
             h_cohort  + e3_ses + 
             h_age + h_edumc + h_mbmi + h_parity +
             h_fage + e3_edufc + e3_fbmic + 
             hs_age_years + e3_sex + h_seabir, 
           family = gaussian)) %>% 
  mice::pool() 


# model 5 multi-pollutant - task shifting - commute exposure to NO2 and PM25
model5_task_shifting_commute_multipollutant <- imputed_data %>% 
  with(glm(formula = task_shifting_score ~ hs_no2_yr_hs_r_log_iqr + hs_pm25_yr_hs_r_iqr +
             h_cohort  + e3_ses + 
             h_age + h_edumc + h_mbmi + h_parity +
             h_fage + e3_edufc + e3_fbmic + 
             hs_age_years + e3_sex + h_seabir, 
           family = gaussian)) %>% 
  mice::pool()  

# summarize models 
model1_task_shifting_commute_no2 <- tidy(model1_task_shifting_commute_no2, conf.int = TRUE) %>% 
  dotwhisker::relabel_predictors("hs_no2_yr_hs_r_log_iqr" = "NO2") %>% 
  filter(term == "NO2") %>% 
  mutate(model = "NO2",
         outcomes = "Task shifting score") 

model2_task_shifting_commute_pm25 <- tidy(model2_task_shifting_commute_pm25, conf.int = TRUE) %>% 
  dotwhisker::relabel_predictors("hs_pm25_yr_hs_r_iqr" = "PM2.5") %>% 
  filter(term == "PM2.5") %>% 
  mutate(model = "PM2.5",
         outcomes = "Task shifting score") 

model3_task_shifting_commute_no2_adj <- tidy(model3_task_shifting_commute_no2_adj, conf.int = TRUE) %>% 
  dotwhisker::relabel_predictors("hs_no2_yr_hs_r_log_iqr" = "NO2 + C") %>% 
  filter(term == "NO2 + C") %>% 
  mutate(model = "NO2 + C",
         outcomes = "Task shifting score") 

model4_task_shifting_commute_pm25_adj <- tidy(model4_task_shifting_commute_pm25_adj, conf.int = TRUE) %>% 
  dotwhisker::relabel_predictors("hs_pm25_yr_hs_r_iqr" = "PM2.5 + C") %>% 
  filter(term == "PM2.5 + C") %>%  
  mutate(model = "PM2.5 + C",
         outcomes = "Task shifting score") 

model5_task_shifting_commute_no2_multipollutant <- tidy(model5_task_shifting_commute_multipollutant, conf.int = TRUE) %>% 
  dotwhisker::relabel_predictors("hs_no2_yr_hs_r_log_iqr" = "NO2 + PM2.5 + C") %>% 
  filter(term == "NO2 + PM2.5 + C") %>% 
  mutate(model = "NO2 + PM2.5 + C",
         outcomes = "Task shifting score") 

model6_task_shifting_commute_pm25_multipollutant <- tidy(model5_task_shifting_commute_multipollutant, conf.int = TRUE)  %>% 
  dotwhisker::relabel_predictors("hs_pm25_yr_hs_r_iqr" = "PM2.5 + NO2 + C") %>%
  filter(term == "PM2.5 + NO2 + C") %>%
  mutate(model = "PM2.5 + NO2 + C",
         outcomes = "Task shifting score") 


# join the plots 
six_models_task_shifting_commute <- rbind(model1_task_shifting_commute_no2, model2_task_shifting_commute_pm25,
                                           model3_task_shifting_commute_no2_adj, model4_task_shifting_commute_pm25_adj,
                                           model5_task_shifting_commute_no2_multipollutant, model6_task_shifting_commute_pm25_multipollutant) %>% 
  mutate(across(where(is.numeric), round, 2))   # to put this into a table


# single pollutant model non-adjusted by cohort
rio::export(six_models_task_shifting_commute,"output/tables/models/commute/six_models_task_shifting_commute.xlsx") # export as xlsx

# single pollutant model adjusted by cohort 
rio::export(six_models_task_shifting_commute,"output/tables/models/commute/six_models_task_shifting_commute_adj_cohort.xlsx") # export as xlsx


# forest plot task switching score - postnatal exposure home
task_shifting_commute_exposure <- dotwhisker::dwplot(six_models_task_shifting_commute, dot_args = list(size = 4),
                                                      vline = geom_vline(xintercept = 0, colour = "black",linetype = 2))  +
  theme_classic(base_size = 13) + theme(legend.position = 'none') + 
  scale_color_manual(values = c("#39568CFF", # sixth
                                "#39568CFF", # fifth
                                "#39568CFF", # fourth
                                "#39568CFF", # third
                                "#EEAD0E", # second
                                "#39568CFF")) + # first
  facet_grid(. ~ "Task shifting score") 

# forest plot task switching score - postnatal exposure commute (single pollutant model adjusted by cohort)
task_shifting_commute_exposure <- dotwhisker::dwplot(six_models_task_shifting_commute, dot_args = list(size = 3),
                                                     vline = geom_vline(xintercept = 0, colour = "black",linetype = 2))  +
  theme_classic(base_size = 13) + theme(legend.position = 'none') + 
  scale_color_manual(values = c("#39568CFF", # sixth
                                "#39568CFF", # fifth
                                "#39568CFF", # fourth
                                "#39568CFF", # third
                                "#39568CFF", # second
                                "#39568CFF")) + # first
  facet_grid(. ~ "Task shifting score") 

################################################################################################################################################################################################

###########################
# Non-verbal intelligence # 
##########################

# microenvironment : commute # 
# outcome : hs_correct_raven #

# model 1 raw - non-verbal intelligence - commute exposure to NO2 
model1_ni_commute_no2 <- imputed_data %>% 
  with(glm(formula = hs_correct_raven ~ hs_no2_yr_hs_r_log_iqr + h_cohort, 
           family = gaussian)) %>% 
  mice::pool()  

# model 2 raw - non-verbal intelligence - commute exposure to PM25
model2_ni_commute_pm25 <- imputed_data %>% 
  with(glm(formula = hs_correct_raven ~ hs_pm25_yr_hs_r_iqr + h_cohort,  
           family = gaussian)) %>% 
  mice::pool()

# model 3 adjusted - non-verbal intelligence - commute exposure to NO2 
model3_ni_commute_no2_adj <- imputed_data %>% 
  with(glm(formula = hs_correct_raven ~ hs_no2_yr_hs_r_log_iqr +
             h_cohort  + e3_ses + 
             h_age + h_edumc + h_mbmi + h_parity +
             h_fage + e3_edufc + e3_fbmic + 
             hs_age_years + e3_sex + h_seabir, 
           family = gaussian)) %>% 
  mice::pool()  

# model 4 adjusted - non-verbal intelligence - commute exposure to PM25
model4_ni_commute_pm25_adj <- imputed_data %>% 
  with(glm(formula = hs_correct_raven ~ hs_pm25_yr_hs_r_iqr +
             h_cohort  + e3_ses + 
             h_age + h_edumc + h_mbmi + h_parity +
             h_fage + e3_edufc + e3_fbmic + 
             hs_age_years + e3_sex + h_seabir, 
           family = gaussian)) %>% 
  mice::pool()  


# model 5 multi-pollutant - non-verbal intelligence - commute exposure to NO2 and PM25
model5_ni_commute_multipollutant <- imputed_data %>% 
  with(glm(formula = hs_correct_raven ~ hs_no2_yr_hs_r_log_iqr + hs_pm25_yr_hs_r_iqr +
             h_cohort  + e3_ses + 
             h_age + h_edumc + h_mbmi + h_parity +
             h_fage + e3_edufc + e3_fbmic + 
             hs_age_years + e3_sex + h_seabir, 
           family = gaussian)) %>% 
  mice::pool()  

# summarize models 
model1_ni_commute_no2  <- tidy(model1_ni_commute_no2, conf.int = TRUE) %>% 
  dotwhisker::relabel_predictors("hs_no2_yr_hs_r_log_iqr" = "NO2") %>% 
  filter(term == "NO2") %>%  
  mutate(model = "NO2",
         outcomes = "Raven score") 

model2_ni_commute_pm25 <- tidy(model2_ni_commute_pm25, conf.int = TRUE) %>% 
  dotwhisker::relabel_predictors("hs_pm25_yr_hs_r_iqr" = "PM2.5") %>% 
  filter(term == "PM2.5") %>% 
  mutate(model = "PM2.5",
         outcomes = "Raven score") 

model3_ni_commute_no2_adj <- tidy(model3_ni_commute_no2_adj, conf.int = TRUE) %>% 
  dotwhisker::relabel_predictors("hs_no2_yr_hs_r_log_iqr" = "NO2 + C") %>% 
  filter(term == "NO2 + C") %>% 
  mutate(model = "NO2 + C",
         outcomes = "Raven score")

model4_ni_commute_pm25_adj <- tidy(model4_ni_commute_pm25_adj, conf.int = TRUE) %>%
  dotwhisker::relabel_predictors("hs_pm25_yr_hs_r_iqr" = "PM2.5 + C") %>% 
  filter(term == "PM2.5 + C") %>% 
  mutate(model = "PM2.5 + C",
         outcomes = "Raven score")

model5_ni_commute_no2_multipollutant <- tidy(model5_ni_commute_multipollutant, conf.int = TRUE) %>% 
  dotwhisker::relabel_predictors("hs_no2_yr_hs_r_log_iqr" = "NO2 + PM2.5 + C") %>% 
  filter(term == "NO2 + PM2.5 + C") %>% 
  mutate(model = "NO2 + PM2.5 + C",
         outcomes = "Raven score")

model6_ni_commute_pm25_multipollutant <- tidy(model5_ni_commute_multipollutant, conf.int = TRUE) %>% 
  dotwhisker::relabel_predictors("hs_pm25_yr_hs_r_iqr" = "PM2.5 + NO2 + C") %>%
  filter(term == "PM2.5 + NO2 + C") %>% 
  mutate(model = "PM2.5 + NO2 + C",
         outcomes = "Raven score")

# joining models 
six_models_ni_commute <- rbind(model1_ni_commute_no2, model2_ni_commute_pm25,
                       model3_ni_commute_no2_adj, model4_ni_commute_pm25_adj,
                       model5_ni_commute_no2_multipollutant, model6_ni_commute_pm25_multipollutant) %>% 
  mutate(across(where(is.numeric), round, 2))   # to put this into a table

# single pollutant model non-adjusted by cohort
rio::export(six_models_ni_commute,"output/tables/models/commute/six_models_ni_commute.xlsx") # export as xlsx

# single pollutant model adjusted by cohort
rio::export(six_models_ni_commute,"output/tables/models/commute/six_models_ni_commute_adj_cohort.xlsx") # export as xlsx

# forest plot raven score - postnatal exposure home
raven_commute_exposure <- dotwhisker::dwplot(six_models_ni_commute, dot_args = list(size = 4),
                                          vline = geom_vline(xintercept = 0, colour = "black",linetype = 2)) +
  theme_classic(base_size = 13) + theme(legend.position = 'none') + 
  scale_color_manual(values = c("#39568CFF", # sixth
                                "#39568CFF", # fifth
                                "#39568CFF", # fourth
                                "#39568CFF", # third
                                "#EEAD0E", # second
                                "#EEAD0E")) + # first
  facet_grid(. ~ "Raven score")

# forest plot raven score - postnatal exposure commute (single pollutant model adjusted by cohort)
raven_commute_exposure <- dotwhisker::dwplot(six_models_ni_commute, dot_args = list(size = 3),
                                             vline = geom_vline(xintercept = 0, colour = "black",linetype = 2)) +
  theme_classic(base_size = 13) + theme(legend.position = 'none') + 
  scale_color_manual(values = c("#39568CFF", # sixth
                                "#39568CFF", # fifth
                                "#39568CFF", # fourth
                                "#39568CFF", # third
                                "#39568CFF", # second
                                "#39568CFF")) + # first
  facet_grid(. ~ "CPM score")

################################################################################################################################################################################################

###########################
### Fine motor function ### 
##########################

# microenvironment : commute # 
# exposures : hs_no2_yr_hs_r_log_iqr , hs_pm25_yr_hs_r_iqr
# outcome :  hs_sum_domhand , hs_laterality

######################################
# sum dom hand - finger tapping test #
#####################################

# model 1 raw - finger tapping test (hs_sum_domhand) - commute exposure to NO2 
model1_sumdomhand_commute_no2 <- imputed_data %>% 
  with(glm(formula = hs_sum_domhand ~ hs_no2_yr_hs_r_log_iqr + h_cohort, 
           family = gaussian)) %>% 
  mice::pool() 

# model 2 raw - finger tapping test (hs_sum_domhand) - commute exposure to PM25
model2_sumdomhand_commute_pm25 <- imputed_data %>% 
  with(glm(formula = hs_sum_domhand ~ hs_pm25_yr_hs_r_iqr + h_cohort,  
           family = gaussian)) %>% 
  mice::pool() 

# model 3 adjusted - finger tapping test (hs_sum_domhand) - commute exposure to NO2 
model3_sumdomhand_commute_no2_adj <- imputed_data %>% 
  with(glm(formula = hs_sum_domhand ~ hs_no2_yr_hs_r_log_iqr +
             h_cohort  + e3_ses + 
             h_age + h_edumc + h_mbmi + h_parity +
             h_fage + e3_edufc + e3_fbmic + 
             hs_age_years + e3_sex + h_seabir, 
           family = gaussian)) %>% 
  mice::pool()  

# model 4 adjusted - finger tapping test (hs_sum_domhand) - commute exposure to PM25
model4_sumdomhand_commute_pm25_adj <- imputed_data %>% 
  with(glm(formula = hs_sum_domhand ~ hs_pm25_yr_hs_r_iqr +
             h_cohort  + e3_ses + 
             h_age + h_edumc + h_mbmi + h_parity +
             h_fage + e3_edufc + e3_fbmic + 
             hs_age_years + e3_sex + h_seabir, 
           family = gaussian)) %>% 
  mice::pool()   


# model 5 multi-pollutant - finger tapping test (hs_sum_domhand) - commute exposure to NO2 and PM25
model5_sumdomhand_commute_multipollutant <- imputed_data %>% 
  with(glm(formula = hs_sum_domhand ~ hs_no2_yr_hs_r_log_iqr + hs_pm25_yr_hs_r_iqr +
             h_cohort  + e3_ses + 
             h_age + h_edumc + h_mbmi + h_parity +
             h_fage + e3_edufc + e3_fbmic + 
             hs_age_years + e3_sex + h_seabir, 
           family = gaussian)) %>% 
  mice::pool()   

# summarize models 
model1_sumdomhand_commute_no2 <- tidy(model1_sumdomhand_commute_no2, conf.int = TRUE) %>% 
  dotwhisker::relabel_predictors("hs_no2_yr_hs_r_log_iqr" = "NO2") %>% 
  filter(term == "NO2") %>%  
  mutate(model = "NO2",
         outcomes = "Sum trials dom hand") 

model2_sumdomhand_commute_pm25 <- tidy(model2_sumdomhand_commute_pm25, conf.int = TRUE) %>% 
  dotwhisker::relabel_predictors("hs_pm25_yr_hs_r_iqr" = "PM2.5") %>% 
  filter(term == "PM2.5") %>% 
  mutate(model = "PM2.5",
         outcomes = "Sum trials dom hand") 

model3_sumdomhand_commute_no2_adj <- tidy(model3_sumdomhand_commute_no2_adj, conf.int = TRUE) %>% 
  dotwhisker::relabel_predictors("hs_no2_yr_hs_r_log_iqr" = "NO2 + C") %>% 
  filter(term == "NO2 + C") %>% 
  mutate(model = "NO2 + C",
         outcomes = "Sum trials dom hand")

model4_sumdomhand_commute_pm25_adj <- tidy(model4_sumdomhand_commute_pm25_adj, conf.int = TRUE) %>%
  dotwhisker::relabel_predictors("hs_pm25_yr_hs_r_iqr" = "PM2.5 + C") %>% 
  filter(term == "PM2.5 + C") %>% 
  mutate(model = "PM2.5 + C",
         outcomes = "Sum trials dom hand")

model5_sumdomhand_commute_no2_multipollutant <- tidy(model5_sumdomhand_commute_multipollutant, conf.int = TRUE) %>% 
  dotwhisker::relabel_predictors("hs_no2_yr_hs_r_log_iqr" = "NO2 + PM2.5 + C") %>% 
  filter(term == "NO2 + PM2.5 + C") %>% 
  mutate(model = "NO2 + PM2.5 + C",
         outcomes = "Sum trials dom hand")

model6_sumdomhand_commute_pm25_multipollutant <- tidy(model5_sumdomhand_commute_multipollutant, conf.int = TRUE) %>% 
  dotwhisker::relabel_predictors("hs_pm25_yr_hs_r_iqr" = "PM2.5 + NO2 + C") %>%
  filter(term == "PM2.5 + NO2 + C") %>% 
  mutate(model = "PM2.5 + NO2 + C",
         outcomes = "Sum trials dom hand")


# joining models 
six_models_sumdomhand_commute <- rbind(model1_sumdomhand_commute_no2, model2_sumdomhand_commute_pm25,
                               model3_sumdomhand_commute_no2_adj, model4_sumdomhand_commute_pm25_adj,
                               model5_sumdomhand_commute_no2_multipollutant, model6_sumdomhand_commute_pm25_multipollutant) %>% 
  mutate(across(where(is.numeric), round, 2))   # to put this into a table


# single pollutant model non-adjusted by cohort
rio::export(six_models_sumdomhand_commute,"output/tables/models/commute/six_models_sumdomhand_commute.xlsx") # export as xlsx

# single pollutant model adjusted by cohort 
rio::export(six_models_sumdomhand_commute,"output/tables/models/commute/six_models_sumdomhand_commute_adj_cohort.xlsx") # export as xlsx



# forest plot sum dom hand - postnatal exposure 
sumdomhand_commute_exposure <- dotwhisker::dwplot(six_models_sumdomhand_commute, dot_args = list(size = 4),
                                               vline = geom_vline(xintercept = 0, colour = "black",linetype = 2)) +
  theme_classic(base_size = 13) + theme(legend.position = 'none') + 
  scale_color_manual(values = c("#39568CFF", # sixth
                                "#39568CFF", # fifth
                                "#39568CFF", # fourth
                                "#39568CFF", # third
                                "#EEAD0E", # second
                                "#EEAD0E")) + # first
  facet_grid(~ "Sum dom hand")

# forest plot sum dom hand - postnatal exposure (single pollutant model adjusted by cohort)
sumdomhand_commute_exposure <- dotwhisker::dwplot(six_models_sumdomhand_commute, dot_args = list(size = 3),
                                                  vline = geom_vline(xintercept = 0, colour = "black",linetype = 2)) +
  theme_classic(base_size = 13) + theme(legend.position = 'none') + 
  scale_color_manual(values = c("#39568CFF", # sixth
                                "#39568CFF", # fifth
                                "#39568CFF", # fourth
                                "#39568CFF", # third
                                "#39568CFF", # second
                                "#39568CFF")) + # first
  facet_grid(~ "Sum dom hand")


##########################################
# Laterality index - finger tapping test #
#########################################

# model 1 raw - finger tapping test (hs_laterality) - commute exposure to NO2 
model1_laterality_commute_no2 <- imputed_data %>% 
  with(glm(formula = hs_laterality ~ hs_no2_yr_hs_r_log_iqr, 
           family = gaussian)) %>% 
  mice::pool() 

# model 2 raw - finger tapping test ( hs_laterality) - commute exposure to PM25
model2_laterality_commute_pm25 <- imputed_data %>% 
  with(glm(formula = hs_laterality ~ hs_pm25_yr_hs_r_iqr ,  
           family = gaussian)) %>% 
  mice::pool() 

# model 3 adjusted - finger tapping test (hs_laterality) - commute exposure to NO2 
model3_laterality_commute_no2_adj <- imputed_data %>% 
  with(glm(formula = hs_laterality ~ hs_no2_yr_hs_r_log_iqr +
             h_cohort  + e3_ses + 
             h_age + h_edumc + h_mbmi + h_parity +
             h_fage + e3_edufc + e3_fbmic + 
             hs_age_years + e3_sex + h_seabir, 
           family = gaussian)) %>% 
  mice::pool()  

# model 4 adjusted - finger tapping test (hs_laterality) - commute exposure to PM25
model4_laterality_commute_pm25_adj <- imputed_data %>% 
  with(glm(formula = hs_laterality ~ hs_pm25_yr_hs_r_iqr +
             h_cohort  + e3_ses + 
             h_age + h_edumc + h_mbmi + h_parity +
             h_fage + e3_edufc + e3_fbmic + 
             hs_age_years + e3_sex + h_seabir, 
           family = gaussian)) %>% 
  mice::pool()   


# model 5 multi-pollutant - finger tapping test ( hs_laterality) - commute exposure to NO2 and PM25
model5_laterality_commute_multipollutant <- imputed_data %>% 
  with(glm(formula = hs_laterality ~ hs_no2_yr_hs_r_log_iqr + hs_pm25_yr_hs_r_iqr +
             h_cohort  + e3_ses + 
             h_age + h_edumc + h_mbmi + h_parity +
             h_fage + e3_edufc + e3_fbmic + 
             hs_age_years + e3_sex + h_seabir, 
           family = gaussian)) %>% 
  mice::pool()   

# summarize models 
model1_laterality_commute_no2 <- tidy(model1_laterality_commute_no2, conf.int = TRUE) %>% 
  dotwhisker::relabel_predictors("hs_no2_yr_hs_r_log_iqr" = "NO2") %>% 
  filter(term == "NO2") %>%  
  mutate(model = "NO2",
         outcomes = "Laterality index") 

model2_laterality_commute_pm25 <- tidy(model2_laterality_commute_pm25, conf.int = TRUE) %>% 
  dotwhisker::relabel_predictors("hs_pm25_yr_hs_r_iqr" = "PM2.5") %>% 
  filter(term == "PM2.5") %>% 
  mutate(model = "PM2.5",
         outcomes = "Laterality index") 

model3_laterality_commute_no2_adj <- tidy(model3_laterality_commute_no2_adj, conf.int = TRUE) %>% 
  dotwhisker::relabel_predictors("hs_no2_yr_hs_r_log_iqr" = "NO2 + C") %>% 
  filter(term == "NO2 + C") %>% 
  mutate(model = "NO2 + C",
         outcomes = "Laterality index")

model4_laterality_commute_pm25_adj <- tidy(model4_laterality_commute_pm25_adj, conf.int = TRUE) %>%
  dotwhisker::relabel_predictors("hs_pm25_yr_hs_r_iqr" = "PM2.5 + C") %>% 
  filter(term == "PM2.5 + C") %>% 
  mutate(model = "PM2.5 + C",
         outcomes = "Laterality index")

model5_laterality_commute_no2_multipollutant <- tidy(model5_laterality_commute_multipollutant, conf.int = TRUE) %>% 
  dotwhisker::relabel_predictors("hs_no2_yr_hs_r_log_iqr" = "NO2 + PM2.5 + C") %>% 
  filter(term == "NO2 + PM2.5 + C") %>% 
  mutate(model = "NO2 + PM2.5 + C",
         outcomes = "Laterality index")

model6_laterality_commute_pm25_multipollutant <- tidy(model5_laterality_commute_multipollutant, conf.int = TRUE) %>% 
  dotwhisker::relabel_predictors("hs_pm25_yr_hs_r_iqr" = "PM2.5 + NO2 + C") %>%
  filter(term == "PM2.5 + NO2 + C") %>% 
  mutate(model = "PM2.5 + NO2 + C",
         outcomes = "Laterality index")

# joining models 
six_models_laterality_commute <- rbind(model1_laterality_commute_no2, model2_laterality_commute_pm25,
                               model3_laterality_commute_no2_adj, model4_laterality_commute_pm25_adj,
                               model5_laterality_commute_no2_multipollutant, model6_laterality_commute_pm25_multipollutant) %>% 
  mutate(across(where(is.numeric), round, 2))   # to put this into a table


rio::export(six_models_laterality_commute,"output/tables/models/commute/six_models_laterality_commute.xlsx") # export as xlsx



# forest plot laterality index - postnatal exposure commute

laterality_commute_exposure <- dotwhisker::dwplot(six_models_laterality_commute,dot_args = list(size = 4), 
                                               vline = geom_vline(xintercept = 0, colour = "black",linetype = 2)) +
  theme_classic(base_size = 13) + theme(legend.position = 'none') + 
  scale_color_manual(values = c("#39568CFF", # sixth
                                "#39568CFF", # fifth
                                "#39568CFF", # fourth
                                "#39568CFF", # third
                                "#39568CFF", # second
                                "#39568CFF")) + # first
  facet_grid(~ "Laterality index")



################################################################################################################################################################################################

##################################
### final figure home exposure ###
#################################

library(patchwork)
install.packages("ggtext")
library(ggtext)


# forest plot attentional function (ANT)
af_commute_exposure <- af_commute_exposure + 
  facet_grid(. ~ "Hit react time (se)") +
  scale_x_continuous(limits = c(-25, 25),
                     breaks = c(-25, 0, 25)) 
  
 

# forest plot task switching score (TMT)
task_switching_commute_exposure <- task_switching_commute_exposure +
  facet_grid(. ~ "Task switching score") +
  theme(axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        axis.line.y = element_blank()) +
  scale_x_continuous(limits = c(-16000, 16000),
                     breaks = c(-15000, 0, 15000))

# forest plot task shifting score (TMT)
task_shifting_commute_exposure <- task_shifting_commute_exposure +
  facet_grid(. ~ "Task shifting score") +
  theme(axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        axis.line.y = element_blank())  +
  scale_x_continuous(limits = c(-0.20, 0.20),
                     breaks = c(-0.20, 0, 0.20))

# forest plot raven score (CMPC Raven)
raven_commute_exposure <- raven_commute_exposure +
  facet_grid(. ~ "CPM score") + 
  theme(axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        axis.line.y = element_blank()) + 
  scale_x_continuous(limits = c(-2, 2),
                     breaks = c(-2, 0, 2))
  
  
# forest plot sum trial dom hand (Finger tapping test)
sumdomhand_commute_exposure <- sumdomhand_commute_exposure +
  facet_grid(. ~ "Sum trial dom hand") + 
  theme(axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        axis.line.y = element_blank()) +
  scale_x_continuous(limits = c(-8, 8),
                     breaks = c(-8, 0, 8))
  

# forest plot laterality (Finger tapping test)
laterality_commute_exposure <- laterality_commute_exposure + 
  facet_grid(. ~ "Laterality index") + 
  theme(axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        axis.line.y = element_blank()) +
  scale_x_continuous(limits = c(-2, 2),
                     breaks = c(-2, -1, 0, 1, 2))
  

# we need to use | instead of + to obtain an image of one row and six column 
commute_forest_plot <- af_commute_exposure | task_switching_commute_exposure | task_shifting_commute_exposure | raven_commute_exposure | sumdomhand_commute_exposure 

commute_forest_plot <- commute_forest_plot + 
  plot_annotation(title = 'C. Exposure to air pollution at commute',
                  subtitle = expression("Effect estimate of air pollution at commute (" *beta * "" [AP-commute] * " (95 % CI))")) &
  theme(plot.title = element_text(face = 'bold', size = 16),
        plot.subtitle = element_text(hjust=0.5, size = 14),
        strip.text.x = element_text(size = 12))


# single pollutant model non-adjusted by cohort
ggsave("output/figures/fig2C_commute_forest_plot_v3.tiff", width = 12, height = 3, dpi = 400) 

# single pollutant model adjusted by cohort 
ggsave("output/figures/fig2C_commute_forest_plot_adj_cohort.tiff", width = 12, height = 3, dpi = 400) 















