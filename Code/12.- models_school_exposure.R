# 12.- models_school_exposure

pacman::p_load(tidyverse, DataExplorer, skimr, haven, patchwork, hablar, rio, magrittr, ggridges, 
               viridis, hrbrthemes, gtsummary, janitor, flextable, kableExtra, corrplot, summarytools,
               foreign, psych, car, readstata13, SmartEDA, GGally, ggcorrplot, 
               naniar, mice, visdat, irr, rstatix, nlme, e1071, readxl, moments, ggpubr,
               lme4, broom.mixed) 



# load data not imputed
helix_data <- readxl::read_xlsx("Data/clean/helix_db_clean_2.xlsx")

# load data imputed and with exposures standardize by IQR
load("Data/imputed/imp.IQR_v2_21_04_2022.RData") # first mids object
load("Data/imputed/imp.IQR_v2_27_04_2022.RData")


# we name the imputed object from mice as imputed_data
imputed_data <- imp.IQR_v2 

# to se the variable names
imputed_data %>% 
  mice::complete("long") %>% # we could select any of the 20 imputed datasets
  glimpse() 


#################
### codebook ###
################

#########################################################################

#################
### exposures ###
################


# (Nitrogen dioxide) : hs_no2_yr_hs_s (at school) 

# (Particulate matter 2,5) : hs_pm25_yr_hs_s (at shool) 

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

# microenvironment : school # 
# outcome : hs_hitrtse #


# model 1 raw - attentional function - school exposure to NO2 
model1_af_school_no2 <- imputed_data %>% 
  with(glm(formula = hs_hitrtse ~ hs_no2_yr_hs_s_log_iqr + h_cohort, 
           family = gaussian)) %>% 
  mice::pool() 


# model 2 raw - attentional function - school exposure to PM25
model2_af_school_pm25 <- imputed_data %>% 
  with(glm(formula = hs_hitrtse ~ hs_pm25_yr_hs_s_iqr + h_cohort,  
           family = gaussian)) %>% 
  mice::pool() 

# model 3 adjusted - attentional function - school exposure to NO2 
model3_af_school_no2_adj <- imputed_data %>% 
  with(glm(formula = hs_hitrtse ~ hs_no2_yr_hs_s_log_iqr +
             h_cohort  + e3_ses +
             h_age + h_edumc + h_mbmi + h_parity +
             h_fage + e3_edufc + e3_fbmic + 
             hs_age_years + e3_sex + h_seabir, 
           family = gaussian)) %>% 
  mice::pool()  

# model 4 adjusted - attentional function - school exposure to PM25
model4_af_school_pm25_adj <- imputed_data %>% 
  with(glm(formula = hs_hitrtse ~ hs_pm25_yr_hs_s_iqr +
             h_cohort  + e3_ses + 
             h_age + h_edumc + h_mbmi + h_parity +
             h_fage + e3_edufc + e3_fbmic + 
             hs_age_years + e3_sex + h_seabir, 
           family = gaussian)) %>% 
  mice::pool() 


# model 5 multi-pollutant - attentional function - school exposure to NO2 and PM25
model5_af_school_multipollutant <- imputed_data %>% 
  with(glm(formula = hs_hitrtse ~ hs_no2_yr_hs_s_log_iqr + hs_pm25_yr_hs_s_iqr +
             h_cohort  + e3_ses + 
             h_age + h_edumc + h_mbmi + h_parity +
             h_fage + e3_edufc + e3_fbmic + 
             hs_age_years + e3_sex + h_seabir, 
           family = gaussian)) %>% 
  mice::pool()  

# summarize models 
model1_af_school_no2 <- tidy(model1_af_school_no2, conf.int = TRUE) %>% 
  dotwhisker::relabel_predictors("hs_no2_yr_hs_s_log_iqr" = "NO2") %>% 
  filter(term == "NO2") %>% 
  mutate(model = "NO2",
         outcomes = "Hit react time (se)") 

model2_af_school_pm25 <- tidy(model2_af_school_pm25, conf.int = TRUE) %>% 
  dotwhisker::relabel_predictors("hs_pm25_yr_hs_s_iqr" = "PM2.5") %>% 
  filter(term == "PM2.5") %>% 
  mutate(model = "PM2.5",
         outcomes = "Hit react time (se)") 

model3_af_school_no2_adj <- tidy(model3_af_school_no2_adj, conf.int = TRUE) %>% 
  dotwhisker::relabel_predictors("hs_no2_yr_hs_s_log_iqr" = "NO2 + C") %>% 
  filter(term == "NO2 + C") %>% 
  mutate(model = "NO2 + C",
         outcomes = "Hit react time (se)")

model4_af_school_pm25_adj <- tidy(model4_af_school_pm25_adj, conf.int = TRUE) %>% 
  dotwhisker::relabel_predictors("hs_pm25_yr_hs_s_iqr" = "PM2.5 + C") %>% 
  filter(term == "PM2.5 + C") %>%  
  mutate(model = "PM2.5 + C",
         outcomes = "Hit react time (se)")

model5_af_school_no2_multipollutant <- tidy(model5_af_school_multipollutant, conf.int = TRUE) %>% 
  dotwhisker::relabel_predictors("hs_no2_yr_hs_s_log_iqr" = "NO2 + PM2.5 + C") %>% 
  filter(term == "NO2 + PM2.5 + C") %>% 
  mutate(model = "NO2 + PM2.5 + C",
         outcomes = "Hit react time (se)")

model6_af_school_pm25_multipollutant <- tidy(model5_af_school_multipollutant, conf.int = TRUE) %>% 
  dotwhisker::relabel_predictors("hs_pm25_yr_hs_s_iqr" = "PM2.5 + NO2 + C") %>%
  filter(term == "PM2.5 + NO2 + C") %>%
  mutate(model = "PM2.5 + NO2 + C",
         outcomes = "Hit react time (se)")


# join the plots 
six_models_af_school <- rbind(model1_af_school_no2, model2_af_school_pm25,
                            model3_af_school_no2_adj, model4_af_school_pm25_adj,
                            model5_af_school_no2_multipollutant, model6_af_school_pm25_multipollutant)


# single pollutant model non-adjusted by cohort
rio::export(six_models_af_school,"output/tables/models/school/six_models_af_school.xlsx") # export as xlsx

# single pollutant model adjusted by cohort
rio::export(six_models_af_school,"output/tables/models/school/six_models_af_school_adj_cohort.xlsx") # export as xlsx



# forest plot attentional function - postnatal exposure school 
attentional_function_school_exposure <- dotwhisker::dwplot(six_models_af_school, dot_args = list(size = 4) ,
                                                         vline = geom_vline(xintercept = 0, colour = "black",linetype = 2))  +
  theme_classic(base_size = 13) + theme(legend.position = 'none') + 
  scale_color_manual(values = c("#1F968BFF", # sixth
                                "#1F968BFF", # fifth
                                "#1F968BFF", # fourth
                                "#1F968BFF", # third
                                "#EEAD0E", # second
                                "#EEAD0E"))  + # first
  facet_grid(. ~ "Hit reac time (se)")

# forest plot attentional function - postnatal exposure school (single pollutant adjusted + cohort)
attentional_function_school_exposure <- dotwhisker::dwplot(six_models_af_school, dot_args = list(size = 3) ,
                                                           vline = geom_vline(xintercept = 0, colour = "black",linetype = 2))  +
  theme_classic(base_size = 13) + theme(legend.position = 'none') + 
  scale_color_manual(values = c("#1F968BFF", # sixth
                                "#1F968BFF", # fifth
                                "#1F968BFF", # fourth
                                "#1F968BFF", # third
                                "#1F968BFF", # second
                                "#1F968BFF"))  + # first
  facet_grid(. ~ "Hit reac time (se)")

################################################################################################################################################################################################

###########################
# Cognitive flexibility # 
##########################

# microenvironment : school # 
# outcome : hs_tmtb_responsetime (task switching) 

############################
### task switching score ### 
###########################

# model 1 raw - task switching - school exposure to NO2 
model1_task_switching_school_no2 <- imputed_data %>% 
  with(glm(formula = hs_tmtb_responsetime ~ hs_no2_yr_hs_s_log_iqr + h_cohort, 
           family = gaussian)) %>% 
  mice::pool() 


# model 2 raw - task switching - school exposure to PM25
model2_task_switching_school_pm25 <- imputed_data %>% 
  with(glm(formula = hs_tmtb_responsetime ~ hs_pm25_yr_hs_s_iqr + h_cohort,  
           family = gaussian)) %>% 
  mice::pool() 

# model 3 adjusted - task switching - school exposure to NO2 
model3_task_switching_school_no2_adj <- imputed_data %>% 
  with(glm(formula = hs_tmtb_responsetime ~ hs_no2_yr_hs_s_log_iqr +
             h_cohort  + e3_ses + 
             h_age + h_edumc + h_mbmi + h_parity +
             h_fage + e3_edufc + e3_fbmic + 
             hs_age_years + e3_sex + h_seabir, 
           family = gaussian)) %>% 
  mice::pool()  

# model 4 adjusted - task switching - school exposure to PM25
model4_task_switching_school_pm25_adj <- imputed_data %>% 
  with(glm(formula = hs_tmtb_responsetime ~ hs_pm25_yr_hs_s_iqr +
             h_cohort  + e3_ses + 
             h_age + h_edumc + h_mbmi + h_parity +
             h_fage + e3_edufc + e3_fbmic + 
             hs_age_years + e3_sex + h_seabir, 
           family = gaussian)) %>% 
  mice::pool() 



# model 5 multi-pollutant - task swithching - school exposure to NO2 and PM25
model5_task_switching_school_multipollutant <- imputed_data %>% 
  with(glm(formula = hs_tmtb_responsetime ~ hs_no2_yr_hs_s_log_iqr + hs_pm25_yr_hs_s_iqr +
             h_cohort  + e3_ses + 
             h_age + h_edumc + h_mbmi + h_parity +
             h_fage + e3_edufc + e3_fbmic + 
             hs_age_years + e3_sex + h_seabir, 
           family = gaussian)) %>% 
  mice::pool()  

# summarize models 
model1_task_switching_school_no2 <- tidy(model1_task_switching_school_no2, conf.int = TRUE) %>% 
  dotwhisker::relabel_predictors("hs_no2_yr_hs_s_log_iqr" = "NO2") %>% 
  filter(term == "NO2") %>% 
  mutate(model = "NO2",
         outcomes = "Task switching score") 

model2_task_switching_school_pm25 <- tidy(model2_task_switching_school_pm25, conf.int = TRUE) %>% 
  dotwhisker::relabel_predictors("hs_pm25_yr_hs_s_iqr" = "PM2.5") %>% 
  filter(term == "PM2.5") %>% 
  mutate(model = "PM2.5",
         outcomes = "Task switching score") 

model3_task_switching_school_no2_adj <- tidy(model3_task_switching_school_no2_adj, conf.int = TRUE) %>% 
  dotwhisker::relabel_predictors("hs_no2_yr_hs_s_log_iqr" = "NO2 + C") %>% 
  filter(term == "NO2 + C") %>% 
  mutate(model = "NO2 + C",
         outcomes = "Task switching score") 

model4_task_switching_school_pm25_adj <- tidy(model4_task_switching_school_pm25_adj, conf.int = TRUE) %>% 
  dotwhisker::relabel_predictors("hs_pm25_yr_hs_s_iqr" = "PM2.5 + C") %>% 
  filter(term == "PM2.5 + C") %>%  
  mutate(model = "PM2.5 + C",
         outcomes = "Task switching score") 

model5_task_switching_school_no2_multipollutant <- tidy(model5_task_switching_school_multipollutant, conf.int = TRUE) %>% 
  dotwhisker::relabel_predictors("hs_no2_yr_hs_s_log_iqr" = "NO2 + PM2.5 + C") %>% 
  filter(term == "NO2 + PM2.5 + C") %>% 
  mutate(model = "NO2 + PM2.5 + C",
         outcomes = "Task switching score") 

model6_task_switching_school_pm25_multipollutant <- tidy(model5_task_switching_school_multipollutant, conf.int = TRUE)  %>% 
  dotwhisker::relabel_predictors("hs_pm25_yr_hs_s_iqr" = "PM2.5 + NO2 + C") %>%
  filter(term == "PM2.5 + NO2 + C") %>%
  mutate(model = "PM2.5 + NO2 + C",
         outcomes = "Task switching score") 


# join the plots 
six_models_task_switching_school <- rbind(model1_task_switching_school_no2, model2_task_switching_school_pm25,
                                        model3_task_switching_school_no2_adj, model4_task_switching_school_pm25_adj,
                                        model5_task_switching_school_no2_multipollutant, model6_task_switching_school_pm25_multipollutant)

# single pollutant model non-adjusted by cohort
rio::export(six_models_task_switching_school,"output/tables/models/school/six_models_task_switching_school.xlsx") # export as xlsx

# single pollutant model adjusted by cohort
rio::export(six_models_task_switching_school,"output/tables/models/school/six_models_task_switching_school_adj_cohort.xlsx") # export as xlsx


# forest plot task switching score - postnatal exposure school
task_switching_school_exposure <- dotwhisker::dwplot(six_models_task_switching_school, dot_args = list(size = 4) ,
                                                   vline = geom_vline(xintercept = 0, colour = "black",linetype = 2))  +
  theme_classic(base_size = 13) + theme(legend.position = 'none') + 
  scale_color_manual(values = c("#1F968BFF", # sixth
                                "#1F968BFF", # fifth
                                "#1F968BFF", # fourth
                                "#1F968BFF", # third
                                "#EEAD0E", # second
                                "#EEAD0E")) + # first
  facet_grid(. ~ "Task switching")


# forest plot task switching score - postnatal exposure school (single pollutant adjusted + cohort)
task_switching_school_exposure <- dotwhisker::dwplot(six_models_task_switching_school, dot_args = list(size = 3) ,
                                                     vline = geom_vline(xintercept = 0, colour = "black",linetype = 2))  +
  theme_classic(base_size = 13) + theme(legend.position = 'none') + 
  scale_color_manual(values = c("#1F968BFF", # sixth
                                "#1F968BFF", # fifth
                                "#1F968BFF", # fourth
                                "#1F968BFF", # third
                                "#1F968BFF", # second
                                "#1F968BFF")) + # first
   facet_grid(. ~ "Task switching")

################################################################################################################################################################################################


############################
### task shifting score ### 
###########################

# outcome : task_shifting_score 
# microenvironment : school 

# model 1 raw - task shifting - school exposure to NO2 
model1_task_shifting_school_no2 <- imputed_data %>% 
  with(glm(formula = task_shifting_score  ~ hs_no2_yr_hs_s_log_iqr + h_cohort, 
           family = gaussian)) %>% 
  mice::pool() 

# model 2 raw -  task shifting - school exposure to PM25
model2_task_shifting_school_pm25 <- imputed_data %>% 
  with(glm(formula = task_shifting_score  ~ hs_pm25_yr_hs_s_iqr + h_cohort,  
           family = gaussian)) %>% 
  mice::pool() 

# model 3 adjusted -  task shifting - school exposure to NO2 
model3_task_shifting_school_no2_adj <- imputed_data %>% 
  with(glm(formula = task_shifting_score  ~ hs_no2_yr_hs_s_log_iqr +
             h_cohort  + e3_ses + 
             h_age + h_edumc + h_mbmi + h_parity +
             h_fage + e3_edufc + e3_fbmic + 
             hs_age_years + e3_sex + h_seabir, 
           family = gaussian)) %>% 
  mice::pool()  

# model 4 adjusted -  task shifting - school exposure to PM25
model4_task_shifting_school_pm25_adj <- imputed_data %>% 
  with(glm(formula = task_shifting_score  ~ hs_pm25_yr_hs_s_iqr +
             h_cohort  + e3_ses + 
             h_age + h_edumc + h_mbmi + h_parity +
             h_fage + e3_edufc + e3_fbmic + 
             hs_age_years + e3_sex + h_seabir, 
           family = gaussian)) %>% 
  mice::pool() 



# model 5 multi-pollutant -  task shifting - school exposure to NO2 and PM25
model5_task_shifting_school_multipollutant <- imputed_data %>% 
  with(glm(formula = task_shifting_score  ~ hs_no2_yr_hs_s_log_iqr + hs_pm25_yr_hs_s_iqr +
             h_cohort  + e3_ses + 
             h_age + h_edumc + h_mbmi + h_parity +
             h_fage + e3_edufc + e3_fbmic + 
             hs_age_years + e3_sex + h_seabir, 
           family = gaussian)) %>% 
  mice::pool()  

# summarize models 
model1_task_shifting_school_no2 <- tidy(model1_task_shifting_school_no2, conf.int = TRUE) %>% 
  dotwhisker::relabel_predictors("hs_no2_yr_hs_s_log_iqr" = "NO2") %>% 
  filter(term == "NO2") %>% 
  mutate(model = "NO2",
         outcomes = "Task shifting score") 

model2_task_shifting_school_pm25 <- tidy(model2_task_shifting_school_pm25, conf.int = TRUE) %>% 
  dotwhisker::relabel_predictors("hs_pm25_yr_hs_s_iqr" = "PM2.5") %>% 
  filter(term == "PM2.5") %>% 
  mutate(model = "PM2.5",
         outcomes = "Task shifting score") 

model3_task_shifting_school_no2_adj <- tidy(model3_task_shifting_school_no2_adj, conf.int = TRUE) %>% 
  dotwhisker::relabel_predictors("hs_no2_yr_hs_s_log_iqr" = "NO2 + C") %>% 
  filter(term == "NO2 + C") %>% 
  mutate(model = "NO2 + C",
         outcomes = "Task shifting score") 

model4_task_shifting_school_pm25_adj <- tidy(model4_task_shifting_school_pm25_adj, conf.int = TRUE) %>% 
  dotwhisker::relabel_predictors("hs_pm25_yr_hs_s_iqr" = "PM2.5 + C") %>% 
  filter(term == "PM2.5 + C") %>%  
  mutate(model = "PM2.5 + C",
         outcomes = "Task shifting score") 

model5_task_shifting_school_no2_multipollutant <- tidy(model5_task_shifting_school_multipollutant, conf.int = TRUE) %>% 
  dotwhisker::relabel_predictors("hs_no2_yr_hs_s_log_iqr" = "NO2 + PM2.5 + C") %>% 
  filter(term == "NO2 + PM2.5 + C") %>% 
  mutate(model = "NO2 + PM2.5 + C",
         outcomes = "Task shifting score") 

model6_task_shifting_school_pm25_multipollutant <- tidy(model5_task_shifting_school_multipollutant, conf.int = TRUE)  %>% 
  dotwhisker::relabel_predictors("hs_pm25_yr_hs_s_iqr" = "PM2.5 + NO2 + C") %>%
  filter(term == "PM2.5 + NO2 + C") %>%
  mutate(model = "PM2.5 + NO2 + C",
         outcomes = "Task shifting score") 


# join the plots 
six_models_task_shifting_school <- rbind(model1_task_shifting_school_no2, model2_task_shifting_school_pm25,
                                       model3_task_shifting_school_no2_adj, model4_task_shifting_school_pm25_adj,
                                       model5_task_shifting_school_no2_multipollutant, model6_task_shifting_school_pm25_multipollutant)

# single pollutant model non-adjusted by cohort
rio::export(six_models_task_shifting_school,"output/tables/models/school/six_models_task_shifting_school.xlsx") # export as xlsx

# single pollutant model adjusted by cohort
rio::export(six_models_task_shifting_school,"output/tables/models/school/six_models_task_shifting_school_adj_cohort.xlsx") # export as xlsx


# forest plot task switching score - postnatal exposure school
task_shifting_school_exposure <- dotwhisker::dwplot(six_models_task_shifting_school, dot_args = list(size = 4) ,
                                                  vline = geom_vline(xintercept = 0, colour = "black",linetype = 2))  +
  theme_classic(base_size = 13) + theme(legend.position = 'none') + 
  scale_color_manual(values = c("#1F968BFF", # sixth
                                "#1F968BFF", # fifth
                                "#1F968BFF", # fourth
                                "#1F968BFF", # third
                                "#EEAD0E", # second
                                "#1F968BFF")) + # first
  facet_grid(. ~ "Task shifting")

# forest plot task switching score - postnatal exposure school (single pollutant adjusted + cohort)
task_shifting_school_exposure <- dotwhisker::dwplot(six_models_task_shifting_school, dot_args = list(size = 3) ,
                                                    vline = geom_vline(xintercept = 0, colour = "black",linetype = 2))  +
  theme_classic(base_size = 13) + theme(legend.position = 'none') + 
  scale_color_manual(values = c("#1F968BFF", # sixth
                                "#1F968BFF", # fifth
                                "#1F968BFF", # fourth
                                "#1F968BFF", # third
                                "#1F968BFF", # second
                                "#1F968BFF")) + # first
  facet_grid(. ~ "Task shifting")

################################################################################################################################################################################################

###########################
# Non-verbal intelligence # 
##########################

# microenvironment : school # 
# outcome : hs_correct_raven #

# model 1 raw - non-verbal intelligence - school exposure to NO2 
model1_ni_school_no2 <- imputed_data %>% 
  with(glm(formula = hs_correct_raven ~ hs_no2_yr_hs_s_log_iqr + h_cohort, 
           family = gaussian)) %>% 
  mice::pool()  

# model 2 raw - non-verbal intelligence - school exposure to PM25
model2_ni_school_pm25 <- imputed_data %>% 
  with(glm(formula = hs_correct_raven ~ hs_pm25_yr_hs_s_iqr + h_cohort,  
           family = gaussian)) %>% 
  mice::pool()

# model 3 adjusted - non-verbal intelligence - school exposure to NO2 
model3_ni_school_no2_adj <- imputed_data %>% 
  with(glm(formula = hs_correct_raven ~ hs_no2_yr_hs_s_log_iqr +
             h_cohort  + e3_ses + 
             h_age + h_edumc + h_mbmi + h_parity +
             h_fage + e3_edufc + e3_fbmic + 
             hs_age_years + e3_sex + h_seabir, 
           family = gaussian)) %>% 
  mice::pool()  

# model 4 adjusted - non-verbal intelligence - school exposure to PM25
model4_ni_school_pm25_adj <- imputed_data %>% 
  with(glm(formula = hs_correct_raven ~ hs_pm25_yr_hs_s_iqr +
             h_cohort  + e3_ses + 
             h_age + h_edumc + h_mbmi + h_parity +
             h_fage + e3_edufc + e3_fbmic + 
             hs_age_years + e3_sex + h_seabir, 
           family = gaussian)) %>% 
  mice::pool()  


# model 5 multi-pollutant - non-verbal intelligence - school exposure to NO2 and PM25
model5_ni_school_multipollutant <- imputed_data %>% 
  with(glm(formula = hs_correct_raven ~ hs_no2_yr_hs_s_log_iqr + hs_pm25_yr_hs_s_iqr +
             h_cohort  + e3_ses + 
             h_age + h_edumc + h_mbmi + h_parity +
             h_fage + e3_edufc + e3_fbmic + 
             hs_age_years + e3_sex + h_seabir, 
           family = gaussian)) %>% 
  mice::pool()  

# summarize models 
model1_ni_school_no2 <- tidy(model1_ni_school_no2, conf.int = TRUE) %>% 
  dotwhisker::relabel_predictors("hs_no2_yr_hs_s_log_iqr" = "NO2") %>% 
  filter(term == "NO2") %>%  
  mutate(model = "NO2",
         outcomes = "Raven score") 

model2_ni_school_pm25 <- tidy(model2_ni_school_pm25, conf.int = TRUE) %>% 
  dotwhisker::relabel_predictors("hs_pm25_yr_hs_s_iqr" = "PM2.5") %>% 
  filter(term == "PM2.5") %>% 
  mutate(model = "PM2.5",
         outcomes = "Raven score") 

model3_ni_school_no2_adj <- tidy(model3_ni_school_no2_adj, conf.int = TRUE) %>% 
  dotwhisker::relabel_predictors("hs_no2_yr_hs_s_log_iqr" = "NO2 + C") %>% 
  filter(term == "NO2 + C") %>% 
  mutate(model = "NO2 + C",
         outcomes = "Raven score")

model4_ni_school_pm25_adj <- tidy(model4_ni_school_pm25_adj, conf.int = TRUE) %>%
  dotwhisker::relabel_predictors("hs_pm25_yr_hs_s_iqr" = "PM2.5 + C") %>% 
  filter(term == "PM2.5 + C") %>% 
  mutate(model = "PM2.5 + C",
         outcomes = "Raven score")

model5_ni_school_no2_multipollutant <- tidy(model5_ni_school_multipollutant, conf.int = TRUE) %>% 
  dotwhisker::relabel_predictors("hs_no2_yr_hs_s_log_iqr" = "NO2 + PM2.5 + C") %>% 
  filter(term == "NO2 + PM2.5 + C") %>% 
  mutate(model = "NO2 + PM2.5 + C",
         outcomes = "Raven score")

model6_ni_school_pm25_multipollutant <- tidy(model5_ni_school_multipollutant, conf.int = TRUE) %>% 
  dotwhisker::relabel_predictors("hs_pm25_yr_hs_s_iqr" = "PM2.5 + NO2 + C") %>%
  filter(term == "PM2.5 + NO2 + C") %>% 
  mutate(model = "PM2.5 + NO2 + C",
         outcomes = "Raven score")

# joining models 
six_models_ni_school <- rbind(model1_ni_school_no2, model2_ni_school_pm25,
                            model3_ni_school_no2_adj, model4_ni_school_pm25_adj,
                            model5_ni_school_no2_multipollutant, model6_ni_school_pm25_multipollutant)


# single pollutant model non-adjusted by cohort
rio::export(six_models_ni_school,"output/tables/models/school/six_models_ni_school.xlsx") # export as xlsx

# single pollutant model adjusted by cohort
rio::export(six_models_ni_school,"output/tables/models/school/six_models_ni_school_adj_cohort.xlsx") # export as xlsx

# forest plot raven score - postnatal exposure school
raven_school_exposure <- dotwhisker::dwplot(six_models_ni_school, dot_args = list(size = 4) ,
                                          vline = geom_vline(xintercept = 0, colour = "black",linetype = 2)) +
  theme_classic(base_size = 13) + theme(legend.position = 'none') + 
  scale_color_manual(values = c("#1F968BFF", # sixth
                                "#1F968BFF", # fifth
                                "#1F968BFF", # fourth
                                "#1F968BFF", # third
                                "#EEAD0E", # second
                                "#EEAD0E")) + # first
  facet_grid(. ~ "Raven score")



# forest plot raven score - postnatal exposure school (single pollutant adjusted + cohort)
raven_school_exposure <- dotwhisker::dwplot(six_models_ni_school, dot_args = list(size = 3) ,
                                            vline = geom_vline(xintercept = 0, colour = "black",linetype = 2)) +
  theme_classic(base_size = 13) + theme(legend.position = 'none') + 
  scale_color_manual(values = c("#1F968BFF", # sixth
                                "#1F968BFF", # fifth
                                "#1F968BFF", # fourth
                                "#1F968BFF", # third
                                "#1F968BFF", # second
                                "#1F968BFF")) + # first
  facet_grid(. ~ "CPM score")


################################################################################################################################################################################################


###########################
### Fine motor function ### 
##########################

# microenvironment : school # 
# exposures : hs_no2_yr_hs_s_log_iqr , hs_pm25_yr_hs_s_iqr
# outcome :  hs_sum_domhand , hs_laterality

######################################
# sum dom hand - finger tapping test #
#####################################

# model 1 raw - finger tapping test (hs_sum_domhand) - school exposure to NO2 
model1_sumdomhand_school_no2 <- imputed_data %>% 
  with(glm(formula = hs_sum_domhand ~ hs_no2_yr_hs_s_log_iqr + h_cohort, 
           family = gaussian)) %>% 
  mice::pool() 

# model 2 raw - finger tapping test (hs_sum_domhand) - school exposure to PM25
model2_sumdomhand_school_pm25 <- imputed_data %>% 
  with(glm(formula = hs_sum_domhand ~ hs_pm25_yr_hs_s_iqr + h_cohort,  
           family = gaussian)) %>% 
  mice::pool() 

# model 3 adjusted - finger tapping test (hs_sum_domhand) - school exposure to NO2 
model3_sumdomhand_school_no2_adj <- imputed_data %>% 
  with(glm(formula = hs_sum_domhand ~ hs_no2_yr_hs_s_log_iqr +
             h_cohort  + e3_ses + 
             h_age + h_edumc + h_mbmi + h_parity +
             h_fage + e3_edufc + e3_fbmic + 
             hs_age_years + e3_sex + h_seabir, 
           family = gaussian)) %>% 
  mice::pool()  

# model 4 adjusted - finger tapping test (hs_sum_domhand) - school exposure to PM25
model4_sumdomhand_school_pm25_adj <- imputed_data %>% 
  with(glm(formula = hs_sum_domhand ~ hs_pm25_yr_hs_s_iqr +
             h_cohort  + e3_ses + 
             h_age + h_edumc + h_mbmi + h_parity +
             h_fage + e3_edufc + e3_fbmic + 
             hs_age_years + e3_sex + h_seabir, 
           family = gaussian)) %>% 
  mice::pool()   


# model 5 multi-pollutant - finger tapping test (hs_sum_domhand) - school exposure to NO2 and PM25
model5_sumdomhand_school_multipollutant <- imputed_data %>% 
  with(glm(formula = hs_sum_domhand ~ hs_no2_yr_hs_s_log_iqr + hs_pm25_yr_hs_s_iqr +
             h_cohort  + e3_ses + 
             h_age + h_edumc + h_mbmi + h_parity +
             h_fage + e3_edufc + e3_fbmic + 
             hs_age_years + e3_sex + h_seabir, 
           family = gaussian)) %>% 
  mice::pool()   

# summarize models 
model1_sumdomhand_school_no2 <- tidy(model1_sumdomhand_school_no2, conf.int = TRUE) %>% 
  dotwhisker::relabel_predictors("hs_no2_yr_hs_s_log_iqr" = "NO2") %>% 
  filter(term == "NO2") %>%  
  mutate(model = "NO2",
         outcomes = "Sum trials dom hand") 

model2_sumdomhand_school_pm25 <- tidy(model2_sumdomhand_school_pm25, conf.int = TRUE) %>% 
  dotwhisker::relabel_predictors("hs_pm25_yr_hs_s_iqr" = "PM2.5") %>% 
  filter(term == "PM2.5") %>% 
  mutate(model = "PM2.5",
         outcomes = "Sum trials dom hand") 

model3_sumdomhand_school_no2_adj <- tidy(model3_sumdomhand_school_no2_adj, conf.int = TRUE) %>% 
  dotwhisker::relabel_predictors("hs_no2_yr_hs_s_log_iqr" = "NO2 + C") %>% 
  filter(term == "NO2 + C") %>% 
  mutate(model = "NO2 + C",
         outcomes = "Sum trials dom hand")

model4_sumdomhand_school_pm25_adj <- tidy(model4_sumdomhand_school_pm25_adj, conf.int = TRUE) %>%
  dotwhisker::relabel_predictors("hs_pm25_yr_hs_s_iqr" = "PM2.5 + C") %>% 
  filter(term == "PM2.5 + C") %>% 
  mutate(model = "PM2.5 + C",
         outcomes = "Sum trials dom hand")

model5_sumdomhand_school_no2_multipollutant <- tidy(model5_sumdomhand_school_multipollutant, conf.int = TRUE) %>% 
  dotwhisker::relabel_predictors("hs_no2_yr_hs_s_log_iqr" = "NO2 + PM2.5 + C") %>% 
  filter(term == "NO2 + PM2.5 + C") %>% 
  mutate(model = "NO2 + PM2.5 + C",
         outcomes = "Sum trials dom hand")

model6_sumdomhand_school_pm25_multipollutant <- tidy(model5_sumdomhand_school_multipollutant, conf.int = TRUE) %>% 
  dotwhisker::relabel_predictors("hs_pm25_yr_hs_s_iqr" = "PM2.5 + NO2 + C") %>%
  filter(term == "PM2.5 + NO2 + C") %>% 
  mutate(model = "PM2.5 + NO2 + C",
         outcomes = "Sum trials dom hand")


# joining models 
six_models_sumdomhand_school <- rbind(model1_sumdomhand_school_no2, model2_sumdomhand_school_pm25,
                                    model3_sumdomhand_school_no2_adj, model4_sumdomhand_school_pm25_adj,
                                    model5_sumdomhand_school_no2_multipollutant, model6_sumdomhand_school_pm25_multipollutant)

# single pollutant model non-adjusted by cohort 
rio::export(six_models_sumdomhand_school,"output/tables/models/school/six_models_sumdomhand_school.xlsx") # export as xlsx

# single pollutant model adjusted by cohort
rio::export(six_models_sumdomhand_school,"output/tables/models/school/six_models_sumdomhand_school_adj_cohort.xlsx") # export as xlsx

# forest plot sum dom hand - postnatal exposure 
sumdomhand_school_exposure <- dotwhisker::dwplot(six_models_sumdomhand_school, dot_args = list(size = 4) ,
                                               vline = geom_vline(xintercept = 0, colour = "black",linetype = 2)) +
  theme_classic(base_size = 13) + theme(legend.position = 'none') + 
  scale_color_manual(values = c("#1F968BFF", # sixth
                                "#1F968BFF", # fifth
                                "#1F968BFF", # fourth
                                "#1F968BFF", # third
                                "#EEAD0E", # second
                                "#1F968BFF"))  + # first
  facet_grid(. ~ "Sum trial dom hand")


# forest plot sum dom hand - postnatal exposure school (single pollutant adjusted + cohort)
sumdomhand_school_exposure <- dotwhisker::dwplot(six_models_sumdomhand_school, dot_args = list(size = 3) ,
                                                 vline = geom_vline(xintercept = 0, colour = "black",linetype = 2)) +
  theme_classic(base_size = 13) + theme(legend.position = 'none') + 
  scale_color_manual(values = c("#1F968BFF", # sixth
                                "#1F968BFF", # fifth
                                "#1F968BFF", # fourth
                                "#1F968BFF", # third
                                "#1F968BFF", # second
                                "#1F968BFF"))  + # first
  facet_grid(. ~ "Sum trial dom hand")

##########################################
# Laterality index - finger tapping test #
#########################################

# model 1 raw - finger tapping test (hs_laterality) - school exposure to NO2 
model1_laterality_school_no2 <- imputed_data %>% 
  with(glm(formula = hs_laterality ~ hs_no2_yr_hs_s_log_iqr, 
           family = gaussian)) %>% 
  mice::pool() 

# model 2 raw - finger tapping test ( hs_laterality) - school exposure to PM25
model2_laterality_school_pm25 <- imputed_data %>% 
  with(glm(formula = hs_laterality ~ hs_pm25_yr_hs_s_iqr ,  
           family = gaussian)) %>% 
  mice::pool() 

# model 3 adjusted - finger tapping test (hs_laterality) - school exposure to NO2 
model3_laterality_school_no2_adj <- imputed_data %>% 
  with(glm(formula = hs_laterality ~ hs_no2_yr_hs_s_log_iqr +
             h_cohort  + e3_ses + 
             h_age + h_edumc + h_mbmi + h_parity +
             h_fage + e3_edufc + e3_fbmic + 
             hs_age_years + e3_sex + h_seabir, 
           family = gaussian)) %>% 
  mice::pool()  

# model 4 adjusted - finger tapping test (hs_laterality) - school exposure to PM25
model4_laterality_school_pm25_adj <- imputed_data %>% 
  with(glm(formula = hs_laterality ~ hs_pm25_yr_hs_s_iqr +
             h_cohort  + e3_ses + 
             h_age + h_edumc + h_mbmi + h_parity +
             h_fage + e3_edufc + e3_fbmic + 
             hs_age_years + e3_sex + h_seabir, 
           family = gaussian)) %>% 
  mice::pool()   


# model 5 multi-pollutant - finger tapping test ( hs_laterality) - school exposure to NO2 and PM25
model5_laterality_school_multipollutant <- imputed_data %>% 
  with(glm(formula = hs_laterality ~ hs_no2_yr_hs_s_log_iqr + hs_pm25_yr_hs_s_iqr +
             h_cohort  + e3_ses + 
             h_age + h_edumc + h_mbmi + h_parity +
             h_fage + e3_edufc + e3_fbmic + 
             hs_age_years + e3_sex + h_seabir, 
           family = gaussian)) %>% 
  mice::pool()   

# summarize models 
model1_laterality_school_no2 <- tidy(model1_laterality_school_no2, conf.int = TRUE) %>% 
  dotwhisker::relabel_predictors("hs_no2_yr_hs_s_log_iqr" = "NO2") %>% 
  filter(term == "NO2") %>%  
  mutate(model = "NO2",
         outcomes = "Laterality index") 

model2_laterality_school_pm25 <- tidy(model2_laterality_school_pm25, conf.int = TRUE) %>% 
  dotwhisker::relabel_predictors("hs_pm25_yr_hs_s_iqr" = "PM2.5") %>% 
  filter(term == "PM2.5") %>% 
  mutate(model = "PM2.5",
         outcomes = "Laterality index") 

model3_laterality_school_no2_adj <- tidy(model3_laterality_school_no2_adj, conf.int = TRUE) %>% 
  dotwhisker::relabel_predictors("hs_no2_yr_hs_s_log_iqr" = "NO2 + C") %>% 
  filter(term == "NO2 + C") %>% 
  mutate(model = "NO2 + C",
         outcomes = "Laterality index")

model4_laterality_school_pm25_adj <- tidy(model4_laterality_school_pm25_adj, conf.int = TRUE) %>%
  dotwhisker::relabel_predictors("hs_pm25_yr_hs_s_iqr" = "PM2.5 + C") %>% 
  filter(term == "PM2.5 + C") %>% 
  mutate(model = "PM2.5 + C",
         outcomes = "Laterality index")

model5_laterality_school_no2_multipollutant <- tidy(model5_laterality_school_multipollutant, conf.int = TRUE) %>% 
  dotwhisker::relabel_predictors("hs_no2_yr_hs_s_log_iqr" = "NO2 + PM2.5 + C") %>% 
  filter(term == "NO2 + PM2.5 + C") %>% 
  mutate(model = "NO2 + PM2.5 + C",
         outcomes = "Laterality index")

model6_laterality_school_pm25_multipollutant <- tidy(model5_laterality_school_multipollutant, conf.int = TRUE) %>% 
  dotwhisker::relabel_predictors("hs_pm25_yr_hs_s_iqr" = "PM2.5 + NO2 + C") %>%
  filter(term == "PM2.5 + NO2 + C") %>% 
  mutate(model = "PM2.5 + NO2 + C",
         outcomes = "Laterality index")

# joining models 
six_models_laterality_school <- rbind(model1_laterality_school_no2, model2_laterality_school_pm25,
                                    model3_laterality_school_no2_adj, model4_laterality_school_pm25_adj,
                                    model5_laterality_school_no2_multipollutant, model6_laterality_school_pm25_multipollutant)



# forest plot laterality index - postnatal exposure school
laterality_school_exposure <- dotwhisker::dwplot(six_models_laterality_school, dot_args = list(size = 4) ,
                                               vline = geom_vline(xintercept = 0, colour = "black",linetype = 2)) +
  theme_classic(base_size = 13) + theme(legend.position = 'none') + 
  scale_color_manual(values = c("#1F968BFF", # sixth
                                "#1F968BFF", # fifth
                                "#1F968BFF", # fourth
                                "#1F968BFF", # third
                                "#1F968BFF", # second
                                "#1F968BFF")) + # first
  facet_grid(. ~ "Laterality index")


################################################################################################################################################################################################

##################################
### final figure school exposure ###
#################################

library(patchwork)
install.packages("ggtext")
library(ggtext)

# forest plot attentional function (ANT)
attentional_function_school_exposure <- attentional_function_school_exposure + 
  facet_grid(. ~ "Hit react time (se)") +
  scale_x_continuous(limits = c(-25, 25),
                     breaks = c(-25, 0, 25))

# forest plot task switching score (TMT)
task_switching_school_exposure <- task_switching_school_exposure +
  facet_grid(. ~ "Task switching score") +
  theme(axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        axis.line.y = element_blank()) +
  scale_x_continuous(limits = c(-16000, 16000),
                     breaks = c(-15000, 0, 15000))

# forest plot task shifting score (TMT)
task_shifting_school_exposure <- task_shifting_school_exposure +
  facet_grid(. ~ "Task shifting score") +
  theme(axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        axis.line.y = element_blank()) +
  scale_x_continuous(limits = c(-0.20, 0.20),
                     breaks = c(-0.20, 0, 0.20))

# forest plot raven score (CMPC Raven)
raven_school_exposure <- raven_school_exposure +
  facet_grid(. ~ "CPM score") + 
  theme(axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        axis.line.y = element_blank()) + 
  scale_x_continuous(limits = c(-2, 2),
                     breaks = c(-2, 0, 2))


# forest plot sum trial dom hand (Finger tapping test)
sumdomhand_school_exposure <- sumdomhand_school_exposure +
  facet_grid(. ~ "Sum trial dom hand") + 
  theme(axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        axis.line.y = element_blank()) +
  scale_x_continuous(limits = c(-8, 8),
                     breaks = c(-8, 0, 8))

# forest plot laterality (Finger tapping test)
laterality_school_exposure <- laterality_school_exposure + 
  facet_grid(. ~ "Laterality index") + 
  theme(axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        axis.line.y = element_blank()) + 
  scale_x_continuous(limits = c(-2, 2),
                     breaks = c(-2, -1, 0, 1, 2))

# we need to use | instead of + to obtain an image of one row and six column 
school_forest_plot <- (attentional_function_school_exposure | task_switching_school_exposure | task_shifting_school_exposure | raven_school_exposure | sumdomhand_school_exposure)

# the final forest plot 
school_forest_plot <- school_forest_plot + 
  plot_annotation(title = 'B. Exposure to air pollution at school',
                  subtitle = expression("Effect estimate of air pollution at school (" *beta * "" [AP-school] * " (95 % CI))")) &
  theme(plot.title = element_text(face = 'bold', size = 16),
        plot.subtitle = element_text(hjust = 0.5, size = 14),
        strip.text.x = element_text(size = 12))

ggsave("output/figures/fig2B_school_forest_plot_v3.tiff", width = 12, height = 3, dpi = 400) 

# single pollutant model sadjusted by cohort
ggsave("output/figures/fig2B_school_forest_plot_adj_cohort.tiff", width = 12, height = 3, dpi = 400) 


















































































