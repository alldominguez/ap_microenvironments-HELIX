# 11.- models_home_exposure

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

# (Nitrogen dioxide) : hs_no2_yr_hs_h (at home) 

# (Particulate matter 2,5) : hs_pm25_yr_hs_h (at home) 

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

# microenvironment : home # 
# outcome : hs_hitrtse #

#################################### run the different models #################################

# model 1 raw - attentional function - home exposure to NO2 
model1_af_home_no2 <- imputed_data %>% 
  with(glm(formula = hs_hitrtse ~ hs_no2_yr_hs_h_log_iqr + h_cohort, 
           family = gaussian)) %>% 
  mice::pool() 


# model 2 raw - attentional function - home exposure to PM25
model2_af_home_pm25 <- imputed_data %>% 
  with(glm(formula = hs_hitrtse ~ hs_pm25_yr_hs_h_iqr + h_cohort,  
           family = gaussian)) %>% 
  mice::pool() 

# model 3 adjusted - attentional function - home exposure to NO2 
model3_af_home_no2_adj <- imputed_data %>% 
  with(glm(formula = hs_hitrtse ~ hs_no2_yr_hs_h_log_iqr +
             h_cohort  + e3_ses + 
             h_age + h_edumc + h_mbmi + h_parity +
             h_fage + e3_edufc + e3_fbmic + 
             hs_age_years + e3_sex + h_seabir, 
           family = gaussian)) %>% 
  mice::pool()  



# model 4 adjusted - attentional function - home exposure to PM25
model4_af_home_pm25_adj <- imputed_data %>% 
  with(glm(formula = hs_hitrtse ~ hs_pm25_yr_hs_h_iqr +
             h_cohort  + e3_ses + 
             h_age + h_edumc + h_mbmi + h_parity +
             h_fage + e3_edufc + e3_fbmic + 
             hs_age_years + e3_sex + h_seabir, 
           family = gaussian)) %>% 
  mice::pool() 


# model 5 multi-pollutant - attentional function - home exposure to NO2 and PM25
model5_af_home_multipollutant <- imputed_data %>% 
  with(glm(formula = hs_hitrtse ~ hs_no2_yr_hs_h_log_iqr + hs_pm25_yr_hs_h_iqr +
             h_cohort  + e3_ses + 
             h_age + h_edumc + h_mbmi + h_parity +
             h_fage + e3_edufc + e3_fbmic + 
             hs_age_years + e3_sex + h_seabir, 
           family = gaussian)) %>% 
  mice::pool()  



########################### create labels for the plot each of the models #########################


# summarize models 
model1_af_home_no2 <- tidy(model1_af_home_no2, conf.int = TRUE) %>% 
  dotwhisker::relabel_predictors("hs_no2_yr_hs_h_log_iqr" = "NO2") %>% 
  filter(term == "NO2") %>% 
  mutate(model = "NO2",
         outcomes = "Hit react time (se)") 

model2_af_home_pm25 <- tidy(model2_af_home_pm25, conf.int = TRUE) %>% 
  dotwhisker::relabel_predictors("hs_pm25_yr_hs_h_iqr" = "PM2.5") %>% 
  filter(term == "PM2.5") %>% 
  mutate(model = "PM2.5",
         outcomes = "Hit react time (se)") 

model3_af_home_no2_adj <- tidy(model3_af_home_no2_adj, conf.int = TRUE) %>% 
  dotwhisker::relabel_predictors("hs_no2_yr_hs_h_log_iqr" = "NO2 + C") %>% 
  filter(term == "NO2 + C") %>% 
  mutate(model = "NO2 + C",
         outcomes = "Hit react time (se)")

model4_af_home_pm25_adj <- tidy(model4_af_home_pm25_adj, conf.int = TRUE) %>% 
  dotwhisker::relabel_predictors("hs_pm25_yr_hs_h_iqr" = "PM2.5 + C") %>% 
  filter(term == "PM2.5 + C") %>%  
  mutate(model = "PM2.5 + C",
         outcomes = "Hit react time (se)")

model5_af_home_no2_multipollutant <- tidy(model5_af_home_multipollutant, conf.int = TRUE) %>% 
  dotwhisker::relabel_predictors("hs_no2_yr_hs_h_log_iqr" = "NO2 + PM2.5 + C") %>% 
  filter(term == "NO2 + PM2.5 + C") %>% 
  mutate(model = "NO2 + PM2.5 + C",
         outcomes = "Hit react time (se)")

model6_af_home_pm25_multipollutant <- tidy(model5_af_home_multipollutant, conf.int = TRUE) %>% 
  dotwhisker::relabel_predictors("hs_pm25_yr_hs_h_iqr" = "PM2.5 + NO2 + C") %>%
  filter(term == "PM2.5 + NO2 + C") %>%
  mutate(model = "PM2.5 + NO2 + C",
         outcomes = "Hit react time (se)")



#################### we join all the objects (models) on one dataframe #############


six_models_af_home <- rbind(model1_af_home_no2, model2_af_home_pm25,
                        model3_af_home_no2_adj, model4_af_home_pm25_adj,
                        model5_af_home_no2_multipollutant, model6_af_home_pm25_multipollutant) %>% 
                       mutate(across(where(is.numeric), round, 2))   # to put this into a table
                       

# single pollutant non-adjusted by cohort
rio::export(six_models_af_home,"output/tables/models/home/six_models_af_home.xlsx") # export as xlsx

# single pollutant adjusted by cohort
rio::export(six_models_af_home,"output/tables/models/home/six_models_af_home_adj_cohort.xlsx") # export as xlsx


# forest plot attentional function - postnatal exposure home 
attentional_function_home_exposure <- dotwhisker::dwplot(six_models_af_home, dot_args = list(size = 4) ,
                   vline = geom_vline(xintercept = 0, colour = "black",linetype = 2))  +
  theme_classic(base_size = 13) + theme(legend.position = 'none') + 
  scale_color_manual(values = c("#440154FF", # sixth
                                "#440154FF", # fifth
                                "#440154FF", # fourth
                                "#440154FF", # third
                                "#EEAD0E", # second
                                "#EEAD0E"))  + # first
  facet_grid(. ~ "Hit reac time (se)")

# forest plot attentional function - postnatal exposure home (single pollutant model adjusted + cohort)
attentional_function_home_exposure <- dotwhisker::dwplot(six_models_af_home, dot_args = list(size = 3) ,
                                                         vline = geom_vline(xintercept = 0, colour = "black",linetype = 2))  +
  theme_classic(base_size = 13) + theme(legend.position = 'none') + 
  scale_color_manual(values = c("#440154FF", # sixth
                                "#440154FF", # fifth
                                "#440154FF", # fourth
                                "#440154FF", # third
                                "#440154FF", # second
                                "#440154FF"))  + # first
  facet_grid(. ~ "Hit reac time (se)")



################################################################################################################################################################################################

###########################
# Cognitive flexibility # 
##########################

# microenvironment : home # 
# outcome : hs_tmtb_responsetime (task switching) 

############################
### task switching score ### 
###########################

# model 1 raw - task switching - home exposure to NO2 
model1_task_switching_home_no2 <- imputed_data %>% 
  with(glm(formula = hs_tmtb_responsetime ~ hs_no2_yr_hs_h_log_iqr + h_cohort, 
           family = gaussian)) %>% 
  mice::pool() 


# model 2 raw - task switching - home exposure to PM25
model2_task_switching_home_pm25 <- imputed_data %>% 
  with(glm(formula = hs_tmtb_responsetime ~ hs_pm25_yr_hs_h_iqr + h_cohort,  
           family = gaussian)) %>% 
  mice::pool() 

# model 3 adjusted - task switching - home exposure to NO2 
model3_task_switching_home_no2_adj <- imputed_data %>% 
  with(glm(formula = hs_tmtb_responsetime ~ hs_no2_yr_hs_h_log_iqr +
             h_cohort  + e3_ses + 
             h_age + h_edumc + h_mbmi + h_parity +
             h_fage + e3_edufc + e3_fbmic + 
             hs_age_years + e3_sex + h_seabir, 
           family = gaussian)) %>% 
  mice::pool()  

# model 4 adjusted - task switching - home exposure to PM25
model4_task_switching_home_pm25_adj <- imputed_data %>% 
  with(glm(formula = hs_tmtb_responsetime ~ hs_pm25_yr_hs_h_iqr +
             h_cohort  + e3_ses + 
             h_age + h_edumc + h_mbmi + h_parity +
             h_fage + e3_edufc + e3_fbmic + 
             hs_age_years + e3_sex + h_seabir, 
           family = gaussian)) %>% 
  mice::pool() 



# model 5 multi-pollutant - task swithching - home exposure to NO2 and PM25
model5_task_switching_home_multipollutant <- imputed_data %>% 
  with(glm(formula = hs_tmtb_responsetime ~ hs_no2_yr_hs_h_log_iqr + hs_pm25_yr_hs_h_iqr +
             h_cohort  + e3_ses + 
             h_age + h_edumc + h_mbmi + h_parity +
             h_fage + e3_edufc + e3_fbmic + 
             hs_age_years + e3_sex + h_seabir, 
           family = gaussian)) %>% 
  mice::pool()  

# summarize models 
model1_task_switching_home_no2 <- tidy(model1_task_switching_home_no2, conf.int = TRUE) %>% 
  dotwhisker::relabel_predictors("hs_no2_yr_hs_h_log_iqr" = "NO2") %>% 
  filter(term == "NO2") %>% 
  mutate(model = "NO2",
         outcomes = "Task switching score") 

model2_task_switching_home_pm25 <- tidy(model2_task_switching_home_pm25, conf.int = TRUE) %>% 
  dotwhisker::relabel_predictors("hs_pm25_yr_hs_h_iqr" = "PM2.5") %>% 
  filter(term == "PM2.5") %>% 
  mutate(model = "PM2.5",
         outcomes = "Task switching score") 

model3_task_switching_home_no2_adj <- tidy(model3_task_switching_home_no2_adj, conf.int = TRUE) %>% 
  dotwhisker::relabel_predictors("hs_no2_yr_hs_h_log_iqr" = "NO2 + C") %>% 
  filter(term == "NO2 + C") %>% 
  mutate(model = "NO2 + C",
         outcomes = "Task switching score") 

model4_task_switching_home_pm25_adj <- tidy(model4_task_switching_home_pm25_adj, conf.int = TRUE) %>% 
  dotwhisker::relabel_predictors("hs_pm25_yr_hs_h_iqr" = "PM2.5 + C") %>% 
  filter(term == "PM2.5 + C") %>%  
  mutate(model = "PM2.5 + C",
         outcomes = "Task switching score") 

model5_task_switching_home_no2_multipollutant <- tidy(model5_task_switching_home_multipollutant, conf.int = TRUE) %>% 
  dotwhisker::relabel_predictors("hs_no2_yr_hs_h_log_iqr" = "NO2 + PM2.5 + C") %>% 
  filter(term == "NO2 + PM2.5 + C") %>% 
  mutate(model = "NO2 + PM2.5 + C",
         outcomes = "Task switching score") 

model6_task_switching_home_pm25_multipollutant <- tidy(model5_task_switching_home_multipollutant, conf.int = TRUE)  %>% 
  dotwhisker::relabel_predictors("hs_pm25_yr_hs_h_iqr" = "PM2.5 + NO2 + C") %>%
  filter(term == "PM2.5 + NO2 + C") %>%
  mutate(model = "PM2.5 + NO2 + C",
         outcomes = "Task switching score") 


# join the plots 
six_models_task_switching_home <- rbind(model1_task_switching_home_no2, model2_task_switching_home_pm25,
                       model3_task_switching_home_no2_adj, model4_task_switching_home_pm25_adj,
                       model5_task_switching_home_no2_multipollutant, model6_task_switching_home_pm25_multipollutant)  %>% 
  mutate(across(where(is.numeric), round, 2)) # to put this into a table

# single pollutant non-adjusted by cohort
rio::export(six_models_task_switching_home,"output/tables/models/home/six_models_task_switching_home.xlsx") # export as xlsx

# single pollutant adjusted by cohort
rio::export(six_models_task_switching_home,"output/tables/models/home/six_models_task_switching_home_adj_cohort.xlsx") # export as xlsx



# forest plot task switching score - postnatal exposure home
task_switching_home_exposure <- dotwhisker::dwplot(six_models_task_switching_home, dot_args = list(size = 4) ,
                                       vline = geom_vline(xintercept = 0, colour = "black",linetype = 2))  +
  theme_classic(base_size = 13) + theme(legend.position = 'none') + 
  scale_color_manual(values = c("#440154FF", # sixth
                                "#440154FF", # fifth
                                "#440154FF", # fourth
                                "#440154FF", # third
                                "#EEAD0E", # second
                                "#EEAD0E")) + # first
  facet_grid(. ~ "Task switching score")

# forest plot task switching score - postnatal exposure home (single pollutant model adjusted + cohort)
task_switching_home_exposure <- dotwhisker::dwplot(six_models_task_switching_home, dot_args = list(size = 3) ,
                                                   vline = geom_vline(xintercept = 0, colour = "black",linetype = 2))  +
  theme_classic(base_size = 13) + theme(legend.position = 'none') + 
  scale_color_manual(values = c("#440154FF", # sixth
                                "#440154FF", # fifth
                                "#440154FF", # fourth
                                "#440154FF", # third
                                "#440154FF", # second
                                "#440154FF")) + # first
  facet_grid(. ~ "Task switching score")

################################################################################################################################################################################################


############################
### task shifting score ### 
###########################

# outcome : task_shifting_score 
# microenvironment : home 

# model 1 raw - task shifting - home exposure to NO2 
model1_task_shifting_home_no2 <- imputed_data %>% 
  with(glm(formula = task_shifting_score  ~ hs_no2_yr_hs_h_log_iqr + h_cohort, 
           family = gaussian)) %>% 
  mice::pool() 


# model 2 raw -  task shifting - home exposure to PM25
model2_task_shifting_home_pm25 <- imputed_data %>% 
  with(glm(formula = task_shifting_score  ~ hs_pm25_yr_hs_h_iqr + h_cohort ,  
           family = gaussian)) %>% 
  mice::pool() 

# model 3 adjusted -  task shifting - home exposure to NO2 
model3_task_shifting_home_no2_adj <- imputed_data %>% 
  with(glm(formula = task_shifting_score  ~ hs_no2_yr_hs_h_log_iqr +
             h_cohort  + e3_ses + 
             h_age + h_edumc + h_mbmi + h_parity +
             h_fage + e3_edufc + e3_fbmic + 
             hs_age_years + e3_sex + h_seabir, 
           family = gaussian)) %>% 
  mice::pool()  

# model 4 adjusted -  task shifting - home exposure to PM25
model4_task_shifting_home_pm25_adj <- imputed_data %>% 
  with(glm(formula = task_shifting_score  ~ hs_pm25_yr_hs_h_iqr +
             h_cohort  + e3_ses + 
             h_age + h_edumc + h_mbmi + h_parity +
             h_fage + e3_edufc + e3_fbmic + 
             hs_age_years + e3_sex + h_seabir, 
           family = gaussian)) %>% 
  mice::pool() 



# model 5 multi-pollutant -  task shifting - home exposure to NO2 and PM25
model5_task_shifting_home_multipollutant <- imputed_data %>% 
  with(glm(formula = task_shifting_score  ~ hs_no2_yr_hs_h_log_iqr + hs_pm25_yr_hs_h_iqr +
             h_cohort  + e3_ses + 
             h_age + h_edumc + h_mbmi + h_parity +
             h_fage + e3_edufc + e3_fbmic + 
             hs_age_years + e3_sex + h_seabir, 
           family = gaussian)) %>% 
  mice::pool()  

# summarize models 
model1_task_shifting_home_no2 <- tidy(model1_task_shifting_home_no2, conf.int = TRUE) %>% 
  dotwhisker::relabel_predictors("hs_no2_yr_hs_h_log_iqr" = "NO2") %>% 
  filter(term == "NO2") %>% 
  mutate(model = "NO2",
         outcomes = "Task shifting score") 

model2_task_shifting_home_pm25 <- tidy(model2_task_shifting_home_pm25, conf.int = TRUE) %>% 
  dotwhisker::relabel_predictors("hs_pm25_yr_hs_h_iqr" = "PM2.5") %>% 
  filter(term == "PM2.5") %>% 
  mutate(model = "PM2.5",
         outcomes = "Task shifting score") 

model3_task_shifting_home_no2_adj <- tidy(model3_task_shifting_home_no2_adj, conf.int = TRUE) %>% 
  dotwhisker::relabel_predictors("hs_no2_yr_hs_h_log_iqr" = "NO2 + C") %>% 
  filter(term == "NO2 + C") %>% 
  mutate(model = "NO2 + C",
         outcomes = "Task shifting score") 

model4_task_shifting_home_pm25_adj <- tidy(model4_task_shifting_home_pm25_adj, conf.int = TRUE) %>% 
  dotwhisker::relabel_predictors("hs_pm25_yr_hs_h_iqr" = "PM2.5 + C") %>% 
  filter(term == "PM2.5 + C") %>%  
  mutate(model = "PM2.5 + C",
         outcomes = "Task shifting score") 

model5_task_shifting_home_no2_multipollutant <- tidy(model5_task_shifting_home_multipollutant, conf.int = TRUE) %>% 
  dotwhisker::relabel_predictors("hs_no2_yr_hs_h_log_iqr" = "NO2 + PM2.5 + C") %>% 
  filter(term == "NO2 + PM2.5 + C") %>% 
  mutate(model = "NO2 + PM2.5 + C",
         outcomes = "Task shifting score") 

model6_task_shifting_home_pm25_multipollutant <- tidy(model5_task_shifting_home_multipollutant, conf.int = TRUE)  %>% 
  dotwhisker::relabel_predictors("hs_pm25_yr_hs_h_iqr" = "PM2.5 + NO2 + C") %>%
  filter(term == "PM2.5 + NO2 + C") %>%
  mutate(model = "PM2.5 + NO2 + C",
         outcomes = "Task shifting score") 


# join the plots 
six_models_task_shifting_home <- rbind(model1_task_shifting_home_no2, model2_task_shifting_home_pm25,
                                        model3_task_shifting_home_no2_adj, model4_task_shifting_home_pm25_adj,
                                        model5_task_shifting_home_no2_multipollutant, model6_task_shifting_home_pm25_multipollutant)  %>% 
  mutate(across(where(is.numeric), round, 2)) # to put this into a table

# single pollutant model non-adjusted by cohort
rio::export(six_models_task_shifting_home,"output/tables/models/home/six_models_task_shifting_home.xlsx") # export as xlsx

# single pollutant model adjusted by cohort
rio::export(six_models_task_shifting_home,"output/tables/models/home/six_models_task_shifting_home_adj_cohort.xlsx") # export as xlsx


# forest plot task switching score - postnatal exposure home
task_shifting_home_exposure <- dotwhisker::dwplot(six_models_task_shifting_home, dot_args = list(size = 4) ,
                                                   vline = geom_vline(xintercept = 0, colour = "black",linetype = 2))  +
  theme_classic(base_size = 13) + theme(legend.position = 'none') + 
  scale_color_manual(values = c("#440154FF", # sixth
                                "#440154FF", # fifth
                                "#440154FF", # fourth
                                "#440154FF", # third
                                "#EEAD0E", # second
                                "#440154FF")) + # first
  facet_grid(. ~ "Task shifting score")

# forest plot task switching score - postnatal exposure home (single pollutant adjusted + cohort)
task_shifting_home_exposure <- dotwhisker::dwplot(six_models_task_shifting_home, dot_args = list(size = 3) ,
                                                  vline = geom_vline(xintercept = 0, colour = "black",linetype = 2))  +
  theme_classic(base_size = 13) + theme(legend.position = 'none') + 
  scale_color_manual(values = c("#440154FF", # sixth
                                "#440154FF", # fifth
                                "#440154FF", # fourth
                                "#440154FF", # third
                                "#440154FF", # second
                                "#440154FF")) + # first
  facet_grid(. ~ "Task shifting score")





################################################################################################################################################################################################

###########################
# Non-verbal intelligence # 
##########################

# microenvironment : home # 
# outcome : hs_correct_raven #

# model 1 raw - non-verbal intelligence - home exposure to NO2 
model1_ni_home_no2 <- imputed_data %>% 
  with(glm(formula = hs_correct_raven ~ hs_no2_yr_hs_h_log_iqr + h_cohort, 
           family = gaussian)) %>% 
  mice::pool()  

# model 2 raw - non-verbal intelligence - home exposure to PM25
model2_ni_home_pm25 <- imputed_data %>% 
  with(glm(formula = hs_correct_raven ~ hs_pm25_yr_hs_h_iqr + h_cohort ,  
           family = gaussian)) %>% 
  mice::pool()

# model 3 adjusted - non-verbal intelligence - home exposure to NO2 
model3_ni_home_no2_adj <- imputed_data %>% 
  with(glm(formula = hs_correct_raven ~ hs_no2_yr_hs_h_log_iqr +
             h_cohort  + e3_ses + 
             h_age + h_edumc + h_mbmi + h_parity +
             h_fage + e3_edufc + e3_fbmic + 
             hs_age_years + e3_sex + h_seabir, 
           family = gaussian)) %>% 
  mice::pool()  

# model 4 adjusted - non-verbal intelligence - home exposure to PM25
model4_ni_home_pm25_adj <- imputed_data %>% 
  with(glm(formula = hs_correct_raven ~ hs_pm25_yr_hs_h_iqr +
             h_cohort  + FAS_cat + 
             h_age + h_edumc + h_mbmi + h_parity +
             h_fage + e3_edufc + e3_fbmic + 
             hs_age_years + e3_sex + h_seabir, 
           family = gaussian)) %>% 
  mice::pool()  


# model 5 multi-pollutant - non-verbal intelligence - home exposure to NO2 and PM25
model5_ni_home_multipollutant <- imputed_data %>% 
  with(glm(formula = hs_correct_raven ~ hs_no2_yr_hs_h_log_iqr + hs_pm25_yr_hs_h_iqr +
             h_cohort  + e3_ses + 
             h_age + h_edumc + h_mbmi + h_parity +
             h_fage + e3_edufc + e3_fbmic + 
             hs_age_years + e3_sex + h_seabir, 
           family = gaussian)) %>% 
  mice::pool()  

# summarize models 
model1_ni_home_no2 <- tidy(model1_ni_home_no2, conf.int = TRUE) %>% 
  dotwhisker::relabel_predictors("hs_no2_yr_hs_h_log_iqr" = "NO2") %>% 
  filter(term == "NO2") %>%  
  mutate(model = "NO2",
         outcomes = "Raven score") 

model2_ni_home_pm25 <- tidy(model2_ni_home_pm25, conf.int = TRUE) %>% 
  dotwhisker::relabel_predictors("hs_pm25_yr_hs_h_iqr" = "PM2.5") %>% 
  filter(term == "PM2.5") %>% 
  mutate(model = "PM2.5",
         outcomes = "Raven score") 

model3_ni_home_no2_adj <- tidy(model3_ni_home_no2_adj, conf.int = TRUE) %>% 
  dotwhisker::relabel_predictors("hs_no2_yr_hs_h_log_iqr" = "NO2 + C") %>% 
  filter(term == "NO2 + C") %>% 
  mutate(model = "NO2 + C",
         outcomes = "Raven score")

model4_ni_home_pm25_adj <- tidy(model4_ni_home_pm25_adj, conf.int = TRUE) %>%
  dotwhisker::relabel_predictors("hs_pm25_yr_hs_h_iqr" = "PM2.5 + C") %>% 
  filter(term == "PM2.5 + C") %>% 
  mutate(model = "PM2.5 + C",
         outcomes = "Raven score")

model5_ni_home_no2_multipollutant <- tidy(model5_ni_home_multipollutant, conf.int = TRUE) %>% 
  dotwhisker::relabel_predictors("hs_no2_yr_hs_h_log_iqr" = "NO2 + PM2.5 + C") %>% 
  filter(term == "NO2 + PM2.5 + C") %>% 
  mutate(model = "NO2 + PM2.5 + C",
         outcomes = "Raven score")

model6_ni_home_pm25_multipollutant <- tidy(model5_ni_home_multipollutant, conf.int = TRUE) %>% 
  dotwhisker::relabel_predictors("hs_pm25_yr_hs_h_iqr" = "PM2.5 + NO2 + C") %>%
  filter(term == "PM2.5 + NO2 + C") %>% 
  mutate(model = "PM2.5 + NO2 + C",
         outcomes = "Raven score")

# joining models 
six_models_ni_home <- rbind(model1_ni_home_no2, model2_ni_home_pm25,
                       model3_ni_home_no2_adj, model4_ni_home_pm25_adj,
                       model5_ni_home_no2_multipollutant, model6_ni_home_pm25_multipollutant) %>% 
  mutate(across(where(is.numeric), round, 2)) # to put this into a table

# single pollutant non-adjusted by cohort
rio::export(six_models_ni_home,"output/tables/models/home/six_models_ni_home.xlsx") # export as xlsx

# single pollutant adjusted by cohort
rio::export(six_models_ni_home,"output/tables/models/home/six_models_ni_home_adj_cohort.xlsx") # export as xlsx


# forest plot raven score - postnatal exposure home
raven_home_exposure <- dotwhisker::dwplot(six_models_ni_home, dot_args = list(size = 4) ,
                                       vline = geom_vline(xintercept = 0, colour = "black",linetype = 2)) +
  theme_classic(base_size = 13) + theme(legend.position = 'none') + 
  scale_color_manual(values = c("#440154FF", # sixth
                                "#440154FF", # fifth
                                "#440154FF", # fourth
                                "#440154FF", # third
                                "#EEAD0E", # second
                                "#EEAD0E")) + # first
  facet_grid(. ~ "CPM score")

# forest plot raven score - postnatal exposure home (single pollutant adjusted + cohort)
raven_home_exposure <- dotwhisker::dwplot(six_models_ni_home, dot_args = list(size = 3) ,
                                          vline = geom_vline(xintercept = 0, colour = "black",linetype = 2)) +
  theme_classic(base_size = 13) + theme(legend.position = 'none') + 
  scale_color_manual(values = c("#440154FF", # sixth
                                "#440154FF", # fifth
                                "#440154FF", # fourth
                                "#440154FF", # third
                                "#440154FF", # second
                                "#440154FF")) + # first
  facet_grid(. ~ "CPM score")






################################################################################################################################################################################################


###########################
### Fine motor function ### 
##########################

# microenvironment : home # 
# exposures : hs_no2_yr_hs_h_log , hs_pm25_yr_hs_h
# outcome :  hs_sum_domhand , hs_laterality

######################################
# sum dom hand - finger tapping test #
#####################################

# model 1 raw - finger tapping test (hs_sum_domhand) - home exposure to NO2 
model1_sumdomhand_home_no2 <- imputed_data %>% 
  with(glm(formula = hs_sum_domhand ~ hs_no2_yr_hs_h_log_iqr + h_cohort, 
           family = gaussian)) %>% 
  mice::pool() 

# model 2 raw - finger tapping test (hs_sum_domhand) - home exposure to PM25
model2_sumdomhand_home_pm25 <- imputed_data %>% 
  with(glm(formula = hs_sum_domhand ~ hs_pm25_yr_hs_h_iqr + h_cohort,  
           family = gaussian)) %>% 
  mice::pool() 

# model 3 adjusted - finger tapping test (hs_sum_domhand) - home exposure to NO2 
model3_sumdomhand_home_no2_adj <- imputed_data %>% 
  with(glm(formula = hs_sum_domhand ~ hs_no2_yr_hs_h_log_iqr +
             h_cohort  + e3_ses + 
             h_age + h_edumc + h_mbmi + h_parity +
             h_fage + e3_edufc + e3_fbmic + 
             hs_age_years + e3_sex + h_seabir, 
           family = gaussian)) %>% 
  mice::pool()  

# model 4 adjusted - finger tapping test (hs_sum_domhand) - home exposure to PM25
model4_sumdomhand_home_pm25_adj <- imputed_data %>% 
  with(glm(formula = hs_sum_domhand ~ hs_pm25_yr_hs_h_iqr +
             h_cohort  + e3_ses + 
             h_age + h_edumc + h_mbmi + h_parity +
             h_fage + e3_edufc + e3_fbmic + 
             hs_age_years + e3_sex + h_seabir, 
           family = gaussian)) %>% 
  mice::pool()   


# model 5 multi-pollutant - finger tapping test (hs_sum_domhand) - home exposure to NO2 and PM25
model5_sumdomhand_home_multipollutant <- imputed_data %>% 
  with(glm(formula = hs_sum_domhand ~ hs_no2_yr_hs_h_log_iqr + hs_pm25_yr_hs_h_iqr +
             h_cohort  + e3_ses + 
             h_age + h_edumc + h_mbmi + h_parity +
             h_fage + e3_edufc + e3_fbmic + 
             hs_age_years + e3_sex + h_seabir, 
           family = gaussian)) %>% 
  mice::pool()   

# summarize models 
model1_sumdomhand_home_no2 <- tidy(model1_sumdomhand_home_no2, conf.int = TRUE) %>% 
  dotwhisker::relabel_predictors("hs_no2_yr_hs_h_log_iqr" = "NO2") %>% 
  filter(term == "NO2") %>%  
  mutate(model = "NO2",
         outcomes = "Sum trials dom hand") 

model2_sumdomhand_home_pm25 <- tidy(model2_sumdomhand_home_pm25, conf.int = TRUE) %>% 
  dotwhisker::relabel_predictors("hs_pm25_yr_hs_h_iqr" = "PM2.5") %>% 
  filter(term == "PM2.5") %>% 
  mutate(model = "PM2.5",
         outcomes = "Sum trials dom hand") 

model3_sumdomhand_home_no2_adj <- tidy(model3_sumdomhand_home_no2_adj, conf.int = TRUE) %>% 
  dotwhisker::relabel_predictors("hs_no2_yr_hs_h_log_iqr" = "NO2 + C") %>% 
  filter(term == "NO2 + C") %>% 
  mutate(model = "NO2 + C",
         outcomes = "Sum trials dom hand")

model4_sumdomhand_home_pm25_adj <- tidy(model4_sumdomhand_home_pm25_adj, conf.int = TRUE) %>%
  dotwhisker::relabel_predictors("hs_pm25_yr_hs_h_iqr" = "PM2.5 + C") %>% 
  filter(term == "PM2.5 + C") %>% 
  mutate(model = "PM2.5 + C",
         outcomes = "Sum trials dom hand")

model5_sumdomhand_home_no2_multipollutant <- tidy(model5_sumdomhand_home_multipollutant, conf.int = TRUE) %>% 
  dotwhisker::relabel_predictors("hs_no2_yr_hs_h_log_iqr" = "NO2 + PM2.5 + C") %>% 
  filter(term == "NO2 + PM2.5 + C") %>% 
  mutate(model = "NO2 + PM2.5 + C",
         outcomes = "Sum trials dom hand")

model6_sumdomhand_home_pm25_multipollutant <- tidy(model5_sumdomhand_home_multipollutant, conf.int = TRUE) %>% 
  dotwhisker::relabel_predictors("hs_pm25_yr_hs_h_iqr" = "PM2.5 + NO2 + C") %>%
  filter(term == "PM2.5 + NO2 + C") %>% 
  mutate(model = "PM2.5 + NO2 + C",
         outcomes = "Sum trials dom hand")


# joining models 
six_models_sumdomhand_home <- rbind(model1_sumdomhand_home_no2, model2_sumdomhand_home_pm25,
                               model3_sumdomhand_home_no2_adj, model4_sumdomhand_home_pm25_adj,
                               model5_sumdomhand_home_no2_multipollutant, model6_sumdomhand_home_pm25_multipollutant) %>% 
  mutate(across(where(is.numeric), round, 2)) # to put this into a table


# single pollutant model non-adjusted by cohort 
rio::export(six_models_sumdomhand_home ,"output/tables/models/home/six_models_sumdomhand_home.xlsx") # export as xlsx

# single pollutant model adjusted by cohort 
rio::export(six_models_sumdomhand_home ,"output/tables/models/home/six_models_sumdomhand_home_adj_cohort.xlsx") # export as xlsx



# forest plot sum dom hand - postnatal exposure (single pollutant model non-adjusted by cohort)
sumdomhand_home_exposure <- dotwhisker::dwplot(six_models_sumdomhand_home, dot_args = list(size = 2) ,
                                          vline = geom_vline(xintercept = 0, colour = "black",linetype = 2)) +
  theme_classic(base_size = 13) + theme(legend.position = 'none') + 
  scale_color_manual(values = c("#440154FF", # sixth
                                "#440154FF", # fifth
                                "#440154FF", # fourth
                                "#440154FF", # third
                                "#EEAD0E", # second
                                "#440154FF"))  + # first
  facet_grid(. ~ "Sum trial dom hand")

# forest plot sum dom hand - postnatal exposure (single pollutant model adjusted by cohort)
sumdomhand_home_exposure <- dotwhisker::dwplot(six_models_sumdomhand_home, dot_args = list(size = 3) ,
                                               vline = geom_vline(xintercept = 0, colour = "black",linetype = 2)) +
  theme_classic(base_size = 13) + theme(legend.position = 'none') + 
  scale_color_manual(values = c("#440154FF", # sixth
                                "#440154FF", # fifth
                                "#440154FF", # fourth
                                "#440154FF", # third
                                "#440154FF", # second
                                "#440154FF"))  + # first
  facet_grid(. ~ "Sum trial dom hand")


##########################################
# Laterality index - finger tapping test #
#########################################

# model 1 raw - finger tapping test (hs_laterality) - home exposure to NO2 
model1_laterality_home_no2 <- imputed_data %>% 
  with(glm(formula = hs_laterality ~ hs_no2_yr_hs_h_log, 
           family = gaussian)) %>% 
  mice::pool() 

# model 2 raw - finger tapping test ( hs_laterality) - home exposure to PM25
model2_laterality_home_pm25 <- imputed_data %>% 
  with(glm(formula = hs_laterality ~ hs_pm25_yr_hs_h ,  
           family = gaussian)) %>% 
  mice::pool() 

# model 3 adjusted - finger tapping test (hs_laterality) - home exposure to NO2 
model3_laterality_home_no2_adj <- imputed_data %>% 
  with(glm(formula = hs_laterality ~ hs_no2_yr_hs_h_log +
             h_cohort  + e3_ses + 
             h_age + h_edumc + h_mbmi + h_parity +
             h_fage + e3_edufc + e3_fbmic + 
             hs_age_years + e3_sex + h_seabir, 
           family = gaussian)) %>% 
  mice::pool()  

# model 4 adjusted - finger tapping test (hs_laterality) - home exposure to PM25
model4_laterality_home_pm25_adj <- imputed_data %>% 
  with(glm(formula = hs_laterality ~ hs_pm25_yr_hs_h +
             h_cohort  + e3_ses + 
             h_age + h_edumc + h_mbmi + h_parity +
             h_fage + e3_edufc + e3_fbmic + 
             hs_age_years + e3_sex + h_seabir, 
           family = gaussian)) %>% 
  mice::pool()   


# model 5 multi-pollutant - finger tapping test ( hs_laterality) - home exposure to NO2 and PM25
model5_laterality_home_multipollutant <- imputed_data %>% 
  with(glm(formula = hs_laterality ~ hs_no2_yr_hs_h_log + hs_pm25_yr_hs_h +
             h_cohort  + e3_ses + 
             h_age + h_edumc + h_mbmi + h_parity +
             h_fage + e3_edufc + e3_fbmic + 
             hs_age_years + e3_sex + h_seabir, 
           family = gaussian)) %>% 
  mice::pool()   

# summarize models 
model1_laterality_home_no2 <- tidy(model1_laterality_home_no2, conf.int = TRUE) %>% 
  dotwhisker::relabel_predictors("hs_no2_yr_hs_h_log" = "NO2") %>% 
  filter(term == "NO2") %>%  
  mutate(model = "NO2",
         outcomes = "Laterality index") 

model2_laterality_home_pm25 <- tidy(model2_laterality_home_pm25, conf.int = TRUE) %>% 
  dotwhisker::relabel_predictors("hs_pm25_yr_hs_h" = "PM2.5") %>% 
  filter(term == "PM2.5") %>% 
  mutate(model = "PM2.5",
         outcomes = "Laterality index") 

model3_laterality_home_no2_adj <- tidy(model3_laterality_home_no2_adj, conf.int = TRUE) %>% 
  dotwhisker::relabel_predictors("hs_no2_yr_hs_h_log" = "NO2 + C") %>% 
  filter(term == "NO2 + C") %>% 
  mutate(model = "NO2 + C",
         outcomes = "Laterality index")

model4_laterality_home_pm25_adj <- tidy(model4_laterality_home_pm25_adj, conf.int = TRUE) %>%
  dotwhisker::relabel_predictors("hs_pm25_yr_hs_h" = "PM2.5 + C") %>% 
  filter(term == "PM2.5 + C") %>% 
  mutate(model = "PM2.5 + C",
         outcomes = "Laterality index")

model5_laterality_home_no2_multipollutant <- tidy(model5_laterality_home_multipollutant, conf.int = TRUE) %>% 
  dotwhisker::relabel_predictors("hs_no2_yr_hs_h_log" = "NO2 + PM2.5 + C") %>% 
  filter(term == "NO2 + PM2.5 + C") %>% 
  mutate(model = "NO2 + PM2.5 + C",
         outcomes = "Laterality index")

model6_laterality_home_pm25_multipollutant <- tidy(model5_laterality_home_multipollutant, conf.int = TRUE) %>% 
  dotwhisker::relabel_predictors("hs_pm25_yr_hs_h" = "PM2.5 + NO2 + C") %>%
  filter(term == "PM2.5 + NO2 + C") %>% 
  mutate(model = "PM2.5 + NO2 + C",
         outcomes = "Laterality index")

# joining models 
six_models_laterality_home <- rbind(model1_laterality_home_no2, model2_laterality_home_pm25,
                               model3_laterality_home_no2_adj, model4_laterality_home_pm25_adj,
                               model5_laterality_home_no2_multipollutant, model6_laterality_home_pm25_multipollutant) %>% 
  mutate(across(where(is.numeric), round, 2)) # to put this into a table


rio::export(six_models_laterality_home,"output/tables/models/home/six_models_laterality_home.xlsx") # export as xlsx





# forest plot laterality index - postnatal exposure home

laterality_home_exposure <- dotwhisker::dwplot(six_models_laterality_home, dot_args = list(size = 4) ,
                                               vline = geom_vline(xintercept = 0, colour = "black",linetype = 2)) +
  theme_classic(base_size = 13) + theme(legend.position = 'none') + 
  scale_color_manual(values = c("#440154FF", # sixth
                                "#440154FF", # fifth
                                "#440154FF", # fourth
                                "#440154FF", # third
                                "#440154FF", # second
                                "#440154FF")) + # first
facet_grid(. ~ "Laterality index")


################################################################################################################################################################################################

##################################
### final figure home exposure ### here we make some final touches to make more pretty figures
#################################

library(patchwork) # this package allows to plot all the figure together side by side using + or |
install.packages("ggtext")
library(ggtext)

# forest plot attentional function (ANT)
attentional_function_home_exposure <- attentional_function_home_exposure + 
  facet_grid(. ~ "Hit react time (se)") +
  scale_x_continuous(limits = c(-25, 25),
                     breaks = c(-25, 0, 25)) # breaks that we want to show on the x-axis

# forest plot task switching score (TMT)
task_switching_home_exposure <- task_switching_home_exposure +
  facet_grid(. ~ "Task switching score") +
  theme(axis.text.y = element_blank(), # with this we erase text
        axis.ticks.y = element_blank(), # with this we erase ticks
        axis.line.y = element_blank()) + # with this we erase y axis 
  scale_x_continuous(limits = c(-16000, 16000),
                     breaks = c(-15000, 0, 15000))

# forest plot task shifting score (TMT)
task_shifting_home_exposure <- task_shifting_home_exposure +
  facet_grid(. ~ "Task shifting score") +
  theme(axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        axis.line.y = element_blank()) +
  scale_x_continuous(limits = c(-0.20, 0.20),
                     breaks = c(-0.20, 0, 0.20))

# forest plot raven score (CMPC Raven)
raven_home_exposure <- raven_home_exposure +
  facet_grid(. ~ "CPM score") + 
  theme(axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        axis.line.y = element_blank()) + 
  scale_x_continuous(limits = c(-2, 2),
                     breaks = c(-2, 0, 2))

  
# forest plot sum trial dom hand (Finger tapping test)
sumdomhand_home_exposure <- sumdomhand_home_exposure +
  facet_grid(. ~ "Sum trial dom hand") + 
  theme(axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        axis.line.y = element_blank()) +
  scale_x_continuous(limits = c(-8, 8),
                     breaks = c(-8, 0, 8))

# forest plot laterality (Finger tapping test)

laterality_home_exposure <- laterality_home_exposure + 
  facet_grid(. ~ "Laterality index") + 
  theme(axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        axis.line.y = element_blank()) + 
  scale_x_continuous(limits = c(-2, 2),
                     breaks = c(-2, -1, 0, 1, 2))

# we need to use | instead of + to obtain an image of one row and six column 
home_forest_plot <- (attentional_function_home_exposure | task_switching_home_exposure | task_shifting_home_exposure | raven_home_exposure | sumdomhand_home_exposure)

# the final forest plot 
home_forest_plot <- home_forest_plot + 
plot_annotation(title = 'A. Exposure to air pollution at home', # name of the plot 
                subtitle = expression("Effect estimate of air pollution at home (" *beta * "" [AP-home] * " (95 % CI))")) & # name of subtitle and beta - 95% CI
  theme(plot.title = element_text(face = 'bold', size = 16), # size and bold text for tittle 
        plot.subtitle = element_text(hjust=0.5, size = 14), # size and bold text for subtittle
        strip.text.x = element_text(size = 12)) # stript text 

ggsave("output/figures/fig2A_home_forest_plot_adj_cohort_v3.tiff", width = 12, height = 3, dpi = 400) # export our figure (you can change width and height depending on how yo want to show your figure)

                




home_forest_plot / school_forest_plot







































































  
  



