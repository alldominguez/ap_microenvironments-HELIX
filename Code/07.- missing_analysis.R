# 07.- Missing analysis 

pacman::p_load(tidyverse, DataExplorer, skimr, haven, patchwork, hablar, rio, magrittr, ggridges, 
               viridis, hrbrthemes, gtsummary, janitor, flextable, kableExtra, corrplot, summarytools,
               foreign, psych, car, readstata13, SmartEDA, GGally, ggcorrplot, 
               naniar, mice, visdat) # packages for missing values

# load the functions created previously 
source("Code/functions_tfm.R") 

# load data 
helix_data <- readxl::read_xlsx("Data/clean/helix_db_clean_2.xlsx")


helix_data %>% dplyr::glimpse()

hist(helix_data$hs_age_years)

summary(helix_data$hs_age_years)

mean(helix_data$hs_age_years)
sd(helix_data$hs_age_years)


########################
### Missing analysis ###
#######################

# select a subset of variables 
all_exposures <- helix_data %>% dplyr::select(h_cohort, # cohort
                                             h_no2_ratio_preg, h_no2_ratio_t1, h_no2_ratio_t2, h_no2_ratio_t3, # nitrogen dioxide - prenatal exposure
                                             h_pm10_ratio_preg, h_pm10_ratio_t1, h_pm10_ratio_t2, h_pm10_ratio_t3, # pm10 - prenatal exposure
                                             h_pm25_ratio_preg, h_pm25_ratio_t1, h_pm25_ratio_t2, h_pm25_ratio_t3,   # pm25 - prenatal exposure
                                             h_abs_ratio_preg, h_abs_ratio_t1, h_abs_ratio_t2, h_abs_ratio_t3,
                                             h_lden_preg, h_ndvi100_preg, # ndvi and noise exposure 
                                             hs_no2_yr_hs_h, hs_no2_yr_hs_s, hs_no2_yr_hs_r, # NO2 microenvironments 
                                             hs_pm10_yr_hs_h, hs_pm10_yr_hs_s, hs_pm10_yr_hs_r, # PM10 microenvironments
                                             hs_pm25_yr_hs_h, hs_pm25_yr_hs_s, hs_pm25_yr_hs_r, # PM25 microenvironments
                                             hs_pm25abs_yr_hs_h, hs_pm25abs_yr_hs_s, hs_pm25abs_yr_hs_r,# PM25 abs microenvironments
                                             hs_lden_h, hs_lden_s, hs_lden_r, # lden microenvironments
                                             hs_ndvi100_h, hs_ndvi100_s, hs_ndvi100_r) # ndvi microenvironments

########################################
### visual representation of missing ###
#######################################

#################
### exposures ###
################

# missing plot matrix for pre and postnatal exposures
all_exposures %>% 
naniar::gg_miss_fct(fct = h_cohort)

# missing plot percentages for pre and postnatal exposures
all_exposures %>% 
naniar::gg_miss_var(facet = h_cohort)


#######################
### final exposures ###
#####################

# selection of exposures
final_exposures <- helix_data %>% 
  select(hs_no2_yr_hs_h, hs_no2_yr_hs_s, hs_no2_yr_hs_r, # NO2 microenvironments 
         hs_pm25_yr_hs_h, hs_pm25_yr_hs_s, hs_pm25_yr_hs_r) # PM25 microenvironments



colnames(final_exposures) <- c("NO2 home", "NO2 school", "NO2 commute", 
                               "PM25 home", "PM25 school", "PM25 commute")




visdat::vis_miss(final_exposures) 

glimpse(helix_data)

helix_data <- helix_data %>%
  mutate(task_shifting_score = ((hs_tmtb_responsetime - hs_tmta_responsetime)) / (hs_tmta_responsetime))


final_outcomes <- helix_data %>% 
  select(hs_hitrtse,
         hs_tmtb_responsetime,
         task_shifting_score,
         hs_correct_raven,
         hs_sum_domhand,
         hs_laterality)

colnames(final_outcomes) <- c("hitr se", "Task switch", "Task shift", 
                               "Raven", "Dom hand", "Laterality")

visdat::vis_miss(final_outcomes) 











# missing matrix plot 
final_exposures %>% 
  naniar::gg_miss_fct(fct = h_cohort)

# missing observartions 
final_exposures %>% 
  naniar::gg_miss_var(facet = h_cohort)

#######################################
### sensitivity exposures variables ###
######################################
  
# exposures sensitivity
exposures_sensitivity <- helix_data %>% 
    select(h_lden_preg, h_ndvi100_preg, # prenatal noise and ndvi
           hs_lden_h, hs_lden_s, hs_lden_r, # posntatal noise
           hs_ndvi100_h, hs_ndvi100_s, hs_ndvi100_r) # postnatal ndvi
  
################
### outcomes ###  
################  

# final outcomes selection
final_outcomes <- helix_data %>% 
  select(hs_correct_raven:hs_tmtb_responsetime, hs_hitrt2_mean, hs_hitrtse, hs_missings)







# missing plot m atrix for the outcomes
final_outcomes %>% 
    naniar::gg_miss_fct(fct = h_cohort)

# missing plot percentages for the outcomes 
outcomes_helix %>% 
    naniar::gg_miss_var(facet = h_cohort)






