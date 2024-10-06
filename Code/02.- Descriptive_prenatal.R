# 02.- Descriptive analysis prenatal exposures

pacman::p_load(tidyverse, DataExplorer, skimr, haven, patchwork, hablar, rio, magrittr, ggridges, 
               viridis, hrbrthemes, gtsummary, janitor, flextable, kableExtra, corrplot, summarytools,
               foreign, psych, car, readstata13)

# load the functions created previously 
source("Code/functions_tfm.R") 

# load data 
helix_data <- readxl::read_xlsx("Data/clean/helix_db_clean_2.xlsx")

# check the structure of the data 
helix_data %>% dplyr::glimpse()

########################################## EXPOSURE OVERVIEW ########################################## 

# we select the pollutant
pregnancy_exposures <- helix_data %>% dplyr::select(h_no2_bg_ratio_preg:h_ndvi100_preg, h_cohort)

# quick check of the data
dplyr::glimpse(pregnancy_exposures)

######################################
########### TABLES & PLOTS ########### ---- PRENATAL ----- # EXPOSURES # 
######################################

source("Code/functions_tfm.R") # charge functions created

######################
### PM25 prenatal #######################################################################################
#####################

########################
### entire pregnancy ###
#######################

# pm2.5 exposure entire pregnancy  
tfm_summary_v2(var = h_pm25_ratio_preg, group = h_cohort, data = pregnancy_exposures) %>% 
  rio::export("output/tables/pm25_entire.xlsx") # export as xlsx

##########
### T1 ###
#########

# pm2.5 exposure trimester 1 
tfm_summary_v2(var = h_pm25_ratio_t1, group = h_cohort, data = pregnancy_exposures) %>% 
  rio::export("output/tables/pm25_t1.xlsx") # export as xlsx

##########
### T2 ###
#########

# pm2.5 exposure trimester 2 
tfm_summary_v2(var = h_pm25_ratio_t2, group = h_cohort, data = pregnancy_exposures) %>% 
  rio::export("output/tables/pm25_t2.xlsx") # export as xlsx

##########
### T3 ###
#########

# pm2.5 exposure trimester 3 
tfm_summary_v2(var = h_pm25_ratio_t3, group = h_cohort, data = pregnancy_exposures) %>% 
  rio::export("output/tables/pm25_t3_cohort.xlsx") # export as xlsx


######################
### PM10 prenatal #######################################################################################
#####################

########################
### entire pregnancy ###
#######################

# pm10 exposure entire pregnancy  - total 
tfm_summary_v2(var = h_pm10_ratio_preg, group = h_cohort, data = pregnancy_exposures) %>% 
  rio::export("output/tables/pm10_entire_cohort.xlsx") # export as xlsx

##########
### T1 ###
#########

# trimester 1 
tfm_summary_v2(var = h_pm10_ratio_t1, group = h_cohort, data = pregnancy_exposures) %>% 
  rio::export("output/tables/pm10_t1.xlsx") # export as xlsx

##########
### T2 ###
#########

# trimester 2 - total 
tfm_summary_v2(var = h_pm10_ratio_t2, group = hcohort, data = pregnancy_exposures) %>% 
  rio::export("output/tables/pm10_t2.xlsx") # export as xlsx

##########
### T3 ###
#########

# trimester 3 - by cohort 
tfm_summary_v2(var = h_pm10_ratio_t3, group = h_cohort, data = pregnancy_exposures) %>% 
  rio::export("output/tables/pm10_t3.xlsx") # export as xlsx


######################
### NO2 prenatal #######################################################################################
#####################

########################
### entire pregnancy ###
#######################
tfm_summary_v2(var = h_no2_ratio_preg, group = h_cohort, data = pregnancy_exposures) %>% 
  rio::export("output/tables/no2_entire_cohort.xlsx") # export as xlsx

##########
### T1 ###
#########

# trimester 1 
tfm_summary_v2(var = h_no2_ratio_t1, group = h_cohort, data = pregnancy_exposures) %>% 
  rio::export("output/tables/no2_t1.xlsx") # export as xlsx

##########
### T2 ###
#########

# trimester 2 - total 
tfm_summary_v2(var = h_no2_ratio_t2, group = h_cohort, data = pregnancy_exposures) %>% 
  rio::export("output/tables/no2_t2.xlsx") # export as xlsx

##########
### T3 ###
#########

# trimester 3 - by cohort 
tfm_summary_v2(var = h_no2_ratio_t3, group = h_cohort, data = pregnancy_exposures) %>% 
  rio::export("output/tables/no2_t3.xlsx") # export as xlsx


######################
### PM25 abs prenatal #######################################################################################
#####################

########################
### entire pregnancy ###
#######################

# pm2.5 abs exposure entire pregnancy  
tfm_summary_v2(var = h_abs_ratio_preg, group = h_cohort, data = pregnancy_exposures) %>% 
  rio::export("output/tables/abs_entire.xlsx") # export as xlsx

##########
### T1 ###
#########

# pm2.5 abs exposure trimester 1 
tfm_summary_v2(var = h_abs_ratio_t1, group = h_cohort, data = pregnancy_exposures) %>% 
  rio::export("output/tables/abs_t1.xlsx") # export as xlsx

##########
### T2 ###
#########

# pm2.5 abs exposure trimester 2 
tfm_summary_v2(var = h_abs_ratio_t2, group = h_cohort, data = pregnancy_exposures) %>% 
  rio::export("output/tables/abs_t2.xlsx") # export as xlsx

##########
### T3 ###
#########

# pm2.5 abs exposure trimester 3 
tfm_summary_v2(var = h_abs_ratio_t3, group = h_cohort, data = pregnancy_exposures) %>% 
  rio::export("output/tables/abs_t3_.xlsx") # export as xlsx


#########################
### NDVI & NOISE EXPOSURE #######################################################################################
########################

# Green exposure during entire pregnancy - NDVI
tfm_summary_v2(var = h_ndvi100_preg, group = h_cohort, data = pregnancy_exposures) %>% 
  rio::export("output/tables/ndvi_entire_pregnancy.xlsx") # export as xlsx

# Noise exposure during entire pregnancy - Lden
tfm_summary_v2(var = h_lden_preg, group = h_cohort, data = pregnancy_exposures) %>% 
  rio::export("output/tables/lden_entire_pregnancy.xlsx") # export as xlsx


#################################################################################################################################################


##############################
########### PLOTS ########### # PRENATAL EXPOSURES # 
#############################

### pm25 ### 

### pm25 - T1 ###
pm25_pre_T1 <- ggplot(data = helix_data, mapping = aes(x = h_cohort, y = h_pm25_ratio_t1, color = h_cohort))  + 
  geom_violin() + theme_classic() + xlab('Cohort') + ylab('PM25 (ug/m3) - T1') + ylim(c(0, 40)) +
  scale_color_viridis(discrete=TRUE) + theme(legend.position="none") + geom_boxplot(alpha = 0.4, width = 0.2, color = 'black') + 
  geom_jitter(alpha = 0.1) 
  
### pm25 - T2 ###
pm25_pre_T2 <- ggplot(data = helix_data, mapping = aes(x = h_cohort, y = h_pm25_ratio_t2, color = h_cohort))  + 
  geom_violin() + theme_classic() + xlab('Cohort') + ylab('PM25 (ug/m3) - T2') + ylim(c(0, 40)) +
  scale_color_viridis(discrete=TRUE) + theme(legend.position="none") + geom_boxplot(alpha = 0.4, width = 0.2, color = 'black') + 
  geom_jitter(alpha = 0.1) 

### pm25 - T3 ###

pm25_pre_T3 <- ggplot(data = helix_data, mapping = aes(x = h_cohort, y = h_pm25_ratio_t3, color = h_cohort))  + 
  geom_violin() + theme_classic() + xlab('Cohort') + ylab('PM25 (ug/m3) - T3') + ylim(c(0, 40)) +
  scale_color_viridis(discrete=TRUE) + theme(legend.position="none") + geom_boxplot(alpha = 0.4, width = 0.2, color = 'black') + 
  geom_jitter(alpha = 0.1) 


pm25_pre_T1 + pm25_pre_T2 + pm25_pre_T3

### pm25 - entire pregnancy ###
pm25_pre_entire <- ggplot(data = helix_data, mapping = aes(x = h_cohort, y = h_pm25_ratio_preg, color = h_cohort))  + 
  geom_violin() + theme_classic() + xlab('Cohort') + ylab('PM25 (ug/m3) - entire preg') + ylim(c(0, 30)) +
  scale_color_viridis(discrete=TRUE) + theme(legend.position="none") + geom_boxplot(alpha = 0.4, width = 0.2, color = 'black') + 
  geom_jitter(alpha = 0.1) 


### pm10 ### 

### pm10 - T1 ###
pm10_pre_T1 <- ggplot(data = helix_data, mapping = aes(x = h_cohort, y = h_pm10_ratio_t1, color = h_cohort))  + 
  geom_violin() + theme_classic() + xlab('Cohort') + ylab('PM10 (ug/m3) - T1') + ylim(c(0, 60)) +
  scale_color_viridis(discrete=TRUE) + theme(legend.position="none") + geom_boxplot(alpha = 0.4, width = 0.2, color = 'black') + 
  geom_jitter(alpha = 0.1) 

### pm10 - T2 ###
pm10_pre_T2 <- ggplot(data = helix_data, mapping = aes(x = h_cohort, y = h_pm10_ratio_t2, color = h_cohort))  + 
  geom_violin() + theme_classic() + xlab('Cohort') + ylab('PM10 (ug/m3) - T2') + ylim(c(0, 60)) +
  scale_color_viridis(discrete=TRUE) + theme(legend.position="none") + geom_boxplot(alpha = 0.4, width = 0.2, color = 'black') + 
  geom_jitter(alpha = 0.1) 

### pm10 - T3 ###

pm10_pre_T3 <- ggplot(data = helix_data, mapping = aes(x = h_cohort, y = h_pm10_ratio_t3, color = h_cohort))  + 
  geom_violin() + theme_classic() + xlab('Cohort') + ylab('PM10 (ug/m3) - T3') + ylim(c(0, 60)) +
  scale_color_viridis(discrete=TRUE) + theme(legend.position="none") + geom_boxplot(alpha = 0.4, width = 0.2, color = 'black') + 
  geom_jitter(alpha = 0.1) 


pm10_pre_T1 + pm10_pre_T2 + pm10_pre_T3


### pm10 - entire pregnancy ###
pm10_pre_entire <- ggplot(data = helix_data, mapping = aes(x = h_cohort, y = h_pm10_ratio_preg, color = h_cohort))  + 
  geom_violin() + theme_classic() + xlab('Cohort') + ylab('NO2 (ug/m3) - entire preg') + ylim(c(0, 50)) +
  scale_color_viridis(discrete=TRUE) + theme(legend.position="none") + geom_boxplot(alpha = 0.4, width = 0.2, color = 'black') + 
  geom_jitter(alpha = 0.1) 


### NO2 ### 

### NO2 - T1 ###
no2_pre_T1 <- ggplot(data = helix_data, mapping = aes(x = h_cohort, y = h_no2_ratio_t1, color = h_cohort))  + 
  geom_violin() + theme_classic() + xlab('Cohort') + ylab('NO2 (ug/m3) - T1') + ylim(c(0, 120)) +
  scale_color_viridis(discrete=TRUE) + theme(legend.position="none") + geom_boxplot(alpha = 0.4, width = 0.2, color = 'black') + 
  geom_jitter(alpha = 0.1) 

### NO2 - T2 ###
no2_pre_T2 <- ggplot(data = helix_data, mapping = aes(x = h_cohort, y = h_no2_ratio_t2, color = h_cohort))  + 
  geom_violin() + theme_classic() + xlab('Cohort') + ylab('NO2 (ug/m3) - T2') + ylim(c(0, 120)) +
  scale_color_viridis(discrete=TRUE) + theme(legend.position="none") + geom_boxplot(alpha = 0.4, width = 0.2, color = 'black') + 
  geom_jitter(alpha = 0.1) 

### NO2 - T3 ###

no2_pre_T3 <- ggplot(data = helix_data, mapping = aes(x = h_cohort, y = h_no2_ratio_t3, color = h_cohort))  + 
  geom_violin() + theme_classic() + xlab('Cohort') + ylab('NO2 (ug/m3) - T3') + ylim(c(0, 120)) +
  scale_color_viridis(discrete=TRUE) + theme(legend.position="none") + geom_boxplot(alpha = 0.4, width = 0.2, color = 'black') + 
  geom_jitter(alpha = 0.1) 


no2_pre_T1 + no2_pre_T2 + no2_pre_T3

### NO2 - entire pregnancy ###
no2_pre_entire <- ggplot(data = helix_data, mapping = aes(x = h_cohort, y = h_no2_ratio_preg, color = h_cohort))  + 
  geom_violin() + theme_classic() + xlab('Cohort') + ylab('NO2 (ug/m3) - entire preg') + ylim(c(0, 100)) +
  scale_color_viridis(discrete=TRUE) + theme(legend.position="none") + geom_boxplot(alpha = 0.4, width = 0.2, color = 'black') + 
  geom_jitter(alpha = 0.1) 



### pm25 abs ### 

### pm25 abs - T1 ###
pm25abs_pre_T1 <- ggplot(data = helix_data, mapping = aes(x = h_cohort, y = h_abs_ratio_t1, color = h_cohort))  + 
  geom_violin() + theme_classic() + xlab('Cohort') + ylab('PM25 abs - T1') + ylim(c(0, 8)) +
  scale_color_viridis(discrete=TRUE) + theme(legend.position="none") + geom_boxplot(alpha = 0.4, width = 0.2, color = 'black') + 
  geom_jitter(alpha = 0.1) 

### pm25 - T2 ###
pm25abs_pre_T2 <- ggplot(data = helix_data, mapping = aes(x = h_cohort, y = h_abs_ratio_t2, color = h_cohort))  + 
  geom_violin() + theme_classic() + xlab('Cohort') + ylab('PM25 abs - T2') + ylim(c(0, 8)) +
  scale_color_viridis(discrete=TRUE) + theme(legend.position="none") + geom_boxplot(alpha = 0.4, width = 0.2, color = 'black') + 
  geom_jitter(alpha = 0.1) 

### pm25 - T3 ###

pm25abs_pre_T3 <- ggplot(data = helix_data, mapping = aes(x = h_cohort, y = h_abs_ratio_t3, color = h_cohort))  + 
  geom_violin() + theme_classic() + xlab('Cohort') + ylab('PM25 abs - T3') + ylim(c(0, 8)) +
  scale_color_viridis(discrete=TRUE) + theme(legend.position="none") + geom_boxplot(alpha = 0.4, width = 0.2, color = 'black') + 
  geom_jitter(alpha = 0.1) 


pm25abs_pre_T1 + pm25abs_pre_T2 + pm25abs_pre_T3

### pm25 - entire pregnancy ###
pm25abs_pre_entire <- ggplot(data = helix_data, mapping = aes(x = h_cohort, y = h_abs_ratio_preg, color = h_cohort))  + 
  geom_violin() + theme_classic() + xlab('Cohort') + ylab('PM25 abs - entire preg') + ylim(c(0, 8)) +
  scale_color_viridis(discrete=TRUE) + theme(legend.position="none") + geom_boxplot(alpha = 0.4, width = 0.2, color = 'black') + 
  geom_jitter(alpha = 0.1) 

### NDVI ###

### pm25 - entire pregnancy ###
ndvi_pre_entire <- ggplot(data = helix_data, mapping = aes(x = h_cohort, y = h_ndvi100_preg, color = h_cohort))  + 
  geom_violin() + theme_classic() + xlab('Cohort') + ylab('NDVI 100 m - entire preg') + ylim(c(0, 1)) +
  scale_color_viridis(discrete=TRUE) + theme(legend.position="none") + geom_boxplot(alpha = 0.4, width = 0.2, color = 'black') + 
  geom_jitter(alpha = 0.1) 

### Lden ###

lden_pre_entire <- ggplot(data = helix_data, mapping = aes(x = h_cohort, y = h_lden_preg, color = h_cohort))  + 
  geom_violin() + theme_classic() + xlab('Cohort') + ylab('NDVI 100 m - entire preg') + ylim(c(0, 80)) +
  scale_color_viridis(discrete=TRUE) + theme(legend.position="none") + geom_boxplot(alpha = 0.4, width = 0.2, color = 'black') + 
  geom_jitter(alpha = 0.1) 













#########################################################################################################

##############################
####### CORRELATIONS ######## # Exposures # 
#############################
glimpse(helix_data)

# we only select the pollutants that we going to use for the correlation matrix
air_pollutants <- helix_data %>% select(hs_no2_yr_hs_h, hs_no2_yr_hs_s, hs_no2_yr_hs_r, # NO2
                                        hs_pm10_yr_hs_h, hs_pm10_yr_hs_s, hs_pm10_yr_hs_r, # PM10
                                        hs_pm25_yr_hs_h, hs_pm25_yr_hs_s, hs_pm25_yr_hs_r, # PM25
                                        hs_pm25abs_yr_hs_h, hs_pm25abs_yr_hs_s, hs_pm25abs_yr_hs_r)  # PM25 abs 
                               
colnames(air_pollutants)<-c("NO2_home", "NO2_school", "NO2_commute", #NO2
                            "PM10_home", "PM10_school", "PM10_commute",
                            "PM25_home", "PM25_school", "PM25_commute",
                            "PM25abs_home", "PM25abs_school", "PM25abs_commute")

glimpse(air_pollutants)

#correlacion con corrplot ----------------------
r_corr_matrix_air <- cor(air_pollutants, method = "spearman", use = "complete.obs")
corrplot(r_corr_matrix_air, method ="shade", addCoef.col = FALSE, type = 'lower')






### univariate model 
glm(formula = hs_correct_raven ~  hs_pm25_yr_hs_h + e3_sex + h_age, data = helix_csv) %>% broom::tidy() %>% 
  dotwhisker::dwplot() + geom_vline( xintercept = 0, linetype = 'dashed', col = 'red') + xlim(c(-2 , 2)) 















































####################################### OTHERS ########################################

# entire pregnancy
pregnancy_exposures %>% group_by(h_cohort) %>% skimr::skim(h_pm25_ratio_preg) %>% skimr::skim(h_pm25_ratio_t1)  %>% 
  select(h_cohort, n_missing, numeric.mean, numeric.sd, 
         numeric.p50, numeric.p25, numeric.p75,
         numeric.p0, numeric.p100) %>% 
  flextable::flextable() %>%  # convert to image
  flextable::autofit() %>% # ensure only one line per row
  colformat_double(digits = 1) %>%
  flextable::save_as_docx(path = "output/tables/pm25entire_pregnancy.docx") 

# trimester 1
pregnancy_exposures %>% group_by(h_cohort) %>% skimr::skim(h_pm25_ratio_t1)  %>% 
  select(h_cohort, n_missing, numeric.mean, numeric.sd, 
         numeric.p50, numeric.p25, numeric.p75,
         numeric.p0, numeric.p100) %>% 
  flextable::flextable() %>%  # convert to image
  flextable::autofit() %>% # ensure only one line per row
  colformat_double(digits = 1) %>%
  flextable::save_as_docx(path = "output/tables/pm25t1.docx") # set the number of digits to 1

# trimester 2
pregnancy_exposures %>% group_by(h_cohort) %>% skimr::skim(h_pm25_ratio_t2)  %>% 
  select(h_cohort, n_missing, numeric.mean, numeric.sd, 
         numeric.p50, numeric.p25, numeric.p75,
         numeric.p0, numeric.p100) %>% 
  flextable::flextable() %>%  # convert to image
  flextable::autofit() %>% # ensure only one line per row
  colformat_double(digits = 1) %>%
  flextable::save_as_docx(path = "output/tables/pm25t2.docx") # set the number of digits to 1


# trimester 3
pregnancy_exposures %>% group_by(h_cohort) %>% skimr::skim(h_pm25_ratio_t3) %>% 
  select(h_cohort, n_missing, numeric.mean, numeric.sd, 
         numeric.p50, numeric.p25, numeric.p75,
         numeric.p0, numeric.p100) %>% 
  flextable::flextable() %>%  # convert to image
  flextable::autofit() %>% # ensure only one line per row
  colformat_double(digits = 1)  %>% # set the number of digits to 1
  flextable::save_as_docx(path = "output/tables/pm25t3.docx")  # export the data 

