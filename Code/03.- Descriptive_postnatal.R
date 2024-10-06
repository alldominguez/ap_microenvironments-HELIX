# 03.- Descriptive analysis postnatal exposures

pacman::p_load(tidyverse, DataExplorer, skimr, haven, patchwork, hablar, rio, magrittr, ggridges, 
               viridis, hrbrthemes, gtsummary, janitor, flextable, kableExtra, corrplot, summarytools,
               foreign, psych, car, readstata13)

# load the functions created previously 
source("Code/functions_tfm.R") 

# load data 
helix_data <- readxl::read_xlsx("Data/clean/helix_db_clean_2.xlsx")

# check the structure of the data 
helix_data %>% dplyr::glimpse()

# we create a separete data for postnatal exposures
postnatal_exposures <- helix_data %>% dplyr::select(hs_no2_yr_hs_h:hs_ndvi100_t_sum, h_cohort)

######################################
########### TABLES & PLOTS ########### ---- POSTNATAL ----- # EXPOSURES # 
######################################

# codebook microenvironments 
# pollutant_yr_hs_h (home), pollutant_yr_hs_s (school), pollutant_yr_hs_r (commute)
# pollutant_yr_hs_p (other places), pollutant_yr_hs_t (total)

# check the data 
glimpse(postnatal_exposures)

######################
### PM25 postnatal    #######################################################################################
#####################

# PM25 home exposure 
tfm_summary_v2(var = hs_pm25_yr_hs_h, group = h_cohort, data = postnatal_exposures) %>% 
  rio::export("output/tables/pm25_postnatal_home.xlsx") # export as xlsx

# PM25 school exposure 
tfm_summary_v2(var = hs_pm25_yr_hs_s, group = h_cohort, data = postnatal_exposures) %>% 
  rio::export("output/tables/pm25_postnatal_school.xlsx") # export as xlsx

# PM25 commute exposure
tfm_summary_v2(var = hs_pm25_yr_hs_r, group = h_cohort, data = postnatal_exposures) %>% 
  rio::export("output/tables/pm25_postnatal_commute.xlsx") # export as xlsx

# PM25 other places exposure 
tfm_summary_v2(var = hs_pm25_yr_hs_p, group = h_cohort, data = postnatal_exposures) %>% 
  rio::export("output/tables/pm25_postnatal_other_places.xlsx") # export as xlsx

# PM25 total exposure 
tfm_summary_v2(var = hs_pm25_yr_hs_t, group = h_cohort, data = postnatal_exposures) %>% 
  rio::export("output/tables/pm25_postnatal_total.xlsx") # export as xlsx

######################
### PM10 postnatal    #######################################################################################
#####################

# PM10 home exposure 
tfm_summary_v2(var = hs_pm10_yr_hs_h, group = h_cohort, data = postnatal_exposures) %>% 
  rio::export("output/tables/pm10_postnatal_home.xlsx") # export as xlsx

# PM10 school exposure 
tfm_summary_v2(var = hs_pm10_yr_hs_s, group = h_cohort, data = postnatal_exposures) %>% 
  rio::export("output/tables/pm10_postnatal_school.xlsx") # export as xlsx

# PM10 commute exposure
tfm_summary_v2(var = hs_pm10_yr_hs_r, group = h_cohort, data = postnatal_exposures) %>% 
  rio::export("output/tables/pm10_postnatal_commute.xlsx") # export as xlsx

# PM10 other places exposure 
tfm_summary_v2(var = hs_pm10_yr_hs_p, group = h_cohort, data = postnatal_exposures) %>% 
  rio::export("output/tables/pm10_postnatal_other_places.xlsx") # export as xlsx

# PM10 total exposure 
tfm_summary_v2(var = hs_pm10_yr_hs_t, group = h_cohort, data = postnatal_exposures) %>% 
  rio::export("output/tables/pm10_postnatal_total.xlsx") # export as xlsx


######################
### NO2 postnatal    #######################################################################################
#####################

# NO2 home exposure 
tfm_summary_v2(var = hs_no2_yr_hs_h, group = h_cohort, data = postnatal_exposures) %>% 
  rio::export("output/tables/no2_postnatal_home.xlsx") # export as xlsx

# NO2 school exposure 
tfm_summary_v2(var = hs_no2_yr_hs_s, group = h_cohort, data = postnatal_exposures) %>% 
  rio::export("output/tables/no2_postnatal_school.xlsx") # export as xlsx

# NO2  commute exposure
tfm_summary_v2(var = hs_no2_yr_hs_r, group = h_cohort, data = postnatal_exposures) %>% 
  rio::export("output/tables/no2_postnatal_commute.xlsx") # export as xlsx

# NO2 other places exposure 
tfm_summary_v2(var = hs_no2_yr_hs_p, group = h_cohort, data = postnatal_exposures) %>% 
  rio::export("output/tables/no2_postnatal_other_places.xlsx") # export as xlsx

# NO2 total exposure 
tfm_summary_v2(var = hs_no2_yr_hs_t, group = h_cohort, data = postnatal_exposures) %>% 
  rio::export("output/tables/no2_postnatal_total.xlsx") # export as xlsx


######################
### PM25 abs postnatal    #######################################################################################
#####################

# PM25 abs home exposure 
tfm_summary_v2(var = hs_pm25abs_yr_hs_h, group = h_cohort, data = postnatal_exposures) %>% 
  rio::export("output/tables/pm25abs_postnatal_home.xlsx") # export as xlsx

# PM25 abs school exposure 
tfm_summary_v2(var = hs_pm25abs_yr_hs_s, group = h_cohort, data = postnatal_exposures) %>% 
  rio::export("output/tables/pm25abs_postnatal_school.xlsx") # export as xlsx

# PM25 abs commute exposure
tfm_summary_v2(var = hs_pm25abs_yr_hs_r, group = h_cohort, data = postnatal_exposures) %>% 
  rio::export("output/tables/pm25abs_postnatal_commute.xlsx") # export as xlsx

# PM25 abs other places exposure 
tfm_summary_v2(var = hs_pm25abs_yr_hs_p, group = h_cohort, data = postnatal_exposures) %>% 
  rio::export("output/tables/pm25abs_postnatal_other_places.xlsx") # export as xlsx

# PM25 abs total exposure 
tfm_summary_v2(var = hs_pm25abs_yr_hs_t, group = h_cohort, data = postnatal_exposures) %>% 
  rio::export("output/tables/pm25abs_postnatal_total.xlsx") # export as xlsx

###########################
### NDVI  100 m - postnatal #######################################################################################
###########################

# we check our created function with skim from the skimr package
postnatal_exposures %>% group_by(h_cohort) %>% skimr::skim(hs_ndvi100_h)

# NDVI 100 m - home exposure
tfm_summary_v2(var = hs_ndvi100_h, group = h_cohort, data = postnatal_exposures) %>% 
  rio::export("output/tables/ndvi_100m_postnatal_home.xlsx")

# NDVI 100 m - school exposure
tfm_summary_v2(var = hs_ndvi100_s, group = h_cohort, data = postnatal_exposures) %>% 
  rio::export("output/tables/ndvi_100m_postnatal_school.xlsx")

# NDVI 100 m - commute exposure
tfm_summary_v2(var = hs_ndvi100_r, group = h_cohort, data = postnatal_exposures) %>% 
  rio::export("output/tables/ndvi_100m_postnatal_commute.xlsx")

# NDVI 100 m - other places exposure
tfm_summary_v2(var = hs_ndvi100_p, group = h_cohort, data = postnatal_exposures) %>% 
  rio::export("output/tables/ndvi_100m_postnatal_otherplaces.xlsx")

# NDVI 100 m - total exposure
tfm_summary_v2(var = hs_ndvi100_t, group = h_cohort, data = postnatal_exposures) %>% 
  rio::export("output/tables/ndvi_100m_postnatal_total.xlsx")

#####################################
### Lden (noise exposure) - postnatal #######################################################################################
####################################

# Lden - home  exposure
tfm_summary_v2(var = hs_lden_h, group = h_cohort, data = postnatal_exposures) %>% 
  rio::export("output/tables/lden_postnatal_home.xlsx")

# Lden - school exposure
tfm_summary_v2(var = hs_lden_s, group = h_cohort, data = postnatal_exposures) %>% 
  rio::export("output/tables/lden_postnatal_school.xlsx")

# Lden - commute exposure
tfm_summary_v2(var = hs_lden_r, group = h_cohort, data = postnatal_exposures) %>% 
  rio::export("output/tables/lden_postnatal_commute.xlsx")

# Lden - other places exposure
tfm_summary_v2(var = hs_lden_p, group = h_cohort, data = postnatal_exposures) %>% 
  rio::export("output/tables/lden_postnatal_otherplaces.xlsx")

# Lden - total exposure
tfm_summary_v2(var = hs_lden_t, group = h_cohort, data = postnatal_exposures) %>% 
  rio::export("output/tables/lden_postnatal_total.xlsx")

#################################################################################################################################################


##############################
########### PLOTS ########### # POSTNATAL EXPOSURES # 
#############################

# we only select the pollutants that we going to use for the correlation matrix
air_pollutants <- helix_data %>% select(hs_no2_yr_hs_h, hs_no2_yr_hs_s, hs_no2_yr_hs_r, # NO2
                                        hs_pm10_yr_hs_h, hs_pm10_yr_hs_s, hs_pm10_yr_hs_r, # PM10
                                        hs_pm25_yr_hs_h, hs_pm25_yr_hs_s, hs_pm25_yr_hs_r, # PM25
                                        hs_pm25abs_yr_hs_h, hs_pm25abs_yr_hs_s, hs_pm25abs_yr_hs_r, h_cohort)  # PM25 abs 

### PM25 boxplot ### 

# PM25 concentration at home 
PM25_home_cohort_plot <- ggplot2::ggplot(data = helix_data, mapping = aes(x = h_cohort, y = hs_pm25_yr_hs_h, color = h_cohort)) + 
  geom_violin() + theme_classic() + xlab('Cohort') + ylab('PM25 ug/m3 at home') + ylim(c(0, 50)) +
  scale_color_viridis(discrete=TRUE) + theme(legend.position="none") + geom_boxplot(alpha = 0.4, width = 0.2, color = 'black') + 
  geom_jitter(alpha = 0.1)

# PM25 concentration at school 
PM25_school_cohort_plot <- ggplot2::ggplot(data = helix_data, mapping = aes(x = h_cohort, y = hs_pm25_yr_hs_s, color = h_cohort)) + 
  geom_violin() + theme_classic() + xlab('Cohort') + ylab('PM25 ug/m3 at school') + ylim(c(0, 50)) +
  scale_color_viridis(discrete=TRUE) + theme(legend.position="none") + geom_boxplot(alpha = 0.4, width = 0.2, color = 'black') + 
  geom_jitter(alpha = 0.1)

# PM25 concentration at commute
PM25_commute_cohort_plot <- ggplot2::ggplot(data = helix_data, mapping = aes(x = h_cohort, y = hs_pm25_yr_hs_r, color = h_cohort)) + 
  geom_violin() + theme_classic() + xlab('Cohort') + ylab('PM25 ug/m3 at commute') + ylim(c(0, 50)) +
  scale_color_viridis(discrete=TRUE) + theme(legend.position="none") + geom_boxplot(alpha = 0.4, width = 0.2, color = 'black') + 
  geom_jitter(alpha = 0.1) 


PM25_home_cohort_plot + PM25_school_cohort_plot + PM25_commute_cohort_plot


### PM10 boxplot ### 

# PM10 concentration at home 
PM10_home_cohort_plot <- ggplot2::ggplot(data = helix_data, mapping = aes(x = h_cohort, y = hs_pm10_yr_hs_h, color = h_cohort)) + 
  geom_violin() + theme_classic() + xlab('Cohort') + ylab('PM10 ug/m3 at home') + ylim(c(0, 100)) +
  scale_color_viridis(discrete=TRUE) + theme(legend.position="none") + geom_boxplot(alpha = 0.4, width = 0.2, color = 'black') + 
  geom_jitter(alpha = 0.1)

# PM10 concentration at school 
PM10_school_cohort_plot <- ggplot2::ggplot(data = helix_data, mapping = aes(x = h_cohort, y = hs_pm10_yr_hs_s, color = h_cohort)) + 
  geom_violin() + theme_classic() + xlab('Cohort') + ylab('PM10 ug/m3 at school') + ylim(c(0, 100)) +
  scale_color_viridis(discrete=TRUE) + theme(legend.position="none") + geom_boxplot(alpha = 0.4, width = 0.2, color = 'black') + 
  geom_jitter(alpha = 0.1)

# PM10 concentration at commute
PM10_commute_cohort_plot <- ggplot2::ggplot(data = helix_data, mapping = aes(x = h_cohort, y = hs_pm10_yr_hs_r, color = h_cohort)) + 
  geom_violin() + theme_classic() + xlab('Cohort') + ylab('PM10 ug/m3 at commute') + ylim(c(0, 100)) +
  scale_color_viridis(discrete=TRUE) + theme(legend.position="none") + geom_boxplot(alpha = 0.4, width = 0.2, color = 'black') + 
  geom_jitter(alpha = 0.1) 


PM10_home_cohort_plot + PM10_school_cohort_plot + PM10_commute_cohort_plot


### NO2 boxplot ### 

# NO2 concentration at home 
NO2_Home_cohort_plot <- ggplot2::ggplot(data = helix_data, mapping = aes(x = h_cohort, y = hs_no2_yr_hs_h, color = h_cohort)) + 
  geom_violin() + theme_classic() + xlab('Cohort') + ylab('NO2 ug/m3 at home') + ylim(c(0, 250)) +
  scale_color_viridis(discrete=TRUE) + theme(legend.position="none") + geom_boxplot(alpha = 0.4, width = 0.2, color = 'black') + 
  geom_jitter(alpha = 0.1)

#NO2 concentration at school 
NO2_school_cohort_plot <- ggplot2::ggplot(data = helix_data, mapping = aes(x = h_cohort, y = hs_no2_yr_hs_s, color = h_cohort)) + 
   geom_violin() + theme_classic() + xlab('Cohort') + ylab('NO2 ug/m3 at school') + ylim(c(0, 250)) +
  scale_color_viridis(discrete=TRUE) + theme(legend.position="none") + geom_boxplot(alpha = 0.4, width = 0.2, color = 'black') + 
  geom_jitter(alpha = 0.1)

#NO2 concentration at commute
NO2_commute_cohort_plot <- ggplot2::ggplot(data = helix_data, mapping = aes(x = h_cohort, y = hs_no2_yr_hs_r, color = h_cohort)) + 
  geom_violin() + theme_classic() + xlab('Cohort') + ylab('NO2 ug/m3 at commute') + ylim(c(0, 250)) +
  scale_color_viridis(discrete=TRUE) + theme(legend.position="none") + geom_boxplot(alpha = 0.4, width = 0.2, color = 'black') + 
  geom_jitter(alpha = 0.1) 


NO2_Home_cohort_plot + NO2_school_cohort_plot + NO2_commute_cohort_plot

### PM25 abs boxplot ### 

# PM25 abs concentration at home 
PM25abs_home_cohort_plot <- ggplot2::ggplot(data = helix_data, mapping = aes(x = h_cohort, y = hs_pm25abs_yr_hs_h, color = h_cohort)) + 
  geom_violin() + theme_classic() + xlab('Cohort') + ylab('PM25 abs ug/m3 at home') + ylim(c(0, 15)) +
  scale_color_viridis(discrete=TRUE) + theme(legend.position="none") + geom_boxplot(alpha = 0.4, width = 0.2, color = 'black') + 
  geom_jitter(alpha = 0.1)

# PM25 abs concentration at school 
PM25abs_school_cohort_plot <- ggplot2::ggplot(data = helix_data, mapping = aes(x = h_cohort, y = hs_pm25abs_yr_hs_s, color = h_cohort)) + 
  geom_violin() + theme_classic() + xlab('Cohort') + ylab('PM25 abs ug/m3 at school') + ylim(c(0, 15)) +
  scale_color_viridis(discrete=TRUE) + theme(legend.position="none") + geom_boxplot(alpha = 0.4, width = 0.2, color = 'black') + 
  geom_jitter(alpha = 0.1)

# PM25 abs concentration at commute
PM25abs_commute_cohort_plot <- ggplot2::ggplot(data = helix_data, mapping = aes(x = h_cohort, y = hs_pm25abs_yr_hs_r, color = h_cohort)) + 
  geom_violin() + theme_classic() + xlab('Cohort') + ylab('PM25 abs ug/m3 at commute') + ylim(c(0, 15)) +
  scale_color_viridis(discrete=TRUE) + theme(legend.position="none") + geom_boxplot(alpha = 0.4, width = 0.2, color = 'black') + 
  geom_jitter(alpha = 0.1) 


PM25abs_home_cohort_plot + PM25abs_school_cohort_plot + PM25abs_commute_cohort_plot

### NDVI boxplot ### hs_ndvi100_ hs_ndvi100_p

# NDVI exposure at home 
NDVI_home_cohort_plot <- ggplot2::ggplot(data = helix_data, mapping = aes(x = h_cohort, y = hs_ndvi100_h, color = h_cohort)) + 
  geom_violin() + theme_classic() + xlab('Cohort') + ylab('NDVI 100 m - home') +
  scale_color_viridis(discrete=TRUE) + theme(legend.position="none") + geom_boxplot(alpha = 0.4, width = 0.2, color = 'black') + 
  geom_jitter(alpha = 0.1) + ylim(c(0 , 1))

# NDVI exposure at school 
NDVI_school_cohort_plot <- ggplot2::ggplot(data = helix_data, mapping = aes(x = h_cohort, y = hs_ndvi100_s, color = h_cohort)) + 
  geom_violin() + theme_classic() + xlab('Cohort') + ylab('NDVI 100 m - school') +
  scale_color_viridis(discrete=TRUE) + theme(legend.position="none") + geom_boxplot(alpha = 0.4, width = 0.2, color = 'black') + 
  geom_jitter(alpha = 0.1) + ylim(c(0 , 1))

# NDVI exposure at commute
NDVI_commute_cohort_plot <- ggplot2::ggplot(data = helix_data, mapping = aes(x = h_cohort, y = hs_ndvi100_r, color = h_cohort)) + 
  geom_violin() + theme_classic() + xlab('Cohort') + ylab('NDVI 100 m - commute') +
  scale_color_viridis(discrete=TRUE) + theme(legend.position="none") + geom_boxplot(alpha = 0.4, width = 0.2, color = 'black') + 
  geom_jitter(alpha = 0.1)  + ylim(c(0 , 1))


NDVI_home_cohort_plot + NDVI_school_cohort_plot + NDVI_commute_cohort_plot

### Noise boxplot ### hs_ndvi100_ hs_ndvi100_p

# Lden exposure at home 
lden_home_cohort_plot <- ggplot2::ggplot(data = helix_data, mapping = aes(x = h_cohort, y = hs_lden_h, color = h_cohort)) + 
  geom_violin() + theme_classic() + xlab('Cohort') + ylab('Lden (db) - home') +
  scale_color_viridis(discrete=TRUE) + theme(legend.position="none") + geom_boxplot(alpha = 0.4, width = 0.2, color = 'black') + 
  geom_jitter(alpha = 0.1) + ylim(c(0 , 80))

# Lden exposure at school 
lden_school_cohort_plot <- ggplot2::ggplot(data = helix_data, mapping = aes(x = h_cohort, y = hs_lden_s, color = h_cohort)) + 
  geom_violin() + theme_classic() + xlab('Cohort') + ylab('Lden (db) - school') +
  scale_color_viridis(discrete=TRUE) + theme(legend.position="none") + geom_boxplot(alpha = 0.4, width = 0.2, color = 'black') + 
  geom_jitter(alpha = 0.1) + ylim(c(0 , 80))

# Lden exposure at commute
lden_commute_cohort_plot <- ggplot2::ggplot(data = helix_data, mapping = aes(x = h_cohort, y = hs_lden_r, color = h_cohort)) + 
  geom_violin() + theme_classic() + xlab('Cohort') + ylab('Lden (db) - commute') +
  scale_color_viridis(discrete=TRUE) + theme(legend.position="none") + geom_boxplot(alpha = 0.4, width = 0.2, color = 'black') + 
  geom_jitter(alpha = 0.1)  + ylim(c(0 , 80))


lden_home_cohort_plot + lden_school_cohort_plot + lden_commute_cohort_plot

helix_data %>% select(h_cohort, h_lden_preg) %>% 
  group_by(h_cohort) %>% 
  skimr::skim()

helix_data$h_lden_preg

helix_data$h_lden_pre


