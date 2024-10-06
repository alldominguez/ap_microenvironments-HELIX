# 08.- Variability

pacman::p_load(tidyverse, DataExplorer, skimr, haven, patchwork, hablar, rio, magrittr, ggridges, 
               viridis, hrbrthemes, gtsummary, janitor, flextable, kableExtra, corrplot, summarytools,
               foreign, psych, car, readstata13, SmartEDA, GGally, ggcorrplot, 
               naniar, mice, visdat, irr, rstatix, nlme) # packages for missing values

# load the functions created previously 
source("Code/functions_tfm.R") 

# load data 
helix_data <- readxl::read_xlsx("Data/clean/helix_db_clean_2.xlsx")


outcomes_helix %>% dplyr::glimpse()

######################################################################

### outcome variabiliyy by cohort ### 


#########################
# Attentional function #
########################

ggplot2::ggplot(data = outcomes_helix, mapping = aes(x = h_cohort, y = hs_hitrtse, color = h_cohort)) + 
  geom_boxplot(alpha = 0.4, width = 0.2, color = 'black', outlier.shape =  NA) + geom_jitter(alpha = 0.2) +
  theme_classic() + scale_fill_viridis(discrete=TRUE) + scale_color_viridis(discrete=TRUE) + 
  xlab("Cohort") + ylab("Hit reaction time SE (ms)") + theme(legend.position = 'none') + 
  coord_flip() + ylim(c(0, 700))


ICC1.lme(hs_hitrtse, h_cohort, helix_data) # 0.3400862

#########################
# Cognitive flexibility #
########################

# task switching # 
ggplot2::ggplot(data = outcomes_helix, mapping = aes(x = h_cohort, y = hs_tmtb_responsetime, color = h_cohort)) + 
  geom_boxplot(alpha = 0.4, width = 0.2, color = 'black', outlier.shape = NA) + geom_jitter(alpha = 0.2) +
  theme_classic() + scale_fill_viridis(discrete=TRUE) + scale_color_viridis(discrete=TRUE) + 
  xlab("Cohort") + ylab("TMTB hit response time (ms)") + theme(legend.position = 'none') + 
  coord_flip() 

ICC1.lme(hs_tmtb_responsetime, h_cohort, helix_data) # 0.4231884


# task shifting #
ggplot2::ggplot(data = outcomes_helix, mapping = aes(x = h_cohort, y = task_shifting_score, color = h_cohort)) + 
  geom_boxplot(alpha = 0.4, width = 0.2, color = 'black', outlier.shape =  NA) + geom_jitter(alpha = 0.2) +
  theme_classic() + scale_fill_viridis(discrete=TRUE) + scale_color_viridis(discrete=TRUE) + 
  xlab("Cohort") + ylab("Task shifting score") + theme(legend.position = 'none') + 
  coord_flip() 

ICC1.lme(task_shifting_score, h_cohort, helix_data) # 0.03415478

############################
# Non verbal intelligence #
###########################

ggplot2::ggplot(data = outcomes_helix, mapping = aes(x = h_cohort, y = hs_correct_raven, color = h_cohort)) + 
  geom_boxplot(alpha = 0.4, width = 0.2, color = 'black', outlier.shape =  NA) + geom_jitter(alpha = 0.2) +
  theme_classic() + scale_fill_viridis(discrete=TRUE) + scale_color_viridis(discrete=TRUE) + 
  xlab("Cohort") + ylab("Raven score") + theme(legend.position = 'none') + 
  coord_flip() + ylim(c(0, 50))

ICC1.lme(hs_correct_raven, h_cohort, helix_data) # 0.5809456

#######################
# Fine motor function # 
######################

# sum trial dominant hand # 
ggplot2::ggplot(data = outcomes_helix, mapping = aes(x = h_cohort, y = hs_sum_domhand, color = h_cohort)) + 
  geom_boxplot(alpha = 0.4, width = 0.2, color = 'black', outlier.shape = NA) + geom_jitter(alpha = 0.2) +
  theme_classic() + scale_fill_viridis(discrete=TRUE) + scale_color_viridis(discrete=TRUE) + 
  xlab("Cohort") + ylab("Sum trial dominant hand") + theme(legend.position = 'none') + 
  coord_flip() 

ICC1.lme(hs_sum_domhand, h_cohort, helix_data) # 0.5571184

# laterality index # 
ggplot2::ggplot(data = outcomes_helix, mapping = aes(x = h_cohort, y = hs_laterality, color = h_cohort)) + 
  geom_boxplot(alpha = 0.4, width = 0.2, color = 'black' ,outlier.shape = NA) + geom_jitter(alpha = 0.2) +
  theme_classic() + scale_fill_viridis(discrete=TRUE) + scale_color_viridis(discrete=TRUE) + 
  xlab("Cohort") + ylab("Laterality index") + theme(legend.position = 'none') + 
  coord_flip()+ ylim(c(0, 30))

ICC1.lme(hs_laterality, h_cohort, helix_data) # 0.02915707

######################################################################

### outcome variability by cohort ### 

###########
### NO2 ### 
###########

# Prenatal exposure to NO2 #
ggplot2::ggplot(data = helix_data, mapping = aes(x = h_cohort, y = h_no2_ratio_preg, color = h_cohort)) +
  geom_boxplot(alpha = 0.4, width = 0.2, color = 'black' ,outlier.shape = NA) + geom_jitter(alpha = 0.2) +
  theme_classic() + scale_fill_viridis(discrete=TRUE) + scale_color_viridis(discrete=TRUE) + 
  xlab("Cohort") + ylab("Prenatal NO2 (ug/m3)") + theme(legend.position = 'none') + 
  coord_flip() + ylim(c(0, 80))

ICC1.lme(h_no2_ratio_preg, h_cohort, helix_data) # 0.7333192


# Postnatal exposure to NO2 at home #
NO2_home <- ggplot2::ggplot(data = helix_data, mapping = aes(x = h_cohort, y = hs_no2_yr_hs_h, color = h_cohort)) +
  geom_boxplot(alpha = 0.4, width = 0.2 , outlier.shape = NA) + 
  theme_classic() + scale_fill_viridis(discrete=TRUE) + scale_color_viridis(discrete=TRUE) + 
  xlab("Cohort") + ylab(expression(" " * mu * "g/m" ^3 * "")) + theme(legend.position = 'none') + 
  coord_flip() + 
  facet_wrap(. ~ "NO[2] - home", labeller = label_parsed) + 
  theme(strip.text.x = element_text(size = 11))

ICC1.lme(hs_no2_yr_hs_h, h_cohort, helix_data) # 0.738813

# Postnatal exposure to NO2 at school #
NO2_school <- ggplot2::ggplot(data = helix_data, mapping = aes(x = h_cohort, y = hs_no2_yr_hs_s, color = h_cohort)) +
  geom_boxplot(alpha = 0.4, width = 0.2, outlier.shape = NA) + 
  theme_classic() + scale_fill_viridis(discrete=TRUE) + scale_color_viridis(discrete=TRUE) + 
  xlab("Cohort") + ylab(expression(" " * mu * "g/m" ^3 * "")) + theme(legend.position = 'none') + 
  coord_flip() + 
  facet_wrap(. ~ "NO[2] - school", labeller = label_parsed) + 
  theme(strip.text.x = element_text(size = 11))

ICC1.lme(hs_no2_yr_hs_s, h_cohort, helix_data) # 0.7582753

# Postnatal exposure to NO2 at commute #
NO2_commute <- ggplot2::ggplot(data = helix_data, mapping = aes(x = h_cohort, y = hs_no2_yr_hs_r, color = h_cohort)) +
  geom_boxplot(alpha = 0.4, width = 0.2, outlier.shape = NA) + 
  theme_classic() + scale_fill_viridis(discrete=TRUE) + scale_color_viridis(discrete=TRUE) + 
  xlab("Cohort") + ylab(expression(" " * mu * "g/m" ^3 * "")) + theme(legend.position = 'none') + 
  coord_flip() + 
  facet_wrap(. ~ "NO[2] - commute", labeller = label_parsed) + 
  theme(strip.text.x = element_text(size = 11))

ICC1.lme(hs_no2_yr_hs_r, h_cohort, helix_data) # 0.3151554


NO2_microenvironment <- (NO2_home / NO2_school / NO2_commute)

############
### PM25 ### 
###########

# Prenatal exposure to PM25 #
ggplot2::ggplot(data = helix_data, mapping = aes(x = h_cohort, y = h_pm25_ratio_preg, color = h_cohort)) +
  geom_boxplot(alpha = 0.4, width = 0.2, color = 'black' ,outlier.shape = NA) + 
  theme_classic() + scale_fill_viridis(discrete=TRUE) + scale_color_viridis(discrete=TRUE) + 
  xlab("Cohort") + ylab(expression(" " * mu * "g/m" ^3 * "")) + theme(legend.position = 'none') + 
  coord_flip() + ylim(c(0, 30)) 

ICC1.lme(h_pm25_ratio_preg, h_cohort, helix_data) #  0.5299789

# Postnatal exposure to PM25 at home #
PM25_home <- ggplot2::ggplot(data = helix_data, mapping = aes(x = h_cohort, y = hs_pm25_yr_hs_h, color = h_cohort)) +
  geom_boxplot(alpha = 0.4, width = 0.2 ,outlier.shape = NA) + 
  theme_classic() + scale_fill_viridis(discrete=TRUE) + scale_color_viridis(discrete=TRUE) + 
  xlab("") + ylab(expression(" " * mu * "g/m" ^3 * "")) + theme(legend.position = 'none') + 
  coord_flip() + ylim(c(0, 30)) + 
  facet_wrap(. ~ "PM[2.5] - home", labeller = label_parsed) + 
  theme(axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        axis.line.y = element_blank()) + 
  theme(strip.text.x = element_text(size = 11))

ICC1.lme(hs_pm25_yr_hs_h, h_cohort, helix_data) # 

# Postnatal exposure to PM25 at school #
PM25_school <- ggplot2::ggplot(data = helix_data, mapping = aes(x = h_cohort, y = hs_pm25_yr_hs_s, color = h_cohort)) +
  geom_boxplot(alpha = 0.4, width = 0.2 ,outlier.shape = NA) + 
  theme_classic() + scale_fill_viridis(discrete=TRUE) + scale_color_viridis(discrete=TRUE) + 
  xlab("") + ylab(expression(" " * mu * "g/m" ^3 * "")) + theme(legend.position = 'none') + 
  coord_flip() + ylim(c(0, 30)) + 
  facet_wrap(. ~ "PM[2.5] - school", labeller = label_parsed) + 
  theme(axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        axis.line.y = element_blank()) + 
  theme(strip.text.x = element_text(size = 11))
  
  
ICC1.lme(hs_pm25_yr_hs_s, h_cohort, helix_data) # 0.8551192

# Postnatal exposure to PM25 at commute #
PM25_commute <- ggplot2::ggplot(data = helix_data, mapping = aes(x = h_cohort, y = hs_pm25_yr_hs_r, color = h_cohort)) +
  geom_boxplot(alpha = 0.4, width = 0.2 ,outlier.shape = NA) + 
  theme_classic() + scale_fill_viridis(discrete=TRUE) + scale_color_viridis(discrete=TRUE) + 
  xlab("") + ylab(expression(" " * mu * "g/m" ^3 * "")) + theme(legend.position = 'none') + 
  coord_flip() + ylim(c(0, 40)) + 
  facet_wrap(. ~ "PM[2.5] - commute", labeller = label_parsed) + 
  theme(axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        axis.line.y = element_blank()) + 
  theme(strip.text.x = element_text(size = 11))


ICC1.lme(hs_pm25_yr_hs_r, h_cohort, helix_data) # 0.6556376




# final plot exposure distribution by cohort
exposure_distribution <-  (NO2_home + PM25_home) / (NO2_school + PM25_school) / (NO2_commute + PM25_commute)

ggsave("output/figures/exposure_distribution.tiff", width = 16, height = 16, dpi = 400) 













