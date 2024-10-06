# 06.- Correlations between exposures

pacman::p_load(tidyverse, DataExplorer, skimr, haven, patchwork, hablar, rio, magrittr, ggridges, 
               viridis, hrbrthemes, gtsummary, janitor, flextable, kableExtra, corrplot, summarytools,
               foreign, psych, car, readstata13, SmartEDA, GGally, ggcorrplot)

# load the functions created previously 
source("Code/functions_tfm.R") 

# load data 
helix_data <- readxl::read_xlsx("Data/clean/helix_db_clean_2.xlsx")


###################
### Correlation ##########################################################################################
################## 


##############################
#### prenatal correlation ####
#############################

glimpse(helix_data) # check the names of the variables

# subset the data 

prenatal_air <- helix_data %>% select(h_no2_ratio_preg, h_no2_ratio_t1, h_no2_ratio_t2, h_no2_ratio_t3, # nitrogen dioxide - prenatal exposure
                                      h_pm10_ratio_preg, h_pm10_ratio_t1, h_pm10_ratio_t2, h_pm10_ratio_t3, # pm10 - prenatal exposure
                                      h_pm25_ratio_preg, h_pm25_ratio_t1, h_pm25_ratio_t2, h_pm25_ratio_t3,   # pm25 - prenatal exposure
                                      h_abs_ratio_preg, h_abs_ratio_t1, h_abs_ratio_t2, h_abs_ratio_t3) # pm25abs ratio - prenatal exposure


colnames(prenatal_air)<-c("NO2_entire_preg", "NO2_T1", "NO2_T2", "NO2_T3", #NO2 prenatal exposures
                           "PM10_entire_preg", "PM10_T1", "PM10_T2", "PM10_T3", # PM10 prenatal exposures
                           "PM25_entire_preg", "PM25_T1", "PM25_T2", "PM25_T3", # PM25 prenatal exposures
                           "abs_entire_preg", "abs_T1", "abs_T2", "abs_T3") # PM25 abs prenatal exposures


# prenatal exposures 
ggcorrplot(cor(prenatal_air, method = "spearman", use = "complete.obs")
           , type = "upper", show.diag = TRUE, lab = TRUE, legend.title = "Spearman \n Correlation") +
  theme_classic() + xlab('') + ylab('') + 
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) 




#############################
### postnatal correlation ###
#############################

# we only select the pollutants that we going to use for the correlation matrix
postnatal_air <- helix_data %>% select(hs_no2_yr_hs_h, hs_no2_yr_hs_s, hs_no2_yr_hs_r, # NO2
                                       hs_pm10_yr_hs_h, hs_pm10_yr_hs_s, hs_pm10_yr_hs_r, # PM10
                                       hs_pm25_yr_hs_h, hs_pm25_yr_hs_s, hs_pm25_yr_hs_r, # PM25
                                       hs_pm25abs_yr_hs_h, hs_pm25abs_yr_hs_s, hs_pm25abs_yr_hs_r)  # PM25 abs 

colnames(postnatal_air)<-c("NO2_home", "NO2_school", "NO2_commute", #NO2
                           "PM10_home", "PM10_school", "PM10_commute",
                           "PM25_home", "PM25_school", "PM25_commute",
                           "abs_home", "abs_school", "abs_commute")

ggcorrplot(cor(postnatal_air, method = "spearman", use = "complete.obs")
           , type = "upper", show.diag = TRUE, lab = TRUE, legend.title = "Spearman \n Correlation") +
  theme_classic() + xlab('') + ylab('') + 
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) 
  



##############################        ############################
#### prenatal correlation ####   +    ### postnatal correlation###
#############################         ############################


# we only select the pollutants that we going to use for the correlation matrix

pre_post_aire <- helix_data %>% select(h_no2_ratio_preg, # NO2- prenatal exposure
                                       h_pm10_ratio_preg, # PM10- prenatal exposure
                                       h_pm25_ratio_preg,   # PM25 - prenatal exposure
                                       h_abs_ratio_preg, # PM25 abs - prenatal exposure
                                       hs_no2_yr_hs_h, hs_no2_yr_hs_s, hs_no2_yr_hs_r, # NO2 - postnatal exposure
                                       hs_pm10_yr_hs_h, hs_pm10_yr_hs_s, hs_pm10_yr_hs_r, # PM10 - postnatal exposure
                                       hs_pm25_yr_hs_h, hs_pm25_yr_hs_s, hs_pm25_yr_hs_r, # PM25 - postnatal exposure
                                       hs_pm25abs_yr_hs_h, hs_pm25abs_yr_hs_s, hs_pm25abs_yr_hs_r)# PM25 abs - postnatal exposure
                                      

colnames(pre_post_aire) <- c("NO2_entire_preg", #NO2 prenatal exposures
                          "PM10_entire_preg", # PM10 prenatal exposures
                          "PM25_entire_preg", # PM25 prenatal exposures
                          "abs_entire_preg",
                          "NO2_home", "NO2_school", "NO2_commute", #NO2
                          "PM10_home", "PM10_school", "PM10_commute",
                          "PM25_home", "PM25_school", "PM25_commute",
                          "abs_home", "abs_school", "abs_commute") 
      

# final list of exposures 

final_exposures <- helix_data %>% select(h_no2_ratio_preg, # NO2 prenatal
                                         h_pm25_ratio_preg, # PM25 prenatal
                                         hs_no2_yr_hs_h, hs_no2_yr_hs_s, hs_no2_yr_hs_r, # NO2 postnatal microenvironments
                                         hs_pm25_yr_hs_h, hs_pm25_yr_hs_s, hs_pm25_yr_hs_r) # PM25 postnatal microenvironments 


colnames(final_exposures) <- c("NO2_pre", #NO2 prenatal exposures
                                "PM25_pre", # PM25 prenatal exposures
                                "NO2_post_home", "NO2_post_school", "NO2_post_commute", #NO2,
                                "PM25_post_home", "PM25_post_school", "PM25_post_commute") 


# correlation plot for final exposures
ggcorrplot(cor(final_exposures, method = "spearman", use = "complete.obs")
           , type = "lower", show.diag = TRUE, lab = TRUE, legend.title = "Spearman \n Correlation") +
  theme_classic() + xlab('') + ylab('') + 
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) 


#########################################################################################################################


### correlation plot by cohort ###

data_BIB <- dplyr::filter(helix_data, h_cohort == 'BIB') # create a dataset with only BIB children
data_EDEN <- dplyr::filter(helix_data, h_cohort == 'EDEN') # create a dataset with only EDEN children
data_KANC <- dplyr::filter(helix_data, h_cohort == 'KANC') # create a dataset with only KANC children
data_MOBA <- dplyr::filter(helix_data, h_cohort == 'MOBA') # create a dataset with only KANC children
data_RHEA <- dplyr::filter(helix_data, h_cohort == 'RHEA') # create a dataset with only RHEA children
data_SAB <- dplyr::filter(helix_data, h_cohort == 'SAB') # create a dataset with only SAB children



###########
### BIB ###
###########

# Subset of final exposures
final_exposures <- data_BIB %>% select(hs_no2_yr_hs_h, hs_no2_yr_hs_s, hs_no2_yr_hs_r, # NO2 postnatal microenvironments
                                       hs_pm25_yr_hs_h, hs_pm25_yr_hs_s, hs_pm25_yr_hs_r) # PM25 postnatal microenvironments 


colnames(final_exposures) <- c("NO2 home", "NO2 school", "NO2 commute", 
                               "PM25 home", "PM25 school", "PM25 commute") 

BIB_corplot <- ggcorrplot(cor(final_exposures, method = "spearman", use = "complete.obs")
           , type = "lower", show.diag = TRUE, lab = TRUE, legend.title = "Spearman \n Correlation") +
  theme_classic() + xlab('') + ylab('') + 
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) + 
  facet_grid(. ~ "BIB")




ggsave("output/figures/correlation_plot_BIB.tiff", width = 5, height = 5, dpi = 400) 

############
### EDEN ###
############

final_exposures <- data_EDEN %>% select(hs_no2_yr_hs_h, hs_no2_yr_hs_s, hs_no2_yr_hs_r, # NO2 postnatal microenvironments
                                       hs_pm25_yr_hs_h, hs_pm25_yr_hs_s, hs_pm25_yr_hs_r) # PM25 postnatal microenvironments 


colnames(final_exposures) <- c("NO2 home", "NO2 school", "NO2 commute", 
                               "PM25 home", "PM25 school", "PM25 commute") 

EDEN_corplot <- ggcorrplot(cor(final_exposures, method = "spearman", use = "complete.obs")
                          , type = "lower", show.diag = TRUE, lab = TRUE, legend.title = "Spearman \n Correlation") +
  theme_classic() + xlab('') + ylab('') + 
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) + 
  facet_grid(. ~ "EDEN")


############
### KANC ###
############

final_exposures <- data_KANC %>% select(hs_no2_yr_hs_h, hs_no2_yr_hs_s, hs_no2_yr_hs_r, # NO2 postnatal microenvironments
                                        hs_pm25_yr_hs_h, hs_pm25_yr_hs_s, hs_pm25_yr_hs_r) # PM25 postnatal microenvironments 


colnames(final_exposures) <- c("NO2 home", "NO2 school", "NO2 commute", 
                               "PM25 home", "PM25 school", "PM25 commute") 

KANC_corplot <- ggcorrplot(cor(final_exposures, method = "spearman", use = "complete.obs")
                           , type = "lower", show.diag = TRUE, lab = TRUE, legend.title = "Spearman \n Correlation") +
  theme_classic() + xlab('') + ylab('') + 
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) + 
  facet_grid(. ~ "KANC")


############
### MOBA ###
############

final_exposures <- data_MOBA %>% select(hs_no2_yr_hs_h, hs_no2_yr_hs_s, hs_no2_yr_hs_r, # NO2 postnatal microenvironments
                                        hs_pm25_yr_hs_h, hs_pm25_yr_hs_s, hs_pm25_yr_hs_r) # PM25 postnatal microenvironments 


colnames(final_exposures) <- c("NO2 home", "NO2 school", "NO2 commute", 
                               "PM25 home", "PM25 school", "PM25 commute") 

MOBA_corplot <- ggcorrplot(cor(final_exposures, method = "spearman", use = "complete.obs")
                           , type = "lower", show.diag = TRUE, lab = TRUE, legend.title = "Spearman \n Correlation") +
  theme_classic() + xlab('') + ylab('') + 
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) + 
  facet_grid(. ~ "MOBA")


############
### RHEA ###
############

final_exposures <- data_RHEA %>% select(hs_no2_yr_hs_h, hs_no2_yr_hs_s, hs_no2_yr_hs_r, # NO2 postnatal microenvironments
                                        hs_pm25_yr_hs_h, hs_pm25_yr_hs_s, hs_pm25_yr_hs_r) # PM25 postnatal microenvironments 


colnames(final_exposures) <- c("NO2 home", "NO2 school", "NO2 commute", 
                               "PM25 home", "PM25 school", "PM25 commute") 

RHEA_corplot <- ggcorrplot(cor(final_exposures, method = "spearman", use = "complete.obs")
                           , type = "lower", show.diag = TRUE, lab = TRUE, legend.title = "Spearman \n Correlation") +
  theme_classic() + xlab('') + ylab('') + 
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) + 
  facet_grid(. ~ "RHEA")


############
### SAB ###
############

final_exposures <- data_SAB %>% select(hs_no2_yr_hs_h, hs_no2_yr_hs_s, hs_no2_yr_hs_r, # NO2 postnatal microenvironments
                                        hs_pm25_yr_hs_h, hs_pm25_yr_hs_s, hs_pm25_yr_hs_r) # PM25 postnatal microenvironments 


colnames(final_exposures) <- c("NO2 home", "NO2 school", "NO2 commute", 
                               "PM25 home", "PM25 school", "PM25 commute") 

SAB_corplot <- ggcorrplot(cor(final_exposures, method = "spearman", use = "complete.obs")
                           , type = "lower", show.diag = TRUE, lab = TRUE, legend.title = "Spearman \n Correlation") +
  theme_classic() + xlab('') + ylab('') + 
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) + 
  facet_grid(. ~ "SAB")



# final plot (add title)
(BIB_corplot +  EDEN_corplot + KANC_corplot) / (MOBA_corplot + RHEA_corplot + SAB_corplot)

ggsave("output/figures/correlation_plot_all_cohorts.tiff", width = 16, height = 10, dpi = 400) 























#################### between and within variability ################

# within exposures home 
within_home_plot <- ggplot2::ggplot(data = helix_data, mapping = aes(x = hs_pm25_yr_hs_h, y = hs_no2_yr_hs_h)) +
  geom_point(alpha = 0.6) + geom_smooth(method = "lm") + theme_classic()  +
  xlab("PM25 at home address") + ylab("NO2 at home address")

# between exposures home                           
between_home_plot <- ggplot2::ggplot(data = helix_data, mapping = aes(x = hs_pm25_yr_hs_h, y = hs_no2_yr_hs_h, color = h_cohort)) +
  geom_point(alpha = 0.6) + geom_smooth(method = "lm") + theme_classic() + 
  scale_fill_viridis(discrete=TRUE) + scale_color_viridis(discrete=TRUE) +
  xlab("PM25 at home address") + ylab("NO2 at home address")

# within exposures school
within_school_plot <- ggplot2::ggplot(data = helix_data, mapping = aes(x = hs_pm25_yr_hs_s, y = hs_no2_yr_hs_s)) +
  geom_point(alpha = 0.6) + geom_smooth(method = "lm") + theme_classic() +
  xlab("PM25 at school") + ylab("NO2 at school")

# between exposures school                           
between_school_plot <- ggplot2::ggplot(data = helix_data, mapping = aes(x = hs_pm25_yr_hs_s, y = hs_no2_yr_hs_s, color = h_cohort)) +
  geom_point(alpha = 0.6) + geom_smooth(method = "lm") + theme_classic() + 
  scale_fill_viridis(discrete=TRUE) + scale_color_viridis(discrete=TRUE) +
  xlab("PM25 at school") + ylab("NO2 at school")

# within exposures commute
within_commute_plot <- ggplot2::ggplot(data = helix_data, mapping = aes(x = hs_pm25_yr_hs_r, y = hs_no2_yr_hs_r)) +
  geom_point(alpha = 0.6) + geom_smooth(method = "lm") + theme_classic() +
  xlab("PM25 at commute") + ylab("NO2 at commute")

# between exposures commute                         
between_commute_plot <- ggplot2::ggplot(data = helix_data, mapping = aes(x = hs_pm25_yr_hs_r, y = hs_no2_yr_hs_r, color = h_cohort)) +
  geom_point(alpha = 0.6) + geom_smooth(method = "lm") + theme_classic() + 
  scale_fill_viridis(discrete=TRUE) + scale_color_viridis(discrete=TRUE) +
  xlab("PM25 at commute") + ylab("NO2 at commute")



(within_home_plot/ between_home_plot) | (within_school_plot / between_school_plot) | (within_commute_plot / between_commute_plot)  


###################################################################################################################

##########################################
### scatterplot outcome - exposures ######
#########################################


### attentional function #### 












### non verbal intelligence ###

# raven - no2 home - by cohort 
ggplot(helix_data, mapping = aes(x = hs_no2_yr_hs_h , y = hs_correct_raven, color = h_cohort)) + 
  geom_point() + geom_smooth(method = "lm") + geom_smooth(method = "lm") +
  theme_classic() + scale_fill_viridis(discrete=TRUE) + scale_color_viridis(discrete=TRUE)

# raven - no2 commute - by cohort
ggplot(helix_data, mapping = aes(x = hs_no2_yr_hs_r , y = hs_correct_raven, color = h_cohort)) + 
  geom_point() + geom_smooth(method = "lm") + geom_smooth(method = "lm") +
  theme_classic() + scale_fill_viridis(discrete=TRUE) + scale_color_viridis(discrete=TRUE)


# raven - pm25 home - by cohort 
ggplot(helix_data, mapping = aes(x = hs_pm25_yr_hs_h , y = hs_correct_raven, color = h_cohort)) + 
  geom_point() + geom_smooth(method = "lm") + geom_smooth(method = "lm") +
  theme_classic() + scale_fill_viridis(discrete=TRUE) + scale_color_viridis(discrete=TRUE)

# raven - pm25 commute - by cohort
ggplot(helix_data, mapping = aes(x = hs_pm25_yr_hs_r , y = hs_correct_raven, color = h_cohort)) + 
  geom_point() + geom_smooth(method = "lm") + geom_smooth(method = "lm") +
  theme_classic() + scale_fill_viridis(discrete=TRUE) + scale_color_viridis(discrete=TRUE)





















































# Change colors and theme
# --------------------------------
# Argument colors
ggcorrplot(corr, hc.order = TRUE, type = "lower",
           outline.col = "white",
           ggtheme = ggplot2::theme_gray,
           colors = c("#6D9EC1", "white", "#E46726"))


### viridis corrplot ### try to change the midpoint 
postnatal_exposures <- ggcorrplot(cor(postnatal_air, method = "spearman", use = "complete.obs"), type = "upper", 
           show.diag = TRUE, lab = TRUE) + theme_classic() + xlab('') + ylab('') + 
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
  scale_fill_viridis(option= "inferno", name = "Spearman \n Correlation", limits = c(-1,1))
  
ggcorrplot()






















