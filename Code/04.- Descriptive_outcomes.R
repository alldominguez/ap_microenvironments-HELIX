# 04.- Descriptive analysis outcomes

pacman::p_load(tidyverse, DataExplorer, skimr, haven, patchwork, hablar, rio, magrittr, ggridges, 
               viridis, hrbrthemes, gtsummary, janitor, flextable, kableExtra, corrplot, summarytools,
               foreign, psych, car, readstata13)

# load the functions created previously 
source("Code/functions_tfm.R") 

# load data 
helix_data <- readxl::read_xlsx("Data/clean/helix_db_clean_2.xlsx")

# check the structure of the data 
helix_data %>% dplyr::glimpse()

# subset our data, only select the 
outcomes_helix <- helix_data %>% select(hs_correct_raven:hs_hitrtse, h_cohort, task_shifting_score)

outcomes_helix %>% skimr::skim() # quick overview of the outcome data 

#################################
### Working memory (N-back test) ##########################################################################################
################################
colnames(outcomes_helix)

#####################
### 2 back color ### 
####################

# Hit react time 
tfm_summary_v2(var = hs_hitrt_meancolors2, group = h_cohort, data = outcomes_helix) %>%
  rio::export("output/tables/Nback_hitreac_color.xlsx") # export as xlsx

# d prima 
tfm_summary_v2(var = hs_dcolors2, group = h_cohort, data = outcomes_helix) %>% # negative values ?
  rio::export("output/tables/Nback_dprima_color.xlsx")# export as xlsx

######################
### 2 back number ### 
#####################

# Hit react time 
tfm_summary_v2(var = hs_hitrt_meannumeros2, group = h_cohort, data = outcomes_helix) %>% 
  rio::export("output/tables/Nback_hitreac_number.xlsx")# export as xlsx

# d prima 
tfm_summary_v2(var = hs_dnumeros2, group = h_cohort, data = outcomes_helix) %>% 
  rio::export("output/tables/Nback_dprima_number.xlsx")# export as xlsx

# IMPORTANT : KANC cohort has missing on all the outcomes (204 missings)

######################
### 3 back number ### 
#####################

# Hit react time 
tfm_summary_v2(var = hs_hitrt_meannumeros3, group = h_cohort, data = outcomes_helix) %>% 
  rio::export("output/tables/Nback_hitreac_number3.xlsx")# export as xlsx

# d prima 
tfm_summary_v2(var = hs_dnumeros3, group = h_cohort, data = outcomes_helix) %>% 
  rio::export("output/tables/Nback_dprima_number3.xlsx")# export as xlsx



####################################
### Attentional function (ANT task) ##########################################################################################
###################################

### hit react time mean ANT ### hs_hitrt2_mean
tfm_summary_v2(var = hs_hitrt2_mean, group = h_cohort, data = outcomes_helix) %>% 
  rio::export("output/tables/hit_react_mean_ANT.xlsx")

### hit reac time se ANT ### hs_hitrtse
tfm_summary_v2(var = hs_hitrtse, group = h_cohort, data = outcomes_helix) %>% 
  rio::export("output/tables/hit_react_se_ANT.xlsx")

### omissions errors ### are the same that missings (are intentional according to the definition)
tfm_summary_v2(var = hs_missings, group = h_cohort, data = outcomes_helix) %>% 
  rio::export("output/tables/omisions_ANT.xlsx")

### comissions errors ###
tfm_summary_v2(var = hs_comisions, group = h_cohort, data = outcomes_helix) %>% 
  rio::export("output/tables/comisions_ANT.xlsx")


######################################
### Cognitive flexibility (TMTA/TMTB) ##########################################################################################
#####################################

### Task switching score ###
tfm_summary_v2(var = hs_tmtb_responsetime, group = h_cohort, data = outcomes_helix) %>% 
  rio::export("output/tables/tmtb_responsetime_task_switching.xlsx")

### Task shifting score ### (this needs to be calculated)
tfm_summary_v2(var = task_shifting_score, group = h_cohort, data = outcomes_helix ) %>% 
  rio::export("output/tables/task_shifting_score.xlsx")

helix_data$hs_tmtb_responsetime

### tmta response time (ms) ### 
tfm_summary_v2(var = hs_tmta_responsetime, group = h_cohort, data = outcomes_helix) %>% 
  rio::export("output/tables/tmta_responsetime_task_switching.xlsx")


#########################################
### Non-verbal intelligence (Raven score) ####################################################################################################
########################################

### Raven score ###
tfm_summary_v2(var = hs_correct_raven, group = h_cohort, data = outcomes_helix) %>% 
  rio::export("output/tables/raven_score.xlsx")

##############################################
### Fine motor function (Finger tapping test) ####################################################################################################
#############################################

### Dominant hand score ### 
tfm_summary_v2(var = hs_sum_domhand, group = h_cohort, data = outcomes_helix) %>% 
  rio::export("output/tables/dominant_hand_fine_motor_function.xlsx")

### Laterality score ### 
tfm_summary_v2(var = hs_laterality, group = h_cohort, data = outcomes_helix) %>% 
  rio::export("output/tables/laterality_score_fine_motor_function.xlsx")


#################################################################################################################################################


##############################
########### PLOTS ########### # OUTCOMES # 
#############################

#######################
### Working memory ### - N-BACK TEST 
#####################

# 3 back number #

# mean hit reaction time (ms) / 3 back number 
hit_reac_time_mean_number_3back <- ggplot(data = helix_csv, mapping = aes(x = hs_hitrt_meannumeros3)) + 
  geom_density(fill = "#440154FF", col = 'white', alpha = 0.6) + theme_classic() + xlab("Hit reaction time (ms)") + ggtitle("3 back number")

# d prima / 3 back number #
d_prima_number_3back <- ggplot(data = helix_csv, mapping = aes(x= hs_dnumeros3)) + 
  geom_density(fill = "#440154FF", col = 'white', alpha = 0.6) + theme_classic() + xlab("d'") 

## 2 back color ##

# mean hit reaction time (ms) / 2 back color
hit_reac_time_mean_color_nback <- ggplot(data = helix_csv, mapping = aes(x = hs_hitrt_meancolors2)) + 
  geom_density(fill = "#440154FF", col = 'white', alpha = 0.6) + theme_classic() + xlab("Hit reaction time mean (ms)") + ggtitle("2 back color")

# d prima / 2 back color #
d_prima_color_nback <- ggplot(data = helix_csv , mapping = aes(x = hs_dcolors2)) + 
  geom_density(fill = "#440154FF", col = 'white', alpha = 0.6) + theme_classic() + xlab("d'")

## 2 back number ##

# mean hit reaction time (ms) / 2 back number 
hit_reac_time_mean_number_nback <- ggplot(data = helix_csv, mapping = aes(x = hs_hitrt_meannumeros2)) + 
  geom_density(fill = "#440154FF", col = 'white', alpha = 0.6) + theme_classic() + xlab("Hit reaction time (ms)") + ggtitle("2 back number")

# d prima / 2 back number #
d_prima_number_nback <- ggplot(data = helix_csv, mapping = aes(x= hs_dnumeros2)) + 
  geom_density(fill = "#440154FF", col = 'white', alpha = 0.6) + theme_classic() + xlab("d'") 

### join plot with patchwork
working_memory_density <- hit_reac_time_mean_number_3back / d_prima_number_3back | hit_reac_time_mean_number_nback / d_prima_number_nback
working_memory_density + plot_annotation(title = 'Working memory') 


### 3 back number ###  by cohort 

# hit reaction time mean (ms) 
ggplot(data = helix_data, aes(x = hs_hitrt_meannumeros3, y = h_cohort, fill = h_cohort))  +
  geom_density_ridges(alpha = 0.4, color = NA) + theme_classic() + 
  scale_fill_viridis(discrete=TRUE) + scale_color_viridis(discrete=TRUE) + 
  xlab("Hit reaction time mean (ms)") + ylab("Cohort") + theme(legend.position = 'none')

# d prima by cohort # 
ggplot(data = helix_data, aes(x = hs_dnumeros3, y = h_cohort, fill = h_cohort))  +
  geom_density_ridges(alpha = 0.4, color = NA) + theme_classic() + 
  scale_fill_viridis(discrete=TRUE) + scale_color_viridis(discrete=TRUE) + 
  xlab("d' 3 back number") + ylab("Cohort") + theme(legend.position = 'none')


############################
### Attentional function ### - Attention network task (ANT)
############################

# mean hit reaction time- ANT 
ANT_mean_hit_reac_time <- ggplot(data = helix_csv, mapping = aes(x = hs_hitrt2_mean)) + 
  geom_density(fill = "#39568CFF", col = 'black', alpha = 0.6) + theme_classic() + xlab("Hit reaction time mean")

# standard deviation (se) hit reaction time - ANT
ANT_se_hit_reac_time <- ggplot(data = helix_csv, mapping = aes(x = hs_hitrtse)) + 
  geom_density(fill = "#39568CFF", col = 'black', alpha = 0.6) + theme_classic() + xlab("Hit reaction time standard error")

# standard deviation (se) hit reaction time by cohort 
ggplot(data = helix_data, aes(x = hs_hitrtse, y = h_cohort, fill = h_cohort))  +
  geom_density_ridges(alpha = 0.4, color = NA) + theme_classic() + 
  scale_fill_viridis(discrete=TRUE) + scale_color_viridis(discrete=TRUE) + 
  xlab("Hit reaction time (se)") + ylab("Cohort") + theme(legend.position = 'none')


# omission errors - ANT
ANT_omission_errors <- ggplot(data = helix_csv, mapping = aes(x = hs_missings)) + 
  geom_density(fill = "#39568CFF", col = 'black', alpha = 0.6) + theme_classic() + xlab("Omissions errors")

# comissions errors - ANT 
ANT_comission_errors <- ggplot(data = helix_csv, mapping = aes(x = hs_comisions)) + 
  geom_density(fill = "#39568CFF", col = 'black', alpha = 0.6) + theme_classic() + xlab("Comissions errors")

### join plot with patchwork - Attentional function

attentional_function_density <- ANT_mean_hit_reac_time / ANT_se_hit_reac_time | ANT_omission_errors / ANT_comission_errors
attentional_function_density + plot_annotation(title = 'Attentional function') 

#############################
### Cognitive flexibility ### - Trail making test Part A/B 
############################

### Task switching  ### response time Trail Making Test B
TMTB_response_time <- ggplot(data = helix_data, mapping = aes(x = hs_tmtb_responsetime)) +
  geom_density(fill = "#1F968BFF", col = 'white', alpha = 0.6) + theme_classic() + xlab("Task switching (tmtb response time in ms)")

### Task shifting score ### - we need to calculate this  

helix_data %<>%  
  mutate(task_shifting_score =  (hs_tmtb_responsetime - hs_tmta_responsetime) /hs_tmta_responsetime)  ### ask to monica what to do with the negative values

task_shifting_density <- ggplot(data = helix_data, mapping = aes(x = task_shifting_score)) +
  geom_density(fill = "#1F968BFF", col = 'white', alpha = 0.6) + theme_classic() + xlab("Task shifting score")

### join plot with patchwork - Cognitive flexibility 
(TMTB_response_time | task_shifting_density) + plot_annotation(title = 'Cognitive flexibility') 


# task switching by cohort
ggplot(data = helix_data, aes(x = hs_tmtb_responsetime, y = h_cohort, fill = h_cohort))  +
  geom_density_ridges(alpha = 0.4, color = NA) + theme_classic() + 
  scale_fill_viridis(discrete=TRUE) + scale_color_viridis(discrete=TRUE) + 
  xlab("Task switching (tmtb responsetime in ms)") + ylab("Cohort") + theme(legend.position = 'none')

# task shifting score by cohort
ggplot(data = helix_data, aes(x = task_shifting_score, y = h_cohort, fill = h_cohort))  +
  geom_density_ridges(alpha = 0.4, color = NA) + theme_classic() + 
  scale_fill_viridis(discrete=TRUE) + scale_color_viridis(discrete=TRUE) + 
  xlab("Task shifting score") + ylab("Cohort") + theme(legend.position = 'none')


###############################
### Non Verbal Intelligence ### - Raven score 
##############################

non_verbal_IQ <- ggplot(data = helix_csv, aes(x = hs_correct_raven))  +
  geom_density(fill = "#B8DE29FF", col = 'black', alpha = 0.6) + theme_classic() + xlab("raven score")

# raven score by cohort 
ggplot(data = helix_data, aes(x = hs_correct_raven, y = h_cohort, fill = h_cohort))  +
  geom_density_ridges(alpha = 0.4, color = NA) + theme_classic() + 
  scale_fill_viridis(discrete=TRUE) + scale_color_viridis(discrete=TRUE) + 
  xlab("Raven score") + ylab("Cohort") + theme(legend.position = 'none')


############################
### Fine motor function ### - Finger tapping test 
###########################

# dominant hand 
dominant_hand_fine_motor <- ggplot(data = helix_csv, aes(x = hs_sum_domhand))  +
  geom_density(fill = "#FDE725FF", col = 'black', alpha = 0.6) + theme_classic() + xlab("dominant hand score")

# laterality score
laterality_score_fine_motor <- ggplot(data = helix_csv, aes(x = hs_laterality))  + 
  geom_density(fill = "#FDE725FF", col = 'black', alpha = 0.6) + theme_classic() + xlab("laterality score")


### join plot with patchwork - Fine motor function
(dominant_hand_fine_motor | laterality_score_fine_motor) + plot_annotation(title = 'Fine motor function')

# dominant score by cohort 
ggplot(data = helix_data, aes(x = hs_sum_domhand, y = h_cohort, fill = h_cohort))  +
  geom_density_ridges(alpha = 0.4, color = NA) + theme_classic() + 
  scale_fill_viridis(discrete=TRUE) + scale_color_viridis(discrete=TRUE) + 
  xlab("Dominant hand score") + ylab("Cohort") + theme(legend.position = 'none')

# laterality score by cohort 
ggplot(data = helix_data, aes(x = hs_laterality, y = h_cohort, fill = h_cohort))  +
  geom_density_ridges(alpha = 0.4, color = NA) + theme_classic() + 
  scale_fill_viridis(discrete=TRUE) + scale_color_viridis(discrete=TRUE) + 
  xlab("Laterality score") + ylab("Cohort") + theme(legend.position = 'none')















