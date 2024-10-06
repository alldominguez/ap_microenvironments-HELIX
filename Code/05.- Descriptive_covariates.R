# 05.- Descriptive analysis covariates

pacman::p_load(tidyverse, DataExplorer, skimr, haven, patchwork, hablar, rio, magrittr, ggridges, 
               viridis, hrbrthemes, gtsummary, janitor, flextable, kableExtra, corrplot, summarytools,
               foreign, psych, car, readstata13, SmartEDA)

# load the functions created previously 
source("Code/functions_tfm.R") 

# load data 
helix_data <- readxl::read_xlsx("Data/clean/helix_db_clean_2.xlsx")

# check the structure of the data 
helix_data %>% dplyr::glimpse()

dplyr::glimpse(helix_data)
levels(helix_data$e3_ses)

helix_data$e3_ses <- as.factor(helix_data$e3_ses)
skimr::skim(helix_data)
#########################
### convert to factor ###
########################

# maternal characteristics #
helix_data$h_edumc <- as.factor(helix_data$h_edumc) # educational level of the mother
helix_data$e3_ses <- as.factor(helix_data$e3_ses) # socioeconomic status of the parents (define by each cohort)
helix_data$h_urban_preg <- as.factor(helix_data$h_urban_preg) # urgan pregnancy 
helix_data$e3_asmokyn_p <- as.factor(helix_data$e3_asmokyn_p) # smoking during pregnancy
helix_data$e3_alcpreg_yn <- as.factor(helix_data$e3_alcpreg_yn) # alcohol during pregnancy
helix_data$h_parity <- as.factor(helix_data$h_parity)

# paternal characteristics 
helix_data$e3_edufc <- as.factor(helix_data$e3_edufc) # paternal educational level
helix_data$e3_fbmic <- as.factor(helix_data$e3_fbmic) # paternal BMI categorized



# child characteristics
helix_data$e3_sex <- as.factor(helix_data$e3_sex)
helix_data$h_seabir <- as.factor(helix_data$h_seabir)
helix_data$hs_areases_tert_h <- as.factor(helix_data$hs_areases_tert_h)

##########################
### reorder the levels ###
#########################

# maternal characteristics # 
helix_data$h_edumc <- factor(helix_data$h_edumc, levels = c('high', 'middle', 'low')) 
helix_data$e3_ses <- factor(helix_data$e3_ses, levels = c('high income','medium income', 'low income'))
helix_data$h_urban_preg <- factor(helix_data$h_urban_preg, levels = c('urban', 'other'))
helix_data$e3_asmokyn_p <- factor(helix_data$e3_asmokyn_p, levels = c('yes', 'no'))
helix_data$e3_alcpreg_yn <- factor(helix_data$e3_alcpreg_yn, levels = c('yes', 'no'))
helix_data$h_parity <- factor(helix_data$h_parity, levels = c('no child', '1 child', '≥ 2 child'))

# paternal characteristics 
helix_data$e3_edufc <- factor(helix_data$e3_edufc, levels = c('high', 'middle', 'low')) 

levels(helix_data$e3_fbmic )[levels(helix_data$e3_fbmic )=="0"] <-"1"

helix_data$e3_fbmic <- factor(helix_data$e3_fbmic, levels = c('< 25 kg/m2', '25-29 kg/m2', '≥ 30 kg/m2'))

# child characteristics
helix_data$h_seabir <- factor(helix_data$h_seabir, levels = c('winter', 'spring', 'summer', 'autumn'))

############################################################################################################

##########################
### Descriptive tables ###
#########################

################################
### Maternal characteristics ###
################################
table_maternal_descriptive <- helix_data %>% 
  dplyr::select(h_age, h_edumc, h_parity, h_urban_preg, h_mbmi, e3_asmokyn_p, e3_alcpreg_yn, 
                e3_race, e3_ses, h_areases_tert_preg, h_areases_quint_preg) %>% # maternal characteristics
  gtsummary::tbl_summary(missing_text = "Missing",                   # using by = h_cohort we could stratified by cohort
    label = list(h_age ~ "Age (years)", 
                 h_edumc ~ "Educational level",
                 h_parity ~ "Parity",
                 h_urban_preg ~ "Urban type of residence",
                 h_mbmi ~ "Maternal pre-pregnancy BMI (kg/m2)",
                 e3_asmokyn_p ~ "Smoking during pregnancy (yes vs. no)",
                 e3_alcpreg_yn ~ "Alcohol during pregnancy (yes vs. no)",
                 e3_race ~ "race",
                 e3_ses ~ "socieconomic status",
                 h_areases_tert_preg ~ "deprivation index area level (tertiles)",
                 h_areases_quint_preg ~ "deprivation index area level (quintiles)"),
    statistic = list(all_continuous() ~ "{mean} ({sd})")) %>% 
  bold_labels() %>% 
  modify_header(label ~ "**Variable**") %>% 
  modify_caption("**Table 1. Maternal Characteristics**")

# maternal age recruitment 
tfm_summary_v2(var = h_age, group = h_cohort, data = helix_data) %>%
  rio::export("output/tables/maternal_age.xlsx") # export as xlsx

# urban type of redicence 
helix_data %>% select(h_cohort, h_urban_preg) %>% group_by(h_cohort) %>% skimr::skim()

# urban type of redicence 
helix_data %>% select(h_cohort, h_parity) %>% group_by(h_cohort) %>% skimr::skim()

# maternal heigth
tfm_summary_v2(var = e3_mheight, group = h_cohort, data = helix_data) %>%
  rio::export("output/tables/maternal_heigth.xlsx") # export as xlsx

# maternal weigth 
tfm_summary_v2(var = e3_mweight, group = h_cohort, data = helix_data) %>%
  rio::export("output/tables/maternal_weigth.xlsx") # export as xlsx

# maternal pre-pregnancy bmi
tfm_summary_v2(var = h_mbmic, group = h_cohort, data = helix_data) %>%
  rio::export("output/tables/maternal_pre_bmi.xlsx") # export as xlsx

# maternal educational level 
helix_data %>% select(h_cohort, h_edumc) %>% group_by(h_cohort) %>% skimr::skim()

# maternal fisch intake during pregnancy 
tfm_summary_v2(var = h_fish_preg, group = h_cohort, data = helix_data) %>%
  rio::export("output/tables/fish_preg.xlsx") # export as xlsx

# maternal active smoking
helix_data %>% select(h_cohort, e3_asmokyn_p) %>% group_by(h_cohort) %>% skimr::skim()

# maternal active alcohol 
helix_data %>% select(h_cohort, e3_alcpreg_yn) %>% group_by(h_cohort) %>% skimr::skim()

# socieconomic status (ses)
helix_data %>% select(h_cohort, e3_ses) %>% group_by(h_cohort) %>% skimr::skim()

# deprivation index preg tertiles 
helix_data %>% select(h_cohort, h_areases_tert_preg) %>% group_by(h_cohort) %>% skimr::skim()

# deprivation index preg quintiles
helix_data %>% select(h_cohort, h_areases_quint_preg) %>% group_by(h_cohort) %>% skimr::skim()


################################
### Paternal characteristics ###
###############################
table_paternal_descriptive <- helix_data %>% 
  dplyr::select(h_fage, e3_edufc, e3_fbmic, e3_edupc, FAS_cat) %>% # paternal characteristics
  gtsummary::tbl_summary(missing_text = "Missing",                   # using by = h_cohort we could stratified by cohort
                         label = list(h_fage ~ "Age (years)", 
                                      e3_edufc ~ "Educational level",
                                      e3_fbmic ~ "Paternal BMI (kg/m2)",
                                      e3_edupc ~ "parental educational level",
                                      FAS_cat ~ "Family affluence scale"),
                         statistic = list(all_continuous() ~ "{mean} ({sd})")) %>% 
  bold_labels() %>% 
  modify_header(label ~ "**Variable**") %>% 
  modify_caption("**Table 1. Paternal Characteristics**") 


# paternal age at recruitment 
tfm_summary_v2(var = h_fage, group = h_cohort, data = helix_data) %>%
  rio::export("output/tables/paternal_age.xlsx") # export as xlsx


# paternal educational level
helix_data %>% select(h_cohort, e3_edufc) %>% group_by(h_cohort) %>% skimr::skim()

# parental educational level (both) e3_edupc
helix_data %>% select(h_cohort, e3_edupc) %>% group_by(h_cohort) %>% skimr::skim()

# paternal bmi 
helix_data %>% select(h_cohort, e3_fbmic) %>% group_by(h_cohort) %>% skimr::skim()

# FAS categorical 
helix_data %>% select(h_cohort, FAS_cat) %>% group_by(h_cohort) %>% skimr::skim()



#############################
### Child characteristics ###
############################
table_child_descriptive <- helix_data_2 %>% 
  dplyr::select(hs_age_years, e3_sex, h_seabir, hs_globalexp2, h_bf, 
                hs_areases_tert_h, hs_areases_quint_h) %>% # child characteristics
  gtsummary::tbl_summary(missing_text = "Missing",                  
                         label = list(hs_age_years ~ "Age (years)",
                                      e3_sex ~ "Sex", 
                                      h_seabir ~ "Season of birth",
                                      hs_globalexp2 ~ "Exposure to tabacoo",
                                      h_bf ~ "breastfeeding",
                                      hs_areases_tert_h = "deprivation index (tertiles)",
                                      hs_areases_quint_h = "deprivation index (quintiles)"),
                         statistic = list(all_continuous() ~ "{mean} ({sd})")) %>% 
  bold_labels() %>% 
  modify_header(label ~ "**Variable**") %>% 
  modify_caption("**Table 1. Child Characteristics**") 


# age at assessment (in years)
tfm_summary_v2(var = h_gestweight, group = h_cohort, data = helix_data) %>%
  rio::export("output/tables/gestational_weigth.xlsx")

helix_data %>% select(h_cohort, hs_age_years) %>% group_by(h_cohort) %>% skimr::skim()

# child sex 
helix_data %>% select(h_cohort, e3_sex) %>% group_by(h_cohort) %>% skimr::skim()

# child birthweight 
tfm_summary_v2(var = e3_bw, group = h_cohort, data = helix_data) %>%
  rio::export("output/tables/birthweight.xlsx") # export as xlsx

# season of birth
helix_data %>% select(h_cohort, h_seabir) %>% group_by(h_cohort) %>% skimr::skim() 

# gestational weight
tfm_summary_v2(var = h_gestweight, group = h_cohort, data = helix_data) %>%
  rio::export("output/tables/gestational_weigth.xlsx") # export as xlsx

# gestational age 
tfm_summary_v2(var = e3_gac, group = h_cohort, data = helix_data) %>%
  rio::export("output/tables/gestational_age.xlsx") # export as xlsx

# global tabacoo exposure smoke 
helix_data %>% select(h_cohort, hs_globalexp2) %>% group_by(h_cohort) %>% skimr::skim() 

# total fish intake
tfm_summary_v2(var = hs_total_fish, group = h_cohort, data = helix_data) %>%
  rio::export("output/tables/total_fish_intake.xlsx") # export as xlsx

# total vegetable intake
tfm_summary_v2(var = hs_total_veg, group = h_cohort, data = helix_data) %>%
  rio::export("output/tables/total_veg_intake.xlsx") # export as xlsx

# total fruit intake 
tfm_summary_v2(var = hs_total_fruits, group = h_cohort, data = helix_data) %>%
  rio::export("output/tables/total_fruits_intake.xlsx") # export as xlsx

# breastfeading 
helix_data %>% select(h_cohort, h_bf) %>% group_by(h_cohort) %>% skimr::skim() 

# sleep duration (in hours per weeks)
tfm_summary_v2(var = dif_hours_total, group = h_cohort, data = helix_data) %>%
  rio::export("output/tables/sleep_hours.xlsx") # export as xlsx

# Area-level SES indicator (deprivation index in tertilers)
helix_data_2 %>% select(h_cohort, hs_areases_tert_h) %>% naniar::miss_scan_count(search = list("null","NA","na"))

helix_data_2 <- helix_data %>%  naniar::replace_with_na(replace = list(hs_areases_quint_h = "null")) 

helix_data_2 %>% select(h_cohort, hs_areases_quint_h) %>% group_by(h_cohort) %>% skimr::skim() 


###############################
# physcial activity variables #
###############################

physical_activity_variables <- helix_data %>% 
  select(h_cohort,hs_mvpa_raw:hs_sd_wk)

# raw measurement MVPA
tfm_summary_v2(var = hs_mvpa_raw, group = h_cohort, data = physical_activity_variables ) %>%
  rio::export("output/tables/raw_mvpa.xlsx") # export as xlsx

# clean measurement MVPA 
tfm_summary_v2(var = hs_mvpa, group = h_cohort, data = physical_activity_variables ) %>%
  rio::export("output/tables/clean_mvpa.xlsx") # export as xlsx

# clean measurement MVPA alt (with missing in discrepancies)
tfm_summary_v2(var = hs_mvpa_alt, group = h_cohort, data = physical_activity_variables ) %>%
  rio::export("output/tables/mvpa_alt.xlsx") # export as xlsx

#  clean and over-reporting measurement MVPA 
tfm_summary_v2(var = hs_mvpa_prd_alt, group = h_cohort, data = physical_activity_variables ) %>%
  rio::export("output/tables/mvpa_over_reporting.xlsx") # export as xlsx

# sedentary behaviour 
tfm_summary_v2(var = hs_sd_wk, group = h_cohort, data = physical_activity_variables ) %>%
  rio::export("output/tables/sedantary.xlsx") # export as xlsx



############################################################################################################
 

#### other  variable transformations ### 

# change parity classes 
helix_data$h_parity <- dplyr::recode(helix_data$h_parity, 'none' = 'no child', 
                                     'one previous delivery' = '1 child', # 460
                                     'more than one previous delivery' = '≥ 2 child') # 227 

# change father educational level classes
helix_data$e3_edufc <- dplyr::recode(helix_data$e3_edufc, '0' = 'low', 
                                     '1' = 'middle',
                                     '2' = 'high') 

# change father BMI classes # 0: <18,5; 1: 18,5-25; 2: 25-30; 3: >=30
helix_data$e3_fbmic <- dplyr::recode(helix_data$e3_fbmic , 'less than 25 kg/m2' = '< 25 kg/m2', 
                                     'less than 25 kg/m2' = '< 25 kg/m2',
                                     '2' = '25-29 kg/m2', 
                                     '3' = '≥ 30 kg/m2') 
