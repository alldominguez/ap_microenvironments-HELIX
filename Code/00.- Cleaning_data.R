# 00.- Cleaning data TFM
pacman::p_load(tidyverse, DataExplorer, skimr, haven, patchwork, hablar, rio, magrittr, ggridges)

### second data from Jose ### --- filter by Jose --- N ~ 1301
# load data in the different formats 
helix_xlsx <- readxl::read_xlsx("Data/raw/ap106_request_updated14feb.2022.xlsx") # file in xlsx format   
helix_csv <- readr::read_csv("Data/raw/ap106_request_updated14feb.2022.csv") # file in dta format
helix_dta <- haven::read_dta("Data/raw/ap106_request_updated_14feb.2022.dta") # file in csv format

### second official request ### 23/03/2022
helix_csv<- readr::read_csv("Data/raw/ap106_request_updated23mar.2022.csv")


######## CHECK THE STRUCTURE OF THE DATA ####### 

# we check the structure of the data and inspect data type constrains
dplyr::glimpse(helix_dta) # check data types
str(helix_dta) # same as above
helix_dta %>% skimr::skim() # some of the continous variables needs to be transform from character to numeric

# change character variables to numeric
class(helix_dta$hs_no2_yr_hs_h)
dplyr::glimpse(helix_csv) # we check the data types 

####################  DATA TYPE CONSTRAINS ####################

######## CHARACTER TO NUMERIC ####### 

# convert character into numeric 
helix_csv <- helix_csv %>% 
  hablar::convert(num(hs_no2_yr_hs_h:hs_ndvi100_t_sum)) # convert into numeric, from hs_no2_yr_hs_h to hs_ndvi100_t_sum

######## DOUBLE TO FACTOR #######

# wee need to change from double to character:  
helix_csv$h_cohort <- as.factor(helix_csv$h_cohort) # cohort

### Child covariates ### 
helix_csv$e3_sex <- as.factor(helix_csv$e3_sex) # child sex
helix_csv$h_seabir <- as.factor(helix_csv$h_seabir) # seasons of birth
helix_csv$h_bf <- as.factor(helix_csv$h_bf) # child breastfeeding
helix_csv$hs_globalexp2 <- as.factor(helix_csv$hs_globalexp2) # global exposure tobacco
helix_csv$hs_areases_tert_h <- as.factor(helix_csv$hs_areases_tert_h) # Area-level SES - deprivation index in tertiles
helix_csv$hs_areases_quint_h <- as.factor(helix_csv$hs_areases_quint_h) # Area-level SES - deprivation index in quintiles

### Parental covariates ###
helix_csv$h_urban_preg <- as.factor(helix_csv$h_urban_preg) # urban type of residence
helix_csv$h_parity <- as.factor(helix_csv$h_parity) # maternal parity before index pregnancy
helix_csv$h_marital <- as.factor(helix_csv$h_marital) # marital status
helix_csv$h_edumc <- as.factor(helix_csv$h_edumc) # maternal educational level (mother)
helix_csv$e3_asmokyn_p <- as.factor(helix_csv$e3_asmokyn_p) # maternal active smoking during pregnancy
helix_csv$e3_alcpreg_yn <- as.factor(helix_csv$e3_alcpreg_yn) # maternal alcohol consumption during pregnancy
helix_csv$e3_ses <- as.factor(helix_csv$e3_ses) # socioeconomic status of the parents (define by each cohort)
helix_csv$e3_edufc <- as.factor(helix_csv$e3_edufc) # paternal educational level (father)
helix_csv$e3_edupc <- as.factor(helix_csv$e3_edupc) # (parental) educational level (define by each cohort)
helix_csv$hs_wrk_m <- as.factor(helix_csv$hs_wrk_m) # current employment status? - questionnaire 
helix_csv$FAS_cat <- as.factor(helix_csv$FAS_cat) # family affluence scale (FAS II) - categorical
helix_csv$hs_marital_m <- as.factor(helix_csv$hs_marital_m) # current civil marital status - questionnaire
helix_csv$hs_finance <- as.factor(helix_csv$hs_finance) # how well would you say your family is managing finnancially these days - questionnaire
helix_csv$h_areases_tert_preg <- as.factor(helix_csv$h_areases_tert_preg) # Area-level SES - at pregnancy period in tertiles
helix_csv$h_areases_quint_preg <- as.factor(helix_csv$h_areases_quint_preg) # Area-level SES - at pregnancy period in quintiles


######## RELABEL FACTOR #######
helix_csv %>% count(h_edumc)  #counter

### Child covariates ### 

# sex / no missing values
helix_csv$e3_sex <-  dplyr::recode(helix_csv$e3_sex, '1' = 'male', # 771
                                                     '2' = 'female') # 590

# season of birth / 278 missing values
helix_csv$h_seabir <- dplyr::recode(helix_csv$h_seabir, '0' = "winter", # 225
                                                        '1' = 'spring', # 229
                                                        '2' = 'summer', # 258
                                                        '3' = 'autumn') # 311 

# child breastfeeding / 316 missing values
helix_csv$h_bf <- dplyr::recode(helix_csv$h_bf, '0' = 'never', # 119
                                                '1' = 'ever') # 866

# global exposure of the child to ETS (external tobacco smoke) / 30 missing values
helix_csv$hs_globalexp2 <- dplyr::recode(helix_csv$hs_globalexp2, '1' = 'no exposure', # 826
                                                                  '2' = 'exposure') # 445 

### Parental covariates ###

# urban type of residence / 336 missing values
helix_csv$h_urban_preg <- dplyr::recode(helix_csv$h_urban_preg, '0' = 'urban', # 846 
                                                                '1' = 'other') # 119
# parity / 31 missing values 
helix_csv$h_parity <- dplyr::recode(helix_csv$h_parity, '0' = 'none', # 583 -- no child 
                                                        '1' = 'one previous delivery', # 460 --  1 child 
                                                        '2' = 'more than one previous delivery') # 227 -- ≥ 2 child

# marital status / 16 missing values 
helix_csv$h_marital <- dplyr::recode(helix_csv$h_marital, '0' = 'living with the father', # 1215
                                                          '1' = 'living alone', # 39
                                                          '2' = 'other situation') # 31 

# maternal educational level / 44 missing values
helix_csv$h_edumc <- dplyr::recode(helix_csv$h_edumc, '0' = 'low', # 173
                                                      '1' = 'middle', # 433
                                                      '2' = 'high') # 651 

# maternal active smoking during pregnancy / 42 missing values
helix_csv$e3_asmokyn_p <- dplyr::recode(helix_csv$e3_asmokyn_p, '0' = 'no', # 1069
                                                                '1' = 'yes') # 190



# maternal alcohol consumption during pregnancy / 75 missing values
helix_csv$e3_alcpreg_yn <- dplyr::recode(helix_csv$e3_alcpreg_yn, '0' = 'no', # 837
                                                                  '1' = 'yes') # 389

# socioeconomic status of the parents (define by each cohort) / 682 missing values
helix_csv$e3_ses <- dplyr::recode(helix_csv$e3_ses, '0' = 'low income', # 229
                                                    '1' = 'medium income', # 272 
                                                    '2' = 'high income',) # 118



# paternal educational level (father) / 89 missing values
helix_csv$e3_edufc <- dplyr::recode(helix_csv$e3_edufc, '0' = 'low', # 209
                                                       '1' = 'middle', # 473
                                                       '2' = 'high') # 530 

# parental education level (both) / 23 missing values
helix_csv$e3_edupc <- dplyr::recode(helix_csv$e3_edupc, '0' = 'low',# 131
                                                        '1' = 'middle', # 393
                                                        '2' = 'high') # 754 


# current employment status? - questionnaire / 3 missing values
helix_csv$hs_wrk_m <- dplyr::recode(helix_csv$hs_wrk_m, '1' = 'employed', # 988
                                                        '2' = 'unemployed', # 106
                                                        '3' = 'student', # 16
                                                        '4' = 'stay-at-home parent', # 146
                                                        '5' = 'rehabilitation/disabled', # 7
                                                        '6' = 'military', # 1 
                                                        '7' = 'other', # 27
                                                        '8' = 'no answer') # 7

# family affluence scale (FAS II) - categorical / 2 missing values 
helix_csv$FAS_cat <- dplyr::recode(helix_csv$FAS_cat, '1' = 'low', # 139
                                                      '2' = 'middle', # 500
                                                      '3' = 'high') # 660


# current civil marital status - questionnaire / 2 missing values
helix_csv$hs_marital_m <- dplyr::recode(helix_csv$hs_marital_m, '1' = 'married', # 972
                                                                '2' = 'cohabitant', # 184 
                                                                '3' = 'widow/widower', # 4 
                                                                '4' = 'divorced/separated', # 81
                                                                '5' = 'single', # 54 
                                                                '6' = 'no answer') # 4

# how well would you say your family is managing finnancially these days - questionnaire / 6 missing values
helix_csv$hs_finance <- dplyr::recode(helix_csv$hs_finance,'1' = 'living comfortably', # 413
                                                           '2' = 'doing alright', # 415 
                                                           '3' = 'finding it quite difficult', # 332
                                                           '4' = 'finding it very difficult', # 87
                                                           '5' = 'finding very difficult', # 40
                                                           '6' = 'no answer') # 8


### we don't change this variables due to their gonna be consider as outdoor exposures

# Area-level SES - at pregnancy period in tertiles
# Area-level SES - at pregnancy period in quintiles

####################  SOME QUICK DESCRIPTIVE SUMMARIES ####################

# first we check with glimpse our clean database
helix_csv %>% dplyr::glimpse()

# summary for air pollution exposure at different microenvironments
helix_csv %>% 
  select(hs_no2_yr_hs_h:hs_no2_yr_hs_t, hs_pm10_yr_hs_h:hs_pm25abs_yr_hs_t) %>% 
  skimr::skim() 

# summart for cognitive and fine motor functions 
helix_csv %>% 
  select(hs_correct_raven:hs_hitrtse) %>% 
  skimr::skim()


############################################
######### exporting clean database #########
############################################

# for import our "clean" database we use the package rio( )

# first request #
rio::export(helix_csv, "helix_db_clean.csv") # export the database in csv format 
rio::export(helix_csv, "helix_db_clean.xlsx") # export the databasae in xlsx format

# second request # 
rio::export(helix_csv, "helix_db_clean_2.csv") # export the database in csv format 
rio::export(helix_csv, "helix_db_clean_2.xlsx") # export the databasae in xlsx format

############################################
########### DESCRIPTIVE ANALYSIS ###########
############################################

### cohort ### BIB (205), EDEN (198) + KANC (204) + MOBA (272) + RHEA (199) + INMA (223)
xtabs(~ cohort, data = helix_csv, addNA = TRUE) # there is no missing (NA) in cohort

# Number for each cohort 
helix_csv %>% count(h_cohort)  # there is no missing in cohort variable




