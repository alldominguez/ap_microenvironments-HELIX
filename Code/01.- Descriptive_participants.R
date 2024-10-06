# 01.- Descriptive analysis - participants 

pacman::p_load(tidyverse, DataExplorer, skimr, haven, patchwork, hablar, rio, magrittr, ggridges, 
               viridis, hrbrthemes, gtsummary, janitor, flextable, kableExtra, corrplot, summarytools,
               foreign, psych, car, readstata13)

# load the functions created previously 
source("Code/functions_tfm.R") 

# load data 
helix_data <- readxl::read_xlsx("Data/clean/helix_db_clean.xlsx")
helix_data <- readxl::read_xlsx("Data/clean/helix_db_clean_2.xlsx")
# check the structure of the data 
helix_data %>% dplyr::glimpse()

########################################## PARTICIPANT OVERVIEW ########################################## 


##################################
############# TABLES ############# ---- CHILDREN ---- # DESCRIPTIVES #
#################################
pacman::p_load(gt, gtsummary, kableExtra, janitor)

# table 1- Sample size by cohort 
helix_data %>% dplyr::count(h_cohort) %>% rename(Cohort = h_cohort, N = n) %>% adorn_totals("row") %>% 
  kableExtra::kable() %>%  kable_classic(full_width = F, html_font = "Cambria") 

# graph 1- Sample size by cohort 
ggplot2::ggplot(data = helix_data, mapping = aes(x = fct_infreq(h_cohort), fill = h_cohort)) +
  geom_bar() + scale_fill_viridis(discrete=TRUE) + scale_color_viridis(discrete=TRUE) + 
  xlab("Cohort") + ylab("Number of participant") + theme_classic() + ylim(c(0, 300)) + 
  geom_text(stat='count', aes(label=..count..), vjust=-1) 


# table 2- Sample size by sex 
helix_data %>% janitor::tabyl(e3_sex ,h_cohort ) %>% 
  adorn_totals(where = "row") %>%             # add a total row
  adorn_percentages(denominator = "col") %>%  # convert to proportions
  adorn_pct_formatting() %>%                  # convert to percents
  adorn_ns(position = "front") %>%            # display as: "count (percent)"
  adorn_title(                                # adjust titles
    row_name = "Gender",
    col_name = "Cohort")


helix_data %>% dplyr::count(e3_sex) %>% rename(Sex = e3_sex, N = n) %>% adorn_totals("row") %>% 
  kableExtra::kable() %>%  kable_classic(full_width = F, html_font = "Cambria") 

helix_data %>% dplyr::group_by(h_cohort, e3_sex) %>% dplyr::count() %>% rename(Cohort = h_cohort, Sex = e3_sex, N = n) %>% 
  adorn_totals("row") %>% 
  kableExtra::kable() %>%  kable_classic(full_width = F, html_font = "Cambria") 

# graph 2 - Sample size by sex
ggplot2::ggplot(data = helix_data, mapping = aes(x = fct_infreq(e3_sex), fill = e3_sex)) +
  geom_bar() + scale_fill_viridis(discrete=TRUE) + scale_color_viridis(discrete=TRUE) + 
  xlab("Sex") + ylab("Number of participant") + theme_classic() + ylim(c(0, 800)) + 
  geom_text(stat='count', aes(label=..count..), vjust=-1)


# table 4- age at the age of assessment
helix_data %>% select(hs_age_years) %>% 
  summarise(N = n(), mean = round(mean(hs_age_years, na.rm = TRUE), digits = 1), 
            sd = round(sd(hs_age_years, na.rm = TRUE), digits = 1), 
            median = round(median(hs_age_years, na.rm = TRUE), digits = 1), 
            p25 = round(quantile(hs_age_years, probs = .25, na.rm = TRUE ), digits = ),
            p75 = round(quantile(hs_age_years, probs = .75, na.rm = TRUE), digits =  1),
            min = round(min(hs_age_years, na.rm = TRUE), digits =  1),
            max = round(max(hs_age_years, na.rm = TRUE), digits = 1)) %>%
  kableExtra::kable() %>%  kable_classic(full_width = F, html_font = "Cambria")  

# esto soprendentemente funciona
helix_data %>% group_by(h_cohort) %>% skimr::skim(hs_age_years) %>% 
  select(h_cohort, n_missing, numeric.mean ) %>% adorn_totals("row") %>% 
  kableExtra::kable() %>%  kable_classic(full_width = F, html_font = "Cambria")  # by cohort

# otra forma de hacerlo con gt
helix_data %>% dplyr::count(h_cohort) %>%
  gt() %>% cols_label(h_cohort = "Corhot", n = "N") 


##########################################
############# TABLES & PLOTS ############# ----- PARENTS---- # DESCRIPTIVES #
#########################################
glimpse(helix_data) # we check and remember our data 

# h_age, e3_mheight, e3_mweight 
helix_data %>% skimr::skim(h_age, e3_mheight, e3_mweight)


helix_data %>% group_by(h_cohort) %>% skimr::skim(h_age, e3_mheight, e3_mweight)
