library(readxl)
library(tidyverse)

######################
## Calibration data ## 
######################

## HIV prevalence in FSW
FSW_prev_data <- read.delim("../data/CSWstudies.txt", header=FALSE)
names(FSW_prev_data) = c("Year", "N", "prev")
FSW_prev_data = FSW_prev_data %>% 
  mutate(tot_pos = round(prev*N)) %>% 
  rowwise() %>% 
  mutate(tot_prev_lwr = binom.test(tot_pos, N)$conf.int[1], 
         tot_prev_upr = binom.test(tot_pos, N)$conf.int[2])

FSW_prev_data = FSW_prev_data %>% 
  mutate(type = "HIV prevalence in FSW") %>% 
  mutate(type = factor(type, levels = c("HIV incidence in FSW", "HIV prevalence in FSW", "ART coverage in FSW")))

FSW_prev_data = FSW_prev_data %>% 
  ungroup() %>% 
  mutate(dodge_group = as.character(row_number()))

## HIV prevalnece in ANC
ANC_prev_data <- read.delim("../data/ANCprevData.txt", header=FALSE) %>% 
  filter(row_number()<=140) %>% 
  mutate(age_group = case_when(V2 == 1 ~ "15-19", 
                               V2 == 2 ~ "20-24", 
                               V2 == 3 ~ "25-29", 
                               V2 == 4 ~ "30-34", 
                               V2 ==5 ~ "35-39")) %>% 
  mutate(Year = as.numeric(V1)) %>% 
  mutate(ANC_prev = V3, 
         Pop_prop = V4) %>% 
  select(-V1, -V2, -V3, -V4)

# ANC prev survey numbers from the excel file (OutPrevalence38.xlsx)
ANC_prev_data2 = read.csv("../data/ancprev_survey_from_OutPrevalence38.csv", header=FALSE, sep=";")
names_ANC = ANC_prev_data2[,1]
h = t(ANC_prev_data2[,-1]) %>% as_tibble()
names(h) = names_ANC
ANC_prev2 = h %>% select(year, (!contains("up") & !contains("lw") & contains("anc"))) %>% 
  pivot_longer(cols = 2:7, values_to = "ANC_prev", names_to = "age_group") %>% 
  mutate(age_group  = gsub("anc_","", age_group)) %>% 
  mutate(age_group = case_when(age_group == "overall" ~ "overall",
                               age_group == "1519" ~ "15-19", 
                               age_group == "2024" ~ "20-24", 
                               age_group == "2529" ~ "25-29", 
                               age_group == "3034" ~ "30-34", 
                               age_group == "3539" ~ "35-39")) 
ANC_prev2_lw = h %>% select(year, contains("lw") ) %>% 
  pivot_longer(cols = 2:7, values_to = "ANC_prev_lw", names_to = "age_group") %>% 
  mutate(age_group  = gsub("anc_","", age_group)) %>% 
  mutate(age_group = gsub("_lw", "", age_group)) %>% 
  mutate(age_group = case_when(age_group == "overall" ~ "overall",
                               age_group == "1519" ~ "15-19", 
                               age_group == "2024" ~ "20-24", 
                               age_group == "2529" ~ "25-29", 
                               age_group == "3034" ~ "30-34", 
                               age_group == "3539" ~ "35-39")) 
ANC_prev2_up = h %>% select(year, contains("up") ) %>% 
  pivot_longer(cols = 2:7, values_to = "ANC_prev_up", names_to = "age_group") %>% 
  mutate(age_group  = gsub("anc_","", age_group)) %>% 
  mutate(age_group = gsub("_up", "", age_group)) %>% 
  mutate(age_group = case_when(age_group == "overall" ~ "overall",
                               age_group == "1519" ~ "15-19", 
                               age_group == "2024" ~ "20-24", 
                               age_group == "2529" ~ "25-29", 
                               age_group == "3034" ~ "30-34", 
                               age_group == "3539" ~ "35-39")) 


ANC_prev_data_2 = ANC_prev2 %>% 
  left_join(ANC_prev2_lw) %>% 
  left_join(ANC_prev2_up) %>% 
  rename(ANC_prev2 = ANC_prev, Year = year)

ANC_prev_data_tot = ANC_prev_data_2 %>% left_join(ANC_prev_data) 

# HIV prevalence 
HIVprevData <- read.csv("../data/HIVprevData_formatted.csv")
HIVprevData = HIVprevData %>% 
  filter(age_group>=3) %>% 
  mutate(age_group = case_when(age_group == 3 ~ "15-19", 
                               age_group == 4 ~ "20-24", 
                               age_group == 5 ~ "25-29", 
                               age_group == 6 ~ "30-34", 
                               age_group == 7 ~ "35-39", 
                               age_group == 8 ~ "40-44", 
                               age_group == 9 ~ "45-49", 
                               age_group == 10 ~ "50-54", 
                               age_group == 11 ~ "55-59"))


########################################
#### models with 30 year projections ##
#######################################

# Mod 1a: constant transmission risk, constant age and SW duration in FSW
mod1a_30y = read.csv("../data/mod1a_30yearART2019.csv", header=FALSE, sep=";") 
mod1a_30y$V2[1] = "Mean"
mod1a_30y = mod1a_30y %>% 
  filter(V2 != "") %>% 
  mutate(V1 = ifelse(V1 =="", NA_character_, V1)) %>% 
  mutate(Indicator = V1) %>% 
  mutate(Indicator = if_else(is.na(Indicator), lag(V1), Indicator)) %>% 
  mutate(Indicator = if_else(is.na(Indicator), lag(Indicator), Indicator)) %>%
  mutate(Indicator = gsub(" ", "_", Indicator)) %>% 
  mutate(V2 = gsub(" ", "", V2)) %>% 
  mutate(Indicator = paste(Indicator, V2, sep = "_")) %>% 
  select(-V1, -V2) 
mod1a_30y$Indicator[1] = "Year" 

mod1a_30y = mod1a_30y[, -c(72:86)]
rownames_temp = mod1a_30y$Indicator
tt = mod1a_30y %>% select(-Indicator) 
tt_trans = t(as.matrix(tt))
rownames(tt_trans) = NULL
mod1a_30y = tt_trans %>% as.tibble()
colnames(mod1a_30y) = rownames_temp


mod1a_30y = mod1a_30y %>% 
  mutate(Year = as.numeric(Year)) %>% 
  mutate(HIV_incidence_FSW_Mean = HIV_incidence_in_female_sex_workers_Mean) %>% 
  mutate(HIV_incidence_FSW_LL = `HIV_incidence_in_female_sex_workers_95%LL`) %>% 
  mutate(HIV_incidence_FSW_UL = `HIV_incidence_in_female_sex_workers_95%UL`) %>% 
  mutate(HIV_prevalence_FSW_Mean = HIV_prevalence_in_females_sex_workers_Mean) %>% 
  mutate(HIV_prevalence_FSW_LL = `HIV_prevalence_in_females_sex_workers_95%LL`) %>% 
  mutate(HIV_prevalence_FSW_UL = `HIV_prevalence_in_females_sex_workers_95%UL`) %>% 
  mutate(ART_coverage_FSW_Mean = ART_coverage_in_female_sex_workers_Mean) %>% 
  mutate(ART_coverage_FSW_LL = `ART_coverage_in_female_sex_workers_95%LL`) %>% 
  mutate(ART_coverage_FSW_UL = `ART_coverage_in_female_sex_workers_95%UL`) %>% 
  mutate(HIV_prev_ANC_15_19_Mean = `HIV_prevalence_in_pregnant_women,_15-19_Mean`) %>% 
  mutate(HIV_prev_ANC_15_19_LL = `HIV_prevalence_in_pregnant_women,_15-19_95%LL`) %>% 
  mutate(HIV_prev_ANC_15_19_UL = `HIV_prevalence_in_pregnant_women,_15-19_95%UL`) %>% 
  mutate(HIV_prev_ANC_20_24_Mean = `HIV_prevalence_in_pregnant_women,_20-24_Mean`) %>% 
  mutate(HIV_prev_ANC_20_24_LL = `HIV_prevalence_in_pregnant_women,_20-24_95%LL`) %>% 
  mutate(HIV_prev_ANC_20_24_UL = `HIV_prevalence_in_pregnant_women,_20-24_95%UL`) %>% 
  mutate(HIV_prev_ANC_25_29_Mean = `HIV_prevalence_in_pregnant_women,_25-29_Mean`) %>% 
  mutate(HIV_prev_ANC_25_29_LL = `HIV_prevalence_in_pregnant_women,_25-29_95%LL`) %>% 
  mutate(HIV_prev_ANC_25_29_UL = `HIV_prevalence_in_pregnant_women,_25-29_95%UL`) %>% 
  mutate(HIV_prev_ANC_30_34_Mean = `HIV_prevalence_in_pregnant_women,_30-34_Mean`) %>% 
  mutate(HIV_prev_ANC_30_34_LL = `HIV_prevalence_in_pregnant_women,_30-34_95%LL`) %>% 
  mutate(HIV_prev_ANC_30_34_UL = `HIV_prevalence_in_pregnant_women,_30-34_95%UL`) %>% 
  mutate(HIV_prev_ANC_35_39_Mean = `HIV_prevalence_in_pregnant_women,_35-39_Mean`) %>% 
  mutate(HIV_prev_ANC_35_39_LL = `HIV_prevalence_in_pregnant_women,_35-39_95%LL`) %>% 
  mutate(HIV_prev_ANC_35_39_UL = `HIV_prevalence_in_pregnant_women,_35-39_95%UL`) %>% 
  mutate(VL_supp_amongART_LL = `Fraction_of_FSWs_on_ART_who_are_virologically_suppressed_(RNA_count_<1000_copies/ml)_95%LL`) %>% 
  mutate(VL_supp_amongART_UL = `Fraction_of_FSWs_on_ART_who_are_virologically_suppressed_(RNA_count_<1000_copies/ml)_95%UL`) %>% 
  mutate(VL_supp_amongART_Mean = `Fraction_of_FSWs_on_ART_who_are_virologically_suppressed_(RNA_count_<1000_copies/ml)_Mean`)

mod1a_30y = lapply(mod1a_30y, function(x) as.numeric(as.character(x))) %>% 
  as_tibble()

mod1a_30y = mod1a_30y %>% 
  mutate(FSW_assumption = "a - constant FSW age and SW duration") %>% 
  mutate(Transmission_assumption = "1 - constant transmission probability") %>% 
  mutate(model = "model 1a")


mod1a_ancprev_30y = read.csv2("../data/mod1a_ancprev_30yearART2019.csv", header=FALSE) 
mod1a_ancprev_30y$V2[1] = "Mean"

mod1a_ancprev_30y = mod1a_ancprev_30y %>% 
  filter(V2 != "") %>% 
  mutate(V1 = ifelse(V1 =="", NA_character_, V1)) %>% 
  mutate(Indicator = V1) %>% 
  mutate(Indicator = if_else(is.na(Indicator), lag(V1), Indicator)) %>% 
  mutate(Indicator = if_else(is.na(Indicator), lag(Indicator), Indicator)) %>%
  mutate(Indicator = gsub(" ", "_", Indicator)) %>% 
  mutate(V2 = gsub(" ", "", V2)) %>% 
  mutate(Indicator = paste(Indicator, V2, sep = "_")) %>% 
  select(-V1, -V2) 
mod1a_ancprev_30y$Indicator[1] = "Year" 

mod1a_ancprev_30y = mod1a_ancprev_30y %>% 
  filter(grepl("Adjusted_ANC", Indicator)| Indicator == "Year")

rownames_temp_anc = mod1a_ancprev_30y$Indicator
tt = mod1a_ancprev_30y %>% select(-Indicator) 
tt_trans = t(as.matrix(tt))
rownames(tt_trans) = NULL
mod1a_ancprev_30y = tt_trans %>% as.tibble()
colnames(mod1a_ancprev_30y) = rownames_temp_anc

mod1a_ancprev_30y = mod1a_ancprev_30y %>% 
  mutate(Year = as.numeric(Year)) %>% 
  mutate(HIV_prev_ANC_adju_15_19_Mean = `Adjusted_ANC_prevalence,_15-19_Mean`) %>% 
  mutate(HIV_prev_ANC_adju_15_19_LL = `Adjusted_ANC_prevalence,_15-19_95%LL`) %>% 
  mutate(HIV_prev_ANC_adju_15_19_UL = `Adjusted_ANC_prevalence,_15-19_95%UL`) %>% 
  mutate(HIV_prev_ANC_adju_20_24_Mean = `Adjusted_ANC_prevalence,_20-24_Mean`) %>% 
  mutate(HIV_prev_ANC_adju_20_24_LL = `Adjusted_ANC_prevalence,_20-24_95%LL`) %>% 
  mutate(HIV_prev_ANC_adju_20_24_UL = `Adjusted_ANC_prevalence,_20-24_95%UL`) %>% 
  mutate(HIV_prev_ANC_adju_25_29_Mean = `Adjusted_ANC_prevalence,_25-29_Mean`) %>% 
  mutate(HIV_prev_ANC_adju_25_29_LL = `Adjusted_ANC_prevalence,_25-29_95%LL`) %>% 
  mutate(HIV_prev_ANC_adju_25_29_UL = `Adjusted_ANC_prevalence,_25-29_95%UL`) %>% 
  mutate(HIV_prev_ANC_adju_30_34_Mean = `Adjusted_ANC_prevalence,_30-34_Mean`) %>% 
  mutate(HIV_prev_ANC_adju_30_34_LL = `Adjusted_ANC_prevalence,_30-34_95%LL`) %>% 
  mutate(HIV_prev_ANC_adju_30_34_UL = `Adjusted_ANC_prevalence,_30-34_95%UL`) %>% 
  mutate(HIV_prev_ANC_adju_35_39_Mean = `Adjusted_ANC_prevalence,_35-39_Mean`) %>% 
  mutate(HIV_prev_ANC_adju_35_39_LL = `Adjusted_ANC_prevalence,_35-39_95%LL`) %>% 
  mutate(HIV_prev_ANC_adju_35_39_UL = `Adjusted_ANC_prevalence,_35-39_95%UL`) 

mod1a_ancprev_30y = lapply(mod1a_ancprev_30y, function(x) as.numeric(as.character(x))) %>% 
  as_tibble()

mod1a_30y = mod1a_30y %>% 
  left_join(mod1a_ancprev_30y)

## Model 1b: constant transmission risk, increasing age and SW duration in FSW 

mod1b_30y = read.csv("../data/mod1b_30yearART2019.csv", header=FALSE, sep=";") 
mod1b_30y$V2[1] = "Mean"
mod1b_30y = mod1b_30y %>% 
  filter(V2 != "") %>% 
  mutate(V1 = ifelse(V1 =="", NA_character_, V1)) %>% 
  mutate(Indicator = V1) %>% 
  mutate(Indicator = if_else(is.na(Indicator), lag(V1), Indicator)) %>% 
  mutate(Indicator = if_else(is.na(Indicator), lag(Indicator), Indicator)) %>%
  # filter(!apply(across(3:88), 1, function(row){ all(is.na(row)) || all(row =="")})) %>% 
  mutate(Indicator = gsub(" ", "_", Indicator)) %>% 
  mutate(V2 = gsub(" ", "", V2)) %>% 
  mutate(Indicator = paste(Indicator, V2, sep = "_")) %>% 
  select(-V1, -V2) 
mod1b_30y$Indicator[1] = "Year" 

mod1b_30y = mod1b_30y[, -c(72:86)]
rownames_temp = mod1b_30y$Indicator
tt = mod1b_30y %>% select(-Indicator) 
tt_trans = t(as.matrix(tt))
rownames(tt_trans) = NULL
mod1b_30y = tt_trans %>% as.tibble()
colnames(mod1b_30y) = rownames_temp

mod1b_30y = mod1b_30y %>% 
  mutate(Year = as.numeric(Year)) %>% 
  mutate(HIV_incidence_FSW_Mean = HIV_incidence_in_female_sex_workers_Mean) %>% 
  mutate(HIV_incidence_FSW_LL = `HIV_incidence_in_female_sex_workers_95%LL`) %>% 
  mutate(HIV_incidence_FSW_UL = `HIV_incidence_in_female_sex_workers_95%UL`) %>% 
  mutate(HIV_prevalence_FSW_Mean = HIV_prevalence_in_females_sex_workers_Mean) %>% 
  mutate(HIV_prevalence_FSW_LL = `HIV_prevalence_in_females_sex_workers_95%LL`) %>% 
  mutate(HIV_prevalence_FSW_UL = `HIV_prevalence_in_females_sex_workers_95%UL`) %>% 
  mutate(ART_coverage_FSW_Mean = ART_coverage_in_female_sex_workers_Mean) %>% 
  mutate(ART_coverage_FSW_LL = `ART_coverage_in_female_sex_workers_95%LL`) %>% 
  mutate(ART_coverage_FSW_UL = `ART_coverage_in_female_sex_workers_95%UL`) %>% 
  mutate(HIV_prev_ANC_15_19_Mean = `HIV_prevalence_in_pregnant_women,_15-19_Mean`) %>% 
  mutate(HIV_prev_ANC_15_19_LL = `HIV_prevalence_in_pregnant_women,_15-19_95%LL`) %>% 
  mutate(HIV_prev_ANC_15_19_UL = `HIV_prevalence_in_pregnant_women,_15-19_95%UL`) %>% 
  mutate(HIV_prev_ANC_20_24_Mean = `HIV_prevalence_in_pregnant_women,_20-24_Mean`) %>% 
  mutate(HIV_prev_ANC_20_24_LL = `HIV_prevalence_in_pregnant_women,_20-24_95%LL`) %>% 
  mutate(HIV_prev_ANC_20_24_UL = `HIV_prevalence_in_pregnant_women,_20-24_95%UL`) %>% 
  mutate(HIV_prev_ANC_25_29_Mean = `HIV_prevalence_in_pregnant_women,_25-29_Mean`) %>% 
  mutate(HIV_prev_ANC_25_29_LL = `HIV_prevalence_in_pregnant_women,_25-29_95%LL`) %>% 
  mutate(HIV_prev_ANC_25_29_UL = `HIV_prevalence_in_pregnant_women,_25-29_95%UL`) %>% 
  mutate(HIV_prev_ANC_30_34_Mean = `HIV_prevalence_in_pregnant_women,_30-34_Mean`) %>% 
  mutate(HIV_prev_ANC_30_34_LL = `HIV_prevalence_in_pregnant_women,_30-34_95%LL`) %>% 
  mutate(HIV_prev_ANC_30_34_UL = `HIV_prevalence_in_pregnant_women,_30-34_95%UL`) %>% 
  mutate(HIV_prev_ANC_35_39_Mean = `HIV_prevalence_in_pregnant_women,_35-39_Mean`) %>% 
  mutate(HIV_prev_ANC_35_39_LL = `HIV_prevalence_in_pregnant_women,_35-39_95%LL`) %>% 
  mutate(HIV_prev_ANC_35_39_UL = `HIV_prevalence_in_pregnant_women,_35-39_95%UL`) %>% 
  mutate(VL_supp_amongART_LL = `Fraction_of_FSWs_on_ART_who_are_virologically_suppressed_(RNA_count_<1000_copies/ml)_95%LL`) %>% 
  mutate(VL_supp_amongART_UL = `Fraction_of_FSWs_on_ART_who_are_virologically_suppressed_(RNA_count_<1000_copies/ml)_95%UL`) %>% 
  mutate(VL_supp_amongART_Mean = `Fraction_of_FSWs_on_ART_who_are_virologically_suppressed_(RNA_count_<1000_copies/ml)_Mean`)

mod1b_30y = lapply(mod1b_30y, function(x) as.numeric(as.character(x))) %>% 
  as_tibble()

mod1b_30y = mod1b_30y %>% 
  mutate(FSW_assumption = "b - changing FSW age and SW duration") %>% 
  mutate(Transmission_assumption = "1 - constant transmission probability") %>% 
  mutate(model = "model 1b")

mod1b_ancprev_30y = read.csv2("../data/mod1b_ancprev_30yearART2019.csv", header=FALSE) 
mod1b_ancprev_30y$V2[1] = "Mean"

mod1b_ancprev_30y = mod1b_ancprev_30y %>% 
  filter(V2 != "") %>% 
  mutate(V1 = ifelse(V1 =="", NA_character_, V1)) %>% 
  mutate(Indicator = V1) %>% 
  mutate(Indicator = if_else(is.na(Indicator), lag(V1), Indicator)) %>% 
  mutate(Indicator = if_else(is.na(Indicator), lag(Indicator), Indicator)) %>%
  mutate(Indicator = gsub(" ", "_", Indicator)) %>% 
  mutate(V2 = gsub(" ", "", V2)) %>% 
  mutate(Indicator = paste(Indicator, V2, sep = "_")) %>% 
  select(-V1, -V2) 
mod1b_ancprev_30y$Indicator[1] = "Year" 

mod1b_ancprev_30y = mod1b_ancprev_30y %>% 
  filter(grepl("Adjusted_ANC", Indicator)| Indicator == "Year")

rownames_temp_anc = mod1b_ancprev_30y$Indicator
tt = mod1b_ancprev_30y %>% select(-Indicator) 
tt_trans = t(as.matrix(tt))
rownames(tt_trans) = NULL
mod1b_ancprev_30y = tt_trans %>% as.tibble()
colnames(mod1b_ancprev_30y) = rownames_temp_anc

mod1b_ancprev_30y = mod1b_ancprev_30y %>% 
  mutate(Year = as.numeric(Year)) %>% 
  mutate(HIV_prev_ANC_adju_15_19_Mean = `Adjusted_ANC_prevalence,_15-19_Mean`) %>% 
  mutate(HIV_prev_ANC_adju_15_19_LL = `Adjusted_ANC_prevalence,_15-19_95%LL`) %>% 
  mutate(HIV_prev_ANC_adju_15_19_UL = `Adjusted_ANC_prevalence,_15-19_95%UL`) %>% 
  mutate(HIV_prev_ANC_adju_20_24_Mean = `Adjusted_ANC_prevalence,_20-24_Mean`) %>% 
  mutate(HIV_prev_ANC_adju_20_24_LL = `Adjusted_ANC_prevalence,_20-24_95%LL`) %>% 
  mutate(HIV_prev_ANC_adju_20_24_UL = `Adjusted_ANC_prevalence,_20-24_95%UL`) %>% 
  mutate(HIV_prev_ANC_adju_25_29_Mean = `Adjusted_ANC_prevalence,_25-29_Mean`) %>% 
  mutate(HIV_prev_ANC_adju_25_29_LL = `Adjusted_ANC_prevalence,_25-29_95%LL`) %>% 
  mutate(HIV_prev_ANC_adju_25_29_UL = `Adjusted_ANC_prevalence,_25-29_95%UL`) %>% 
  mutate(HIV_prev_ANC_adju_30_34_Mean = `Adjusted_ANC_prevalence,_30-34_Mean`) %>% 
  mutate(HIV_prev_ANC_adju_30_34_LL = `Adjusted_ANC_prevalence,_30-34_95%LL`) %>% 
  mutate(HIV_prev_ANC_adju_30_34_UL = `Adjusted_ANC_prevalence,_30-34_95%UL`) %>% 
  mutate(HIV_prev_ANC_adju_35_39_Mean = `Adjusted_ANC_prevalence,_35-39_Mean`) %>% 
  mutate(HIV_prev_ANC_adju_35_39_LL = `Adjusted_ANC_prevalence,_35-39_95%LL`) %>% 
  mutate(HIV_prev_ANC_adju_35_39_UL = `Adjusted_ANC_prevalence,_35-39_95%UL`) 

mod1b_ancprev_30y = lapply(mod1b_ancprev_30y, function(x) as.numeric(as.character(x))) %>% 
  as_tibble()

mod1b_30y = mod1b_30y %>% 
  left_join(mod1b_ancprev_30y)


# Model 2a: exponentially declining transmission risk, constant age and SW duration in FSW 

mod2a_30y = read.csv("../data/mod2a_30yearART2019.csv", header=FALSE, sep=";") 
mod2a_30y$V2[1] = "Mean"
mod2a_30y = mod2a_30y %>% 
  filter(V2 != "") %>% 
  mutate(V1 = ifelse(V1 =="", NA_character_, V1)) %>% 
  mutate(Indicator = V1) %>% 
  mutate(Indicator = if_else(is.na(Indicator), lag(V1), Indicator)) %>% 
  mutate(Indicator = if_else(is.na(Indicator), lag(Indicator), Indicator)) %>%
  # filter(!apply(across(3:88), 1, function(row){ all(is.na(row)) || all(row =="")})) %>% 
  mutate(Indicator = gsub(" ", "_", Indicator)) %>% 
  mutate(V2 = gsub(" ", "", V2)) %>% 
  mutate(Indicator = paste(Indicator, V2, sep = "_")) %>% 
  select(-V1, -V2) 
mod2a_30y$Indicator[1] = "Year" 

mod2a_30y = mod2a_30y[, -c(72:86)]
rownames_temp = mod2a_30y$Indicator
tt = mod2a_30y %>% select(-Indicator) 
tt_trans = t(as.matrix(tt))
rownames(tt_trans) = NULL
mod2a_30y = tt_trans %>% as.tibble()
colnames(mod2a_30y) = rownames_temp


mod2a_30y = mod2a_30y %>% 
  mutate(Year = as.numeric(Year)) %>% 
  mutate(HIV_incidence_FSW_Mean = HIV_incidence_in_female_sex_workers_Mean) %>% 
  mutate(HIV_incidence_FSW_LL = `HIV_incidence_in_female_sex_workers_95%LL`) %>% 
  mutate(HIV_incidence_FSW_UL = `HIV_incidence_in_female_sex_workers_95%UL`) %>% 
  mutate(HIV_prevalence_FSW_Mean = HIV_prevalence_in_females_sex_workers_Mean) %>% 
  mutate(HIV_prevalence_FSW_LL = `HIV_prevalence_in_females_sex_workers_95%LL`) %>% 
  mutate(HIV_prevalence_FSW_UL = `HIV_prevalence_in_females_sex_workers_95%UL`) %>% 
  mutate(ART_coverage_FSW_Mean = ART_coverage_in_female_sex_workers_Mean) %>% 
  mutate(ART_coverage_FSW_LL = `ART_coverage_in_female_sex_workers_95%LL`) %>% 
  mutate(ART_coverage_FSW_UL = `ART_coverage_in_female_sex_workers_95%UL`) %>% 
  mutate(HIV_prev_ANC_15_19_Mean = `HIV_prevalence_in_pregnant_women,_15-19_Mean`) %>% 
  mutate(HIV_prev_ANC_15_19_LL = `HIV_prevalence_in_pregnant_women,_15-19_95%LL`) %>% 
  mutate(HIV_prev_ANC_15_19_UL = `HIV_prevalence_in_pregnant_women,_15-19_95%UL`) %>% 
  mutate(HIV_prev_ANC_20_24_Mean = `HIV_prevalence_in_pregnant_women,_20-24_Mean`) %>% 
  mutate(HIV_prev_ANC_20_24_LL = `HIV_prevalence_in_pregnant_women,_20-24_95%LL`) %>% 
  mutate(HIV_prev_ANC_20_24_UL = `HIV_prevalence_in_pregnant_women,_20-24_95%UL`) %>% 
  mutate(HIV_prev_ANC_25_29_Mean = `HIV_prevalence_in_pregnant_women,_25-29_Mean`) %>% 
  mutate(HIV_prev_ANC_25_29_LL = `HIV_prevalence_in_pregnant_women,_25-29_95%LL`) %>% 
  mutate(HIV_prev_ANC_25_29_UL = `HIV_prevalence_in_pregnant_women,_25-29_95%UL`) %>% 
  mutate(HIV_prev_ANC_30_34_Mean = `HIV_prevalence_in_pregnant_women,_30-34_Mean`) %>% 
  mutate(HIV_prev_ANC_30_34_LL = `HIV_prevalence_in_pregnant_women,_30-34_95%LL`) %>% 
  mutate(HIV_prev_ANC_30_34_UL = `HIV_prevalence_in_pregnant_women,_30-34_95%UL`) %>% 
  mutate(HIV_prev_ANC_35_39_Mean = `HIV_prevalence_in_pregnant_women,_35-39_Mean`) %>% 
  mutate(HIV_prev_ANC_35_39_LL = `HIV_prevalence_in_pregnant_women,_35-39_95%LL`) %>% 
  mutate(HIV_prev_ANC_35_39_UL = `HIV_prevalence_in_pregnant_women,_35-39_95%UL`) %>% 
  mutate(VL_supp_amongART_LL = `Fraction_of_FSWs_on_ART_who_are_virologically_suppressed_(RNA_count_<1000_copies/ml)_95%LL`) %>% 
  mutate(VL_supp_amongART_UL = `Fraction_of_FSWs_on_ART_who_are_virologically_suppressed_(RNA_count_<1000_copies/ml)_95%UL`) %>% 
  mutate(VL_supp_amongART_Mean = `Fraction_of_FSWs_on_ART_who_are_virologically_suppressed_(RNA_count_<1000_copies/ml)_Mean`)

mod2a_30y = lapply(mod2a_30y, function(x) as.numeric(as.character(x))) %>% 
  as_tibble()

mod2a_30y = mod2a_30y %>% 
  mutate(FSW_assumption = "a - constant FSW age and SW duration") %>% 
  mutate(Transmission_assumption = "2 - exponentially declining transmission probability") %>% 
  mutate(model = "model 2a")

mod2a_ancprev_30y = read.csv2("../data/mod2a_ancprev_30yearART2019.csv", header=FALSE) 
mod2a_ancprev_30y$V2[1] = "Mean"

mod2a_ancprev_30y = mod2a_ancprev_30y %>% 
  filter(V2 != "") %>% 
  mutate(V1 = ifelse(V1 =="", NA_character_, V1)) %>% 
  mutate(Indicator = V1) %>% 
  mutate(Indicator = if_else(is.na(Indicator), lag(V1), Indicator)) %>% 
  mutate(Indicator = if_else(is.na(Indicator), lag(Indicator), Indicator)) %>%
  # filter(!apply(across(3:88), 1, function(row){ all(is.na(row)) || all(row =="")})) %>% 
  mutate(Indicator = gsub(" ", "_", Indicator)) %>% 
  mutate(V2 = gsub(" ", "", V2)) %>% 
  mutate(Indicator = paste(Indicator, V2, sep = "_")) %>% 
  select(-V1, -V2) 
mod2a_ancprev_30y$Indicator[1] = "Year" 

mod2a_ancprev_30y = mod2a_ancprev_30y %>% 
  filter(grepl("Adjusted_ANC", Indicator)| Indicator == "Year")

rownames_temp_anc = mod2a_ancprev_30y$Indicator
tt = mod2a_ancprev_30y %>% select(-Indicator) 
tt_trans = t(as.matrix(tt))
rownames(tt_trans) = NULL
mod2a_ancprev_30y = tt_trans %>% as.tibble()
colnames(mod2a_ancprev_30y) = rownames_temp_anc

mod2a_ancprev_30y = mod2a_ancprev_30y %>% 
  mutate(Year = as.numeric(Year)) %>% 
  mutate(HIV_prev_ANC_adju_15_19_Mean = `Adjusted_ANC_prevalence,_15-19_Mean`) %>% 
  mutate(HIV_prev_ANC_adju_15_19_LL = `Adjusted_ANC_prevalence,_15-19_95%LL`) %>% 
  mutate(HIV_prev_ANC_adju_15_19_UL = `Adjusted_ANC_prevalence,_15-19_95%UL`) %>% 
  mutate(HIV_prev_ANC_adju_20_24_Mean = `Adjusted_ANC_prevalence,_20-24_Mean`) %>% 
  mutate(HIV_prev_ANC_adju_20_24_LL = `Adjusted_ANC_prevalence,_20-24_95%LL`) %>% 
  mutate(HIV_prev_ANC_adju_20_24_UL = `Adjusted_ANC_prevalence,_20-24_95%UL`) %>% 
  mutate(HIV_prev_ANC_adju_25_29_Mean = `Adjusted_ANC_prevalence,_25-29_Mean`) %>% 
  mutate(HIV_prev_ANC_adju_25_29_LL = `Adjusted_ANC_prevalence,_25-29_95%LL`) %>% 
  mutate(HIV_prev_ANC_adju_25_29_UL = `Adjusted_ANC_prevalence,_25-29_95%UL`) %>% 
  mutate(HIV_prev_ANC_adju_30_34_Mean = `Adjusted_ANC_prevalence,_30-34_Mean`) %>% 
  mutate(HIV_prev_ANC_adju_30_34_LL = `Adjusted_ANC_prevalence,_30-34_95%LL`) %>% 
  mutate(HIV_prev_ANC_adju_30_34_UL = `Adjusted_ANC_prevalence,_30-34_95%UL`) %>% 
  mutate(HIV_prev_ANC_adju_35_39_Mean = `Adjusted_ANC_prevalence,_35-39_Mean`) %>% 
  mutate(HIV_prev_ANC_adju_35_39_LL = `Adjusted_ANC_prevalence,_35-39_95%LL`) %>% 
  mutate(HIV_prev_ANC_adju_35_39_UL = `Adjusted_ANC_prevalence,_35-39_95%UL`) 

mod2a_ancprev_30y = lapply(mod2a_ancprev_30y, function(x) as.numeric(as.character(x))) %>% 
  as_tibble()

mod2a_30y = mod2a_30y %>% 
  left_join(mod2a_ancprev_30y)

## Model 2b: exponentially declining transmission risk, increasing age and SW duraiton in FSW 

mod2b_30y = read.csv("../data/mod2b_30yearART2019.csv", header=FALSE, sep=";") 

mod2b_30y$V2[1] = "Mean"
mod2b_30y = mod2b_30y %>% 
  filter(V2 != "") %>% 
  mutate(V1 = ifelse(V1 =="", NA_character_, V1)) %>% 
  mutate(Indicator = V1) %>% 
  mutate(Indicator = if_else(is.na(Indicator), lag(V1), Indicator)) %>% 
  mutate(Indicator = if_else(is.na(Indicator), lag(Indicator), Indicator)) %>%
  mutate(Indicator = gsub(" ", "_", Indicator)) %>% 
  mutate(V2 = gsub(" ", "", V2)) %>% 
  mutate(Indicator = paste(Indicator, V2, sep = "_")) %>% 
  select(-V1, -V2) 
mod2b_30y$Indicator[1] = "Year" 

mod2b_30y = mod2b_30y[, -c(72:86)]
rownames_temp = mod2b_30y$Indicator
tt = mod2b_30y %>% select(-Indicator) 
tt_trans = t(as.matrix(tt))
rownames(tt_trans) = NULL
mod2b_30y = tt_trans %>% as.tibble()
colnames(mod2b_30y) = rownames_temp

mod2b_30y = mod2b_30y %>% 
  mutate(Year = as.numeric(Year)) %>% 
  mutate(HIV_incidence_FSW_Mean = HIV_incidence_in_female_sex_workers_Mean) %>% 
  mutate(HIV_incidence_FSW_LL = `HIV_incidence_in_female_sex_workers_95%LL`) %>% 
  mutate(HIV_incidence_FSW_UL = `HIV_incidence_in_female_sex_workers_95%UL`) %>% 
  mutate(HIV_prevalence_FSW_Mean = HIV_prevalence_in_females_sex_workers_Mean) %>% 
  mutate(HIV_prevalence_FSW_LL = `HIV_prevalence_in_females_sex_workers_95%LL`) %>% 
  mutate(HIV_prevalence_FSW_UL = `HIV_prevalence_in_females_sex_workers_95%UL`) %>% 
  mutate(ART_coverage_FSW_Mean = ART_coverage_in_female_sex_workers_Mean) %>% 
  mutate(ART_coverage_FSW_LL = `ART_coverage_in_female_sex_workers_95%LL`) %>% 
  mutate(ART_coverage_FSW_UL = `ART_coverage_in_female_sex_workers_95%UL`) %>% 
  mutate(HIV_prev_ANC_15_19_Mean = `HIV_prevalence_in_pregnant_women,_15-19_Mean`) %>% 
  mutate(HIV_prev_ANC_15_19_LL = `HIV_prevalence_in_pregnant_women,_15-19_95%LL`) %>% 
  mutate(HIV_prev_ANC_15_19_UL = `HIV_prevalence_in_pregnant_women,_15-19_95%UL`) %>% 
  mutate(HIV_prev_ANC_20_24_Mean = `HIV_prevalence_in_pregnant_women,_20-24_Mean`) %>% 
  mutate(HIV_prev_ANC_20_24_LL = `HIV_prevalence_in_pregnant_women,_20-24_95%LL`) %>% 
  mutate(HIV_prev_ANC_20_24_UL = `HIV_prevalence_in_pregnant_women,_20-24_95%UL`) %>% 
  mutate(HIV_prev_ANC_25_29_Mean = `HIV_prevalence_in_pregnant_women,_25-29_Mean`) %>% 
  mutate(HIV_prev_ANC_25_29_LL = `HIV_prevalence_in_pregnant_women,_25-29_95%LL`) %>% 
  mutate(HIV_prev_ANC_25_29_UL = `HIV_prevalence_in_pregnant_women,_25-29_95%UL`) %>% 
  mutate(HIV_prev_ANC_30_34_Mean = `HIV_prevalence_in_pregnant_women,_30-34_Mean`) %>% 
  mutate(HIV_prev_ANC_30_34_LL = `HIV_prevalence_in_pregnant_women,_30-34_95%LL`) %>% 
  mutate(HIV_prev_ANC_30_34_UL = `HIV_prevalence_in_pregnant_women,_30-34_95%UL`) %>% 
  mutate(HIV_prev_ANC_35_39_Mean = `HIV_prevalence_in_pregnant_women,_35-39_Mean`) %>% 
  mutate(HIV_prev_ANC_35_39_LL = `HIV_prevalence_in_pregnant_women,_35-39_95%LL`) %>% 
  mutate(HIV_prev_ANC_35_39_UL = `HIV_prevalence_in_pregnant_women,_35-39_95%UL`) %>% 
  mutate(VL_supp_amongART_LL = `Fraction_of_FSWs_on_ART_who_are_virologically_suppressed_(RNA_count_<1000_copies/ml)_95%LL`) %>% 
  mutate(VL_supp_amongART_UL = `Fraction_of_FSWs_on_ART_who_are_virologically_suppressed_(RNA_count_<1000_copies/ml)_95%UL`) %>% 
  mutate(VL_supp_amongART_Mean = `Fraction_of_FSWs_on_ART_who_are_virologically_suppressed_(RNA_count_<1000_copies/ml)_Mean`)

mod2b_30y = lapply(mod2b_30y, function(x) as.numeric(as.character(x))) %>% 
  as_tibble()

mod2b_30y = mod2b_30y %>% 
  mutate(FSW_assumption = "b - changing FSW age and SW duration") %>% 
  mutate(Transmission_assumption = "2 - exponentially declining transmission probability") %>% 
  mutate(model = "model 2b")


mod2b_ancprev_30y = read.csv2("../data/mod2b_ancprev_30yearART2019.csv", header=FALSE) 
mod2b_ancprev_30y$V2[1] = "Mean"

mod2b_ancprev_30y = mod2b_ancprev_30y %>% 
  filter(V2 != "") %>% 
  mutate(V1 = ifelse(V1 =="", NA_character_, V1)) %>% 
  mutate(Indicator = V1) %>% 
  mutate(Indicator = if_else(is.na(Indicator), lag(V1), Indicator)) %>% 
  mutate(Indicator = if_else(is.na(Indicator), lag(Indicator), Indicator)) %>%
  mutate(Indicator = gsub(" ", "_", Indicator)) %>% 
  mutate(V2 = gsub(" ", "", V2)) %>% 
  mutate(Indicator = paste(Indicator, V2, sep = "_")) %>% 
  select(-V1, -V2) 
mod2b_ancprev_30y$Indicator[1] = "Year" 

mod2b_ancprev_30y = mod2b_ancprev_30y %>% 
  filter(grepl("Adjusted_ANC", Indicator)| Indicator == "Year")

rownames_temp_anc = mod2b_ancprev_30y$Indicator
tt = mod2b_ancprev_30y %>% select(-Indicator) 
tt_trans = t(as.matrix(tt))
rownames(tt_trans) = NULL
mod2b_ancprev_30y = tt_trans %>% as.tibble()
colnames(mod2b_ancprev_30y) = rownames_temp_anc

mod2b_ancprev_30y = mod2b_ancprev_30y %>% 
  mutate(Year = as.numeric(Year)) %>% 
  mutate(HIV_prev_ANC_adju_15_19_Mean = `Adjusted_ANC_prevalence,_15-19_Mean`) %>% 
  mutate(HIV_prev_ANC_adju_15_19_LL = `Adjusted_ANC_prevalence,_15-19_95%LL`) %>% 
  mutate(HIV_prev_ANC_adju_15_19_UL = `Adjusted_ANC_prevalence,_15-19_95%UL`) %>% 
  mutate(HIV_prev_ANC_adju_20_24_Mean = `Adjusted_ANC_prevalence,_20-24_Mean`) %>% 
  mutate(HIV_prev_ANC_adju_20_24_LL = `Adjusted_ANC_prevalence,_20-24_95%LL`) %>% 
  mutate(HIV_prev_ANC_adju_20_24_UL = `Adjusted_ANC_prevalence,_20-24_95%UL`) %>% 
  mutate(HIV_prev_ANC_adju_25_29_Mean = `Adjusted_ANC_prevalence,_25-29_Mean`) %>% 
  mutate(HIV_prev_ANC_adju_25_29_LL = `Adjusted_ANC_prevalence,_25-29_95%LL`) %>% 
  mutate(HIV_prev_ANC_adju_25_29_UL = `Adjusted_ANC_prevalence,_25-29_95%UL`) %>% 
  mutate(HIV_prev_ANC_adju_30_34_Mean = `Adjusted_ANC_prevalence,_30-34_Mean`) %>% 
  mutate(HIV_prev_ANC_adju_30_34_LL = `Adjusted_ANC_prevalence,_30-34_95%LL`) %>% 
  mutate(HIV_prev_ANC_adju_30_34_UL = `Adjusted_ANC_prevalence,_30-34_95%UL`) %>% 
  mutate(HIV_prev_ANC_adju_35_39_Mean = `Adjusted_ANC_prevalence,_35-39_Mean`) %>% 
  mutate(HIV_prev_ANC_adju_35_39_LL = `Adjusted_ANC_prevalence,_35-39_95%LL`) %>% 
  mutate(HIV_prev_ANC_adju_35_39_UL = `Adjusted_ANC_prevalence,_35-39_95%UL`) 

mod2b_ancprev_30y = lapply(mod2b_ancprev_30y, function(x) as.numeric(as.character(x))) %>% 
  as_tibble()

mod2b_30y = mod2b_30y %>% 
  left_join(mod2b_ancprev_30y)

# Model 3a: exposure-dependent transmission risk, constant age and SW duration in FSW 

mod3a_30y = read.csv("../data/mod3a_30yearART2019.csv", header=FALSE, sep=";") 
mod3a_30y$V2[1] = "Mean"
mod3a_30y = mod3a_30y %>% 
  filter(V2 != "") %>% 
  mutate(V1 = ifelse(V1 =="", NA_character_, V1)) %>% 
  mutate(Indicator = V1) %>% 
  mutate(Indicator = if_else(is.na(Indicator), lag(V1), Indicator)) %>% 
  mutate(Indicator = if_else(is.na(Indicator), lag(Indicator), Indicator)) %>%
  mutate(Indicator = gsub(" ", "_", Indicator)) %>% 
  mutate(V2 = gsub(" ", "", V2)) %>% 
  mutate(Indicator = paste(Indicator, V2, sep = "_")) %>% 
  select(-V1, -V2) 
mod3a_30y$Indicator[1] = "Year" 

mod3a_30y = mod3a_30y[, -c(72:86)]
rownames_temp = mod3a_30y$Indicator
tt = mod3a_30y %>% select(-Indicator) 
tt_trans = t(as.matrix(tt))
rownames(tt_trans) = NULL
mod3a_30y = tt_trans %>% as.tibble()
colnames(mod3a_30y) = rownames_temp


mod3a_30y = mod3a_30y %>% 
  mutate(Year = as.numeric(Year)) %>% 
  mutate(HIV_incidence_FSW_Mean = HIV_incidence_in_female_sex_workers_Mean) %>% 
  mutate(HIV_incidence_FSW_LL = `HIV_incidence_in_female_sex_workers_95%LL`) %>% 
  mutate(HIV_incidence_FSW_UL = `HIV_incidence_in_female_sex_workers_95%UL`) %>% 
  mutate(HIV_prevalence_FSW_Mean = HIV_prevalence_in_females_sex_workers_Mean) %>% 
  mutate(HIV_prevalence_FSW_LL = `HIV_prevalence_in_females_sex_workers_95%LL`) %>% 
  mutate(HIV_prevalence_FSW_UL = `HIV_prevalence_in_females_sex_workers_95%UL`) %>% 
  mutate(ART_coverage_FSW_Mean = ART_coverage_in_female_sex_workers_Mean) %>% 
  mutate(ART_coverage_FSW_LL = `ART_coverage_in_female_sex_workers_95%LL`) %>% 
  mutate(ART_coverage_FSW_UL = `ART_coverage_in_female_sex_workers_95%UL`) %>% 
  mutate(HIV_prev_ANC_15_19_Mean = `HIV_prevalence_in_pregnant_women,_15-19_Mean`) %>% 
  mutate(HIV_prev_ANC_15_19_LL = `HIV_prevalence_in_pregnant_women,_15-19_95%LL`) %>% 
  mutate(HIV_prev_ANC_15_19_UL = `HIV_prevalence_in_pregnant_women,_15-19_95%UL`) %>% 
  mutate(HIV_prev_ANC_20_24_Mean = `HIV_prevalence_in_pregnant_women,_20-24_Mean`) %>% 
  mutate(HIV_prev_ANC_20_24_LL = `HIV_prevalence_in_pregnant_women,_20-24_95%LL`) %>% 
  mutate(HIV_prev_ANC_20_24_UL = `HIV_prevalence_in_pregnant_women,_20-24_95%UL`) %>% 
  mutate(HIV_prev_ANC_25_29_Mean = `HIV_prevalence_in_pregnant_women,_25-29_Mean`) %>% 
  mutate(HIV_prev_ANC_25_29_LL = `HIV_prevalence_in_pregnant_women,_25-29_95%LL`) %>% 
  mutate(HIV_prev_ANC_25_29_UL = `HIV_prevalence_in_pregnant_women,_25-29_95%UL`) %>% 
  mutate(HIV_prev_ANC_30_34_Mean = `HIV_prevalence_in_pregnant_women,_30-34_Mean`) %>% 
  mutate(HIV_prev_ANC_30_34_LL = `HIV_prevalence_in_pregnant_women,_30-34_95%LL`) %>% 
  mutate(HIV_prev_ANC_30_34_UL = `HIV_prevalence_in_pregnant_women,_30-34_95%UL`) %>% 
  mutate(HIV_prev_ANC_35_39_Mean = `HIV_prevalence_in_pregnant_women,_35-39_Mean`) %>% 
  mutate(HIV_prev_ANC_35_39_LL = `HIV_prevalence_in_pregnant_women,_35-39_95%LL`) %>% 
  mutate(HIV_prev_ANC_35_39_UL = `HIV_prevalence_in_pregnant_women,_35-39_95%UL`) %>% 
  mutate(VL_supp_amongART_LL = `Fraction_of_FSWs_on_ART_who_are_virologically_suppressed_(RNA_count_<1000_copies/ml)_95%LL`) %>% 
  mutate(VL_supp_amongART_UL = `Fraction_of_FSWs_on_ART_who_are_virologically_suppressed_(RNA_count_<1000_copies/ml)_95%UL`) %>% 
  mutate(VL_supp_amongART_Mean = `Fraction_of_FSWs_on_ART_who_are_virologically_suppressed_(RNA_count_<1000_copies/ml)_Mean`)

mod3a_30y = lapply(mod3a_30y, function(x) as.numeric(as.character(x))) %>% 
  as_tibble()

mod3a_30y = mod3a_30y %>% 
  mutate(FSW_assumption = "a - constant FSW age and SW duration") %>% 
  mutate(Transmission_assumption = "3 - dynamically changing transmission probability") %>% 
  mutate(model = "model 3a")

mod3a_ancprev_30y = read.csv2("../data/mod3a_ancprev_30yearART2019.csv", header=FALSE) 
mod3a_ancprev_30y$V2[1] = "Mean"

mod3a_ancprev_30y = mod3a_ancprev_30y %>% 
  filter(V2 != "") %>% 
  mutate(V1 = ifelse(V1 =="", NA_character_, V1)) %>% 
  mutate(Indicator = V1) %>% 
  mutate(Indicator = if_else(is.na(Indicator), lag(V1), Indicator)) %>% 
  mutate(Indicator = if_else(is.na(Indicator), lag(Indicator), Indicator)) %>%
  # filter(!apply(across(3:88), 1, function(row){ all(is.na(row)) || all(row =="")})) %>% 
  mutate(Indicator = gsub(" ", "_", Indicator)) %>% 
  mutate(V2 = gsub(" ", "", V2)) %>% 
  mutate(Indicator = paste(Indicator, V2, sep = "_")) %>% 
  select(-V1, -V2) 
mod3a_ancprev_30y$Indicator[1] = "Year" 

mod3a_ancprev_30y = mod3a_ancprev_30y %>% 
  filter(grepl("Adjusted_ANC", Indicator)| Indicator == "Year")

rownames_temp_anc = mod3a_ancprev_30y$Indicator
tt = mod3a_ancprev_30y %>% select(-Indicator) 
tt_trans = t(as.matrix(tt))
rownames(tt_trans) = NULL
mod3a_ancprev_30y = tt_trans %>% as.tibble()
colnames(mod3a_ancprev_30y) = rownames_temp_anc

mod3a_ancprev_30y = mod3a_ancprev_30y %>% 
  mutate(Year = as.numeric(Year)) %>% 
  mutate(HIV_prev_ANC_adju_15_19_Mean = `Adjusted_ANC_prevalence,_15-19_Mean`) %>% 
  mutate(HIV_prev_ANC_adju_15_19_LL = `Adjusted_ANC_prevalence,_15-19_95%LL`) %>% 
  mutate(HIV_prev_ANC_adju_15_19_UL = `Adjusted_ANC_prevalence,_15-19_95%UL`) %>% 
  mutate(HIV_prev_ANC_adju_20_24_Mean = `Adjusted_ANC_prevalence,_20-24_Mean`) %>% 
  mutate(HIV_prev_ANC_adju_20_24_LL = `Adjusted_ANC_prevalence,_20-24_95%LL`) %>% 
  mutate(HIV_prev_ANC_adju_20_24_UL = `Adjusted_ANC_prevalence,_20-24_95%UL`) %>% 
  mutate(HIV_prev_ANC_adju_25_29_Mean = `Adjusted_ANC_prevalence,_25-29_Mean`) %>% 
  mutate(HIV_prev_ANC_adju_25_29_LL = `Adjusted_ANC_prevalence,_25-29_95%LL`) %>% 
  mutate(HIV_prev_ANC_adju_25_29_UL = `Adjusted_ANC_prevalence,_25-29_95%UL`) %>% 
  mutate(HIV_prev_ANC_adju_30_34_Mean = `Adjusted_ANC_prevalence,_30-34_Mean`) %>% 
  mutate(HIV_prev_ANC_adju_30_34_LL = `Adjusted_ANC_prevalence,_30-34_95%LL`) %>% 
  mutate(HIV_prev_ANC_adju_30_34_UL = `Adjusted_ANC_prevalence,_30-34_95%UL`) %>% 
  mutate(HIV_prev_ANC_adju_35_39_Mean = `Adjusted_ANC_prevalence,_35-39_Mean`) %>% 
  mutate(HIV_prev_ANC_adju_35_39_LL = `Adjusted_ANC_prevalence,_35-39_95%LL`) %>% 
  mutate(HIV_prev_ANC_adju_35_39_UL = `Adjusted_ANC_prevalence,_35-39_95%UL`) 

mod3a_ancprev_30y = lapply(mod3a_ancprev_30y, function(x) as.numeric(as.character(x))) %>% 
  as_tibble()

mod3a_30y = mod3a_30y %>% 
  left_join(mod3a_ancprev_30y)

## Model 3b: exposure-dependent transmission risk, increasing age and SW duration in FSW 

mod3b_30y = read.csv("../data/mod3b_30yearART2019.csv", header=FALSE, sep=";") 
mod3b_30y$V2[1] = "Mean"
mod3b_30y = mod3b_30y %>% 
  filter(V2 != "") %>% 
  mutate(V1 = ifelse(V1 =="", NA_character_, V1)) %>% 
  mutate(Indicator = V1) %>% 
  mutate(Indicator = if_else(is.na(Indicator), lag(V1), Indicator)) %>% 
  mutate(Indicator = if_else(is.na(Indicator), lag(Indicator), Indicator)) %>%
  # filter(!apply(across(3:88), 1, function(row){ all(is.na(row)) || all(row =="")})) %>% 
  mutate(Indicator = gsub(" ", "_", Indicator)) %>% 
  mutate(V2 = gsub(" ", "", V2)) %>% 
  mutate(Indicator = paste(Indicator, V2, sep = "_")) %>% 
  select(-V1, -V2) 
mod3b_30y$Indicator[1] = "Year" 

mod3b_30y = mod3b_30y[, -c(72:86)]
rownames_temp = mod3b_30y$Indicator
tt = mod3b_30y %>% select(-Indicator) 
tt_trans = t(as.matrix(tt))
rownames(tt_trans) = NULL
mod3b_30y = tt_trans %>% as.tibble()
colnames(mod3b_30y) = rownames_temp

mod3b_30y = mod3b_30y %>% 
  mutate(Year = as.numeric(Year)) %>% 
  mutate(HIV_incidence_FSW_Mean = HIV_incidence_in_female_sex_workers_Mean) %>% 
  mutate(HIV_incidence_FSW_LL = `HIV_incidence_in_female_sex_workers_95%LL`) %>% 
  mutate(HIV_incidence_FSW_UL = `HIV_incidence_in_female_sex_workers_95%UL`) %>% 
  mutate(HIV_prevalence_FSW_Mean = HIV_prevalence_in_females_sex_workers_Mean) %>% 
  mutate(HIV_prevalence_FSW_LL = `HIV_prevalence_in_females_sex_workers_95%LL`) %>% 
  mutate(HIV_prevalence_FSW_UL = `HIV_prevalence_in_females_sex_workers_95%UL`) %>% 
  mutate(ART_coverage_FSW_Mean = ART_coverage_in_female_sex_workers_Mean) %>% 
  mutate(ART_coverage_FSW_LL = `ART_coverage_in_female_sex_workers_95%LL`) %>% 
  mutate(ART_coverage_FSW_UL = `ART_coverage_in_female_sex_workers_95%UL`) %>% 
  mutate(HIV_prev_ANC_15_19_Mean = `HIV_prevalence_in_pregnant_women,_15-19_Mean`) %>% 
  mutate(HIV_prev_ANC_15_19_LL = `HIV_prevalence_in_pregnant_women,_15-19_95%LL`) %>% 
  mutate(HIV_prev_ANC_15_19_UL = `HIV_prevalence_in_pregnant_women,_15-19_95%UL`) %>% 
  mutate(HIV_prev_ANC_20_24_Mean = `HIV_prevalence_in_pregnant_women,_20-24_Mean`) %>% 
  mutate(HIV_prev_ANC_20_24_LL = `HIV_prevalence_in_pregnant_women,_20-24_95%LL`) %>% 
  mutate(HIV_prev_ANC_20_24_UL = `HIV_prevalence_in_pregnant_women,_20-24_95%UL`) %>% 
  mutate(HIV_prev_ANC_25_29_Mean = `HIV_prevalence_in_pregnant_women,_25-29_Mean`) %>% 
  mutate(HIV_prev_ANC_25_29_LL = `HIV_prevalence_in_pregnant_women,_25-29_95%LL`) %>% 
  mutate(HIV_prev_ANC_25_29_UL = `HIV_prevalence_in_pregnant_women,_25-29_95%UL`) %>% 
  mutate(HIV_prev_ANC_30_34_Mean = `HIV_prevalence_in_pregnant_women,_30-34_Mean`) %>% 
  mutate(HIV_prev_ANC_30_34_LL = `HIV_prevalence_in_pregnant_women,_30-34_95%LL`) %>% 
  mutate(HIV_prev_ANC_30_34_UL = `HIV_prevalence_in_pregnant_women,_30-34_95%UL`) %>% 
  mutate(HIV_prev_ANC_35_39_Mean = `HIV_prevalence_in_pregnant_women,_35-39_Mean`) %>% 
  mutate(HIV_prev_ANC_35_39_LL = `HIV_prevalence_in_pregnant_women,_35-39_95%LL`) %>% 
  mutate(HIV_prev_ANC_35_39_UL = `HIV_prevalence_in_pregnant_women,_35-39_95%UL`) %>% 
  mutate(VL_supp_amongART_LL = `Fraction_of_FSWs_on_ART_who_are_virologically_suppressed_(RNA_count_<1000_copies/ml)_95%LL`) %>% 
  mutate(VL_supp_amongART_UL = `Fraction_of_FSWs_on_ART_who_are_virologically_suppressed_(RNA_count_<1000_copies/ml)_95%UL`) %>% 
  mutate(VL_supp_amongART_Mean = `Fraction_of_FSWs_on_ART_who_are_virologically_suppressed_(RNA_count_<1000_copies/ml)_Mean`)

mod3b_30y = lapply(mod3b_30y, function(x) as.numeric(as.character(x))) %>% 
  as_tibble()

mod3b_30y = mod3b_30y %>% 
  mutate(FSW_assumption = "b - changing FSW age and SW duration") %>% 
  mutate(Transmission_assumption = "3 - dynamically changing transmission probability") %>% 
  mutate(model = "model 3b")

mod3b_ancprev_30y = read.csv2("../data/mod3b_ancprev_30yearART2019.csv", header=FALSE) 
mod3b_ancprev_30y$V2[1] = "Mean"

mod3b_ancprev_30y = mod3b_ancprev_30y %>% 
  filter(V2 != "") %>% 
  mutate(V1 = ifelse(V1 =="", NA_character_, V1)) %>% 
  mutate(Indicator = V1) %>% 
  mutate(Indicator = if_else(is.na(Indicator), lag(V1), Indicator)) %>% 
  mutate(Indicator = if_else(is.na(Indicator), lag(Indicator), Indicator)) %>%
  mutate(Indicator = gsub(" ", "_", Indicator)) %>% 
  mutate(V2 = gsub(" ", "", V2)) %>% 
  mutate(Indicator = paste(Indicator, V2, sep = "_")) %>% 
  select(-V1, -V2) 
mod3b_ancprev_30y$Indicator[1] = "Year" 

mod3b_ancprev_30y = mod3b_ancprev_30y %>% 
  filter(grepl("Adjusted_ANC", Indicator)| Indicator == "Year")

rownames_temp_anc = mod3b_ancprev_30y$Indicator
tt = mod3b_ancprev_30y %>% select(-Indicator) 
tt_trans = t(as.matrix(tt))
rownames(tt_trans) = NULL
mod3b_ancprev_30y = tt_trans %>% as.tibble()
colnames(mod3b_ancprev_30y) = rownames_temp_anc

mod3b_ancprev_30y = mod3b_ancprev_30y %>% 
  mutate(Year = as.numeric(Year)) %>% 
  mutate(HIV_prev_ANC_adju_15_19_Mean = `Adjusted_ANC_prevalence,_15-19_Mean`) %>% 
  mutate(HIV_prev_ANC_adju_15_19_LL = `Adjusted_ANC_prevalence,_15-19_95%LL`) %>% 
  mutate(HIV_prev_ANC_adju_15_19_UL = `Adjusted_ANC_prevalence,_15-19_95%UL`) %>% 
  mutate(HIV_prev_ANC_adju_20_24_Mean = `Adjusted_ANC_prevalence,_20-24_Mean`) %>% 
  mutate(HIV_prev_ANC_adju_20_24_LL = `Adjusted_ANC_prevalence,_20-24_95%LL`) %>% 
  mutate(HIV_prev_ANC_adju_20_24_UL = `Adjusted_ANC_prevalence,_20-24_95%UL`) %>% 
  mutate(HIV_prev_ANC_adju_25_29_Mean = `Adjusted_ANC_prevalence,_25-29_Mean`) %>% 
  mutate(HIV_prev_ANC_adju_25_29_LL = `Adjusted_ANC_prevalence,_25-29_95%LL`) %>% 
  mutate(HIV_prev_ANC_adju_25_29_UL = `Adjusted_ANC_prevalence,_25-29_95%UL`) %>% 
  mutate(HIV_prev_ANC_adju_30_34_Mean = `Adjusted_ANC_prevalence,_30-34_Mean`) %>% 
  mutate(HIV_prev_ANC_adju_30_34_LL = `Adjusted_ANC_prevalence,_30-34_95%LL`) %>% 
  mutate(HIV_prev_ANC_adju_30_34_UL = `Adjusted_ANC_prevalence,_30-34_95%UL`) %>% 
  mutate(HIV_prev_ANC_adju_35_39_Mean = `Adjusted_ANC_prevalence,_35-39_Mean`) %>% 
  mutate(HIV_prev_ANC_adju_35_39_LL = `Adjusted_ANC_prevalence,_35-39_95%LL`) %>% 
  mutate(HIV_prev_ANC_adju_35_39_UL = `Adjusted_ANC_prevalence,_35-39_95%UL`) 

mod3b_ancprev_30y = lapply(mod3b_ancprev_30y, function(x) as.numeric(as.character(x))) %>% 
  as_tibble()

mod3b_30y = mod3b_30y %>% 
  left_join(mod3b_ancprev_30y)

data_models_30y = mod1a_30y %>% 
  bind_rows(mod1b_30y) %>% 
  bind_rows(mod2a_30y) %>% 
  bind_rows(mod2b_30y) %>% 
  bind_rows(mod3a_30y) %>% 
  bind_rows(mod3b_30y)

####################################################################################################################################################################
### "Sensitivity analyses: "b" scenarios (increasing age and SW duration in FSW) differentiated between "only increasing age" and "only increasing SW duration". ###
####################################################################################################################################################################
### Model 1b age: (constant transmision, constant SW duration, incrasing age)
mod1b_30y_age = read.csv("../data/mod1b_30yearART2019_age.csv", header=FALSE, sep=";") 
mod1b_30y_age$V2[1] = "Mean"
mod1b_30y_age = mod1b_30y_age %>% 
  filter(V2 != "") %>% 
  mutate(V1 = ifelse(V1 =="", NA_character_, V1)) %>% 
  mutate(Indicator = V1) %>% 
  mutate(Indicator = if_else(is.na(Indicator), lag(V1), Indicator)) %>% 
  mutate(Indicator = if_else(is.na(Indicator), lag(Indicator), Indicator)) %>%
  # filter(!apply(across(3:88), 1, function(row){ all(is.na(row)) || all(row =="")})) %>% 
  mutate(Indicator = gsub(" ", "_", Indicator)) %>% 
  mutate(V2 = gsub(" ", "", V2)) %>% 
  mutate(Indicator = paste(Indicator, V2, sep = "_")) %>% 
  select(-V1, -V2) 
mod1b_30y_age$Indicator[1] = "Year" 

mod1b_30y_age = mod1b_30y_age[, -c(72:86)]
rownames_temp = mod1b_30y_age$Indicator
tt = mod1b_30y_age %>% select(-Indicator) 
tt_trans = t(as.matrix(tt))
rownames(tt_trans) = NULL
mod1b_30y_age = tt_trans %>% as.tibble()
colnames(mod1b_30y_age) = rownames_temp

mod1b_30y_age = mod1b_30y_age %>% 
  mutate(Year = as.numeric(Year)) %>% 
  mutate(HIV_incidence_FSW_Mean = HIV_incidence_in_female_sex_workers_Mean) %>% 
  mutate(HIV_incidence_FSW_LL = `HIV_incidence_in_female_sex_workers_95%LL`) %>% 
  mutate(HIV_incidence_FSW_UL = `HIV_incidence_in_female_sex_workers_95%UL`) %>% 
  mutate(HIV_prevalence_FSW_Mean = HIV_prevalence_in_females_sex_workers_Mean) %>% 
  mutate(HIV_prevalence_FSW_LL = `HIV_prevalence_in_females_sex_workers_95%LL`) %>% 
  mutate(HIV_prevalence_FSW_UL = `HIV_prevalence_in_females_sex_workers_95%UL`) %>% 
  mutate(ART_coverage_FSW_Mean = ART_coverage_in_female_sex_workers_Mean) %>% 
  mutate(ART_coverage_FSW_LL = `ART_coverage_in_female_sex_workers_95%LL`) %>% 
  mutate(ART_coverage_FSW_UL = `ART_coverage_in_female_sex_workers_95%UL`) %>% 
  mutate(HIV_prev_ANC_15_19_Mean = `HIV_prevalence_in_pregnant_women,_15-19_Mean`) %>% 
  mutate(HIV_prev_ANC_15_19_LL = `HIV_prevalence_in_pregnant_women,_15-19_95%LL`) %>% 
  mutate(HIV_prev_ANC_15_19_UL = `HIV_prevalence_in_pregnant_women,_15-19_95%UL`) %>% 
  mutate(HIV_prev_ANC_20_24_Mean = `HIV_prevalence_in_pregnant_women,_20-24_Mean`) %>% 
  mutate(HIV_prev_ANC_20_24_LL = `HIV_prevalence_in_pregnant_women,_20-24_95%LL`) %>% 
  mutate(HIV_prev_ANC_20_24_UL = `HIV_prevalence_in_pregnant_women,_20-24_95%UL`) %>% 
  mutate(HIV_prev_ANC_25_29_Mean = `HIV_prevalence_in_pregnant_women,_25-29_Mean`) %>% 
  mutate(HIV_prev_ANC_25_29_LL = `HIV_prevalence_in_pregnant_women,_25-29_95%LL`) %>% 
  mutate(HIV_prev_ANC_25_29_UL = `HIV_prevalence_in_pregnant_women,_25-29_95%UL`) %>% 
  mutate(HIV_prev_ANC_30_34_Mean = `HIV_prevalence_in_pregnant_women,_30-34_Mean`) %>% 
  mutate(HIV_prev_ANC_30_34_LL = `HIV_prevalence_in_pregnant_women,_30-34_95%LL`) %>% 
  mutate(HIV_prev_ANC_30_34_UL = `HIV_prevalence_in_pregnant_women,_30-34_95%UL`) %>% 
  mutate(HIV_prev_ANC_35_39_Mean = `HIV_prevalence_in_pregnant_women,_35-39_Mean`) %>% 
  mutate(HIV_prev_ANC_35_39_LL = `HIV_prevalence_in_pregnant_women,_35-39_95%LL`) %>% 
  mutate(HIV_prev_ANC_35_39_UL = `HIV_prevalence_in_pregnant_women,_35-39_95%UL`) %>% 
  mutate(VL_supp_amongART_LL = `Fraction_of_FSWs_on_ART_who_are_virologically_suppressed_(RNA_count_<1000_copies/ml)_95%LL`) %>% 
  mutate(VL_supp_amongART_UL = `Fraction_of_FSWs_on_ART_who_are_virologically_suppressed_(RNA_count_<1000_copies/ml)_95%UL`) %>% 
  mutate(VL_supp_amongART_Mean = `Fraction_of_FSWs_on_ART_who_are_virologically_suppressed_(RNA_count_<1000_copies/ml)_Mean`)

mod1b_30y_age = lapply(mod1b_30y_age, function(x) as.numeric(as.character(x))) %>% 
  as_tibble()

mod1b_30y_age = mod1b_30y_age %>% 
  mutate(FSW_assumption = "b[age] - changing FSW age, constant SW duration") %>% 
  mutate(Transmission_assumption = "1 - constant transmission probability") %>% 
  mutate(model = "model 1b[age]")

mod1b_ancprev_30y_age = read.csv2("../data/mod1b_ancprev_30yearART2019_age.csv", header=FALSE) 
mod1b_ancprev_30y_age$V2[1] = "Mean"

mod1b_ancprev_30y_age = mod1b_ancprev_30y_age %>% 
  filter(V2 != "") %>% 
  mutate(V1 = ifelse(V1 =="", NA_character_, V1)) %>% 
  mutate(Indicator = V1) %>% 
  mutate(Indicator = if_else(is.na(Indicator), lag(V1), Indicator)) %>% 
  mutate(Indicator = if_else(is.na(Indicator), lag(Indicator), Indicator)) %>%
  mutate(Indicator = gsub(" ", "_", Indicator)) %>% 
  mutate(V2 = gsub(" ", "", V2)) %>% 
  mutate(Indicator = paste(Indicator, V2, sep = "_")) %>% 
  select(-V1, -V2) 
mod1b_ancprev_30y_age$Indicator[1] = "Year" 

mod1b_ancprev_30y_age = mod1b_ancprev_30y_age %>% 
  filter(grepl("Adjusted_ANC", Indicator)| Indicator == "Year")

rownames_temp_anc = mod1b_ancprev_30y_age$Indicator
tt = mod1b_ancprev_30y_age %>% select(-Indicator) 
tt_trans = t(as.matrix(tt))
rownames(tt_trans) = NULL
mod1b_ancprev_30y_age = tt_trans %>% as.tibble()
colnames(mod1b_ancprev_30y_age) = rownames_temp_anc

mod1b_ancprev_30y_age = mod1b_ancprev_30y_age %>% 
  mutate(Year = as.numeric(Year)) %>% 
  mutate(HIV_prev_ANC_adju_15_19_Mean = `Adjusted_ANC_prevalence,_15-19_Mean`) %>% 
  mutate(HIV_prev_ANC_adju_15_19_LL = `Adjusted_ANC_prevalence,_15-19_95%LL`) %>% 
  mutate(HIV_prev_ANC_adju_15_19_UL = `Adjusted_ANC_prevalence,_15-19_95%UL`) %>% 
  mutate(HIV_prev_ANC_adju_20_24_Mean = `Adjusted_ANC_prevalence,_20-24_Mean`) %>% 
  mutate(HIV_prev_ANC_adju_20_24_LL = `Adjusted_ANC_prevalence,_20-24_95%LL`) %>% 
  mutate(HIV_prev_ANC_adju_20_24_UL = `Adjusted_ANC_prevalence,_20-24_95%UL`) %>% 
  mutate(HIV_prev_ANC_adju_25_29_Mean = `Adjusted_ANC_prevalence,_25-29_Mean`) %>% 
  mutate(HIV_prev_ANC_adju_25_29_LL = `Adjusted_ANC_prevalence,_25-29_95%LL`) %>% 
  mutate(HIV_prev_ANC_adju_25_29_UL = `Adjusted_ANC_prevalence,_25-29_95%UL`) %>% 
  mutate(HIV_prev_ANC_adju_30_34_Mean = `Adjusted_ANC_prevalence,_30-34_Mean`) %>% 
  mutate(HIV_prev_ANC_adju_30_34_LL = `Adjusted_ANC_prevalence,_30-34_95%LL`) %>% 
  mutate(HIV_prev_ANC_adju_30_34_UL = `Adjusted_ANC_prevalence,_30-34_95%UL`) %>% 
  mutate(HIV_prev_ANC_adju_35_39_Mean = `Adjusted_ANC_prevalence,_35-39_Mean`) %>% 
  mutate(HIV_prev_ANC_adju_35_39_LL = `Adjusted_ANC_prevalence,_35-39_95%LL`) %>% 
  mutate(HIV_prev_ANC_adju_35_39_UL = `Adjusted_ANC_prevalence,_35-39_95%UL`) 

mod1b_ancprev_30y_age = lapply(mod1b_ancprev_30y_age, function(x) as.numeric(as.character(x))) %>% 
  as_tibble()

mod1b_30y_age = mod1b_30y_age %>% 
  left_join(mod1b_ancprev_30y_age)

### Model 1b duration: (constant transmission, constant age, increasing SW duration)

mod1b_30y_duration = read.csv("../data/mod1b_30yearART2019_duration.csv", header=FALSE, sep=";") 
mod1b_30y_duration$V2[1] = "Mean"
mod1b_30y_duration = mod1b_30y_duration %>% 
  filter(V2 != "") %>% 
  mutate(V1 = ifelse(V1 =="", NA_character_, V1)) %>% 
  mutate(Indicator = V1) %>% 
  mutate(Indicator = if_else(is.na(Indicator), lag(V1), Indicator)) %>% 
  mutate(Indicator = if_else(is.na(Indicator), lag(Indicator), Indicator)) %>%
  mutate(Indicator = gsub(" ", "_", Indicator)) %>% 
  mutate(V2 = gsub(" ", "", V2)) %>% 
  mutate(Indicator = paste(Indicator, V2, sep = "_")) %>% 
  select(-V1, -V2) 
mod1b_30y_duration$Indicator[1] = "Year" 

mod1b_30y_duration = mod1b_30y_duration[, -c(72:86)]
rownames_temp = mod1b_30y_duration$Indicator
tt = mod1b_30y_duration %>% select(-Indicator) 
tt_trans = t(as.matrix(tt))
rownames(tt_trans) = NULL
mod1b_30y_duration = tt_trans %>% as.tibble()
colnames(mod1b_30y_duration) = rownames_temp

mod1b_30y_duration = mod1b_30y_duration %>% 
  mutate(Year = as.numeric(Year)) %>% 
  mutate(HIV_incidence_FSW_Mean = HIV_incidence_in_female_sex_workers_Mean) %>% 
  mutate(HIV_incidence_FSW_LL = `HIV_incidence_in_female_sex_workers_95%LL`) %>% 
  mutate(HIV_incidence_FSW_UL = `HIV_incidence_in_female_sex_workers_95%UL`) %>% 
  mutate(HIV_prevalence_FSW_Mean = HIV_prevalence_in_females_sex_workers_Mean) %>% 
  mutate(HIV_prevalence_FSW_LL = `HIV_prevalence_in_females_sex_workers_95%LL`) %>% 
  mutate(HIV_prevalence_FSW_UL = `HIV_prevalence_in_females_sex_workers_95%UL`) %>% 
  mutate(ART_coverage_FSW_Mean = ART_coverage_in_female_sex_workers_Mean) %>% 
  mutate(ART_coverage_FSW_LL = `ART_coverage_in_female_sex_workers_95%LL`) %>% 
  mutate(ART_coverage_FSW_UL = `ART_coverage_in_female_sex_workers_95%UL`) %>% 
  mutate(HIV_prev_ANC_15_19_Mean = `HIV_prevalence_in_pregnant_women,_15-19_Mean`) %>% 
  mutate(HIV_prev_ANC_15_19_LL = `HIV_prevalence_in_pregnant_women,_15-19_95%LL`) %>% 
  mutate(HIV_prev_ANC_15_19_UL = `HIV_prevalence_in_pregnant_women,_15-19_95%UL`) %>% 
  mutate(HIV_prev_ANC_20_24_Mean = `HIV_prevalence_in_pregnant_women,_20-24_Mean`) %>% 
  mutate(HIV_prev_ANC_20_24_LL = `HIV_prevalence_in_pregnant_women,_20-24_95%LL`) %>% 
  mutate(HIV_prev_ANC_20_24_UL = `HIV_prevalence_in_pregnant_women,_20-24_95%UL`) %>% 
  mutate(HIV_prev_ANC_25_29_Mean = `HIV_prevalence_in_pregnant_women,_25-29_Mean`) %>% 
  mutate(HIV_prev_ANC_25_29_LL = `HIV_prevalence_in_pregnant_women,_25-29_95%LL`) %>% 
  mutate(HIV_prev_ANC_25_29_UL = `HIV_prevalence_in_pregnant_women,_25-29_95%UL`) %>% 
  mutate(HIV_prev_ANC_30_34_Mean = `HIV_prevalence_in_pregnant_women,_30-34_Mean`) %>% 
  mutate(HIV_prev_ANC_30_34_LL = `HIV_prevalence_in_pregnant_women,_30-34_95%LL`) %>% 
  mutate(HIV_prev_ANC_30_34_UL = `HIV_prevalence_in_pregnant_women,_30-34_95%UL`) %>% 
  mutate(HIV_prev_ANC_35_39_Mean = `HIV_prevalence_in_pregnant_women,_35-39_Mean`) %>% 
  mutate(HIV_prev_ANC_35_39_LL = `HIV_prevalence_in_pregnant_women,_35-39_95%LL`) %>% 
  mutate(HIV_prev_ANC_35_39_UL = `HIV_prevalence_in_pregnant_women,_35-39_95%UL`) %>% 
  mutate(VL_supp_amongART_LL = `Fraction_of_FSWs_on_ART_who_are_virologically_suppressed_(RNA_count_<1000_copies/ml)_95%LL`) %>% 
  mutate(VL_supp_amongART_UL = `Fraction_of_FSWs_on_ART_who_are_virologically_suppressed_(RNA_count_<1000_copies/ml)_95%UL`) %>% 
  mutate(VL_supp_amongART_Mean = `Fraction_of_FSWs_on_ART_who_are_virologically_suppressed_(RNA_count_<1000_copies/ml)_Mean`)

mod1b_30y_duration = lapply(mod1b_30y_duration, function(x) as.numeric(as.character(x))) %>% 
  as_tibble()

mod1b_30y_duration = mod1b_30y_duration %>% 
  mutate(FSW_assumption = "b[duration] - constant FSW age, changing SW duration") %>% 
  mutate(Transmission_assumption = "1 - constant transmission probability") %>% 
  mutate(model = "model 1b[duration]")

mod1b_ancprev_30y_duration = read.csv2("../data/mod1b_ancprev_30yearART2019_duration.csv", header=FALSE) 
mod1b_ancprev_30y_duration$V2[1] = "Mean"

mod1b_ancprev_30y_duration = mod1b_ancprev_30y_duration %>% 
  filter(V2 != "") %>% 
  mutate(V1 = ifelse(V1 =="", NA_character_, V1)) %>% 
  mutate(Indicator = V1) %>% 
  mutate(Indicator = if_else(is.na(Indicator), lag(V1), Indicator)) %>% 
  mutate(Indicator = if_else(is.na(Indicator), lag(Indicator), Indicator)) %>%
  mutate(Indicator = gsub(" ", "_", Indicator)) %>% 
  mutate(V2 = gsub(" ", "", V2)) %>% 
  mutate(Indicator = paste(Indicator, V2, sep = "_")) %>% 
  select(-V1, -V2) 
mod1b_ancprev_30y_duration$Indicator[1] = "Year" 

mod1b_ancprev_30y_duration = mod1b_ancprev_30y_duration %>% 
  filter(grepl("Adjusted_ANC", Indicator)| Indicator == "Year")

rownames_temp_anc = mod1b_ancprev_30y_duration$Indicator
tt = mod1b_ancprev_30y_duration %>% select(-Indicator) 
tt_trans = t(as.matrix(tt))
rownames(tt_trans) = NULL
mod1b_ancprev_30y_duration = tt_trans %>% as.tibble()
colnames(mod1b_ancprev_30y_duration) = rownames_temp_anc

mod1b_ancprev_30y_duration = mod1b_ancprev_30y_duration %>% 
  mutate(Year = as.numeric(Year)) %>% 
  mutate(HIV_prev_ANC_adju_15_19_Mean = `Adjusted_ANC_prevalence,_15-19_Mean`) %>% 
  mutate(HIV_prev_ANC_adju_15_19_LL = `Adjusted_ANC_prevalence,_15-19_95%LL`) %>% 
  mutate(HIV_prev_ANC_adju_15_19_UL = `Adjusted_ANC_prevalence,_15-19_95%UL`) %>% 
  mutate(HIV_prev_ANC_adju_20_24_Mean = `Adjusted_ANC_prevalence,_20-24_Mean`) %>% 
  mutate(HIV_prev_ANC_adju_20_24_LL = `Adjusted_ANC_prevalence,_20-24_95%LL`) %>% 
  mutate(HIV_prev_ANC_adju_20_24_UL = `Adjusted_ANC_prevalence,_20-24_95%UL`) %>% 
  mutate(HIV_prev_ANC_adju_25_29_Mean = `Adjusted_ANC_prevalence,_25-29_Mean`) %>% 
  mutate(HIV_prev_ANC_adju_25_29_LL = `Adjusted_ANC_prevalence,_25-29_95%LL`) %>% 
  mutate(HIV_prev_ANC_adju_25_29_UL = `Adjusted_ANC_prevalence,_25-29_95%UL`) %>% 
  mutate(HIV_prev_ANC_adju_30_34_Mean = `Adjusted_ANC_prevalence,_30-34_Mean`) %>% 
  mutate(HIV_prev_ANC_adju_30_34_LL = `Adjusted_ANC_prevalence,_30-34_95%LL`) %>% 
  mutate(HIV_prev_ANC_adju_30_34_UL = `Adjusted_ANC_prevalence,_30-34_95%UL`) %>% 
  mutate(HIV_prev_ANC_adju_35_39_Mean = `Adjusted_ANC_prevalence,_35-39_Mean`) %>% 
  mutate(HIV_prev_ANC_adju_35_39_LL = `Adjusted_ANC_prevalence,_35-39_95%LL`) %>% 
  mutate(HIV_prev_ANC_adju_35_39_UL = `Adjusted_ANC_prevalence,_35-39_95%UL`) 

mod1b_ancprev_30y_duration = lapply(mod1b_ancprev_30y_duration, function(x) as.numeric(as.character(x))) %>% 
  as_tibble()

mod1b_30y_duration = mod1b_30y_duration %>% 
  left_join(mod1b_ancprev_30y_duration)

## Model 2b age:  (exponentially declining transmission, increasing age, constant SW duration) 

mod2b_30y_age = read.csv("../data/mod2b_30yearART2019_age.csv", header=FALSE, sep=";") 
mod2b_30y_age$V2[1] = "Mean"
mod2b_30y_age = mod2b_30y_age %>% 
  filter(V2 != "") %>% 
  mutate(V1 = ifelse(V1 =="", NA_character_, V1)) %>% 
  mutate(Indicator = V1) %>% 
  mutate(Indicator = if_else(is.na(Indicator), lag(V1), Indicator)) %>% 
  mutate(Indicator = if_else(is.na(Indicator), lag(Indicator), Indicator)) %>%
  mutate(Indicator = gsub(" ", "_", Indicator)) %>% 
  mutate(V2 = gsub(" ", "", V2)) %>% 
  mutate(Indicator = paste(Indicator, V2, sep = "_")) %>% 
  select(-V1, -V2) 
mod2b_30y_age$Indicator[1] = "Year" 

mod2b_30y_age = mod2b_30y_age[, -c(72:86)]
rownames_temp = mod2b_30y_age$Indicator
tt = mod2b_30y_age %>% select(-Indicator) 
tt_trans = t(as.matrix(tt))
rownames(tt_trans) = NULL
mod2b_30y_age = tt_trans %>% as.tibble()
colnames(mod2b_30y_age) = rownames_temp

mod2b_30y_age = mod2b_30y_age %>% 
  mutate(Year = as.numeric(Year)) %>% 
  mutate(HIV_incidence_FSW_Mean = HIV_incidence_in_female_sex_workers_Mean) %>% 
  mutate(HIV_incidence_FSW_LL = `HIV_incidence_in_female_sex_workers_95%LL`) %>% 
  mutate(HIV_incidence_FSW_UL = `HIV_incidence_in_female_sex_workers_95%UL`) %>% 
  mutate(HIV_prevalence_FSW_Mean = HIV_prevalence_in_females_sex_workers_Mean) %>% 
  mutate(HIV_prevalence_FSW_LL = `HIV_prevalence_in_females_sex_workers_95%LL`) %>% 
  mutate(HIV_prevalence_FSW_UL = `HIV_prevalence_in_females_sex_workers_95%UL`) %>% 
  mutate(ART_coverage_FSW_Mean = ART_coverage_in_female_sex_workers_Mean) %>% 
  mutate(ART_coverage_FSW_LL = `ART_coverage_in_female_sex_workers_95%LL`) %>% 
  mutate(ART_coverage_FSW_UL = `ART_coverage_in_female_sex_workers_95%UL`) %>% 
  mutate(HIV_prev_ANC_15_19_Mean = `HIV_prevalence_in_pregnant_women,_15-19_Mean`) %>% 
  mutate(HIV_prev_ANC_15_19_LL = `HIV_prevalence_in_pregnant_women,_15-19_95%LL`) %>% 
  mutate(HIV_prev_ANC_15_19_UL = `HIV_prevalence_in_pregnant_women,_15-19_95%UL`) %>% 
  mutate(HIV_prev_ANC_20_24_Mean = `HIV_prevalence_in_pregnant_women,_20-24_Mean`) %>% 
  mutate(HIV_prev_ANC_20_24_LL = `HIV_prevalence_in_pregnant_women,_20-24_95%LL`) %>% 
  mutate(HIV_prev_ANC_20_24_UL = `HIV_prevalence_in_pregnant_women,_20-24_95%UL`) %>% 
  mutate(HIV_prev_ANC_25_29_Mean = `HIV_prevalence_in_pregnant_women,_25-29_Mean`) %>% 
  mutate(HIV_prev_ANC_25_29_LL = `HIV_prevalence_in_pregnant_women,_25-29_95%LL`) %>% 
  mutate(HIV_prev_ANC_25_29_UL = `HIV_prevalence_in_pregnant_women,_25-29_95%UL`) %>% 
  mutate(HIV_prev_ANC_30_34_Mean = `HIV_prevalence_in_pregnant_women,_30-34_Mean`) %>% 
  mutate(HIV_prev_ANC_30_34_LL = `HIV_prevalence_in_pregnant_women,_30-34_95%LL`) %>% 
  mutate(HIV_prev_ANC_30_34_UL = `HIV_prevalence_in_pregnant_women,_30-34_95%UL`) %>% 
  mutate(HIV_prev_ANC_35_39_Mean = `HIV_prevalence_in_pregnant_women,_35-39_Mean`) %>% 
  mutate(HIV_prev_ANC_35_39_LL = `HIV_prevalence_in_pregnant_women,_35-39_95%LL`) %>% 
  mutate(HIV_prev_ANC_35_39_UL = `HIV_prevalence_in_pregnant_women,_35-39_95%UL`) %>% 
  mutate(VL_supp_amongART_LL = `Fraction_of_FSWs_on_ART_who_are_virologically_suppressed_(RNA_count_<1000_copies/ml)_95%LL`) %>% 
  mutate(VL_supp_amongART_UL = `Fraction_of_FSWs_on_ART_who_are_virologically_suppressed_(RNA_count_<1000_copies/ml)_95%UL`) %>% 
  mutate(VL_supp_amongART_Mean = `Fraction_of_FSWs_on_ART_who_are_virologically_suppressed_(RNA_count_<1000_copies/ml)_Mean`)

mod2b_30y_age = lapply(mod2b_30y_age, function(x) as.numeric(as.character(x))) %>% 
  as_tibble()

mod2b_30y_age = mod2b_30y_age %>% 
  mutate(FSW_assumption = "b[age] - changing FSW age, constant SW duration") %>% 
  mutate(Transmission_assumption = "2 - exponentially declining transmission probability") %>% 
  mutate(model = "model 2b[age]")

mod2b_ancprev_30y_age = read.csv2("../data/mod2b_ancprev_30yearART2019_age.csv", header=FALSE) 
mod2b_ancprev_30y_age$V2[1] = "Mean"

mod2b_ancprev_30y_age = mod2b_ancprev_30y_age %>% 
  filter(V2 != "") %>% 
  mutate(V1 = ifelse(V1 =="", NA_character_, V1)) %>% 
  mutate(Indicator = V1) %>% 
  mutate(Indicator = if_else(is.na(Indicator), lag(V1), Indicator)) %>% 
  mutate(Indicator = if_else(is.na(Indicator), lag(Indicator), Indicator)) %>%
  mutate(Indicator = gsub(" ", "_", Indicator)) %>% 
  mutate(V2 = gsub(" ", "", V2)) %>% 
  mutate(Indicator = paste(Indicator, V2, sep = "_")) %>% 
  select(-V1, -V2) 
mod2b_ancprev_30y_age$Indicator[1] = "Year" 

mod2b_ancprev_30y_age = mod2b_ancprev_30y_age %>% 
  filter(grepl("Adjusted_ANC", Indicator)| Indicator == "Year")

rownames_temp_anc = mod2b_ancprev_30y_age$Indicator
tt = mod2b_ancprev_30y_age %>% select(-Indicator) 
tt_trans = t(as.matrix(tt))
rownames(tt_trans) = NULL
mod2b_ancprev_30y_age = tt_trans %>% as.tibble()
colnames(mod2b_ancprev_30y_age) = rownames_temp_anc

mod2b_ancprev_30y_age = mod2b_ancprev_30y_age %>% 
  mutate(Year = as.numeric(Year)) %>% 
  mutate(HIV_prev_ANC_adju_15_19_Mean = `Adjusted_ANC_prevalence,_15-19_Mean`) %>% 
  mutate(HIV_prev_ANC_adju_15_19_LL = `Adjusted_ANC_prevalence,_15-19_95%LL`) %>% 
  mutate(HIV_prev_ANC_adju_15_19_UL = `Adjusted_ANC_prevalence,_15-19_95%UL`) %>% 
  mutate(HIV_prev_ANC_adju_20_24_Mean = `Adjusted_ANC_prevalence,_20-24_Mean`) %>% 
  mutate(HIV_prev_ANC_adju_20_24_LL = `Adjusted_ANC_prevalence,_20-24_95%LL`) %>% 
  mutate(HIV_prev_ANC_adju_20_24_UL = `Adjusted_ANC_prevalence,_20-24_95%UL`) %>% 
  mutate(HIV_prev_ANC_adju_25_29_Mean = `Adjusted_ANC_prevalence,_25-29_Mean`) %>% 
  mutate(HIV_prev_ANC_adju_25_29_LL = `Adjusted_ANC_prevalence,_25-29_95%LL`) %>% 
  mutate(HIV_prev_ANC_adju_25_29_UL = `Adjusted_ANC_prevalence,_25-29_95%UL`) %>% 
  mutate(HIV_prev_ANC_adju_30_34_Mean = `Adjusted_ANC_prevalence,_30-34_Mean`) %>% 
  mutate(HIV_prev_ANC_adju_30_34_LL = `Adjusted_ANC_prevalence,_30-34_95%LL`) %>% 
  mutate(HIV_prev_ANC_adju_30_34_UL = `Adjusted_ANC_prevalence,_30-34_95%UL`) %>% 
  mutate(HIV_prev_ANC_adju_35_39_Mean = `Adjusted_ANC_prevalence,_35-39_Mean`) %>% 
  mutate(HIV_prev_ANC_adju_35_39_LL = `Adjusted_ANC_prevalence,_35-39_95%LL`) %>% 
  mutate(HIV_prev_ANC_adju_35_39_UL = `Adjusted_ANC_prevalence,_35-39_95%UL`) 

mod2b_ancprev_30y_age = lapply(mod2b_ancprev_30y_age, function(x) as.numeric(as.character(x))) %>% 
  as_tibble()

mod2b_30y_age = mod2b_30y_age %>% 
  left_join(mod2b_ancprev_30y_age)

## Model 2b duration: (exponentially declining transmission, constant age, increasing SW duration) 

mod2b_30y_duration = read.csv("../data/mod2b_30yearART2019_duration.csv", header=FALSE, sep=";") 
mod2b_30y_duration$V2[1] = "Mean"
mod2b_30y_duration = mod2b_30y_duration %>% 
  filter(V2 != "") %>% 
  mutate(V1 = ifelse(V1 =="", NA_character_, V1)) %>% 
  mutate(Indicator = V1) %>% 
  mutate(Indicator = if_else(is.na(Indicator), lag(V1), Indicator)) %>% 
  mutate(Indicator = if_else(is.na(Indicator), lag(Indicator), Indicator)) %>%
  mutate(Indicator = gsub(" ", "_", Indicator)) %>% 
  mutate(V2 = gsub(" ", "", V2)) %>% 
  mutate(Indicator = paste(Indicator, V2, sep = "_")) %>% 
  select(-V1, -V2) 
mod2b_30y_duration$Indicator[1] = "Year" 

mod2b_30y_duration = mod2b_30y_duration[, -c(72:86)]
rownames_temp = mod2b_30y_duration$Indicator
tt = mod2b_30y_duration %>% select(-Indicator) 
tt_trans = t(as.matrix(tt))
rownames(tt_trans) = NULL
mod2b_30y_duration = tt_trans %>% as.tibble()
colnames(mod2b_30y_duration) = rownames_temp

mod2b_30y_duration = mod2b_30y_duration %>% 
  mutate(Year = as.numeric(Year)) %>% 
  mutate(HIV_incidence_FSW_Mean = HIV_incidence_in_female_sex_workers_Mean) %>% 
  mutate(HIV_incidence_FSW_LL = `HIV_incidence_in_female_sex_workers_95%LL`) %>% 
  mutate(HIV_incidence_FSW_UL = `HIV_incidence_in_female_sex_workers_95%UL`) %>% 
  mutate(HIV_prevalence_FSW_Mean = HIV_prevalence_in_females_sex_workers_Mean) %>% 
  mutate(HIV_prevalence_FSW_LL = `HIV_prevalence_in_females_sex_workers_95%LL`) %>% 
  mutate(HIV_prevalence_FSW_UL = `HIV_prevalence_in_females_sex_workers_95%UL`) %>% 
  mutate(ART_coverage_FSW_Mean = ART_coverage_in_female_sex_workers_Mean) %>% 
  mutate(ART_coverage_FSW_LL = `ART_coverage_in_female_sex_workers_95%LL`) %>% 
  mutate(ART_coverage_FSW_UL = `ART_coverage_in_female_sex_workers_95%UL`) %>% 
  mutate(HIV_prev_ANC_15_19_Mean = `HIV_prevalence_in_pregnant_women,_15-19_Mean`) %>% 
  mutate(HIV_prev_ANC_15_19_LL = `HIV_prevalence_in_pregnant_women,_15-19_95%LL`) %>% 
  mutate(HIV_prev_ANC_15_19_UL = `HIV_prevalence_in_pregnant_women,_15-19_95%UL`) %>% 
  mutate(HIV_prev_ANC_20_24_Mean = `HIV_prevalence_in_pregnant_women,_20-24_Mean`) %>% 
  mutate(HIV_prev_ANC_20_24_LL = `HIV_prevalence_in_pregnant_women,_20-24_95%LL`) %>% 
  mutate(HIV_prev_ANC_20_24_UL = `HIV_prevalence_in_pregnant_women,_20-24_95%UL`) %>% 
  mutate(HIV_prev_ANC_25_29_Mean = `HIV_prevalence_in_pregnant_women,_25-29_Mean`) %>% 
  mutate(HIV_prev_ANC_25_29_LL = `HIV_prevalence_in_pregnant_women,_25-29_95%LL`) %>% 
  mutate(HIV_prev_ANC_25_29_UL = `HIV_prevalence_in_pregnant_women,_25-29_95%UL`) %>% 
  mutate(HIV_prev_ANC_30_34_Mean = `HIV_prevalence_in_pregnant_women,_30-34_Mean`) %>% 
  mutate(HIV_prev_ANC_30_34_LL = `HIV_prevalence_in_pregnant_women,_30-34_95%LL`) %>% 
  mutate(HIV_prev_ANC_30_34_UL = `HIV_prevalence_in_pregnant_women,_30-34_95%UL`) %>% 
  mutate(HIV_prev_ANC_35_39_Mean = `HIV_prevalence_in_pregnant_women,_35-39_Mean`) %>% 
  mutate(HIV_prev_ANC_35_39_LL = `HIV_prevalence_in_pregnant_women,_35-39_95%LL`) %>% 
  mutate(HIV_prev_ANC_35_39_UL = `HIV_prevalence_in_pregnant_women,_35-39_95%UL`) %>% 
  mutate(VL_supp_amongART_LL = `Fraction_of_FSWs_on_ART_who_are_virologically_suppressed_(RNA_count_<1000_copies/ml)_95%LL`) %>% 
  mutate(VL_supp_amongART_UL = `Fraction_of_FSWs_on_ART_who_are_virologically_suppressed_(RNA_count_<1000_copies/ml)_95%UL`) %>% 
  mutate(VL_supp_amongART_Mean = `Fraction_of_FSWs_on_ART_who_are_virologically_suppressed_(RNA_count_<1000_copies/ml)_Mean`)

mod2b_30y_duration = lapply(mod2b_30y_duration, function(x) as.numeric(as.character(x))) %>% 
  as_tibble()

mod2b_30y_duration = mod2b_30y_duration %>% 
  mutate(FSW_assumption = "b[duration] - constant FSW age, changing SW duration") %>% 
  mutate(Transmission_assumption = "2 - exponentially declining transmission probability") %>% 
  mutate(model = "model 2b[duration]")

mod2b_ancprev_30y_duration = read.csv2("../data/mod2b_ancprev_30yearART2019_duration.csv", header=FALSE) 
mod2b_ancprev_30y_duration$V2[1] = "Mean"

mod2b_ancprev_30y_duration = mod2b_ancprev_30y_duration %>% 
  filter(V2 != "") %>% 
  mutate(V1 = ifelse(V1 =="", NA_character_, V1)) %>% 
  mutate(Indicator = V1) %>% 
  mutate(Indicator = if_else(is.na(Indicator), lag(V1), Indicator)) %>% 
  mutate(Indicator = if_else(is.na(Indicator), lag(Indicator), Indicator)) %>%
  mutate(Indicator = gsub(" ", "_", Indicator)) %>% 
  mutate(V2 = gsub(" ", "", V2)) %>% 
  mutate(Indicator = paste(Indicator, V2, sep = "_")) %>% 
  select(-V1, -V2) 
mod2b_ancprev_30y_duration$Indicator[1] = "Year" 

mod2b_ancprev_30y_duration = mod2b_ancprev_30y_duration %>% 
  filter(grepl("Adjusted_ANC", Indicator)| Indicator == "Year")

rownames_temp_anc = mod2b_ancprev_30y_duration$Indicator
tt = mod2b_ancprev_30y_duration %>% select(-Indicator) 
tt_trans = t(as.matrix(tt))
rownames(tt_trans) = NULL
mod2b_ancprev_30y_duration = tt_trans %>% as.tibble()
colnames(mod2b_ancprev_30y_duration) = rownames_temp_anc

mod2b_ancprev_30y_duration = mod2b_ancprev_30y_duration %>% 
  mutate(Year = as.numeric(Year)) %>% 
  mutate(HIV_prev_ANC_adju_15_19_Mean = `Adjusted_ANC_prevalence,_15-19_Mean`) %>% 
  mutate(HIV_prev_ANC_adju_15_19_LL = `Adjusted_ANC_prevalence,_15-19_95%LL`) %>% 
  mutate(HIV_prev_ANC_adju_15_19_UL = `Adjusted_ANC_prevalence,_15-19_95%UL`) %>% 
  mutate(HIV_prev_ANC_adju_20_24_Mean = `Adjusted_ANC_prevalence,_20-24_Mean`) %>% 
  mutate(HIV_prev_ANC_adju_20_24_LL = `Adjusted_ANC_prevalence,_20-24_95%LL`) %>% 
  mutate(HIV_prev_ANC_adju_20_24_UL = `Adjusted_ANC_prevalence,_20-24_95%UL`) %>% 
  mutate(HIV_prev_ANC_adju_25_29_Mean = `Adjusted_ANC_prevalence,_25-29_Mean`) %>% 
  mutate(HIV_prev_ANC_adju_25_29_LL = `Adjusted_ANC_prevalence,_25-29_95%LL`) %>% 
  mutate(HIV_prev_ANC_adju_25_29_UL = `Adjusted_ANC_prevalence,_25-29_95%UL`) %>% 
  mutate(HIV_prev_ANC_adju_30_34_Mean = `Adjusted_ANC_prevalence,_30-34_Mean`) %>% 
  mutate(HIV_prev_ANC_adju_30_34_LL = `Adjusted_ANC_prevalence,_30-34_95%LL`) %>% 
  mutate(HIV_prev_ANC_adju_30_34_UL = `Adjusted_ANC_prevalence,_30-34_95%UL`) %>% 
  mutate(HIV_prev_ANC_adju_35_39_Mean = `Adjusted_ANC_prevalence,_35-39_Mean`) %>% 
  mutate(HIV_prev_ANC_adju_35_39_LL = `Adjusted_ANC_prevalence,_35-39_95%LL`) %>% 
  mutate(HIV_prev_ANC_adju_35_39_UL = `Adjusted_ANC_prevalence,_35-39_95%UL`) 

mod2b_ancprev_30y_duration = lapply(mod2b_ancprev_30y_duration, function(x) as.numeric(as.character(x))) %>% 
  as_tibble()

mod2b_30y_duration = mod2b_30y_duration %>% 
  left_join(mod2b_ancprev_30y_duration)

## Model 3b age:  (exposure-dependent transmission, increasing age, constant SW duration) 

mod3b_30y_age = read.csv("../data/mod3b_30yearART2019_age.csv", header=FALSE, sep=";") 
mod3b_30y_age$V2[1] = "Mean"
mod3b_30y_age = mod3b_30y_age %>% 
  filter(V2 != "") %>% 
  mutate(V1 = ifelse(V1 =="", NA_character_, V1)) %>% 
  mutate(Indicator = V1) %>% 
  mutate(Indicator = if_else(is.na(Indicator), lag(V1), Indicator)) %>% 
  mutate(Indicator = if_else(is.na(Indicator), lag(Indicator), Indicator)) %>%
  mutate(Indicator = gsub(" ", "_", Indicator)) %>% 
  mutate(V2 = gsub(" ", "", V2)) %>% 
  mutate(Indicator = paste(Indicator, V2, sep = "_")) %>% 
  select(-V1, -V2) 
mod3b_30y_age$Indicator[1] = "Year" 

mod3b_30y_age = mod3b_30y_age[, -c(72:86)]
rownames_temp = mod3b_30y_age$Indicator
tt = mod3b_30y_age %>% select(-Indicator) 
tt_trans = t(as.matrix(tt))
rownames(tt_trans) = NULL
mod3b_30y_age = tt_trans %>% as.tibble()
colnames(mod3b_30y_age) = rownames_temp

mod3b_30y_age = mod3b_30y_age %>% 
  mutate(Year = as.numeric(Year)) %>% 
  mutate(HIV_incidence_FSW_Mean = HIV_incidence_in_female_sex_workers_Mean) %>% 
  mutate(HIV_incidence_FSW_LL = `HIV_incidence_in_female_sex_workers_95%LL`) %>% 
  mutate(HIV_incidence_FSW_UL = `HIV_incidence_in_female_sex_workers_95%UL`) %>% 
  mutate(HIV_prevalence_FSW_Mean = HIV_prevalence_in_females_sex_workers_Mean) %>% 
  mutate(HIV_prevalence_FSW_LL = `HIV_prevalence_in_females_sex_workers_95%LL`) %>% 
  mutate(HIV_prevalence_FSW_UL = `HIV_prevalence_in_females_sex_workers_95%UL`) %>% 
  mutate(ART_coverage_FSW_Mean = ART_coverage_in_female_sex_workers_Mean) %>% 
  mutate(ART_coverage_FSW_LL = `ART_coverage_in_female_sex_workers_95%LL`) %>% 
  mutate(ART_coverage_FSW_UL = `ART_coverage_in_female_sex_workers_95%UL`) %>% 
  mutate(HIV_prev_ANC_15_19_Mean = `HIV_prevalence_in_pregnant_women,_15-19_Mean`) %>% 
  mutate(HIV_prev_ANC_15_19_LL = `HIV_prevalence_in_pregnant_women,_15-19_95%LL`) %>% 
  mutate(HIV_prev_ANC_15_19_UL = `HIV_prevalence_in_pregnant_women,_15-19_95%UL`) %>% 
  mutate(HIV_prev_ANC_20_24_Mean = `HIV_prevalence_in_pregnant_women,_20-24_Mean`) %>% 
  mutate(HIV_prev_ANC_20_24_LL = `HIV_prevalence_in_pregnant_women,_20-24_95%LL`) %>% 
  mutate(HIV_prev_ANC_20_24_UL = `HIV_prevalence_in_pregnant_women,_20-24_95%UL`) %>% 
  mutate(HIV_prev_ANC_25_29_Mean = `HIV_prevalence_in_pregnant_women,_25-29_Mean`) %>% 
  mutate(HIV_prev_ANC_25_29_LL = `HIV_prevalence_in_pregnant_women,_25-29_95%LL`) %>% 
  mutate(HIV_prev_ANC_25_29_UL = `HIV_prevalence_in_pregnant_women,_25-29_95%UL`) %>% 
  mutate(HIV_prev_ANC_30_34_Mean = `HIV_prevalence_in_pregnant_women,_30-34_Mean`) %>% 
  mutate(HIV_prev_ANC_30_34_LL = `HIV_prevalence_in_pregnant_women,_30-34_95%LL`) %>% 
  mutate(HIV_prev_ANC_30_34_UL = `HIV_prevalence_in_pregnant_women,_30-34_95%UL`) %>% 
  mutate(HIV_prev_ANC_35_39_Mean = `HIV_prevalence_in_pregnant_women,_35-39_Mean`) %>% 
  mutate(HIV_prev_ANC_35_39_LL = `HIV_prevalence_in_pregnant_women,_35-39_95%LL`) %>% 
  mutate(HIV_prev_ANC_35_39_UL = `HIV_prevalence_in_pregnant_women,_35-39_95%UL`) %>% 
  mutate(VL_supp_amongART_LL = `Fraction_of_FSWs_on_ART_who_are_virologically_suppressed_(RNA_count_<1000_copies/ml)_95%LL`) %>% 
  mutate(VL_supp_amongART_UL = `Fraction_of_FSWs_on_ART_who_are_virologically_suppressed_(RNA_count_<1000_copies/ml)_95%UL`) %>% 
  mutate(VL_supp_amongART_Mean = `Fraction_of_FSWs_on_ART_who_are_virologically_suppressed_(RNA_count_<1000_copies/ml)_Mean`)

mod3b_30y_age = lapply(mod3b_30y_age, function(x) as.numeric(as.character(x))) %>% 
  as_tibble()

mod3b_30y_age = mod3b_30y_age %>% 
  mutate(FSW_assumption = "b[age] - changing FSW age, constant SW duration") %>% 
  mutate(Transmission_assumption = "3 - dynamically changing transmission probability") %>% 
  mutate(model = "model 3b[age]")

mod3b_ancprev_30y_age = read.csv2("../data/mod3b_ancprev_30yearART2019_age.csv", header=FALSE) 
mod3b_ancprev_30y_age$V2[1] = "Mean"

mod3b_ancprev_30y_age = mod3b_ancprev_30y_age %>% 
  filter(V2 != "") %>% 
  mutate(V1 = ifelse(V1 =="", NA_character_, V1)) %>% 
  mutate(Indicator = V1) %>% 
  mutate(Indicator = if_else(is.na(Indicator), lag(V1), Indicator)) %>% 
  mutate(Indicator = if_else(is.na(Indicator), lag(Indicator), Indicator)) %>%
  mutate(Indicator = gsub(" ", "_", Indicator)) %>% 
  mutate(V2 = gsub(" ", "", V2)) %>% 
  mutate(Indicator = paste(Indicator, V2, sep = "_")) %>% 
  select(-V1, -V2) 
mod3b_ancprev_30y_age$Indicator[1] = "Year" 

mod3b_ancprev_30y_age = mod3b_ancprev_30y_age %>% 
  filter(grepl("Adjusted_ANC", Indicator)| Indicator == "Year")

rownames_temp_anc = mod3b_ancprev_30y_age$Indicator
tt = mod3b_ancprev_30y_age %>% select(-Indicator) 
tt_trans = t(as.matrix(tt))
rownames(tt_trans) = NULL
mod3b_ancprev_30y_age = tt_trans %>% as.tibble()
colnames(mod3b_ancprev_30y_age) = rownames_temp_anc

mod3b_ancprev_30y_age = mod3b_ancprev_30y_age %>% 
  mutate(Year = as.numeric(Year)) %>% 
  mutate(HIV_prev_ANC_adju_15_19_Mean = `Adjusted_ANC_prevalence,_15-19_Mean`) %>% 
  mutate(HIV_prev_ANC_adju_15_19_LL = `Adjusted_ANC_prevalence,_15-19_95%LL`) %>% 
  mutate(HIV_prev_ANC_adju_15_19_UL = `Adjusted_ANC_prevalence,_15-19_95%UL`) %>% 
  mutate(HIV_prev_ANC_adju_20_24_Mean = `Adjusted_ANC_prevalence,_20-24_Mean`) %>% 
  mutate(HIV_prev_ANC_adju_20_24_LL = `Adjusted_ANC_prevalence,_20-24_95%LL`) %>% 
  mutate(HIV_prev_ANC_adju_20_24_UL = `Adjusted_ANC_prevalence,_20-24_95%UL`) %>% 
  mutate(HIV_prev_ANC_adju_25_29_Mean = `Adjusted_ANC_prevalence,_25-29_Mean`) %>% 
  mutate(HIV_prev_ANC_adju_25_29_LL = `Adjusted_ANC_prevalence,_25-29_95%LL`) %>% 
  mutate(HIV_prev_ANC_adju_25_29_UL = `Adjusted_ANC_prevalence,_25-29_95%UL`) %>% 
  mutate(HIV_prev_ANC_adju_30_34_Mean = `Adjusted_ANC_prevalence,_30-34_Mean`) %>% 
  mutate(HIV_prev_ANC_adju_30_34_LL = `Adjusted_ANC_prevalence,_30-34_95%LL`) %>% 
  mutate(HIV_prev_ANC_adju_30_34_UL = `Adjusted_ANC_prevalence,_30-34_95%UL`) %>% 
  mutate(HIV_prev_ANC_adju_35_39_Mean = `Adjusted_ANC_prevalence,_35-39_Mean`) %>% 
  mutate(HIV_prev_ANC_adju_35_39_LL = `Adjusted_ANC_prevalence,_35-39_95%LL`) %>% 
  mutate(HIV_prev_ANC_adju_35_39_UL = `Adjusted_ANC_prevalence,_35-39_95%UL`) 

mod3b_ancprev_30y_age = lapply(mod3b_ancprev_30y_age, function(x) as.numeric(as.character(x))) %>% 
  as_tibble()

mod3b_30y_age = mod3b_30y_age %>% 
  left_join(mod3b_ancprev_30y_age)

## Model 3b duration:  (exposure-dependent transmission, constant age, increasing SW duration) 

mod3b_30y_duration = read.csv("../data/mod3b_30yearART2019_duration.csv", header=FALSE, sep=";") 
mod3b_30y_duration$V2[1] = "Mean"
mod3b_30y_duration = mod3b_30y_duration %>% 
  filter(V2 != "") %>% 
  mutate(V1 = ifelse(V1 =="", NA_character_, V1)) %>% 
  mutate(Indicator = V1) %>% 
  mutate(Indicator = if_else(is.na(Indicator), lag(V1), Indicator)) %>% 
  mutate(Indicator = if_else(is.na(Indicator), lag(Indicator), Indicator)) %>%
  mutate(Indicator = gsub(" ", "_", Indicator)) %>% 
  mutate(V2 = gsub(" ", "", V2)) %>% 
  mutate(Indicator = paste(Indicator, V2, sep = "_")) %>% 
  select(-V1, -V2) 
mod3b_30y_duration$Indicator[1] = "Year" 

mod3b_30y_duration = mod3b_30y_duration[, -c(72:86)]
rownames_temp = mod3b_30y_duration$Indicator
tt = mod3b_30y_duration %>% select(-Indicator) 
tt_trans = t(as.matrix(tt))
rownames(tt_trans) = NULL
mod3b_30y_duration = tt_trans %>% as.tibble()
colnames(mod3b_30y_duration) = rownames_temp

mod3b_30y_duration = mod3b_30y_duration %>% 
  mutate(Year = as.numeric(Year)) %>% 
  mutate(HIV_incidence_FSW_Mean = HIV_incidence_in_female_sex_workers_Mean) %>% 
  mutate(HIV_incidence_FSW_LL = `HIV_incidence_in_female_sex_workers_95%LL`) %>% 
  mutate(HIV_incidence_FSW_UL = `HIV_incidence_in_female_sex_workers_95%UL`) %>% 
  mutate(HIV_prevalence_FSW_Mean = HIV_prevalence_in_females_sex_workers_Mean) %>% 
  mutate(HIV_prevalence_FSW_LL = `HIV_prevalence_in_females_sex_workers_95%LL`) %>% 
  mutate(HIV_prevalence_FSW_UL = `HIV_prevalence_in_females_sex_workers_95%UL`) %>% 
  mutate(ART_coverage_FSW_Mean = ART_coverage_in_female_sex_workers_Mean) %>% 
  mutate(ART_coverage_FSW_LL = `ART_coverage_in_female_sex_workers_95%LL`) %>% 
  mutate(ART_coverage_FSW_UL = `ART_coverage_in_female_sex_workers_95%UL`) %>% 
  mutate(HIV_prev_ANC_15_19_Mean = `HIV_prevalence_in_pregnant_women,_15-19_Mean`) %>% 
  mutate(HIV_prev_ANC_15_19_LL = `HIV_prevalence_in_pregnant_women,_15-19_95%LL`) %>% 
  mutate(HIV_prev_ANC_15_19_UL = `HIV_prevalence_in_pregnant_women,_15-19_95%UL`) %>% 
  mutate(HIV_prev_ANC_20_24_Mean = `HIV_prevalence_in_pregnant_women,_20-24_Mean`) %>% 
  mutate(HIV_prev_ANC_20_24_LL = `HIV_prevalence_in_pregnant_women,_20-24_95%LL`) %>% 
  mutate(HIV_prev_ANC_20_24_UL = `HIV_prevalence_in_pregnant_women,_20-24_95%UL`) %>% 
  mutate(HIV_prev_ANC_25_29_Mean = `HIV_prevalence_in_pregnant_women,_25-29_Mean`) %>% 
  mutate(HIV_prev_ANC_25_29_LL = `HIV_prevalence_in_pregnant_women,_25-29_95%LL`) %>% 
  mutate(HIV_prev_ANC_25_29_UL = `HIV_prevalence_in_pregnant_women,_25-29_95%UL`) %>% 
  mutate(HIV_prev_ANC_30_34_Mean = `HIV_prevalence_in_pregnant_women,_30-34_Mean`) %>% 
  mutate(HIV_prev_ANC_30_34_LL = `HIV_prevalence_in_pregnant_women,_30-34_95%LL`) %>% 
  mutate(HIV_prev_ANC_30_34_UL = `HIV_prevalence_in_pregnant_women,_30-34_95%UL`) %>% 
  mutate(HIV_prev_ANC_35_39_Mean = `HIV_prevalence_in_pregnant_women,_35-39_Mean`) %>% 
  mutate(HIV_prev_ANC_35_39_LL = `HIV_prevalence_in_pregnant_women,_35-39_95%LL`) %>% 
  mutate(HIV_prev_ANC_35_39_UL = `HIV_prevalence_in_pregnant_women,_35-39_95%UL`) %>% 
  mutate(VL_supp_amongART_LL = `Fraction_of_FSWs_on_ART_who_are_virologically_suppressed_(RNA_count_<1000_copies/ml)_95%LL`) %>% 
  mutate(VL_supp_amongART_UL = `Fraction_of_FSWs_on_ART_who_are_virologically_suppressed_(RNA_count_<1000_copies/ml)_95%UL`) %>% 
  mutate(VL_supp_amongART_Mean = `Fraction_of_FSWs_on_ART_who_are_virologically_suppressed_(RNA_count_<1000_copies/ml)_Mean`)

mod3b_30y_duration = lapply(mod3b_30y_duration, function(x) as.numeric(as.character(x))) %>% 
  as_tibble()

mod3b_30y_duration = mod3b_30y_duration %>% 
  mutate(FSW_assumption = "b[duration] - constant FSW age, changing SW duration") %>% 
  mutate(Transmission_assumption = "3 - dynamically changing transmission probability") %>% 
  mutate(model = "model 3b[duration]")

mod3b_ancprev_30y_duration = read.csv2("../data/mod3b_ancprev_30yearART2019_duration.csv", header=FALSE) 
mod3b_ancprev_30y_duration$V2[1] = "Mean"

mod3b_ancprev_30y_duration = mod3b_ancprev_30y_duration %>% 
  filter(V2 != "") %>% 
  mutate(V1 = ifelse(V1 =="", NA_character_, V1)) %>% 
  mutate(Indicator = V1) %>% 
  mutate(Indicator = if_else(is.na(Indicator), lag(V1), Indicator)) %>% 
  mutate(Indicator = if_else(is.na(Indicator), lag(Indicator), Indicator)) %>%
  mutate(Indicator = gsub(" ", "_", Indicator)) %>% 
  mutate(V2 = gsub(" ", "", V2)) %>% 
  mutate(Indicator = paste(Indicator, V2, sep = "_")) %>% 
  select(-V1, -V2) 
mod3b_ancprev_30y_duration$Indicator[1] = "Year" 

mod3b_ancprev_30y_duration = mod3b_ancprev_30y_duration %>% 
  filter(grepl("Adjusted_ANC", Indicator)| Indicator == "Year")

rownames_temp_anc = mod3b_ancprev_30y_duration$Indicator
tt = mod3b_ancprev_30y_duration %>% select(-Indicator) 
tt_trans = t(as.matrix(tt))
rownames(tt_trans) = NULL
mod3b_ancprev_30y_duration = tt_trans %>% as.tibble()
colnames(mod3b_ancprev_30y_duration) = rownames_temp_anc

mod3b_ancprev_30y_duration = mod3b_ancprev_30y_duration %>% 
  mutate(Year = as.numeric(Year)) %>% 
  mutate(HIV_prev_ANC_adju_15_19_Mean = `Adjusted_ANC_prevalence,_15-19_Mean`) %>% 
  mutate(HIV_prev_ANC_adju_15_19_LL = `Adjusted_ANC_prevalence,_15-19_95%LL`) %>% 
  mutate(HIV_prev_ANC_adju_15_19_UL = `Adjusted_ANC_prevalence,_15-19_95%UL`) %>% 
  mutate(HIV_prev_ANC_adju_20_24_Mean = `Adjusted_ANC_prevalence,_20-24_Mean`) %>% 
  mutate(HIV_prev_ANC_adju_20_24_LL = `Adjusted_ANC_prevalence,_20-24_95%LL`) %>% 
  mutate(HIV_prev_ANC_adju_20_24_UL = `Adjusted_ANC_prevalence,_20-24_95%UL`) %>% 
  mutate(HIV_prev_ANC_adju_25_29_Mean = `Adjusted_ANC_prevalence,_25-29_Mean`) %>% 
  mutate(HIV_prev_ANC_adju_25_29_LL = `Adjusted_ANC_prevalence,_25-29_95%LL`) %>% 
  mutate(HIV_prev_ANC_adju_25_29_UL = `Adjusted_ANC_prevalence,_25-29_95%UL`) %>% 
  mutate(HIV_prev_ANC_adju_30_34_Mean = `Adjusted_ANC_prevalence,_30-34_Mean`) %>% 
  mutate(HIV_prev_ANC_adju_30_34_LL = `Adjusted_ANC_prevalence,_30-34_95%LL`) %>% 
  mutate(HIV_prev_ANC_adju_30_34_UL = `Adjusted_ANC_prevalence,_30-34_95%UL`) %>% 
  mutate(HIV_prev_ANC_adju_35_39_Mean = `Adjusted_ANC_prevalence,_35-39_Mean`) %>% 
  mutate(HIV_prev_ANC_adju_35_39_LL = `Adjusted_ANC_prevalence,_35-39_95%LL`) %>% 
  mutate(HIV_prev_ANC_adju_35_39_UL = `Adjusted_ANC_prevalence,_35-39_95%UL`) 

mod3b_ancprev_30y_duration = lapply(mod3b_ancprev_30y_duration, function(x) as.numeric(as.character(x))) %>% 
  as_tibble()

mod3b_30y_duration = mod3b_30y_duration %>% 
  left_join(mod3b_ancprev_30y_duration)

data_models_30y_sens = mod1a_30y %>% 
  bind_rows(mod1b_30y) %>% 
  bind_rows(mod2a_30y) %>% 
  bind_rows(mod2b_30y) %>% 
  bind_rows(mod3a_30y) %>% 
  bind_rows(mod3b_30y) %>% 
  bind_rows(mod1b_30y_age) %>% 
  bind_rows(mod1b_30y_duration) %>% 
  bind_rows(mod2b_30y_age) %>% 
  bind_rows(mod2b_30y_duration) %>% 
  bind_rows(mod3b_30y_age) %>% 
  bind_rows(mod3b_30y_duration)

### put in one data frame : 
###########################

art_data = data_models_30y %>% select(Year, model, 
                                      y = ART_coverage_FSW_Mean, 
                                      ymin = ART_coverage_FSW_LL, 
                                      ymax = ART_coverage_FSW_UL, 
                                      Transmission_assumption) %>% 
  mutate(type = "ART coverage in FSW")

inc_data = data_models_30y %>% select(Year, model, 
                                      y = HIV_incidence_FSW_Mean, 
                                      ymin = HIV_incidence_FSW_LL, 
                                      ymax = HIV_incidence_FSW_UL, Transmission_assumption) %>% 
  mutate(type = "HIV incidence in FSW")


prev_data = data_models_30y %>% select(Year, model, 
                                       y = HIV_prevalence_FSW_Mean, 
                                       ymin = HIV_prevalence_FSW_LL, 
                                       ymax = HIV_prevalence_FSW_UL, 
                                       Transmission_assumption) %>% 
  mutate(type = "HIV prevalence in FSW")

vl_data_new = data_models_30y %>% select(Year, model, 
                                         y_new = VL_supp_amongART_Mean) 

vl_data_new = vl_data_new %>% left_join(art_data) %>% 
  mutate(y = y *y_new, 
         ymin  = y_new*ymin, 
         ymax = y_new*ymax) %>% 
  mutate(type = "VL suppression in FSW")

data_tot2 = inc_data %>% bind_rows(prev_data) %>% 
  bind_rows(vl_data_new)

### the same for the sensitivity analysis (b separate age and duration )

art_data_sens = data_models_30y_sens %>% select(Year, model, 
                                      y = ART_coverage_FSW_Mean, 
                                      ymin = ART_coverage_FSW_LL, 
                                      ymax = ART_coverage_FSW_UL, 
                                      Transmission_assumption, 
                                      FSW_assumption) %>% 
  mutate(type = "ART coverage in FSW")

inc_data_sens = data_models_30y_sens %>% select(Year, model, 
                                      y = HIV_incidence_FSW_Mean, 
                                      ymin = HIV_incidence_FSW_LL, 
                                      ymax = HIV_incidence_FSW_UL, 
                                      Transmission_assumption, 
                                      FSW_assumption) %>% 
  mutate(type = "HIV incidence in FSW")


prev_data_sens = data_models_30y_sens %>% select(Year, model, 
                                       y = HIV_prevalence_FSW_Mean, 
                                       ymin = HIV_prevalence_FSW_LL, 
                                       ymax = HIV_prevalence_FSW_UL, 
                                       Transmission_assumption, 
                                       FSW_assumption) %>% 
  mutate(type = "HIV prevalence in FSW")

vl_data_new_sens = data_models_30y_sens %>% select(Year, model, 
                                         y_new = VL_supp_amongART_Mean) 

vl_data_new_sens = vl_data_new_sens %>% left_join(art_data_sens) %>% 
  mutate(y = y *y_new, 
         ymin  = y_new*ymin, 
         ymax = y_new*ymax) %>% 
  mutate(type = "VL suppression in FSW")

data_tot2_sens = inc_data_sens %>% bind_rows(prev_data_sens) %>% 
  bind_rows(vl_data_new_sens)

#################################################
## validation data (Reshma and Jaffer studies) ##
#################################################

reshma = data.frame(est = c(0.621, 4.60/100), lb = c(0.603, 1.53/100), ub = c(0.657,8.45/100) , type = c("prevalence", "incidence"), year = 2019)
jaffer = data.frame(est = c(0.619, 1453/1673, 1024/1385), 
                    lb = c(0.619, 1453/1673, 1453/1673), 
                    ub = c(0.619, 1453/1673, 1453/1673), 
                    type = c("prevalence", "ART coverage", "VL supp"), 
                    year = 2019)

data_reshandjaff3 = 
  reshma %>% mutate(shape = "Validation data") %>% 
  bind_rows(jaffer %>% mutate(lb = est, ub = est, shape = "Validation data") %>% 
              filter(!(type %in% c("VL supp", "prevalence")))) %>% 
  mutate(type = paste0(type, " in FSW")) %>% 
  mutate(type = case_when(type == "ART coverage in FSW" ~ type, 
                          TRUE  ~paste0("HIV ", type))) %>% 
  mutate(type = factor(type, levels = c("HIV incidence in FSW", 
                                        "HIV prevalence in FSW", 
                                        "ART coverage in FSW")))

data_reshandjaff4 = data_reshandjaff3 %>% 
  mutate(est = ifelse(type == "ART coverage in FSW",1708/1862*1024/1385*est,est), 
         lb = ifelse(type == "ART coverage in FSW", 1708/1862*1024/1385*lb,lb), 
         ub = ifelse(type == "ART coverage in FSW", 1708/1862*1024/1385*ub,ub)) %>% 
  mutate(type = case_when(type == "ART coverage in FSW" ~ "VL suppression in FSW", 
                          TRUE ~ type))

## calculation of CI for VL suppression with delta method: 

p1=1708/1862
p2=1453/1673
p3=1024/1385

n1 = 1862
n2 = 1673
n3 = 1385

p_vlsupp = p1*p2*p3
var_logpsupp = (1-p1)/(p1*n1) + (1-p2)/(p2*n2) + (1-p3)/(p3*n3)

vlsupp_lb = exp(log(p_vlsupp) - 1.96*sqrt(var_logpsupp))
vlsupp_ub = exp(log(p_vlsupp) + 1.96*sqrt(var_logpsupp))

data_reshandjaff4 = data_reshandjaff4 %>% 
  mutate(lb = case_when(type == "VL suppression in FSW" ~ vlsupp_lb, 
                        TRUE ~ lb), 
         ub = case_when(type == "VL suppression in FSW" ~ vlsupp_ub, 
                        TRUE ~ ub))

####################################################
### IRR age-matched and non-aged matched and PAF ###
####################################################

IRR_mod1a = read_excel("../data/IRR_agematched.xlsx", sheet = 1, col_names =  TRUE) 
IRR_mod1a = apply(IRR_mod1a, 2, function(x) {
  c(IRR_mean = mean(x), 
    IRR_median = median(x), 
    IRR_q025 = quantile(x, 0.025), 
    IRR_q975 = quantile(x, 0.975))
  })

rownames(IRR_mod1a)[c(3,4)] = c("IRR_q025", "IRR_q975")
IRR_mod1a <- as.data.frame(t(IRR_mod1a))

IRR_mod1a$year <- as.numeric(rownames(IRR_mod1a))
IRR_mod1a = IRR_mod1a %>% 
  mutate(FSW_assumption = "a - constant FSW age and SW duration") %>% 
  mutate(Transmission_assumption = "1 - constant transmission probability") %>% 
  mutate(model = "model 1a")


IRR_mod1b = read_excel("../data/IRR_agematched.xlsx", sheet = 2, col_names =  TRUE) 
IRR_mod1b = apply(IRR_mod1b, 2, function(x) {
  c(IRR_mean = mean(x), 
    IRR_median = median(x), 
    IRR_q025 = quantile(x, 0.025), 
    IRR_q975 = quantile(x, 0.975))
})

rownames(IRR_mod1b)[c(3,4)] = c("IRR_q025", "IRR_q975")
IRR_mod1b <- as.data.frame(t(IRR_mod1b))

IRR_mod1b$year <- as.numeric(rownames(IRR_mod1b))
IRR_mod1b = IRR_mod1b %>% 
  mutate(FSW_assumption = "b - changing FSW age and SW duration") %>% 
  mutate(Transmission_assumption = "1 - constant transmission probability") %>% 
  mutate(model = "model 1b")


IRR_mod2a = read_excel("../data/IRR_agematched.xlsx", sheet = 3, col_names =  TRUE) 
IRR_mod2a = apply(IRR_mod2a, 2, function(x) {
  c(IRR_mean = mean(x), 
    IRR_median = median(x), 
    IRR_q025 = quantile(x, 0.025), 
    IRR_q975 = quantile(x, 0.975))
})

rownames(IRR_mod2a)[c(3,4)] = c("IRR_q025", "IRR_q975")
IRR_mod2a <- as.data.frame(t(IRR_mod2a))

IRR_mod2a$year <- as.numeric(rownames(IRR_mod2a))
IRR_mod2a = IRR_mod2a %>% 
  mutate(FSW_assumption = "a - constant FSW age and SW duration") %>% 
  mutate(Transmission_assumption = "2 - exponentially declining transmission probability") %>% 
  mutate(model = "model 2a")

IRR_mod2b = read_excel("../data/IRR_agematched.xlsx", sheet = 4, col_names =  TRUE) 
IRR_mod2b = apply(IRR_mod2b, 2, function(x) {
  c(IRR_mean = mean(x), 
    IRR_median = median(x), 
    IRR_q025 = quantile(x, 0.025), 
    IRR_q975 = quantile(x, 0.975))
})

rownames(IRR_mod2b)[c(3,4)] = c("IRR_q025", "IRR_q975")
IRR_mod2b <- as.data.frame(t(IRR_mod2b))

IRR_mod2b$year <- as.numeric(rownames(IRR_mod2b))
IRR_mod2b = IRR_mod2b %>% 
  mutate(FSW_assumption = "b - changing FSW age and SW duration") %>% 
  mutate(Transmission_assumption = "2 - exponentially declining transmission probability") %>% 
  mutate(model = "model 2b")


IRR_mod3a = read_excel("../data/IRR_agematched.xlsx", sheet = 5, col_names =  TRUE) 
IRR_mod3a = apply(IRR_mod3a, 2, function(x) {
  c(IRR_mean = mean(x), 
    IRR_median = median(x), 
    IRR_q025 = quantile(x, 0.025), 
    IRR_q975 = quantile(x, 0.975))
})

rownames(IRR_mod3a)[c(3,4)] = c("IRR_q025", "IRR_q975")
IRR_mod3a <- as.data.frame(t(IRR_mod3a))

IRR_mod3a$year <- as.numeric(rownames(IRR_mod3a))
IRR_mod3a = IRR_mod3a %>% 
  mutate(FSW_assumption = "a - constant FSW age and SW duration") %>% 
  mutate(Transmission_assumption = "3 - dynamically changing transmission probability") %>% 
  mutate(model = "model 3a")


IRR_mod3b = read_excel("../data/IRR_agematched.xlsx", sheet = 6, col_names =  TRUE) 
IRR_mod3b = apply(IRR_mod3b, 2, function(x) {
  c(IRR_mean = mean(x), 
    IRR_median = median(x), 
    IRR_q025 = quantile(x, 0.025), 
    IRR_q975 = quantile(x, 0.975))
})

rownames(IRR_mod3b)[c(3,4)] = c("IRR_q025", "IRR_q975")
IRR_mod3b <- as.data.frame(t(IRR_mod3b))

IRR_mod3b$year <- as.numeric(rownames(IRR_mod3b))
IRR_mod3b = IRR_mod3b %>% 
  mutate(FSW_assumption = "b - changing FSW age and SW duration") %>% 
  mutate(Transmission_assumption = "3 - dynamically changing transmission probability") %>% 
  mutate(model = "model 3b")


IRR_agematched = IRR_mod1a %>% 
  bind_rows(IRR_mod1b) %>% 
  bind_rows(IRR_mod2a) %>% 
  bind_rows(IRR_mod2b) %>% 
  bind_rows(IRR_mod3a) %>% 
  bind_rows(IRR_mod3b)


#########
## PAF ##
#########

PAF_mod1a = read_excel("../data/PAF_CSW.xlsx", sheet = 1, col_names =  TRUE) 
PAF_mod1a = apply(PAF_mod1a, 2, function(x) {
  c(PAF_mean = mean(x), 
    PAF_median = median(x), 
    PAF_q025 = quantile(x, 0.025), 
    PAF_q975 = quantile(x, 0.975))
})

rownames(PAF_mod1a)[c(3,4)] = c("PAF_q025", "PAF_q975")
PAF_mod1a <- as.data.frame(t(PAF_mod1a))

PAF_mod1a$year <- as.numeric(rownames(PAF_mod1a))
PAF_mod1a = PAF_mod1a %>% 
  mutate(FSW_assumption = "a - constant FSW age and SW duration") %>% 
  mutate(Transmission_assumption = "1 - constant transmission probability") %>% 
  mutate(model = "model 1a")


PAF_mod1b = read_excel("../data/PAF_CSW.xlsx", sheet = 2, col_names =  TRUE) 
PAF_mod1b = apply(PAF_mod1b, 2, function(x) {
  c(PAF_mean = mean(x), 
    PAF_median = median(x), 
    PAF_q025 = quantile(x, 0.025), 
    PAF_q975 = quantile(x, 0.975))
})

rownames(PAF_mod1b)[c(3,4)] = c("PAF_q025", "PAF_q975")
PAF_mod1b <- as.data.frame(t(PAF_mod1b))

PAF_mod1b$year <- as.numeric(rownames(PAF_mod1b))
PAF_mod1b = PAF_mod1b %>% 
  mutate(FSW_assumption = "b - changing FSW age and SW duration") %>% 
  mutate(Transmission_assumption = "1 - constant transmission probability") %>% 
  mutate(model = "model 1b")


PAF_mod2a = read_excel("../data/PAF_CSW.xlsx", sheet = 3, col_names =  TRUE) 
PAF_mod2a = apply(PAF_mod2a, 2, function(x) {
  c(PAF_mean = mean(x), 
    PAF_median = median(x), 
    PAF_q025 = quantile(x, 0.025), 
    PAF_q975 = quantile(x, 0.975))
})

rownames(PAF_mod2a)[c(3,4)] = c("PAF_q025", "PAF_q975")
PAF_mod2a <- as.data.frame(t(PAF_mod2a))

PAF_mod2a$year <- as.numeric(rownames(PAF_mod2a))
PAF_mod2a = PAF_mod2a %>% 
  mutate(FSW_assumption = "a - constant FSW age and SW duration") %>% 
  mutate(Transmission_assumption = "2 - exponentially declining transmission probability") %>% 
  mutate(model = "model 2a")

PAF_mod2b = read_excel("../data/PAF_CSW.xlsx", sheet = 4, col_names =  TRUE) 
PAF_mod2b = apply(PAF_mod2b, 2, function(x) {
  c(PAF_mean = mean(x), 
    PAF_median = median(x), 
    PAF_q025 = quantile(x, 0.025), 
    PAF_q975 = quantile(x, 0.975))
})

rownames(PAF_mod2b)[c(3,4)] = c("PAF_q025", "PAF_q975")
PAF_mod2b <- as.data.frame(t(PAF_mod2b))

PAF_mod2b$year <- as.numeric(rownames(PAF_mod2b))
PAF_mod2b = PAF_mod2b %>% 
  mutate(FSW_assumption = "b - changing FSW age and SW duration") %>% 
  mutate(Transmission_assumption = "2 - exponentially declining transmission probability") %>% 
  mutate(model = "model 2b")


PAF_mod3a = read_excel("../data/PAF_CSW.xlsx", sheet = 5, col_names =  TRUE) 
PAF_mod3a = apply(PAF_mod3a, 2, function(x) {
  c(PAF_mean = mean(x), 
    PAF_median = median(x), 
    PAF_q025 = quantile(x, 0.025), 
    PAF_q975 = quantile(x, 0.975))
})

rownames(PAF_mod3a)[c(3,4)] = c("PAF_q025", "PAF_q975")
PAF_mod3a <- as.data.frame(t(PAF_mod3a))

PAF_mod3a$year <- as.numeric(rownames(PAF_mod3a))
PAF_mod3a = PAF_mod3a %>% 
  mutate(FSW_assumption = "a - constant FSW age and SW duration") %>% 
  mutate(Transmission_assumption = "3 - dynamically changing transmission probability") %>% 
  mutate(model = "model 3a")


PAF_mod3b = read_excel("../data/PAF_CSW.xlsx", sheet = 6, col_names =  TRUE) 
PAF_mod3b = apply(PAF_mod3b, 2, function(x) {
  c(PAF_mean = mean(x), 
    PAF_median = median(x), 
    PAF_q025 = quantile(x, 0.025), 
    PAF_q975 = quantile(x, 0.975))
})

rownames(PAF_mod3b)[c(3,4)] = c("PAF_q025", "PAF_q975")
PAF_mod3b <- as.data.frame(t(PAF_mod3b))

PAF_mod3b$year <- as.numeric(rownames(PAF_mod3b))
PAF_mod3b = PAF_mod3b %>% 
  mutate(FSW_assumption = "b - changing FSW age and SW duration") %>% 
  mutate(Transmission_assumption = "3 - dynamically changing transmission probability") %>% 
  mutate(model = "model 3b")

PAF_CSW = PAF_mod1a %>% 
  bind_rows(PAF_mod1b) %>% 
  bind_rows(PAF_mod2a) %>% 
  bind_rows(PAF_mod2b) %>% 
  bind_rows(PAF_mod3a) %>% 
  bind_rows(PAF_mod3b)


## PAF: construct it such that we can distinguish between new infections in FSW and new
## infections in clients.

totalhiv_mod1a = read_excel("../data/NewAdultHIV.xlsx", sheet = 1, col_names = TRUE)
totalFSW_mod1a = read_excel("../data/NewHIVinFSW.xlsx", sheet = 1, col_names = TRUE)
clients_mod1a = read_excel("../data/NewHIVClients.xlsx", sheet = 1, col_names =  TRUE)

clientspaf_mod1a = (clients_mod1a)/totalhiv_mod1a
clientspaf_mod1a = apply(clientspaf_mod1a, 2, function(x) {
  c(PAF_mean = mean(x), 
    PAF_median = median(x), 
    PAF_q025 = quantile(x, 0.025), 
    PAF_q975 = quantile(x, 0.975))
})

rownames(clientspaf_mod1a)[c(3,4)] = c("PAF_q025", "PAF_q975")
clientspaf_mod1a <- as.data.frame(t(clientspaf_mod1a))

clientspaf_mod1a$year <- as.numeric(rownames(clientspaf_mod1a))
clientspaf_mod1a = clientspaf_mod1a %>% 
  mutate(FSW_assumption = "a - constant FSW age and SW duration") %>% 
  mutate(Transmission_assumption = "1 - constant transmission probability") %>% 
  mutate(model = "model 1a")

fswpaf_mod1a = totalFSW_mod1a/totalhiv_mod1a
fswpaf_mod1a = apply(fswpaf_mod1a, 2, function(x) {
  c(PAF_mean = mean(x), 
    PAF_median = median(x), 
    PAF_q025 = quantile(x, 0.025), 
    PAF_q975 = quantile(x, 0.975))
})

rownames(fswpaf_mod1a)[c(3,4)] = c("PAF_q025", "PAF_q975")
fswpaf_mod1a <- as.data.frame(t(fswpaf_mod1a))

fswpaf_mod1a$year <- as.numeric(rownames(fswpaf_mod1a))
fswpaf_mod1a = fswpaf_mod1a %>% 
  mutate(FSW_assumption = "a - constant FSW age and SW duration") %>% 
  mutate(Transmission_assumption = "1 - constant transmission probability") %>% 
  mutate(model = "model 1a")


## relshare

relshare_mod1a = (totalFSW_mod1a)/(totalFSW_mod1a + clients_mod1a)
relshare_mod1a = apply(relshare_mod1a, 2, function(x) {
  c(share_mean = mean(x), 
    share_median = median(x), 
    share_q025 = quantile(x, 0.025), 
    share_q975 = quantile(x, 0.975))
})

rownames(relshare_mod1a)[c(3,4)] = c("share_q025", "share_q975")
relshare_mod1a <- as.data.frame(t(relshare_mod1a))

relshare_mod1a$year <- as.numeric(rownames(relshare_mod1a))
relshare_mod1a = relshare_mod1a %>% 
  mutate(FSW_assumption = "a - constant FSW age and SW duration") %>% 
  mutate(Transmission_assumption = "1 - constant transmission probability") %>% 
  mutate(model = "model 1a")


totalhiv_mod1b = read_excel("../data/NewAdultHIV.xlsx", sheet = 2, col_names = TRUE)
totalFSW_mod1b = read_excel("../data/NewHIVinFSW.xlsx", sheet = 2, col_names = TRUE)
clients_mod1b = read_excel("../data/NewHIVClients.xlsx", sheet = 2, col_names =  TRUE)

clientspaf_mod1b= (clients_mod1b)/totalhiv_mod1b
clientspaf_mod1b = apply(clientspaf_mod1b, 2, function(x) {
  c(PAF_mean = mean(x), 
    PAF_median = median(x), 
    PAF_q025 = quantile(x, 0.025), 
    PAF_q975 = quantile(x, 0.975))
})

rownames(clientspaf_mod1b)[c(3,4)] = c("PAF_q025", "PAF_q975")
clientspaf_mod1b <- as.data.frame(t(clientspaf_mod1b))

clientspaf_mod1b$year <- as.numeric(rownames(clientspaf_mod1b))
clientspaf_mod1b = clientspaf_mod1b %>% 
  mutate(FSW_assumption = "b - changing FSW age and SW duration") %>% 
  mutate(Transmission_assumption = "1 - constant transmission probability") %>% 
  mutate(model = "model 1b")

fswpaf_mod1b = totalFSW_mod1b/totalhiv_mod1b
fswpaf_mod1b = apply(fswpaf_mod1b, 2, function(x) {
  c(PAF_mean = mean(x), 
    PAF_median = median(x), 
    PAF_q025 = quantile(x, 0.025), 
    PAF_q975 = quantile(x, 0.975))
})

rownames(fswpaf_mod1b)[c(3,4)] = c("PAF_q025", "PAF_q975")
fswpaf_mod1b <- as.data.frame(t(fswpaf_mod1b))

fswpaf_mod1b$year <- as.numeric(rownames(fswpaf_mod1b))
fswpaf_mod1b = fswpaf_mod1b %>% 
  mutate(FSW_assumption = "b - changing FSW age and SW duration") %>% 
  mutate(Transmission_assumption = "1 - constant transmission probability") %>% 
  mutate(model = "model 1b")


relshare_mod1b = (totalFSW_mod1b)/(totalFSW_mod1b + clients_mod1b)
relshare_mod1b = apply(relshare_mod1b, 2, function(x) {
  c(share_mean = mean(x), 
    share_median = median(x), 
    share_q025 = quantile(x, 0.025), 
    share_q975 = quantile(x, 0.975))
})

rownames(relshare_mod1b)[c(3,4)] = c("share_q025", "share_q975")
relshare_mod1b <- as.data.frame(t(relshare_mod1b))

relshare_mod1b$year <- as.numeric(rownames(relshare_mod1b))
relshare_mod1b = relshare_mod1b %>% 
  mutate(FSW_assumption = "b - changing FSW age and SW duration") %>% 
  mutate(Transmission_assumption = "1 - constant transmission probability") %>% 
  mutate(model = "model 1b")


totalhiv_mod2a = read_excel("../data/NewAdultHIV.xlsx", sheet = 3, col_names = TRUE)
totalFSW_mod2a = read_excel("../data/NewHIVinFSW.xlsx", sheet = 3, col_names = TRUE)
clients_mod2a = read_excel("../data/NewHIVClients.xlsx", sheet = 3, col_names =  TRUE)

clientspaf_mod2a = (clients_mod2a)/totalhiv_mod2a
clientspaf_mod2a = apply(clientspaf_mod2a, 2, function(x) {
  c(PAF_mean = mean(x), 
    PAF_median = median(x), 
    PAF_q025 = quantile(x, 0.025), 
    PAF_q975 = quantile(x, 0.975))
})

rownames(clientspaf_mod2a)[c(3,4)] = c("PAF_q025", "PAF_q975")
clientspaf_mod2a <- as.data.frame(t(clientspaf_mod2a))

clientspaf_mod2a$year <- as.numeric(rownames(clientspaf_mod2a))
clientspaf_mod2a = clientspaf_mod2a %>% 
  mutate(FSW_assumption = "a - constant FSW age and SW duration") %>% 
  mutate(Transmission_assumption = "2 - exponentially declining transmission probability") %>% 
  mutate(model = "model 2a")

fswpaf_mod2a = totalFSW_mod2a/totalhiv_mod2a
fswpaf_mod2a = apply(fswpaf_mod2a, 2, function(x) {
  c(PAF_mean = mean(x), 
    PAF_median = median(x), 
    PAF_q025 = quantile(x, 0.025), 
    PAF_q975 = quantile(x, 0.975))
})

rownames(fswpaf_mod2a)[c(3,4)] = c("PAF_q025", "PAF_q975")
fswpaf_mod2a <- as.data.frame(t(fswpaf_mod2a))

fswpaf_mod2a$year <- as.numeric(rownames(fswpaf_mod2a))
fswpaf_mod2a = fswpaf_mod2a %>%
  mutate(FSW_assumption = "a - constant FSW age and SW duration") %>% 
  mutate(Transmission_assumption = "2 - exponentially declining transmission probability") %>% 
  mutate(model = "model 2a")
  

relshare_mod2a = (totalFSW_mod2a)/(totalFSW_mod2a + clients_mod2a)
relshare_mod2a = apply(relshare_mod2a, 2, function(x) {
  c(share_mean = mean(x), 
    share_median = median(x), 
    share_q025 = quantile(x, 0.025), 
    share_q975 = quantile(x, 0.975))
})

rownames(relshare_mod2a)[c(3,4)] = c("share_q025", "share_q975")
relshare_mod2a <- as.data.frame(t(relshare_mod2a))

relshare_mod2a$year <- as.numeric(rownames(relshare_mod2a))
relshare_mod2a = relshare_mod2a %>% 
  mutate(FSW_assumption = "a - constant FSW age and SW duration") %>% 
  mutate(Transmission_assumption = "2 - exponentially declining transmission probability") %>% 
  mutate(model = "model 2a")



totalhiv_mod2b = read_excel("../data/NewAdultHIV.xlsx", sheet = 4, col_names = TRUE)
totalFSW_mod2b = read_excel("../data/NewHIVinFSW.xlsx", sheet = 4, col_names = TRUE)
clients_mod2b = read_excel("../data/NewHIVClients.xlsx", sheet = 4, col_names =  TRUE)

clientspaf_mod2b= (clients_mod2b)/totalhiv_mod2b
clientspaf_mod2b = apply(clientspaf_mod2b, 2, function(x) {
  c(PAF_mean = mean(x), 
    PAF_median = median(x), 
    PAF_q025 = quantile(x, 0.025), 
    PAF_q975 = quantile(x, 0.975))
})

rownames(clientspaf_mod2b)[c(3,4)] = c("PAF_q025", "PAF_q975")
clientspaf_mod2b <- as.data.frame(t(clientspaf_mod2b))

clientspaf_mod2b$year <- as.numeric(rownames(clientspaf_mod2b))
clientspaf_mod2b = clientspaf_mod2b %>% 
  mutate(FSW_assumption = "b - changing FSW age and SW duration") %>% 
  mutate(Transmission_assumption = "2 - exponentially declining transmission probability") %>% 
  mutate(model = "model 2b")

fswpaf_mod2b = totalFSW_mod2b/totalhiv_mod2b
fswpaf_mod2b = apply(fswpaf_mod2b, 2, function(x) {
  c(PAF_mean = mean(x), 
    PAF_median = median(x), 
    PAF_q025 = quantile(x, 0.025), 
    PAF_q975 = quantile(x, 0.975))
})

rownames(fswpaf_mod2b)[c(3,4)] = c("PAF_q025", "PAF_q975")
fswpaf_mod2b <- as.data.frame(t(fswpaf_mod2b))

fswpaf_mod2b$year <- as.numeric(rownames(fswpaf_mod2b))
fswpaf_mod2b = fswpaf_mod2b %>% 
  mutate(FSW_assumption = "b - changing FSW age and SW duration") %>% 
  mutate(Transmission_assumption = "2 - exponentially declining transmission probability") %>% 
  mutate(model = "model 2b")


relshare_mod2b = (totalFSW_mod2b)/(totalFSW_mod2b + clients_mod2b)
relshare_mod2b = apply(relshare_mod2b, 2, function(x) {
  c(share_mean = mean(x), 
    share_median = median(x), 
    share_q025 = quantile(x, 0.025), 
    share_q975 = quantile(x, 0.975))
})

rownames(relshare_mod2b)[c(3,4)] = c("share_q025", "share_q975")
relshare_mod2b <- as.data.frame(t(relshare_mod2b))

relshare_mod2b$year <- as.numeric(rownames(relshare_mod2b))
relshare_mod2b = relshare_mod2b %>% 
  mutate(FSW_assumption = "b - changing FSW age and SW duration") %>% 
  mutate(Transmission_assumption = "2 - exponentially declining transmission probability") %>% 
  mutate(model = "model 2b")

totalhiv_mod3a = read_excel("../data/NewAdultHIV.xlsx", sheet = 5, col_names = TRUE)
totalFSW_mod3a = read_excel("../data/NewHIVinFSW.xlsx", sheet = 5, col_names = TRUE)
clients_mod3a = read_excel("../data/NewHIVClients.xlsx", sheet = 5, col_names =  TRUE)

clientspaf_mod3a = (clients_mod3a)/totalhiv_mod3a
clientspaf_mod3a = apply(clientspaf_mod3a, 2, function(x) {
  c(PAF_mean = mean(x), 
    PAF_median = median(x), 
    PAF_q025 = quantile(x, 0.025), 
    PAF_q975 = quantile(x, 0.975))
})

rownames(clientspaf_mod3a)[c(3,4)] = c("PAF_q025", "PAF_q975")
clientspaf_mod3a <- as.data.frame(t(clientspaf_mod3a))

clientspaf_mod3a$year <- as.numeric(rownames(clientspaf_mod3a))
clientspaf_mod3a = clientspaf_mod3a %>% 
  mutate(FSW_assumption = "a - constant FSW age and SW duration") %>% 
  mutate(Transmission_assumption = "3 - dynamically changing transmission probability") %>% 
  mutate(model = "model 3a")

fswpaf_mod3a = totalFSW_mod3a/totalhiv_mod3a
fswpaf_mod3a = apply(fswpaf_mod3a, 2, function(x) {
  c(PAF_mean = mean(x), 
    PAF_median = median(x), 
    PAF_q025 = quantile(x, 0.025), 
    PAF_q975 = quantile(x, 0.975))
})

rownames(fswpaf_mod3a)[c(3,4)] = c("PAF_q025", "PAF_q975")
fswpaf_mod3a <- as.data.frame(t(fswpaf_mod3a))

fswpaf_mod3a$year <- as.numeric(rownames(fswpaf_mod3a))
fswpaf_mod3a = fswpaf_mod3a %>%
  mutate(FSW_assumption = "a - constant FSW age and SW duration") %>% 
  mutate(Transmission_assumption = "3 - dynamically changing transmission probability") %>% 
  mutate(model = "model 3a")

relshare_mod3a = (totalFSW_mod3a)/(totalFSW_mod3a + clients_mod3a)
relshare_mod3a = apply(relshare_mod3a, 2, function(x) {
  c(share_mean = mean(x), 
    share_median = median(x), 
    share_q025 = quantile(x, 0.025), 
    share_q975 = quantile(x, 0.975))
})

rownames(relshare_mod3a)[c(3,4)] = c("share_q025", "share_q975")
relshare_mod3a <- as.data.frame(t(relshare_mod3a))

relshare_mod3a$year <- as.numeric(rownames(relshare_mod3a))
relshare_mod3a = relshare_mod3a %>% 
  mutate(FSW_assumption = "a - constant FSW age and SW duration") %>% 
  mutate(Transmission_assumption = "3 - dynamically changing transmission probability") %>% 
  mutate(model = "model 3a")

totalhiv_mod3b = read_excel("../data/NewAdultHIV.xlsx", sheet = 6, col_names = TRUE)
totalFSW_mod3b = read_excel("../data/NewHIVinFSW.xlsx", sheet = 6, col_names = TRUE)
clients_mod3b = read_excel("../data/NewHIVClients.xlsx", sheet = 6, col_names =  TRUE)

clientspaf_mod3b= (clients_mod3b)/totalhiv_mod3b
clientspaf_mod3b = apply(clientspaf_mod3b, 2, function(x) {
  c(PAF_mean = mean(x), 
    PAF_median = median(x), 
    PAF_q025 = quantile(x, 0.025), 
    PAF_q975 = quantile(x, 0.975))
})

rownames(clientspaf_mod3b)[c(3,4)] = c("PAF_q025", "PAF_q975")
clientspaf_mod3b <- as.data.frame(t(clientspaf_mod3b))

clientspaf_mod3b$year <- as.numeric(rownames(clientspaf_mod3b))
clientspaf_mod3b = clientspaf_mod3b %>% 
  mutate(FSW_assumption = "b - changing FSW age and SW duration") %>% 
  mutate(Transmission_assumption = "3 - dynamically changing transmission probability") %>% 
  mutate(model = "model 3b")

fswpaf_mod3b = totalFSW_mod3b/totalhiv_mod3b
fswpaf_mod3b = apply(fswpaf_mod3b, 2, function(x) {
  c(PAF_mean = mean(x), 
    PAF_median = median(x), 
    PAF_q025 = quantile(x, 0.025), 
    PAF_q975 = quantile(x, 0.975))
})

rownames(fswpaf_mod3b)[c(3,4)] = c("PAF_q025", "PAF_q975")
fswpaf_mod3b <- as.data.frame(t(fswpaf_mod3b))

fswpaf_mod3b$year <- as.numeric(rownames(fswpaf_mod3b))
fswpaf_mod3b = fswpaf_mod3b %>% 
  mutate(FSW_assumption = "b - changing FSW age and SW duration") %>% 
  mutate(Transmission_assumption = "3 - dynamically changing transmission probability") %>% 
  mutate(model = "model 3b")

relshare_mod3b = (totalFSW_mod3b)/(totalFSW_mod3b + clients_mod3b)
relshare_mod3b = apply(relshare_mod3b, 2, function(x) {
  c(share_mean = mean(x), 
    share_median = median(x), 
    share_q025 = quantile(x, 0.025), 
    share_q975 = quantile(x, 0.975))
})

rownames(relshare_mod3b)[c(3,4)] = c("share_q025", "share_q975")
relshare_mod3b <- as.data.frame(t(relshare_mod3b))

relshare_mod3b$year <- as.numeric(rownames(relshare_mod3b))
relshare_mod3b = relshare_mod3b %>% 
  mutate(FSW_assumption = "b - changing FSW age and SW duration") %>% 
  mutate(Transmission_assumption = "3 - dynamically changing transmission probability") %>% 
  mutate(model = "model 3b")

PAF_clients = clientspaf_mod1a %>% 
  bind_rows(clientspaf_mod1b) %>% 
  bind_rows(clientspaf_mod2a) %>% 
  bind_rows(clientspaf_mod2b) %>% 
  bind_rows(clientspaf_mod3a) %>% 
  bind_rows(clientspaf_mod3b)

PAF_fsw = fswpaf_mod1a %>% 
  bind_rows(fswpaf_mod1b) %>% 
  bind_rows(fswpaf_mod2a) %>% 
  bind_rows(fswpaf_mod2b) %>% 
  bind_rows(fswpaf_mod3a) %>% 
  bind_rows(fswpaf_mod3b)

relPAF = relshare_mod1a %>% 
  bind_rows(relshare_mod1b) %>% 
  bind_rows(relshare_mod2a) %>% 
  bind_rows(relshare_mod2b) %>% 
  bind_rows(relshare_mod3a) %>% 
  bind_rows(relshare_mod3b)


############################################
## HIV prevalence data general population ##
############################################


colnames_modprev = read_excel("../data/generalprevs_allmodels_30yART2019.xlsx", sheet = 1, col_names = FALSE) %>% select(1) %>% pull()
mod1aprev  = read_excel("../data/generalprevs_allmodels_30yART2019.xlsx", sheet = 1, col_names = FALSE) %>% select(-1) %>% t() %>% as.tibble()
rownames(mod1aprev) = NULL
colnames(mod1aprev) = colnames_modprev 
mod1aprev = mod1aprev %>% select(-4) %>% 
  mutate(Mean = as.numeric(as.character(Mean)), 
         LL = as.numeric(as.character(LL)), 
         UL = as.numeric(as.character(UL))) %>% 
  mutate() %>% 
  mutate(FSW_assumption = "a - constant FSW age and SW duration") %>% 
  mutate(Transmission_assumption = "1 - constant transmission probability") %>% 
  mutate(model = "model 1a")

mod1bprev  = read_excel("../data/generalprevs_allmodels_30yART2019.xlsx", sheet = 2, col_names = FALSE) %>% select(-1) %>% t() %>% as.tibble()
rownames(mod1bprev) = NULL
colnames(mod1bprev) = colnames_modprev 
mod1bprev = mod1bprev %>% select(-4) %>% 
  mutate(Mean = as.numeric(as.character(Mean)), 
         LL = as.numeric(as.character(LL)), 
         UL = as.numeric(as.character(UL))) %>% 
  mutate() %>% 
  mutate(FSW_assumption = "b - changing FSW age and SW duration") %>% 
  mutate(Transmission_assumption = "1 - constant transmission probability") %>% 
  mutate(model = "model 1b")

mod2aprev  = read_excel("../data/generalprevs_allmodels_30yART2019.xlsx", sheet = 3, col_names = FALSE) %>% select(-1) %>% t() %>% as.tibble()
rownames(mod2aprev) = NULL
colnames(mod2aprev) = colnames_modprev 
mod2aprev = mod2aprev %>% select(-4) %>% 
  mutate(Mean = as.numeric(as.character(Mean)), 
         LL = as.numeric(as.character(LL)), 
         UL = as.numeric(as.character(UL))) %>% 
  mutate() %>% 
  mutate(FSW_assumption = "a - constant FSW age and SW duration") %>% 
  mutate(Transmission_assumption = "2 - exponentially declining transmission probability") %>% 
  mutate(model = "model 2a")


mod2bprev  = read_excel("../data/generalprevs_allmodels_30yART2019.xlsx", sheet = 4, col_names = FALSE) %>% select(-1) %>% t() %>% as.tibble()
rownames(mod2bprev) = NULL
colnames(mod2bprev) = colnames_modprev 
mod2bprev = mod2bprev %>% select(-4) %>% 
  mutate(Mean = as.numeric(as.character(Mean)), 
         LL = as.numeric(as.character(LL)), 
         UL = as.numeric(as.character(UL))) %>% 
  mutate() %>% 
  mutate(FSW_assumption = "b - changing FSW age and SW duration") %>% 
  mutate(Transmission_assumption = "2 - exponentially declining transmission probability") %>% 
  mutate(model = "model 2b")


mod3aprev  = read_excel("../data/generalprevs_allmodels_30yART2019.xlsx", sheet = 5, col_names = FALSE) %>% select(-1) %>% t() %>% as.tibble()
rownames(mod3aprev) = NULL
colnames(mod3aprev) = colnames_modprev 
mod3aprev = mod3aprev %>% select(-4) %>% 
  mutate(Mean = as.numeric(as.character(Mean)), 
         LL = as.numeric(as.character(LL)), 
         UL = as.numeric(as.character(UL))) %>% 
  mutate() %>% 
  mutate(FSW_assumption = "a - constant FSW age and SW duration") %>% 
  mutate(Transmission_assumption = "3 - dynamically changing transmission probability") %>% 
  mutate(model = "model 3a")


mod3bprev  = read_excel("../data/generalprevs_allmodels_30yART2019.xlsx", sheet = 6, col_names = FALSE) %>% select(-1) %>% t() %>% as.tibble()
rownames(mod3bprev) = NULL
colnames(mod3bprev) = colnames_modprev 
mod3bprev = mod3bprev %>% select(-4) %>% 
  mutate(Mean = as.numeric(as.character(Mean)), 
         LL = as.numeric(as.character(LL)), 
         UL = as.numeric(as.character(UL))) %>% 
  mutate() %>% 
  mutate(FSW_assumption = "b - changing FSW age and SW duration") %>% 
  mutate(Transmission_assumption = "3 - dynamically changing transmission probability") %>% 
  mutate(model = "model 3b")

data_prev_models = 
  mod1aprev %>% 
  bind_rows(mod1bprev) %>% 
  bind_rows(mod2aprev) %>% 
  bind_rows(mod2bprev) %>% 
  bind_rows(mod3aprev) %>% 
  bind_rows(mod3bprev)


###############################################
### Posterior distributions model parameter
###########################################

mod1a_post = read_excel("../data/ModelParameters_posterior.xlsx", sheet = 1, col_names = TRUE) %>% as.tibble() %>% 
  mutate(simulation = row_number()) %>% 
  pivot_longer(cols = 1:10) %>% 
  mutate(scenario = "Scenario 1a")

mod1b_post = read_excel("../data/ModelParameters_posterior.xlsx", sheet = 2, col_names = TRUE) %>% as.tibble() %>% 
  mutate(simulation = row_number()) %>% 
  pivot_longer(cols = 1:10) %>% 
  mutate(scenario = "Scenario 1b")

mod2a_post = read_excel("../data/ModelParameters_posterior.xlsx", sheet = 3, col_names = TRUE) %>% as.tibble() %>% 
  mutate(simulation = row_number()) %>% 
  pivot_longer(cols = 1:11) %>% 
  mutate(scenario = "Scenario 2a")

mod2b_post = read_excel("../data/ModelParameters_posterior.xlsx", sheet = 4, col_names = TRUE) %>% as.tibble() %>% 
  mutate(simulation = row_number()) %>% 
  pivot_longer(cols = 1:11) %>% 
  mutate(scenario = "Scenario 2b")

mod3a_post = read_excel("../data/ModelParameters_posterior.xlsx", sheet = 5, col_names = TRUE) %>% as.tibble() %>% 
  mutate(simulation = row_number()) %>% 
  pivot_longer(cols = 1:11) %>% 
  mutate(scenario = "Scenario 3a")

mod3b_post = read_excel("../data/ModelParameters_posterior.xlsx", sheet = 6, col_names = TRUE) %>% as.tibble() %>% 
  mutate(simulation = row_number()) %>% 
  pivot_longer(cols = 1:11) %>% 
  mutate(scenario = "Scenario 3b")

modelparameters_post = mod1a_post %>% 
  bind_rows(mod1b_post) %>% 
  bind_rows(mod2a_post) %>% 
  bind_rows(mod2b_post) %>% 
  bind_rows(mod3a_post) %>% 
  bind_rows(mod3b_post)


modelparameters_post_summary = 
  modelparameters_post %>% 
  group_by(scenario, name) %>% 
  summarise(mean = mean(value), 
         median = median(value), 
         q0025 = quantile(value, 0.025), 
         q0975 = quantile(value, 0.975)) 



RRclientToFSW1985 = rgamma(n = 1000000, shape = 6.25, rate= 1.25) # from priors.txt file 
FSWcontactHRmen = rgamma(n= 1000000, shape = 5.44444, rate = 1.55556)
HIVdiagentrySW = rbeta(n=1000000, shape1 = 1.0000, shape2=1.0000)
modelparameters_prior = data.frame(name = unique(modelparameters_post$name),  
                                   mean = c(0.6, 0.05, 0.0120, 0.0080, 1.30, 35.0, 22.0, mean(FSWcontactHRmen), mean(HIVdiagentrySW), 0.0010, mean(RRclientToFSW1985)), # these values we can also get either with the distribution and parameters stated in the priors txt file or from the table 7.1 or 8.1 from the thembisa v4.7 documentation. 
                                   q0025 = c(0.295, 0.03, 0.0043,  0.0032, 1.06, 25.9, 18.3, quantile(FSWcontactHRmen, 0.025), quantile(HIVdiagentrySW, 0.025), 0.0003, quantile(RRclientToFSW1985, 0.025)), 
                                   q0975 = c(0.866, 0.098, 0.0236, 0.0149, 1.57, 45.5, 26.1, quantile(FSWcontactHRmen, 0.975), quantile(HIVdiagentrySW, 0.975), 0.0022, quantile(RRclientToFSW1985, 0.975))) %>% 
  mutate(scenario = "prior")

modelparameters_post_summary = modelparameters_post_summary %>% 
  mutate(mean = ifelse(name == "HIV_prevalence_init_HR", 0.001*100*mean, mean), 
         median = ifelse(name == "HIV_prevalence_init_HR", 0.001*100*median, median), 
         q0025 = ifelse(name == "HIV_prevalence_init_HR", 0.001*100*q0025, q0025), 
         q0975 = ifelse(name == "HIV_prevalence_init_HR", 0.001*100*q0975, q0975))

modelparameters_summary= modelparameters_prior %>% 
  bind_rows(modelparameters_post_summary)


##############################################################
#### Construct data: illustration of different assumptions ###
############################################################## 

### models *a* = age = gamma distribution with mean 29 and SD 9, leave sex work at rate of 0.33/ annum (duration = exponential distribution with rate = 0.33)
FSWchar_constant = data.frame(year = 1985:2045, 
                              mean_age = 29, 
                              sd_age = 9, 
                              mean_dur = 3) %>% 
  mutate(shape_age = (mean_age/sd_age)^2, 
         rate_age = mean_age/(sd_age^2), 
         rate_dur  = 1/mean_dur) %>% 
  mutate(q0025_age = qgamma(0.025, shape = shape_age, rate = rate_age), 
         q0795_age = qgamma(0.975, shape = shape_age, rate = rate_age), 
         q0025_dur = qexp(0.025, rate = rate_dur), 
         q0975_dur = qexp(0.975, rate = rate_dur), 
         q025_age = qgamma(0.25, shape = shape_age, rate = rate_age), 
         q075_age = qgamma(0.75, shape = shape_age, rate = rate_age), 
         q025_dur = qexp(0.25, rate = rate_dur), 
         q075_dur = qexp(0.75, rate = rate_dur))


### models *b*: 
FSWchar_timevar = 
       data.frame(year = 1996:2019, mean_age = c(26.4257, 26.6403,	26.8549,	27.0794,	27.3074,	
       27.5418,	27.7807,	28.0261,	28.2608,	28.5060,	28.7488,	29.0033,	29.2575,	29.5073,	29.7679,
       30.0221,	30.2870,	30.5627,	30.8417,	31.1231,	31.4050,	31.6977,	31.9844,	32.2761),
       sd_age  = c(5.5413,	5.6170,	5.6947,	5.7722,	5.8500,	
                   5.9298,	6.0081,	6.0910,	6.1715,	6.2534,	6.3359,	6.4194,	6.5050,	6.5899,	6.6776,	
                   6.7655,	6.8555,	6.9439,	7.0375,	7.1321,	7.2305,	7.3283,	7.4246,	7.5166), 
       mean_dur = c(2.6848,	2.8073,	2.9322,	3.0675,	3.2114,	
                       3.3526,	3.5018,	3.6612,	3.8241,	3.9925,	4.1619,	4.3524,	4.5538,	4.7584,	4.9791,	
                       5.2042,	5.4378,	5.6825,	5.9397,	6.2024,	6.4941,	6.7968,	7.1042,	7.4234))

FSWchar_timevar_tot = data.frame(year = 1985:1995, mean_age=26.4257, sd_age = 5.5413, mean_dur = 2.6848 ) %>% 
  bind_rows(FSWchar_timevar) %>% 
  bind_rows(data.frame(year = 2020:2045, mean_age = 32.2761, sd_age = 7.5166, mean_dur = 7.4234))

FSWchar_timevar = FSWchar_timevar_tot %>% 
  mutate(shape_age = (mean_age/sd_age)^2, 
         rate_age = mean_age/(sd_age^2), 
         rate_dur  = 1/mean_dur) %>% 
  mutate(q0025_age = qgamma(0.025, shape = shape_age, rate = rate_age), 
         q0795_age = qgamma(0.975, shape = shape_age, rate = rate_age), 
         q0025_dur = qexp(0.025, rate = rate_dur), 
         q0975_dur = qexp(0.975, rate = rate_dur), 
         q025_age = qgamma(0.25, shape = shape_age, rate = rate_age), 
         q075_age = qgamma(0.75, shape = shape_age, rate = rate_age), 
         q025_dur = qexp(0.25, rate = rate_dur), 
         q075_dur = qexp(0.75, rate = rate_dur))


### models *1*

transm_assumps = modelparameters_post %>% filter(name %in% c("Client_FSW_transmission", "FSW_start_epi"))
transm_assumps_conts = transm_assumps %>% filter(scenario %in% c("Scenario 1a", "Scenario 1b"))

transm_assumps_conts = transm_assumps_conts %>% 
  ungroup() %>% 
  group_by(scenario) %>% 
  summarise(mean_tr = mean(value), 
            q0025_tr = quantile(value, 0.025), 
            q0975_tr = quantile(value, 0.975), 
            q025 =  quantile(value, 0.25),
            q075 = quantile(value, 0.75))

transmission_constant_1a = 
  data_frame(year = 1985:2045, 
             transm_assumps_conts[1, ])

transmission_constant_1b = 
  data_frame(year = 1985:2045, 
             transm_assumps_conts[2, ])
  
transm_assumps_const = transmission_constant_1a %>% bind_rows(transmission_constant_1b)

params_exp = transm_assumps %>% filter(scenario %in% c("Scenario 2a", "Scenario 2b"))
params_exp = params_exp %>% pivot_wider(values_from = value, names_from = name)

transm_assumps_exp_sc2a = data.frame(scenario = "Scenario 2a", 
                                year = 1985:2045, 
                                mean_tr = NA, 
                                q0025_tr = NA, 
                                q0975_tr =NA, 
                                q025_tr = NA, 
                                q075_tr = NA)

transm_assumps_exp_sc2b = data.frame(scenario = "Scenario 2b", 
                                     year = 1985:2045, 
                                     mean_tr = NA, 
                                     q0025_tr = NA, 
                                     q0975_tr =NA, 
                                     q025_tr = NA, 
                                     q075_tr = NA)

for (i in 1985:2045){
  params_exp_temp = params_exp %>% 
    mutate(baseprob = Client_FSW_transmission * (1+(FSW_start_epi-1)*0.75^(i-1985)))
  params_exp_sum = params_exp_temp %>% 
    group_by(scenario) %>% 
    summarise(mean_tr = mean(baseprob), 
              q0025_tr = quantile(baseprob, 0.025), 
              q0975_tr = quantile(baseprob, 0.975), 
              q025 =  quantile(baseprob, 0.25),
              q075 = quantile(baseprob, 0.75))
  transm_assumps_exp_sc2a[i-1984, c(3:7)] = params_exp_sum[1, -1]
  transm_assumps_exp_sc2b[i-1984, c(3:7)] = params_exp_sum[2, -1]
}


transm_assumps_exp = transm_assumps_exp_sc2a %>% 
  bind_rows(transm_assumps_exp_sc2b)


### 3a / 3b


params_dyn = transm_assumps %>% filter(scenario %in% c("Scenario 3a", "Scenario 3b"))
params_dyn = params_dyn %>% pivot_wider(values_from = value, names_from = name)


# intermediate 

client_prev_ART = data_models_30y %>% filter(model %in% c("model 3a", "model 3b")) %>% 
  select(Year, model, 
         ART_coverage_in_FSW_clients_Mean, `ART_coverage_in_FSW_clients_95%LL`, `ART_coverage_in_FSW_clients_95%UL`, 
         HIV_prevalence_in_male_clients_of_sex_workers_Mean,`HIV_prevalence_in_male_clients_of_sex_workers_95%LL`, `HIV_prevalence_in_male_clients_of_sex_workers_95%UL`) %>% 
  mutate(across(everything(), ~replace_na(.x, 0)))

client_prev_ART = client_prev_ART %>% 
  mutate(scenario = gsub("model", "Scenario", model))

transm_assumps_dyn_sc3a = data.frame(scenario = "Scenario 3a", 
                                     year = 1985:2045, 
                                     mean_tr = NA, 
                                     q0025_tr = NA, 
                                     q0975_tr =NA, 
                                     q025_tr = NA, 
                                     q075_tr = NA)

transm_assumps_dyn_sc3b = data.frame(scenario = "Scenario 3b", 
                                     year = 1985:2045, 
                                     mean_tr = NA, 
                                     q0025_tr = NA, 
                                     q0975_tr =NA, 
                                     q025_tr = NA, 
                                     q075_tr = NA)


for (i in 1985:2045){
  client_prev_ART_temp = client_prev_ART %>% filter(Year == i)
  dur_const = 3
  dur_change = FSWchar_timevar$mean_dur[FSWchar_timevar$year == i]
  
  params_dyn_temp = params_dyn %>%
    left_join(client_prev_ART_temp) %>% 
     mutate(baseprob = 
              case_when(scenario == "Scenario 3a" ~Client_FSW_transmission * (1+(FSW_start_epi-1)*exp(-4.0*HIV_prevalence_in_male_clients_of_sex_workers_Mean * (1- ART_coverage_in_FSW_clients_Mean) * dur_const)), 
                        scenario == "Scenario 3b" ~ Client_FSW_transmission * (1.0 + (FSW_start_epi -1) * exp(-4.0*HIV_prevalence_in_male_clients_of_sex_workers_Mean * (1.0-ART_coverage_in_FSW_clients_Mean) * dur_change))))
  
  params_dyn_temp = params_dyn_temp %>% 
    group_by(scenario) %>% 
    summarise(mean_tr = mean(baseprob), 
              q0025_tr = quantile(baseprob, 0.025), 
              q0975_tr = quantile(baseprob, 0.975), 
              q025 =  quantile(baseprob, 0.25),
              q075 = quantile(baseprob, 0.75))
  transm_assumps_dyn_sc3a[i-1984, c(3:7)] = params_dyn_temp[1, -1]
  transm_assumps_dyn_sc3b[i-1984, c(3:7)] = params_dyn_temp[2, -1]
}


transm_assumps_dyn = transm_assumps_dyn_sc3a %>% 
  bind_rows(transm_assumps_dyn_sc3b)


FSWchar_constant = FSWchar_constant %>% mutate(assumption = "Time-invariant (a: constant)", 
                                               scenario = "Scenario 1a / 2a / 3a")


FSWchar_constant_age = FSWchar_constant %>% select(year, mean = mean_age, q0025= q0025_age, q0975 = q0795_age, q025 = q025_age, q075 = q075_age, assumption, scenario) %>% 
  mutate(parameter = "FSW age (years)")
FSWchar_constant_dur = FSWchar_constant %>% select(year, mean = mean_dur, q0025= q0025_dur, q0975 = q0975_dur, q025 = q025_dur, q075 = q075_dur, assumption, scenario) %>% 
  mutate(parameter = "SW duration (years)")

FSWchar_timevar = FSWchar_timevar %>% mutate(assumption = "Time-varying (b: increasing)", 
                                             scenario = "Scenario 1b / 2b / 3b")

FSWchar_timevar_age = FSWchar_timevar %>% select(year, mean = mean_age, q0025= q0025_age, q0975 = q0795_age, q025 = q025_age, q075 = q075_age, assumption, scenario) %>% 
  mutate(parameter = "FSW age (years)")
FSWchar_timevar_dur = FSWchar_timevar %>% select(year, mean = mean_dur, q0025= q0025_dur, q0975 = q0975_dur, q025 = q025_dur, q075 = q075_dur, assumption, scenario) %>% 
  mutate(parameter = "SW duration (years)")

FSWchar_assumptions = FSWchar_constant_age %>% 
  bind_rows(FSWchar_constant_dur) %>% 
  bind_rows(FSWchar_timevar_age) %>% 
  bind_rows(FSWchar_timevar_dur)


transm_assumps_const = transm_assumps_const %>% mutate(assumption = "Time-invariant (1: constant)", 
                                                       parameter = "Transmission risk") %>% 
  rename(q025_tr = q025, q075_tr = q075)

transm_assumps_exp = transm_assumps_exp %>% mutate(assumption = "Time-varying (2: exponentially declining)", 
                                                   parameter = "Transmission risk") 

transm_assumps_dyn = transm_assumps_dyn %>% mutate(assumption = "Time-varying (3: dynamically changing)", 
                                                   parameter = "Transmission risk") 

Trans_assumptions = transm_assumps_const %>% 
  bind_rows(transm_assumps_exp) %>% 
  bind_rows(transm_assumps_dyn)

datalist = c(
  "data_tot2", 
  "data_models_30y",
  "data_reshandjaff4",
  "ANC_prev_data_tot", 
  "HIVprevData", 
  "FSW_prev_data", 
  "IRR_agematched",
  "PAF_CSW",
  "PAF_clients",
  "PAF_fsw",
  "relPAF",
  "data_prev_models", 
  "data_tot2_sens", 
  "modelparameters_summary", 
  "FSWchar_assumptions",
  "Trans_assumptions")
  
  
rm(list=setdiff(ls(), datalist))


