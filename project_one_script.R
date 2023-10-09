# Packages Used

library(tidyverse)
library(ggplot2)
library(gtsummary)
library(kableExtra)
library(ghibli)
library(gridExtra)

# Load in the data
# Data has not been provided to protect privacy.
data <- read.csv('../Projects/project_1.csv')

# Data Pre-Processing
# (1) Turn race columns from dummy-coded into single race column.
#     Note: in cases where there are two races checked, store them as biracial.
# (2) Convert sex, ethnicity, employment, and education columns from numbers
#     into the actual values (taken from the data codebook).
# (3) For income value 250,000 remove the extra space and column.
#     Convert income column to numeric.
# (4) Convert smoking column values to 1 or 0 for smoking or no smoking.
# (5) Create new column for smoking level at 34 weeks gestation period and six
#     months postpartum. The smoking level is calculated based on the urine
#     cotinine values. The cut-off values for each level have been taken from
#     The Transplantation Journal. See citations for more details.
data <- data %>%
  mutate(parent_race = case_when(paian == 1 & pwhite == 1 | 
                                   paian == 1 & pnhpi == 1 ~ 'Biracial',
                                 paian == 1 ~ 'American Indian/Alaska Native',
                                 pasian == 1 ~ 'Asian',
                                 pnhpi == 1 ~ 'Native Hawaiian/Pacific Islander',
                                 pblack == 1 ~ 'Black',
                                 pwhite == 1 ~ 'White',
                                 prace_other == 1 ~ 'Other')) %>%
  mutate(parent_sex = case_when(psex == 1 ~ 'F',
                                psex == 0 ~ 'M',
                                TRUE ~ NA)) %>%
  mutate(ethnic = case_when(pethnic == 1 ~ 'Yes',
                            pethnic == 0 ~ 'No',
                            TRUE ~ NA)) %>%
  mutate(employed = case_when(employ == 0 ~ 'No',
                              employ == 1 ~ 'Part-Time',
                              employ == 2 ~ 'Full-Time',
                              TRUE ~ NA)) %>%
  mutate(education = case_when(pedu == 0 ~ 'Some High School',
                               pedu == 1 ~ 'High School',
                               pedu == 2 ~ 'GED',
                               pedu == 3 ~ 'Some College',
                               pedu == 4 ~ '2-Year Degree',
                               pedu == 5 ~ '4-Year Degree',
                               pedu == 6 ~ 'Post-Graduate Degree')) %>%
  mutate(income = case_when(income == '250, 000' ~ '250000', 
                            TRUE ~ income)) %>%
  mutate(income = as.numeric(income)) %>%
  mutate(mom_smoke_16wk = case_when(mom_smoke_16wk=='1=Yes' ~ 1, 
                                    mom_smoke_16wk=='2=No' ~ 0),
         mom_smoke_22wk = case_when(mom_smoke_22wk=='1=Yes' ~ 1,
                                    mom_smoke_22wk=='2=No' ~ 0),
         mom_smoke_32wk = case_when(mom_smoke_32wk=='1=Yes' ~ 1, 
                                    mom_smoke_32wk=='2=No' ~ 0),
         mom_smoke_pp1 = case_when(mom_smoke_pp1=='1=Yes' ~ 1, 
                                   mom_smoke_pp1=='2=No' ~ 0),
         mom_smoke_pp2 = case_when(mom_smoke_pp2=='1=Yes' ~ 1, 
                                   mom_smoke_pp2=='2=No' ~ 0),
         mom_smoke_pp12wk = case_when(mom_smoke_pp12wk=='1=Yes' ~ 1, 
                                      mom_smoke_pp12wk=='2=No' ~ 0),
         mom_smoke_pp6mo = case_when(mom_smoke_pp6mo=='1=Yes' ~ 1, 
                                     mom_smoke_pp6mo=='2=No' ~ 0)) %>%
  mutate(smoking_level_34wk = case_when(is.na(cotimean_34wk) ~ NA,
                                        cotimean_34wk < 100 ~ 'nonsmoker',
                                        cotimean_34wk < 500 ~ 'passive',
                                        TRUE ~ 'active')) %>%
  mutate(smoking_level_pp6mo = case_when(is.na(cotimean_pp6mo) ~ NA,
                                         cotimean_pp6mo < 100 ~ 'nonsmoker',
                                         cotimean_pp6mo < 500 ~ 'passive',
                                         TRUE ~ 'active smoker'))

# Creating a race lookup table for the parent
race_lookup <- data %>%
  gather(key = 'race', value = 'value', 
         c(paian, pasian, pblack, pwhite, pnhpi, prace_other)) %>%
  filter(!is.na(value)) %>%
  filter(value == 1) %>%
  select(parent_id, race)


# The SWAN scores for these individuals should be NA.
# In the data pre-processing, they became 0. Changing them back to NA.
parent_ids <- c(50502, 51202, 51602, 52302, 53002, 53502, 53902, 54402, 54602, 54702)

for (id in parent_ids) {
  data[data$parent_id == id,]['swan_inattentive'] = NA
  data[data$parent_id == id,]['swan_hyperactive'] = NA
}



# Data Manipulation/Tables
data %>%
  mutate(page = as.numeric(page)) %>%
  select(page, parent_sex, parent_race, ethnic) %>%
  tbl_summary(type = page ~ 'continuous',
              label = list(page ~ 'Age',
                           parent_sex ~ 'Sex',
                           parent_race ~ 'Race',
                           ethnic ~ 'Hispanic/Latino'),
              sort = list(everything() ~ 'frequency')) %>%
  as_gt() %>%
  gt::tab_header(title = 'Table 1: Data Demographics')

# Financial Table
data %>%
  select(employed, education, income) %>%
  tbl_summary(type = income ~ 'continuous',
              label = list(employed ~ 'Employed',
                           education ~ 'Highest Level of Education',
                           income ~ 'Annual Income'),
              sort = list(everything() ~ 'frequency')) %>%
  as_gt() %>%
  gt::tab_header(title = 'Table 2: Education & Financial Status')

# Pivot the data to get whether or not the mother smoked during each trimester.
# 16 weeks represents trimester 1, 22 weeks represents trimester 2, and 32
# weeks represents trimester 3. This is used to create the trimester column.
smoking_trimester <- data %>%
  gather(key = 'time', value = 'smoking', 
         c(mom_smoke_16wk, mom_smoke_22wk, mom_smoke_32wk)) %>%
  mutate(trimester = case_when(time == 'mom_smoke_16wk' ~ 1,
                               time == 'mom_smoke_22wk' ~ 2,
                               time == 'mom_smoke_32wk' ~ 3))


# Smoking during pregnancy
# If they smoked at any point during pregnancy, set to SDP. 
# If not, set to No SDP.
sdp <- smoking_trimester %>%
  filter(!is.na(smoking)) %>%
  mutate(sdp = case_when(smoking == 1 ~ 'SDP',
                         TRUE ~ 'No SDP'))

# Smoking through pregnancy
# If they smoked at all points during pregnancy, set to 1.
# If not, set to 1.
# Get the percentage of individuals who smoked throughout pregnancy divided by
# the number of individuals who smoked during pregnancy.
percentage_stp <- sdp %>%
  filter(sdp == 'SDP') %>%
  group_by(parent_id) %>%
  summarize(stp = case_when(sum(smoking, na.rm=TRUE) == 3 ~ 1,
                            TRUE ~ 0)) %>%
  summarize(perc = sum(stp)/n())

# Pivot the data to get whether or not the mother smoked during each postpartum
# visit.
post_partum_smoking <- data %>%
  gather(key = 'time', value = 'smoking',
         c(mom_smoke_pp1, mom_smoke_pp2, mom_smoke_pp12wk, mom_smoke_pp6mo)) %>%
  mutate(pp_visit = case_when(time == 'mom_smoke_pp1' ~ 1,
                              time == 'mom_smoke_pp2' ~ 2,
                              time == 'mom_smoke_pp12wk' ~ 3,
                              time == 'mom_smoke_pp6mo' ~ 4))

# Get the number of smokers at each trimester stratified by race.
smoking_by_race <- smoking_trimester %>%
  select(parent_id, trimester, smoking) %>%
  left_join(race_lookup, by = 'parent_id', relationship='many-to-many') %>%
  group_by(trimester, race) %>%
  summarize(count = sum(smoking, na.rm = TRUE), .groups = 'drop_last') %>%
  filter(count > 0)

# Get the number of smokers at each postpartum visit stratified by race.
pp_smoking_by_race <- post_partum_smoking %>%
  select(parent_id, pp_visit, smoking) %>%
  left_join(race_lookup, by = 'parent_id', relationship='many-to-many') %>%
  group_by(pp_visit, race) %>%
  summarize(count = sum(smoking, na.rm = TRUE), .groups = 'drop_last') %>%
  filter(count > 0)

# Pivot the data to get values for smoke exposure at each time point postpartum.
# This data comes from the newer study and includes time periods at 6 months,
# 12 months, 2 years, 3 years, 4 years, and 5 years.
smoke_exp <- data %>%
  gather(key = 'time', value = 'smoke_exposure', c(smoke_exposure_6mo, smoke_exposure_12mo,
                                                   smoke_exposure_2yr, smoke_exposure_3yr,
                                                   smoke_exposure_4yr, smoke_exposure_5yr)) %>%
  select(parent_id, time, smoke_exposure) %>%
  mutate(time = substring(time, 16))

# Get the number of self-reported smoke exposures at each time point
# stratified by race.
smoke_exp_over_time <- smoke_exp %>%
  left_join(race_lookup, by = 'parent_id', relationship='many-to-many') %>%
  filter(!is.na(smoke_exposure)) %>%
  group_by(race, time) %>%
  summarize(count = sum(smoke_exposure, na.rm = TRUE), .groups = 'drop_last') %>%
  filter(count > 0)

# Show the number of individuals that fall into each of the smoking levels at both time periods that urine cotinine levels are provided for.
data %>%
  select(smoking_level_34wk, smoking_level_pp6mo) %>%
  tbl_summary(label = list('smoking_level_34wk' ~ 'Smoking Level (34 Week Gestation)',
                           'smoking_level_pp6mo' ~ 'Smoking Level (6 Months Postpartum)')) %>%
  as_gt() %>%
  gt::tab_header(title = 'Table 3: Smoking Level by Urine Cotinine Pre- and Post- Pregnancy')

# Get the percentage of child substance use each substance contributes to based
# on the smoking level of the parent at 34 weeks gestation.
substance_use_34wk <- data %>%
  mutate(smoking_level = smoking_level_34wk) %>%
  group_by(smoking_level) %>%
  filter(!is.na(smoking_level)) %>%
  mutate(substance_use = ifelse(e_cig_ever == 1 | cig_ever == 1 | alc_ever == 1 | mj_ever == 1, 1, 0)) %>%
  summarize(sub_use_perc = sum(substance_use, na.rm=TRUE)/n() * 100) %>%
  mutate(time = '34 weeks') %>%
  select(smoking_level, time, sub_use_perc)

# Get the percentage of child substance use each substance contributes to based
# on the smoking level of the parent at 6 months postpartum.
# Join it with the results from 34 weeks gestation.
substance_use_total <- data %>%
  mutate(smoking_level = smoking_level_pp6mo) %>%
  group_by(smoking_level) %>%
  filter(!is.na(smoking_level)) %>%
  mutate(substance_use = ifelse(e_cig_ever == 1 | cig_ever == 1 | alc_ever == 1 | mj_ever == 1, 1, 0)) %>%
  summarize(sub_use_perc = sum(substance_use, na.rm=TRUE)/n() * 100) %>%
  mutate(time = '6 months postpartum') %>%
  select(smoking_level, time, sub_use_perc) %>%
  rbind(substance_use_34wk) %>%
  arrange(desc(smoking_level), time)

# Get the rate of total substance use by medium.
# Filter to include only individuals who have self-reported substance use and
# get the rate of use among each medium.
substance_use <- data %>%
  filter(e_cig_ever == 1 | cig_ever == 1 | alc_ever == 1 | mj_ever == 1) %>%
  select(e_cig_ever, mj_ever, alc_ever, cig_ever) %>%
  summarize(e_cig = sum(e_cig_ever, na.rm=TRUE)/n(),
            mj = sum(mj_ever, na.rm=TRUE)/n(),
            alc = sum(alc_ever, na.rm=TRUE)/n(),
            cig = sum(cig_ever, na.rm=TRUE)/n())

# Get the average number of days (out of the past 30 days) that children who
# have engaged in substance use have used a substance, organized by substance.
# Join this with a vector of substance names using cbind for cleanliness.
sub_use_30 <- data %>%
  filter(e_cig_ever == 1 | cig_ever == 1 | alc_ever == 1 | mj_ever == 1) %>%
  select(num_cigs_30, num_e_cigs_30, num_alc_30, num_mj_30) %>%
  summarize(avg_e_cigs = mean(num_e_cigs_30, na.rm=TRUE),
            avg_mj = mean(num_mj_30, na.rm=TRUE),
            avg_alc = mean(num_alc_30, na.rm=TRUE),
            avg_cigs = mean(num_cigs_30, na.rm=TRUE)) %>%
  t() %>%
  as.data.frame() %>%
  mutate(days_used = V1) %>%
  cbind(substance = c('E Cigarette', 'Marijuana', 'Alcohol', 'Cigarette')) %>%
  select(substance, days_used)

# Combine the two tables together to display using kable.
substance_use <- substance_use %>% 
  t() %>%
  as.data.frame() %>%
  mutate(percentage = V1 * 100) %>%
  select(percentage) %>%
  cbind(substance = c('E Cigarette', 'Marijuana', 'Alcohol', 'Cigarette')) %>%
  select(substance, percentage) %>%
  left_join(sub_use_30, by = "substance")

# Create a separate column to store the races of the children along with the SDP
# effects. There are two categories of biracial children (AIAN/Black and Black/
# White) so I just defined these as biracial.
sdp_race <- sdp %>%
  mutate(child_race = case_when(taian == 1 & tblack == 'white' |
                                  twhite == 1 & tblack == 1~ 'biracial',
                                taian == 1 ~ 'aian',
                                tasian == 1 ~ 'asian',
                                tnhpi == 1 ~ 'nhpi',
                                tblack == 1 ~ 'black',
                                twhite == 1 ~ 'white',
                                trace_other == 1 ~ 'other'))

# For each category of SDP and Race, calculate the average ERQ scores for
# expressive suppression and cognitive reappraisal.
erq <- sdp_race %>%
  filter(!is.na(child_race)) %>%
  group_by(sdp, child_race) %>%
  summarize(cog = mean(erq_cog, na.rm=TRUE),
            exp = mean(erq_exp, na.rm=TRUE), .groups = 'drop_last') %>%
  gather(key = 'erq', value = 'value', c(cog, exp))

# For each category of SDP and Race, calculate the average BPM scores for
# attention, externalizing, and internalizing.
bpm <- sdp_race %>%
  filter(!is.na(child_race)) %>%
  group_by(sdp, child_race) %>%
  summarize(att = mean(bpm_att, na.rm=TRUE),
            ext = mean(bpm_ext, na.rm=TRUE),
            int = mean(bpm_int, na.rm=TRUE), .groups = 'drop_last') %>%
  gather(key = 'bpm', value = 'value', c(att,ext,int))
