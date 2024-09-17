# Project: Election Admin
# Author: NaLette Brodnax
# Sequence: 8
# Description: Create dataset for replication via dataverse

library(tidyverse)
library(stargazer)
library(lme4)
lmerControl(optimizer = 'bobyqa', optCtrl = list(maxfun=2e5))

source_dir = 'out/'

admin_vot <- read.csv(paste(source_dir, 'admin_vot.csv', sep = ''),
                      header = TRUE, na.strings = "",
                      colClasses = c(rep("character", 5), rep("numeric", 106))) %>% 
  mutate(reg_rate = 1 - resid_reg_rate, x_reg_rate = 1 - x_resid_reg_rate,
         out_rate = 1 - resid_out_rate, x_out_rate = 1 - x_resid_out_rate,
         bal_rate = 1 - resid_bal_rate, x_bal_rate = 1 - x_resid_bal_rate,
         vot_rate = 1 - resid_vot_rate) %>% 
  select(year, fips, countyname, statefp, state, felony_str_mod, same_day_reg,
         pct_purged,  website,all_mail, early_inperson, join_abs_list, strict_id, 
         photo_id, counts_pb, high_density, hava_spend, cost_100_less, rep_control, 
         dem_control, pct_highschool, pct_unemployed, ln_median_income, 
         median_age, pct_female, pct_black, pct_latino, x_reg_rate, x_out_rate, 
         x_bal_rate, reg_rate, out_rate, bal_rate, vot_rate)

write_csv(admin_vot, paste(source_dir, 'election_admin_data.csv', sep = ''))
