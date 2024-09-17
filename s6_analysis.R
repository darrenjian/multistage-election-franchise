# Project: Election Admin
# Author: NaLette Brodnax
# Sequence: 6
# Description: Multilevel model estimation for residual registration rate

library(tidyverse)
library(lme4)
library(stargazer)

admin <- read.csv("out/election_admin.csv", header = TRUE, na.strings = "",
                  colClasses = c(rep("character", 5), rep("numeric", 85)))

# validation of population measures; alternative voting population (not used)
population <- admin %>% 
  filter(year == "2016") %>% 
  select(fips, countyname, state, adult_pop, citizen_pop, population,
         voting_pop) %>% 
  mutate(pct_citizen = citizen_pop/population,
         pct_adult = adult_pop/population,
         voting_pop_est = citizen_pop*pct_adult,
         voting_pop_error = abs(voting_pop - voting_pop_est)/voting_pop)

# use 2016 voting population as pct of adult population to impute prior years
voting_pop_factor <- population %>% 
  select(fips, adult_pop, voting_pop) %>% 
  transmute(fips = fips, voting_pop_factor = voting_pop/adult_pop)

# assess missingness
missing <- admin %>% 
  group_by(year) %>% 
  mutate_all(is.na) %>% 
  mutate_if(is.logical, as.integer) %>% 
  summarise_all(sum) %>% 
  gather(variable, missing, -year) %>% 
  spread(year, missing) %>% 
  group_by(variable) 

# computer variables
admin_vot <- admin %>% 
  left_join(voting_pop_factor, by = "fips") %>% 
  mutate(pct_female = female_pop/(female_pop + male_pop),
         ln_median_income = log(median_income),
         ln_population = log(population),
         pct_nonwhite = 1 - pct_white,
         voting_pop_est = ifelse(year == "2016", voting_pop, 
                                 as.integer(adult_pop*voting_pop_factor)),
         pct_purged = ifelse(is.na(purged), 0, purged/voting_pop_est),
         resid_reg = ifelse(voting_pop_est > total_voters, 
                            voting_pop_est - total_voters, 0),
         resid_reg_rate = (resid_reg/adult_pop),
         no_grace = 1 - reg_grace,
         felony_str_mod = felony_strict + felony_moderate,
         same_day_reg = ifelse(days_reg_inperson == 0, 1, 0),
         resid_out_rate = ifelse(turnout > total_voters|total_voters == 0,
                                 0, (total_voters - turnout)/total_voters),
         resid_bal_rate = ifelse(is.na(prov_submit)|turnout == 0, 
                                 0, prov_submit/turnout),
         prov_rej = ifelse(is.na(prov_rej), 0, prov_rej),
         abs_rej = ifelse(is.na(abs_rej), 0, abs_rej),
         dca_rej = ifelse(is.na(dca_rej), 0, dca_rej),
         rej_ballots = prov_rej,
         resid_vot_rate = ifelse(turnout <= rej_ballots, 0, 
                                 rej_ballots/turnout),
         eday_office = ifelse(is.na(eday_office), 0, eday_office),
         poll_count = eday_polls + eday_office,
         x_resid_reg_rate = resid_reg_rate, 
         x_resid_out_rate = resid_out_rate,
         x_resid_bal_rate = resid_bal_rate,
         poll_density = total_voters/poll_count,
         high_density = ifelse(poll_density > 1000, 1, 0),
         high_density = ifelse(is.na(high_density), 0, high_density))

write_csv(admin_vot, path = 'out/admin_vot.csv', na = '')

# continuous variables scaled and centered for multilevel model
data_scaled <- admin_vot %>% 
  select(year, fips, countyname, statefp, state, felony_str_mod, same_day_reg,
         pct_purged,  website, # info_standard, info_other, info_ssn, info_address, 
         all_mail, early_inperson, join_abs_list, strict_id, photo_id,  
         counts_pb, high_density, hava_spend, cost_100_less, rep_control, 
         dem_control, pct_highschool, pct_unemployed, ln_median_income, 
         median_age, pct_female, pct_black, pct_latino, vote_margin, 
         x_resid_reg_rate, x_resid_out_rate, x_resid_bal_rate, resid_reg_rate, 
         resid_out_rate, resid_bal_rate, resid_vot_rate) %>% 
  mutate_at(c("pct_purged", "hava_spend", "pct_highschool", "pct_unemployed",
              "ln_median_income", "pct_female", "median_age", "pct_black",
              "pct_latino", "vote_margin", "x_resid_reg_rate", 
              "x_resid_out_rate", "x_resid_bal_rate"), scale)

# build model formulas
rhs <- data_scaled %>% 
  select(-c(year, fips, countyname, statefp, state, vote_margin, x_resid_reg_rate, 
            x_resid_out_rate, x_resid_bal_rate, resid_reg_rate, resid_out_rate,
            resid_bal_rate, resid_vot_rate)) %>% 
  names()
#interactions <- c("felony_str_mod*pct_black","high_density*pct_black")
interactions <- c("felony_str_mod*pct_black")
random_intercepts <- c("(1|statefp/fips)", "(1|year)")
add_terms <- paste(c(interactions, random_intercepts), collapse = " + ")
outcomes <- list(NULL, "x_resid_reg_rate", 
                 "x_resid_reg_rate + x_resid_out_rate", 
                 "x_resid_reg_rate + x_resid_out_rate + x_resid_bal_rate")

models <- c("resid_reg_rate", "resid_out_rate", "resid_bal_rate", "resid_vot_rate") %>% 
  lapply(function(y) formula(paste(y, "~", paste(rhs, collapse = " + ")))) %>% 
  lapply(mod=., seq(1:4), function(x, mod) update(formula(mod[[x]]), 
                                                  paste("~ . + ", outcomes[x], 
                                                        " + ", add_terms))) %>% 
  lapply(function(model) lmer(model, data = data_scaled))

ranef_sd <- models %>% 
  lapply(function(model) VarCorr(model)) %>% 
  lapply(function(r) as.data.frame(r)) 

depvars <- c("Registration", "Turnout", "Balloting", "Counting")
indvars <- c("Constant", "Felon Voting", "Felon Voting*\\% Black", 
             "Same Day Registration", "Pct Voters Purged", "Registration Website", 
             # "Website: Name, DOB, DL", "Website: Other", 
             # "Website: SSN", "Website: Address", 
             "All-Mail Voting", "Early Voting", "Absentee List Voting", 
             "Strict ID Law", "Photo ID Required", 
             "Provisional Balloting", 
             "High Poll Density", "HAVA Spending", "Low Cost Voter Data", 
             "Republican Majority", "Democratic Majority", "\\% Highschool Grad", 
             "\\% Unemployed", "Ln Median Income", "Median Age", "\\% Women", 
             "\\% Black", "\\% Latino", 
              #"High Density*Pct Black",
             "Residual Registr. Loss", "Residual Turnout Loss", "Residual Ballot Loss")
estimates <- stargazer(models, align = TRUE, type = "text", out.header = FALSE,
                       out = 'out/multistage_results.tex', 
                       title = 'Linear Mixed Effects Regression Results',
                       dep.var.labels = depvars,
                       covariate.labels = indvars,
                       float = TRUE, float.env = "table", header = FALSE,
                       intercept.top = TRUE, intercept.bottom = FALSE,
                       model.numbers = FALSE, no.space = TRUE, 
                       order = c(1, 2, 27, 3:23, 24:26),
                       dep.var.caption = "Residual Loss:", keep.stat = c('n'),
                       single.row = TRUE, report = 'vc*', digits = 5,
                       column.sep.width = '-20pt')
