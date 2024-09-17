# Project: Election Admin
# Author: NaLette Brodnax
# Sequence: 6
# Description: Multilevel model estimation for residual registration rate

library(tidyverse)
library(lme4)
library(stargazer)
lmerControl(optimizer = 'bobyqa', optCtrl = list(maxfun=2e5))

admin_vot <- read.csv("out/admin_vot.csv", header = TRUE, na.strings = "",
                      colClasses = c(rep("character", 5), rep("numeric", 106))) %>% 
  mutate(t_vap = ifelse(turnout > adult_pop|adult_pop == 0,
                                 NA, (adult_pop - turnout)/adult_pop),
         t_vep = ifelse(turnout > voting_pop_est|voting_pop_est == 0,
                        NA, (voting_pop_est - turnout)/voting_pop_est),
         t_rvp = resid_out_rate)

# of interest
# VAP = adult_pop
# VEP = voting_pop_est
# RVP = total_voters
# T = turnout

data_scaled <- admin_vot %>% 
  select(year, fips, countyname, statefp, state, felony_str_mod, same_day_reg,
         # info_standard, info_other, info_ssn, info_address, 
         pct_purged, website, strict_id, photo_id, all_mail, early_inperson, 
         join_abs_list,  high_density, counts_pb, hava_spend, rep_control, 
         dem_control, 
         # cost_100_less, 
         pct_highschool, pct_unemployed, 
         ln_median_income, median_age, pct_female, pct_black, pct_latino, 
         vote_margin, x_resid_reg_rate, x_resid_out_rate, x_resid_bal_rate, 
         resid_reg_rate, resid_out_rate, resid_bal_rate, resid_vot_rate,
         t_vap, t_vep, t_rvp) %>% 
  mutate_at(c("pct_purged", "hava_spend", "pct_highschool", "pct_unemployed",
              "ln_median_income", "pct_female", "median_age", "pct_black",
              "pct_latino", "vote_margin", "x_resid_reg_rate", 
              "x_resid_out_rate", "x_resid_bal_rate"), scale) %>% 
  filter(!is.infinite(t_vap), !is.infinite(t_vep))

# build model formulas
rhs <- data_scaled %>% 
  select(-c(year, fips, countyname, statefp, state, vote_margin, x_resid_reg_rate, 
            x_resid_out_rate, x_resid_bal_rate, resid_reg_rate, resid_out_rate,
            resid_bal_rate, resid_vot_rate, t_vap, t_vep, t_rvp)) %>% 
  names()
interactions <- c("felony_str_mod*pct_black","high_density*pct_black")
random_intercepts <- c("(1|statefp/fips)", "(1|year)")
add_terms <- paste(c(interactions, random_intercepts), collapse = " + ")

# removing vote_margin as post-treatment
# reverse decision by adding `outcomes[x], " + ",` to update(paste())
# outcomes <- list(NULL,
#                  NULL,
#                  "x_resid_reg_rate",
#                  "x_resid_reg_rate + x_resid_out_rate",
#                  "x_resid_reg_rate + x_resid_out_rate + x_resid_bal_rate")

# models <- c("resid_reg_rate", "resid_out_rate", "resid_bal_rate", "resid_vot_rate") %>% 
models <- c("t_vap", "t_vep", "t_rvp") %>% 
  lapply(function(y) formula(paste(y, "~", paste(rhs, collapse = " + ")))) %>% 
  lapply(mod=., seq(1:3), function(x, mod) update(formula(mod[[x]]), 
                                                  paste("~ . + ", add_terms))) %>% 
  lapply(function(model) lmer(model, data = data_scaled))

# check fits
all_opt <- allFit(models[[2]])
all_summary <- summary(all_opt)
all_summary$fixef
all_summary$llik
all_summary$sdcor
all_summary$theta
all_summary$which.OK

ranef_sd <- models %>% 
  lapply(function(model) VarCorr(model)) %>% 
  lapply(function(r) as.data.frame(r)) 

estimates <- stargazer(models, align = TRUE, type = "text", out.header = TRUE,
                       out = 'out/simulation_estimates.tex', 
                       title = 'Turnout Estimates by Population',
                       # dep.var.labels = depvars,
                       # covariate.labels = indvars,
                       float = TRUE, float.env = "table", header = FALSE,
                       intercept.top = TRUE, intercept.bottom = FALSE,
                       model.numbers = FALSE, no.space = TRUE, 
                       # order = c(1, 15:19, 11:14, 4:10, 2:3, 20:26),
                       dep.var.caption = "Residual Rate:", keep.stat = c('n'),
                       single.row = TRUE, report = 'vc*', digits = 5,
                       column.sep.width = '-20pt')

# correction
sim_data <- data_scaled %>% 
  select(t_vap, t_vep, t_rvp, x = strict_id, z1 = felony_str_mod,
         z2 = pct_purged, pct_highschool, pct_unemployed, ln_median_income,
         median_age, pct_female, pct_black, pct_latino, year, fips, statefp,
         x_resid_reg_rate,)

sim_rhs <- sim_data %>% 
  select(-starts_with('t_'), -c(z1, z2, year, fips, statefp)) %>% 
  names()

random_intercepts <- c("(1|statefp/fips)", "(1|year)")
add_terms <- paste(random_intercepts, collapse = " + ")

sims <- c("t_vap", "t_vep", "t_rvp") %>% 
  lapply(function(y) formula(paste(y, "~", paste(sim_rhs, collapse = " + ")))) %>% 
  lapply(mod=., seq(1:3), function(x, mod) update(formula(mod[[x]]), 
                                                  paste("~ . + ", add_terms))) %>% 
  lapply(function(model) lmer(model, data = sim_data))

stargazer(sims, align = TRUE, type = "text")

