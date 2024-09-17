# Project: Election Admin
# Author: Darren Jian
# Sequence: 7
# Description: Turnout simulation for EAC, Leip, CPS
# Input: admin_vot.csv, admin_vot.leip.csv, admin_vot.adj,csv (from s6_analysis.leip)
# Output: simulation_estimates.tex, simulation_estimates.leip.tex, simulation_estimates.adj.tex

library(tidyverse)
library(lme4)
library(stargazer)
#source('s6_analysis.leip.Rmd')
lmerControl(optimizer = 'bobyqa', optCtrl = list(maxfun=2e5))
admin_vot_t <- read.csv("out/admin_vot.csv", header = TRUE, na.strings = "",
                        colClasses = c(rep("character", 5), rep("numeric", 106))
) %>% 
  mutate(t_vap = ifelse(turnout > adult_pop|adult_pop == 0,
                        NA, 1 - (adult_pop - turnout)/adult_pop),
         t_vep = ifelse(turnout > voting_pop_est|voting_pop_est == 0,
                        NA, 1 - (voting_pop_est - turnout)/voting_pop_est),
         t_rvp = out_rate)


admin_vot_t.leip <- read.csv("out/admin_vot.leip.csv"
                             , header = TRUE
                             , na.strings = "",
                             #colClasses = c(rep("character", 5)
                             #               , rep("numeric", 106)
                             #               )
) 
admin_vot_t.leip$statefp<-as.factor(admin_vot.leip$statefp)
admin_vot_t.leip$year<-as.factor(admin_vot.leip$year)
admin_vot_t.leip$fips<-as.factor(admin_vot.leip$fips)
admin_vot_t.leip$countyname<-as.factor(admin_vot.leip$countyname)
admin_vot_t.leip$Geographic.Subtype<-as.factor(admin_vot.leip$Geographic.Subtype)
admin_vot_t.leip$state<-as.factor(admin_vot.leip$state)
str(admin_vot_t.leip)
admin_vot_t.leip<- admin_vot_t.leip %>% 
  mutate(t_vap = ifelse(turnout > adult_pop|adult_pop == 0,
                        NA, 1 - (adult_pop - turnout)/adult_pop),
         t_vep = ifelse(turnout > voting_pop_est|voting_pop_est == 0,
                        NA, 1 - (voting_pop_est - turnout)/voting_pop_est),
         t_rvp = out_rate)


admin_vot_t.adj <- read.csv("out/admin_vot.adj.csv"
                            , header = TRUE
                            , na.strings = "",
                            #colClasses = c(rep("character", 5)
                            #               , rep("numeric", 106)
                            #               )
) 
admin_vot_t.adj$statefp<-as.factor(admin_vot.adj$statefp)
admin_vot_t.adj$year<-as.factor(admin_vot.adj$year)
admin_vot_t.adj$fips<-as.factor(admin_vot.adj$fips)
admin_vot_t.adj$countyname<-as.factor(admin_vot.adj$countyname)
admin_vot_t.adj$Geographic.Subtype<-as.factor(admin_vot.adj$Geographic.Subtype)
admin_vot_t.adj$state<-as.factor(admin_vot.adj$state)
str(admin_vot_t.adj)
admin_vot_t.adj<- admin_vot_t.adj %>% 
  mutate(t_vap = ifelse(turnout > adult_pop|adult_pop == 0,
                        NA, 1 - (adult_pop - turnout)/adult_pop),
         t_vep = ifelse(turnout > voting_pop_est|voting_pop_est == 0,
                        NA, 1 - (voting_pop_est - turnout)/voting_pop_est),
         t_rvp = out_rate)


# of interest
# VAP = adult_pop
# VEP = voting_pop_est
# RVP = total_voters
# T = turnout
admin_vot_t$x_reg_rate<-as.numeric(admin_vot_t$x_reg_rate)
admin_vot_t$x_out_rate<-as.numeric(admin_vot_t$x_out_rate)
data_scaled_t <- admin_vot_t %>% 
  select(year, fips, countyname, statefp, state, felony_str_mod, same_day_reg,
         # info_standard, info_other, info_ssn, info_address, 
         pct_purged, website, strict_id, photo_id, all_mail, early_inperson, 
         join_abs_list,  high_density, counts_pb, hava_spend, rep_control, 
         dem_control, 
         # cost_100_less, 
         pct_highschool, pct_unemployed, 
         ln_median_income, median_age, pct_female, pct_black, pct_latino, 
         vote_margin, x_reg_rate, x_out_rate, x_bal_rate, 
         reg_rate, out_rate, bal_rate, vot_rate,
         t_vap, t_vep, t_rvp) %>%
  mutate_at(c("pct_purged", "hava_spend", "pct_highschool", "pct_unemployed",
              "ln_median_income", "pct_female", "median_age", "pct_black",
              "pct_latino", "vote_margin", "x_reg_rate",
              "x_out_rate", "x_bal_rate"), scale)
# %>% 
#   filter(!is.infinite(t_vap), !is.infinite(t_vep))


admin_vot_t.leip$x_reg_rate<-as.numeric(admin_vot_t.leip$x_reg_rate)
admin_vot_t.leip$x_out_rate<-as.numeric(admin_vot_t.leip$x_out_rate)
data_scaled_t.leip <- admin_vot_t.leip %>% 
  select(year, fips, countyname, statefp, state, felony_str_mod, same_day_reg,
         # info_standard, info_other, info_ssn, info_address, 
         pct_purged, website, strict_id, photo_id, all_mail, early_inperson, 
         join_abs_list,  high_density, counts_pb, hava_spend, rep_control, 
         dem_control, 
         # cost_100_less, 
         pct_highschool, pct_unemployed, 
         ln_median_income, median_age, pct_female, pct_black, pct_latino, 
         vote_margin, x_reg_rate, x_out_rate, x_bal_rate, 
         reg_rate, out_rate, bal_rate, vot_rate,
         t_vap, t_vep, t_rvp) %>% 
  mutate_at(c("pct_purged", "hava_spend", "pct_highschool", "pct_unemployed",
              "ln_median_income", "pct_female", "median_age", "pct_black",
              "pct_latino", "vote_margin", "x_reg_rate", 
              "x_out_rate", "x_bal_rate"), scale) %>% 
  filter(!is.infinite(t_vap), !is.infinite(t_vep))


admin_vot_t.adj$x_reg_rate<-as.numeric(admin_vot_t.adj$x_reg_rate)
admin_vot_t.adj$x_out_rate<-as.numeric(admin_vot_t.adj$x_out_rate)
data_scaled_t.adj <- admin_vot_t.adj %>% 
  select(year, fips, countyname, statefp, state, felony_str_mod, same_day_reg,
         # info_standard, info_other, info_ssn, info_address, 
         pct_purged, website, strict_id, photo_id, all_mail, early_inperson, 
         join_abs_list,  high_density, counts_pb, hava_spend, rep_control, 
         dem_control, 
         # cost_100_less, 
         pct_highschool, pct_unemployed, 
         ln_median_income, median_age, pct_female, pct_black, pct_latino, 
         vote_margin, x_reg_rate, x_out_rate, x_bal_rate, 
         reg_rate, out_rate, bal_rate, vot_rate,
         t_vap, t_vep, t_rvp) %>% 
  mutate_at(c("pct_purged", "hava_spend", "pct_highschool", "pct_unemployed",
              "ln_median_income", "pct_female", "median_age", "pct_black",
              "pct_latino", "vote_margin", "x_reg_rate", 
              "x_out_rate", "x_bal_rate"), scale) %>% 
  filter(!is.infinite(t_vap), !is.infinite(t_vep))

# build model formulas
rhs_t <- data_scaled_t %>% 
  select(-c(year, fips, countyname, statefp, state, vote_margin, x_reg_rate, 
            x_out_rate, x_bal_rate, reg_rate, out_rate,
            bal_rate, vot_rate, t_vap, t_vep, t_rvp)) %>% 
  names()


rhs_t.leip <- data_scaled_t.leip %>% 
  select(-c(year, fips, countyname, statefp, state, vote_margin, x_reg_rate, 
            x_out_rate, x_bal_rate, reg_rate, out_rate,
            bal_rate, vot_rate, t_vap, t_vep, t_rvp)) %>% 
  names()


rhs_t.adj <- data_scaled_t.adj %>% 
  select(-c(year, fips, countyname, statefp, state, vote_margin, x_reg_rate, 
            x_out_rate, x_bal_rate, reg_rate, out_rate,
            bal_rate, vot_rate, t_vap, t_vep, t_rvp)) %>% 
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

data_scaled_t$t_rvp<-as.numeric(data_scaled_t$t_rvp)
models_t <- c("t_vap", "t_vep", "t_rvp") %>% 
  lapply(function(y) formula(paste(y, "~", paste(rhs_t, collapse = " + ")))) %>% 
  lapply(mod=., seq(1:3), function(x, mod) update(formula(mod[[x]]), 
                                                  paste("~ . + ", add_terms))) %>% 
  lapply(function(model) lmer(model, data = data_scaled_t))


data_scaled_t.leip$t_rvp<-as.numeric(data_scaled_t.leip$t_rvp)

models_t.leip <- c("t_vap", "t_vep", "t_rvp") %>% 
  lapply(function(y) formula(paste(y, "~", paste(rhs_t.leip, collapse = " + ")))) %>% 
  lapply(mod=., seq(1:3), function(x, mod) update(formula(mod[[x]]), 
                                                  paste("~ . + ", add_terms))) %>% 
  lapply(function(model) lmer(model, data = data_scaled_t.leip))

data_scaled_t.adj$t_rvp<-as.numeric(data_scaled_t.adj$t_rvp)

models_t.adj <- c("t_vap", "t_vep", "t_rvp") %>% 
  lapply(function(y) formula(paste(y, "~", paste(rhs_t.adj, collapse = " + ")))) %>% 
  lapply(mod=., seq(1:3), function(x, mod) update(formula(mod[[x]]), 
                                                  paste("~ . + ", add_terms))) %>% 
  lapply(function(model) lmer(model, data = data_scaled_t.adj))

# check fits
all_opt <- allFit(models_t[[2]])
all_summary <- summary(all_opt)
all_summary$fixef
all_summary$llik
all_summary$sdcor
all_summary$theta
all_summary$which.OK


all_opt.leip <- allFit(models_t.leip[[2]])
all_summary.leip <- summary(all_opt.leip)
all_summary.leip$fixef
all_summary.leip$llik
all_summary.leip$sdcor
all_summary.leip$theta
all_summary.leip$which.OK


all_opt.adj <- allFit(models_t.adj[[2]])
all_summary.adj <- summary(all_opt.adj)
all_summary.adj$fixef
all_summary.adj$llik
all_summary.adj$sdcor
all_summary.adj$theta
all_summary.adj$which.OK


ranef_sd <- models_t %>% 
  lapply(function(model) VarCorr(model)) %>% 
  lapply(function(r) as.data.frame(r)) 


ranef_sd.leip <- models_t.leip %>% 
  lapply(function(model) VarCorr(model)) %>% 
  lapply(function(r) as.data.frame(r)) 


ranef_sd.adj <- models_t.adj %>% 
  lapply(function(model) VarCorr(model)) %>% 
  lapply(function(r) as.data.frame(r)) 


estimates <- stargazer(models_t, align = TRUE, type = "text", out.header = TRUE,
                       out = 'out/simulation_estimates.tex', 
                       title = 'Turnout Estimates by Population (EAC Data)',
                       # dep.var.labels = depvars,
                       # covariate.labels = indvars,
                       float = TRUE, float.env = "table", header = FALSE,
                       intercept.top = TRUE, intercept.bottom = FALSE,
                       model.numbers = FALSE, no.space = TRUE, 
                       # order = c(1, 15:19, 11:14, 4:10, 2:3, 20:26),
                       dep.var.caption = "Rate:", keep.stat = c('n'),
                       single.row = TRUE, report = 'vc*', digits = 5,
                       column.sep.width = '-20pt')


estimates.leip <- stargazer(models_t.leip, align = TRUE, type = "text", out.header = TRUE,
                            out = 'out/simulation_estimates.leip.tex', 
                            title = 'Turnout Estimates by Population (Leip Data)',
                            # dep.var.labels = depvars,
                            # covariate.labels = indvars,
                            float = TRUE, float.env = "table", header = FALSE,
                            intercept.top = TRUE, intercept.bottom = FALSE,
                            model.numbers = FALSE, no.space = TRUE, 
                            # order = c(1, 15:19, 11:14, 4:10, 2:3, 20:26),
                            dep.var.caption = "Rate:", keep.stat = c('n'),
                            single.row = TRUE, report = 'vc*', digits = 5,
                            column.sep.width = '-20pt')


estimates.adj <- stargazer(models_t.adj, align = TRUE, type = "text", out.header = TRUE,
                           out = 'out/simulation_estimates.adj.tex', 
                           title = 'Turnout Estimates by Population (CPS-Adjusted Leip Data)',
                           # dep.var.labels = depvars,
                           # covariate.labels = indvars,
                           float = TRUE, float.env = "table", header = FALSE,
                           intercept.top = TRUE, intercept.bottom = FALSE,
                           model.numbers = FALSE, no.space = TRUE, 
                           # order = c(1, 15:19, 11:14, 4:10, 2:3, 20:26),
                           dep.var.caption = "Rate:", keep.stat = c('n'),
                           single.row = TRUE, report = 'vc*', digits = 5,
                           column.sep.width = '-20pt')

# correction
sim_data <- data_scaled_t %>% 
  select(t_vap, t_vep, t_rvp, x = strict_id, z1 = felony_str_mod,
         z2 = pct_purged, pct_highschool, pct_unemployed, ln_median_income,
         median_age, pct_female, pct_black, pct_latino, year, fips, statefp,
         x_reg_rate,)


sim_data.leip <- data_scaled_t.leip %>% 
  select(t_vap, t_vep, t_rvp, x = strict_id, z1 = felony_str_mod,
         z2 = pct_purged, pct_highschool, pct_unemployed, ln_median_income,
         median_age, pct_female, pct_black, pct_latino, year, fips, statefp,
         x_reg_rate,)


sim_data.adj <- data_scaled_t.adj %>% 
  select(t_vap, t_vep, t_rvp, x = strict_id, z1 = felony_str_mod,
         z2 = pct_purged, pct_highschool, pct_unemployed, ln_median_income,
         median_age, pct_female, pct_black, pct_latino, year, fips, statefp,
         x_reg_rate,)


sim_rhs <- sim_data %>% 
  select(-starts_with('t_'), -c(z1, z2, year, fips, statefp)) %>% 
  names()


sim_rhs.leip <- sim_data.leip %>% 
  select(-starts_with('t_'), -c(z1, z2, year, fips, statefp)) %>% 
  names()


sim_rhs.adj <- sim_data.adj %>% 
  select(-starts_with('t_'), -c(z1, z2, year, fips, statefp)) %>% 
  names()


random_intercepts <- c("(1|statefp/fips)", "(1|year)")
add_terms <- paste(random_intercepts, collapse = " + ")

sims <- c("t_vap", "t_vep", "t_rvp") %>% 
  lapply(function(y) formula(paste(y, "~", paste(sim_rhs, collapse = " + ")))) %>% 
  lapply(mod=., seq(1:3), function(x, mod) update(formula(mod[[x]]), 
                                                  paste("~ . + ", add_terms))) %>% 
  lapply(function(model) lmer(model, data = sim_data))

stargazer(sims, align = TRUE, type = "text", title = "EAC Table")


sims.leip <- c("t_vap", "t_vep", "t_rvp") %>% 
  lapply(function(y) formula(paste(y, "~", paste(sim_rhs.leip, collapse = " + ")))) %>% 
  lapply(mod=., seq(1:3), function(x, mod) update(formula(mod[[x]]), 
                                                  paste("~ . + ", add_terms))) %>% 
  lapply(function(model) lmer(model, data = sim_data.leip))

stargazer(sims.leip, align = TRUE, type = "text", title = "Leip Table")


sims.adj <- c("t_vap", "t_vep", "t_rvp") %>% 
  lapply(function(y) formula(paste(y, "~", paste(sim_rhs.adj, collapse = " + ")))) %>% 
  lapply(mod=., seq(1:3), function(x, mod) update(formula(mod[[x]]), 
                                                  paste("~ . + ", add_terms))) %>% 
  lapply(function(model) lmer(model, data = sim_data.adj))

stargazer(sims.adj, align = TRUE, type = "text", title = "CPS-Adjusted Leip Table")
