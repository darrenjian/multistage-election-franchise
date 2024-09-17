# Project: State Policies and the US Election Franchise: A Multistage Approach
# Replication Code

library(tidyverse) # for data wrangling
library(stargazer) # for table output
library(lme4) # for multilevel model estimation
lmerControl(optimizer = 'bobyqa', optCtrl = list(maxfun=2e5))

# location of files (set path to desired working directory)
source_dir = ''

# load election admin data
admin_vot <- read.csv(paste(source_dir, 'election_admin_data.csv', sep = ''),
                      header = TRUE, na.strings = "",
                      colClasses = c(rep("character", 5), rep("numeric", 29)))

# scale continuous measures for use with multilevel models
data_scaled <- admin_vot %>% 
  mutate_at(c("pct_purged", "hava_spend", "pct_highschool", "pct_unemployed",
              "ln_median_income", "pct_female", "median_age", "pct_black",
              "pct_latino", "x_reg_rate", "x_out_rate", "x_bal_rate"), scale)

# build model formulas
rhs <- data_scaled %>% 
  select(-c(year, fips, countyname, statefp, state, x_reg_rate, x_out_rate, 
            x_bal_rate, reg_rate, out_rate, bal_rate, vot_rate)) %>% 
  names()

# additional terms to add to baseline formula
interactions <- c("felony_str_mod*pct_black")
random_intercepts <- c("(1|statefp/fips)", "(1|year)")
add_terms <- paste(c(interactions, random_intercepts), collapse = " + ")
outcomes <- list(NULL, "x_reg_rate", 
                 "x_reg_rate + x_out_rate", 
                 "x_reg_rate + x_out_rate + x_bal_rate")

# fit multilevel models
models <- c("reg_rate", "out_rate", "bal_rate", "vot_rate") %>% 
  lapply(function(y) formula(paste(y, "~", paste(rhs, collapse = " + ")))) %>% 
  lapply(mod=., seq(1:4), function(x, mod) update(formula(mod[[x]]), 
                                                  paste("~ . + ", outcomes[x], 
                                                        " + ", add_terms))) %>% 
  lapply(function(model) lmer(model, data = data_scaled))

# check sd for random intercepts (year, state/county)
ranef_sd <- models %>% 
  lapply(function(model) VarCorr(model)) %>% 
  lapply(function(r) as.data.frame(r)) 

depvars <- c("Registration", "Turnout", "Balloting", "Counting")
indvars <- c("Constant", "Felon Voting", "Felon Voting*\\% Black", 
             "Same Day Registration", "Pct Voters Purged", "Registration Website", 
             "All-Mail Voting", "Early Voting", "Absentee List Voting", 
             "Strict ID Law", "Photo ID Required", 
             "Provisional Balloting", 
             "High Poll Density", "HAVA Spending", "Low Cost Voter Data", 
             "Republican Majority", "Democratic Majority", "\\% Highschool Grad", 
             "\\% Unemployed", "Ln Median Income", "Median Age", "\\% Women", 
             "\\% Black", "\\% Latino", 
             "Registration Rate", "Turnout Rate", "Ballot Rate")

# produce table for manuscript (standalone)
estimates <- stargazer(models, align = TRUE, type = "text", out.header = TRUE,
                       out = paste(source_dir, 'results.tex', sep = ''),
                       title = 'Linear Mixed Effects Regression Results',
                       keep.stat = c('n'), dep.var.labels = depvars,
                       covariate.labels = indvars,
                       float = TRUE, float.env = "table", header = FALSE,
                       intercept.top = TRUE, intercept.bottom = FALSE,
                       model.numbers = FALSE, no.space = TRUE, 
                       order = c(1, 2, 27, 3:23, 24:26),
                       dep.var.caption = "Dependent Variable (Rate)", 
                       single.row = TRUE, report = 'vc*', digits = 5,
                       column.sep.width = '-20pt')


