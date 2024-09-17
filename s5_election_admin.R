# Project: Election Admin
# Author: NaLette Brodnax
# Sequence: 5
# Description: Construct master dataset with county-year and state-year data

library(tidyverse)

counties <- read.csv("out/fips.csv", header = TRUE, colClasses = "character",
                   col.names = c("state", "statefp", "countyfp", 
                                 "countyname","classfp", "fips", "mcdfp",
                                 "mcd")) %>% 
  select("fips", "statefp", "countyname") %>% 
  distinct()

# county-year survey
coltypes <- c("character", "character", rep("numeric", 35))
eac_county <- read.csv("out/eac_county.csv", header = TRUE, 
                       colClasses = coltypes, na.strings = "") %>% 
  mutate(turnout = as.numeric(turnout), prov_rate = prov_submit/turnout)

# county-year stats
coltypes <- c("character", "character", rep("numeric", 17))
county_stats <- read.csv("out/county_stats.csv", header = TRUE,
                         colClasses = coltypes, na.strings = "") %>% 
  left_join(counties, by = "fips") %>% 
  select(year, fips, countyname, statefp, everything())

# state-year data
coltypes <- c(rep("character", 3), rep("numeric", 28))
state_stats <- read.csv("out/state_stats.csv", header = TRUE,
                       colClasses = coltypes, na.strings = "") %>% 
  select(year, statefp, state, everything())

election_admin <- county_stats %>% 
  left_join(eac_county, by = c("fips", "year")) %>% 
  left_join(state_stats, by = c("statefp", "year")) %>% 
  distinct() %>% 
  mutate_at(vars(starts_with("pct")), funs(./100)) %>% 
  mutate(vote_margin = abs(rep_frac - dem_frac),
         rep_margin = rep_frac - dem_frac) %>% 
  select(year, fips, countyname, statefp, state, everything())

write_csv(election_admin, path = 'out/election_admin.csv', na = '')
rm(list = ls())
