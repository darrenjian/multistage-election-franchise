# Project: Election Admin
# Author: NaLette Brodnax
# Sequence: 3
# Description: County year observations for demographics and election returns

library(tidyverse)
library(USA.county.data)

# Demographics from Census SAIPE and ACS
population <- read.csv("raw/census/population_2008_2012_2016_out.csv", 
                       na.strings = "", stringsAsFactors = FALSE, header = TRUE,
                       colClasses = c("numeric", "character", "numeric",
                                      "character")) %>% 
  group_by(year, fips) %>% 
  spread(type, population) %>% 
  mutate(population = total) %>% 
  select(-total)

saipe <- c("raw/census/pov_inc_saipe_2008_out.csv", 
           "raw/census/pov_inc_saipe_2012_out.csv",
           "raw/census/pov_inc_saipe_2016_out.csv") %>% 
  lapply(function(x) read.csv(x, header = TRUE, na.strings = "",
                              stringsAsFactors = FALSE, 
                              colClasses = c("character", rep("numeric", 3),
                                             rep("character", 3)))) %>% 
  bind_rows() %>% 
  select(year, fips, pct_poverty, median_income)

acs <- c("raw/census/demographics_acs_2011_out.csv",
         "raw/census/demographics_acs_2013_out.csv") %>% 
  lapply(function(x) read.csv(x, header = TRUE, na.strings = "",
                              stringsAsFactors = FALSE, 
                              colClasses = c("character", rep("numeric", 11),
                                             rep("character", 3), "numeric"))) %>% 
  lapply(function(x) mutate(x, voting_pop = as.numeric(NA))) %>% 
  bind_rows(read.csv("raw/census/demographics_acs_2016_out.csv", header = TRUE, 
                     na.strings = "", stringsAsFactors = FALSE, 
                     colClasses = c("character", rep("numeric", 12),
                                    rep("character", 3), "numeric"))) %>% 
  mutate(year = ifelse(year == 2011, 2008, year),
         year = ifelse(year == 2013, 2012, year),
         citizen_pop = native_pop + natural_pop) %>% 
  select(-countyname, -statefp, -countyfp, -median_age, -adult_pop,
         -native_pop, -natural_pop)

county_demo <- population %>% 
  left_join(saipe, by = c("year", "fips")) %>% 
  left_join(acs, by = c("year", "fips"))

# Election returns by year (see https://github.com/Deleetdk/USA.county.data)
data(USA_county_data)

pct_rep <- USA_county_data %>% 
  select(fips, `2008` = rep08_frac, `2012` = rep12_frac, `2016` = rep16_frac) %>% 
  mutate(fips = as.character(fips),
         fips = str_pad(fips, width = 5, side = "left", pad = "0")) %>% 
  gather(`2008`, `2012`, `2016`, key = "year", value = "rep_frac")

pct_dem <- USA_county_data %>% 
  select(fips, `2008` = dem08_frac, `2012` = dem12_frac, `2016` = dem16_frac) %>% 
  mutate(fips = as.character(fips),
         fips = str_pad(fips, width = 5, side = "left", pad = "0")) %>% 
  gather(`2008`, `2012`, `2016`, key = "year", value = "dem_frac")

total_votes <- USA_county_data %>% 
  select(fips, `2008` = total08, `2012` = total12, `2016` = total16) %>% 
  mutate(fips = as.character(fips),
         fips = str_pad(fips, width = 5, side = "left", pad = "0")) %>% 
  gather(`2008`, `2012`, `2016`, key = "year", value = "total_votes")

election_results <- total_votes %>% 
  left_join(pct_dem, by = c("fips", "year")) %>% 
  left_join(pct_rep, by = c("fips", "year")) %>% 
  arrange(fips, year) %>% 
  mutate(year = as.numeric(year))

# Combine demographics and election returns
county_stats <- election_results %>% 
  left_join(county_demo, by = c("year", "fips"))

write_csv(county_stats, path = 'out/county_stats.csv', na = '')
rm(list = ls())