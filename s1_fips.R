# Project: Election Admin
# Author: NaLette Brodnax
# Sequence: 1
# Description: Creates FIPS code map for states, counties, cities, and WI wards

library(tidyverse)

# fips codes for counties
# see https://www.census.gov/geo/reference/codes/cou.html
county_fips <- read.csv("raw/fips_national_county.txt", header = FALSE, 
                        colClasses = "character",
                        col.names = c("state", "statefp", "countyfp", 
                                      "countyname", "classfp")) %>% 
  mutate(fips = paste(statefp, countyfp, sep = ""))

# fips codes for wisconsin MCDs
# see http://legis.wisconsin.gov/ltsb/gis/data/
wisc_fips <- read.csv("raw/wi_ward_fips.csv", na.strings = "", 
                      stringsAsFactors = FALSE,
                      colClasses = "character", strip.white = TRUE) %>% 
  select(mcdfp = COUSUBFP, fips = CNTY_FIPS, mcd = MCD_FIPS) %>% 
  na.omit() %>% 
  distinct(mcdfp, .keep_all = TRUE) %>% 
  left_join(county_fips, by = "fips")

# fips codes for other MCDs/CCDs
mcd_fips <- tibble(
  state = c('AK', 'DC', rep('IL', 7), 'ME', 'MO', 'NY', 'NY', 'AS', 'GU', 'PR',
            'VI'),
  statefp = c('02', '11', rep('17', 7), '23', '29', '36', '36', '60', '66', 
              '72', '78'),
  countyfp = c('000', '000', '030', '066', '140', '222', '283', '590', '650',
               '099', '380', '000', '122', rep('000', 4)),
  countyname = c('All Counties', 'District of Columbia', 'City of Aurora', 
                 'City of Bloomington', 'City of Chicago', 'City of East St Louis', 
                 'City of Galesburg', 'City of Peoria', 'City of Rockford', 
                 'Shapleigh County', 'City of Kansas City', 'All Counties', 
                 'Yates County', 'American Samoa', 'Guam', 'Puerto Rico', 
                 'Virgin Islands'),
  classfp = as.character(rep(NA, 17)),
  fips = paste(statefp, countyfp, sep = ""))

fips <- county_fips %>% 
  bind_rows(mcd_fips) %>% 
  mutate(mcdfp = as.character(NA), mcd = as.character(NA)) %>% 
  filter(state != "WI") %>% 
  bind_rows(wisc_fips)

write.csv(fips, file = "out/fips.csv", na = "", row.names = FALSE)
rm(list = ls())