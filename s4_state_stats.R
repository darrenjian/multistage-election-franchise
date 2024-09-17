# Project: Election Admin
# Author: NaLette Brodnax
# Sequence: 4
# Description: Construct variables to assess state-level attack risk: 
# id stringency, registration stringency, hackability of website, and
# provisional ballot type

library(tidyverse)
library(lubridate)

# import fips codes
states <- read.csv("out/fips.csv", header = TRUE, colClasses = "character",
                    col.names = c("state", "statefp", "countyfp", 
                                  "countyname","classfp", "fips", "mcdfp",
                                  "mcd")) %>% 
  group_by(statefp) %>% 
  select("state", "statefp") %>% 
  distinct()

# provisional ballot type (full/partial count, or no count)
# source: Sweeney et al 2017 Table 1
states_count <- c('ME', 'AL', 'AR', 'CA', 'CO', 'DC', 'GA', 'IL', 'KS', 
                  'LA', 'MD', 'MA', 'NJ', 'NM', 'NY', 'OH', 'OR', 'PA', 
                  'RI', 'UT', 'WA', 'WV')

# voter id stringency
# source: http://www.ncsl.org/research/elections-and-campaigns/voter-id.aspx
# Table 1
states_strict <- c('GA', 'IN', 'KS', 'MS', 'TN', 'VA', 'WI', 'AZ', 'ND', 'OH')
states_photo <- c('GA', 'IN', 'KS', 'MS', 'TN', 'VA', 'WI', 'AR', 'AL', 'FL',
                  'HI', 'ID', 'LA', 'MI', 'RI', 'SD', 'TX')

# felony voting restoration
felony_lenient <- c("ME", "VT", "DC", "HI", "IL", "IN", "MD", "MA", "MI", "MT", 
                    "NH", "ND", "OH", "OR", "PA", "RI", "UT")
felony_moderate <-c("AK", "AR", "CA", "CO", "CT", "GA", "ID", "KS", "LA", "MN", 
                    "MO", "NE", "NJ", "NM", "NY", "NC", "OK", "SC", "SD", "TX", 
                    "WA", "WV", "WI")
felony_strict <- c("AZ", "NV", "FL", "IA", "KY", "MI", "TN", "VA", "WY", "AL", "DE")

# voter registration deadlines
reg_deadlines <- read.csv("raw/reg_deadlines.csv", header = TRUE, na.strings = "", 
                          colClasses = c(rep("character", 2), rep("numeric", 4)), 
                          stringsAsFactors = FALSE) %>% select(-state_fips)

# early voting provisions
# source: http://www.ncsl.org/research/elections-and-campaigns/absentee-and-
# early-voting.aspx
# source: http://www.ncsl.org/research/elections-and-campaigns/early-voting-in
# -state-elections.aspx
early <- read.csv("raw/early_voting.csv", header = TRUE, na.strings = "",
                  colClasses = c("character", rep("numeric", 7)),
                  stringsAsFactors = FALSE)

# construct variables
state_policy <- states %>% 
  left_join(reg_deadlines, by = 'state') %>% 
  left_join(early, by = "state") %>% 
  mutate(counts_pb = ifelse(state %in% states_count, 1, 0),
         strict_id = ifelse(state %in% states_strict, 1, 0),
         photo_id = ifelse(state %in% states_photo, 1, 0),
         felony_lenient = ifelse(state %in% felony_lenient, 1, 0),
         felony_moderate = ifelse(state %in% felony_moderate, 1, 0),
         felony_strict = ifelse(state %in% felony_strict, 1, 0))

# website features
# source: Sweeney et al 2017 Table 3, Figure 30, 
# source: https://www.vote.org/voter-registration-deadlines/
features <- read.csv("raw/website_features.csv", header = TRUE, na.strings = "",
                     colClasses = c("character", rep("numeric", 11)),
                     stringsAsFactors = FALSE) %>% 
  gather(year, site_live, -c(state:info_address)) %>% 
  mutate(year = as.numeric(str_sub(year, 5, 8))) %>% 
  group_by(year) %>% 
  mutate_at(c("website", "cost_100_less", "cost_50_less", "cost_25_less",
              "info_standard", "info_other", "info_ssn", "info_address"),
            funs(ifelse(site_live, ., 0))) %>% 
  left_join(states, by = 'state') %>% 
  select(state, statefp, year, everything()) %>% 
  select(-site_live)

# HAVA Spending (file missing)
election_year <- tibble(year = seq(2007, 2016, by = 1),
                        election = c(rep(2008, 2), rep(2012, 4), rep(2016, 4)))
hava <- read.csv("raw/hava_transactions.csv", header = TRUE, na.strings = "") %>% 
  select(ID = award_id_fain, amount = total_funding_amount, action_date, 
         recipient_name, state = recipient_state_code, 
         state_name = primary_place_of_performance_state_name,
         cfda_number, cfda_title) %>% 
  mutate(action_date = ymd(action_date), year = year(action_date),
         cfda_number = str_pad(cfda_number, width = 6, side = "right", 
                               pad = "0")) %>% 
  left_join(election_year, by = "year") %>% 
  mutate_if(is.factor, as.character)

hava_spend <- hava %>% 
  group_by(state, year = election) %>% 
  summarise(hava_spend = sum(amount)) %>% 
  left_join(states, by = 'state')

# State partisanship
partisan_hist <- read.csv("raw/partisan_hist.csv", header = TRUE, na.strings = "",
                          stringsAsFactors = FALSE) %>% 
  gather(year, legis_control, -state) %>% 
  mutate(year = as.numeric(str_sub(year, 2, 5)))

partisan <- read.csv("raw/partisan.csv", header = TRUE, na.strings = "",
                     stringsAsFactors = FALSE) %>% 
  select(state, year, legis_control) %>% 
  bind_rows(partisan_hist) %>% 
  mutate(dem_control = ifelse(legis_control == "Dem", 1, 0),
         dem_control = ifelse(is.na(dem_control), 0, dem_control),
         rep_control = ifelse(legis_control == "Rep", 1, 0),
         rep_control = ifelse(is.na(rep_control), 0, rep_control)) %>% 
  select(-legis_control) %>% 
  left_join(states, by = 'state') %>% 
  filter(year %in% c(2008, 2012, 2016)) %>% 
  distinct()

state_stats <- states %>% 
  mutate(year08 = "2008", year12 = "2012", year16 = "2016") %>% 
  gather(label, year, -state, -statefp) %>% 
  mutate(year = as.numeric(year)) %>% 
  group_by(state, statefp, year) %>% 
  left_join(partisan, by = c("state", "statefp", "year")) %>% 
  left_join(hava_spend, by = c("state", "statefp", "year")) %>% 
  left_join(features, by = c("state", "statefp", "year")) %>% 
  left_join(state_policy, by = c("state", "statefp")) %>% 
  select(-label)

write_csv(state_stats, path = 'out/state_stats.csv', na = '')
rm(list = ls())
