# construct variables to assess state-level attack risk: id stringency,
# registration stringency, hackability of website, provisional ballot type
library(tidyverse)

# import fips codes
state_risk <- read.csv("raw/fips_national_county.txt", header = FALSE, 
                       colClasses = "character",
                       col.names = c("state", "statefp", "countyfp", 
                                     "countyname","classfp")) %>% 
  group_by(statefp) %>% 
  select("state", "statefp") %>% 
  distinct()

# provisional ballot type (full/partial count, or no count)
# source: Sweeney et al 2017 Table 1
states_count <- c('ME', 'AL', 'AR', 'CA', 'CO', 'DC', 'GA', 'IL', 'KS', 
                  'LA', 'MD', 'MA', 'NJ', 'NM', 'NY', 'OH', 'OR', 'PA', 
                  'RI', 'UT', 'WA', 'WV')

# states allowing voter registration changes online
# source: Sweeney et al 2017 Figure 4
states_not_online <- c('AR', 'FL', 'ID', 'IA', 'ME', 'MS', 'MT', 'NH', 
                       'NC', 'ND', 'OK', 'SD', 'TN', 'WI', 'WY',
                       'AS', 'GU', 'MP', 'PR', 'UM', 'VI')

# information required to change registration online
# source: Sweeney et al 2017 Table 3, Figure 30
state_reg <- read.csv("raw/info_requirements.csv", header = TRUE,
                      na.strings = c('NA', ''), stringsAsFactors = FALSE)

# voter id stringency
# source: http://www.ncsl.org/research/elections-and-campaigns/voter-id.aspx
# Table 1

states_strict <- c('GA', 'IN', 'KS', 'MS', 'TN', 'VA', 'WI', 'AZ', 'ND', 'OH')
states_photo <- c('GA', 'IN', 'KS', 'MS', 'TN', 'VA', 'WI', 'AR', 'AL', 'FL',
                  'HI', 'ID', 'LA', 'MI', 'RI', 'SD', 'TX')

# construct variables
state_risk <- state_risk %>% 
  left_join(state_reg, by = 'state') %>% 
  mutate(
    counts_pb = ifelse(state %in% states_count, 1, 0),
    change_online = ifelse(state %in% states_not_online, 0, 1),
    strict_id = ifelse(state %in% states_strict, 1, 0),
    photo_id = ifelse(state %in% states_photo, 1, 0)
  )
write_csv(state_risk, path = 'out/state_risk.csv', na = '')
