# Project: Election Admin
# Author: NaLette Brodnax
# Sequence: 2
# Description: Combines EAC survey data into county-year observations

library(tidyverse)
library(readxl)

fips <- read.csv("out/fips.csv", header = TRUE, colClasses = "character",
                 col.names = c("state", "statefp", "countyfp", "countyname",
                               "classfp", "fips", "mcdfp", "mcd"), 
                 na.strings = "")

# variables of interest
survey <- read.csv("raw/eac_variables.csv", stringsAsFactors = FALSE,
                   colClasses = "character", na.strings = "")

# 2008 data from eac.gov - survey section files a, b, c, e, f
# Note: Illinois and Kentucky reported missing but appear included
eac08a <- read_excel("raw/2008/Combined_SectionA.xls", sheet = "SectionA",
                     col_names = TRUE, col_types = "text", 
                     na = c("", "N/A", "-888888", "-999999")) %>% 
  select(one_of(c("STATE_", "FIPS_CODE", survey$item2008)))

eac08b <- read_excel("raw/2008/Combined_SectionB.xls", sheet = "SectionB",
                     col_names = TRUE, col_types = "text", 
                     na = c("", "N/A", "-888888", "-999999")) %>% 
  select(one_of(survey$item2008))

eac08c <- read_excel("raw/2008/Combined_SectionC.xls", sheet = "SectionC",
                     col_names = TRUE, col_types = "text", 
                     na = c("", "N/A", "-888888", "-999999")) %>% 
  select(one_of(survey$item2008))

eac08d <- read_excel("raw/2008/Combined_SectionD.xls", sheet = "SectionD",
                     col_names = TRUE, col_types = "text", 
                     na = c("", "N/A", "-888888", "-999999")) %>% 
  select(one_of(survey$item2008))

eac08e <- read_excel("raw/2008/Combined_SectionE.xls", sheet = "SectionE",
                     col_names = TRUE, col_types = "text", 
                     na = c("", "N/A", "-888888", "-999999")) %>% 
  select(one_of(survey$item2008))

eac08f <- read_excel("raw/2008/Combined_SectionF.xls", sheet = "SectionF",
                     col_names = TRUE, col_types = "text", 
                     na = c("", "N/A", "-888888", "-999999")) %>% 
  select(one_of(survey$item2008))

eac08 <- eac08a %>% 
  left_join(eac08b, by = "JurisID") %>%
  left_join(eac08c, by = "JurisID") %>%
  left_join(eac08d, by = "JurisID") %>%
  left_join(eac08e, by = "JurisID") %>%
  left_join(eac08f, by = "JurisID") %>% 
  mutate(FIPS = str_sub(FIPS_CODE, 1, 5), year = "2008") %>% 
  mutate(A1 = as.numeric(A1), A3a = as.numeric(A3a), A3b = as.numeric(A3b)) %>% 
  rowwise() %>% 
  mutate(A1 = ifelse(STATE_ == 'IL', sum(A3a, A3b, na.rm = TRUE), A1)) %>% 
  mutate(A1 = as.character(A1), A3a = as.character(A3a), 
         A3b = as.character(A3b)) %>% 
  select(-STATE_, -JurisID, -FIPS_CODE)
colnames(eac08) <- c(survey$name[1:35], "fips", "year")

# 2012 data from eac.gov - survey section files a, b, c, e, f
eac12a <- read_excel("raw/2012/2012EAVS_NVRAData.xlsx", sheet = "2012NVRAData",
                     col_names = TRUE, col_types = "text", 
                     na = c("", "N/A", "-888888", "-999999")) %>% 
  select(one_of(c("State", survey$item2012)))

eac12b <- read_excel("raw/2012/2012EAVS_UOCAVAData.xls", 
                     sheet = "2012EAVS_UOCAVAData",
                     col_names = TRUE, col_types = "text", 
                     na = c("", "N/A", "-888888", "-999999")) %>% 
  select(one_of(survey$item2012))

eac12c <- read_excel("raw/2012/Section C.xls", sheet = "Section C",
                     col_names = TRUE, col_types = "text", 
                     na = c("", "N/A", "-888888", "-999999")) %>% 
  select(one_of(survey$item2012))

eac12d <- read_excel("raw/2012/Section D.xls", sheet = "Section D",
                     col_names = TRUE, col_types = "text", 
                     na = c("", "N/A", "-888888", "-999999")) %>% 
  select(one_of(survey$item2012))

eac12e <- read_excel("raw/2012/Section E.xls", sheet = "Section E",
                     col_names = TRUE, col_types = "text", 
                     na = c("", "N/A", "-888888", "-999999")) %>% 
  select(one_of(survey$item2012))

eac12f <- read_excel("raw/2012/Section F.xls", sheet = "Section F",
                     col_names = TRUE, col_types = "text", 
                     na = c("", "N/A", "-888888", "-999999")) %>% 
  mutate(FIPSCode = `FIPSCode...3`) %>% 
  select(one_of(survey$item2012))

eac12 <- eac12a %>% 
  left_join(eac12b, by = "FIPSCode") %>% 
  left_join(eac12c, by = "FIPSCode") %>% 
  left_join(eac12d, by = "FIPSCode") %>% 
  left_join(eac12e, by = "FIPSCode") %>% 
  left_join(eac12f, by = "FIPSCode") %>% 
  mutate(FIPS = str_sub(FIPSCode, 1, 5), year = "2012") %>% 
  select(-State, -FIPSCode)
colnames(eac12) <- c(survey$name[1:35], "fips", "year")

# States with leading zeroes
stateids <- fips %>% 
  filter(str_sub(statefp, 1, 1) == "0") %>% 
  select(state) %>% 
  unique() %>% 
  unlist()

eac16 <- read.csv("raw/2016/EAVS 2016 Final Data for Public Release v.3.csv",
                  na.strings = c("", "N/A", "-888888: Not Applicable",
                                 "-999999: Data Not Available"),
                  stringsAsFactors = FALSE, colClasses = "character",
                  strip.white = TRUE) %>% 
  select(one_of(c("State", survey$item2016))) %>% 
  mutate(FIPSCode = ifelse(State %in% stateids, 
                           paste("0", FIPSCode, sep = ""), FIPSCode)) 

# validation table
# note: 2008 IL voters missing from total, included 8,895,584 from A3a, A3b
from_excel <- tibble(year = c("2008", "2012", "2016"),
                     obs_excel = c(4627, 8154, 6467),
                     voters_excel = c(sum(180984324, 8895584), 
                                      193653908, 214109367))

year = c("2008", "2012", "2016")
obs_join = lapply(list(eac08, eac12, eac16), function(i){nrow(i)})
voters_join = c(lapply(list(eac08, eac12), 
                       function(i){sum(as.numeric(i$total_voters), na.rm = TRUE)}),
                sum(as.numeric(eac16$A1a), na.rm = TRUE))
validate <- tibble(year, obs_join, voters_join) %>% 
  left_join(from_excel, by = "year") %>% 
  select(year, obs_excel, obs_join, voters_excel, voters_join)

# map Wisconsin wards to counties (not 1:1)
wisc <- fips %>% 
  filter(state == "WI") %>% 
  select(mcdfp, mcd)

eac16 <- eac16 %>% 
  left_join(wisc, by = c("FIPSCode" = "mcdfp")) %>% 
  mutate(FIPSCode = ifelse(State == 'WI', mcd, FIPSCode)) %>% 
  mutate(FIPS = str_sub(FIPSCode, 1, 5), year = "2016") %>% 
  select(-State, -FIPSCode, -mcd)
colnames(eac16) <- c(survey$name[1:35], "fips", "year")

obs_wisc = lapply(list(eac08, eac12, eac16), function(i){nrow(i)})
voters_wisc = lapply(list(eac08, eac12, eac16), 
                     function(i){sum(as.numeric(i$total_voters), na.rm = TRUE)})
# validate <- validate %>% 
#   mutate(voters_wisc, obs_wisc) %>% 
#   select(year, obs_excel, obs_join, obs_wisc, voters_excel, voters_join, 
#          voters_wisc)

# combine rows and convert columns to numeric/factor
rm(list = ls()[!ls() %in% c("fips", "states", "eac08", "eac12", "eac16",
                            "validate")])
num_vars <- colnames(eac16)[c(-36)]
eac <- bind_rows(eac08, eac12, eac16)
eac[num_vars] <- lapply(eac[num_vars], as.numeric)

# aggregate to county level 
eac_cnty <- eac %>% 
  group_by(fips, year) %>% 
  summarise_all(sum)

# validate aggregation
voters_cnty <- eac_cnty %>% 
  group_by(year) %>% 
  summarise(voters_fips = sum(total_voters, na.rm = TRUE))

write_csv(eac_cnty, path = "out/eac_county.csv", na = "")
rm(list = ls())