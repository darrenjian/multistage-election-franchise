# Project: Election Admin
# Author: NaLette Brodnax
# Sequence: 1
# Description: Acquire county-year covariates for population and demographics

# See https://www.census.gov/data/developers/data-sets/popest-popproj/
# popest.2000-2010_Intercensals.html and https://www.census.gov/data/developers/
# data-sets/popest-popproj/popest.html for census population and demographics.
# See https://www.census.gov/data/developers/data-sets/acs-5year.html and
# https://api.census.gov/data.html for economic and demographic estimates

import csv
import requests

# get authentication parameters from local file
local_file = 'census_auth.txt'
with open(local_file) as txtfile:
    auth = "&key=" + txtfile.read()
    # print("API Key: " + my_key)

# data sources and variables
acs = "NAME,DP02_0066PE,DP02_0087E,DP02_0094E,DP03_0005PE,DP05_0017E," \
      "DP05_0022E,DP05_0032PE,DP05_0033PE,DP05_0034PE,DP05_0039PE,DP05_0066PE"

data_sources = [
    {"output": "population",
     "census": "Intercensals 2000-2010",
     "api_url": "2000/pep/int_population",
     "variables": "GEONAME,DATE_DESC,POP",
     "filter": "&DATE=10",
     "filename": "population_2008.csv"},
    {"output": "population",
     "census": "Vintage 2016",
     "api_url": "2016/pep/population",
     "variables": "GEONAME,DATE_DESC,POP",
     "filter": "&DATE=5,9",
     "filename": "population_2012_2016.csv"},
    {"output": "poverty and income",
     "census": "SAIPE 1989, 1993, 1995-2016",
     "api_url": "timeseries/poverty/saipe",
     "variables": "NAME,SAEPOVRTALL_PT,SAEMHI_PT",
     "filter": "&time=2016",
     "filename": "pov_inc_saipe_2016.csv"},
    {"output": "poverty and income",
     "census": "SAIPE 1989, 1993, 1995-2016",
     "api_url": "timeseries/poverty/saipe",
     "variables": "NAME,SAEPOVRTALL_PT,SAEMHI_PT",
     "filter": "&time=2012",
     "filename": "pov_inc_saipe_2012.csv"},
    {"output": "poverty and income",
     "census": "SAIPE 1989, 1993, 1995-2016",
     "api_url": "timeseries/poverty/saipe",
     "variables": "NAME,SAEPOVRTALL_PT,SAEMHI_PT",
     "filter": "&time=2008",
     "filename": "pov_inc_saipe_2008.csv"},
    {"output": "demographics",
     "census": "ACS 5-Year 2016",
     "api_url": "2016/acs/acs5/profile",
     "variables": acs + ",DP05_0082E",
     "filter": "",
     "filename": "demographics_acs_2016.csv"},
    {"output": "demographics",
     "census": "ACS 5-Year 2013",
     "api_url": "2013/acs5/profile",
     "variables": acs,
     "filter": "",
     "filename": "demographics_acs_2013.csv"},
    {"output": "demographics",
     "census": "ACS 5-Year 2011",
     "api_url": "2011/acs/acs5/profile",
     "variables": acs,
     "filter": "",
     "filename": "demographics_acs_2011.csv"},
    {"output": "gender",
     "census": "Vintage 2016",
     "api_url": "2016/pep/charagegroups",
     "variables": 'GEONAME,DATE_DESC,POP',
     "filter": "&DATE=5,9&SEX=1",
     "filename": "gender_male_2012_2016.csv"},
    {"output": "gender",
     "census": "Vintage 2016",
     "api_url": "2016/pep/charagegroups",
     "variables": 'GEONAME,DATE_DESC,POP',
     "filter": "&DATE=5,9&SEX=2",
     "filename": "gender_female_2012_2016.csv"}
]

# api GET requests
for dsrc in data_sources:
    url = "https://api.census.gov/data/" + dsrc["api_url"] + "?get=" + \
          dsrc["variables"] + "&for=county:*" + dsrc["filter"] + auth
    response = requests.request('GET', url)
    print("County query response: " + str(response.status_code))
    data = response.json()
    with open(dsrc["filename"], "w") as csvfile:
        writer = csv.writer(csvfile)
        for row in data:
            writer.writerow(row)
    print("Generated " + dsrc["filename"] + " with " + dsrc["output"] +
          " from " + dsrc["census"])

# get and combine state files for adult population and median age
with open('state_fips.csv', 'r') as csvfile:
    reader = csv.reader(csvfile)
    next(reader, None)
    states = [row[1] for row in reader]
dates = [5, 9]

base = "https://api.census.gov/data/2016/pep/charagegroups?get=GEONAME" \
       ",DATE_DESC,AGEGROUP,POP&for=county:*&in=state:{state}&DATE={date}"

all_rows = [['GEONAME', 'DATE_DESC', 'AGEGROUP', 'POP', 'DATE', 'state',
             'county']]

for state in states:
    for date in dates:
        url = base.format(state=state, date=date) + auth
        response = requests.request('GET', url)
        print("State: " + str(state) + ", Date: " + str(date) +
              ", Response: " + str(response.status_code))
        data = response.json()
        for row in data[1:]:
            if row[2] in ['29', '31']:
                all_rows.append(row)

with open("population_age_2012_2016.csv", 'w') as csvfile:
    writer = csv.writer(csvfile)
    for row in all_rows:
        writer.writerow(row)
