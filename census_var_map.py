# Project: Election Admin
# Author: NaLette Brodnax
# Sequence: 3
# Description: Combines Census data and converts variable names

import csv
import re

# population
pop_files = [{"file": "population_2008.csv",
              "fields": ["GEONAME", "DATE_DESC", "POP", "DATE", "state",
                         "county"]},
             {"file": "population_2012_2016.csv",
              "fields": ["GEONAME", "DATE_DESC", "POP", "DATE", "state",
                         "county"]},
             {"file": "population_age_2008.csv",
              "fields": ['state', 'county', 'GEONAME', 'DATE', 'POP',
                         'AGEGROUP']},
             {"file": "population_age_2012_2016.csv",
              "fields": ["GEONAME", "DATE_DESC", "AGEGROUP", "POP", "DATE",
                         "state", "county"]},
             {"file": "gender_2008.csv",
              "fields": ['state', 'county', 'GEONAME', 'DATE', 'POP', 'SEX']},
             {"file": "gender_male_2012_2016.csv",
              "fields": ["GEONAME", "DATE_DESC", "POP", "DATE", "SEX", "state",
                         "county"]},
             {"file": "gender_female_2012_2016.csv",
              "fields": ["GEONAME", "DATE_DESC", "POP", "DATE", "SEX", "state",
                         "county"]}
]

pop_data = []
key_map = {"1": "male_pop", "2": "female_pop", "29": "adult_pop",
           "31": "median_age", "5": "2012", "9": "2016", "10": "2008",
           "11": "2008"}

for censusfile in pop_files:
    with open(censusfile["file"], 'r') as csvfile:
        print("Reading data from " + censusfile["file"])
        reader = csv.DictReader(csvfile, fieldnames=censusfile["fields"])
        next(reader, None)
        for row in reader:
            tmp = {}
            tmp["year"] = key_map[row["DATE"]]
            tmp["fips"] = row["state"].zfill(2) + row["county"].zfill(3)
            tmp["population"] = row["POP"]
            tmp["type"] = "total"
            if "AGEGROUP" in row.keys():
                tmp["type"] = key_map[row["AGEGROUP"]]
            elif "SEX" in row.keys():
                tmp["type"] = key_map[row["SEX"]]
            else:
                tmp["type"] = "total"
            pop_data.append(tmp)

with open("../code/raw/census/population_2008_2012_2016_out.csv",
          'w') as outfile:
    writer = csv.DictWriter(outfile, fieldnames=["year", "fips", "population",
                                                 "type"])
    writer.writeheader()
    for row in pop_data:
        writer.writerow(row)

# demographics
bykeys = {"NAME": "countyname", "GEONAME": "countyname", "time": "year",
          "state": "statefp", "county": "countyfp", "DATE_DESC": "date_desc",
          "DATE": "date_code", "POP": "population",
          "SAEPOVRTALL_PT": "pct_poverty", "SAEMHI_PT": "median_income",
          "DP02_0066PE": "pct_highschool", "DP02_0087E": "native_pop",
          "DP02_0094E": "natural_pop", "DP03_0005PE": "pct_unemployed",
          "DP05_0017E": "median_age", "DP05_0022E": "adult_pop",
          "DP05_0032PE": "pct_white", "DP05_0033PE": "pct_black",
          "DP05_0034PE": "pct_amind", "DP05_0039PE": "pct_asian",
          "DP05_0066PE": "pct_latino", "DP05_0082E": "voting_pop"}

byvalues = {bykeys[key]: key for key in bykeys}
byvalues["countyname"] = "NAME"

acs_files = ["demographics_acs_2011.csv", "demographics_acs_2013.csv",
             "demographics_acs_2016.csv", "pov_inc_saipe_2008.csv",
             "pov_inc_saipe_2012.csv", "pov_inc_saipe_2016.csv"]

acs_infields = {}
acs_outfields = {}
for file in acs_files:
    with open(file, 'r') as f:
        infields = f.readline()[:-1].split(",")
        acs_infields[file] = infields
        acs_outfields[file] = [bykeys.get(column) for column in infields]
        if "year" not in acs_outfields[file]:
            acs_outfields[file].extend(["fips", "year"])
        else:
            acs_outfields[file].append("fips")


for acs_file in acs_files:
    outfilename = "../code/raw/census/" + acs_file[:-4] + "_out.csv"
    year = re.search(r'\d{4}', acs_file)
    with open(acs_file, 'r') as infile, \
            open(outfilename, 'w') as outfile:
        print("Reading data from " + acs_file)
        reader = csv.DictReader(infile, acs_infields[acs_file])
        next(reader, None)
        writer = csv.DictWriter(outfile, acs_outfields[acs_file])
        writer.writeheader()
        tmp = {}
        for row in reader:
            for column in acs_outfields[acs_file][:-2]:
                tmp[column] = row[byvalues[column]]
            tmp["fips"] = row["state"] + row["county"]
            if "time" not in row.keys():
                if year:
                    tmp["year"] = year.group()
            writer.writerow(tmp)
