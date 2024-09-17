# Project: Election Admin
# Author: NaLette Brodnax
# Sequence: 2
# Description: Acquire county-year population from census state files

import glob
import csv
import pandas as pd

# state fips codes
with open('state_fips.csv', 'r') as csvfile:
    reader = csv.reader(csvfile)
    next(reader, None)
    states = [row[1] for row in reader]

src_files = glob.glob('ftp/*')

df_all = pd.DataFrame()
df_all_gen = pd.DataFrame()

for filename in src_files:
    print('Reading: ' + filename)
    df = pd.read_csv(filename)

    df_adult_pop = df.loc[
        (df.YEAR == 11), ['STATE', 'COUNTY', 'CTYNAME', 'YEAR',
                          'AGE18PLUS_TOT']]
    df_adult_pop['AGEGROUP'] = '29'
    df_adult_pop.columns = ['state', 'county', 'GEONAME', 'DATE', 'POP',
                            'AGEGROUP']

    df_med_age = df.loc[
        (df.YEAR == 11), ['STATE', 'COUNTY', 'CTYNAME', 'YEAR',
                          'MEDIAN_AGE_TOT']]
    df_med_age['AGEGROUP'] = '31'
    df_med_age.columns = ['state', 'county', 'GEONAME', 'DATE', 'POP',
                          'AGEGROUP']

    df_male = df.loc[
        (df.YEAR == 11), ['STATE', 'COUNTY', 'CTYNAME', 'YEAR', 'POPEST_MALE']]
    df_male['SEX'] = '1'
    df_male.columns = ['state', 'county', 'GEONAME', 'DATE', 'POP', 'SEX']

    df_female = df.loc[
        (df.YEAR == 11), ['STATE', 'COUNTY', 'CTYNAME', 'YEAR', 'POPEST_FEM']]
    df_female['SEX'] = '2'
    df_female.columns = ['state', 'county', 'GEONAME', 'DATE', 'POP', 'SEX']

    df_all = pd.concat([df_all, df_adult_pop, df_med_age])
    df_all_gen = pd.concat([df_all_gen, df_male, df_female])

print('Rows added, AGEGROUP: ' + str(len(df_all)))
df_all.to_csv('population_age_2008.csv', index=False, encoding='utf-8-sig')

print('Rows added, SEX: ' + str(len(df_all_gen)))
df_all_gen.to_csv('gender_2008.csv', index=False, encoding='utf-8-sig')
