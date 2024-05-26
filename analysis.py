import pandas as pd
from functions import CalculateShareRatios, CalculatePercentileRatios, CalculateGINI, LorenzCurve, AtkinsonIndex, TheilIndex

# Loading the datasets.
SILCHousehold22 = pd.read_csv('gyk22_h.csv', delimiter=';')
SILCHousehold23 = pd.read_csv('gyk23_h.csv')
SILCPersonal22 = pd.read_csv('gyk22_f.csv', delimiter=';')
SILCPersonal23 = pd.read_csv('gyk23_f.csv')
SILCPersonalRecord22 = pd.read_csv('gyk22_fk.csv', delimiter=';')
SILCPersonalRecord23 = pd.read_csv('gyk23_fk.csv')

# Creating dictionaries to store the datasets, for easier access to both years.
SILCHousehold = {'22': SILCHousehold22, '23': SILCHousehold23}
SILCPersonal = {'22': SILCPersonal22, '23': SILCPersonal23}
SILCPersonalRecord = {'22': SILCPersonalRecord22, '23': SILCPersonalRecord23}

# Cleaning the datasets, keeping only the necessary columns, and renaming them for easier access.
for Year in ['22', '23']:
    SILCHousehold[Year] = SILCHousehold[Year].loc[:, ['BULTEN', 'HB031', 'HB040', 'HG110', 'HG030N', 'HG030A', 'HG040', 'HG050N', 'HG050A', 'HG060N', 'HG060A']]
    SILCHousehold[Year].loc[:, 'Transfers'] = SILCHousehold[Year][['HG030N', 'HG030A', 'HG040', 'HG050N', 'HG050A', 'HG060N', 'HG060A']].sum(axis=1)
    SILCHousehold[Year].drop(['HG030N', 'HG030A', 'HG040', 'HG050N', 'HG050A', 'HG060N', 'HG060A'], axis=1, inplace=True)
    SILCPersonalRecord[Year] = SILCPersonalRecord[Year].loc[:, ['BULTEN', 'FERT_ID', 'FK050', 'FK070', 'FK090']]
    SILCPersonal[Year] = SILCPersonal[Year].loc[:, ['BULTEN', 'FERT_ID', 'FE030', 'FI145', 'FG140']]
    SILCHousehold[Year].rename(columns={'BULTEN': 'Family', 'HB031': 'Region', 'HB040': 'Household Weight', 'HG110': 'Household Income'}, inplace=True)
    SILCPersonalRecord[Year].rename(columns={'BULTEN': 'Family', 'FERT_ID': 'Individual', 'FK050': 'Individual Record Weight', 'FK070': 'Age', 'FK090': 'Gender'}, inplace=True)
    SILCPersonal[Year].rename(columns={'BULTEN': 'Family', 'FERT_ID': 'Individual', 'FE030': 'Education', 'FI145': 'Place of Work', 'FG140': 'Income'}, inplace=True)

# Creating columns in the datasets to store the average years of education for each family and individual.
EducationYears = {0: 0, 1: 0, 2: 5, 3: 4, 4: 8, 5: 11.5, 6: 11.5, 7: 13.5, 8: 15.5, 9: 17.5, 10: 22.5}
for Year in ['22', '23']:
    SILCPersonal[Year]['Years of Education'] = SILCPersonal[Year]['Education'].map(EducationYears)
    SILCPersonal[Year]['Average Years of Education'] = SILCPersonal[Year].groupby('Family')['Years of Education'].transform('mean')
    SILCHousehold[Year]['Average Years of Education'] = SILCHousehold[Year]['Family'].map(SILCPersonal[Year].groupby('Family')['Average Years of Education'].mean())

# Creating a dataset to store both personal and household data, mapping household data to personal data.
# Also calculating the OECD Equivalence Scale values for each family.
SILCAll = {}
for Year in ['22', '23']:
    SILCAll[Year] = pd.merge(pd.merge(SILCPersonalRecord[Year], SILCPersonal[Year], on=['Family', 'Individual'], how='left'), SILCHousehold[Year], on=['Family'])
    Over14Count = SILCAll[Year].groupby('Family')['Age'].apply(lambda x: (x > 14).sum())
    Under14Count = SILCAll[Year].groupby('Family')['Age'].apply(lambda x: (x <= 14).sum())
    FamilyCounts = pd.DataFrame({'Over14Count': Over14Count, 'Under14Count': Under14Count})
    FamilyCounts['Family EFB'] = 1 + (FamilyCounts['Over14Count'] - 1) * 0.5 + FamilyCounts['Under14Count'] * 0.3
    SILCAll[Year] = SILCAll[Year].merge(FamilyCounts['Family EFB'], on='Family', how='left')
    SILCHousehold[Year]['Family EFB'] = SILCHousehold[Year]['Family'].map(SILCAll[Year].groupby('Family')['Family EFB'].mean())

# Calculating income statistics for each family, including scaled income values and transfer statistics.
for Year in ['22', '23']:
    for Dataset in SILCAll[Year], SILCHousehold[Year]:
        Dataset['Scaled Household Income'] = Dataset['Household Income'] / Dataset['Family EFB']
        Dataset['Household Income Without Transfers'] = Dataset['Household Income'] - Dataset['Transfers']
        Dataset['Scaled Household Income Without Transfers'] = Dataset['Household Income Without Transfers'] / Dataset['Family EFB']
        Dataset['Scaled Transfers'] = Dataset['Transfers'] / Dataset['Family EFB']

# These are obvious and harmful outliers in the dataset, and they are removed for further analysis.
SILCAll['22'].drop(SILCAll['22'][SILCAll['22']['Family'].isin([9292, 9289])].index, inplace=True)
SILCHousehold['22'].drop(SILCHousehold['22'][SILCHousehold['22']['Family'].isin([9292, 9289])].index, inplace=True)

# Classifying individuals by their genders for further analysis.
SILCMen = {}
SILCWomen = {}
for Year in ['22', '23']:
    SILCMen[Year] = SILCAll[Year].loc[(SILCAll[Year]['Gender'] == 1) & (SILCAll[Year]['Age'] > 14)]
    SILCWomen[Year] = SILCAll[Year].loc[(SILCAll[Year]['Gender'] == 2) & (SILCAll[Year]['Age'] > 14)]
    for Dataset in [SILCMen[Year], SILCWomen[Year]]:
        Dataset.loc[:, 'Income'] = Dataset['Income'].fillna(0).apply(lambda x: 0 if x < 0 else x)

# Regional Analysis
AllRegions = []
for Year in ['22', '23']:
    AllRegions.extend(SILCHousehold[Year]['Region'].unique())
Regions = sorted(set(AllRegions))
RegionData = {}

# Calculating income statistics for each region, including scaled income values and transfer statistics.
# Assigning the calculated values to the RegionData dictionary.
for Year in ['22', '23']:
    RegionData[Year] = pd.DataFrame(index=Regions)
    RegionData[Year]['Region'] = Regions
    RegionData[Year]['Scaled Household Income'] = SILCHousehold[Year].groupby('Region')['Scaled Household Income'].mean()
    RegionData[Year]['Scaled Household Income Without Transfers'] = SILCHousehold[Year].groupby('Region')['Scaled Household Income Without Transfers'].mean()
    RegionData[Year]['Household Income'] = SILCHousehold[Year].groupby('Region')['Household Income'].mean()
    RegionData[Year]['Household Income Without Transfers'] = SILCHousehold[Year].groupby('Region')['Household Income Without Transfers'].mean()
    RegionData[Year]['Transfers'] = SILCHousehold[Year].groupby('Region')['Transfers'].mean()
    RegionData[Year]['Family EFB'] = SILCHousehold[Year].groupby('Region')['Family EFB'].mean()
    RegionData[Year]['Transfers as % of Income'] = RegionData[Year]['Transfers'] / RegionData[Year]['Household Income'] * 100

# Creating datasets to store the individual income statistics for each individual, stored grouped by their genders and regions.
RegionDataMen = {}
RegionDataWomen = {}
for Year in ['22', '23']:
    RegionDataMen[Year] = {}
    RegionDataWomen[Year] = {}
    for Region in Regions:
        RegionDataMen[Year][Region] = SILCMen[Year].loc[SILCMen[Year]['Region'] == Region]
        RegionDataWomen[Year][Region] = SILCWomen[Year].loc[SILCWomen[Year]['Region'] == Region]

# Calculating GINI coefficients for genders grouped by regions.
RegionWomenGINI22 = pd.DataFrame({Region: CalculateGINI(RegionDataWomen['22'][Region]['Income']) for Region in Regions}, index=[0]).transpose().rename(columns={0: 'GINI Coefficient for Women - 2022'}).sort_values(by='GINI Coefficient for Women - 2022')
RegionMenGINI22 = pd.DataFrame({Region: CalculateGINI(RegionDataMen['22'][Region]['Income']) for Region in Regions}, index=[0]).transpose().rename(columns={0: 'GINI Coefficient for Men - 2022'}).sort_values(by='GINI Coefficient for Men - 2022')
RegionWomenGINI23 = pd.DataFrame({Region: CalculateGINI(RegionDataWomen['23'][Region]['Income']) for Region in Regions}, index=[0]).transpose().rename(columns={0: 'GINI Coefficient for Women - 2023'}).sort_values(by='GINI Coefficient for Women - 2023')
RegionMenGINI23 = pd.DataFrame({Region: CalculateGINI(RegionDataMen['23'][Region]['Income']) for Region in Regions}, index=[0]).transpose().rename(columns={0: 'GINI Coefficient for Men - 2023'}).sort_values(by='GINI Coefficient for Men - 2023')
RegionAllGINI22 = pd.DataFrame({Region: CalculateGINI(SILCHousehold['22'].loc[SILCHousehold['22']['Region'] == Region]['Scaled Household Income']) for Region in Regions}, index=[0]).transpose().rename(columns={0: 'GINI Coefficient - 2022'}).sort_values(by='GINI Coefficient - 2022')
RegionAllGINI23 = pd.DataFrame({Region: CalculateGINI(SILCHousehold['23'].loc[SILCHousehold['23']['Region'] == Region]['Scaled Household Income']) for Region in Regions}, index=[0]).transpose().rename(columns={0: 'GINI Coefficient - 2023'}).sort_values(by='GINI Coefficient - 2023')

RegionGINI = pd.concat([RegionWomenGINI22, RegionMenGINI22, RegionWomenGINI23, RegionMenGINI23, RegionAllGINI22, RegionAllGINI23], axis=1).sort_values(by='GINI Coefficient - 2023')

# Exporting the datasets to CSV files for later use in mapping.
RegionGINI.to_csv('RegionGINI.csv')
RegionData['22'].to_csv('RegionIncomeStatistics22.csv', index=False)
RegionData['23'].to_csv('RegionIncomeStatistics23.csv', index=False)
