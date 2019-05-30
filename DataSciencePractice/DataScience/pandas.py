# -*- coding: utf-8 -*-
"""
Created on Tue May 28 12:41:11 2019

@author: Nikita
"""
import pandas as pd
import numpy as np
# create a data frame - dictionary is used here where keys get converted to column names and values to row values.
data = pd.DataFrame({'Country': ['Russia', 'Colombia', 'Chile', 'Equador', 'Nigeria'],
                     'Rank': [121, 40, 100, 130, 11]})
print(data)

## to describe the data
data.describe()
print(data.describe())

print(data.info())

# now , work on sorting
data = pd.DataFrame({'group': ['a', 'a', 'a', 'b', 'b', 'b', 'c', 'c', 'c'], 'ounces': [4, 3, 12, 6, 7.5, 8, 3, 5, 6]})
data.sort_values(by=['ounces'], ascending=True, inplace=False)
##inplace = True will make changes to the data
## multiple column sort
data.sort_values(by=['group', 'ounces'], ascending=[True, False], inplace=False)

## to remove the duplicates
data.drop_duplicates()

data = pd.DataFrame(
    {'food': ['bacon', 'pulled pork', 'bacon', 'Pastrami', 'corned beef', 'Bacon', 'pastrami', 'honey ham', 'nova lox'],
     'ounces': [4, 3, 12, 6, 7.5, 8, 3, 5, 6]})
print(data)

data = pd.DataFrame(
    {'food': ['bacon', 'pulled pork', 'bacon', 'Pastrami', 'corned beef', 'Bacon', 'pastrami', 'honey ham', 'nova lox'],
     'ounces': [4, 3, 12, 6, 7.5, 8, 3, 5, 6]})

## want to add animal based on what they eat
meat_to_animal = {
    'bacon': 'pig',
    'pulled pork': 'pig',
    'pastrami': 'cow',
    'corned beef': 'cow',
    'honey ham': 'pig',
    'nova lox': 'salmon'
}


def meat_2_animal(series):
    if series['food'] == 'bacon':
        return 'pig'
    elif series['food'] == 'pulled pork':
        return 'pig'
    elif series['food'] == 'pastrami':
        return 'cow'
    elif series['food'] == 'corned beef':
        return 'cow'
    elif series['food'] == 'honey ham':
        return 'pig'
    else:
        return 'salmon'


lower = lambda x: x.lower()
data['food'] = data['food'].apply(lower)
data['animal2'] = data.apply(meat_2_animal, axis='columns')
print(data)

## SERIES
data = pd.Series([1., -999., 2., -999., -1000., 3.])
data
##Series function from pandas are used to create array

data = pd.Series([1., -999., 2., -999., -1000., 3.])
data.replace([-999, -1000], np.nan, inplace=True)
data

## categorize data into bins
ages = [20, 22, 25, 27, 21, 23, 37, 31, 61, 45, 41, 32]
bins = [18, 25, 35, 60, 100]
data = pd.cut(ages, bins)
print(data)

# how many observations fall under each bin
counter = pd.value_counts(data)
print(counter)

## Group and Pivots in PANDAS
df = pd.DataFrame({'key1': ['a', 'a', 'b', 'b', 'a'],
                   'key2': ['one', 'two', 'one', 'two', 'one'],
                   'data1': np.random.randn(5),
                   'data2': np.random.randn(5)})
grouped = df['data1'].groupby(df['key1'])
grouped.mean()
import random

# read the data from the downloaded CSV file.
data = pd.read_csv('https://s3-eu-west-1.amazonaws.com/shanebucket/downloads/uk-500.csv')
# set a numeric id for use as an index for examples.
data['id'] = [random.randint(0, 1000) for x in range(data.shape[0])]

data = data.head(5)
print(data)

data.iloc[0]  # first row of data frame (Aleshia Tomkiewicz) - Note a Series data type output.
print(data.iloc[0])
data.iloc[1]  # second row of data frame (Evan Zigomalas)
data.iloc[-1]  # last row of data frame (Mi Richan)
# Columns:
print(data.iloc[:, 0])  # first column of data frame (first_name)
data.iloc[:, 1]  # second column of data frame (last_name)
