import os
import seaborn as sns
import pandas as pd
#Reading and changing path
print(os.getcwd())
os.chdir('/media/achint/INSOFE/Internship')
#Listing everything in current directory
print(os.listdir())


#Reading data
derm_raw=pd.read_csv('dermatology_data.csv')
#to check dimensions
print(derm_raw.shape)
print(derm_raw.head())
#Structure of dataset
#include=all done coz it shows only numerical attributes
print(derm_raw.describe(include='all'))
#check data type of dataset
print(derm_raw.dtypes)


#checking missing values
print(sum(derm_raw.isnull().any(axis=0)))


print(derm_raw.corr())
print(sns.heatmap(derm_raw.corr(),vmax=1))
