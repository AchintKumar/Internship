import os
import seaborn as sns
import pandas as pd
import numpy as np
import matplotlib.pyplot as plt
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
print(derm_raw.age.isnull().any())
print(derm_raw.np.nan())

#changing values with ? to na in age column
derm_raw.replace('?','')
print(derm_raw.age.isnull)

#Converting variables to respective data types
derm_raw.erythema=derm_raw.erythema.astype('category')
print(derm_raw.erythema)







#Univariate Analysis
plt.hist(derm_raw.erythema)
plt.show()














#correlation
corr=derm_raw.corr()
print(corr)
corr=pd.DataFrame(corr)
print(corr.dtypes)

#plotting corrplot
plt.subplots(figsize=(20,20))
sns.heatmap(corr,vmax=1)
plt.show()