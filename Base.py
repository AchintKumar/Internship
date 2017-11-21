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
colnames=list(derm_raw.columns.values)
colnames=colnames[:-2]
print(colnames[0])
for i in colnames:
	derm_raw[i]=derm_raw[i].astype('category')
print(derm_raw.dtypes)
print(derm_raw.describe(include='all'))
derm_raw.disease_type=derm_raw.disease_type.astype('category')
print(derm_raw.disease_type)
derm_raw.age[derm_raw.age=='?']='0'
derm_raw.age=derm_raw.age.astype('float32')
print(derm_raw.age[32:38])




#Univariate Analysis of first 33 elements
for i in colnames:
	derm_raw[i].value_counts().plot.pie(autopct='%1.1f%%',labels=['','','',''],title=i)
	plt.gca().set_aspect('equal')
	plt.legend(labels=derm_raw.index)
	plt.show()
print(derm_raw.shape)
 
#Of age
derm_raw.age.plot.hist()
plt.title('age')
plt.show()

#Of disease type (different methos of plotting)
a = pd.crosstab(derm_raw.disease_type,columns="count")
a.plot(kind='pie',subplots=True,autopct='%1.1f%%',labels=['','','','','',''],title='disease_type')
plt.gca().set_aspect('equal')
plt.legend(loc=1,bbox_to_anchor=(1.5,1),labels=['Psoriasis','Seboreic Dermatitis','Lichen Planus','Pityriasis Rosea','Cronic Dermatitis','Pityriasis Rubra Pilaris'])
plt.show()


#correlation (drawn before converting to categorical)
corr=derm_raw.corr()
print(corr)
corr=pd.DataFrame(corr)
print(corr.dtypes)

#plotting corrplot
plt.subplots(figsize=(20,20))
sns.heatmap(corr,vmax=1)
plt.show()


plt.boxplot(derm_raw.age)
plt.title('Age')
plt.show()

#later to do