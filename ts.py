# -*- coding: utf-8 -*-
"""
Created on Mon Mar 25 16:23:32 2019

@author: saimohan
"""

import pandas as pd
import numpy as np
import matplotlib.pyplot as plt
from datetime import datetime
from pandas import Series
%matplotlib inline
import warnings

train=pd.read_csv("Train_SU63ISt.csv")
test=pd.read_csv("Test_0qrQsBZ.csv")

train_original=train.copy()
test_original=test.copy()

train.columns, test.columns

train.dtypes, test.dtypes

train.shape, test.shape

train['Datetime']=pd.to_datetime(train.Datetime, format='%d-%m-%Y %H:%M')
test['Datetime']=pd.to_datetime(test.Datetime, format='%d-%m-%Y %H:%M')

train_original['Datetime']=pd.to_datetime(train_original.Datetime, format='%d-%m-%Y %H:%M')
test_original['Datetime']=pd.to_datetime(test_original.Datetime, format='%d-%m-%Y %H:%M')

for i in (train, test, train_original, test_original):
    i['year']=i.Datetime.dt.year
    i['month']=i.Datetime.dt.month
    i['day']=i.Datetime.dt.day
    i['Hour']=i.Datetime.dt.hour
train['Day of the week']=train['Datetime'].dt.dayofweek
temp=train['Datetime']

def applyer(row):
    if row.dayofweek==5 or row.dayofweek==6:
        return 1
    else:
        return 0
temp2=train['Datetime'].apply(applyer)
train['weekend']=temp2

train.index=train['Datetime']
df=train.drop('ID',1)
ts=df['Count']
plt.figure(figsize=(16,8))
plt.plot(ts, label='Passenger Count')
plt.title('Time Series')
plt.xlabel("Time(year-month)")
plt.ylabel("passenger count")
plt.legend(loc='best')


train.groupby('year')['Count'].mean().plot.bar()


  