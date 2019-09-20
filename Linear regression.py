import numpy as np
import pandas as pd
from pandas import Series, DataFrame
from sklearn.model_selection import train_test_split
#import test and train
train=pd.read_csv("train.csv")
test=pd.read_csv("test.csv")
#importing linear regression from sklearn
from sklearn.linear_model import LinearRegression
lreg=LinearRegression()
#splitting into train 
x=train.loc[:,['Outlet_Establishment_Year','Item_MRP']]
x_train,x_cv,y_train,y_cv=train_test_split(x,train.Item_Outlet_Sales)
#training the model
lreg.fit(x_train,y_train)
#predicting on cv
pred=lreg.predict(x_cv)

mse=np.mean((pred-y_cv)**2)
#calculating coefficients
coeff=DataFrame(x_train.columns)

coeff['Coefficent_Estimate'] = Series(lreg.coef_)

coeff


x = train.loc[:,['Outlet_Establishment_Year','Item_MRP','Item_Weight']]
# splitting into training and cv for cross validation
x_train, x_cv, y_train, y_cv = train_test_split(x,train.Item_Outlet_Sales)
## training the model 
lreg.fit(x_train,y_train) 

