# -*- coding: utf-8 -*-
"""
Created on Tue May 26 21:49:38 2020

@author: RAVI
"""
# Multilinear Regression
import pandas as pd
import numpy as np


# loading the data
startup = pd.read_csv("C:/RAVI/Data science/Assignments/Module 7 Multiple linear regression/MLR Assignment Q1 Dataset/50_Startups.csv/50_Startups.csv")
startup.describe()

#Convert the column into categorical columns
startup.State=pd.get_dummies(startup['State'],drop_first=True)

# Scatter plot between the variables along with histograms
import seaborn as sns
sns.pairplot(startup.iloc[:,:])

# Correlation matrix 
startup.corr()

startup.columns="Profit","RD_Spend","Administration","Marketing_Spend","State"

# preparing model considering all the variables 
import statsmodels.formula.api as smf # for regression model
ml1 = smf.ols('Profit ~ RD_Spend + Administration + Marketing_Spend + State',data=startup).fit() # regression model
ml1.summary()


#if Coefficients are insignificant then check the model for individual inputs
m_RD=smf.ols('Profit ~ RD_Spend',data=startup).fit()
m_RD.summary()

m_ADM=smf.ols('Profit ~ Administration',data=startup).fit()
m_ADM.summary()

m_MRSPEND=smf.ols('Profit ~ Marketing_Spend',data=startup).fit()
m_MRSPEND.summary()

#Check combinatin of input variables
m_comb=smf.ols('Profit ~ RD_Spend+Administration+Marketing_Spend',data=startup).fit()
m_comb.summary() #one of independent variable p value not statistically significant

# calculating VIF's values of independent variables
rsq_RD_Spend = smf.ols('RD_Spend ~ Administration + Marketing_Spend + State',data=startup).fit().rsquared  
vif_RD_Spend = 1/(1-rsq_RD_Spend)

rsq_Administration = smf.ols('Administration ~ RD_Spend + Marketing_Spend + State',data=startup).fit().rsquared  
vif_Administration = 1/(1-rsq_Administration)

rsq_Marketing_Spend = smf.ols('Marketing_Spend ~ RD_Spend + Administration + State',data=startup).fit().rsquared  
vif_Marketing_Spend = 1/(1-rsq_Marketing_Spend)

rsq_State = smf.ols('State ~ RD_Spend + Administration + Marketing_Spend',data=startup).fit().rsquared  
vif_State = 1/(1-rsq_State)

# Storing vif values in a data frame
d1 = {'Variables':['RD_Spend','Administration','Marketing_Spend','State'],'VIF':[vif_RD_Spend,vif_Administration,vif_Marketing_Spend,vif_State]}
Vif_frame = pd.DataFrame(d1)  
Vif_frame
# As State and Administration is having higher VIF value, we are not going to include this prediction model

# final model
final_ml= smf.ols('Profit ~ RD_Spend + Marketing_Spend ',data=startup).fit()
final_ml.summary()


### Splitting the data into train and test data 
from sklearn.model_selection import train_test_split
startup_train,startup_test  = train_test_split(startup,test_size = 0.3) # 30% test data

# preparing the model on train data 
model_train = smf.ols('Profit ~ RD_Spend  +  Marketing_Spend',data=startup_train).fit()


# train_data prediction
train_pred = model_train.predict(startup_train)

# train residual values 
train_resid  = train_pred - startup_train.Profit
train_resid 

# RMSE value for train data 
train_rmse = np.sqrt(np.mean(train_resid*train_resid))  #44476.02
train_rmse

# prediction on test data set 
test_pred = model_train.predict(startup_test)

# test residual values 
test_resid  = test_pred - startup_test.Profit

# RMSE value for test data 
test_rmse = np.sqrt(np.mean(test_resid*test_resid))   #42859.30
test_rmse










