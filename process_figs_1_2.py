# -*- coding: utf-8 -*-
"""
Created on Wed May 11 15:08:12 2022

@author: valenter
"""

import pickle as pk
import numpy as np
import pandas as pd
import os
import statsmodels.api as sm
import statsmodels.formula.api as smf
from sklearn.preprocessing import StandardScaler



 
seed = 22345


#load data
fold = os.getcwd()
file = open(fold + "/data_aux/immune_clean_a1.pkl","rb")
data = pk.load(file)
file.close()
del file

#continous
immune_col = data["immune_col"]
## part 1 and 2
d2_d = data["d2_d"]
d3_d = data["d3_d"]
d3_p = data["d3_p"]

df = d2_d.copy()
out = "d2_d"
name = []
beta = []
p = []
betaCI1=[]
betaCI2=[]
scaler = StandardScaler()
x = d2_d[immune_col[28:29]].copy()
x = scaler.fit_transform(x)

ve = []
for var in immune_col:
    try:
        df["var"] =  scaler.fit_transform(df[[var,"sex"]])[:,0]
#        df["var"] =  df[var]
        res = smf.glm(formula=out+"~var+type+sex+age+tx_time", data=df, family=sm.families.Binomial()).fit()
        ci = res.conf_int()
        name.append(var)
        beta.append(np.exp(res.params[1]))
        
        betaCI1.append(np.exp(ci.iloc[1,0]))
        betaCI2.append(np.exp(ci.iloc[1,1]))
        p.append(res.pvalues[1])
    except:
        ve.append(var)
    

d2_d = pd.DataFrame({"var":name,"beta":beta,"ci1":betaCI1,"ci2":betaCI2,"pvalue":p})


df = d3_d.copy()
out = "d3_d"
name = []
beta = []
p = []
betaCI1=[]
betaCI2=[]
scaler = StandardScaler()
ve = []
for var in immune_col:
    try:
        df["var"] =  scaler.fit_transform(df[[var,"sex"]])[:,0]
#        df["var"] =  df[var]
        res = smf.glm(formula=out+"~var+type+sex+age+tx_time", data=df, family=sm.families.Binomial()).fit()
        ci = res.conf_int()
        name.append(var)
        beta.append(np.exp(res.params[1]))        
        betaCI1.append(np.exp(ci.iloc[1,0]))
        betaCI2.append(np.exp(ci.iloc[1,1]))
        p.append(res.pvalues[1])
        
    except:
        ve.append(var)
    

d3_d = pd.DataFrame({"var":name,"beta":beta,"ci1":betaCI1,"ci2":betaCI2,"pvalue":p})

df = d3_p.copy()
out = "d3_p"
name = []
beta = []
p = []
betaCI1=[]
betaCI2=[]
scaler = StandardScaler()
ve = []
for var in immune_col:
    try:
        df["var"] =  scaler.fit_transform(df[[var,"sex"]])[:,0]
        res = smf.glm(formula=out+"~var+type+sex+age+tx_time", data=df, family=sm.families.Binomial()).fit()
        ci = res.conf_int()
        name.append(var)
        beta.append(np.exp(res.params[1]))     
        betaCI1.append(np.exp(ci.iloc[1,0]))
        betaCI2.append(np.exp(ci.iloc[1,1]))
        p.append(res.pvalues[1])
        
        
    except:
        ve.append(var)
    

d3_p = pd.DataFrame({"var":name,"beta":beta,"ci1":betaCI1,"ci2":betaCI2,"pvalue":p})

data = {"d2_d":d2_d,"d3_d":d3_d,"d3_p":d3_p}
file = open(fold + "/data_aux/feature_a1.pkl","wb")
pk.dump(data,file)
file.close()

