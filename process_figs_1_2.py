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
from Tools import Dicretization



 
seed = 22345


 #load data
fold = os.getcwd()
# file = open(fold + "/data_aux/immune_clean_a1.pkl","rb")
# data = pk.load(file)
# file.close()
# del file

# #continous
# immune_col = data["immune_col"]
# ## part 1 and 2
# d2_d = data["d2_d"]
# d3_d = data["d3_d"]
# d3_p = data["d3_p"]

# df = d2_d.copy()
# out = "d2_d"
# name = []
# beta = []
# p = []
# betaCI1=[]
# betaCI2=[]
# scaler = StandardScaler()
# x = d2_d[immune_col[28:29]].copy()
# x = scaler.fit_transform(x)

# ve = []
# for var in immune_col:
#     try:
#         df["var"] =  scaler.fit_transform(df[[var,"sex"]])[:,0]
# #        df["var"] =  df[var]
#         res = smf.glm(formula=out+"~var+type+sex+age+tx_time", data=df, family=sm.families.Binomial()).fit()
#         ci = res.conf_int()
#         name.append(var)
#         beta.append(np.exp(res.params[1]))
        
#         betaCI1.append(np.exp(ci.iloc[1,0]))
#         betaCI2.append(np.exp(ci.iloc[1,1]))
#         p.append(res.pvalues[1])
#     except:
#         ve.append(var)
    

# d2_d = pd.DataFrame({"var":name,"or":beta,"ci1":betaCI1,"ci2":betaCI2,"pvalue":p})


# df = d3_d.copy()
# out = "d3_d"
# name = []
# beta = []
# p = []
# betaCI1=[]
# betaCI2=[]
# scaler = StandardScaler()
# ve = []
# for var in immune_col:
#     try:
#         df["var"] =  scaler.fit_transform(df[[var,"sex"]])[:,0]
# #        df["var"] =  df[var]
#         res = smf.glm(formula=out+"~var+type+sex+age+tx_time", data=df, family=sm.families.Binomial()).fit()
#         ci = res.conf_int()
#         name.append(var)
#         beta.append(np.exp(res.params[1]))        
#         betaCI1.append(np.exp(ci.iloc[1,0]))
#         betaCI2.append(np.exp(ci.iloc[1,1]))
#         p.append(res.pvalues[1])
        
#     except:
#         ve.append(var)
    

# d3_d = pd.DataFrame({"var":name,"or":beta,"ci1":betaCI1,"ci2":betaCI2,"pvalue":p})

# df = d3_p.copy()
# out = "d3_p"
# name = []
# beta = []
# p = []
# betaCI1=[]
# betaCI2=[]
# scaler = StandardScaler()
# ve = []
# for var in immune_col:
#     try:
#         df["var"] =  scaler.fit_transform(df[[var,"sex"]])[:,0]
#         res = smf.glm(formula=out+"~var+type+sex+age+tx_time", data=df, family=sm.families.Binomial()).fit()
#         ci = res.conf_int()
#         name.append(var)
#         beta.append(np.exp(res.params[1]))     
#         betaCI1.append(np.exp(ci.iloc[1,0]))
#         betaCI2.append(np.exp(ci.iloc[1,1]))
#         p.append(res.pvalues[1])
        
        
#     except:
#         ve.append(var)
    

# d3_p = pd.DataFrame({"var":name,"or":beta,"ci1":betaCI1,"ci2":betaCI2,"pvalue":p})

# data = {"d2_d":d2_d,"d3_d":d3_d,"d3_p":d3_p}
# file = open(fold + "/data_aux/feature_a1.pkl","wb")
# pk.dump(data,file)
# file.close()

# d2_d = d2_d.sort_values("or",ascending=False)
# d3_d = d3_d.sort_values("or",ascending=False)
# d3_p = d3_p.sort_values("or",ascending=False)

# d2_d.to_csv(fold +"/data_aux/cont_or_d2_d.csv",index=False)
# d3_d.to_csv(fold +"/data_aux/cont_or_d3_d.csv",index=False)
# d3_p.to_csv(fold +"/data_aux/cont_or_d3_p.csv",index=False)

# # discrete
# file = open(fold + "/data_aux/immune_clean_a1.pkl","rb")
# data = pk.load(file)
# file.close()
# del file

# immune_col = data["immune_col"]
# ## part 1 and 2
# d2_d = data["d2_d"]
# d3_d = data["d3_d"]
# d3_p = data["d3_p"]

# out = "d2_d"
# name = []
# beta1 = []
# beta2 = []
# p1 = []
# p2 = []
# beta1CI1=[]
# beta1CI2=[]
# beta2CI1=[]
# beta2CI2=[]
# cut = []
# disc = Dicretization(immune_col)
# d2_d = disc.fit_transform(d2_d, d2_d["d2_d"])
# df = disc.get_mapa()
# ve = []
# for var in immune_col:
#     try:
#         d2_d["var"] =  d2_d[var]
#         res = smf.glm(formula=out+"~var+type+sex+age+tx_time", data=d2_d, family=sm.families.Binomial()).fit()
        
#         name.append(var)
#         p1.append(res.pvalues[1])
#         p2.append(res.pvalues[2])
#         cut.append(list(df.loc[df["var"]==var,"cutof"])[0])
#         beta1.append(np.exp(res.params[1]))
#         beta2.append(np.exp(res.params[2]))
#         ci = res.conf_int()
#         beta1CI1.append(np.exp(ci.iloc[1,0]))
#         beta1CI2.append(np.exp(ci.iloc[1,1]))
#         beta2CI1.append(np.exp(ci.iloc[2,0]))
#         beta2CI2.append(np.exp(ci.iloc[2,1]))
        
        
#     except:
#         ve.append(var)

# d2_d = pd.DataFrame({"var":name,"cutof":cut,
#                      "OR_medXlow":beta1,"CI_1_medXlow":beta1CI1,"CI_2_medXlow":beta1CI2,"p_value_medXlow":p1,
#                      "OR_highXlow":beta2,"CI_1_highXlow":beta2CI1,"CI_2_highXlow":beta2CI2,"p_value_highXlow":p2})  
# d2_d.to_csv(fold +"/data_aux/disc_or_d2_d.csv",index=False)

# out = "d3_d"
# name = []
# beta1 = []
# beta2 = []
# p1 = []
# p2 = []
# beta1CI1=[]
# beta1CI2=[]
# beta2CI1=[]
# beta2CI2=[]
# cut = []
# disc = Dicretization(immune_col)
# d3_d = disc.fit_transform(d3_d, d3_d["d3_d"])
# df = disc.get_mapa()
# ve = []
# for var in immune_col:
#     try:
#         d3_d["var"] =  d3_d[var]
#         res = smf.glm(formula=out+"~var+type+sex+age+tx_time", data=d3_d, family=sm.families.Binomial()).fit()
        
#         name.append(var)
#         p1.append(res.pvalues[1])
#         p2.append(res.pvalues[2])
#         cut.append(list(df.loc[df["var"]==var,"cutof"])[0])
#         beta1.append(np.exp(res.params[1]))
#         beta2.append(np.exp(res.params[2]))
#         ci = res.conf_int()
#         beta1CI1.append(np.exp(ci.iloc[1,0]))
#         beta1CI2.append(np.exp(ci.iloc[1,1]))
#         beta2CI1.append(np.exp(ci.iloc[2,0]))
#         beta2CI2.append(np.exp(ci.iloc[2,1]))
        
        
#     except:
#         ve.append(var)

# d3_d = pd.DataFrame({"var":name,"cutof":cut,
#                      "OR_medXlow":beta1,"CI_1_medXlow":beta1CI1,"CI_2_medXlow":beta1CI2,"p_value_medXlow":p1,
#                      "OR_highXlow":beta2,"CI_1_highXlow":beta2CI1,"CI_2_highXlow":beta2CI2,"p_value_highXlow":p2})  
# d3_d.to_csv(fold +"/data_aux/disc_or_d3_d.csv",index=False)

# out = "d3_p"
# name = []
# beta1 = []
# beta2 = []
# p1 = []
# p2 = []
# beta1CI1=[]
# beta1CI2=[]
# beta2CI1=[]
# beta2CI2=[]
# cut = []
# disc = Dicretization(immune_col)
# d3_p = disc.fit_transform(d3_p, d3_p["d3_p"])
# df = disc.get_mapa()
# ve = []
# for var in immune_col:
#     try:
#         d3_p["var"] =  d3_p[var]
#         res = smf.glm(formula=out+"~var+type+sex+age+tx_time", data=d3_p, family=sm.families.Binomial()).fit()
        
#         name.append(var)
#         p1.append(res.pvalues[1])
#         p2.append(res.pvalues[2])
#         cut.append(list(df.loc[df["var"]==var,"cutof"])[0])
#         beta1.append(np.exp(res.params[1]))
#         beta2.append(np.exp(res.params[2]))
#         ci = res.conf_int()
#         beta1CI1.append(np.exp(ci.iloc[1,0]))
#         beta1CI2.append(np.exp(ci.iloc[1,1]))
#         beta2CI1.append(np.exp(ci.iloc[2,0]))
#         beta2CI2.append(np.exp(ci.iloc[2,1]))
        
        
#     except:
#         ve.append(var)

# d3_p = pd.DataFrame({"var":name,"cutof":cut,
#                      "OR_medXlow":beta1,"CI_1_medXlow":beta1CI1,"CI_2_medXlow":beta1CI2,"p_value_medXlow":p1,
#                      "OR_highXlow":beta2,"CI_1_highXlow":beta2CI1,"CI_2_highXlow":beta2CI2,"p_value_highXlow":p2})  
# d3_p.to_csv(fold +"/data_aux/disc_or_d3_p.csv",index=False)


#batch2
file = open(fold + "/data_aux/immune_clean_a1.pkl","rb")
data = pk.load(file)
file.close()
del file

immune_col = data["immune_col"]
## part 1 and 2
d2_d = data["d2_d"]
d3_d = data["d3_d"]
d3_p = data["d3_p"]
d2_d = d2_d.loc[d2_d["batch"]==2,:]
d3_d = d3_d.loc[d3_d["batch"]==2,:]
d3_p = d3_p.loc[d3_p["batch"]==2,:]
out = "d2_d"
name = []
beta1 = []
beta2 = []
p1 = []
p2 = []
beta1CI1=[]
beta1CI2=[]
beta2CI1=[]
beta2CI2=[]
cut = []
for var in immune_col:
    if len(d2_d[d2_d[var].isna()!=True])==0:
        immune_col.remove(var)

disc = Dicretization(immune_col)
d2_d = disc.fit_transform(d2_d, d2_d["d2_d"])
df = disc.get_mapa()
ve = []
for var in immune_col:
    try:
        d2_d["var"] =  d2_d[var]
        res = smf.glm(formula=out+"~var+type+sex+age+tx_time", data=d2_d, family=sm.families.Binomial()).fit()
        
        name.append(var)
        p1.append(res.pvalues[1])
        p2.append(res.pvalues[2])
        cut.append(list(df.loc[df["var"]==var,"cutof"])[0])
        beta1.append(np.exp(res.params[1]))
        beta2.append(np.exp(res.params[2]))
        ci = res.conf_int()
        beta1CI1.append(np.exp(ci.iloc[1,0]))
        beta1CI2.append(np.exp(ci.iloc[1,1]))
        beta2CI1.append(np.exp(ci.iloc[2,0]))
        beta2CI2.append(np.exp(ci.iloc[2,1]))
        
        
    except:
        ve.append(var)

d2_d = pd.DataFrame({"var":name,"cutof":cut,
                      "OR_medXlow":beta1,"CI_1_medXlow":beta1CI1,"CI_2_medXlow":beta1CI2,"p_value_medXlow":p1,
                      "OR_highXlow":beta2,"CI_1_highXlow":beta2CI1,"CI_2_highXlow":beta2CI2,"p_value_highXlow":p2})  
d2_d.to_csv(fold +"/data_aux/batch2_disc_or_d2_d.csv",index=False)

out = "d3_d"
name = []
beta1 = []
beta2 = []
p1 = []
p2 = []
beta1CI1=[]
beta1CI2=[]
beta2CI1=[]
beta2CI2=[]
cut = []
immune_col = data["immune_col"]
for var in immune_col:
    if len(d3_d[d3_d[var].isna()!=True])==0:
        immune_col.remove(var)
disc = Dicretization(immune_col)
d3_d = disc.fit_transform(d3_d, d3_d["d3_d"])
df = disc.get_mapa()
ve = []
for var in immune_col:
    try:
        d3_d["var"] =  d3_d[var]
        res = smf.glm(formula=out+"~var+type+sex+age+tx_time", data=d3_d, family=sm.families.Binomial()).fit()
        
        name.append(var)
        p1.append(res.pvalues[1])
        p2.append(res.pvalues[2])
        cut.append(list(df.loc[df["var"]==var,"cutof"])[0])
        beta1.append(np.exp(res.params[1]))
        beta2.append(np.exp(res.params[2]))
        ci = res.conf_int()
        beta1CI1.append(np.exp(ci.iloc[1,0]))
        beta1CI2.append(np.exp(ci.iloc[1,1]))
        beta2CI1.append(np.exp(ci.iloc[2,0]))
        beta2CI2.append(np.exp(ci.iloc[2,1]))
        
        
    except:
        ve.append(var)

d3_d = pd.DataFrame({"var":name,"cutof":cut,
                      "OR_medXlow":beta1,"CI_1_medXlow":beta1CI1,"CI_2_medXlow":beta1CI2,"p_value_medXlow":p1,
                      "OR_highXlow":beta2,"CI_1_highXlow":beta2CI1,"CI_2_highXlow":beta2CI2,"p_value_highXlow":p2})  
d3_d.to_csv(fold +"/data_aux/batch2_disc_or_d3_d.csv",index=False)

out = "d3_p"
name = []
beta1 = []
beta2 = []
p1 = []
p2 = []
beta1CI1=[]
beta1CI2=[]
beta2CI1=[]
beta2CI2=[]
cut = []
immune_col = data["immune_col"]
for var in immune_col:
    if len(d3_p[d3_p[var].isna()!=True])==0:
        immune_col.remove(var)
disc = Dicretization(immune_col)
d3_p = disc.fit_transform(d3_p, d3_p["d3_p"])
df = disc.get_mapa()
ve = []
for var in immune_col:
    try:
        d3_p["var"] =  d3_p[var]
        res = smf.glm(formula=out+"~var+type+sex+age+tx_time", data=d3_p, family=sm.families.Binomial()).fit()
        
        name.append(var)
        p1.append(res.pvalues[1])
        p2.append(res.pvalues[2])
        cut.append(list(df.loc[df["var"]==var,"cutof"])[0])
        beta1.append(np.exp(res.params[1]))
        beta2.append(np.exp(res.params[2]))
        ci = res.conf_int()
        beta1CI1.append(np.exp(ci.iloc[1,0]))
        beta1CI2.append(np.exp(ci.iloc[1,1]))
        beta2CI1.append(np.exp(ci.iloc[2,0]))
        beta2CI2.append(np.exp(ci.iloc[2,1]))
        
        
    except:
        ve.append(var)

d3_p = pd.DataFrame({"var":name,"cutof":cut,
                      "OR_medXlow":beta1,"CI_1_medXlow":beta1CI1,"CI_2_medXlow":beta1CI2,"p_value_medXlow":p1,
                      "OR_highXlow":beta2,"CI_1_highXlow":beta2CI1,"CI_2_highXlow":beta2CI2,"p_value_highXlow":p2})  
d3_p.to_csv(fold +"/data_aux/batch2_disc_or_d3_p.csv",index=False)


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
d2_d = d2_d.loc[d2_d["batch"]==2,:]
d3_d = d3_d.loc[d3_d["batch"]==2,:]
d3_p = d3_p.loc[d3_p["batch"]==2,:]

df = d2_d.copy()
out = "d2_d"
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
    

d2_d = pd.DataFrame({"var":name,"or":beta,"ci1":betaCI1,"ci2":betaCI2,"pvalue":p})


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
    

d3_d = pd.DataFrame({"var":name,"or":beta,"ci1":betaCI1,"ci2":betaCI2,"pvalue":p})

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
    

d3_p = pd.DataFrame({"var":name,"or":beta,"ci1":betaCI1,"ci2":betaCI2,"pvalue":p})


d2_d = d2_d.sort_values("or",ascending=False)
d3_d = d3_d.sort_values("or",ascending=False)
d3_p = d3_p.sort_values("or",ascending=False)

d2_d.to_csv(fold +"/data_aux/batch2_cont_or_d2_d.csv",index=False)
d3_d.to_csv(fold +"/data_aux/batch2_cont_or_d3_d.csv",index=False)
d3_p.to_csv(fold +"/data_aux/batch2_cont_or_d3_p.csv",index=False)
