# -*- coding: utf-8 -*-
"""
Created on Wed May 11 10:16:16 2022

@author: valenter
"""

from Tools import Data,Remove__col_NA,Imputation_mean,Standart,Imputation,Remove__Outliers
import numpy as np
import pickle as pk
import os
seed = 123571113
fold = os.getcwd()

#export data to R
data = Data()
data.load(fold + "/data_aux/banco_meta.dat")
data = data.get_dataset()
#table 1
data = data[(data.type=="k")|(data.type=="l")|(data.type=="i") ]
data.to_csv(fold + "/data_aux/table1.csv",index=False)
#table 2
data = data[(data.type=="k")|(data.type=="l") ]
data.to_csv(fold + "/data_aux/table2.csv",index=False)


#ML, fig 1, fig 2
# load data
data = Data()
fold = os.getcwd()
data.load(fold + "/data_aux/banco_immune.dat")
    
## select kidney and lung patients
banco = data._Data__data
fil = (banco.type=="k")|(banco.type=="l")
data.filter_sample(fil)
meta = data.get_col_name(modo="type",key="meta")
median = data.get_col_name(modo="type",key="median")
proportion = data.get_col_name(modo="type",key="proportion")
banco = data._Data__data.copy()


print("immune total var: "+str(len(median + proportion)))

immune_col = median + proportion

## add age, sex, type and outcome
age = []
sex = []
tp = []
tx = []
d2_d = []
d3_d = []
d3_p = []

meta = Data()
meta.load(fold + "/data_aux/banco_meta.dat")

col = ["Sample","age","sex","type","tx_time","D2_detect","D3_detect","D3_pos"]
meta = meta.get_dataset(cols=col)
sam = list(data.get_dataset(cols=["Sample"]).Sample)
for s in sam:
    age.append(list(meta.loc[meta.Sample==s].age)[0])
    sex.append(list(meta.loc[meta.Sample==s].sex)[0])
    tp.append(list(meta.loc[meta.Sample==s].type)[0])
    tx.append(list(meta.loc[meta.Sample==s].tx_time)[0])
    d2_d.append(list(meta.loc[meta.Sample==s].D2_detect)[0])
    d3_d.append(list(meta.loc[meta.Sample==s].D3_detect)[0])
    d3_p.append(list(meta.loc[meta.Sample==s].D3_pos)[0])

for i in range(len(sex)):
    if sex[i]=="f":
        sex[i] = 0
    if sex[i]=="m":
        sex[i]=1
    if tp[i]=="k":
        tp[i]=0
    if tp[i]=="l":
        tp[i] = 1

data.add_var(age, "age", "meta")
data.add_var(sex, "sex", "meta")
data.add_var(tp, "type", "meta")
data.add_var(tx, "tx_time", "meta")
data.add_var(d2_d, "d2_d", "outcome")
data.add_var(d3_d, "d3_d", "outcome")
data.add_var(d3_p, "d3_p", "outcome")


data.save(fold + "/data_aux/immune_clean_1.dat")

banco = data.get_dataset()
d2_d = banco.copy()
d3_d = banco.copy()
d3_p = banco.copy()
d2_d = d2_d[~d2_d.d2_d.isna()]
d3_d = d3_d[~d3_d.d3_d.isna()]
d3_p = d3_p[~d3_p.d3_p.isna()]
#d2_d["d2_d"] = d2_d["d2_d"].astype('category')
del d2_d["d3_d"]
del d2_d["d3_p"]
#d3_d["d3_d"] = d3_d["d3_d"].astype('category')
del d3_d["d2_d"]
del d3_d["d3_p"]
#d3_p["d3_p"] = d3_p["d3_p"].astype('category')
del d3_p["d3_d"]
del d3_p["d2_d"]




comp = {"d2_d":d2_d,"d3_d":d3_d,"d3_p":d3_p,"immune_col":immune_col}
file = open(fold + "/data_aux/immune_clean_a1.pkl","wb")
pk.dump(comp,file)
file.close()

#fig 3
# load data
data = Data()
fold = os.getcwd()
data.load(fold + "/data_aux/banco_immune.dat")
    
## select kidney and lung patients
banco = data._Data__data
fil = (banco.type=="k")|(banco.type=="l") |(banco.type=="i")
data.filter_sample(fil)
meta = data.get_col_name(modo="type",key="meta")
median = data.get_col_name(modo="type",key="median")
proportion = data.get_col_name(modo="type",key="proportion")
banco = data._Data__data.copy()


print("immune total var: "+str(len(median + proportion)))

immune_col = median + proportion

## add age, sex, type and outcome


gr_d2_d = []
gr_d3_d = []
gr_d3_p = []

mmf=[]
ctc = []

meta = Data()
meta.load(fold + "/data_aux/banco_meta.dat")

col = ["Sample","age","sex","type","tx_time","D2_detect","D3_detect","D3_pos","MMF","CTC"]
meta = meta.get_dataset(cols=col)

sam = list(data.get_dataset(cols=["Sample"]).Sample)
for s in sam:
    if int(meta.loc[meta.Sample==s].MMF)==0:
        mmf.append(0)
    elif int(meta.loc[meta.Sample==s].MMF)==1:
        mmf.append(1)
    else:
        mmf.append(np.na)
    if int(meta.loc[meta.Sample==s].CTC)==0:
        ctc.append(0)
    elif int(meta.loc[meta.Sample==s].CTC)==1:
        ctc.append(1)
    else:
        ctc.append(np.na)
    if meta.loc[meta.Sample==s].type.iloc[0] =="i":
        gr_d2_d.append("h")
        gr_d3_d.append("h")
        gr_d3_p.append("h")
    else:
        if meta.loc[meta.Sample==s].D2_detect.iloc[0] ==1:
            gr_d2_d.append("d")
        elif meta.loc[meta.Sample==s].D2_detect.iloc[0] ==0:
            gr_d2_d.append("n")
        else:
            gr_d2_d.append(np.nan)
        
        if meta.loc[meta.Sample==s].D3_detect.iloc[0] ==1:
            gr_d3_d.append("d")
        elif meta.loc[meta.Sample==s].D3_detect.iloc[0] ==0:
            gr_d3_d.append("n")
        else:
            gr_d3_d.append(np.nan)
            
        if meta.loc[meta.Sample==s].D3_pos.iloc[0] ==1:
            gr_d3_p.append("d")
        elif meta.loc[meta.Sample==s].D3_pos.iloc[0] ==0:
            gr_d3_p.append("n")
        else:
            gr_d3_p.append(np.nan)
            


data.add_var(gr_d2_d, "gr_d2_d", "outcome")
data.add_var(gr_d3_d, "gr_d3_d", "outcome")
data.add_var(gr_d3_p, "gr_d3_p", "outcome")
data.add_var(mmf, "MMF", "outcome")
data.add_var(ctc, "CTC", "outcome")

banco = data.get_dataset()
d2_d = banco.copy()
d3_d = banco.copy()
d3_p = banco.copy()
d2_d = d2_d[~d2_d.gr_d2_d.isna()]
d3_d = d3_d[~d3_d.gr_d3_d.isna()]
d3_p = d3_p[~d3_p.gr_d3_p.isna()]

#d2_d["d2_d"] = d2_d["d2_d"].astype('category')
del d2_d["gr_d3_d"]
del d2_d["gr_d3_p"]

del d3_d["gr_d2_d"]
del d3_d["gr_d3_p"]

del d3_p["gr_d2_d"]
del d3_p["gr_d3_d"]

comp = {"d2_d":d2_d,"d3_d":d3_d,"d3_p":d3_p}
file = open(fold + "/data_aux/immune_clean_a3.pkl","wb")
pk.dump(comp,file)
file.close()


x_d2_d = d2_d[immune_col].copy()
y_d2_d = d2_d.gr_d2_d.copy()
y2_d2_d = d2_d.MMF.copy()
y3_d2_d = d2_d.CTC.copy()

x_d3_d = d3_d[immune_col].copy()
y_d3_d = d3_d.gr_d3_d.copy()
y2_d3_d = d3_d.MMF.copy()
y3_d3_d = d3_d.CTC.copy()

x_d3_p = d3_p[immune_col].copy()
y_d3_p = d3_p.gr_d3_p.copy()
y2_d3_p = d3_p.MMF.copy()
y3_d3_p = d3_p.CTC.copy()

#######################
# remove na
#re = Remove__col_NA(percentage_of_NA=30)
#x_d2_d = re.fit_transform(x_d2_d)
#x_d3_d = re.fit_transform(x_d3_d)
#x_d3_p = re.fit_transform(x_d3_p)
#####################################

# remove outliers
re = Remove__Outliers(std=3)
x_d2_d = re.fit_transform(x_d2_d)
x_d3_d = re.fit_transform(x_d3_d)
x_d3_p = re.fit_transform(x_d3_p)
#################################
#imputation
imp = Imputation_mean()
#imp = Imputation(immune_col=immune_col)
x_d2_d = imp.fit_transform(x_d2_d)
x_d3_d = imp.fit_transform(x_d3_d)
x_d3_p = imp.fit_transform(x_d3_p)


##############################################################
#stantartization
std =   Standart()
x_d2_d = std.fit_transform(x_d2_d)
x_d3_d = std.fit_transform(x_d3_d)
x_d3_p = std.fit_transform(x_d3_p)
##############################################################

comp = {"x_d2_d":x_d2_d,"x_d3_d":x_d3_d,"x_d3_p":x_d3_p,
        "y_d2_d":y_d2_d,"y_d3_d":y_d3_d,"y_d3_p":y_d3_p,
        "y2_d2_d":y2_d2_d,"y2_d3_d":y2_d3_d,"y2_d3_p":y2_d3_p,
        "y3_d2_d":y3_d2_d,"y3_d3_d":y3_d3_d,"y3_d3_p":y3_d3_p
        }

file = open(fold + "/data_aux/immune_clean_a2.pkl","wb")
pk.dump(comp,file)
file.close()
