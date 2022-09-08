# -*- coding: utf-8 -*-
"""
Created on Fri Aug 12 09:55:38 2022

@author: valenter
"""

import pickle as pk
from copy import deepcopy
import os
from Tools import Remove__col_NA,LR,Imputation_mean,SVM,RF,GB,Feature_Boruta,Feature_SVM_RFE,Dicretization,Standart
from sklearn.pipeline import Pipeline
from sklearn.model_selection import StratifiedKFold
from sklearn.metrics import accuracy_score,roc_auc_score,balanced_accuracy_score,precision_score,recall_score
from tempfile import mkdtemp
from joblib import Memory
from sklearn.inspection import permutation_importance
import numpy as np
import pandas as pd


 
seed = 123571113
fold = os.getcwd()
## boruta / SVM-RFE
## load data

file = open(fold + "/data_aux/feature.dat","rb")
comp = pk.load(file)
file.close()
del file

n_outcv = 10
n_incv = 5
n_jobs = 15

x_d2_d = comp["SVM_RFE_d2_d"]["x"]
x_d3_d = comp["SVM_RFE_d3_d"]["x"]

y_d2_d = comp["SVM_RFE_d2_d"]["y"]
y_d3_d = comp["SVM_RFE_d3_d"]["y"]



y_d2_d_train = []
x_d2_d_train = []
y_d2_d_test = []
x_d2_d_test = [] 

y_d3_d_train = []
x_d3_d_train = []
y_d3_d_test = []
x_d3_d_test = [] 

skf = StratifiedKFold(n_splits=n_outcv,random_state=seed,shuffle=True)
for train_index, test_index in skf.split(x_d2_d, y_d2_d):
     x_d2_d_train.append(x_d2_d.iloc[train_index,:].copy())
     y_d2_d_train.append(y_d2_d.iloc[train_index].copy())
     x_d2_d_test.append(x_d2_d.iloc[test_index,:].copy())
     y_d2_d_test.append(y_d2_d.iloc[test_index].copy())


skf = StratifiedKFold(n_splits=n_outcv,random_state=seed,shuffle=True)
for train_index, test_index in skf.split(x_d3_d, y_d3_d):
     x_d3_d_train.append(x_d3_d.iloc[train_index,:].copy())
     y_d3_d_train.append(y_d3_d.iloc[train_index].copy())
     x_d3_d_test.append(x_d3_d.iloc[test_index,:].copy())
     y_d3_d_test.append(y_d3_d.iloc[test_index].copy())

     
def select_feature(x_train,x_test,features):
    x_train_ = x_train.copy()
    x_test_ = x_test.copy()
    for i in range(len(x_test)):
        x_train_[i] = x_train_[i][features]
        x_test_[i] = x_test_[i][features]
    return x_train_.copy(),x_test_.copy()

def run_models(x_train,y_train,x_test,y_test,name,n_jobs,n_intcv):
    comp = {}
    var = []
    accuracy = []
    accuracy_sd = []
    b_accuracy = []
    b_accuracy_sd = []
    auc = []
    auc_sd = []
    #LR
    print("LR")
    model = LR(random_state=seed,intercv = n_intcv,n_jobs = n_jobs)
    acu = []
    b_acu = []
    cur = []
    comp["LR_"+name] = []
    for i in range(len(x_train)):
        comp["LR_"+name].append({})
        model.fit(x_train[i], y_train[i])
        comp["LR_"+name][i]["y_true"] = y_test[i]
        comp["LR_"+name][i]["x_test"] = x_test[i]
        y_pred = model.predict(x_test[i])
        z = model.predict_proba(x_test[i])[:,1]
        comp["LR_"+name][i]["y_pred"] = y_pred
        comp["LR_"+name][i]["z"] = z
        comp["LR_"+name][i]["model"] = deepcopy(model)
        acu.append(accuracy_score(y_test[i],y_pred))
        b_acu.append(balanced_accuracy_score(y_test[i],y_pred))
        cur.append(roc_auc_score(y_test[i], z))
    var.append("LR_"+name)
    accuracy.append(np.mean(acu))
    accuracy_sd.append(np.std(acu))
    b_accuracy.append(np.mean(b_acu))
    b_accuracy_sd.append(np.std(b_acu))
    auc.append(np.mean(cur))
    auc_sd.append(np.std(cur))
    #SVM
    print("SVM")
    model = SVM(random_state=seed,intercv = n_intcv,n_jobs = n_jobs)
    acu = []
    b_acu = []
    cur = []
    comp["SVM_"+name] = []
    for i in range(len(x_train)):
        comp["SVM_"+name].append({})
        model.fit(x_train[i], y_train[i])
        comp["SVM_"+name][i]["y_true"] = y_test[i]
        comp["SVM_"+name][i]["x_test"] = x_test[i]
        y_pred = model.predict(x_test[i])
        z = model.predict_proba(x_test[i])[:,1]
        comp["SVM_"+name][i]["y_pred"] = y_pred
        comp["SVM_"+name][i]["z"] = z
        comp["SVM_"+name][i]["model"] =deepcopy(model)
        acu.append(accuracy_score(y_test[i],y_pred))
        b_acu.append(balanced_accuracy_score(y_test[i],y_pred))
        cur.append(roc_auc_score(y_test[i], z))
    var.append("SVM_"+name)
    accuracy.append(np.mean(acu))
    accuracy_sd.append(np.std(acu))
    b_accuracy.append(np.mean(b_acu))
    b_accuracy_sd.append(np.std(b_acu))
    auc.append(np.mean(cur))
    auc_sd.append(np.std(cur))
    #RF
    print("RF")
    model = RF(random_state=seed,intercv = n_intcv,n_jobs = n_jobs)
    acu = []
    b_acu = []
    cur = []
    comp["RF_"+name] = []
    for i in range(len(x_train)):
        comp["RF_"+name].append({})
        model.fit(x_train[i], y_train[i])
        comp["RF_"+name][i]["y_true"] = y_test[i]
        comp["RF_"+name][i]["x_test"] = x_test[i]
        y_pred = model.predict(x_test[i])
        z = model.predict_proba(x_test[i])[:,1]
        comp["RF_"+name][i]["y_pred"] = y_pred
        comp["RF_"+name][i]["z"] = z
        comp["RF_"+name][i]["model"] =deepcopy(model)
        acu.append(accuracy_score(y_test[i],y_pred))
        b_acu.append(balanced_accuracy_score(y_test[i],y_pred))
        cur.append(roc_auc_score(y_test[i], z))
    var.append("RF_"+name)
    accuracy.append(np.mean(acu))
    accuracy_sd.append(np.std(acu))
    b_accuracy.append(np.mean(b_acu))
    b_accuracy_sd.append(np.std(b_acu))
    auc.append(np.mean(cur))
    auc_sd.append(np.std(cur))
    #GB
    print("GB")
    model = GB(random_state=seed,intercv = n_intcv,n_jobs = n_jobs)
    acu = []
    b_acu = []
    cur = []
    comp["GB_"+name] = []
    for i in range(len(x_train)):
        comp["GB_"+name].append({})
        model.fit(x_train[i], y_train[i])
        comp["GB_"+name][i]["y_true"] = y_test[i]
        comp["GB_"+name][i]["x_test"] = x_test[i]
        y_pred = model.predict(x_test[i])
        z = model.predict_proba(x_test[i])[:,1]
        comp["GB_"+name][i]["y_pred"] = y_pred
        comp["GB_"+name][i]["z"] = z
        comp["GB_"+name][i]["model"] = deepcopy(model)
        acu.append(accuracy_score(y_test[i],y_pred))
        b_acu.append(balanced_accuracy_score(y_test[i],y_pred))
        cur.append(roc_auc_score(y_test[i], z))
    var.append("GB_"+name)
    accuracy.append(np.mean(acu))
    accuracy_sd.append(np.std(acu))
    b_accuracy.append(np.mean(b_acu))
    b_accuracy_sd.append(np.std(b_acu))
    auc.append(np.mean(cur))
    auc_sd.append(np.std(cur))
    
    return pd.DataFrame({"model":var,"accuracy":accuracy,"accuracy_sd":accuracy_sd,
                         "b_accuracy":b_accuracy,"b_accuracy_sd":b_accuracy_sd,
                         "auc":auc,"auc_sd":auc_sd}), comp
data = {}
#################################################################
#SVM_RFE
#d2_d
print("d2_d")
name = "d2_d"
x_train = x_d2_d_train
y_train = y_d2_d_train
x_test = x_d2_d_test
y_test = y_d2_d_test
features = list(comp["SVM_RFE_d2_d"]["df"]["var"])
x_train,x_test = select_feature(x_train,x_test, features)
#x_train,y_train,x_test,y_test,name,n_jobs,n_intcv
df_d2_d,comp_d2_d = run_models(x_train=x_train, y_train=y_train,x_test=x_test,y_test=y_test, name=name, n_jobs=n_jobs, n_intcv=n_incv)
#d3_d
print("d3_d")
name = "d3_d"
x_train = x_d3_d_train
y_train = y_d3_d_train
x_test = x_d3_d_test
y_test = y_d3_d_test
features = list(comp["SVM_RFE_d3_d"]["df"]["var"])
x_train,x_test = select_feature(x_train,x_test, features)
df_d3_d,comp_d3_d = run_models(x_train=x_train, y_train=y_train,x_test=x_test,y_test=y_test, name=name, n_jobs=n_jobs, n_intcv=n_incv)


data["[SVM_RFE]"] = pd.concat([df_d2_d,df_d3_d])
comp = {"SVM_RFE_d2_d":comp_d2_d,"SVM_RFE_d3_d":comp_d3_d}
file = open(fold + "/data_aux/feature_eval.pkl","wb")
pk.dump({"data":data,"comp":comp},file)
file.close()
#################################################################
