# -*- coding: utf-8 -*-
"""
Created on Wed May 11 15:08:12 2022

@author: valenter
"""

import pickle as pk
import os
import pandas as pd
from Tools import SVM,RF,GB,Feature_SVM_RFE
from sklearn.pipeline import Pipeline
from sklearn.model_selection import StratifiedKFold
from sklearn.metrics import accuracy_score,roc_auc_score,balanced_accuracy_score,precision_score,recall_score
from tempfile import mkdtemp
from joblib import Memory
from sklearn.inspection import permutation_importance
from scipy.stats import spearmanr
from scipy.cluster import hierarchy
import numpy as np
from scipy.spatial.distance import squareform
#cachedir = mkdtemp()
#memory = Memory(cachedir=cachedir, verbose=1)
 
seed = 123571113
fold = os.getcwd()

def getCluster(n,x):
    corr = spearmanr(x).correlation
    corr = (corr + corr.T) / 2
    np.fill_diagonal(corr, 1)
    distance_matrix = 1 - np.abs(corr)
    dist_linkage = hierarchy.ward(squareform(distance_matrix))
    corte = len(x.columns)-n
    for i in [0.001,0.02,0.05,0.07,0.1,0.2,0.3,0.4,0.5,0.6,0.7,0.9,1.2,1.5,2,2.5,3]:
        cluster_ids = hierarchy.fcluster(dist_linkage, i, criterion="distance")
        if max(cluster_ids)<corte+1:
            return cluster_ids
    
    return cluster_ids


def cluster_permutation(model,x,y,clus):
    uniq, counts = np.unique(clus, return_counts=True)
    l = []
    for i in range(len(uniq)):
        l.append(list(x.columns[clus==uniq[i]]))
    x_ = x[[a[0] for a in l]]
    model.fit(x_,y)
    per = permutation_importance(model.model, x_, y, n_repeats=100,scoring='roc_auc')
    var = []
    perm_mean = []
    perm_sd = []
    for i in range(len(x_.columns)):
        var.append(x_.columns[i])
        perm_mean.append(per["importances_mean"][i])
        perm_sd.append(per["importances_std"][i])
    
    for i in range(len(l)):
        if len(l[i])>1:
            for b in range(1,len(l[i])):
                x2 = x_.copy()
                del x2[l[i][0]]
                x2[l[i][b]] = x[l[i][b]]
                model.model.fit(x2,y)
                per = permutation_importance(model.model, x2, y, n_repeats=100,scoring='roc_auc')
                var.append(l[i][b])
                perm_mean.append(per["importances_mean"][x2.columns==l[i][b]][0])
                perm_sd.append(per["importances_std"][x2.columns==l[i][b]][0])
    df = pd.DataFrame({"var":var,"impor_mean":perm_mean,"impor_sd":perm_sd})
    return df
    
## load data
file = open(fold + "/data_aux/immune_clean_ml.pkl","rb")
data = pk.load(file)
file.close()
del file

n_outcv = 10
n_incv = 5
n_jobs = 15

immune_col_d2_d = data["immune_col_d2_d"].copy()
immune_col_d3_d = data["immune_col_d3_d"].copy()
x_d2_d = data["x_d2_d"].copy()
y_d2_d = data["y_d2_d"].copy()
x_d3_d = data["x_d3_d"].copy()
y_d3_d = data["y_d3_d"].copy()

del data

###############################################################
data = {}


f_SVM = Feature_SVM_RFE(immune_col=None,outcv=n_outcv,incv=n_incv,n_jobs=n_jobs)

# ###############################################################
nome = "SVM_RFE_d2_d"
print(nome)
model = f_SVM
x = x_d2_d
y = y_d2_d

model.fit(x, y)
x = x.iloc[:,model.bo.support_]
print("permitation")
cluster = getCluster(10, x)
model = SVM(random_state=seed,intercv = n_incv,n_jobs = n_jobs)
df = cluster_permutation(model, x, y, cluster)
data[nome] = {"df":df,"x":x,"y":y}
# # ##########################################################
nome = "SVM_RFE_d3_d"
model = f_SVM
x = x_d3_d
y = y_d3_d
print(nome)
model.fit(x, y)
x = x.iloc[:,model.bo.support_]
print("permitation")
cluster = getCluster(95, x)
model = SVM(random_state=seed,intercv = n_incv,n_jobs = n_jobs)
df = cluster_permutation(model, x, y, cluster)
data[nome] = {"df":df,"x":x,"y":y}
# # # ##########################################################
file = open(fold + "/data_aux/feature_ml.dat","wb")
pk.dump(data,file)
file.close()
