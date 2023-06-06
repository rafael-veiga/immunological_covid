# -*- coding: utf-8 -*-
"""
Created on Wed May 11 15:08:12 2022

@author: valenter
"""

import pickle as pk
import os
import pandas as pd
from Tools import SVM,Feature_SVM_RFE,Remove__col_NA,Imputation_mean,Standart,LR
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


def cluster_permutation(model,x,y,clus,seed=0):
    uniq, counts = np.unique(clus, return_counts=True)
    l = []
    for i in range(len(uniq)):
        l.append(list(x.columns[clus==uniq[i]]))
    x_ = x[[a[0] for a in l]]
    model.fit(x_,y)
    per = permutation_importance(model.model, x_, y, n_repeats=100,scoring='roc_auc',random_state=seed)
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
                per = permutation_importance(model.model, x2, y, n_repeats=100,scoring='roc_auc',random_state=seed)
                var.append(l[i][b])
                perm_mean.append(per["importances_mean"][x2.columns==l[i][b]][0])
                perm_sd.append(per["importances_std"][x2.columns==l[i][b]][0])
    df = pd.DataFrame({"var":var,"impor_mean":perm_mean,"impor_sd":perm_sd})
    return df
    
    
file = open(fold + "/data_aux/immune_clean_a1.pkl","rb")
data = pk.load(file)
file.close()
del file

n_outcv = 5
n_incv = 5
n_jobs = 4

immune_col = data["immune_col"]
## part 1 and 2
d2_d = data["d2_d"]
d3_d = data["d3_d"]
d3_p = data["d3_p"]

#d2_d = d2_d.loc[d2_d["batch"]==2,:]
#d3_d = d3_d.loc[d3_d["batch"]==2,:]
#d3_p = d3_p.loc[d3_p["batch"]==2,:]

x_d2_d = d2_d[["age","sex","tx_time","type"]+immune_col].copy()
re = Remove__col_NA(percentage_of_NA=20)
x_d2_d = re.fit_transform(x_d2_d)
immune_col_d2_d = immune_col.copy()
for c in immune_col:
    if not c in x_d2_d.columns:
        immune_col_d2_d.remove(c)
y_d2_d = d2_d["d2_d"].copy()


x_d3_d = d3_d[["age","sex","tx_time","type"]+immune_col].copy()
x_d3_d = re.fit_transform(x_d3_d)
immune_col_d3_d = immune_col.copy()
for c in immune_col:
    if not c in x_d3_d.columns:
        immune_col_d3_d.remove(c)
y_d3_d = d3_d["d3_d"].copy()


x_d3_p = d3_p[["age","sex","tx_time","type"]+immune_col].copy()
x_d3_p = re.fit_transform(x_d3_p)
immune_col_d3_p = immune_col.copy()
for c in immune_col:
    if not c in x_d3_p.columns:
        immune_col_d3_p.remove(c)
y_d3_p = d3_p["d3_p"].copy()




#######################
# remove na
re = Remove__col_NA(percentage_of_NA=30)
x_d2_d = re.fit_transform(x_d2_d)
x_d3_d = re.fit_transform(x_d3_d)
x_d3_p = re.fit_transform(x_d3_p)

##############################################################
#imputation
imp = Imputation_mean()
x_d2_d = imp.fit_transform(x_d2_d)
x_d3_d = imp.fit_transform(x_d3_d)
x_d3_p = imp.fit_transform(x_d3_p)


##############################################################
#stantartization
std =   Standart()
x_d2_d = std.fit_transform(x_d2_d)
x_d3_d = std.fit_transform(x_d3_d)
x_d3_p = std.fit_transform(x_d3_p)

###############################################################
data = {}
cv = 5

f_SVM = Feature_SVM_RFE(immune_col=None,cv=cv,n_jobs=15)
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
#10->29; 20->19;30 - > 0
model = SVM(random_state=seed,intercv = n_incv,n_jobs = n_jobs)
#model = LR(random_state=seed,intercv = n_incv,n_jobs = n_jobs)
df = cluster_permutation(model, x, y, cluster)
data[nome] = {"df":df,"x":x,"y":y}
# ##########################################################
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
#model = LR(random_state=seed,intercv = n_incv,n_jobs = n_jobs)
df = cluster_permutation(model, x, y, cluster)
data[nome] = {"df":df,"x":x,"y":y}
# # ##########################################################
nome = "SVM_RFE_d3_p"
model = f_SVM
x = x_d3_p
y = y_d3_p
print(nome)
model.fit(x, y)
x = x.iloc[:,model.bo.ranking_<10]
print("permitation")
cluster = getCluster(14, x)
# 25 -> 24; 30 -> 24; 35 -> 2;36 -> 2; 40 -> 15; 34 -> 2; 33-> 2; 32 -> 2; 31 -> 2
model = SVM(random_state=seed,intercv = n_incv,n_jobs = n_jobs)
df = cluster_permutation(model, x, y, cluster)
data[nome] = {"df":df,"x":x,"y":y}
# # # ##########################################################
file = open(fold + "/data_aux/feature.dat","wb")
pk.dump(data,file)
file.close()