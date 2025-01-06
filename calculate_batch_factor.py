# -*- coding: utf-8 -*-
"""
Created on Fri Jan 28 10:29:35 2022

@author: valenter
"""

import pandas as pd
import numpy as np
from scipy import stats
import statistics
import os

fold = os.getcwd() 
# create dataset
banco = pd.read_csv(fold + "/analisis/data/batch1_activation_statistics_final.csv")
# banco_u = pd.read_csv("M:/EXIMIOUS/normalizarion_flow/autogatingdata/analisis/data/batch1_activation_statistics_updated.csv")

# for c in banco.columns:
#     if c not in banco_u.columns:
#         banco_u[c] = banco[c]

# for c in banco.columns:
#     if c not in banco_u.columns:
#         print(c)
# banco = banco_u
batch2 = pd.read_csv("M:/EXIMIOUS/normalizarion_flow/autogatingdata/analisis/data/batch2_activation_statistics.csv")

banco["batch"] = 1
batch2["batch"] = 2
banco = pd.concat([banco,batch2], ignore_index=True, sort=False)
del batch2
#create col labels
pop = []
mark = []
ty = [] 
c = banco.columns.values
pscale = []
scale = []

for i in range(len(c)):
    aux = c[i].split(":")
    if len(aux)>1:
        pop.append(aux[2].strip())
        aux = aux[0].strip().split(" ")
        mark.append(aux[0].strip())
        if aux[1]=="median":
            ty.append("median")
        else:
            ty.append("iqr")
    else:
        pop.append(np.nan)
        mark.append(np.nan)
        ty.append(np.nan)
del aux
del i

#create rows labes
pacient = []
for i in range(len(banco)):
    if "ICOV" in banco.Sample[i]:
        pacient.append("i")
    elif "Kidney" in banco.Sample[i]:
        pacient.append("k")
    elif "Lung" in banco.Sample[i]:
        pacient.append("l")
    else:
        pacient.append(np.nan)

banco["pa"] = pacient
u_mark = pd.unique(mark)
u_mark = list(u_mark[pd.isnull(u_mark)==False])
u_pop = pd.unique(pop)
u_pop = list(u_pop[pd.isnull(u_pop)==False])
total_med = {}
for key in u_mark:
    aux_val = []
    data = pd.DataFrame({"idd":banco.Sample,"type":banco.pa, "batch":banco.batch })
    for po in u_pop:
        # percorrer banco col
        for col in range(len(pop)):
            if (mark[col]==key) & (pop[col]==po) & (ty[col]=="median"):
                data[po] = list(banco[c[col]])
    total_med[key] = data

## POP
banco = pd.read_csv("M:/EXIMIOUS/normalizarion_flow/autogatingdata/analisis/data/batch1_population_statistics_final.csv")
# banco_u = pd.read_csv("M:/EXIMIOUS/normalizarion_flow/autogatingdata/analisis/data/batch1_population_statistics_updated.csv")
# for c in banco.columns:
#     if c not in banco_u.columns:
#         banco_u[c] = banco[c]

# for c in banco.columns:
#     if c not in banco_u.columns:
#         print(c)
# banco = banco_u

batch2 = pd.read_csv("M:/EXIMIOUS/normalizarion_flow/autogatingdata/analisis/data/batch2_population_statistics.csv")
banco["batch"] = 1
batch2["batch"] = 2
banco = pd.concat([banco,batch2], ignore_index=True, sort=False)
del batch2
  
m_pop = {}
for p in u_pop:
    aux = banco[["batch","Number of "+ p]]
    a = aux[aux.batch==1].iloc[:,1]
    b = aux[aux.batch==2].iloc[:,1]
    a = [i for i in a if not np.isnan(i)]
    b = [i for i in b if not np.isnan(i)]
    if len(a)==0:
        a = (np.nan,0)
    else:
        a = (statistics.median(a),len(a))
    if len(b)==0:
        b = (np.nan,0)
    else:
        b = (statistics.median(b),len(b))
    r = {"median":(a[0],b[0]),"tam":(a[1],b[1])}
    m_pop[p] = r


saida =open("M:/EXIMIOUS/normalizarion_flow/autogatingdata/analisis/res/analise.txt","w")
for m in total_med.keys():
    colname = []
    valt = []
    pvalor = []
    mean1 = []
    mean2 = []
    sd1 = []
    sd2 = []
    tot = total_med[m]
    for col in range(3,len(tot.columns)):
        colname.append(tot.columns[col])
        a = tot.iloc[:,col][tot.iloc[:,2]==1]
        a =a[a.isnull()==False]
        b = tot.iloc[:,col][tot.iloc[:,2]==2]
        b =b[b.isnull()==False]
        v =stats.ttest_ind(a, b)
        valt.append(v[0])
        pvalor.append(v[1])
        if len(a)>=1:
            mean1.append(statistics.mean(a))
            sd1.append(statistics.stdev(a))
        else:
            mean1.append(np.nan)
            sd1.append(np.nan)
        if len(b)>=1:
            mean2.append(statistics.mean(b))
            sd2.append(statistics.stdev(b))
        else:
            mean2.append(np.nan)
            sd2.append(np.nan)
    sd1 = pd.Series(sd1)
    sd2 = pd.Series(sd2)
    sdm1=sd1/(mean1+sd1)
    sdm2=sd2/(mean2+sd2)
    sdm2.sort_values(inplace=True)
    idx = sdm2.index
    colname = pd.Series(colname)
    mean1 = pd.Series(mean1)
    mean2 = pd.Series(mean2)
    valt = np.round(valt, 4)
    pvalor = np.round(pvalor, 4)
    valt = pd.Series(valt)
    pvalor = pd.Series(pvalor)
    df1 = pd.DataFrame({"population":colname[idx],"mean":mean1[idx],"sd":sd1[idx],"sd%":sdm1[idx],"ttest":valt[idx],"pvalor":pvalor[idx]})
    df2 = pd.DataFrame({"population":colname[idx],"mean":mean2[idx],"sd":sd2[idx],"sd%":sdm2[idx],"ttest":valt[idx],"pvalor":pvalor[idx]})
    df1 = df1[df1['pvalor'].notna()]
    df2 = df2[df2['pvalor'].notna()]
    aux = list(df1.iloc[:,0])
    med1 = []
    med2 = []
    tam1 = []
    tam2 = []
    for pi in aux:
        med1.append(m_pop[pi]["median"][0])
        med2.append(m_pop[pi]["median"][1])
        tam1.append(m_pop[pi]["tam"][0])
        tam2.append(m_pop[pi]["tam"][1])
    df1["numb_cells"] = med1
    df2["numb_cells"] = med2
    df1["tam_num_cells"] = tam1
    df2["tam_num_cells"] = tam2
    aux1 = df1.copy()
    aux2 = df2.copy()
    aux = (aux1.numb_cells>=30) & (aux2.numb_cells>=30) & (aux1.tam_num_cells>=30) & (aux2.tam_num_cells>=30)
    aux1 = aux1[aux]
    aux2 = aux2[aux]
    pscale.append(m)
    if len(aux1)>0:
        
        scale.append(aux2.iloc[0,:]["mean"]/aux1.iloc[0,:]["mean"])
    else:
        scale.append(np.nan)
    saida.write(str(m)+"\nbatch 1\n")
    saida.write(df1.to_string(index=False)+"\nbatch 2\n")
    saida.write(df2.to_string(index=False)+"\n")
    
 
saida.close()
df = pd.DataFrame({"population":pscale,"factor":scale})
df.to_csv("M:/EXIMIOUS/normalizarion_flow/autogatingdata/analisis/data_aux/factor_for_batch1.txt",index=False)



