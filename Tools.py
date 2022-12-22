# -*- coding: utf-8 -*-
"""
Created on Wed Jul 20 14:42:29 2022

@author: valenter
"""

from sklearn.base import is_classifier
from sklearn.experimental import enable_iterative_imputer
from sklearn.base import BaseEstimator, TransformerMixin,ClassifierMixin
from sklearn.tree import DecisionTreeClassifier
import pickle as pk
import pandas as pd
import numpy as np 
from sklearn.svm import SVC
from sklearn.metrics import accuracy_score, balanced_accuracy_score,roc_auc_score
from sklearn.model_selection import StratifiedKFold
from sklearn.impute import IterativeImputer,SimpleImputer,KNNImputer
from sklearn.linear_model import LinearRegression,LogisticRegression
from sklearn.ensemble import RandomForestClassifier, GradientBoostingClassifier
import statsmodels.api as sm
import statsmodels.formula.api as smf
from joblib import Parallel, delayed
from sklearn.feature_selection import RFECV
from sklearn.preprocessing import StandardScaler
from boruta import BorutaPy
import torch

class Data:
    __data = None
    __col_map  = None
    __heritage = None
    
    def __init__(self, dataframe=None,col_map=None):
        if type(col_map)!=type(None):
            self.__col_map = col_map.copy()
        if type(dataframe)==type(None):
            return
        else:
            self.__data = dataframe.copy()
    
    def __is_empty(self):
        if type(self.__data)==type(None):
            print("Error: the dataset is empty")
            return True
        else:
            return False
    
    def remove_cols_NA(self, cols_ignored = None, percentage_of_NA = 30):
        total = round(len(self.__data)*(percentage_of_NA/100))
        col = list(self.__data.columns)
        if type(cols_ignored)==type([]):
            for c in cols_ignored:
                col.remove(c)
        
        for c in col:
            if self.__data[c].isnull().sum() >=total:
                self.remove_col(c)
        return
    
    def get_dataset(self,modo=False,key=False,cols=False):
        b = self.__data.copy()
        if modo=="type":
            if key in self.get_types():
                b = b.iloc[:,self.__col_map[modo][key]]
        if modo=="population":
            if key in self.get_cell_pop():
                b = b.iloc[:,self.__col_map[modo][key]]
        if modo=="mark":
            if key in self.get_marks():
                b = b.iloc[:,self.__col_map[modo][key]]
        if type(cols)==type([]):
            b = self.__data[cols]
        return b
    
    def save(self,file):
        file = open(file,"wb")
        pk.dump((self.__data,self.__col_map), file)
        file.close()
        return
    
    def load(self,file):
        file = open(file,"rb")
        self.__data, self.__col_map = pk.load(file)
        file.close()
        return
    
    
    def imputaton_KNN(self,cols_ignored = None,numb_neibors = 3):
        col = list(self.__data.columns)
        if type(cols_ignored)==type([]):
            for c in cols_ignored:
                col.remove(c)
        b = self.__data[col]
        scale = StandardScaler().fit(b)
        b = scale.transform(b)
        imputer = KNNImputer(n_neighbors=numb_neibors, weights="uniform",)
        b = imputer.fit_transform(b)
        b = scale.inverse_transform(b)
        for i in range(len(col)):
            self.__data[col[i]] = b[:,i] 
        return
    
    def imputaton_MICE(self,cols_ignored = None,max_iter =10,toll=22 ,seed=0):
        col = list(self.__data.columns)
        if type(cols_ignored)==type([]):
            for c in cols_ignored:
                col.remove(c)
        b = self.__data[col]
        lr = LinearRegression()
        imp = IterativeImputer(estimator=lr,missing_values=np.nan, max_iter=max_iter, verbose=2, imputation_order='ascending',tol=toll,random_state=seed)
        b=imp.fit_transform(b)
        for i in range(len(col)):
            self.__data[col[i]] = b[:,i] 
        return
    
    def set_col_map(self, col_map):
        self.__col_map = col_map.copy()
        
    def get_cell_pop(self):
        return list(self.__col_map["population"].keys())
    
    def get_marks(self):
        return list(self.__col_map["mark"].keys())
    
    def get_types(self):
        return list(self.__col_map["type"].keys())
    
    def remove_col(self,col_name):
        if col_name not in self.__data.columns:
            print("not find " + col_name)
            return
        col = self.__data.columns
        for i in range(len(col)):
            if col[i]==col_name:
                del self.__data[col[i]]
                #type
                keys = self.get_types()
                for k in keys:
                    del self.__col_map["type"][k][i]
                #population
                keys = self.get_cell_pop()
                for k in keys:
                    del self.__col_map["population"][k][i]
                #mark
                keys = self.get_marks()
                for k in keys:
                    del self.__col_map["mark"][k][i]
    def filter_sample(self,array):
        self.__data = self.__data[array].copy()
        return
    
    def get_col_name(self,modo = False,key=False):
        if (not modo) and (not key):
            return list(self.__data.columns)
        if modo=="type":
            if key in self.get_types():
               col = self.__data.columns 
               col = list(col[self.__col_map[modo][key]])
               return col
            else:
                print("ERROR!!! key not found")
                return None
        if modo=="population":
            if key in self.get_cell_pop():
               col = self.__data.columns 
               col = list(col[self.__col_map[modo][key]])
               return col
            else:
               print("ERROR!!! key not found")
               return None
        if modo=="mark":
            if key in self.get_marks():
               col = self.__data.columns 
               col = list(col[self.__col_map[modo][key]])
               return col
            else:
               print("ERROR!!! key not found")
               return None
        return None
   
    def remove_row(self,sample):
       self.__data = self.__data[self.__data.Sample!=sample]
    
    
    def remove_type(self, key):
        if key in self.__col_map["type"].keys():
            col = self.get_col_name(modo="type",key=key)
            for c in col:
                self.remove_col(c)
            del self.__col_map["type"][key]
    
    def add_var(self,var,name,tipo,mark=False,pop=False):
        for k in self.__col_map["type"].keys():
            if k==tipo:
                self.__col_map["type"][k].append(True)
            else:
                self.__col_map["type"][k].append(False)
        
        for k in self.__col_map["population"].keys():
            if k==pop:
                self.__col_map["population"][k].append(True)
            else:
                self.__col_map["population"][k].append(False)
        
        for k in self.__col_map["mark"].keys():
            if k==mark:
                self.__col_map["mark"][k].append(True)
            else:
                self.__col_map["mark"][k].append(False)
        
        if not (tipo in self.__col_map["type"].keys()):
            n = [False for c in self.__data.columns]
            n.append(True)
            self.__col_map["type"][tipo] = n
        if mark!=False:
            if not (mark in self.__col_map["mark"].keys()):
                n = [False for c in self.__data.columns]
                n.append(True)
                self.__col_map["mark"][mark] = n
        if pop!=False:
            if not (pop in self.__col_map["population"].keys()):
                n = [False for c in self.__data.columns]
                n.append(True)
                self.__col_map["population"][pop] = n
        self.__data[name] = var
        


                

            


class Remove__col_NA(BaseEstimator,TransformerMixin):
    def __init__(self, percentage_of_NA=20):
        self.percentage_of_NA = percentage_of_NA
    
    def fit(self, x,y = None):        
        return self
    
    def transform(self, x,y=None):
        x_ = x.copy()
        total = round(len(x_)*(self.percentage_of_NA/100))
        for c in x_.columns:
            if x_[c].isnull().sum() >=total:
                del x_[c]
        return x_.copy()
    
class Remove__Outliers(BaseEstimator,TransformerMixin):
    def __init__(self, std=3):
        self.std = std
    
    def fit(self, x,y = None):        
        return self
    
    def transform(self, x,y=None):
        x_ = x.copy()
        
        for c in x_.columns:
            lim_s = np.mean(x_[c]) + np.std(x_[c])*self.std
            lim_i = np.mean(x_[c]) - np.std(x_[c])*self.std
            x_.loc[x_[c]>lim_s,c] = lim_s
            x_.loc[x_[c]<lim_i,c] = lim_i
        return x_.copy()

class Imputation(BaseEstimator,TransformerMixin):
    def __init__(self,immune_col,max_iter=100,random_state=0,tol = 0.02):
        self.immune_col = immune_col
        self.max_iter =max_iter
        self.random_state = random_state
        self.tol=tol
        
    def fit(self, x,y = None):
        print("imputation")
        x_ = x.copy()
        col = list(x.columns)
        for c in col:
            if c not in self.immune_col:
                del x_[c]
        lr = LinearRegression()
        imp = IterativeImputer(estimator=lr,missing_values=np.nan, max_iter=self.max_iter, verbose=0,tol = self.tol ,imputation_order='ascending',random_state=self.random_state)
        imp.fit(x_)
        self.imp = imp        
        return self
    def transform(self, x,y=None):
        x_ = x.copy()
        x = x.copy()
        col = list(x.columns)
        for c in col:
            if c not in self.immune_col:
                del x_[c]
        aux = self.imp.transform(x_)
        col = list(x_.columns)
        for c in range(len(col)):
            x[col[c]] = aux[:,c] 
        return x
     


class Imputation_mean(BaseEstimator,TransformerMixin):
    def __init__(self,random_state=0):
        self.random_state = random_state
        
        
    def fit(self, x,y = None):
        x_ = x.copy()
        self.imp = SimpleImputer()
        self.imp.fit(x_)
        return self
    def transform(self, x,y=None):
        x_ = x.copy()
        x_2 = self.imp.transform(x_)
        for i in range(len(x_.columns)):
            x_.iloc[:,i] = x_2[:,i]
        return x_.copy()


class Standart(BaseEstimator,TransformerMixin):
    def __init__(self,random_state=0):
        self.random_state = random_state
        
        
    def fit(self, x,y = None):
        x_ = x.copy()
        self.std = StandardScaler()
        self.std.fit(x_)
        return self
    def transform(self, x,y=None):
        x_ = x.copy()
        x_2 = self.std.transform(x_)
        for i in range(len(x_.columns)):
            x_.iloc[:,i] = x_2[:,i]
        return x_.copy()

class Dicretization(BaseEstimator,TransformerMixin):
    def __init__(self,immune_col):
        self.immune_col = immune_col
    
    def fit(self, x,y):
        print("Dicretization")
        mape = {}
        for v in self.immune_col:
            x_ = x.copy()
            y_ = y.copy()
            y_ = y_[~x_[v].isna()]
            x_ = x_[~x_[v].isna()]
            x_ = x_[v].to_frame()  
            tree_model = DecisionTreeClassifier(max_leaf_nodes=3,criterion="entropy")
            tree_model.fit(x_, y_)
            a=tree_model.predict_proba(x_)[:,1]
            x_["bins"] = a
            x_ = x_.sort_values(by=v)
            prob = x_["bins"].iloc[0]
            cut = []
            x_["bin2"] = 2
            for i in range(1,len(x_.index)):
                if x_["bins"].iloc[i] != prob:
                    prob = x_["bins"].iloc[i]
                    cut.append(x_[v].iloc[i-1])
            mape[v] = cut
        self.mape = mape
        return self
    
    def transform(self, x,y=None):
        x_ = x.copy()
        for v in self.immune_col:
            cut = self.mape[v]
            if len(cut)==2:
                x_["bin2"] = 2
                x_.loc[x_[v].isna(),"bin2"] = np.nan
                x_.loc[x_[v]<=cut[1],"bin2"] = 1
                x_.loc[x_[v]<=cut[0],"bin2"] = 0
            if len(cut)==1:
                x_["bin2"] = 1
                x_.loc[x_[v].isna(),"bin2"] = np.na
                x_.loc[x_[v]<=cut[0],"bin2"] = 0
            x_[v] = x_["bin2"]
            del x_["bin2"]
            x_ = x_.copy()
            x_[v] = x_[v].astype('category')
        return x_
    def get_mapa(self):
        cut = []
        for v in self.immune_col:
            cut.append(self.mape[v])
        return pd.DataFrame({"var":self.immune_col,"cutof":cut})
    

class Feature_Boruta(BaseEstimator,TransformerMixin):
    def __init__(self,immune_col,tol=100,n_jobs = 15,cv=3,seed = 0):
        self.immune_col = immune_col
        self.n_jobs = n_jobs
        self.tol = tol
        self.cv = cv
        self.seed = seed
        
    def fit(self, x,y):
        print("Feature selection")
        x_ = x.copy()
        x = x.copy()
        col = list(x_.columns)
        if not self.immune_col==None:
            for c in col:
                if c not in self.immune_col:
                    del x_[c]
        comp = get_hyper_RF(x_, y,seed=self.seed,cv=self.cv)
        rf = RandomForestClassifier(n_estimators=4001,max_features=comp["max_features"],max_depth=comp["max_depth"],random_state=self.seed,class_weight="balanced_subsample",n_jobs=self.n_jobs)
        bo = BorutaPy(rf, n_estimators=4001, verbose=0, random_state=self.seed,perc = self.tol)
        print("Boruta")
        bo.fit(np.array(x_), np.array(y))
        col =list(x.columns)
        for i in range(len(x_.columns)):
            if bo.support_[i]==False:
                col.remove(list(x_.columns)[i])
        self.col = col
        return self
    
    def transform(self, x,y=None):
        return x[self.col].copy()
    
class Feature_SVM_RFE(BaseEstimator,TransformerMixin):
    def __init__(self,immune_col,n_jobs = 15,cv=3,seed = 0):
        self.immune_col = immune_col
        self.n_jobs = n_jobs
        self.cv = cv
        self.seed = seed
        
    def fit(self, x,y):
        print("Feature selection")
        x_ = x.copy()
        x = x.copy()
        col = list(x_.columns)
        if not self.immune_col==None:
            for c in col:
                if c not in self.immune_col:
                    del x_[c]
        comp = get_hyper_SVM(x_, y,seed=self.seed,cv=self.cv,n_jobs=self.n_jobs,line=True)
        model = SVC(C=comp["c"],gamma=comp["gamma"],kernel=comp["kernel"],class_weight="balanced",probability=True)
        bo = RFECV(model, step=2, cv=StratifiedKFold(10),scoring='roc_auc',verbose=3,n_jobs=self.n_jobs)
        print("RF-RFE")
        bo.fit(np.array(x_), np.array(y))
        col =list(x.columns)
        for i in range(len(x_.columns)):
            if bo.support_[i]==False:
                col.remove(list(x_.columns)[i])
        self.col = col
        self.bo=bo
        return self
    
    def transform(self, x,y=None):
        return x[self.col].copy()
    

class Feature_RF_RFE(BaseEstimator,TransformerMixin):
    def __init__(self,immune_col,n_jobs = 15,cv=3,seed = 0):
        self.immune_col = immune_col
        self.n_jobs = n_jobs
        self.cv = cv
        self.seed = seed
        
    def fit(self, x,y):
        print("Feature selection")
        x_ = x.copy()
        x = x.copy()
        col = list(x_.columns)
        if not self.immune_col==None:
            for c in col:
                if c not in self.immune_col:
                    del x_[c]
        comp = get_hyper_RF(x_, y,seed=self.seed,cv=self.cv,n_jobs=self.n_jobs)
        model = RandomForestClassifier(n_estimators=4001,max_features=comp["max_features"],max_depth=comp["max_depth"],random_state=self.seed,class_weight="balanced_subsample",n_jobs=self.n_jobs)
        bo = RFECV(model, step=2, cv=StratifiedKFold(10),scoring='roc_auc',verbose=3,n_jobs=self.n_jobs)
        print("RF-RFE")
        bo.fit(np.array(x_), np.array(y))
        col =list(x.columns)
        for i in range(len(x_.columns)):
            if bo.support_[i]==False:
                col.remove(list(x_.columns)[i])
        self.col = col
        self.bo = bo
        return self
    
    def transform(self, x,y=None):
        return x[self.col].copy()

class Feature_GB_RFE(BaseEstimator,TransformerMixin):
    def __init__(self,immune_col,n_jobs = 15,cv=3,seed = 0):
        self.immune_col = immune_col
        self.n_jobs = n_jobs
        self.cv = cv
        self.seed = seed
        
    def fit(self, x,y):
        print("Feature selection")
        x_ = x.copy()
        x = x.copy()
        col = list(x_.columns)
        if not self.immune_col==None:
            for c in col:
                if c not in self.immune_col:
                    del x_[c]
        comp = get_hyper_GB(x_, y,seed=self.seed,cv=self.cv,n_jobs=self.n_jobs)
        model = GradientBoostingClassifier(n_estimators=4001,subsample=comp["subsample"],max_features=comp["max_features"],max_depth=comp["max_depth"],random_state=self.seed)
        bo = RFECV(model, step=2, cv=StratifiedKFold(10),scoring='roc_auc',verbose=3,n_jobs=self.n_jobs)
        print("GB-RFE")
        bo.fit(np.array(x_), np.array(y))
        col =list(x.columns)
        for i in range(len(x_.columns)):
            if bo.support_[i]==False:
                col.remove(list(x_.columns)[i])
        self.col = col
        self.bo = bo
        return self
    
    def transform(self, x,y=None):
        return x[self.col].copy()


class RF(ClassifierMixin,BaseEstimator):
    def __init__(self,random_state=0,intercv = 3,n_jobs = 15):
        self.random_state = random_state
        self.intercv = intercv
        self.n_jobs = n_jobs

    def fit(self, x, y):
        comp = get_hyper_RF(x,y,seed=self.random_state,n_jobs = self.n_jobs, cv=self.intercv)
        rf = RandomForestClassifier(n_estimators=4001,max_features=comp["max_features"],max_depth=comp["max_depth"],random_state=self.random_state,class_weight="balanced_subsample",n_jobs=self.n_jobs)
        rf.fit(x, y)
        self.rf = rf
        return self

    def predict(self, x):
        return self.rf.predict(x)
    
    def predict_proba(self,x):
        return self.rf.predict_proba(x)
    
    def score(self,X, y, sample_weight=None):
        return self.rf.score(X,y)    
    
class LR(ClassifierMixin, BaseEstimator):
    def __init__(self,random_state=0,intercv = 3,n_jobs = 15):
        self.random_state = random_state
        self.intercv = intercv
        self.n_jobs = n_jobs

    def fit(self, x, y):
        comp = get_hyper_LR(x,y,seed=self.random_state,n_jobs = self.n_jobs, cv=self.intercv)
        model = LogisticRegression(random_state=self.random_state,C=comp["c"],penalty="l1",solver="liblinear",class_weight="balanced")
        model.fit(x, y)
        self.model = model
        return self

    def predict(self, x):
        self.x_test = x
        return self.model.predict(x)
    
    def predict_proba(self,x):
        return self.model.predict_proba(x)
    
    def decision_function(self, x):
        return self.model.decision_function(x)[:,1]    
    
class GB(ClassifierMixin,BaseEstimator):
    def __init__(self,random_state=0,intercv = 3,n_jobs = 15):
        self.random_state = random_state
        self.intercv = intercv
        self.n_jobs = n_jobs

    def fit(self, x, y):
        comp = get_hyper_GB(x,y,seed=self.random_state,n_jobs = self.n_jobs, cv=self.intercv)
        model = GradientBoostingClassifier(n_estimators=4001,subsample=comp["subsample"],max_features=comp["max_features"],max_depth=comp["max_depth"],random_state=self.random_state)
        model.fit(x, y)
        self.model = model
        return self

    def predict(self, x):
        return self.model.predict(x)
    
    def predict_proba(self,x):
        return self.model.predict_proba(x)
    
    def score(self,X, y, sample_weight=None):
        return self.model.score(X,y)    
    
    
class SVM(ClassifierMixin, BaseEstimator):
    def __init__(self,random_state=0,intercv = 3,n_jobs = 15,line=False):
        self.random_state = random_state
        self.intercv = intercv
        self.n_jobs = n_jobs
        self.line=line

    def fit(self, x, y):
        comp = get_hyper_SVM(x,y,seed=self.random_state,n_jobs = self.n_jobs, cv=self.intercv,line=self.line)
        model = SVC(C=comp["c"],gamma=comp["gamma"],kernel=comp["kernel"],class_weight="balanced",probability=True)
        model.fit(x, y)
        self.model = model
        return self

    def predict(self, x):
        self.x_test = x
        return self.model.predict(x)
    
    def predict_proba(self,x):
        return self.model.predict_proba(x)
    
    def decision_function(self, x):
        return self.model.decision_function(x)[:,1]
    
class ANN(ClassifierMixin, BaseEstimator):
    def __init__(self,random_state=0,intercv = 3,n_jobs = 15):
        self.random_state = random_state
        self.intercv = intercv
        self.n_jobs = n_jobs

        

    def fit(self, x, y):
        comp = get_hyper_SVM(x,y,seed=self.random_state,n_jobs = self.n_jobs, cv=self.intercv,line=self.line)
        model = SVC(C=comp["c"],gamma=comp["gamma"],kernel=comp["kernel"],class_weight="balanced",probability=True)
        model.fit(x, y)
        self.model = model
        return self

    def predict(self, x):
        self.x_test = x
        return self.model.predict(x)
    
    def predict_proba(self,x):
        return self.model.predict_proba(x)
    
    def decision_function(self, x):
        return self.model.decision_function(x)[:,1]

def feature_selector_boruta(immune_col,d2_d,d3_d,d3_p,seed=0,n_jobs=15,per = 100):
    x_d2_d = d2_d.copy()
    y_d2_d = d2_d.d2_d.copy()
    cov = ["type","sex","age","tx_time"]
    for c in x_d2_d.columns:
        if not c in immune_col:
           del x_d2_d[c]
           
    x_d3_d = d3_d.copy()
    y_d3_d = d3_d.d3_d.copy()
    for c in x_d3_d.columns:
        if not c in immune_col:
           del x_d3_d[c]
           
    x_d3_p = d3_p.copy()
    y_d3_p = d3_p.d3_p.copy()
    for c in x_d3_p.columns:
        if not c in immune_col:
           del x_d3_p[c]
    
    res = get_hyper_RF(x_d2_d,x_d3_d,x_d3_p,y_d2_d,y_d3_d,y_d3_p,seed=seed,n_jobs = n_jobs)
    # d2_d
    rf = RandomForestClassifier(n_estimators=4001,max_features=res["d2_d"]["mf"],min_samples_split=res["d2_d"]["mi"],random_state=seed,class_weight="balanced_subsample")
    bo = BorutaPy(rf, n_estimators=4001, verbose=2, random_state=seed,perc = per )
    bo.fit(np.array(x_d2_d), np.array(y_d2_d))
    df_d2_d = pd.DataFrame({"var":x_d2_d.columns,"ranki":list(bo.ranking_)})
    li = list(df_d2_d.loc[df_d2_d.ranki<=1,"var"])+cov
    x_d2_d = d2_d.loc[:,li]

    #d3_d
    rf = RandomForestClassifier(n_estimators=4001,max_features=res["d3_d"]["mf"],min_samples_split=res["d3_d"]["mi"],random_state=seed,class_weight="balanced_subsample")
    bo = BorutaPy(rf, n_estimators=4001, verbose=2, random_state=seed,perc = per)
    bo.fit(np.array(x_d3_d), np.array(y_d3_d))
    df_d3_d = pd.DataFrame({"var":x_d3_d.columns,"ranki":list(bo.ranking_)})
    li = list(df_d3_d.loc[df_d3_d.ranki<=1,"var"])+cov
    x_d3_d = d3_d.loc[:,li]
    
    #d3_p
    rf = RandomForestClassifier(n_estimators=4001,max_features=res["d3_p"]["mf"],min_samples_split=res["d3_p"]["mi"],random_state=seed,class_weight="balanced_subsample")
    bo = BorutaPy(rf, n_estimators=4001, verbose=2, random_state=seed,perc = per)
    bo.fit(np.array(x_d3_p), np.array(y_d3_p))
    df_d3_p = pd.DataFrame({"var":x_d3_p.columns,"ranki":list(bo.ranking_)})
    li = list(df_d3_p.loc[df_d3_p.ranki<=1,"var"])+cov
    x_d3_p = d3_p.loc[:,li]
    
    comp = {"df_d2_d":df_d2_d,"df_d3_d":df_d3_d,"df_d3_p":df_d3_p,
            "x_d2_d":x_d2_d,"x_d3_d":x_d3_d,"x_d3_p":x_d3_p,
            "y_d2_d":y_d2_d,"y_d3_d":y_d3_d,"y_d3_p":y_d3_p}
    return comp

def feature_selector_SVM_RFE(immune_col,d2_d,d3_d,d3_p,seed=0,n_jobs=15):
    x_d2_d = d2_d.copy()
    y_d2_d = d2_d.d2_d.copy()
    cov = ["type","sex","age","tx_time"]
    for c in x_d2_d.columns:
        if not c in immune_col:
           del x_d2_d[c]
           
    x_d3_d = d3_d.copy()
    y_d3_d = d3_d.d3_d.copy()
    for c in x_d3_d.columns:
        if not c in immune_col:
           del x_d3_d[c]
           
    x_d3_p = d3_p.copy()
    y_d3_p = d3_p.d3_p.copy()
    for c in x_d3_p.columns:
        if not c in immune_col:
           del x_d3_p[c]
    
    res = get_hyper_SVM(x_d2_d,x_d3_d,x_d3_p,y_d2_d,y_d3_d,y_d3_p,seed=seed,n_jobs = n_jobs)
    # d2_d
    svm = SVC(C=res["d2_d"]["c"],gamma=res["d2_d"]["gamma"],kernel=res["d2_d"]["kernel"],class_weight="balanced",probability=True)
    rfecv = RFECV(estimator=svm, step=2, cv=StratifiedKFold(10),scoring='balanced_accuracy',n_jobs=n_jobs,verbose=2)
    rfecv.fit(x_d2_d,y_d2_d)
    li = list(x_d2_d.columns[rfecv.support_])+cov
    x_d2_d = d2_d.loc[:,li]
    
    # d3_d
    svm = SVC(C=res["d3_d"]["c"],gamma=res["d3_d"]["gamma"],kernel=res["d3_d"]["kernel"],class_weight="balanced",probability=True)
    rfecv = RFECV(estimator=svm, step=2, cv=StratifiedKFold(10),scoring='balanced_accuracy',n_jobs=n_jobs)
    rfecv.fit(x_d3_d,y_d3_d)
    li = list(x_d3_d.columns[rfecv.support_])+cov
    x_d3_d = d3_d.loc[:,li]
    
    # d3_p
    svm = SVC(C=res["d3_p"]["c"],gamma=res["d3_p"]["gamma"],kernel=res["d3_p"]["kernel"],class_weight="balanced",probability=True)
    rfecv = RFECV(estimator=svm, step=2, cv=StratifiedKFold(10),scoring='balanced_accuracy',n_jobs=n_jobs)
    rfecv.fit(x_d3_p,y_d3_p)
    li = list(x_d3_p.columns[rfecv.support_])+cov
    x_d3_p = d3_p.loc[:,li]
    
    comp = {"x_d2_d":x_d2_d,"x_d3_d":x_d3_d,"x_d3_p":x_d3_p,
            "y_d2_d":y_d2_d,"y_d3_d":y_d3_d,"y_d3_p":y_d3_p}
    return comp        
        
def feature_process(immune_col,d2_d,d3_d,d3_p):
     v = []
     ve = []
     count = 0
     for name in immune_col:
         count+=1
         
         for out,df in (("d2_d",d2_d),("d3_d",d3_d),("d3_p",d3_p)):
         
             var = df[name]
             for cut in (20,30,40,50,60,70,80):
                 try:
                     per = np.percentile(list(var[~var.isna()]),cut)
                     var1 = var.copy()
                     var1[var1<=per] = 0
                     var1[var1>per] = 1
                     df1 = df.copy()
                     df1["var"] = var1
                     v.append((out,per,df1,name))
                 except:
                     ve.append(var)
     def calcu(v):
         out,cut,df,name = v
         try:
             res = smf.glm(formula=out+"~var+type+sex+age+tx_time", data=df, family=sm.families.Binomial()).fit()
             if res.converged:
                 p = res.pvalues[1]
                 ort=np.exp(res.params[1])
                 ci = res.conf_int()
                 ci1 = np.exp(ci.iloc[1,0])
                 ci2 = np.exp(ci.iloc[1,1])
             else:
                p=np.nan
                ort = np.nan
                ci1 = np.nan
                ci2 = np.nan 
         except:
             p=np.nan
             ort = np.nan
             ci1 = np.nan
             ci2 = np.nan
         
         return (p,ort,ci1,ci2)
     
         
     print("start")
     res = Parallel(n_jobs=15,verbose=7)(delayed(calcu)(p) for p in v)
     print("stop")                    
     fout = []
     fcut = []
     fname = []
     fp = []
     fort = []
     fci1 = []
     fci2 = []
     for i in range(len(v)):
         out,cut,df,name = v[i]
         p,ort,ci1,ci2 = res[i]
         fout.append(out)
         fcut.append(cut)
         fname.append(name)
         fp.append(p)
         fort.append(ort)
         fci1.append(ci1)
         fci2.append(ci2)
     df = pd.DataFrame({"outcume":fout,"var":fname,"cut":fcut,"pvalue":fp,"odds":fort,"ci1":fci1,"ci2":fci2})
     df_d2_d = df[df.outcume=="d2_d"].copy()
     df_d3_d = df[df.outcume=="d3_d"].copy()
     df_d3_p = df[df.outcume=="d3_p"].copy()
     # d2_d
     l = df_d2_d["var"].unique()
     idx = []
     for n in l:
         idx.append(df_d2_d[df_d2_d["var"]==n]["pvalue"].idxmin())
     idx = [x for x in idx if str(x) != 'nan']
     df_d2_d = df_d2_d.loc[idx]
     # d3_d
     l = df_d3_d["var"].unique()
     idx = []
     for n in l:
         idx.append(df_d3_d[df_d3_d["var"]==n]["pvalue"].idxmin())
     idx = [x for x in idx if str(x) != 'nan']
     df_d3_d = df_d3_d.loc[idx]
     # d3_p
     l = df_d3_p["var"].unique()
     idx = []
     for n in l:
         idx.append(df_d3_p[df_d3_p["var"]==n]["pvalue"].idxmin())
     idx = [x for x in idx if str(x) != 'nan']
     df_d3_p = df_d3_p.loc[idx]
     
     ### apply filter
     for n in immune_col:
         vari = list(df_d2_d["var"])
         if n in vari:
             cut = df_d2_d.loc[df_d2_d["var"]==n,"cut"].iloc[0]
             d2_d.loc[d2_d[n]<=cut,n] = 0
             d2_d.loc[d2_d[n]>cut,n] = 1
         else:
             del d2_d[n]
         
         vari = list(df_d3_d["var"])
         if n in vari:
             cut = df_d3_d.loc[df_d3_d["var"]==n,"cut"].iloc[0]
             d3_d.loc[d3_d[n]<=cut,n] = 0
             d3_d.loc[d3_d[n]>cut,n] = 1
         else:
             del d3_d[n]
             
         vari = list(df_d3_p["var"])
         if n in vari:
             cut = df_d3_p.loc[df_d3_p["var"]==n,"cut"].iloc[0]
             d3_p.loc[d3_p[n]<=cut,n] = 0
             d3_p.loc[d3_p[n]>cut,n] = 1
         else:
             del d3_p[n]
     
     return {"d2_d":d2_d,"d3_d":d3_d,"d3_p":d3_p,
             "df_d2_d":df_d2_d,"df_d3_d":df_d3_d,"df_d3_p":df_d3_p}               
 
def model_avaliation(dataset,model,dataframe_out,model_name):
    x_d2_d_train = dataset["x_d2_d_train"]
    x_d3_d_train = dataset["x_d3_d_train"]
    x_d3_p_train = dataset["x_d3_p_train"]
    x_d2_d_test = dataset["x_d2_d_test"]
    x_d3_d_test = dataset["x_d3_d_test"]
    x_d3_p_test = dataset["x_d3_p_test"]
    y_d2_d_train = dataset["y_d2_d_train"]
    y_d3_d_train = dataset["y_d3_d_train"]
    y_d3_p_train = dataset["y_d3_p_train"]
    y_d2_d_test = dataset["y_d2_d_test"]
    y_d3_d_test = dataset["y_d3_d_test"]
    y_d3_p_test = dataset["y_d3_p_test"]
    
    #d2_d
    accuracy = []
    bal_accuracy = []
    AUC = []
    for i in range(10): 
        model.fit(x_d2_d_train[i],y_d2_d_train[i])
        p = model.predict(x_d2_d_test[i])
        z = model.predict_proba(x_d2_d_test[i])
        y_true = y_d2_d_test[i]
        accuracy.append(accuracy_score(y_true=y_true,y_pred=p))
        bal_accuracy.append(balanced_accuracy_score(y_true=y_true,y_pred=p))
        AUC.append(roc_auc_score(y_true=y_true,y_score=z[:,1]))
    dataframe_out = dataframe_out.append({"model":model_name + "-d2_d",
                  "accuracy":np.mean(accuracy),
                  "accuracy_sd":np.std(accuracy),
                  "bal_accuracy":np.mean(bal_accuracy),
                  "bal_accuracy_sd":np.std(bal_accuracy),
                  "AUC":np.mean(AUC),
                  "AUC_sd":np.std(AUC),
                },ignore_index=True)
    #d3_d
    accuracy = []
    bal_accuracy = []
    AUC = []
    for i in range(10): 
        model.fit(x_d3_d_train[i],y_d3_d_train[i])
        p = model.predict(x_d3_d_test[i])
        z = model.predict_proba(x_d3_d_test[i])
        y_true = y_d3_d_test[i]
        accuracy.append(accuracy_score(y_true=y_true,y_pred=p))
        bal_accuracy.append(balanced_accuracy_score(y_true=y_true,y_pred=p))
        AUC.append(roc_auc_score(y_true=y_true,y_score=z[:,1]))
    dataframe_out = dataframe_out.append({"model":model_name + "-d3_d",
                  "accuracy":np.mean(accuracy),
                  "accuracy_sd":np.std(accuracy),
                  "bal_accuracy":np.mean(bal_accuracy),
                  "bal_accuracy_sd":np.std(bal_accuracy),
                  "AUC":np.mean(AUC),
                  "AUC_sd":np.std(AUC),
                },ignore_index=True)
    #d3_p
    accuracy = []
    bal_accuracy = []
    AUC = []
    for i in range(10): 
        model.fit(x_d3_p_train[i],y_d3_p_train[i])
        p = model.predict(x_d3_p_test[i])
        z = model.predict_proba(x_d3_p_test[i])
        y_true = y_d3_p_test[i]
        accuracy.append(accuracy_score(y_true=y_true,y_pred=p))
        bal_accuracy.append(balanced_accuracy_score(y_true=y_true,y_pred=p))
        AUC.append(roc_auc_score(y_true=y_true,y_score=z[:,1]))
    dataframe_out = dataframe_out.append({"model":model_name + "-d3_p",
                  "accuracy":np.mean(accuracy),
                  "accuracy_sd":np.std(accuracy),
                  "bal_accuracy":np.mean(bal_accuracy),
                  "bal_accuracy_sd":np.std(bal_accuracy),
                  "AUC":np.mean(AUC),
                  "AUC_sd":np.std(AUC),
                },ignore_index=True)
    return dataframe_out

def get_hyper_LR(x,y,seed=0,n_jobs = 15, cv=3):
        print("hyper LR")            
        v = 1
        reg = []
        for a in range(100):
            reg.append(v)
            v = v-v*1/10
        
        v = []
        skf = StratifiedKFold(n_splits=cv,shuffle=True,random_state=seed)
        i = 0
        for train_index, test_index in skf.split(x, y):
            x_train = x.iloc[train_index,:].copy()
            y_train = y.iloc[train_index].copy()
            x_test = x.iloc[test_index,:].copy()
            y_test = y.iloc[test_index].copy()
            for r in reg:
                v.append((i,x_train,y_train,x_test,y_test,r))
            i+=1
                
        def calcu(v):
            i,x_train,y_train,x_test,y_test,c = v
            rf = LogisticRegression(random_state=seed,C=c,penalty="l1",solver="liblinear",class_weight="balanced")
            rf.fit(x_train, y_train)
            y_pred = rf.predict(x_test)
            return balanced_accuracy_score(y_true=y_test,y_pred=y_pred)
        print("start")
        res = Parallel(n_jobs=n_jobs,verbose=0)(delayed(calcu)(p) for p in v)
        print("stop")
        fold = []
        c = []
        for i in range(len(v)):
            fold.append(v[i][0])
            c.append(v[i][5])     
        df = pd.DataFrame({"fold":fold,"c":c,"res":res})
        c = []
        res = []
        comp = {}
        for a in reg:
            aux = df[df.c == a]
            c.append(a)
            res.append(np.average(aux.res))
        df = pd.DataFrame({"c":c,"res":res})
        df.sort_values(by="res",ascending=False,inplace=True)
        comp["c"] = df.iloc[0].c
        return comp
    
def get_hyper_RF(x,y,seed=0,n_jobs = 15, cv=3): 
        print("hyper RF")            
        res_max_depth = [2,3,4,5,6,7]
        res_max_features = ["sqrt","log2"]
        
        v = []
        skf = StratifiedKFold(n_splits=cv,shuffle=True,random_state=seed)
        i = 0
        for train_index, test_index in skf.split(x, y):
            x_train = x.iloc[train_index,:].copy()
            y_train = y.iloc[train_index].copy()
            x_test = x.iloc[test_index,:].copy()
            y_test = y.iloc[test_index].copy()
            for max_depth in res_max_depth:
                for max_features in res_max_features:
                    v.append((i,x_train,y_train,x_test,y_test,max_depth,max_features))
            i+=1
                
        def calcu(v):
            i,x_train,y_train,x_test,y_test,max_depth,max_features = v
            rf = RandomForestClassifier(n_estimators=4001,max_features=max_features,max_depth=max_depth,random_state=seed,class_weight="balanced_subsample")
            rf.fit(x_train, y_train)
            y_pred = rf.predict(x_test)
            return balanced_accuracy_score(y_true=y_test,y_pred=y_pred)
        print("start")
        res = Parallel(n_jobs=n_jobs,verbose=0)(delayed(calcu)(p) for p in v)
        print("stop")
        fold = []
        md = []
        mf = []
        comp = {}
        for i in range(len(v)):
            fold.append(v[i][0])
            md.append(v[i][5])
            mf.append(v[i][6])

        df = pd.DataFrame({"fold":fold,"max_depth":md,"max_features":mf,"res":res})
        md = []
        mf = []
        res = []
        comp = {}
        for max_depth in res_max_depth:
            for max_features in res_max_features:
                aux = df.loc[df.max_depth == max_depth,:].loc[df.max_features==max_features,:]
                md.append(max_depth)
                mf.append(max_features)
                res.append(np.average(aux.res))
        df = pd.DataFrame({"max_depth":md,"max_features":mf,"res":res})
        df.sort_values(by="res",ascending=False,inplace=True)
        comp["max_depth"] = df.iloc[0].max_depth
        comp["max_features"] = df.iloc[0].max_features        
        return comp

def get_hyper_GB(x,y,seed=0,n_jobs = 15, cv=3): 
        print("hyper GB")            
        res_max_depth = [2,3,4,5,6,7]
        res_max_features = ["sqrt","log2"]
        res_subsample = [1,0.8,0.6]
        
        v = []
        skf = StratifiedKFold(n_splits=cv,shuffle=True,random_state=seed)
        i = 0
        for train_index, test_index in skf.split(x, y):
            x_train = x.iloc[train_index,:].copy()
            y_train = y.iloc[train_index].copy()
            x_test = x.iloc[test_index,:].copy()
            y_test = y.iloc[test_index].copy()
            for max_depth in res_max_depth:
                for max_features in res_max_features:
                    for subsample in res_subsample:
                        v.append((i,x_train,y_train,x_test,y_test,max_depth,max_features,subsample))
            i+=1
                
        def calcu(v):
            i,x_train,y_train,x_test,y_test,max_depth,max_features,subsample = v
            model = GradientBoostingClassifier(n_estimators=4001,subsample=subsample,max_features=max_features,max_depth=max_depth,random_state=seed)
            model.fit(x_train, y_train)
            y_pred = model.predict(x_test)
            return balanced_accuracy_score(y_true=y_test,y_pred=y_pred)
        print("start")
        res = Parallel(n_jobs=n_jobs,verbose=0)(delayed(calcu)(p) for p in v)
        print("stop")
        fold = []
        md = []
        mf = []
        su = []
        comp = {}
        for i in range(len(v)):
            fold.append(v[i][0])
            md.append(v[i][5])
            mf.append(v[i][6])
            su.append(v[i][7])
        df = pd.DataFrame({"fold":fold,"max_depth":md,"max_features":mf,"subsample":su,"res":res})
        md = []
        mf = []
        su = []
        res = []
        comp = {}
        for max_depth in res_max_depth:
            for max_features in res_max_features:
                for subsample in res_subsample:
                    aux = df.loc[df.max_depth == max_depth,:].loc[df.max_features==max_features,:].loc[df.subsample==subsample]
                    md.append(max_depth)
                    mf.append(max_features)
                    su.append(subsample)
                    res.append(np.average(aux.res))
        df = pd.DataFrame({"max_depth":md,"max_features":mf,"subsample":su,"res":res})
        df.sort_values(by="res",ascending=False,inplace=True)
        comp["max_depth"] = df.iloc[0].max_depth
        comp["max_features"] = df.iloc[0].max_features
        comp["subsample"] = df.iloc[0].subsample        
        return comp

def get_hyper_SVM(x,y,seed=0,n_jobs = 15, cv=3,line=False): 
        print("hyper SVM")            
        res_c = [0.001,0.01,0.05,0.07,0.1,0.3,0.5,0.7,1,3,5,7, 10,30,50,70, 100,200]
        res_gamma = [1,0.7,0.5,0.3,0.1,0.07,0.05,0.03,0.01,0.007,0.005,0.003,0.001,0.0001]
        res_kernel = ["linear",'rbf', 'poly', 'sigmoid']
        if line:
            res_kernel = ["linear"]
        
        v = []
        skf = StratifiedKFold(n_splits=cv,shuffle=True,random_state=seed)
        i = 0
        for train_index, test_index in skf.split(x, y):
            x_train = x.iloc[train_index,:].copy()
            y_train = y.iloc[train_index].copy()
            x_test = x.iloc[test_index,:].copy()
            y_test = y.iloc[test_index].copy()
            for c in res_c:
                for gamma in res_gamma:
                    for kernel in res_kernel:
                        v.append((i,x_train,y_train,x_test,y_test,c,gamma,kernel))
            i+=1
                
        def calcu(v):
            i,x_train,y_train,x_test,y_test,c,gamma,kernel = v
            model = SVC(C=c,gamma=gamma,kernel=kernel,class_weight="balanced",probability=True)
            model.fit(x_train, y_train)
            y_pred = model.predict(x_test)
            return balanced_accuracy_score(y_true=y_test,y_pred=y_pred)
        print("start")
        res = Parallel(n_jobs=n_jobs,verbose=0)(delayed(calcu)(p) for p in v)
        print("stop")
        fold = []
        c = []
        gamma = []
        kernel = []
        comp = {}
        for i in range(len(v)):
            fold.append(v[i][0])
            c.append(v[i][5])
            gamma.append(v[i][6])
            kernel.append(v[i][7])

        df = pd.DataFrame({"fold":fold,"c":c,"gamma":gamma,"kernel":kernel,"res":res})
        c = []
        gamma = []
        kernel = []
        res = []
        comp = {}
        for ac in res_c:
            for agamma in res_gamma:
                for akernel in res_kernel:
                    aux = df.loc[df.c == ac,:].loc[df.gamma==agamma,:].loc[df.kernel==akernel,:]
                    c.append(ac)
                    gamma.append(agamma)
                    kernel.append(akernel)
                    res.append(np.average(aux.res))
        df = pd.DataFrame({"c":c,"gamma":gamma,"kernel":kernel,"res":res})
        df.sort_values(by="res",ascending=False,inplace=True)
        comp["c"] = df.iloc[0].c
        comp["gamma"] = df.iloc[0].gamma
        comp["kernel"] = df.iloc[0].kernel         
        return comp