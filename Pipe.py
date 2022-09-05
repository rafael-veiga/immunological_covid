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
from sklearn.impute import IterativeImputer,SimpleImputer
from sklearn.linear_model import LinearRegression,LogisticRegression
from sklearn.ensemble import RandomForestClassifier, GradientBoostingClassifier
import statsmodels.api as sm
import statsmodels.formula.api as smf
from joblib import Parallel, delayed
from sklearn.feature_selection import RFECV
from sklearn.preprocessing import StandardScaler
from boruta import BorutaPy



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
        print("Dicotomize")
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

