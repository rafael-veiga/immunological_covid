# -*- coding: utf-8 -*-
"""
Created on Tue Feb  1 13:22:01 2022

@author: valenter
"""

import pandas as pd
import numpy as np
import os
from Tools import Data

def openCleanData(file_immuno,file_icov,file_lung,file_kidney,file_batch):
      bi = pd.read_csv(file_immuno)
      bi.replace("NA",np.nan)
      batch = pd.read_csv(file_batch)
      bi = bi.merge(batch,on="Sample",how="outer")
      col = []
      for s in bi.Sample:
          if s.startswith("ICOV"):
              col.append("i")
          elif s.startswith("Kidney"):
              col.append("k")
          elif  s.startswith("Lung"):
              col.append("l")
          elif  s.startswith("internal"):
              col.append("control")
          else:
              col.append(np.nan)
      bi["type"] = col
      col = list(bi.columns)
      
      col.remove("batch")
      col.remove("type")
      col.insert(1, "batch")
      col.insert(2, "type")
      bi = bi[col]

# #  map     
      col = list(bi.columns)
      mark = [False,False,False]
      pop = [False,False,False]
      tipo = ["meta","meta","meta"]
      fal = []
#       # markers
      for i in range(3,len(col)):
          s = col[i].strip().split(" ")
          if s[1]=="median":
              mark.append(s[0].strip())
              tipo.append("median")
              s = col[i].strip().split(" :in: ")
              pop.append(s[1].strip())
          elif s[1]=="iqr":
              mark.append(s[0].strip())
              tipo.append("iqr")
              s = col[i].strip().split(" :in: ")
              pop.append(s[1].strip())
          elif col[i].startswith("Number of "):
              s = col[i].strip().split("Number of ")
              mark.append(False)
              tipo.append("number of cell")
              pop.append(s[1])
          elif " :in: " in col[i]:
              tipo.append("proportion")           
              s = col[i].strip().split(" :in: ")
              pop.append(s[0])
              mark.append(False)
                 
          else:
              fal.append(col[i])
              print(col[i])
      len(fal)
            
      df = pd.DataFrame({"v":mark,"popu":pop})
      u_mark = df.v.unique()
      u_mark = list(u_mark[1:len(u_mark)])
      u_popu = df.popu.unique()
      u_popu = list(u_popu[1:len(u_popu)]) 


# #add here
      df = pd.DataFrame({"Sample":bi["Sample"],"type":bi["type"]})
      icov = pd.read_csv(file_icov)
      icov["Sample"] = ""
      for i in icov["ID"].index:
          icov.loc[i,"Sample"] = "ICOV_"+str("{:03d}".format(icov.ID[i]))
      del icov["ID"]
      icov.columns = ["age","HTA","Cancer","sex","Sample"]
      icov.sex.replace({"Homme":"m","Femme":"f"},inplace=True)
      df = df.merge(icov,on="Sample",how="outer")
      lung = pd.read_csv(file_lung)
      lung["Sample"] = ""    
      for i in lung["patient"].index:
          lung.loc[i,"Sample"] = "Lung_"+str("{:03d}".format(lung.patient.loc[i]))
      del lung["patient"]
      
      lung.sex.replace({"h":"m"},inplace=True)
      lung.rename({"temps_greffe":"tx_time","GFR <30":"GFR_30","Time after transplant (years)":"tx_time","Transplant rank":"Transplant_rank","Induction treatment":"Induction_treatment","Number of treatments":"Number_of_treatments","covid BC":"covid_BC","Aza":"AZA"},axis=1,inplace=True)
      lung.replace({"<5":2.5,"SNA":np.nan,"Discard":np.nan},inplace=True)
      lung["D2_v"] = pd.to_numeric(lung["D2_v"])
      lung["D3_v"] = pd.to_numeric(lung["D3_v"])

      df = df.merge(lung,on="Sample",how="outer")
      
      df.loc[df.type=="l","sex_x"] = df.loc[df.type=="l","sex_y"]
      del df["sex_y"]
      df.loc[df.type=="l","age_x"] = df.loc[df.type=="l","age_y"]
      del df["age_y"]
      df.loc[df.type=="l","HTA_x"] = df.loc[df.type=="l","HTA_y"]
      del df["HTA_y"]
      df.loc[df.type=="l","Cancer_x"] = df.loc[df.type=="l","Cancer_y"]
      del df["Cancer_y"]
      
      del df["covid_BC"]
      df.rename({"sex_x":"sex","age_x":"age","HTA_x":"HTA","Cancer_x":"Cancer"},axis=1,inplace=True)
      df.loc[df.type=="i","FK"] = 0
      df.loc[df.type=="i","CTC"] = 0
      df.loc[df.type=="i","AZA"] = 0
      df.loc[df.type=="i","MMF"] = 0
      df.loc[df.type=="i","Evero"] = 0
      df.loc[df.type=="i","Orencia"] = 0
      df.loc[df.type=="i","CsA"] = 0
      df.loc[df.type=="i","D2_response"] = "Detectable"
      df.loc[df.type=="i","D3_response"] = "Positive"


      nefro = pd.read_csv(file_kidney)
      nefro["Sample"] = ""
      for i in nefro["ID patient"].index:
          nefro.loc[i,"Sample"] = "Kidney_Tx_n_"+str("{:03d}".format(nefro.loc[i,"ID patient"]))
      del nefro["ID patient"]
      nefro.sex.replace({"h":"m"},inplace=True)
      #del nefro["Tx/HD"]
#      del nefro["naïf / infecté"]
      del nefro['Statut']
      #nefro.Ethnie.replace({0:"Europe",
      #                      1: "other",
      #                      2: "other",
      #                      3: "other",
      #                      4: "other",
      #                      5: "other",
      #                      6: "other"},inplace=True)

      nefro.rename({"diabète":"Diabetes","Time after transplant (years)":"tx_time","GFR <30":"GFR_30",
                    "Transplant rank":"Transplant_rank","Induction treatment":"Induction_treatment","Number of treatments":"Number_of_treatments",
                    "covid BC":"covid_BC","Ethnie":"Ethnicity","Aza":"AZA"},axis=1,inplace=True)
      
     
      nefro["Orencia"] = pd.to_numeric(nefro["Orencia"])
      nefro.replace({"Discard":np.nan,"2618*":2618,"75049*":75049,"mAB":np.nan,"sortie":np.nan},inplace=True)
      nefro["D2_v"] = pd.to_numeric(nefro["D2_v"])
      nefro["D3_v"] = pd.to_numeric(nefro["D3_v"])      
      a=[]
      for l in nefro.Sample:
          if not l in list(df.Sample):
              a.append(l)
      b = []
      for l in df.Sample[df.type=="k"]:
          if not l in list(nefro.Sample):
              b.append(l)

                  
      df = df.merge(nefro,on="Sample",how="outer")
      for l in a:
          df.loc[l==df.Sample,"type"] = "k"

      df.loc[df.type=="k","Cancer_x"] = df.loc[df.type=="k","Cancer_y"]
      del df["Cancer_y"]
      df.loc[df.type=="k","BMI_x"] = df.loc[df.type=="k","BMI_y"]
      del df["BMI_y"]
      df.loc[df.type=="k","HTA_x"] = df.loc[df.type=="k","HTA_y"]
      del df["HTA_y"]
      df.loc[df.type=="k","CV_x"] = df.loc[df.type=="k","CV_y"]
      del df["CV_y"]
      df.loc[df.type=="k","GFR_30_x"] = df.loc[df.type=="k","GFR_30_y"]
      del df["GFR_30_y"]
      df.loc[df.type=="k","Ethnicity_x"] = df.loc[df.type=="k","Ethnicity_y"]
      del df["Ethnicity_y"]
      df.loc[df.type=="k","Transplant_rank_x"] = df.loc[df.type=="k","Transplant_rank_y"]
      del df["Transplant_rank_y"]
      df.loc[df.type=="k","Induction_treatment_x"] = df.loc[df.type=="k","Induction_treatment_y"]
      del df["Induction_treatment_y"]
      df.loc[df.type=="k","Number_of_treatments_x"] = df.loc[df.type=="k","Number_of_treatments_y"]
      del df["Number_of_treatments_y"]
      df.loc[df.type=="k","Diabetes_x"] = df.loc[df.type=="k","Diabetes_y"]
      del df["Diabetes_y"]

      df.loc[df.type=="k","sex_x"] = df.loc[df.type=="k","sex_y"]
      del df["sex_y"]
      
      
      df.loc[df.type=="k","age_x"] = df.loc[df.type=="k","age_y"]
      del df["age_y"]
      df.loc[df.type=="k","FK_x"] = df.loc[df.type=="k","FK_y"]
      del df["FK_y"]
      df.loc[df.type=="k","CTC_x"] = df.loc[df.type=="k","CTC_y"]
      del df["CTC_y"]
      df.loc[df.type=="k","MMF_x"] = df.loc[df.type=="k","MMF_y"]
      del df["MMF_y"]
      df.loc[df.type=="k","Evero_x"] = df.loc[df.type=="k","Evero_y"]
      del df["Evero_y"]
      df.loc[df.type=="k","Orencia_x"] = df.loc[df.type=="k","Orencia_y"]
      del df["Orencia_y"]
      df.loc[df.type=="k","CsA_x"] = df.loc[df.type=="k","CsA_y"]
      del df["CsA_y"]
      df.loc[df.type=="k","D2_v_x"] = df.loc[df.type=="k","D2_v_y"]
      del df["D2_v_y"]
      df.loc[df.type=="k","D3_v_x"] = df.loc[df.type=="k","D3_v_y"]
      del df["D3_v_y"]
      df.loc[df.type=="k","D2_response_x"] = df.loc[df.type=="k","D2_response_y"]
      del df["D2_response_y"]
      df.loc[df.type=="k","D3_response_x"] = df.loc[df.type=="k","D3_response_y"]
      del df["D3_response_y"]
      df.loc[df.type=="k","AZA_x"] = df.loc[df.type=="k","AZA_y"]
      del df["AZA_y"]
      df.loc[df.type=="k","tx_time_x"] = df.loc[df.type=="k","tx_time_y"]
      del df["tx_time_y"]
      df.rename({"sex_x":"sex","age_x":"age","FK_x":"FK","CTC_x":"CTC","MMF_x":"MMF",
                  "Evero_x":"Evero","Orencia_x":"Orencia","CsA_x":"CsA","D2_v_x":"D2_v",
                  "D3_v_x":"D3_v","D2_response_x":"D2_response","D3_response_x":"D3_response",
                  "AZA_x":"AZA","tx_time_x":"tx_time","Cancer_x":"Cancer","BMI_x":"BMI","HTA_x":"HTA","CV_x":"CV","GFR_30_x":"GFR_30",
                  "Ethnicity_x":"Ethnicity","Diabetes_x":"Diabetes","Transplant_rank_x":"Transplant_rank",
                  "Induction_treatment_x":"Induction_treatment","Number_of_treatments_x":"Number_of_treatments"},axis=1,inplace=True)
      #del df["Orencia"]
      #del df["cirrhose"]
      del df["autre comorbidités"]
      # positive 
      d2 = df.D2_response.copy()
      d2.replace({"Negative":0,"Positive":1,"Detectable":0},inplace=True)
      df["D2_pos"] = d2
      d3 = df.D3_response.copy()
      d3.replace({"Negative":0,"Positive":1,"Detectable":0},inplace=True)
      df["D3_pos"] = d3
      df["out_pos"] = np.nan
      df.loc[d2==1,"out_pos"] = 1
      df.loc[d3==1,"out_pos"] = 1
      df.loc[(d2==0)&(d3==0),"out_pos"] = 0
      #detection
      df["D2_detect"] = np.nan
      df.loc[df["D2_response"]=="Negative","D2_detect"]=0
      df.loc[df["D2_response"]=="Detectable","D2_detect"]=1
      df.loc[df["D2_response"]=="Positive","D2_detect"]=1
      df["D3_detect"] = np.nan
      df.loc[df["D3_response"]=="Negative","D3_detect"]=0
      df.loc[df["D3_response"]=="Detectable","D3_detect"]=1
      df.loc[df["D3_response"]=="Positive","D3_detect"]=1
      df["out_detect"] = np.nan
      df.loc[(df["D2_detect"]==1) & (df["D3_detect"]==1),"out_detect"] = 1
      df.loc[(df["D2_detect"]==0) & (df["D3_detect"]==0),"out_detect"] = 0
      df.loc[(df["D2_detect"]==1) & (df["D3_detect"]==0),"out_detect"] = 0
      df.loc[(df["D2_detect"]==0) & (df["D3_detect"]==1),"out_detect"] = 0
      for i in a:
          df = df.loc[df["Sample"]!= i]
      del df["Iresp"]
      
      col = list(df.columns)
      col.insert(2, "sex")
      col.pop(6)
      col.insert(4,"BMI")
      col.pop(7)
      col.insert(5,col.pop(7))
      col.insert(6,col.pop(15))
      col.insert(7,col.pop(12))
      col.insert(8,col.pop(13))
      col.insert(9,col.pop(14))
      col.insert(10,col.pop(11))
      col.insert(11,col.pop(12))
      col.insert(12,col.pop(14))
      col.insert(13,col.pop(26))
     
      df = df[col]
      #merge banco
      #df = df.merge(bi,on="Sample",how="outer")
      banco = df.copy()

      #meta
      


                    
      df = pd.DataFrame({"v":mark,"popu":pop,"tipo":tipo,"vari":bi.columns})
      u_mark = df.v.unique()
      u_mark = list(u_mark[1:len(u_mark)])
      u_popu = df.popu.unique()
      u_popu = list(u_popu[1:len(u_popu)])
      u_tipo = df.tipo.unique()
      u_tipo = list(u_tipo[:len(u_tipo)]) 

      d_popu = {}
      for p in u_popu:
          aux = []
          for i in range(len(pop)):
              if pop[i]==p:
                  aux.append(True)
              else:
                  aux.append(False)
          d_popu[p]=aux
      d_mark = {}
      for p in u_mark:
          aux = []
          for i in range(len(mark)):
              if mark[i]==p:
                  aux.append(True)
              else:
                  aux.append(False)
          d_mark[p]=aux
      d_tipo = {}
      for p in u_tipo:
          aux = []
          for i in range(len(tipo)):
              if tipo[i]==p:
                  aux.append(True)
              else:
                  aux.append(False)
          d_tipo[p]=aux
      

                
                  
              
      col_map_imune = {}
      col_map_imune["type"] = d_tipo
      col_map_imune["mark"] = d_mark
      col_map_imune["population"] = d_popu
      
      
      col_map_meta = {}
      col = list(banco.columns)
      tipo = {}
      u_tipo = ["id","general","treatment","comorbity","drugs","outcome"]
      t = ["id","general","general","general","general","general",
           "treatment","treatment","treatment","treatment",
           "comorbity","comorbity","comorbity",
           "drugs","drugs","drugs","drugs","drugs","drugs","drugs","drugs","drugs","drugs",
           "outcome","outcome","outcome","outcome","outcome","outcome","outcome","outcome","outcome","outcome"]
      for p in u_tipo:
          aux = []
          for i in range(len(t)):
              if t[i]==p:
                  aux.append(True)
              else:
                  aux.append(False)
          tipo[p]=aux
      
      col_map_meta["type"] = tipo
      
      return (banco.copy(),col_map_meta,bi.copy(),col_map_imune)


 #open and clean data
fold = os.getcwd()

banco_meta,col_map_meta,banco_imune,col_map_imune = openCleanData(fold + "/data/20220728_final_data_combined.csv",
                        file_icov= fold+"/data/n_ICOV.csv",
                        file_lung= fold + "/data/n_lung.csv",
                        file_kidney= fold + "/data/n_nefro.csv",
                        file_batch=fold + "/data/batch.csv")

banco_imune = Data(banco_imune,col_map_imune)
banco_imune.remove_row("internalControl")
banco_imune.remove_row("internalCtrl")
banco_imune.save(fold + "/data_aux/banco_immune.dat")
banco_meta = Data(banco_meta,col_map_meta)    
banco_meta.save(fold + "/data_aux/banco_meta.dat")
    
