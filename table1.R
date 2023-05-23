
banco = read.csv("data_aux/table1.csv")
banco$type = as.factor(banco$type)
# sex
banco$sex = as.factor(banco$sex)

tab = table(banco$sex,banco$type)
c =2
l = length(tab[,1])
ni = rep(NA,l)
si = rep(NA,l)
nl = rep(NA,l)
sl = rep(NA,l)
nk = rep(NA,l)
sk = rep(NA,l)
nt = rep(NA,l)
st = rep(NA,l)
for(i in 1:l){
  ni[i] = tab[i,"i"]
  nl[i] = tab[i,"l"]
  nk[i] = tab[i,"k"]
  nt[i] = tab[i,"i"]+tab[i,"l"]+tab[i,"k"]
}
for(i in 1:l){
  si[i] = sprintf(100*(ni[i]/sum(ni)), fmt = '%#.1f')
  sl[i] = sprintf(100*(nl[i]/sum(nl)), fmt = '%#.1f') 
  sk[i] = sprintf(100*(nk[i]/sum(nk)), fmt = '%#.1f')
  st[i] = sprintf(100*(nt[i]/sum(nt)), fmt = '%#.1f')
}

for(i in 1:l){
  print(paste(row.names(tab)[i],ni[i],si[i],nl[i],sl[i],nk[i],sk[i],nt[i],st[i],sep=" "))
}

#age
age = trunc(banco$age)
age[age<50] = 0
age[(age>=50) & (age<60)] = 1
age[age>=60] = 2
banco$age = age
tab = table(banco$age,banco$type)
c =2
l = length(tab[,1])
ni = rep(NA,l)
si = rep(NA,l)
nl = rep(NA,l)
sl = rep(NA,l)
nk = rep(NA,l)
sk = rep(NA,l)
nt = rep(NA,l)
st = rep(NA,l)
for(i in 1:l){
  ni[i] = tab[i,"i"]
  nl[i] = tab[i,"l"]
  nk[i] = tab[i,"k"]
  nt[i] = tab[i,"i"]+tab[i,"l"]+tab[i,"k"]
}
for(i in 1:l){
  si[i] = sprintf(100*(ni[i]/sum(ni)), fmt = '%#.1f')
  sl[i] = sprintf(100*(nl[i]/sum(nl)), fmt = '%#.1f') 
  sk[i] = sprintf(100*(nk[i]/sum(nk)), fmt = '%#.1f')
  st[i] = sprintf(100*(nt[i]/sum(nt)), fmt = '%#.1f')
}

for(i in 1:l){
  print(paste(row.names(tab)[i],ni[i],si[i],nl[i],sl[i],nk[i],sk[i],nt[i],st[i],sep=" "))
}

#BMI
bmi = banco$BMI
bmi[bmi < 25] = 0
bmi[bmi>=25] = 1
banco$BMI = as.factor(bmi)
tab = table(banco$BMI,banco$type)
c =2
l = length(tab[,1])
ni = rep(NA,l)
si = rep(NA,l)
nl = rep(NA,l)
sl = rep(NA,l)
nk = rep(NA,l)
sk = rep(NA,l)
nt = rep(NA,l)
st = rep(NA,l)
for(i in 1:l){
  ni[i] = tab[i,"i"]
  nl[i] = tab[i,"l"]
  nk[i] = tab[i,"k"]
  nt[i] = tab[i,"i"]+tab[i,"l"]+tab[i,"k"]
}
for(i in 1:l){
  si[i] = sprintf(100*(ni[i]/sum(ni)), fmt = '%#.1f')
  sl[i] = sprintf(100*(nl[i]/sum(nl)), fmt = '%#.1f') 
  sk[i] = sprintf(100*(nk[i]/sum(nk)), fmt = '%#.1f')
  st[i] = sprintf(100*(nt[i]/sum(nt)), fmt = '%#.1f')
}

for(i in 1:l){
  print(paste(row.names(tab)[i],ni[i],si[i],nl[i],sl[i],nk[i],sk[i],nt[i],st[i],sep=" "))
}

#Ethnie
ethni = banco$Ethnicity
ethni[ethni==1] = 2
ethni[ethni==3] = 1
ethni[ethni==4] = 1
banco$Ethnicity = ethni
tab = table(banco$Ethnicity,banco$type)
c =2
l = length(tab[,1])
ni = rep(NA,l)
si = rep(NA,l)
nl = rep(NA,l)
sl = rep(NA,l)
nk = rep(NA,l)
sk = rep(NA,l)
nt = rep(NA,l)
st = rep(NA,l)
for(i in 1:l){
  ni[i] = tab[i,"i"]
  nl[i] = tab[i,"l"]
  nk[i] = tab[i,"k"]
  nt[i] = tab[i,"i"]+tab[i,"l"]+tab[i,"k"]
}
for(i in 1:l){
  si[i] = sprintf(100*(ni[i]/sum(ni)), fmt = '%#.1f')
  sl[i] = sprintf(100*(nl[i]/sum(nl)), fmt = '%#.1f') 
  sk[i] = sprintf(100*(nk[i]/sum(nk)), fmt = '%#.1f')
  st[i] = sprintf(100*(nt[i]/sum(nt)), fmt = '%#.1f')
}

for(i in 1:l){
  print(paste(row.names(tab)[i],ni[i],si[i],nl[i],sl[i],nk[i],sk[i],nt[i],st[i],sep=" "))
}

#tx_time
tx_time = banco$tx_time
tx_time[tx_time<3] = 2
tx_time[(tx_time>=3) & (tx_time<=10)] = 1
tx_time[tx_time>10] = 0
banco$tx_time = as.factor(tx_time)
tab = table(banco$tx_time,banco$type)
c =2
l = length(tab[,1])
ni = rep(NA,l)
si = rep(NA,l)
nl = rep(NA,l)
sl = rep(NA,l)
nk = rep(NA,l)
sk = rep(NA,l)
nt = rep(NA,l)
st = rep(NA,l)
for(i in 1:l){
  ni[i] = tab[i,"i"]
  nl[i] = tab[i,"l"]
  nk[i] = tab[i,"k"]
  nt[i] = tab[i,"i"]+tab[i,"l"]+tab[i,"k"]
}
for(i in 1:l){
  si[i] = sprintf(100*(ni[i]/sum(ni)), fmt = '%#.1f')
  sl[i] = sprintf(100*(nl[i]/sum(nl)), fmt = '%#.1f') 
  sk[i] = sprintf(100*(nk[i]/sum(nk)), fmt = '%#.1f')
  st[i] = sprintf(100*(nt[i]/sum(nt)), fmt = '%#.1f')
}

for(i in 1:l){
  print(paste(row.names(tab)[i],ni[i],si[i],nl[i],sl[i],nk[i],sk[i],nt[i],st[i],sep=" "))
}

#tx_rank
tx_rank = banco$Transplant_rank
tx_rank[tx_rank==1] = 0
tx_rank[tx_rank>=2] = 1
banco$Transplant_rank = tx_rank
tab = table(banco$Transplant_rank,banco$type)
c =2
l = length(tab[,1])
ni = rep(NA,l)
si = rep(NA,l)
nl = rep(NA,l)
sl = rep(NA,l)
nk = rep(NA,l)
sk = rep(NA,l)
nt = rep(NA,l)
st = rep(NA,l)
for(i in 1:l){
  ni[i] = tab[i,"i"]
  nl[i] = tab[i,"l"]
  nk[i] = tab[i,"k"]
  nt[i] = tab[i,"i"]+tab[i,"l"]+tab[i,"k"]
}
for(i in 1:l){
  si[i] = sprintf(100*(ni[i]/sum(ni)), fmt = '%#.1f')
  sl[i] = sprintf(100*(nl[i]/sum(nl)), fmt = '%#.1f') 
  sk[i] = sprintf(100*(nk[i]/sum(nk)), fmt = '%#.1f')
  st[i] = sprintf(100*(nt[i]/sum(nt)), fmt = '%#.1f')
}

for(i in 1:l){
  print(paste(row.names(tab)[i],ni[i],si[i],nl[i],sl[i],nk[i],sk[i],nt[i],st[i],sep=" "))
}




#diabetes
tab = table(banco$Diabetes,banco$type)
c =2
l = length(tab[,1])
ni = rep(NA,l)
si = rep(NA,l)
nl = rep(NA,l)
sl = rep(NA,l)
nk = rep(NA,l)
sk = rep(NA,l)
nt = rep(NA,l)
st = rep(NA,l)
for(i in 1:l){
  ni[i] = tab[i,"i"]
  nl[i] = tab[i,"l"]
  nk[i] = tab[i,"k"]
  nt[i] = tab[i,"i"]+tab[i,"l"]+tab[i,"k"]
}
for(i in 1:l){
  si[i] = sprintf(100*(ni[i]/sum(ni)), fmt = '%#.1f')
  sl[i] = sprintf(100*(nl[i]/sum(nl)), fmt = '%#.1f') 
  sk[i] = sprintf(100*(nk[i]/sum(nk)), fmt = '%#.1f')
  st[i] = sprintf(100*(nt[i]/sum(nt)), fmt = '%#.1f')
}

for(i in 1:l){
  print(paste(row.names(tab)[i],ni[i],si[i],nl[i],sl[i],nk[i],sk[i],nt[i],st[i],sep=" "))
}

#HTA
banco$HTA = as.factor(banco$HTA)

tab = table(banco$HTA,banco$type)
c =2
l = length(tab[,1])
ni = rep(NA,l)
si = rep(NA,l)
nl = rep(NA,l)
sl = rep(NA,l)
nk = rep(NA,l)
sk = rep(NA,l)
nt = rep(NA,l)
st = rep(NA,l)
for(i in 1:l){
  ni[i] = tab[i,"i"]
  nl[i] = tab[i,"l"]
  nk[i] = tab[i,"k"]
  nt[i] = tab[i,"i"]+tab[i,"l"]+tab[i,"k"]
}
for(i in 1:l){
  si[i] = sprintf(100*(ni[i]/sum(ni)), fmt = '%#.1f')
  sl[i] = sprintf(100*(nl[i]/sum(nl)), fmt = '%#.1f') 
  sk[i] = sprintf(100*(nk[i]/sum(nk)), fmt = '%#.1f')
  st[i] = sprintf(100*(nt[i]/sum(nt)), fmt = '%#.1f')
}

for(i in 1:l){
  print(paste(row.names(tab)[i],ni[i],si[i],nl[i],sl[i],nk[i],sk[i],nt[i],st[i],sep=" "))
}

#CV
banco$CV = as.factor(banco$CV)

tab = table(banco$CV,banco$type)
c =2
l = length(tab[,1])
ni = rep(NA,l)
si = rep(NA,l)
nl = rep(NA,l)
sl = rep(NA,l)
nk = rep(NA,l)
sk = rep(NA,l)
nt = rep(NA,l)
st = rep(NA,l)
for(i in 1:l){
  ni[i] = tab[i,"i"]
  nl[i] = tab[i,"l"]
  nk[i] = tab[i,"k"]
  nt[i] = tab[i,"i"]+tab[i,"l"]+tab[i,"k"]
}
for(i in 1:l){
  si[i] = sprintf(100*(ni[i]/sum(ni)), fmt = '%#.1f')
  sl[i] = sprintf(100*(nl[i]/sum(nl)), fmt = '%#.1f') 
  sk[i] = sprintf(100*(nk[i]/sum(nk)), fmt = '%#.1f')
  st[i] = sprintf(100*(nt[i]/sum(nt)), fmt = '%#.1f')
}

for(i in 1:l){
  print(paste(row.names(tab)[i],ni[i],si[i],nl[i],sl[i],nk[i],sk[i],nt[i],st[i],sep=" "))
}


#GFR<30
banco$GFR_30 = as.factor(banco$GFR_30)

tab = table(banco$GFR_30,banco$type)
c =2
l = length(tab[,1])
ni = rep(NA,l)
si = rep(NA,l)
nl = rep(NA,l)
sl = rep(NA,l)
nk = rep(NA,l)
sk = rep(NA,l)
nt = rep(NA,l)
st = rep(NA,l)
for(i in 1:l){
  ni[i] = tab[i,"i"]
  nl[i] = tab[i,"l"]
  nk[i] = tab[i,"k"]
  nt[i] = tab[i,"i"]+tab[i,"l"]+tab[i,"k"]
}
for(i in 1:l){
  si[i] = sprintf(100*(ni[i]/sum(ni)), fmt = '%#.1f')
  sl[i] = sprintf(100*(nl[i]/sum(nl)), fmt = '%#.1f') 
  sk[i] = sprintf(100*(nk[i]/sum(nk)), fmt = '%#.1f')
  st[i] = sprintf(100*(nt[i]/sum(nt)), fmt = '%#.1f')
}

for(i in 1:l){
  print(paste(row.names(tab)[i],ni[i],si[i],nl[i],sl[i],nk[i],sk[i],nt[i],st[i],sep=" "))
}


#Cancer
banco$Cancer = as.factor(banco$Cancer)

tab = table(banco$Cancer,banco$type)
c =2
l = length(tab[,1])
ni = rep(NA,l)
si = rep(NA,l)
nl = rep(NA,l)
sl = rep(NA,l)
nk = rep(NA,l)
sk = rep(NA,l)
nt = rep(NA,l)
st = rep(NA,l)
for(i in 1:l){
  ni[i] = tab[i,"i"]
  nl[i] = tab[i,"l"]
  nk[i] = tab[i,"k"]
  nt[i] = tab[i,"i"]+tab[i,"l"]+tab[i,"k"]
}
for(i in 1:l){
  si[i] = sprintf(100*(ni[i]/sum(ni)), fmt = '%#.1f')
  sl[i] = sprintf(100*(nl[i]/sum(nl)), fmt = '%#.1f') 
  sk[i] = sprintf(100*(nk[i]/sum(nk)), fmt = '%#.1f')
  st[i] = sprintf(100*(nt[i]/sum(nt)), fmt = '%#.1f')
}

for(i in 1:l){
  print(paste(row.names(tab)[i],ni[i],si[i],nl[i],sl[i],nk[i],sk[i],nt[i],st[i],sep=" "))
}

#Calcineurin
cal = banco$FK
cal[(banco$FK==0) & (banco$CsA==0)] = 0
cal[(banco$FK==1) & (banco$CsA==0)] = 1
cal[(banco$FK==0) & (banco$CsA==1)] = 2
banco$cal = as.factor(cal)

tab = table(banco$cal,banco$type)
c =2
l = length(tab[,1])
ni = rep(NA,l)
si = rep(NA,l)
nl = rep(NA,l)
sl = rep(NA,l)
nk = rep(NA,l)
sk = rep(NA,l)
nt = rep(NA,l)
st = rep(NA,l)
for(i in 1:l){
  ni[i] = tab[i,"i"]
  nl[i] = tab[i,"l"]
  nk[i] = tab[i,"k"]
  nt[i] = tab[i,"i"]+tab[i,"l"]+tab[i,"k"]
}
for(i in 1:l){
  si[i] = sprintf(100*(ni[i]/sum(ni)), fmt = '%#.1f')
  sl[i] = sprintf(100*(nl[i]/sum(nl)), fmt = '%#.1f') 
  sk[i] = sprintf(100*(nk[i]/sum(nk)), fmt = '%#.1f')
  st[i] = sprintf(100*(nt[i]/sum(nt)), fmt = '%#.1f')
}

for(i in 1:l){
  print(paste(row.names(tab)[i],ni[i],si[i],nl[i],sl[i],nk[i],sk[i],nt[i],st[i],sep=" "))
}


#CTC
banco$CTC = as.factor(banco$CTC)

tab = table(banco$CTC,banco$type)
c =2
l = length(tab[,1])
ni = rep(NA,l)
si = rep(NA,l)
nl = rep(NA,l)
sl = rep(NA,l)
nk = rep(NA,l)
sk = rep(NA,l)
nt = rep(NA,l)
st = rep(NA,l)
for(i in 1:l){
  ni[i] = tab[i,"i"]
  nl[i] = tab[i,"l"]
  nk[i] = tab[i,"k"]
  nt[i] = tab[i,"i"]+tab[i,"l"]+tab[i,"k"]
}
for(i in 1:l){
  si[i] = sprintf(100*(ni[i]/sum(ni)), fmt = '%#.1f')
  sl[i] = sprintf(100*(nl[i]/sum(nl)), fmt = '%#.1f') 
  sk[i] = sprintf(100*(nk[i]/sum(nk)), fmt = '%#.1f')
  st[i] = sprintf(100*(nt[i]/sum(nt)), fmt = '%#.1f')
}

for(i in 1:l){
  print(paste(row.names(tab)[i],ni[i],si[i],nl[i],sl[i],nk[i],sk[i],nt[i],st[i],sep=" "))
}


#AZA
banco$AZA = as.factor(banco$AZA)

tab = table(banco$AZA,banco$type)
c =2
l = length(tab[,1])
ni = rep(NA,l)
si = rep(NA,l)
nl = rep(NA,l)
sl = rep(NA,l)
nk = rep(NA,l)
sk = rep(NA,l)
nt = rep(NA,l)
st = rep(NA,l)
for(i in 1:l){
  ni[i] = tab[i,"i"]
  nl[i] = tab[i,"l"]
  nk[i] = tab[i,"k"]
  nt[i] = tab[i,"i"]+tab[i,"l"]+tab[i,"k"]
}
for(i in 1:l){
  si[i] = sprintf(100*(ni[i]/sum(ni)), fmt = '%#.1f')
  sl[i] = sprintf(100*(nl[i]/sum(nl)), fmt = '%#.1f') 
  sk[i] = sprintf(100*(nk[i]/sum(nk)), fmt = '%#.1f')
  st[i] = sprintf(100*(nt[i]/sum(nt)), fmt = '%#.1f')
}

for(i in 1:l){
  print(paste(row.names(tab)[i],ni[i],si[i],nl[i],sl[i],nk[i],sk[i],nt[i],st[i],sep=" "))
}


#MMF
banco$MMF = as.factor(banco$MMF)

tab = table(banco$MMF,banco$type)
c =2
l = length(tab[,1])
ni = rep(NA,l)
si = rep(NA,l)
nl = rep(NA,l)
sl = rep(NA,l)
nk = rep(NA,l)
sk = rep(NA,l)
nt = rep(NA,l)
st = rep(NA,l)
for(i in 1:l){
  ni[i] = tab[i,"i"]
  nl[i] = tab[i,"l"]
  nk[i] = tab[i,"k"]
  nt[i] = tab[i,"i"]+tab[i,"l"]+tab[i,"k"]
}
for(i in 1:l){
  si[i] = sprintf(100*(ni[i]/sum(ni)), fmt = '%#.1f')
  sl[i] = sprintf(100*(nl[i]/sum(nl)), fmt = '%#.1f') 
  sk[i] = sprintf(100*(nk[i]/sum(nk)), fmt = '%#.1f')
  st[i] = sprintf(100*(nt[i]/sum(nt)), fmt = '%#.1f')
}

for(i in 1:l){
  print(paste(row.names(tab)[i],ni[i],si[i],nl[i],sl[i],nk[i],sk[i],nt[i],st[i],sep=" "))
}


#Evero
banco$Evero = as.factor(banco$Evero)

tab = table(banco$Evero,banco$type)
c =2
l = length(tab[,1])
ni = rep(NA,l)
si = rep(NA,l)
nl = rep(NA,l)
sl = rep(NA,l)
nk = rep(NA,l)
sk = rep(NA,l)
nt = rep(NA,l)
st = rep(NA,l)
for(i in 1:l){
  ni[i] = tab[i,"i"]
  nl[i] = tab[i,"l"]
  nk[i] = tab[i,"k"]
  nt[i] = tab[i,"i"]+tab[i,"l"]+tab[i,"k"]
}
for(i in 1:l){
  si[i] = sprintf(100*(ni[i]/sum(ni)), fmt = '%#.1f')
  sl[i] = sprintf(100*(nl[i]/sum(nl)), fmt = '%#.1f') 
  sk[i] = sprintf(100*(nk[i]/sum(nk)), fmt = '%#.1f')
  st[i] = sprintf(100*(nt[i]/sum(nt)), fmt = '%#.1f')
}

for(i in 1:l){
  print(paste(row.names(tab)[i],ni[i],si[i],nl[i],sl[i],nk[i],sk[i],nt[i],st[i],sep=" "))
}



#Induction_treatment
ind = as.integer(banco$Induction_treatment)
ind[ind==0] = NA
ind[ind==1] = 0
ind[ind==2] = 1
ind[ind==3] = 2
banco$Induction_treatment = as.factor(ind)

tab = table(banco$Induction_treatment,banco$type)
c =2
l = length(tab[,1])
ni = rep(NA,l)
si = rep(NA,l)
nl = rep(NA,l)
sl = rep(NA,l)
nk = rep(NA,l)
sk = rep(NA,l)
nt = rep(NA,l)
st = rep(NA,l)
for(i in 1:l){
  ni[i] = tab[i,"i"]
  nl[i] = tab[i,"l"]
  nk[i] = tab[i,"k"]
  nt[i] = tab[i,"i"]+tab[i,"l"]+tab[i,"k"]
}
for(i in 1:l){
  si[i] = sprintf(100*(ni[i]/sum(ni)), fmt = '%#.1f')
  sl[i] = sprintf(100*(nl[i]/sum(nl)), fmt = '%#.1f') 
  sk[i] = sprintf(100*(nk[i]/sum(nk)), fmt = '%#.1f')
  st[i] = sprintf(100*(nt[i]/sum(nt)), fmt = '%#.1f')
}

for(i in 1:l){
  print(paste(row.names(tab)[i],ni[i],si[i],nl[i],sl[i],nk[i],sk[i],nt[i],st[i],sep=" "))
}

#Number_of_treatments
num = banco$Number_of_treatments
num[num<3] = 0
num[num>=3] = 1
banco$n_treatments = num
banco$Number_of_treatments = NULL
tab = table(banco$n_treatments,banco$type)
c =2
l = length(tab[,1])
ni = rep(NA,l)
si = rep(NA,l)
nl = rep(NA,l)
sl = rep(NA,l)
nk = rep(NA,l)
sk = rep(NA,l)
nt = rep(NA,l)
st = rep(NA,l)
for(i in 1:l){
  ni[i] = tab[i,"i"]
  nl[i] = tab[i,"l"]
  nk[i] = tab[i,"k"]
  nt[i] = tab[i,"i"]+tab[i,"l"]+tab[i,"k"]
}
for(i in 1:l){
  si[i] = sprintf(100*(ni[i]/sum(ni)), fmt = '%#.1f')
  sl[i] = sprintf(100*(nl[i]/sum(nl)), fmt = '%#.1f') 
  sk[i] = sprintf(100*(nk[i]/sum(nk)), fmt = '%#.1f')
  st[i] = sprintf(100*(nt[i]/sum(nt)), fmt = '%#.1f')
}

for(i in 1:l){
  print(paste(row.names(tab)[i],ni[i],si[i],nl[i],sl[i],nk[i],sk[i],nt[i],st[i],sep=" "))
}

