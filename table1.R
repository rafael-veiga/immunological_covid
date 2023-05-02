
banco = read.csv("data/total.csv")
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

#age_c

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
tab = table(banco$Ethnie,banco$type)
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
tab = table(banco$tx_rank,banco$type)
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




#FK
banco$FK = as.factor(banco$FK)

tab = table(banco$FK,banco$type)
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

#CsA
banco$CsA = as.factor(banco$CsA)

tab = table(banco$CsA,banco$type)
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

