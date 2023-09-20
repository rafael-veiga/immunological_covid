library(tidyverse)

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

#test for association of factors with type of transplant
banco = read.csv("data/total.csv")
banco$type = as.factor(banco$type)
# sex
banco$sex = as.factor(banco$sex)
banco$age = as.factor(banco$age)
banco$BMI = as.factor(banco$BMI)
banco$Ethnie = as.factor(banco$Ethnie)
banco$tx_time = as.factor(banco$tx_time)
banco$tx_rank = as.factor(banco$tx_rank)
banco$Induction_treatment = as.factor(banco$Induction_treatment)
banco2 = banco[banco$type!="i",]
#sex
mod = chisq.test(banco2$sex[!is.na(banco2$sex)],banco2$type[!is.na(banco2$sex)])
#age
mod = chisq.test(banco2$age[!is.na(banco2$age)],banco2$type[!is.na(banco2$age)])
#BMI
mod = chisq.test(banco2$BMI[!is.na(banco2$age)],banco2$type)
#Ethnie
mod = chisq.test(banco2$Ethnie,banco2$type)
#tx_time
mod = chisq.test(banco2$tx_time,banco2$type)
#tx_rank
mod = chisq.test(banco2$tx_rank,banco2$type)
#Diabetes
mod = chisq.test(banco2$Diabetes,banco2$type)
#HTA
mod = chisq.test(banco2$HTA,banco2$type)
#CV
mod = chisq.test(banco2$CV,banco2$type)
#GFR_30
mod = chisq.test(banco2$GFR_30,banco2$type)
#Cancer
mod = chisq.test(banco2$Cancer,banco2$type)
#CTC
mod = chisq.test(banco2$CTC,banco2$type)
#AZA
mod = chisq.test(banco2$AZA,banco2$type)
#MMF
mod = chisq.test(banco2$MMF,banco2$type)
#Fk
mod = chisq.test(banco2$FK,banco2$type)
#CsA
mod = chisq.test(banco2$CsA,banco2$type)
#Evero
mod = chisq.test(banco2$Evero,banco2$type)
#induction
mod = chisq.test(banco2$Induction_treatment,banco2$type)
#n_treatments
mod = chisq.test(banco2$n_treatments,banco2$type)

# correlaion

banco = read.csv("data/20230413_clinical_data.csv")
banco = banco[banco$Kidney.Lung.ICOV.healthy.controls!="ICOV",]
ggplot(banco, aes(x = Time.after.transplant..years., y = Age)) +
  facet_wrap(~ Kidney.Lung.ICOV.healthy.controls, scales = "free_x") +
  geom_point() +
  geom_smooth(formula = y ~ x, method = "lm", se = FALSE) +
  ggpubr::stat_cor(aes(label = tolower(..r.label..)), label.y = 8.1) +
  theme_classic() +
  theme(panel.spacing = unit(1, "lines")) +
  labs(x = "Sepal Width",
       y = "Sepal Length",
       title = "Sepal Length vs. Sepal Width in Irises",
       subtitle = "Grouped by Species")
