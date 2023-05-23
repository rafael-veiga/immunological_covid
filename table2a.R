banco = read.csv("data_aux/table2.csv")

banco$d2_d = NA
banco$d2_d[banco$D2_response=="Negative"] = 0
banco$d2_d[banco$D2_response=="Detectable"] = 1
banco$d2_d[banco$D2_response=="Positive"] = 1

banco$d3_d = NA
banco$d3_d[banco$D3_response=="Negative"] = 0
banco$d3_d[banco$D3_response=="Detectable"] = 1
banco$d3_d[banco$D3_response=="Positive"] = 1

banco$d3_p = NA
banco$d3_p[banco$D3_response=="Negative"] = 0
banco$d3_p[banco$D3_response=="Detectable"] = 0
banco$d3_p[banco$D3_response=="Positive"] = 1

banco = banco[banco$type!="i",]

# sex
banco$sex = as.factor(banco$sex)
tab = table(banco$sex,banco$d2_d)
t = tab[,1]+tab[,2]
p = 100*tab[,2]/t
s = rep(NA,length(t))
for(i in 1:length(tab[,2])){
  s[i] = paste0(tab[i,2],"(",sprintf(p[i], fmt = '%#.1f'),")")
}

# age
age = trunc(banco$age)
age[age<50] = 0
age[(age>=50) & (age<60)] = 1
age[age>=60] = 2
banco$age = as.factor(age)
tab = table(banco$age,banco$d2_d)
t = tab[,1]+tab[,2]
p = 100*tab[,2]/t
s = rep(NA,length(t))
for(i in 1:length(tab[,2])){
  s[i] = paste0(tab[i,2],"(",sprintf(p[i], fmt = '%#.1f'),")")
}

#BMI
bmi = banco$BMI
bmi[bmi < 25] = 0
bmi[bmi>=25] = 1
banco$BMI = as.factor(bmi)
tab = table(banco$BMI,banco$d2_d)
t = tab[,1]+tab[,2]
p = 100*tab[,2]/t
s = rep(NA,length(t))
for(i in 1:length(tab[,2])){
  s[i] = paste0(tab[i,2],"(",sprintf(p[i], fmt = '%#.1f'),")")
}

#Ethnie
ethni = banco$Ethnicity
ethni[ethni==1] = 2
ethni[ethni==3] = 1
ethni[ethni==4] = 1
banco$Ethnie = as.factor(ethni)
banco$Ethnicity = NULL
tab = table(banco$Ethnie,banco$d2_d)
t = tab[,1]+tab[,2]
p = 100*tab[,2]/t
s = rep(NA,length(t))
for(i in 1:length(tab[,2])){
  s[i] = paste0(tab[i,2],"(",sprintf(p[i], fmt = '%#.1f'),")")
}

# type
banco$type = as.factor(banco$type)
tab = table(banco$type,banco$d2_d)
t = tab[,1]+tab[,2]
p = 100*tab[,2]/t
s = rep(NA,length(t))
for(i in 1:length(tab[,2])){
  s[i] = paste0(tab[i,2],"(",sprintf(p[i], fmt = '%#.1f'),")")
}



#tx_time
tx_time = banco$tx_time
tx_time[tx_time<3] = 2
tx_time[(tx_time>=3) & (tx_time<=10)] = 1
tx_time[tx_time>10] = 0
banco$tx_time = as.factor(tx_time)
tab = table(banco$tx_time,banco$d2_d)
t = tab[,1]+tab[,2]
p = 100*tab[,2]/t
s = rep(NA,length(t))
for(i in 1:length(tab[,2])){
  s[i] = paste0(tab[i,2],"(",sprintf(p[i], fmt = '%#.1f'),")")
}


#tx_rank
tx_rank = banco$Transplant_rank
tx_rank[tx_rank==1] = 0
tx_rank[tx_rank>=2] = 1
banco$tx_rank = tx_rank
banco$Transplant_rank = NULL
tab = table(banco$tx_rank,banco$d2_d)
t = tab[,1]+tab[,2]
p = 100*tab[,2]/t
s = rep(NA,length(t))
for(i in 1:length(tab[,2])){
  s[i] = paste0(tab[i,2],"(",sprintf(p[i], fmt = '%#.1f'),")")
}

#diabetes
banco$Diabetes = as.factor(banco$Diabetes)
tab = table(banco$Diabetes,banco$d2_d)
t = tab[,1]+tab[,2]
p = 100*tab[,2]/t
s = rep(NA,length(t))
for(i in 1:length(tab[,2])){
  s[i] = paste0(tab[i,2],"(",sprintf(p[i], fmt = '%#.1f'),")")
}

#HTA
banco$HTA = as.factor(banco$HTA)
tab = table(banco$HTA,banco$d2_d)
t = tab[,1]+tab[,2]
p = 100*tab[,2]/t
s = rep(NA,length(t))
for(i in 1:length(tab[,2])){
  s[i] = paste0(tab[i,2],"(",sprintf(p[i], fmt = '%#.1f'),")")
}

#CV
banco$CV = as.factor(banco$CV)
tab = table(banco$CV,banco$d2_d)
t = tab[,1]+tab[,2]
p = 100*tab[,2]/t
s = rep(NA,length(t))
for(i in 1:length(tab[,2])){
  s[i] = paste0(tab[i,2],"(",sprintf(p[i], fmt = '%#.1f'),")")
}

#GFR<30
banco$GFR_30 = as.factor(banco$GFR_30)
tab = table(banco$GFR_30,banco$d2_d)
t = tab[,1]+tab[,2]
p = 100*tab[,2]/t
s = rep(NA,length(t))
for(i in 1:length(tab[,2])){
  s[i] = paste0(tab[i,2],"(",sprintf(p[i], fmt = '%#.1f'),")")
}


#Cancer
banco$Cancer = as.factor(banco$Cancer)
tab = table(banco$Cancer,banco$d2_d)
t = tab[,1]+tab[,2]
p = 100*tab[,2]/t
s = rep(NA,length(t))
for(i in 1:length(tab[,2])){
  s[i] = paste0(tab[i,2],"(",sprintf(p[i], fmt = '%#.1f'),")")
}

#Calcineurin
cal = banco$FK
cal[(banco$FK==0) & (banco$CsA==0)] = 0
cal[(banco$FK==1) & (banco$CsA==0)] = 1
cal[(banco$FK==0) & (banco$CsA==1)] = 2
cal = as.factor(cal)
banco$FK = NULL
banco$CsA = NULL
banco$cal = as.factor(cal)
tab = table(banco$cal,banco$d2_d)
t = tab[,1]+tab[,2]
p = 100*tab[,2]/t
s = rep(NA,length(t))
for(i in 1:length(tab[,2])){
  s[i] = paste0(tab[i,2],"(",sprintf(p[i], fmt = '%#.1f'),")")
}

#CTC
banco$CTC = as.factor(banco$CTC)
tab = table(banco$CTC,banco$d2_d)
t = tab[,1]+tab[,2]
p = 100*tab[,2]/t
s = rep(NA,length(t))
for(i in 1:length(tab[,2])){
  s[i] = paste0(tab[i,2],"(",sprintf(p[i], fmt = '%#.1f'),")")
}

#AZA
banco$AZA = as.factor(banco$AZA)
tab = table(banco$AZA,banco$d2_d)
t = tab[,1]+tab[,2]
p = 100*tab[,2]/t
s = rep(NA,length(t))
for(i in 1:length(tab[,2])){
  s[i] = paste0(tab[i,2],"(",sprintf(p[i], fmt = '%#.1f'),")")
}

#MMF
banco$MMF = as.factor(banco$MMF)
tab = table(banco$MMF,banco$d2_d)
t = tab[,1]+tab[,2]
p = 100*tab[,2]/t
s = rep(NA,length(t))
for(i in 1:length(tab[,2])){
  s[i] = paste0(tab[i,2],"(",sprintf(p[i], fmt = '%#.1f'),")")
}

#Evero
banco$Evero = as.factor(banco$Evero)
tab = table(banco$Evero,banco$d2_d)
t = tab[,1]+tab[,2]
p = 100*tab[,2]/t
s = rep(NA,length(t))
for(i in 1:length(tab[,2])){
  s[i] = paste0(tab[i,2],"(",sprintf(p[i], fmt = '%#.1f'),")")
}



#Induction treatment
ind = as.integer(banco$Induction_treatment)
ind[ind==0] = NA
ind[ind==1] = 0
ind[ind==2] = 1
ind[ind==3] = 2
banco$Induction_treatment = as.factor(ind)
tab = table(banco$Induction_treatment,banco$d2_d)
t = tab[,1]+tab[,2]
p = 100*tab[,2]/t
s = rep(NA,length(t))
for(i in 1:length(tab[,2])){
  s[i] = paste0(tab[i,2],"(",sprintf(p[i], fmt = '%#.1f'),")")
}

#n_treatments
num = banco$Number_of_treatments
num[num<3] = 0
num[num>=3] = 1
banco$n_treatments = num
banco$Number_of_treatments = NULL

tab = table(banco$n_treatments,banco$d2_d)
t = tab[,1]+tab[,2]
p = 100*tab[,2]/t
s = rep(NA,length(t))
for(i in 1:length(tab[,2])){
  s[i] = paste0(tab[i,2],"(",sprintf(p[i], fmt = '%#.1f'),")")
}

#######################################################################
# model selection
# D2_detect
#var = c("n_treatments","Induction_treatment","Cancer","GFR_30","CV","HTA","Diabetes","tx_rank","Ethnie","BMI","tx_time","type","cal","CTC","AZA","MMF","Evero")
var = c("n_treatments","Cancer","CV","HTA","Diabetes","tx_rank","Ethnie","BMI","tx_time","type","cal","CTC","AZA","Evero")
acepted = c("MMF","GFR_30","Induction_treatment")

#forward
ac = ""
for(v in acepted){
  ac = paste0(ac,"+",v)
}
aic = rep(NA,length(var))
for(i in 1:length(var)){
  s = paste0("d2_d~",var[i],"+sex+age", ac)
  model = glm(s,data = banco,family = binomial(link = "logit") )
  aic[i] = model$aic
  print(paste0(var[i]," : ", aic[i]))
}
#best
i = which.min(aic)
print(paste0(var[i]," : ", aic[i]))
ante = aic[i]


#backward
aic = rep(NA,length(acepted))
for(i in 1:length(acepted)){
  ac = ""
  for(l in 1:length(acepted)){
    if (i!=l){
      ac = paste0(ac,"+",acepted[l])
    }
  }
  s = paste0("d2_d~","sex+age", ac)
  model = glm(s,data = banco,family = binomial(link = "logit") )
  aic[i] = model$aic
  print(paste0(acepted[i]," : ", aic[i]))
}
#best
i = which.min(aic)
print(paste0(acepted[i]," : ", aic[i]))
print(paste0("before ",ante))
print(paste0("after ",aic[i]))

#####################################################################
#best model
#sex+age+MMF+GFR_30+Induction_treatment"
var ="+sex+age+MMF+GFR_30+Induction_treatment"
# regression
# sex
v="sex"
model = glm(paste0("d2_d~",v,var),data = banco,family = binomial(link = "logit") )
or = exp(coefficients(model))[2]
ci = exp(confint(model))
c1 = ci[2,][1]
c2 = ci[2,][2]
s = paste0(sprintf(or, fmt = '%#.2f'),"(",sprintf(c1, fmt = '%#.2f'),"-",sprintf(c2, fmt = '%#.2f'),")")
summary(model)

# age
v="age"
model = glm(paste0("d2_d~",v,var),data = banco,family = binomial(link = "logit") )
or = exp(coefficients(model))[2]
ci = exp(confint(model))
c1 = ci[2,][1]
c2 = ci[2,][2]
s = paste0(sprintf(or, fmt = '%#.2f'),"(",sprintf(c1, fmt = '%#.2f'),"-",sprintf(c2, fmt = '%#.2f'),")")

or = exp(coefficients(model))[3]
ci = exp(confint(model))
c1 = ci[3,][1]
c2 = ci[3,][2]
s2 = paste0(sprintf(or, fmt = '%#.2f'),"(",sprintf(c1, fmt = '%#.2f'),"-",sprintf(c2, fmt = '%#.2f'),")")
summary(model)

# BMI
v="BMI"
model = glm(paste0("d2_d~",v,var),data = banco,family = binomial(link = "logit") )
or = exp(coefficients(model))[2]
ci = exp(confint(model))
c1 = ci[2,][1]
c2 = ci[2,][2]
s = paste0(sprintf(or, fmt = '%#.2f'),"(",sprintf(c1, fmt = '%#.2f'),"-",sprintf(c2, fmt = '%#.2f'),")")

or = exp(coefficients(model))[3]
ci = exp(confint(model))
c1 = ci[3,][1]
c2 = ci[3,][2]
s2 = paste0(sprintf(or, fmt = '%#.2f'),"(",sprintf(c1, fmt = '%#.2f'),"-",sprintf(c2, fmt = '%#.2f'),")")
summary(model)

# Ethnie
v="Ethnie"
model = glm(paste0("d2_d~",v,var),data = banco,family = binomial(link = "logit") )
or = exp(coefficients(model))[2]
ci = exp(confint(model))
c1 = ci[2,][1]
c2 = ci[2,][2]
s = paste0(sprintf(or, fmt = '%#.2f'),"(",sprintf(c1, fmt = '%#.2f'),"-",sprintf(c2, fmt = '%#.2f'),")")

or = exp(coefficients(model))[3]
ci = exp(confint(model))
c1 = ci[3,][1]
c2 = ci[3,][2]
s2 = paste0(sprintf(or, fmt = '%#.2f'),"(",sprintf(c1, fmt = '%#.2f'),"-",sprintf(c2, fmt = '%#.2f'),")")
summary(model)

# type
v="type"
model = glm(paste0("d2_d~",v,var),data = banco,family = binomial(link = "logit") )
or = exp(coefficients(model))[2]
ci = exp(confint(model))
c1 = ci[2,][1]
c2 = ci[2,][2]
s = paste0(sprintf(or, fmt = '%#.2f'),"(",sprintf(c1, fmt = '%#.2f'),"-",sprintf(c2, fmt = '%#.2f'),")")

or = exp(coefficients(model))[3]
ci = exp(confint(model))
c1 = ci[3,][1]
c2 = ci[3,][2]
s2 = paste0(sprintf(or, fmt = '%#.2f'),"(",sprintf(c1, fmt = '%#.2f'),"-",sprintf(c2, fmt = '%#.2f'),")")
summary(model)

# tx_time
v="tx_time"
model = glm(paste0("d2_d~",v,var),data = banco,family = binomial(link = "logit") )
or = exp(coefficients(model))[2]
ci = exp(confint(model))
c1 = ci[2,][1]
c2 = ci[2,][2]
s = paste0(sprintf(or, fmt = '%#.2f'),"(",sprintf(c1, fmt = '%#.2f'),"-",sprintf(c2, fmt = '%#.2f'),")")

or = exp(coefficients(model))[3]
ci = exp(confint(model))
c1 = ci[3,][1]
c2 = ci[3,][2]
s2 = paste0(sprintf(or, fmt = '%#.2f'),"(",sprintf(c1, fmt = '%#.2f'),"-",sprintf(c2, fmt = '%#.2f'),")")
summary(model)

# tx_rank
v="tx_rank"
model = glm(paste0("d2_d~",v,var),data = banco,family = binomial(link = "logit") )
or = exp(coefficients(model))[2]
ci = exp(confint(model))
c1 = ci[2,][1]
c2 = ci[2,][2]
s = paste0(sprintf(or, fmt = '%#.2f'),"(",sprintf(c1, fmt = '%#.2f'),"-",sprintf(c2, fmt = '%#.2f'),")")

or = exp(coefficients(model))[3]
ci = exp(confint(model))
c1 = ci[3,][1]
c2 = ci[3,][2]
s2 = paste0(sprintf(or, fmt = '%#.2f'),"(",sprintf(c1, fmt = '%#.2f'),"-",sprintf(c2, fmt = '%#.2f'),")")
summary(model)

# Diabetes
v="Diabetes"
model = glm(paste0("d2_d~",v,var),data = banco,family = binomial(link = "logit") )
or = exp(coefficients(model))[2]
ci = exp(confint(model))
c1 = ci[2,][1]
c2 = ci[2,][2]
s = paste0(sprintf(or, fmt = '%#.2f'),"(",sprintf(c1, fmt = '%#.2f'),"-",sprintf(c2, fmt = '%#.2f'),")")

or = exp(coefficients(model))[3]
ci = exp(confint(model))
c1 = ci[3,][1]
c2 = ci[3,][2]
s2 = paste0(sprintf(or, fmt = '%#.2f'),"(",sprintf(c1, fmt = '%#.2f'),"-",sprintf(c2, fmt = '%#.2f'),")")
summary(model)

# HTA
v="HTA"
model = glm(paste0("d2_d~",v,var),data = banco,family = binomial(link = "logit") )
or = exp(coefficients(model))[2]
ci = exp(confint(model))
c1 = ci[2,][1]
c2 = ci[2,][2]
s = paste0(sprintf(or, fmt = '%#.2f'),"(",sprintf(c1, fmt = '%#.2f'),"-",sprintf(c2, fmt = '%#.2f'),")")

or = exp(coefficients(model))[3]
ci = exp(confint(model))
c1 = ci[3,][1]
c2 = ci[3,][2]
s2 = paste0(sprintf(or, fmt = '%#.2f'),"(",sprintf(c1, fmt = '%#.2f'),"-",sprintf(c2, fmt = '%#.2f'),")")
summary(model)

# CV
v="CV"
model = glm(paste0("d2_d~",v,var),data = banco,family = binomial(link = "logit") )
or = exp(coefficients(model))[2]
ci = exp(confint(model))
c1 = ci[2,][1]
c2 = ci[2,][2]
s = paste0(sprintf(or, fmt = '%#.2f'),"(",sprintf(c1, fmt = '%#.2f'),"-",sprintf(c2, fmt = '%#.2f'),")")

or = exp(coefficients(model))[3]
ci = exp(confint(model))
c1 = ci[3,][1]
c2 = ci[3,][2]
s2 = paste0(sprintf(or, fmt = '%#.2f'),"(",sprintf(c1, fmt = '%#.2f'),"-",sprintf(c2, fmt = '%#.2f'),")")
summary(model)

# GFR_30
v="GFR_30"
model = glm(paste0("d2_d~",v,var),data = banco,family = binomial(link = "logit") )
or = exp(coefficients(model))[2]
ci = exp(confint(model))
c1 = ci[2,][1]
c2 = ci[2,][2]
s = paste0(sprintf(or, fmt = '%#.2f'),"(",sprintf(c1, fmt = '%#.2f'),"-",sprintf(c2, fmt = '%#.2f'),")")

or = exp(coefficients(model))[3]
ci = exp(confint(model))
c1 = ci[3,][1]
c2 = ci[3,][2]
s2 = paste0(sprintf(or, fmt = '%#.2f'),"(",sprintf(c1, fmt = '%#.2f'),"-",sprintf(c2, fmt = '%#.2f'),")")
summary(model)

# Cancer
v="Cancer"
model = glm(paste0("d2_d~",v,var),data = banco,family = binomial(link = "logit") )
or = exp(coefficients(model))[2]
ci = exp(confint(model))
c1 = ci[2,][1]
c2 = ci[2,][2]
s = paste0(sprintf(or, fmt = '%#.2f'),"(",sprintf(c1, fmt = '%#.2f'),"-",sprintf(c2, fmt = '%#.2f'),")")

or = exp(coefficients(model))[3]
ci = exp(confint(model))
c1 = ci[3,][1]
c2 = ci[3,][2]
s2 = paste0(sprintf(or, fmt = '%#.2f'),"(",sprintf(c1, fmt = '%#.2f'),"-",sprintf(c2, fmt = '%#.2f'),")")
summary(model)


var ="+sex+age+MMF+GFR_30+Induction_treatment"
# cal
v="cal"
model = glm(paste0("d2_d~",v,var),data = banco,family = binomial(link = "logit") )
or = exp(coefficients(model))[2]
ci = exp(confint(model))
c1 = ci[2,][1]
c2 = ci[2,][2]
s = paste0(sprintf(or, fmt = '%#.2f'),"(",sprintf(c1, fmt = '%#.2f'),"-",sprintf(c2, fmt = '%#.2f'),")")

or = exp(coefficients(model))[3]
ci = exp(confint(model))
c1 = ci[3,][1]
c2 = ci[3,][2]
s2 = paste0(sprintf(or, fmt = '%#.2f'),"(",sprintf(c1, fmt = '%#.2f'),"-",sprintf(c2, fmt = '%#.2f'),")")
summary(model)

# CTC
v="CTC"
model = glm(paste0("d2_d~",v,var),data = banco,family = binomial(link = "logit") )
or = exp(coefficients(model))[2]
ci = exp(confint(model))
c1 = ci[2,][1]
c2 = ci[2,][2]
s = paste0(sprintf(or, fmt = '%#.2f'),"(",sprintf(c1, fmt = '%#.2f'),"-",sprintf(c2, fmt = '%#.2f'),")")

or = exp(coefficients(model))[3]
ci = exp(confint(model))
c1 = ci[3,][1]
c2 = ci[3,][2]
s2 = paste0(sprintf(or, fmt = '%#.2f'),"(",sprintf(c1, fmt = '%#.2f'),"-",sprintf(c2, fmt = '%#.2f'),")")
summary(model)

# AZA
v="AZA"
model = glm(paste0("d2_d~",v,var),data = banco,family = binomial(link = "logit") )
or = exp(coefficients(model))[2]
ci = exp(confint(model))
c1 = ci[2,][1]
c2 = ci[2,][2]
s = paste0(sprintf(or, fmt = '%#.2f'),"(",sprintf(c1, fmt = '%#.2f'),"-",sprintf(c2, fmt = '%#.2f'),")")

or = exp(coefficients(model))[3]
ci = exp(confint(model))
c1 = ci[3,][1]
c2 = ci[3,][2]
s2 = paste0(sprintf(or, fmt = '%#.2f'),"(",sprintf(c1, fmt = '%#.2f'),"-",sprintf(c2, fmt = '%#.2f'),")")
summary(model)

# MMF
v="MMF"
model = glm(paste0("d2_d~",v,var),data = banco,family = binomial(link = "logit") )
or = exp(coefficients(model))[2]
ci = exp(confint(model))
c1 = ci[2,][1]
c2 = ci[2,][2]
s = paste0(sprintf(or, fmt = '%#.2f'),"(",sprintf(c1, fmt = '%#.2f'),"-",sprintf(c2, fmt = '%#.2f'),")")

or = exp(coefficients(model))[3]
ci = exp(confint(model))
c1 = ci[3,][1]
c2 = ci[3,][2]
s2 = paste0(sprintf(or, fmt = '%#.2f'),"(",sprintf(c1, fmt = '%#.2f'),"-",sprintf(c2, fmt = '%#.2f'),")")
summary(model)

# Evero
v="Evero"
model = glm(paste0("d2_d~",v,var),data = banco,family = binomial(link = "logit") )
or = exp(coefficients(model))[2]
ci = exp(confint(model))
c1 = ci[2,][1]
c2 = ci[2,][2]
s = paste0(sprintf(or, fmt = '%#.2f'),"(",sprintf(c1, fmt = '%#.2f'),"-",sprintf(c2, fmt = '%#.2f'),")")

or = exp(coefficients(model))[3]
ci = exp(confint(model))
c1 = ci[3,][1]
c2 = ci[3,][2]
s2 = paste0(sprintf(or, fmt = '%#.2f'),"(",sprintf(c1, fmt = '%#.2f'),"-",sprintf(c2, fmt = '%#.2f'),")")
summary(model)


# Induction_treatment
v="Induction_treatment"
model = glm(paste0("d2_d~",v,var),data = banco,family = binomial(link = "logit") )
or = exp(coefficients(model))[2]
ci = exp(confint(model))
c1 = ci[2,][1]
c2 = ci[2,][2]
s = paste0(sprintf(or, fmt = '%#.2f'),"(",sprintf(c1, fmt = '%#.2f'),"-",sprintf(c2, fmt = '%#.2f'),")")

or = exp(coefficients(model))[3]
ci = exp(confint(model))
c1 = ci[3,][1]
c2 = ci[3,][2]
s2 = paste0(sprintf(or, fmt = '%#.2f'),"(",sprintf(c1, fmt = '%#.2f'),"-",sprintf(c2, fmt = '%#.2f'),")")
summary(model)

var ="+sex+age+GFR_30"

# n_treatments
v="n_treatments"
model = glm(paste0("d2_d~",v,var),data = banco,family = binomial(link = "logit") )
or = exp(coefficients(model))[2]
ci = exp(confint(model))
c1 = ci[2,][1]
c2 = ci[2,][2]
s = paste0(sprintf(or, fmt = '%#.2f'),"(",sprintf(c1, fmt = '%#.2f'),"-",sprintf(c2, fmt = '%#.2f'),")")

or = exp(coefficients(model))[3]
ci = exp(confint(model))
c1 = ci[3,][1]
c2 = ci[3,][2]
s2 = paste0(sprintf(or, fmt = '%#.2f'),"(",sprintf(c1, fmt = '%#.2f'),"-",sprintf(c2, fmt = '%#.2f'),")")
summary(model)