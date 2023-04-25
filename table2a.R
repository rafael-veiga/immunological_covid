banco = read.csv("./data_aux/table2.csv")

# type
banco$type = as.factor(banco$type)
tab = table(banco$type,banco$D2_detect)
t = tab[,1]+tab[,2]
p = 100*tab[,2]/t
s = rep(NA,length(t))
for(i in 1:length(tab[,2])){
  s[i] = paste0(tab[i,2],"(",sprintf(p[i], fmt = '%#.1f'),")")
}

#Age
banco$age_c = rep(NA,length(banco$age))
banco$age_c[banco$age<=50] = 0
banco$age_c[(banco$age>50)& (banco$age<=70)] = 1
banco$age_c[banco$age>70] = 2

banco$age_c = as.factor(banco$age_c)
tab = table(banco$age_c,banco$D2_detect)
t = tab[,1]+tab[,2]
p = 100*tab[,2]/t
s = rep(NA,length(t))
for(i in 1:length(tab[,2])){
  s[i] = paste0(tab[i,2],"(",sprintf(p[i], fmt = '%#.1f'),")")
}
# sex
banco$sex = as.factor(banco$sex)
tab = table(banco$sex,banco$D2_detect)
t = tab[,1]+tab[,2]
p = 100*tab[,2]/t
s = rep(NA,length(t))
for(i in 1:length(tab[,2])){
  s[i] = paste0(tab[i,2],"(",sprintf(p[i], fmt = '%#.1f'),")")
}

# BMI apenas 1 underweight
banco$BMI_c = rep(NA,length(banco$BMI))
banco$BMI_c[banco$BMI<25] = 0
banco$BMI_c[(banco$BMI>=25)& (banco$BMI<30)] = 1
banco$BMI_c[banco$BMI>=30] = 2

banco$BMI_c = as.factor(banco$BMI_c)
tab = table(banco$BMI_c,banco$D2_detect)
t = tab[,1]+tab[,2]
p = 100*tab[,2]/t
s = rep(NA,length(t))
for(i in 1:length(tab[,2])){
  s[i] = paste0(tab[i,2],"(",sprintf(p[i], fmt = '%#.1f'),")")
}
#Underweight = <18.5
#Normal = 18.5-24.9
#Overweight = 25-29.9
#Obesity = BMI of 30

#Ethnie
banco$Ethnicity[banco$Ethnicity==0] = 0
banco$Ethnicity[banco$Ethnicity==1] = 2
banco$Ethnicity[banco$Ethnicity==2] = 2 # only 1 case
banco$Ethnicity[banco$Ethnicity==3] = 1
banco$Ethnicity[banco$Ethnicity==4] = 1
banco$Ethnicity = as.factor(banco$Ethnicity)
tab = table(banco$Ethnicity,banco$D2_detect)
t = tab[,1]+tab[,2]
p = 100*tab[,2]/t
s = rep(NA,length(t))
for(i in 1:length(tab[,2])){
  s[i] = paste0(tab[i,2],"(",sprintf(p[i], fmt = '%#.1f'),")")
}

#tx_time
banco$tx_time_c = rep(NA,length(banco$tx_time))
banco$tx_time_c[banco$tx_time<=3] = 2
banco$tx_time_c[(banco$tx_time>3)& (banco$tx_time<=10)] = 1
banco$tx_time_c[banco$tx_time>10] = 0

banco$tx_time_c = as.factor(banco$tx_time_c)
tab = table(banco$tx_time_c,banco$D2_detect)
t = tab[,1]+tab[,2]
p = 100*tab[,2]/t
s = rep(NA,length(t))
for(i in 1:length(tab[,2])){
  s[i] = paste0(tab[i,2],"(",sprintf(p[i], fmt = '%#.1f'),")")
}


#Transplant rank
banco$Transplant_rank[banco$Transplant_rank==1] = 0
banco$Transplant_rank[banco$Transplant_rank==2] = 1
banco$Transplant_rank[banco$Transplant_rank==3] = 1
banco$Transplant_rank = as.factor(banco$Transplant_rank)

tab = table(banco$Transplant_rank,banco$D2_detect)
t = tab[,1]+tab[,2]
p = 100*tab[,2]/t
s = rep(NA,length(t))
for(i in 1:length(tab[,2])){
  s[i] = paste0(tab[i,2],"(",sprintf(p[i], fmt = '%#.1f'),")")
}

#diabetes
banco$Diabetes = as.factor(banco$Diabetes)
tab = table(banco$Diabetes,banco$D2_detect)
t = tab[,1]+tab[,2]
p = 100*tab[,2]/t
s = rep(NA,length(t))
for(i in 1:length(tab[,2])){
  s[i] = paste0(tab[i,2],"(",sprintf(p[i], fmt = '%#.1f'),")")
}

#HTA
banco$HTA = as.factor(banco$HTA)
tab = table(banco$HTA,banco$D2_detect)
t = tab[,1]+tab[,2]
p = 100*tab[,2]/t
s = rep(NA,length(t))
for(i in 1:length(tab[,2])){
  s[i] = paste0(tab[i,2],"(",sprintf(p[i], fmt = '%#.1f'),")")
}

#CV
banco$CV = as.factor(banco$CV)
tab = table(banco$CV,banco$D2_detect)
t = tab[,1]+tab[,2]
p = 100*tab[,2]/t
s = rep(NA,length(t))
for(i in 1:length(tab[,2])){
  s[i] = paste0(tab[i,2],"(",sprintf(p[i], fmt = '%#.1f'),")")
}

#GFR<30
banco$GFR_30 = as.factor(banco$GFR_30)
tab = table(banco$GFR_30,banco$D2_detect)
t = tab[,1]+tab[,2]
p = 100*tab[,2]/t
s = rep(NA,length(t))
for(i in 1:length(tab[,2])){
  s[i] = paste0(tab[i,2],"(",sprintf(p[i], fmt = '%#.1f'),")")
}


#Cancer
banco$Cancer = as.factor(banco$Cancer)
tab = table(banco$Cancer,banco$D2_detect)
t = tab[,1]+tab[,2]
p = 100*tab[,2]/t
s = rep(NA,length(t))
for(i in 1:length(tab[,2])){
  s[i] = paste0(tab[i,2],"(",sprintf(p[i], fmt = '%#.1f'),")")
}

#FK
banco$FK = as.factor(banco$FK)
tab = table(banco$FK,banco$D2_detect)
t = tab[,1]+tab[,2]
p = 100*tab[,2]/t
s = rep(NA,length(t))
for(i in 1:length(tab[,2])){
  s[i] = paste0(tab[i,2],"(",sprintf(p[i], fmt = '%#.1f'),")")
}

#CTC
banco$CTC = as.factor(banco$CTC)
tab = table(banco$CTC,banco$D2_detect)
t = tab[,1]+tab[,2]
p = 100*tab[,2]/t
s = rep(NA,length(t))
for(i in 1:length(tab[,2])){
  s[i] = paste0(tab[i,2],"(",sprintf(p[i], fmt = '%#.1f'),")")
}

#AZA
banco$AZA = as.factor(banco$AZA)
tab = table(banco$AZA,banco$D2_detect)
t = tab[,1]+tab[,2]
p = 100*tab[,2]/t
s = rep(NA,length(t))
for(i in 1:length(tab[,2])){
  s[i] = paste0(tab[i,2],"(",sprintf(p[i], fmt = '%#.1f'),")")
}

#MMF
banco$MMF = as.factor(banco$MMF)
tab = table(banco$MMF,banco$D2_detect)
t = tab[,1]+tab[,2]
p = 100*tab[,2]/t
s = rep(NA,length(t))
for(i in 1:length(tab[,2])){
  s[i] = paste0(tab[i,2],"(",sprintf(p[i], fmt = '%#.1f'),")")
}

#Evero
banco$Evero = as.factor(banco$Evero)
tab = table(banco$Evero,banco$D2_detect)
t = tab[,1]+tab[,2]
p = 100*tab[,2]/t
s = rep(NA,length(t))
for(i in 1:length(tab[,2])){
  s[i] = paste0(tab[i,2],"(",sprintf(p[i], fmt = '%#.1f'),")")
}

#CsA
banco$CsA = as.factor(banco$CsA)
tab = table(banco$CsA,banco$D2_detect)
t = tab[,1]+tab[,2]
p = 100*tab[,2]/t
s = rep(NA,length(t))
for(i in 1:length(tab[,2])){
  s[i] = paste0(tab[i,2],"(",sprintf(p[i], fmt = '%#.1f'),")")
}

#Induction treatment
banco$Induction_treatment[banco$Induction_treatment==0] = NA
banco$Induction_treatment[banco$Induction_treatment==1] = 0
banco$Induction_treatment[banco$Induction_treatment==2] = 1
banco$Induction_treatment[banco$Induction_treatment==3] = 2
banco$Induction_treatment = as.factor(banco$Induction_treatment)

tab = table(banco$Induction_treatment,banco$D2_detect)
t = tab[,1]+tab[,2]
p = 100*tab[,2]/t
s = rep(NA,length(t))
for(i in 1:length(tab[,2])){
  s[i] = paste0(tab[i,2],"(",sprintf(p[i], fmt = '%#.1f'),")")
}

#Transplant rank
banco$Number_of_treatments[banco$Number_of_treatments==2] = 0
banco$Number_of_treatments[banco$Number_of_treatments==3] = 1
banco$Number_of_treatments = as.factor(banco$Number_of_treatments)

tab = table(banco$Transplant_rank,banco$D2_detect)
t = tab[,1]+tab[,2]
p = 100*tab[,2]/t
s = rep(NA,length(t))
for(i in 1:length(tab[,2])){
  s[i] = paste0(tab[i,2],"(",sprintf(p[i], fmt = '%#.1f'),")")
}

#######################################################################
# model selection
# D2_detect
#var = c("Number_of_treatments","Induction_treatment","Cancer","GFR_30","CV","HTA","Diabetes","Transplant_rank","Ethnicity","BMI_c","tx_time_c","type","FK","CTC","AZA","MMF","Evero","CsA")
var = c("Induction_treatment","Cancer","CV","HTA","Diabetes","Transplant_rank","Ethnicity","BMI_c","tx_time_c","type","FK","CTC","AZA","Evero","CsA")
acepted = c("MMF","GFR_30","Number_of_treatments")

#forward
ac = ""
for(v in acepted){
  ac = paste0(ac,"+",v)
}
aic = rep(NA,length(var))
for(i in 1:length(var)){
  s = paste0("D2_detect~",var[i],"+sex+age_c", ac)
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
  s = paste0("D2_detect~","sex+age_c", ac)
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
#sex+age_c+MMF+GFR_30+Number_of_treatments

# regression
# sex
model = glm(D2_detect~sex+age_c+MMF+GFR_30+Number_of_treatments,data = banco,family = binomial(link = "logit") )
or = exp(coefficients(model))[2]
ci = exp(confint(model))
c1 = ci[2,][1]
c2 = ci[2,][2]
s = paste0(sprintf(or, fmt = '%#.2f'),"(",sprintf(c1, fmt = '%#.2f'),"-",sprintf(c2, fmt = '%#.2f'),")")
summary(model)

# age_c
model = glm(D2_detect~age_c+sex+MMF+GFR_30+Number_of_treatments,data = banco,family = binomial(link = "logit") )
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

#BMI
model = glm(D2_detect~BMI_c+sex+age_c+MMF+GFR_30+Number_of_treatments,data = banco,family = binomial(link = "logit") )
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

#Ethnicity
model = glm(D2_detect~Ethnicity+sex+age_c+MMF+GFR_30+Number_of_treatments,data = banco,family = binomial(link = "logit") )
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

# Transplant type
model = glm(D2_detect~type+sex+age_c+MMF+GFR_30+Number_of_treatments,data = banco,family = binomial(link = "logit") )
or = exp(coefficients(model))[2]
ci = exp(confint(model))
c1 = ci[2,][1]
c2 = ci[2,][2]
s = paste0(sprintf(or, fmt = '%#.2f'),"(",sprintf(c1, fmt = '%#.2f'),"-",sprintf(c2, fmt = '%#.2f'),")")
summary(model)


#tx_time
model = glm(D2_detect~tx_time_c+sex+age_c+MMF+GFR_30+Number_of_treatments,data = banco,family = binomial(link = "logit") )
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

#Transplant rank
model = glm(D2_detect~Transplant_rank+sex+age_c+MMF+GFR_30+Number_of_treatments,data = banco,family = binomial(link = "logit") )
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

#Diabetes
model = glm(D2_detect~Diabetes+sex+age_c+MMF+GFR_30+Number_of_treatments,data = banco,family = binomial(link = "logit") )
or = exp(coefficients(model))[2]
ci = exp(confint(model))
c1 = ci[2,][1]
c2 = ci[2,][2]
s = paste0(sprintf(or, fmt = '%#.2f'),"(",sprintf(c1, fmt = '%#.2f'),"-",sprintf(c2, fmt = '%#.2f'),")")
summary(model)

#HTA
model = glm(D2_detect~HTA+sex+age_c+MMF+GFR_30+Number_of_treatments,data = banco,family = binomial(link = "logit") )
or = exp(coefficients(model))[2]
ci = exp(confint(model))
c1 = ci[2,][1]
c2 = ci[2,][2]
s = paste0(sprintf(or, fmt = '%#.2f'),"(",sprintf(c1, fmt = '%#.2f'),"-",sprintf(c2, fmt = '%#.2f'),")")
summary(model)

#CV
model = glm(D2_detect~CV+sex+age_c+MMF+GFR_30+Number_of_treatments,data = banco,family = binomial(link = "logit") )
or = exp(coefficients(model))[2]
ci = exp(confint(model))
c1 = ci[2,][1]
c2 = ci[2,][2]
s = paste0(sprintf(or, fmt = '%#.2f'),"(",sprintf(c1, fmt = '%#.2f'),"-",sprintf(c2, fmt = '%#.2f'),")")
summary(model)

#GFR<30
model = glm(D2_detect~GFR_30+sex+age_c+MMF+Number_of_treatments,data = banco,family = binomial(link = "logit") )
or = exp(coefficients(model))[2]
ci = exp(confint(model))
c1 = ci[2,][1]
c2 = ci[2,][2]
s = paste0(sprintf(or, fmt = '%#.2f'),"(",sprintf(c1, fmt = '%#.2f'),"-",sprintf(c2, fmt = '%#.2f'),")")
summary(model)

#Cancer
model = glm(D2_detect~Cancer+sex+age_c+MMF+GFR_30+Number_of_treatments,data = banco,family = binomial(link = "logit") )
or = exp(coefficients(model))[2]
ci = exp(confint(model))
c1 = ci[2,][1]
c2 = ci[2,][2]
s = paste0(sprintf(or, fmt = '%#.2f'),"(",sprintf(c1, fmt = '%#.2f'),"-",sprintf(c2, fmt = '%#.2f'),")")
summary(model)

#FK
model = glm(D2_detect~FK+sex+age_c+MMF+GFR_30,data = banco,family = binomial(link = "logit") )
or = exp(coefficients(model))[2]
ci = exp(confint(model))
c1 = ci[2,][1]
c2 = ci[2,][2]
s = paste0(sprintf(or, fmt = '%#.2f'),"(",sprintf(c1, fmt = '%#.2f'),"-",sprintf(c2, fmt = '%#.2f'),")")
summary(model)


#CTC
model = glm(D2_detect~CTC+sex+age_c+MMF+GFR_30,data = banco,family = binomial(link = "logit") )
or = exp(coefficients(model))[2]
ci = exp(confint(model))
c1 = ci[2,][1]
c2 = ci[2,][2]
s = paste0(sprintf(or, fmt = '%#.2f'),"(",sprintf(c1, fmt = '%#.2f'),"-",sprintf(c2, fmt = '%#.2f'),")")
summary(model)

#AZA
model = glm(D2_detect~AZA+sex+age_c+MMF+GFR_30,data = banco,family = binomial(link = "logit") )
or = exp(coefficients(model))[2]
ci = exp(confint(model))
c1 = ci[2,][1]
c2 = ci[2,][2]
s = paste0(sprintf(or, fmt = '%#.2f'),"(",sprintf(c1, fmt = '%#.2f'),"-",sprintf(c2, fmt = '%#.2f'),")")
summary(model)


#MMF
model = glm(D2_detect~MMF+sex+age_c+GFR_30,data = banco,family = binomial(link = "logit") )
or = exp(coefficients(model))[2]
ci = exp(confint(model))
c1 = ci[2,][1]
c2 = ci[2,][2]
s = paste0(sprintf(or, fmt = '%#.2f'),"(",sprintf(c1, fmt = '%#.2f'),"-",sprintf(c2, fmt = '%#.2f'),")")
summary(model)

#Evero
model = glm(D2_detect~Evero+sex+age_c+MMF+GFR_30,data = banco,family = binomial(link = "logit") )
or = exp(coefficients(model))[2]
ci = exp(confint(model))
c1 = ci[2,][1]
c2 = ci[2,][2]
s = paste0(sprintf(or, fmt = '%#.2f'),"(",sprintf(c1, fmt = '%#.2f'),"-",sprintf(c2, fmt = '%#.2f'),")")
summary(model)

#CsA
model = glm(D2_detect~CsA+sex+age_c+MMF+GFR_30,data = banco,family = binomial(link = "logit") )
or = exp(coefficients(model))[2]
ci = exp(confint(model))
c1 = ci[2,][1]
c2 = ci[2,][2]
s = paste0(sprintf(or, fmt = '%#.2f'),"(",sprintf(c1, fmt = '%#.2f'),"-",sprintf(c2, fmt = '%#.2f'),")")
summary(model)

#Induction
model = glm(D2_detect~Induction_treatment+sex+age_c+MMF+GFR_30,data = banco,family = binomial(link = "logit") )
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

#Number of treatments
model = glm(D2_detect~Number_of_treatments+sex+age_c+GFR_30,data = banco,family = binomial(link = "logit") )
or = exp(coefficients(model))[2]
ci = exp(confint(model))
c1 = ci[2,][1]
c2 = ci[2,][2]
s = paste0(sprintf(or, fmt = '%#.2f'),"(",sprintf(c1, fmt = '%#.2f'),"-",sprintf(c2, fmt = '%#.2f'),")")
summary(model)