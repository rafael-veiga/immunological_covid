banco = read.csv("./data_aux/table2.csv")
## table 1

# type
banco$type = as.factor(banco$type)
tab = table(banco$type,banco$D3_detect)
t = tab[,1]+tab[,2]
p = 100*tab[,2]/t
s = rep(NA,length(t))
for(i in 1:length(tab[,2])){
  s[i] = paste0(tab[i,2],"(",sprintf(p[i], fmt = '%#.1f'),")")
}

# batch
banco$batch = as.factor(banco$batch)
tab = table(banco$batch,banco$D3_detect)
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
tab = table(banco$age_c,banco$D3_detect)
t = tab[,1]+tab[,2]
p = 100*tab[,2]/t
s = rep(NA,length(t))
for(i in 1:length(tab[,2])){
  s[i] = paste0(tab[i,2],"(",sprintf(p[i], fmt = '%#.1f'),")")
}

# sex
banco$sex = as.factor(banco$sex)
tab = table(banco$sex,banco$D3_detect)
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
tab = table(banco$BMI_c,banco$D3_detect)
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


#tx_time
banco$tx_time_c = rep(NA,length(banco$tx_time))
banco$tx_time_c[banco$tx_time<=3] = 2
banco$tx_time_c[(banco$tx_time>3)& (banco$tx_time<=10)] = 1
banco$tx_time_c[banco$tx_time>10] = 0

banco$tx_time_c = as.factor(banco$tx_time_c)
tab = table(banco$tx_time_c,banco$D3_detect)
t = tab[,1]+tab[,2]
p = 100*tab[,2]/t
s = rep(NA,length(t))
for(i in 1:length(tab[,2])){
  s[i] = paste0(tab[i,2],"(",sprintf(p[i], fmt = '%#.1f'),")")
}


#Ethnie
banco$Ethnie[banco$Ethnie==""] = NA
banco$Ethnie[banco$Ethnie=="Asiatique"] = NA # only 1 case
banco$Ethnie[banco$Ethnie=="afro-Subsaharien"] = "afro"
banco$Ethnie[banco$Ethnie=="Afro NA"] = "afro"
banco$Ethnie[banco$Ethnie=="Latino"] = NA
banco$Ethnie = as.factor(banco$Ethnie)
tab = table(banco$Ethnie,banco$D3_detect)
t = tab[,1]+tab[,2]
p = 100*tab[,2]/t
s = rep(NA,length(t))
for(i in 1:length(tab[,2])){
  s[i] = paste0(tab[i,2],"(",sprintf(p[i], fmt = '%#.1f'),")")
}

#diabetes
banco$diabete = as.factor(banco$diabete)
tab = table(banco$diabete,banco$D3_detect)
t = tab[,1]+tab[,2]
p = 100*tab[,2]/t
s = rep(NA,length(t))
for(i in 1:length(tab[,2])){
  s[i] = paste0(tab[i,2],"(",sprintf(p[i], fmt = '%#.1f'),")")
}

#HTA
banco$HTA = as.factor(banco$HTA)
tab = table(banco$HTA,banco$D3_detect)
t = tab[,1]+tab[,2]
p = 100*tab[,2]/t
s = rep(NA,length(t))
for(i in 1:length(tab[,2])){
  s[i] = paste0(tab[i,2],"(",sprintf(p[i], fmt = '%#.1f'),")")
}

#CV
banco$CV = as.factor(banco$CV)
tab = table(banco$CV,banco$D3_detect)
t = tab[,1]+tab[,2]
p = 100*tab[,2]/t
s = rep(NA,length(t))
for(i in 1:length(tab[,2])){
  s[i] = paste0(tab[i,2],"(",sprintf(p[i], fmt = '%#.1f'),")")
}

#IRC30
banco$IRC30 = as.factor(banco$IRC30)
tab = table(banco$IRC30,banco$D3_detect)
t = tab[,1]+tab[,2]
p = 100*tab[,2]/t
s = rep(NA,length(t))
for(i in 1:length(tab[,2])){
  s[i] = paste0(tab[i,2],"(",sprintf(p[i], fmt = '%#.1f'),")")
}

#Iresp
banco$Iresp = as.factor(banco$Iresp)
tab = table(banco$Iresp,banco$D3_detect)
t = tab[,1]+tab[,2]
p = 100*tab[,2]/t
s = rep(NA,length(t))
for(i in 1:length(tab[,2])){
  s[i] = paste0(tab[i,2],"(",sprintf(p[i], fmt = '%#.1f'),")")
}

#Cancer
banco$Cancer = as.factor(banco$Cancer)
tab = table(banco$Cancer,banco$D3_detect)
t = tab[,1]+tab[,2]
p = 100*tab[,2]/t
s = rep(NA,length(t))
for(i in 1:length(tab[,2])){
  s[i] = paste0(tab[i,2],"(",sprintf(p[i], fmt = '%#.1f'),")")
}

#other_cormobity
banco$other_cormobity = as.factor(banco$other_cormobity)
tab = table(banco$other_cormobity,banco$D3_detect)
t = tab[,1]+tab[,2]
p = 100*tab[,2]/t
s = rep(NA,length(t))
for(i in 1:length(tab[,2])){
  s[i] = paste0(tab[i,2],"(",sprintf(p[i], fmt = '%#.1f'),")")
}

#FK
banco$FK = as.factor(banco$FK)
tab = table(banco$FK,banco$D3_detect)
t = tab[,1]+tab[,2]
p = 100*tab[,2]/t
s = rep(NA,length(t))
for(i in 1:length(tab[,2])){
  s[i] = paste0(tab[i,2],"(",sprintf(p[i], fmt = '%#.1f'),")")
}

#CTC
banco$CTC = as.factor(banco$CTC)
tab = table(banco$CTC,banco$D3_detect)
t = tab[,1]+tab[,2]
p = 100*tab[,2]/t
s = rep(NA,length(t))
for(i in 1:length(tab[,2])){
  s[i] = paste0(tab[i,2],"(",sprintf(p[i], fmt = '%#.1f'),")")
}

#AZA
banco$AZA = as.factor(banco$AZA)
tab = table(banco$AZA,banco$D3_detect)
t = tab[,1]+tab[,2]
p = 100*tab[,2]/t
s = rep(NA,length(t))
for(i in 1:length(tab[,2])){
  s[i] = paste0(tab[i,2],"(",sprintf(p[i], fmt = '%#.1f'),")")
}

#MMF
banco$MMF = as.factor(banco$MMF)
tab = table(banco$MMF,banco$D3_detect)
t = tab[,1]+tab[,2]
p = 100*tab[,2]/t
s = rep(NA,length(t))
for(i in 1:length(tab[,2])){
  s[i] = paste0(tab[i,2],"(",sprintf(p[i], fmt = '%#.1f'),")")
}

#Evero
banco$Evero = as.factor(banco$Evero)
tab = table(banco$Evero,banco$D3_detect)
t = tab[,1]+tab[,2]
p = 100*tab[,2]/t
s = rep(NA,length(t))
for(i in 1:length(tab[,2])){
  s[i] = paste0(tab[i,2],"(",sprintf(p[i], fmt = '%#.1f'),")")
}

#CsA
banco$CsA = as.factor(banco$CsA)
tab = table(banco$CsA,banco$D3_detect)
t = tab[,1]+tab[,2]
p = 100*tab[,2]/t
s = rep(NA,length(t))
for(i in 1:length(tab[,2])){
  s[i] = paste0(tab[i,2],"(",sprintf(p[i], fmt = '%#.1f'),")")
}


# model selection
# Positive for d2 or d3
#var = c("tx_time","type","FK","CTC","AZA","MMF","Evero","CsA")
var = c("type","FK","CTC","AZA","Evero")
for(v in var){
  s = paste0("D3_detect~",v,"+sex+age_c+tx_time+MMF+CsA")
  model = glm(s,data = banco,family = binomial(link = "logit") )
  tab = summary(model)
  print(tab$coefficients[2,4])
}

#best model = "sex+age_c+tx_time+MMF+CsA"

# regression
# batch
model = glm(D3_detect~batch+sex+age_c+tx_time+MMF+CsA,data = banco,family = binomial(link = "logit") )
or = exp(coefficients(model))[2]
ci = exp(confint(model))
c1 = ci[2,][1]
c2 = ci[2,][2]
s = paste0(sprintf(or, fmt = '%#.2f'),"(",sprintf(c1, fmt = '%#.2f'),"-",sprintf(c2, fmt = '%#.2f'),")")
summary(model)

# type
model = glm(D3_detect~type+sex+age_c+tx_time+MMF+CsA,data = banco,family = binomial(link = "logit") )
or = exp(coefficients(model))[2]
ci = exp(confint(model))
c1 = ci[2,][1]
c2 = ci[2,][2]
s = paste0(sprintf(or, fmt = '%#.2f'),"(",sprintf(c1, fmt = '%#.2f'),"-",sprintf(c2, fmt = '%#.2f'),")")
summary(model)

# age_c
model = glm(D3_detect~age_c+sex+tx_time+MMF+CsA,data = banco,family = binomial(link = "logit") )
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

# sex
model = glm(D3_detect~sex+age_c+tx_time+MMF+CsA,data = banco,family = binomial(link = "logit") )
or = exp(coefficients(model))[2]
ci = exp(confint(model))
c1 = ci[2,][1]
c2 = ci[2,][2]
s = paste0(sprintf(or, fmt = '%#.2f'),"(",sprintf(c1, fmt = '%#.2f'),"-",sprintf(c2, fmt = '%#.2f'),")")
summary(model)

#tx_time
model = glm(D3_detect~tx_time_c+sex+age_c+MMF+CsA,data = banco,family = binomial(link = "logit") )
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


#FK
model = glm(D3_detect~FK+sex+age_c+tx_time+MMF+CsA,data = banco,family = binomial(link = "logit") )
or = exp(coefficients(model))[2]
ci = exp(confint(model))
c1 = ci[2,][1]
c2 = ci[2,][2]
s = paste0(sprintf(or, fmt = '%#.2f'),"(",sprintf(c1, fmt = '%#.2f'),"-",sprintf(c2, fmt = '%#.2f'),")")
summary(model)


#CTC
model = glm(D3_detect~CTC+sex+age_c+tx_time+MMF+CsA,data = banco,family = binomial(link = "logit") )
or = exp(coefficients(model))[2]
ci = exp(confint(model))
c1 = ci[2,][1]
c2 = ci[2,][2]
s = paste0(sprintf(or, fmt = '%#.2f'),"(",sprintf(c1, fmt = '%#.2f'),"-",sprintf(c2, fmt = '%#.2f'),")")
summary(model)

#AZA
model = glm(D3_detect~AZA+sex+age_c+tx_time+MMF+CsA,data = banco,family = binomial(link = "logit") )
or = exp(coefficients(model))[2]
ci = exp(confint(model))
c1 = ci[2,][1]
c2 = ci[2,][2]
s = paste0(sprintf(or, fmt = '%#.2f'),"(",sprintf(c1, fmt = '%#.2f'),"-",sprintf(c2, fmt = '%#.2f'),")")
summary(model)


#MMF
model = glm(D3_detect~MMF+sex+age_c+tx_time+CsA,data = banco,family = binomial(link = "logit") )
or = exp(coefficients(model))[2]
ci = exp(confint(model))
c1 = ci[2,][1]
c2 = ci[2,][2]
s = paste0(sprintf(or, fmt = '%#.2f'),"(",sprintf(c1, fmt = '%#.2f'),"-",sprintf(c2, fmt = '%#.2f'),")")
summary(model)

#Evero
model = glm(D3_detect~Evero+sex+age_c+tx_time+MMF+CsA,data = banco,family = binomial(link = "logit") )
or = exp(coefficients(model))[2]
ci = exp(confint(model))
c1 = ci[2,][1]
c2 = ci[2,][2]
s = paste0(sprintf(or, fmt = '%#.2f'),"(",sprintf(c1, fmt = '%#.2f'),"-",sprintf(c2, fmt = '%#.2f'),")")
summary(model)

#CsA
model = glm(D3_detect~CsA+sex+age_c+tx_time+MMF,data = banco,family = binomial(link = "logit") )
or = exp(coefficients(model))[2]
ci = exp(confint(model))
c1 = ci[2,][1]
c2 = ci[2,][2]
s = paste0(sprintf(or, fmt = '%#.2f'),"(",sprintf(c1, fmt = '%#.2f'),"-",sprintf(c2, fmt = '%#.2f'),")")
summary(model)


#best model = "sex+age_c+tx_time_c+MMF"


# BMI_c
model = glm(D3_detect~BMI_c+sex+age_c+tx_time+MMF+CsA,data = banco,family = binomial(link = "logit") )
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

#Ethnie
model = glm(D3_detect~Ethnie+sex+age_c+tx_time+MMF+CsA,data = banco,family = binomial(link = "logit") )
or = exp(coefficients(model))[2]
ci = exp(confint(model))
c1 = ci[2,][1]
c2 = ci[2,][2]
s = paste0(sprintf(or, fmt = '%#.2f'),"(",sprintf(c1, fmt = '%#.2f'),"-",sprintf(c2, fmt = '%#.2f'),")")
summary(model)

#diabete
model = glm(D3_detect~diabete+sex+age_c+tx_time+MMF+CsA,data = banco,family = binomial(link = "logit") )
or = exp(coefficients(model))[2]
ci = exp(confint(model))
c1 = ci[2,][1]
c2 = ci[2,][2]
s = paste0(sprintf(or, fmt = '%#.2f'),"(",sprintf(c1, fmt = '%#.2f'),"-",sprintf(c2, fmt = '%#.2f'),")")
summary(model)

#HTA
model = glm(D3_detect~HTA+sex+age_c+tx_time+MMF+CsA,data = banco,family = binomial(link = "logit") )
or = exp(coefficients(model))[2]
ci = exp(confint(model))
c1 = ci[2,][1]
c2 = ci[2,][2]
s = paste0(sprintf(or, fmt = '%#.2f'),"(",sprintf(c1, fmt = '%#.2f'),"-",sprintf(c2, fmt = '%#.2f'),")")
summary(model)

#CV
model = glm(D3_detect~CV+sex+age_c+tx_time+MMF+CsA,data = banco,family = binomial(link = "logit") )
or = exp(coefficients(model))[2]
ci = exp(confint(model))
c1 = ci[2,][1]
c2 = ci[2,][2]
s = paste0(sprintf(or, fmt = '%#.2f'),"(",sprintf(c1, fmt = '%#.2f'),"-",sprintf(c2, fmt = '%#.2f'),")")
summary(model)

#IRC30
model = glm(D3_detect~IRC30+sex+age_c+tx_time+MMF+CsA,data = banco,family = binomial(link = "logit") )
or = exp(coefficients(model))[2]
ci = exp(confint(model))
c1 = ci[2,][1]
c2 = ci[2,][2]
s = paste0(sprintf(or, fmt = '%#.2f'),"(",sprintf(c1, fmt = '%#.2f'),"-",sprintf(c2, fmt = '%#.2f'),")")
summary(model)


#Iresp
model = glm(D3_detect~Iresp+sex+age_c+tx_time+MMF+CsA,data = banco,family = binomial(link = "logit") )
or = exp(coefficients(model))[2]
ci = exp(confint(model))
c1 = ci[2,][1]
c2 = ci[2,][2]
s = paste0(sprintf(or, fmt = '%#.2f'),"(",sprintf(c1, fmt = '%#.2f'),"-",sprintf(c2, fmt = '%#.2f'),")")
summary(model)

#Cancer
model = glm(D3_detect~Cancer+sex+age_c+tx_time+MMF+CsA,data = banco,family = binomial(link = "logit") )
or = exp(coefficients(model))[2]
ci = exp(confint(model))
c1 = ci[2,][1]
c2 = ci[2,][2]
s = paste0(sprintf(or, fmt = '%#.2f'),"(",sprintf(c1, fmt = '%#.2f'),"-",sprintf(c2, fmt = '%#.2f'),")")
summary(model)

#other_cormobity
model = glm(D3_detect~other_cormobity+sex+age_c+tx_time+MMF+CsA,data = banco,family = binomial(link = "logit") )
or = exp(coefficients(model))[2]
ci = exp(confint(model))
c1 = ci[2,][1]
c2 = ci[2,][2]
s = paste0(sprintf(or, fmt = '%#.2f'),"(",sprintf(c1, fmt = '%#.2f'),"-",sprintf(c2, fmt = '%#.2f'),")")
summary(model)

