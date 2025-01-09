banco = read.csv("./data/total.csv")
df = readxl::read_xlsx("./data/20230413_clinical_data.xlsx")
colnames(df)[2] = "Sample"
df = df[,c("Sample","Transplant rank","Number of treatments","Induction treatment","Ethnicity")]
df$n_treatments = NA
df$n_treatments[df$`Number of treatments`<3] = 0
df$n_treatments[!df$`Number of treatments`<3] = 1
df$`Number of treatments` = NULL
df$Induction_treatment = NA
df$Induction_treatment[df$`Induction treatment`==1] = 0
df$Induction_treatment[df$`Induction treatment`==2] = 1
df$Induction_treatment[df$`Induction treatment`==3] = 2
df$Induction_treatment = as.factor(df$Induction_treatment)
df$`Induction treatment` = NULL
df$tx_rank = NA
df$tx_rank[df$`Transplant rank`=="1"]=0
df$tx_rank[df$`Transplant rank`=="2"]=1
df$tx_rank[df$`Transplant rank`=="3"]=1
df$`Transplant rank` = NULL


banco = merge.data.frame(banco,df,by="Sample",all = TRUE)

banco$d3_p = NA
banco$d3_p[banco$D3_response=="Negative"] = 0
banco$d3_p[banco$D3_response=="Detectable"] = 0
banco$d3_p[banco$D3_response=="Positive"] = 1

banco$d3_depos = NA
banco$d3_depos[banco$D3_response=="Negative"] = NA
banco$d3_depos[banco$D3_response=="Detectable"] = 0
banco$d3_depos[banco$D3_response=="Positive"] = 1

banco = banco[banco$type!="i",]
banco = banco[banco$type!="control",]

banco$sex[banco$sex=="f"]=0
banco$sex[banco$sex=="m"]=1
banco$sex = as.factor(banco$sex)
age = banco$age
age[banco$age<50] = 0
age[banco$age>=50 & banco$age<=70] = 1
age[banco$age>70] = 2
age = as.factor(age)
banco$age = age
BMI = banco$BMI
BMI[banco$BMI<25] = 0
BMI[banco$BMI>=25] = 1
BMI = as.factor(BMI)
banco$BMI = BMI
local = banco$Ethnicity
local[local==0] = 0
local[local==4] = 1
local[local==3] = 1
local[local==2] = NA
banco$Ethnie = local
banco$Ethnicity = NULL
banco$type[banco$type=="k"] = 0
banco$type[banco$type=="l"] = 1
banco$type = as.factor(banco$type)
tx_time = banco$tx_time
tx_time[banco$tx_time>10] = 0
tx_time[banco$tx_time<=10 & banco$tx_time>=3] = 1
tx_time[banco$tx_time<3] = 2
banco$tx_time = as.factor(tx_time)
colnames(banco)[11] = "GFR_30"
colnames(banco)[8] = "Diabetes"

#######################################################################
# model selection
# d3_petect
#var = c("n_treatments","Induction_treatment","Cancer","GFR_30","CV","HTA","Diabetes","tx_rank","Ethnie","BMI","tx_time","type","FK","CTC","AZA","MMF","Evero","CsA")
var = c("n_treatments","Induction_treatment","Cancer","CV","HTA","Diabetes","tx_rank","Ethnie","BMI","tx_time","type","FK","CTC","AZA","MMF","Evero","CsA")
var = c("n_treatments","Induction_treatment","Cancer","CV","HTA","Diabetes","tx_rank","Ethnie","BMI","tx_time","FK","CTC","AZA","MMF","Evero","CsA")
acepted = c("GFR_30")

#forward
ac = ""
for(v in acepted){
  ac = paste0(ac,"+",v)
}
aic = rep(NA,length(var))
for(i in 1:length(var)){
  s = paste0("d3_depos~",var[i],"+sex+age", ac)
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
  s = paste0("d3_depos~","sex+age", ac)
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
#sex+age+GFR_30+MMF+type+n_treatments+CsA
var ="+sex+age+GFR_30"
# regression
# sex
v="sex"
model = glm(paste0("d3_depos~",v,var),data = banco,family = binomial(link = "logit") )
or = exp(coefficients(model))[2]
ci = exp(confint(model))
c1 = ci[2,][1]
c2 = ci[2,][2]
s = paste0(sprintf(or, fmt = '%#.2f'),"(",sprintf(c1, fmt = '%#.2f'),"-",sprintf(c2, fmt = '%#.2f'),")")
summary(model)

# age
v="age"
model = glm(paste0("d3_depos~",v,var),data = banco,family = binomial(link = "logit") )
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
model = glm(paste0("d3_depos~",v,var),data = banco,family = binomial(link = "logit") )
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
model = glm(paste0("d3_depos~",v,var),data = banco,family = binomial(link = "logit") )
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
model = glm(paste0("d3_depos~",v,var),data = banco,family = binomial(link = "logit") )
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
model = glm(paste0("d3_depos~",v,var),data = banco,family = binomial(link = "logit") )
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
model = glm(paste0("d3_depos~",v,var),data = banco,family = binomial(link = "logit") )
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
model = glm(paste0("d3_depos~",v,var),data = banco,family = binomial(link = "logit") )
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
model = glm(paste0("d3_depos~",v,var),data = banco,family = binomial(link = "logit") )
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
model = glm(paste0("d3_depos~",v,var),data = banco,family = binomial(link = "logit") )
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
model = glm(paste0("d3_depos~",v,var),data = banco,family = binomial(link = "logit") )
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
model = glm(paste0("d3_depos~",v,var),data = banco,family = binomial(link = "logit") )
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


# FK
v="FK"
model = glm(paste0("d3_depos~",v,var),data = banco,family = binomial(link = "logit") )
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
model = glm(paste0("d3_depos~",v,var),data = banco,family = binomial(link = "logit") )
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
model = glm(paste0("d3_depos~",v,var),data = banco,family = binomial(link = "logit") )
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
model = glm(paste0("d3_depos~",v,var),data = banco,family = binomial(link = "logit") )
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
model = glm(paste0("d3_depos~",v,var),data = banco,family = binomial(link = "logit") )
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

# CsA
v="CsA"
model = glm(paste0("d3_depos~",v,var),data = banco,family = binomial(link = "logit") )
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
model = glm(paste0("d3_depos~",v,var),data = banco,family = binomial(link = "logit") )
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

var ="+sex+age+GFR_30+type"

# n_treatments
v="n_treatments"
model = glm(paste0("d3_depos~",v,var),data = banco,family = binomial(link = "logit") )
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
