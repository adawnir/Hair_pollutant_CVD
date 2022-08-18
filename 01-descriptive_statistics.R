## Descriptive statistics: covariates and outcomes

rm(list=ls())
project_path=dirname(rstudioapi::getActiveDocumentContext()$path)
setwd(project_path)

source("functions.R")
library(tidyverse)
library(colorspace)

## Overview of population characteristics ----
filepaths = c("PesticidesPCBBDEs","PAHs","Combined")

for (i in 1:3){
  data = readRDS(paste0("../Data/",filepaths[i],".rds"))
  covars = data[,c("Country","Age","Gender","Smoking", "Energy",
                   "Proteins", "Fats", "Carbohydrates", "Fibers")]
  mytable=NULL
  # Age
  mymean=formatC(mean(covars$Age, na.rm=TRUE), format="f", digits=1)
  mysd=formatC(sd(covars$Age, na.rm=TRUE), format="f", digits=1)
  out=paste0(mymean, " (", mysd, ")")
  for (k in levels(covars$Country)){
    mymean=formatC(mean(covars$Age[covars$Country==k], na.rm=TRUE), format="f", digits=1)
    mysd=formatC(sd(covars$Age[covars$Country==k], na.rm=TRUE), format="f", digits=1)
    out=c(out, paste0(mymean, " (", mysd, ")"))
  }
  mytable=rbind(mytable, out)
  rownames(mytable)[nrow(mytable)]="Age (years), Mean (SD)"
  myn = sum(is.na(covars$Age))
  if(myn > 0){
    myp = formatC(myn/length(covars$Age)*100, format = "f", digits = 1)
    out = paste0(myn, " (", myp, "%)")
    for (k in levels(covars$Country)){
      myn = sum(is.na(covars$Age[covars$Country==k]))
      myp = formatC(myn/sum(covars$Country==k)*100, format = "f", digits = 1)
      out=c(out, paste0(myn, " (", myp, "%)"))
    }
    mytable=rbind(mytable, out)
    rownames(mytable)[nrow(mytable)]="Missing, N (%)"
  }
  # Gender
  mytable=rbind(mytable, rep(NA, ncol(mytable)))
  rownames(mytable)[nrow(mytable)]="Gender, N (%)"
  myn = sum(covars$Gender=="Male", na.rm = T)
  myp = formatC(myn/length(covars$Gender)*100, format = "f", digits = 1)
  out = paste0(myn, " (", myp, "%)")
  for (k in levels(covars$Country)){
    myn = sum(covars$Gender[covars$Country==k]=="Male", na.rm = T)
    myp = formatC(myn/sum(covars$Country==k)*100, format = "f", digits = 1)
    out=c(out, paste0(myn, " (", myp, "%)"))
  }
  mytable=rbind(mytable, out)
  rownames(mytable)[nrow(mytable)]="Male"
  myn = sum(covars$Gender=="Female", na.rm = T)
  myp = formatC(myn/length(covars$Gender)*100, format = "f", digits = 1)
  out = paste0(myn, " (", myp, "%)")
  for (k in levels(covars$Country)){
    myn = sum(covars$Gender[covars$Country==k]=="Female", na.rm = T)
    myp = formatC(myn/sum(covars$Country==k)*100, format = "f", digits = 1)
    out=c(out, paste0(myn, " (", myp, "%)"))
  }
  mytable=rbind(mytable, out)
  rownames(mytable)[nrow(mytable)]="Female"
  myn = sum(is.na(covars$Gender))
  if(myn > 0){
    myp = formatC(myn/length(covars$Gender)*100, format = "f", digits = 1)
    out = paste0(myn, " (", myp, "%)")
    for (k in levels(covars$Country)){
      myn = sum(is.na(covars$Gender[covars$Country==k]))
      myp = formatC(myn/sum(covars$Country==k)*100, format = "f", digits = 1)
      out=c(out, paste0(myn, " (", myp, "%)"))
    }
    mytable=rbind(mytable, out)
    rownames(mytable)[nrow(mytable)]="Missing, N (%)"
  }
  # Smoking
  mytable=rbind(mytable, rep(NA, ncol(mytable)))
  rownames(mytable)[nrow(mytable)]="Smoking status, N (%)"
  myn = sum(covars$Smoking=="Never smoker", na.rm = T)
  myp = formatC(myn/length(covars$Smoking)*100, format = "f", digits = 1)
  out = paste0(myn, " (", myp, "%)")
  for (k in levels(covars$Country)){
    myn = sum(covars$Smoking[covars$Country==k]=="Never smoker", na.rm = T)
    myp = formatC(myn/sum(covars$Country==k)*100, format = "f", digits = 1)
    out=c(out, paste0(myn, " (", myp, "%)"))
  }
  mytable=rbind(mytable, out)
  rownames(mytable)[nrow(mytable)]="Never smoker"
  myn = sum(covars$Smoking=="Former smoker", na.rm = T)
  myp = formatC(myn/length(covars$Smoking)*100, format = "f", digits = 1)
  out = paste0(myn, " (", myp, "%)")
  for (k in levels(covars$Country)){
    myn = sum(covars$Smoking[covars$Country==k]=="Former smoker", na.rm = T)
    myp = formatC(myn/sum(covars$Country==k)*100, format = "f", digits = 1)
    out=c(out, paste0(myn, " (", myp, "%)"))
  }
  mytable=rbind(mytable, out)
  rownames(mytable)[nrow(mytable)]="Current smoker"
  myn = sum(covars$Smoking=="Current smoker", na.rm = T)
  myp = formatC(myn/length(covars$Smoking)*100, format = "f", digits = 1)
  out = paste0(myn, " (", myp, "%)")
  for (k in levels(covars$Country)){
    myn = sum(covars$Smoking[covars$Country==k]=="Current smoker", na.rm = T)
    myp = formatC(myn/sum(covars$Country==k)*100, format = "f", digits = 1)
    out=c(out, paste0(myn, " (", myp, "%)"))
  }
  mytable=rbind(mytable, out)
  rownames(mytable)[nrow(mytable)]="Current smoker"
  myn = sum(is.na(covars$Smoking))
  if(myn > 0){
    myp = formatC(myn/length(covars$Smoking)*100, format = "f", digits = 1)
    out = paste0(myn, " (", myp, "%)")
    for (k in levels(covars$Country)){
      myn = sum(is.na(covars$Smoking[covars$Country==k]))
      myp = formatC(myn/sum(covars$Country==k)*100, format = "f", digits = 1)
      out=c(out, paste0(myn, " (", myp, "%)"))
    }
    mytable=rbind(mytable, out)
    rownames(mytable)[nrow(mytable)]="Missing, N (%)"
  }
  # Energy
  mymean=formatC(mean(covars$Energy, na.rm=TRUE), format="f", digits=1)
  mysd=formatC(sd(covars$Energy, na.rm=TRUE), format="f", digits=1)
  out=paste0(mymean, " (", mysd, ")")
  for (k in levels(covars$Country)){
    mymean=formatC(mean(covars$Energy[covars$Country==k], na.rm=TRUE), format="f", digits=1)
    mysd=formatC(sd(covars$Energy[covars$Country==k], na.rm=TRUE), format="f", digits=1)
    out=c(out, paste0(mymean, " (", mysd, ")"))
  }
  mytable=rbind(mytable, out)
  rownames(mytable)[nrow(mytable)]="Energy (kcal/day), Mean (SD)"
  myn = sum(is.na(covars$Energy))
  if(myn > 0){
    myp = formatC(myn/length(covars$Energy)*100, format = "f", digits = 1)
    out = paste0(myn, " (", myp, "%)")
    for (k in levels(covars$Country)){
      myn = sum(is.na(covars$Energy[covars$Country==k]))
      myp = formatC(myn/sum(covars$Country==k)*100, format = "f", digits = 1)
      out=c(out, paste0(myn, " (", myp, "%)"))
    }
    mytable=rbind(mytable, out)
    rownames(mytable)[nrow(mytable)]="Missing, N (%)"
  }
  # Proteins
  mymean=formatC(mean(covars$Proteins, na.rm=TRUE), format="f", digits=1)
  mysd=formatC(sd(covars$Proteins, na.rm=TRUE), format="f", digits=1)
  out=paste0(mymean, " (", mysd, ")")
  for (k in levels(covars$Country)){
    mymean=formatC(mean(covars$Proteins[covars$Country==k], na.rm=TRUE), format="f", digits=1)
    mysd=formatC(sd(covars$Proteins[covars$Country==k], na.rm=TRUE), format="f", digits=1)
    out=c(out, paste0(mymean, " (", mysd, ")"))
  }
  mytable=rbind(mytable, out)
  rownames(mytable)[nrow(mytable)]="Proteins (g/day), Mean (SD)"
  myn = sum(is.na(covars$Proteins))
  if(myn > 0){
    myp = formatC(myn/length(covars$Proteins)*100, format = "f", digits = 1)
    out = paste0(myn, " (", myp, "%)")
    for (k in levels(covars$Country)){
      myn = sum(is.na(covars$Proteins[covars$Country==k]))
      myp = formatC(myn/sum(covars$Country==k)*100, format = "f", digits = 1)
      out=c(out, paste0(myn, " (", myp, "%)"))
    }
    mytable=rbind(mytable, out)
    rownames(mytable)[nrow(mytable)]="Missing, N (%)"
  }
  # Fats
  mymean=formatC(mean(covars$Fats, na.rm=TRUE), format="f", digits=1)
  mysd=formatC(sd(covars$Fats, na.rm=TRUE), format="f", digits=1)
  out=paste0(mymean, " (", mysd, ")")
  for (k in levels(covars$Country)){
    mymean=formatC(mean(covars$Fats[covars$Country==k], na.rm=TRUE), format="f", digits=1)
    mysd=formatC(sd(covars$Fats[covars$Country==k], na.rm=TRUE), format="f", digits=1)
    out=c(out, paste0(mymean, " (", mysd, ")"))
  }
  mytable=rbind(mytable, out)
  rownames(mytable)[nrow(mytable)]="Fats (g/day), Mean (SD)"
  myn = sum(is.na(covars$Fats))
  if(myn > 0){
    myp = formatC(myn/length(covars$Fats)*100, format = "f", digits = 1)
    out = paste0(myn, " (", myp, "%)")
    for (k in levels(covars$Country)){
      myn = sum(is.na(covars$Fats[covars$Country==k]))
      myp = formatC(myn/sum(covars$Country==k)*100, format = "f", digits = 1)
      out=c(out, paste0(myn, " (", myp, "%)"))
    }
    mytable=rbind(mytable, out)
    rownames(mytable)[nrow(mytable)]="Missing, N (%)"
  }
  # Carbohydrates
  mymean=formatC(mean(covars$Carbohydrates, na.rm=TRUE), format="f", digits=1)
  mysd=formatC(sd(covars$Carbohydrates, na.rm=TRUE), format="f", digits=1)
  out=paste0(mymean, " (", mysd, ")")
  for (k in levels(covars$Country)){
    mymean=formatC(mean(covars$Carbohydrates[covars$Country==k], na.rm=TRUE), format="f", digits=1)
    mysd=formatC(sd(covars$Carbohydrates[covars$Country==k], na.rm=TRUE), format="f", digits=1)
    out=c(out, paste0(mymean, " (", mysd, ")"))
  }
  mytable=rbind(mytable, out)
  rownames(mytable)[nrow(mytable)]="Carbohydrates (g/day), Mean (SD)"
  myn = sum(is.na(covars$Carbohydrates))
  if(myn > 0){
    myp = formatC(myn/length(covars$Carbohydrates)*100, format = "f", digits = 1)
    out = paste0(myn, " (", myp, "%)")
    for (k in levels(covars$Country)){
      myn = sum(is.na(covars$Carbohydrates[covars$Country==k]))
      myp = formatC(myn/sum(covars$Country==k)*100, format = "f", digits = 1)
      out=c(out, paste0(myn, " (", myp, "%)"))
    }
    mytable=rbind(mytable, out)
    rownames(mytable)[nrow(mytable)]="Missing, N (%)"
  }
  # Fibers
  mymean=formatC(mean(covars$Fibers, na.rm=TRUE), format="f", digits=1)
  mysd=formatC(sd(covars$Fibers, na.rm=TRUE), format="f", digits=1)
  out=paste0(mymean, " (", mysd, ")")
  for (k in levels(covars$Country)){
    mymean=formatC(mean(covars$Fibers[covars$Country==k], na.rm=TRUE), format="f", digits=1)
    mysd=formatC(sd(covars$Fibers[covars$Country==k], na.rm=TRUE), format="f", digits=1)
    out=c(out, paste0(mymean, " (", mysd, ")"))
  }
  mytable=rbind(mytable, out)
  rownames(mytable)[nrow(mytable)]="Fibres (g/day), Mean (SD)"
  myn = sum(is.na(covars$Fibers))
  if(myn > 0){
    myp = formatC(myn/length(covars$Fibers)*100, format = "f", digits = 1)
    out = paste0(myn, " (", myp, "%)")
    for (k in levels(covars$Country)){
      myn = sum(is.na(covars$Fibers[covars$Country==k]))
      myp = formatC(myn/sum(covars$Country==k)*100, format = "f", digits = 1)
      out=c(out, paste0(myn, " (", myp, "%)"))
    }
    mytable=rbind(mytable, out)
    rownames(mytable)[nrow(mytable)]="Missing, N (%)"
  }
  out = nrow(covars)
  for (k in levels(covars$Country)){
    out=c(out, sum(covars$Country==k))
  }
  colnames(mytable)=paste0(c("Total",levels(covars$Country))," (N=",out,")")
  SaveExcelWithSuperscripts(cbind(rownames(mytable),mytable), paste0("../Exports/",filepaths[i],"_characteristics.xlsx"))
}


## Overview of population characteristics (related to outcome) ----
filepaths = c("PesticidesPCBBDEs","PAHs","Combined")

for (i in 1:3){
  data = readRDS(paste0("../Data/",filepaths[i],".rds"))
  covars = data[,c("Country",
                   "BMI_cat",
                   "SBP", "DBP",
                   "FPG",
                   "TC", "HDLC", "LDLC", "TG",
                   "WC_cat",
                   "Diabetes_medication","Hypertension_medication","Lipid_disorder_medication")]
  mytable=NULL
  # BMI
  mytable=rbind(mytable, rep(NA, length(levels(covars$Country))+1))
  rownames(mytable)[nrow(mytable)]="BMI, N (%)"
  myn = sum(covars$BMI_cat=="Underweight", na.rm = T)
  myp = formatC(myn/length(covars$BMI_cat)*100, format = "f", digits = 1)
  out = paste0(myn, " (", myp, "%)")
  for (k in levels(covars$Country)){
    myn = sum(covars$BMI_cat[covars$Country==k]=="Underweight", na.rm = T)
    myp = formatC(myn/sum(covars$Country==k)*100, format = "f", digits = 1)
    out=c(out, paste0(myn, " (", myp, "%)"))
  }
  mytable=rbind(mytable, out)
  rownames(mytable)[nrow(mytable)]="Underweight"
  myn = sum(covars$BMI_cat=="Normal", na.rm = T)
  myp = formatC(myn/length(covars$BMI_cat)*100, format = "f", digits = 1)
  out = paste0(myn, " (", myp, "%)")
  for (k in levels(covars$Country)){
    myn = sum(covars$BMI_cat[covars$Country==k]=="Normal", na.rm = T)
    myp = formatC(myn/sum(covars$Country==k)*100, format = "f", digits = 1)
    out=c(out, paste0(myn, " (", myp, "%)"))
  }
  mytable=rbind(mytable, out)
  rownames(mytable)[nrow(mytable)]="Normal"
  myn = sum(covars$BMI_cat=="Pre-obesity", na.rm = T)
  myp = formatC(myn/length(covars$BMI_cat)*100, format = "f", digits = 1)
  out = paste0(myn, " (", myp, "%)")
  for (k in levels(covars$Country)){
    myn = sum(covars$BMI_cat[covars$Country==k]=="Pre-obesity", na.rm = T)
    myp = formatC(myn/sum(covars$Country==k)*100, format = "f", digits = 1)
    out=c(out, paste0(myn, " (", myp, "%)"))
  }
  mytable=rbind(mytable, out)
  rownames(mytable)[nrow(mytable)]="Pre-obesity"
  myn = sum(covars$BMI_cat=="Obesity class I", na.rm = T)
  myp = formatC(myn/length(covars$BMI_cat)*100, format = "f", digits = 1)
  out = paste0(myn, " (", myp, "%)")
  for (k in levels(covars$Country)){
    myn = sum(covars$BMI_cat[covars$Country==k]=="Obesity class I", na.rm = T)
    myp = formatC(myn/sum(covars$Country==k)*100, format = "f", digits = 1)
    out=c(out, paste0(myn, " (", myp, "%)"))
  }
  mytable=rbind(mytable, out)
  rownames(mytable)[nrow(mytable)]="Obesity class I"
  myn = sum(covars$BMI_cat=="Obesity class II", na.rm = T)
  myp = formatC(myn/length(covars$BMI_cat)*100, format = "f", digits = 1)
  out = paste0(myn, " (", myp, "%)")
  for (k in levels(covars$Country)){
    myn = sum(covars$BMI_cat[covars$Country==k]=="Obesity class II", na.rm = T)
    myp = formatC(myn/sum(covars$Country==k)*100, format = "f", digits = 1)
    out=c(out, paste0(myn, " (", myp, "%)"))
  }
  mytable=rbind(mytable, out)
  rownames(mytable)[nrow(mytable)]="Obesity class II"
  myn = sum(covars$BMI_cat=="Obesity class III", na.rm = T)
  myp = formatC(myn/length(covars$BMI_cat)*100, format = "f", digits = 1)
  out = paste0(myn, " (", myp, "%)")
  for (k in levels(covars$Country)){
    myn = sum(covars$BMI_cat[covars$Country==k]=="Obesity class III", na.rm = T)
    myp = formatC(myn/sum(covars$Country==k)*100, format = "f", digits = 1)
    out=c(out, paste0(myn, " (", myp, "%)"))
  }
  mytable=rbind(mytable, out)
  rownames(mytable)[nrow(mytable)]="Obesity class III"
  myn = sum(is.na(covars$BMI_cat))
  if(myn > 0){
    myp = formatC(myn/length(covars$BMI_cat)*100, format = "f", digits = 1)
    out = paste0(myn, " (", myp, "%)")
    for (k in levels(covars$Country)){
      myn = sum(is.na(covars$BMI_cat[covars$Country==k]))
      myp = formatC(myn/sum(covars$Country==k)*100, format = "f", digits = 1)
      out=c(out, paste0(myn, " (", myp, "%)"))
    }
    mytable=rbind(mytable, out)
    rownames(mytable)[nrow(mytable)]="Missing, N (%)"
  }
  # Systolic blood pressure
  mymean=formatC(mean(covars$SBP, na.rm=TRUE), format="f", digits=1)
  mysd=formatC(sd(covars$SBP, na.rm=TRUE), format="f", digits=1)
  out=paste0(mymean, " (", mysd, ")")
  for (k in levels(covars$Country)){
    mymean=formatC(mean(covars$SBP[covars$Country==k], na.rm=TRUE), format="f", digits=1)
    mysd=formatC(sd(covars$SBP[covars$Country==k], na.rm=TRUE), format="f", digits=1)
    out=c(out, paste0(mymean, " (", mysd, ")"))
  }
  mytable=rbind(mytable, out)
  rownames(mytable)[nrow(mytable)]="Systolic blood pressure (mmHg), Mean (SD)"
  myn = sum(is.na(covars$SBP))
  if(myn > 0){
    myp = formatC(myn/length(covars$SBP)*100, format = "f", digits = 1)
    out = paste0(myn, " (", myp, "%)")
    for (k in levels(covars$Country)){
      myn = sum(is.na(covars$SBP[covars$Country==k]))
      myp = formatC(myn/sum(covars$Country==k)*100, format = "f", digits = 1)
      out=c(out, paste0(myn, " (", myp, "%)"))
    }
    mytable=rbind(mytable, out)
    rownames(mytable)[nrow(mytable)]="Missing, N (%)"
  }
  # Diastolic blood pressure
  mymean=formatC(mean(covars$DBP, na.rm=TRUE), format="f", digits=1)
  mysd=formatC(sd(covars$DBP, na.rm=TRUE), format="f", digits=1)
  out=paste0(mymean, " (", mysd, ")")
  for (k in levels(covars$Country)){
    mymean=formatC(mean(covars$DBP[covars$Country==k], na.rm=TRUE), format="f", digits=1)
    mysd=formatC(sd(covars$DBP[covars$Country==k], na.rm=TRUE), format="f", digits=1)
    out=c(out, paste0(mymean, " (", mysd, ")"))
  }
  mytable=rbind(mytable, out)
  rownames(mytable)[nrow(mytable)]="Diastolic blood pressure (mmHg), Mean (SD)"
  myn = sum(is.na(covars$DBP))
  if(myn > 0){
    myp = formatC(myn/length(covars$DBP)*100, format = "f", digits = 1)
    out = paste0(myn, " (", myp, "%)")
    for (k in levels(covars$Country)){
      myn = sum(is.na(covars$DBP[covars$Country==k]))
      myp = formatC(myn/sum(covars$Country==k)*100, format = "f", digits = 1)
      out=c(out, paste0(myn, " (", myp, "%)"))
    }
    mytable=rbind(mytable, out)
    rownames(mytable)[nrow(mytable)]="Missing, N (%)"
  }
  # Fasting blood glucose
  mymean=formatC(mean(covars$FPG, na.rm=TRUE), format="f", digits=1)
  mysd=formatC(sd(covars$FPG, na.rm=TRUE), format="f", digits=1)
  out=paste0(mymean, " (", mysd, ")")
  for (k in levels(covars$Country)){
    mymean=formatC(mean(covars$FPG[covars$Country==k], na.rm=TRUE), format="f", digits=1)
    mysd=formatC(sd(covars$FPG[covars$Country==k], na.rm=TRUE), format="f", digits=1)
    out=c(out, paste0(mymean, " (", mysd, ")"))
  }
  mytable=rbind(mytable, out)
  rownames(mytable)[nrow(mytable)]="Fasting blood glucose (mg/dl), Mean (SD)"
  myn = sum(is.na(covars$FPG))
  if(myn > 0){
    myp = formatC(myn/length(covars$FPG)*100, format = "f", digits = 1)
    out = paste0(myn, " (", myp, "%)")
    for (k in levels(covars$Country)){
      myn = sum(is.na(covars$FPG[covars$Country==k]))
      myp = formatC(myn/sum(covars$Country==k)*100, format = "f", digits = 1)
      out=c(out, paste0(myn, " (", myp, "%)"))
    }
    mytable=rbind(mytable, out)
    rownames(mytable)[nrow(mytable)]="Missing, N (%)"
  }
  # Total cholesterol
  mymean=formatC(mean(covars$TC, na.rm=TRUE), format="f", digits=1)
  mysd=formatC(sd(covars$TC, na.rm=TRUE), format="f", digits=1)
  out=paste0(mymean, " (", mysd, ")")
  for (k in levels(covars$Country)){
    mymean=formatC(mean(covars$TC[covars$Country==k], na.rm=TRUE), format="f", digits=1)
    mysd=formatC(sd(covars$TC[covars$Country==k], na.rm=TRUE), format="f", digits=1)
    out=c(out, paste0(mymean, " (", mysd, ")"))
  }
  mytable=rbind(mytable, out)
  rownames(mytable)[nrow(mytable)]="Total cholesterol (mg/dl), Mean (SD)"
  myn = sum(is.na(covars$TC))
  if(myn > 0){
    myp = formatC(myn/length(covars$TC)*100, format = "f", digits = 1)
    out = paste0(myn, " (", myp, "%)")
    for (k in levels(covars$Country)){
      myn = sum(is.na(covars$TC[covars$Country==k]))
      myp = formatC(myn/sum(covars$Country==k)*100, format = "f", digits = 1)
      out=c(out, paste0(myn, " (", myp, "%)"))
    }
    mytable=rbind(mytable, out)
    rownames(mytable)[nrow(mytable)]="Missing, N (%)"
  }
  # HDL cholesterol
  mymean=formatC(mean(covars$HDLC, na.rm=TRUE), format="f", digits=1)
  mysd=formatC(sd(covars$HDLC, na.rm=TRUE), format="f", digits=1)
  out=paste0(mymean, " (", mysd, ")")
  for (k in levels(covars$Country)){
    mymean=formatC(mean(covars$HDLC[covars$Country==k], na.rm=TRUE), format="f", digits=1)
    mysd=formatC(sd(covars$HDLC[covars$Country==k], na.rm=TRUE), format="f", digits=1)
    out=c(out, paste0(mymean, " (", mysd, ")"))
  }
  mytable=rbind(mytable, out)
  rownames(mytable)[nrow(mytable)]="HDL cholesterol (mg/dl), Mean (SD)"
  myn = sum(is.na(covars$HDLC))
  if(myn > 0){
    myp = formatC(myn/length(covars$HDLC)*100, format = "f", digits = 1)
    out = paste0(myn, " (", myp, "%)")
    for (k in levels(covars$Country)){
      myn = sum(is.na(covars$HDLC[covars$Country==k]))
      myp = formatC(myn/sum(covars$Country==k)*100, format = "f", digits = 1)
      out=c(out, paste0(myn, " (", myp, "%)"))
    }
    mytable=rbind(mytable, out)
    rownames(mytable)[nrow(mytable)]="Missing, N (%)"
  }
  # LDL cholesterol
  mymean=formatC(mean(covars$LDLC, na.rm=TRUE), format="f", digits=1)
  mysd=formatC(sd(covars$LDLC, na.rm=TRUE), format="f", digits=1)
  out=paste0(mymean, " (", mysd, ")")
  for (k in levels(covars$Country)){
    mymean=formatC(mean(covars$LDLC[covars$Country==k], na.rm=TRUE), format="f", digits=1)
    mysd=formatC(sd(covars$LDLC[covars$Country==k], na.rm=TRUE), format="f", digits=1)
    out=c(out, paste0(mymean, " (", mysd, ")"))
  }
  mytable=rbind(mytable, out)
  rownames(mytable)[nrow(mytable)]="LDL cholesterol (mg/dl), Mean (SD)"
  myn = sum(is.na(covars$LDLC))
  if(myn > 0){
    myp = formatC(myn/length(covars$LDLC)*100, format = "f", digits = 1)
    out = paste0(myn, " (", myp, "%)")
    for (k in levels(covars$Country)){
      myn = sum(is.na(covars$LDLC[covars$Country==k]))
      myp = formatC(myn/sum(covars$Country==k)*100, format = "f", digits = 1)
      out=c(out, paste0(myn, " (", myp, "%)"))
    }
    mytable=rbind(mytable, out)
    rownames(mytable)[nrow(mytable)]="Missing, N (%)"
  }
  # Waist circumference
  mytable=rbind(mytable, rep(NA, ncol(mytable)))
  rownames(mytable)[nrow(mytable)]="Waist circumference, N (%)"
  myn = sum(covars$WC_cat=="High", na.rm = T)
  myp = formatC(myn/length(covars$WC_cat)*100, format = "f", digits = 1)
  out = paste0(myn, " (", myp, "%)")
  for (k in levels(covars$Country)){
    myn = sum(covars$WC_cat[covars$Country==k]=="High", na.rm = T)
    myp = formatC(myn/sum(covars$Country==k)*100, format = "f", digits = 1)
    out=c(out, paste0(myn, " (", myp, "%)"))
  }
  mytable=rbind(mytable, out)
  rownames(mytable)[nrow(mytable)]="High"
  myn = sum(covars$WC_cat=="Normal", na.rm = T)
  myp = formatC(myn/length(covars$WC_cat)*100, format = "f", digits = 1)
  out = paste0(myn, " (", myp, "%)")
  for (k in levels(covars$Country)){
    myn = sum(covars$WC_cat[covars$Country==k]=="Normal", na.rm = T)
    myp = formatC(myn/sum(covars$Country==k)*100, format = "f", digits = 1)
    out=c(out, paste0(myn, " (", myp, "%)"))
  }
  mytable=rbind(mytable, out)
  rownames(mytable)[nrow(mytable)]="Normal"
  myn = sum(is.na(covars$WC_cat))
  if(myn > 0){
    myp = formatC(myn/length(covars$WC_cat)*100, format = "f", digits = 1)
    out = paste0(myn, " (", myp, "%)")
    for (k in levels(covars$Country)){
      myn = sum(is.na(covars$WC_cat[covars$Country==k]))
      myp = formatC(myn/sum(covars$Country==k)*100, format = "f", digits = 1)
      out=c(out, paste0(myn, " (", myp, "%)"))
    }
    mytable=rbind(mytable, out)
    rownames(mytable)[nrow(mytable)]="Missing, N (%)"
  }
  # Triglyceride
  mytable=rbind(mytable, rep(NA, ncol(mytable)))
  rownames(mytable)[nrow(mytable)]="Triglyceride, N (%)"
  myn = sum(covars$TG>=150, na.rm = T)
  myp = formatC(myn/length(covars$TG)*100, format = "f", digits = 1)
  out = paste0(myn, " (", myp, "%)")
  for (k in levels(covars$Country)){
    myn = sum(covars$TG[covars$Country==k]>=150, na.rm = T)
    myp = formatC(myn/sum(covars$Country==k)*100, format = "f", digits = 1)
    out=c(out, paste0(myn, " (", myp, "%)"))
  }
  mytable=rbind(mytable, out)
  rownames(mytable)[nrow(mytable)]=">=150mg/dl"
  myn = sum(covars$TG<150, na.rm = T)
  myp = formatC(myn/length(covars$TG)*100, format = "f", digits = 1)
  out = paste0(myn, " (", myp, "%)")
  for (k in levels(covars$Country)){
    myn = sum(covars$TG[covars$Country==k]<150, na.rm = T)
    myp = formatC(myn/sum(covars$Country==k)*100, format = "f", digits = 1)
    out=c(out, paste0(myn, " (", myp, "%)"))
  }
  mytable=rbind(mytable, out)
  rownames(mytable)[nrow(mytable)]="<150mg/dl"
  myn = sum(is.na(covars$TG))
  if(myn > 0){
    myp = formatC(myn/length(covars$TG)*100, format = "f", digits = 1)
    out = paste0(myn, " (", myp, "%)")
    for (k in levels(covars$Country)){
      myn = sum(is.na(covars$WC_cat[covars$Country==k]))
      myp = formatC(myn/sum(covars$Country==k)*100, format = "f", digits = 1)
      out=c(out, paste0(myn, " (", myp, "%)"))
    }
    mytable=rbind(mytable, out)
    rownames(mytable)[nrow(mytable)]="Missing, N (%)"
  }
  # Diabetes medication
  mytable=rbind(mytable, rep(NA, ncol(mytable)))
  rownames(mytable)[nrow(mytable)]="Diabetes medication, N (%)"
  myn = sum(covars$Diabetes_medication==1, na.rm = T)
  myp = formatC(myn/length(covars$Diabetes_medication)*100, format = "f", digits = 1)
  out = paste0(myn, " (", myp, "%)")
  for (k in levels(covars$Country)){
    myn = sum(covars$Diabetes_medication[covars$Country==k]==1, na.rm = T)
    myp = formatC(myn/sum(covars$Country==k)*100, format = "f", digits = 1)
    out=c(out, paste0(myn, " (", myp, "%)"))
  }
  mytable=rbind(mytable, out)
  rownames(mytable)[nrow(mytable)]="Yes"
  myn = sum(covars$Diabetes_medication==0, na.rm = T)
  myp = formatC(myn/length(covars$Diabetes_medication)*100, format = "f", digits = 1)
  out = paste0(myn, " (", myp, "%)")
  for (k in levels(covars$Country)){
    myn = sum(covars$Diabetes_medication[covars$Country==k]==0, na.rm = T)
    myp = formatC(myn/sum(covars$Country==k)*100, format = "f", digits = 1)
    out=c(out, paste0(myn, " (", myp, "%)"))
  }
  mytable=rbind(mytable, out)
  rownames(mytable)[nrow(mytable)]="No"
  myn = sum(is.na(covars$Diabetes_medication))
  if(myn > 0){
    myp = formatC(myn/length(covars$Diabetes_medication)*100, format = "f", digits = 1)
    out = paste0(myn, " (", myp, "%)")
    for (k in levels(covars$Country)){
      myn = sum(is.na(covars$WC_cat[covars$Country==k]))
      myp = formatC(myn/sum(covars$Country==k)*100, format = "f", digits = 1)
      out=c(out, paste0(myn, " (", myp, "%)"))
    }
    mytable=rbind(mytable, out)
    rownames(mytable)[nrow(mytable)]="Missing, N (%)"
  }
  # Hypertension medication
  mytable=rbind(mytable, rep(NA, ncol(mytable)))
  rownames(mytable)[nrow(mytable)]="Hypertension medication, N (%)"
  myn = sum(covars$Hypertension_medication==1, na.rm = T)
  myp = formatC(myn/length(covars$Hypertension_medication)*100, format = "f", digits = 1)
  out = paste0(myn, " (", myp, "%)")
  for (k in levels(covars$Country)){
    myn = sum(covars$Hypertension_medication[covars$Country==k]==1, na.rm = T)
    myp = formatC(myn/sum(covars$Country==k)*100, format = "f", digits = 1)
    out=c(out, paste0(myn, " (", myp, "%)"))
  }
  mytable=rbind(mytable, out)
  rownames(mytable)[nrow(mytable)]="Yes"
  myn = sum(covars$Hypertension_medication==0, na.rm = T)
  myp = formatC(myn/length(covars$Hypertension_medication)*100, format = "f", digits = 1)
  out = paste0(myn, " (", myp, "%)")
  for (k in levels(covars$Country)){
    myn = sum(covars$Hypertension_medication[covars$Country==k]==0, na.rm = T)
    myp = formatC(myn/sum(covars$Country==k)*100, format = "f", digits = 1)
    out=c(out, paste0(myn, " (", myp, "%)"))
  }
  mytable=rbind(mytable, out)
  rownames(mytable)[nrow(mytable)]="No"
  myn = sum(is.na(covars$Hypertension_medication))
  if(myn > 0){
    myp = formatC(myn/length(covars$Hypertension_medication)*100, format = "f", digits = 1)
    out = paste0(myn, " (", myp, "%)")
    for (k in levels(covars$Country)){
      myn = sum(is.na(covars$WC_cat[covars$Country==k]))
      myp = formatC(myn/sum(covars$Country==k)*100, format = "f", digits = 1)
      out=c(out, paste0(myn, " (", myp, "%)"))
    }
    mytable=rbind(mytable, out)
    rownames(mytable)[nrow(mytable)]="Missing, N (%)"
  }
  # Lipid disorder medication
  mytable=rbind(mytable, rep(NA, ncol(mytable)))
  rownames(mytable)[nrow(mytable)]="Lipid disorder medication, N (%)"
  myn = sum(covars$Lipid_disorder_medication==1, na.rm = T)
  myp = formatC(myn/length(covars$Lipid_disorder_medication)*100, format = "f", digits = 1)
  out = paste0(myn, " (", myp, "%)")
  for (k in levels(covars$Country)){
    myn = sum(covars$Lipid_disorder_medication[covars$Country==k]==1, na.rm = T)
    myp = formatC(myn/sum(covars$Country==k)*100, format = "f", digits = 1)
    out=c(out, paste0(myn, " (", myp, "%)"))
  }
  mytable=rbind(mytable, out)
  rownames(mytable)[nrow(mytable)]="Yes"
  myn = sum(covars$Lipid_disorder_medication==0, na.rm = T)
  myp = formatC(myn/length(covars$Lipid_disorder_medication)*100, format = "f", digits = 1)
  out = paste0(myn, " (", myp, "%)")
  for (k in levels(covars$Country)){
    myn = sum(covars$Lipid_disorder_medication[covars$Country==k]==0, na.rm = T)
    myp = formatC(myn/sum(covars$Country==k)*100, format = "f", digits = 1)
    out=c(out, paste0(myn, " (", myp, "%)"))
  }
  mytable=rbind(mytable, out)
  rownames(mytable)[nrow(mytable)]="No"
  myn = sum(is.na(covars$Lipid_disorder_medication))
  if(myn > 0){
    myp = formatC(myn/length(covars$Lipid_disorder_medication)*100, format = "f", digits = 1)
    out = paste0(myn, " (", myp, "%)")
    for (k in levels(covars$Country)){
      myn = sum(is.na(covars$WC_cat[covars$Country==k]))
      myp = formatC(myn/sum(covars$Country==k)*100, format = "f", digits = 1)
      out=c(out, paste0(myn, " (", myp, "%)"))
    }
    mytable=rbind(mytable, out)
    rownames(mytable)[nrow(mytable)]="Missing, N (%)"
  }
  out = nrow(covars)
  for (k in levels(covars$Country)){
    out=c(out, sum(covars$Country==k))
  }
  colnames(mytable)=paste0(c("Total",levels(covars$Country))," (N=",out,")")
  SaveExcelWithSuperscripts(cbind(rownames(mytable),mytable), paste0("../Exports/",filepaths[i],"_characteristics_outcome.xlsx"))
}

## Comparing outcome prevalence between populations (by country) ----
filepaths = c("PesticidesPCBBDEs","PAHs","Combined")

for (i in 1:3){
  data = readRDS(paste0("../Data/",filepaths[i],".rds"))
  outcomes = data[,c("Obesity","Hypertension","Diabetes","Lipid_disorder","Metabolic_syndrome")]
  mytable=NULL
  for (j in 1:ncol(outcomes)){
    vars = outcomes[,j]
    myn = sum(vars=="Yes", na.rm = T)
    myp = formatC(myn/length(vars)*100, format = "f", digits = 1)
    out = paste0(myn, " (", myp, "%)")
    for (k in levels(data$Country)){
      myn = sum(vars[data$Country==k]=="Yes", na.rm = T)
      myp = formatC(myn/sum(data$Country==k)*100, format = "f", digits = 1)
      out=c(out, paste0(myn, " (", myp, "%)"))
    }
    tmp=formatC(chisq.test(table(vars,data$Country))$p.value, format="e", digits=2)
    out = c(out,tmp)
    mytable=rbind(mytable, out)
    rownames(mytable)[nrow(mytable)]=colnames(outcomes)[j]
  }
  out = nrow(data)
  for (k in levels(data$Country)){
    out=c(out, sum(data$Country==k))
  }
  colnames(mytable)=c(paste0(c("Total",levels(data$Country))," (N=",out,")"),"P-value")
  SaveExcelWithSuperscripts(cbind(rownames(mytable),mytable), paste0("../Exports/",filepaths[i],"_outcome_by_country.xlsx"))
}

## Comparing outcome prevalence between populations (by gender) ----
filepaths = c("PesticidesPCBBDEs","PAHs","Combined")

for (i in 1:3){
  data = readRDS(paste0("../Data/",filepaths[i],".rds"))
  outcomes = data[,c("Obesity","Hypertension","Diabetes","Lipid_disorder","Metabolic_syndrome")]
  mytable=NULL
  for (j in 1:ncol(outcomes)){
    vars = outcomes[,j]
    myn = sum(vars=="Yes", na.rm = T)
    myp = formatC(myn/length(vars)*100, format = "f", digits = 1)
    out = paste0(myn, " (", myp, "%)")
    for (k in levels(data$Gender)){
      myn = sum(vars[data$Gender==k]=="Yes", na.rm = T)
      myp = formatC(myn/sum(data$Gender==k)*100, format = "f", digits = 1)
      out=c(out, paste0(myn, " (", myp, "%)"))
    }
    tmp=formatC(chisq.test(table(vars,data$Gender))$p.value, format="e", digits=2)
    out = c(out,tmp)
    mytable=rbind(mytable, out)
    rownames(mytable)[nrow(mytable)]=colnames(outcomes)[j]
  }
  out = nrow(data)
  for (k in levels(data$Gender)){
    out=c(out, sum(data$Gender==k))
  }
  colnames(mytable)=c(paste0(c("Total",levels(data$Gender))," (N=",out,")"),"P-value")
  SaveExcelWithSuperscripts(cbind(rownames(mytable),mytable), paste0("../Exports/",filepaths[i],"_outcome_by_gender.xlsx"))
}

