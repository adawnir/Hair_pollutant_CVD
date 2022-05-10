## Descriptive statistics: covariates, exposures and outcomes

rm(list=ls())
project_path=dirname(rstudioapi::getActiveDocumentContext()$path)
setwd(project_path)

source("functions.R")
library(tidyverse)
library(colorspace)

## Save chemical family annotation as named list----
pest_expo = readRDS("../Processed/PesticidesPCBBDEs_ndimp_naimp.rds")
pahs_expo = readRDS("../Processed/PAHs_ndimp_naimp.rds")
chem_family = c("PCBs","PBDEs","Organochlorine","Organophosphate","Pyrethroids",
                "Phenylpyrazoles","Carbomates","Carboxamides","Dinitroanilines",
                "Oxadiazines","Tobacco-related","PAH metabolites")
annot = rep(chem_family,c(3,1,9,9,4,2,2,1,1,1,2,ncol(pahs_expo)-2))
names(annot) = c(colnames(pest_expo),colnames(pahs_expo))
saveRDS(annot, "../Data/Compound_family_annotation.rds")

## LOD, proportion detected, and concentration range and proportion missing ----
ifelse(dir.exists("../Exports"), "", dir.create("../Exports"))

filepaths = c("PesticidesPCBBDEs","PAHs")
annot = readRDS("../Data/Compound_family_annotation.rds")

for (i in 1:2){
  expo = readRDS(paste0("../Processed/",filepaths[i],"_ndimp_naimp.rds"))
  info = readRDS(paste0("../Processed/",filepaths[i],"_info.rds"))
  annot_sub = annot[colnames(expo)]
  all(colnames(expo) == rownames(info))
  mytable=NULL
  for (k in 1:length(colnames(expo))){
    print(colnames(expo)[k])
    lod = flex_format(info$lod[k], 2, 0.1)
    detect = formatC(1-(info$ndprop[k]+info$naprop[k]), format="f", digits=2)
    missing = formatC(info$naprop[k], format="f", digits=2)
    q = quantile(expo[,k], na.rm = T)
    q = flex_format(q, 2, 0.1)
    tmp=c(lod,detect,q,missing)
    if(!duplicated(annot_sub)[k]){
      mytable=rbind(mytable, rep(NA,length(tmp)))
      rownames(mytable)[nrow(mytable)]=annot_sub[k]
    }
    mytable=rbind(mytable, tmp)
    rownames(mytable)[nrow(mytable)]=colnames(expo)[k]
  }
  colnames(mytable)=c("LOD (pg/mg)","% detected","Min", "Q1","Q2","Q3","Max","% missing")
  # mytable[,-2:3]=ReformatScientificNotation(mytable[,-2:3])
  SaveExcelWithSuperscripts(cbind(rownames(mytable),mytable), paste0("../Exports/",filepaths[i],"_exposure_summary.xlsx"))
}

## Overview of population characteristics ----
filepaths = c("PesticidesPCBBDEs","PAHs","Combined")

for (i in 1:3){
  data = readRDS(paste0("../Processed/",filepaths[i],"_XY.rds"))
  covars = data[,c("Country","Age","Gender","SysBP","DiaBP","BMI_cat","WC")]
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
  # Systolic blood pressure
  mymean=formatC(mean(covars$SysBP, na.rm=TRUE), format="f", digits=1)
  mysd=formatC(sd(covars$SysBP, na.rm=TRUE), format="f", digits=1)
  out=paste0(mymean, " (", mysd, ")")
  for (k in levels(covars$Country)){
    mymean=formatC(mean(covars$SysBP[covars$Country==k], na.rm=TRUE), format="f", digits=1)
    mysd=formatC(sd(covars$SysBP[covars$Country==k], na.rm=TRUE), format="f", digits=1)
    out=c(out, paste0(mymean, " (", mysd, ")"))
  }
  mytable=rbind(mytable, out)
  rownames(mytable)[nrow(mytable)]="Systolic blood pressure (mmHg), Mean (SD)"
  myn = sum(is.na(covars$SysBP))
  if(myn > 0){
    myp = formatC(myn/length(covars$SysBP)*100, format = "f", digits = 1)
    out = paste0(myn, " (", myp, "%)")
    for (k in levels(covars$Country)){
      myn = sum(is.na(covars$SysBP[covars$Country==k]))
      myp = formatC(myn/sum(covars$Country==k)*100, format = "f", digits = 1)
      out=c(out, paste0(myn, " (", myp, "%)"))
    }
    mytable=rbind(mytable, out)
    rownames(mytable)[nrow(mytable)]="Missing, N (%)"
  }
  # Diastolic blood pressure
  mymean=formatC(mean(covars$DiaBP, na.rm=TRUE), format="f", digits=1)
  mysd=formatC(sd(covars$DiaBP, na.rm=TRUE), format="f", digits=1)
  out=paste0(mymean, " (", mysd, ")")
  for (k in levels(covars$Country)){
    mymean=formatC(mean(covars$DiaBP[covars$Country==k], na.rm=TRUE), format="f", digits=1)
    mysd=formatC(sd(covars$DiaBP[covars$Country==k], na.rm=TRUE), format="f", digits=1)
    out=c(out, paste0(mymean, " (", mysd, ")"))
  }
  mytable=rbind(mytable, out)
  rownames(mytable)[nrow(mytable)]="Diastolic blood pressure (mmHg), Mean (SD)"
  myn = sum(is.na(covars$DiaBP))
  if(myn > 0){
    myp = formatC(myn/length(covars$DiaBP)*100, format = "f", digits = 1)
    out = paste0(myn, " (", myp, "%)")
    for (k in levels(covars$Country)){
      myn = sum(is.na(covars$DiaBP[covars$Country==k]))
      myp = formatC(myn/sum(covars$Country==k)*100, format = "f", digits = 1)
      out=c(out, paste0(myn, " (", myp, "%)"))
    }
    mytable=rbind(mytable, out)
    rownames(mytable)[nrow(mytable)]="Missing, N (%)"
  }
  # BMI
  mytable=rbind(mytable, rep(NA, ncol(mytable)))
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
  # Waist circumference
  mymean=formatC(mean(covars$WC, na.rm=TRUE), format="f", digits=1)
  mysd=formatC(sd(covars$WC, na.rm=TRUE), format="f", digits=1)
  out=paste0(mymean, " (", mysd, ")")
  for (k in levels(covars$Country)){
    mymean=formatC(mean(covars$WC[covars$Country==k], na.rm=TRUE), format="f", digits=1)
    mysd=formatC(sd(covars$WC[covars$Country==k], na.rm=TRUE), format="f", digits=1)
    out=c(out, paste0(mymean, " (", mysd, ")"))
  }
  mytable=rbind(mytable, out)
  rownames(mytable)[nrow(mytable)]="Waist circumference (cm), Mean (SD)"
  myn = sum(is.na(covars$WC))
  if(myn > 0){
    myp = formatC(myn/length(covars$WC)*100, format = "f", digits = 1)
    out = paste0(myn, " (", myp, "%)")
    for (k in levels(covars$Country)){
      myn = sum(is.na(covars$DiaBP[covars$Country==k]))
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

## Comparing exposures between populations ----
filepaths = c("PesticidesPCBBDEs","PAHs","Combined")
annot = readRDS("../Data/Compound_family_annotation.rds")

for (i in 1:3){
  data = readRDS(paste0("../Processed/",filepaths[i],"_XY.rds"))
  expo = data[,21:ncol(data)]
  annot_sub = annot[colnames(expo)]
  
  mytable=NULL
  for (k in 1:length(colnames(expo))){
    print(colnames(expo)[k])
    tmp=ContinuousTest3(x=expo[,k], y=data$Country)
    if(!duplicated(annot_sub)[k]){
      mytable=rbind(mytable, rep(NA,length(tmp)))
      rownames(mytable)[nrow(mytable)]=as.character(annot_sub[k])
    }
    mytable=rbind(mytable, tmp)
    rownames(mytable)[nrow(mytable)]=colnames(expo)[k]
  }
  colnames(mytable)=c(paste0(levels(data$Country),", Mean (sd)"),"P-value")
  # mytable=ReformatScientificNotation(mytable)
  SaveExcelWithSuperscripts(cbind(rownames(mytable),mytable), paste0("../Exports/",filepaths[i],"_exposure_comparison.xlsx"))
}

## Comparing outcome prevalence between populations (by country) ----
filepaths = c("PesticidesPCBBDEs","PAHs","Combined")

for (i in 1:3){
  data = readRDS(paste0("../Processed/",filepaths[i],"_XY.rds"))
  outcomes = data[,c("Obesity","Hypertension","Diabetes","Lipid disorder","Metabolic syndrome")]
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
  data = readRDS(paste0("../Processed/",filepaths[i],"_XY.rds"))
  outcomes = data[,c("Obesity","Hypertension","Diabetes","Lipid disorder","Metabolic syndrome")]
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



