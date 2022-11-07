## Descriptive statistics: exposures

rm(list=ls())
project_path=dirname(rstudioapi::getActiveDocumentContext()$path)
setwd(project_path)

source("functions.R")
library(tidyverse)
library(colorspace)

## Save chemical family annotation as named list----
# chem_family = c("PCBs","PBDEs","Organochlorine","Organophosphate","Pyrethroids",
#                 "Phenylpyrazoles","Carbomates","Carboxamides","Dinitroanilines",
#                 "Oxadiazines","Tobacco-related","PAH metabolites")
# annot = rep(chem_family,c(3,1,9,9,4,2,2,1,1,1,2,ncol(pahs_expo)-2))
# names(annot) = c(colnames(pest_expo),colnames(pahs_expo))
# saveRDS(annot, "../Data/Compound_family_annotation.rds")

## LOD, proportion detected, and concentration range and proportion missing ----

filepaths = c("PesticidesPCBBDEs","PAHs")
annot = readRDS("../Data/Compound_family_annotation.rds")

for (i in 1:2){
  expo = readRDS(paste0("../Processed/",filepaths[i],"_expo_ndimp.rds"))
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
  SaveExcelWithSuperscripts(cbind(rownames(mytable),mytable), paste0("../Exports/Descriptive_statistics/",filepaths[i],"_exposure_summary.xlsx"))
}

## Comparing exposures between populations: by data_type ----
annot = readRDS("../Data/Compound_family_annotation.rds")

filepaths = c("PesticidesPCBBDEs_train","PAHs_train","Overlap_test")
for (m in 1:2){
  data1 = readRDS(paste0("../Processed/",filepaths[m],"_expo_ndimp_naimp_log",".rds"))
  data2 = readRDS(paste0("../Processed/",filepaths[3],"_expo_ndimp_naimp_log",".rds"))
  expo = rbind(data1,data2[,colnames(data1)])
  data_type = factor(rep(c(0,1),c(nrow(data1), nrow(data2))), labels = c("Train","Test"))
  
  annot_sub = annot[colnames(expo)]
  
  mytable=NULL
  for (k in 1:length(colnames(expo))){
    print(colnames(expo)[k])
    tmp=ContinuousTest(x=expo[,k], y=data_type)
    if(!duplicated(annot_sub)[k]){
      mytable=rbind(mytable, rep(NA,length(tmp)))
      rownames(mytable)[nrow(mytable)]=as.character(annot_sub[k])
    }
    mytable=rbind(mytable, tmp)
    rownames(mytable)[nrow(mytable)]=colnames(expo)[k]
  }
  colnames(mytable)=c(paste0(levels(data_type),", Mean (sd)"),"P-value")
  # mytable=ReformatScientificNotation(mytable)
  SaveExcelWithSuperscripts(cbind(rownames(mytable),mytable), paste0("../Exports/Descriptive_statistics/Compare_mean_exposure_by_data_type_",gsub("_train","",filepaths[m]),".xlsx"))
}

## Comparing exposures between populations: by Centre ----
annot = readRDS("../Data/Compound_family_annotation.rds")

filepaths = c("PesticidesPCBBDEs_train","PAHs_train","Overlap_test")
for (m in 1:2){
  data1 = readRDS(paste0("../Processed/",filepaths[m],"_expo_ndimp_naimp_log",".rds"))
  data2 = readRDS(paste0("../Processed/",filepaths[3],"_expo_ndimp_naimp_log",".rds"))
  expo = rbind(data1,data2[,colnames(data1)])
  
  data1 = readRDS(paste0("../Data/",filepaths[m],"_covar",".rds"))
  data2 = readRDS(paste0("../Data/",filepaths[3],"_covar",".rds"))
  Centre = factor(c(data1$Centre, data2$Centre), labels = levels(data1$Centre))
  
  annot_sub = annot[colnames(expo)]
  
  mytable=NULL
  for (k in 1:length(colnames(expo))){
    print(colnames(expo)[k])
    tmp=ContinuousTest(x=expo[,k], y=Centre)
    if(!duplicated(annot_sub)[k]){
      mytable=rbind(mytable, rep(NA,length(tmp)))
      rownames(mytable)[nrow(mytable)]=as.character(annot_sub[k])
    }
    mytable=rbind(mytable, tmp)
    rownames(mytable)[nrow(mytable)]=colnames(expo)[k]
  }
  colnames(mytable)=c(paste0(levels(Centre),", Mean (sd)"),"P-value")
  # mytable=ReformatScientificNotation(mytable)
  SaveExcelWithSuperscripts(cbind(rownames(mytable),mytable), paste0("../Exports/Descriptive_statistics/Compare_mean_exposure_by_Centre_",gsub("_train","",filepaths[m]),".xlsx"))
}

## Comparing exposures between populations: by Gender ----
annot = readRDS("../Data/Compound_family_annotation.rds")

filepaths = c("PesticidesPCBBDEs_train","PAHs_train","Overlap_test")
for (m in 1:2){
  data1 = readRDS(paste0("../Processed/",filepaths[m],"_expo_ndimp_naimp_log",".rds"))
  data2 = readRDS(paste0("../Processed/",filepaths[3],"_expo_ndimp_naimp_log",".rds"))
  expo = rbind(data1,data2[,colnames(data1)])
  
  data1 = readRDS(paste0("../Data/",filepaths[m],"_covar",".rds"))
  data2 = readRDS(paste0("../Data/",filepaths[3],"_covar",".rds"))
  Gender = factor(c(data1$Gender, data2$Gender), labels = levels(data1$Gender))
  
  annot_sub = annot[colnames(expo)]
  
  mytable=NULL
  for (k in 1:length(colnames(expo))){
    print(colnames(expo)[k])
    tmp=ContinuousTest(x=expo[,k], y=Gender)
    if(!duplicated(annot_sub)[k]){
      mytable=rbind(mytable, rep(NA,length(tmp)))
      rownames(mytable)[nrow(mytable)]=as.character(annot_sub[k])
    }
    mytable=rbind(mytable, tmp)
    rownames(mytable)[nrow(mytable)]=colnames(expo)[k]
  }
  colnames(mytable)=c(paste0(levels(Gender),", Mean (sd)"),"P-value")
  # mytable=ReformatScientificNotation(mytable)
  SaveExcelWithSuperscripts(cbind(rownames(mytable),mytable), paste0("../Exports/Descriptive_statistics/Compare_mean_exposure_by_Gender_",gsub("_train","",filepaths[m]),".xlsx"))
}


