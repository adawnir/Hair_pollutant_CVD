## Descriptive statistics: exposures

rm(list=ls())
project_path=dirname(rstudioapi::getActiveDocumentContext()$path)
setwd(project_path)

source("functions.R")
library(tidyverse)
library(colorspace)

## Save chemical family annotation as named list----
pest = readRDS("../Data/PesticidesPCBBDEs.rds")
pahs = readRDS("../Data/PAHs.rds")

pest_expo = pest[,33:ncol(pest)]
pahs_expo = pahs[,33:ncol(pahs)]

## Chemical family annotation ----
chem_family = c("PCBs","PBDEs","Organochlorine","Organophosphate","Pyrethroids",
                "Phenylpyrazoles","Carbomates","Carboxamides","Dinitroanilines",
                "Oxadiazines","Tobacco-related","PAH metabolites")
annot = rep(chem_family,c(3,1,9,9,4,2,2,1,1,1,2,ncol(pahs_expo)-2))
names(annot) = c(colnames(pest_expo),colnames(pahs_expo))
saveRDS(annot, "../Data/Compound_family_annotation.rds")

## LOD, proportion detected, and concentration range and proportion missing ----

filepaths = c("PesticidesPCBBDEs","PAHs")
annot = readRDS("../Data/Compound_family_annotation.rds")

for (i in 1:2){
  expo = readRDS(paste0("../Processed/",filepaths[i],"_ndimp.rds"))
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

## Comparing exposures between populations (Pesticides and POPs): by data_type ----
annot = readRDS("../Data/Compound_family_annotation.rds")

{
  data1 = readRDS(paste0("../Processed/","PesticidesPCBBDEs_train_info_expo_ndimp_naimp_log",".rds"))
  data2 = readRDS(paste0("../Processed/","Overlap_test_info_expo_ndimp_naimp_log",".rds"))
  expo = rbind(data1[,34:ncol(data1)],data2[,34:66])
  data_type = factor(c(data1$data_type,data2$data_type), labels = c("Train","Test"))

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
  SaveExcelWithSuperscripts(cbind(rownames(mytable),mytable), paste0("../Exports/Descriptive_statistics/Compare_mean_exposure_by_data_type_PesticidesPCBBDEs.xlsx"))
}

## Comparing exposures between populations (Tobacco-related and PAHs): by data_type ----
annot = readRDS("../Data/Compound_family_annotation.rds")

{
  data1 = readRDS(paste0("../Processed/","PAHs_train_info_expo_ndimp_naimp_log",".rds"))
  data2 = readRDS(paste0("../Processed/","Overlap_test_info_expo_ndimp_naimp_log",".rds"))
  expo = rbind(data1[,34:ncol(data1)],data2[,67:ncol(data2)])
  data_type = factor(c(data1$data_type,data2$data_type), labels = c("Train","Test"))
  
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
  SaveExcelWithSuperscripts(cbind(rownames(mytable),mytable), paste0("../Exports/Descriptive_statistics/Compare_mean_exposure_by_data_type_PAHs.xlsx"))
}

## Comparing exposures between populations (Pesticides and POPs): by Country ----
annot = readRDS("../Data/Compound_family_annotation.rds")

{
  data1 = readRDS(paste0("../Processed/","PesticidesPCBBDEs_train_info_expo_ndimp_naimp_log",".rds"))
  data2 = readRDS(paste0("../Processed/","Overlap_test_info_expo_ndimp_naimp_log",".rds"))
  expo = rbind(data1[,34:ncol(data1)],data2[,34:66])
  Country = factor(c(data1$Country,data2$Country), labels = c("Belgium","Luxembourg"))
  
  annot_sub = annot[colnames(expo)]
  
  mytable=NULL
  for (k in 1:length(colnames(expo))){
    print(colnames(expo)[k])
    tmp=ContinuousTest(x=expo[,k], y=Country)
    if(!duplicated(annot_sub)[k]){
      mytable=rbind(mytable, rep(NA,length(tmp)))
      rownames(mytable)[nrow(mytable)]=as.character(annot_sub[k])
    }
    mytable=rbind(mytable, tmp)
    rownames(mytable)[nrow(mytable)]=colnames(expo)[k]
  }
  colnames(mytable)=c(paste0(levels(Country),", Mean (sd)"),"P-value")
  # mytable=ReformatScientificNotation(mytable)
  SaveExcelWithSuperscripts(cbind(rownames(mytable),mytable), paste0("../Exports/Descriptive_statistics/Compare_mean_exposure_by_Country_PesticidesPCBBDEs.xlsx"))
}

## Comparing exposures between populations (Tobacco-related and PAHs): by Country ----
annot = readRDS("../Data/Compound_family_annotation.rds")

{
  data1 = readRDS(paste0("../Processed/","PAHs_train_info_expo_ndimp_naimp_log",".rds"))
  data2 = readRDS(paste0("../Processed/","Overlap_test_info_expo_ndimp_naimp_log",".rds"))
  expo = rbind(data1[,34:ncol(data1)],data2[,67:ncol(data2)])
  Country = factor(c(data1$Country,data2$Country), labels = c("Belgium","Luxembourg"))
  
  annot_sub = annot[colnames(expo)]
  
  mytable=NULL
  for (k in 1:length(colnames(expo))){
    print(colnames(expo)[k])
    tmp=ContinuousTest(x=expo[,k], y=Country)
    if(!duplicated(annot_sub)[k]){
      mytable=rbind(mytable, rep(NA,length(tmp)))
      rownames(mytable)[nrow(mytable)]=as.character(annot_sub[k])
    }
    mytable=rbind(mytable, tmp)
    rownames(mytable)[nrow(mytable)]=colnames(expo)[k]
  }
  colnames(mytable)=c(paste0(levels(Country),", Mean (sd)"),"P-value")
  # mytable=ReformatScientificNotation(mytable)
  SaveExcelWithSuperscripts(cbind(rownames(mytable),mytable), paste0("../Exports/Descriptive_statistics/Compare_mean_exposure_by_Country_PAHs.xlsx"))
}


