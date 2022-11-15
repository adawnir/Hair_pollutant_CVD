## Descriptive statistics: exposures

rm(list=ls())
project_path=dirname(rstudioapi::getActiveDocumentContext()$path)
setwd(project_path)

source("functions.R")
library(tidyverse)
library(colorspace)

ifelse(dir.exists("../Exports/Descriptive_statistics/"), "", dir.create("../Exports/Descriptive_statistics/"))

filepaths = c("PesticidesPCBBDEs","PAHs")
m = 1

annot = readRDS("../Data/Compound_family_annotation.rds")

## Save chemical family annotation as named list----
# chem_family = c("PCBs","PBDEs","Organochlorine","Organophosphate","Pyrethroids",
#                 "Phenylpyrazoles","Carbomates","Carboxamides","Dinitroanilines",
#                 "Oxadiazines","Tobacco-related","PAH metabolites")
# annot = rep(chem_family,c(3,1,9,9,4,2,2,1,1,1,2,ncol(pahs_expo)-2))
# names(annot) = c(colnames(pest_expo),colnames(pahs_expo))
# saveRDS(annot, "../Data/Compound_family_annotation.rds")

## LOD, proportion detected, and concentration range and proportion missing ----
expo = readRDS(paste0("../Processed/",filepaths[m],"_expo_ndimp.rds"))
info = readRDS(paste0("../Processed/",filepaths[m],"_info.rds"))
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
SaveExcelWithSuperscripts(cbind(rownames(mytable),mytable), paste0("../Exports/Descriptive_statistics/",filepaths[m],"_exposure_summary.xlsx"))

## Comparing exposures between populations: by Centre ----
expo = readRDS(paste0("../Processed/",filepaths[m],"_expo_ndimp_naimp_log",".rds"))

data = readRDS(paste0("../Data/",filepaths[m],"_covar",".rds"))
Centre = factor(data$Centre, labels = levels(data$Centre))

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
SaveExcelWithSuperscripts(cbind(rownames(mytable),mytable), paste0("../Exports/Descriptive_statistics/Compare_mean_exposure_by_Centre_",filepaths[m],".xlsx"))

## Comparing exposures between populations: by Gender ----

expo = readRDS(paste0("../Processed/",filepaths[m],"_expo_ndimp_naimp_log",".rds"))

data = readRDS(paste0("../Data/",filepaths[m],"_covar",".rds"))
Gender = factor(data$Gender, labels = levels(data$Gender))

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
SaveExcelWithSuperscripts(cbind(rownames(mytable),mytable), paste0("../Exports/Descriptive_statistics/Compare_mean_exposure_by_Gender_",filepaths[m],".xlsx"))


