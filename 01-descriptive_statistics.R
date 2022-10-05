## Descriptive statistics: covariates and outcomes

rm(list=ls())
project_path=dirname(rstudioapi::getActiveDocumentContext()$path)
setwd(project_path)

source("functions.R")
library(tidyverse)
library(colorspace)

ifelse(dir.exists("../Exports/Descriptive_statistics/"), "", dir.create("../Exports/Descriptive_statistics/"))

## Overview of population characteristics ----
data = readRDS(paste0("../Data/","All_participants_info",".rds"))
covars = data[,c("Country","Age","Gender","Education","Smoking", "Energy",
                 "Proteins", "Fats", "Carbohydrates", "Fibers",
                 "Physical_activity","Alcohol","data_type")]
## Overview of population characteristics: by Country ----
{mytable=NULL
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
  # Education
  mytable=rbind(mytable, rep(NA, ncol(mytable)))
  rownames(mytable)[nrow(mytable)]="Education, N (%)"
  myn = sum(covars$Education=="Low", na.rm = T)
  myp = formatC(myn/length(covars$Education)*100, format = "f", digits = 1)
  out = paste0(myn, " (", myp, "%)")
  for (k in levels(covars$Country)){
    myn = sum(covars$Education[covars$Country==k]=="Low", na.rm = T)
    myp = formatC(myn/sum(covars$Country==k)*100, format = "f", digits = 1)
    out=c(out, paste0(myn, " (", myp, "%)"))
  }
  mytable=rbind(mytable, out)
  rownames(mytable)[nrow(mytable)]="Low (before high school)"
  myn = sum(covars$Education=="Intermediate", na.rm = T)
  myp = formatC(myn/length(covars$Education)*100, format = "f", digits = 1)
  out = paste0(myn, " (", myp, "%)")
  for (k in levels(covars$Country)){
    myn = sum(covars$Education[covars$Country==k]=="Intermediate", na.rm = T)
    myp = formatC(myn/sum(covars$Country==k)*100, format = "f", digits = 1)
    out=c(out, paste0(myn, " (", myp, "%)"))
  }
  mytable=rbind(mytable, out)
  rownames(mytable)[nrow(mytable)]="Intermediate (high school)"
  myn = sum(covars$Education=="High", na.rm = T)
  myp = formatC(myn/length(covars$Education)*100, format = "f", digits = 1)
  out = paste0(myn, " (", myp, "%)")
  for (k in levels(covars$Country)){
    myn = sum(covars$Education[covars$Country==k]=="High", na.rm = T)
    myp = formatC(myn/sum(covars$Country==k)*100, format = "f", digits = 1)
    out=c(out, paste0(myn, " (", myp, "%)"))
  }
  mytable=rbind(mytable, out)
  rownames(mytable)[nrow(mytable)]="High (above high school)"
  myn = sum(is.na(covars$Education))
  if(myn > 0){
    myp = formatC(myn/length(covars$Education)*100, format = "f", digits = 1)
    out = paste0(myn, " (", myp, "%)")
    for (k in levels(covars$Country)){
      myn = sum(is.na(covars$Education[covars$Country==k]))
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
  rownames(mytable)[nrow(mytable)]="Former smoker"
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
  # Fibres
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
  # Physical activity
  mytable=rbind(mytable, rep(NA, ncol(mytable)))
  rownames(mytable)[nrow(mytable)]="Physical activity, N (%)"
  myn = sum(covars$Physical_activity=="Inactive", na.rm = T)
  myp = formatC(myn/length(covars$Physical_activity)*100, format = "f", digits = 1)
  out = paste0(myn, " (", myp, "%)")
  for (k in levels(covars$Country)){
    myn = sum(covars$Physical_activity[covars$Country==k]=="Inactive", na.rm = T)
    myp = formatC(myn/sum(covars$Country==k)*100, format = "f", digits = 1)
    out=c(out, paste0(myn, " (", myp, "%)"))
  }
  mytable=rbind(mytable, out)
  rownames(mytable)[nrow(mytable)]="Inactive"
  myn = sum(covars$Physical_activity=="Moderately active", na.rm = T)
  myp = formatC(myn/length(covars$Physical_activity)*100, format = "f", digits = 1)
  out = paste0(myn, " (", myp, "%)")
  for (k in levels(covars$Country)){
    myn = sum(covars$Physical_activity[covars$Country==k]=="Moderately active", na.rm = T)
    myp = formatC(myn/sum(covars$Country==k)*100, format = "f", digits = 1)
    out=c(out, paste0(myn, " (", myp, "%)"))
  }
  mytable=rbind(mytable, out)
  rownames(mytable)[nrow(mytable)]="Moderately active"
  myn = sum(covars$Physical_activity=="Active", na.rm = T)
  myp = formatC(myn/length(covars$Physical_activity)*100, format = "f", digits = 1)
  out = paste0(myn, " (", myp, "%)")
  for (k in levels(covars$Country)){
    myn = sum(covars$Physical_activity[covars$Country==k]=="Active", na.rm = T)
    myp = formatC(myn/sum(covars$Country==k)*100, format = "f", digits = 1)
    out=c(out, paste0(myn, " (", myp, "%)"))
  }
  mytable=rbind(mytable, out)
  rownames(mytable)[nrow(mytable)]="Active"
  myn = sum(is.na(covars$Physical_activity))
  if(myn > 0){
    myp = formatC(myn/length(covars$Physical_activity)*100, format = "f", digits = 1)
    out = paste0(myn, " (", myp, "%)")
    for (k in levels(covars$Country)){
      myn = sum(is.na(covars$Physical_activity[covars$Country==k]))
      myp = formatC(myn/sum(covars$Country==k)*100, format = "f", digits = 1)
      out=c(out, paste0(myn, " (", myp, "%)"))
    }
    mytable=rbind(mytable, out)
    rownames(mytable)[nrow(mytable)]="Missing, N (%)"
  }
  # Alcohol
  mytable=rbind(mytable, rep(NA, ncol(mytable)))
  rownames(mytable)[nrow(mytable)]="Alcohol consumption, N (%)"
  myn = sum(covars$Alcohol=="Low", na.rm = T)
  myp = formatC(myn/length(covars$Alcohol)*100, format = "f", digits = 1)
  out = paste0(myn, " (", myp, "%)")
  for (k in levels(covars$Country)){
    myn = sum(covars$Alcohol[covars$Country==k]=="Low", na.rm = T)
    myp = formatC(myn/sum(covars$Country==k)*100, format = "f", digits = 1)
    out=c(out, paste0(myn, " (", myp, "%)"))
  }
  mytable=rbind(mytable, out)
  rownames(mytable)[nrow(mytable)]="Low (<0.1g/day)"
  myn = sum(covars$Alcohol=="Moderate", na.rm = T)
  myp = formatC(myn/length(covars$Alcohol)*100, format = "f", digits = 1)
  out = paste0(myn, " (", myp, "%)")
  for (k in levels(covars$Country)){
    myn = sum(covars$Alcohol[covars$Country==k]=="Moderate", na.rm = T)
    myp = formatC(myn/sum(covars$Country==k)*100, format = "f", digits = 1)
    out=c(out, paste0(myn, " (", myp, "%)"))
  }
  mytable=rbind(mytable, out)
  rownames(mytable)[nrow(mytable)]="Moderate (0.1-10g/day)"
  myn = sum(covars$Alcohol=="High", na.rm = T)
  myp = formatC(myn/length(covars$Alcohol)*100, format = "f", digits = 1)
  out = paste0(myn, " (", myp, "%)")
  for (k in levels(covars$Country)){
    myn = sum(covars$Alcohol[covars$Country==k]=="High", na.rm = T)
    myp = formatC(myn/sum(covars$Country==k)*100, format = "f", digits = 1)
    out=c(out, paste0(myn, " (", myp, "%)"))
  }
  mytable=rbind(mytable, out)
  rownames(mytable)[nrow(mytable)]="High (>10g/day)"
  myn = sum(is.na(covars$Alcohol))
  if(myn > 0){
    myp = formatC(myn/length(covars$Alcohol)*100, format = "f", digits = 1)
    out = paste0(myn, " (", myp, "%)")
    for (k in levels(covars$Country)){
      myn = sum(is.na(covars$Alcohol[covars$Country==k]))
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
  SaveExcelWithSuperscripts(cbind(rownames(mytable),mytable), paste0("../Exports/Descriptive_statistics/","Characteristics_by_","Country",".xlsx"))
}
# ## Overview of population characteristics: by Gender ----
# {mytable=NULL
#   # Age
#   mymean=formatC(mean(covars$Age, na.rm=TRUE), format="f", digits=1)
#   mysd=formatC(sd(covars$Age, na.rm=TRUE), format="f", digits=1)
#   out=paste0(mymean, " (", mysd, ")")
#   for (k in levels(covars$Gender)){
#     mymean=formatC(mean(covars$Age[covars$Gender==k], na.rm=TRUE), format="f", digits=1)
#     mysd=formatC(sd(covars$Age[covars$Gender==k], na.rm=TRUE), format="f", digits=1)
#     out=c(out, paste0(mymean, " (", mysd, ")"))
#   }
#   mytable=rbind(mytable, out)
#   rownames(mytable)[nrow(mytable)]="Age (years), Mean (SD)"
#   myn = sum(is.na(covars$Age))
#   if(myn > 0){
#     myp = formatC(myn/length(covars$Age)*100, format = "f", digits = 1)
#     out = paste0(myn, " (", myp, "%)")
#     for (k in levels(covars$Gender)){
#       myn = sum(is.na(covars$Age[covars$Gender==k]))
#       myp = formatC(myn/sum(covars$Gender==k)*100, format = "f", digits = 1)
#       out=c(out, paste0(myn, " (", myp, "%)"))
#     }
#     mytable=rbind(mytable, out)
#     rownames(mytable)[nrow(mytable)]="Missing, N (%)"
#   }
#   # Country
#   mytable=rbind(mytable, rep(NA, ncol(mytable)))
#   rownames(mytable)[nrow(mytable)]="Country, N (%)"
#   myn = sum(covars$Country=="Belgium", na.rm = T)
#   myp = formatC(myn/length(covars$Country)*100, format = "f", digits = 1)
#   out = paste0(myn, " (", myp, "%)")
#   for (k in levels(covars$Gender)){
#     myn = sum(covars$Country[covars$Gender==k]=="Belgium", na.rm = T)
#     myp = formatC(myn/sum(covars$Gender==k)*100, format = "f", digits = 1)
#     out=c(out, paste0(myn, " (", myp, "%)"))
#   }
#   mytable=rbind(mytable, out)
#   rownames(mytable)[nrow(mytable)]="Belgium"
#   myn = sum(covars$Country=="Luxembourg", na.rm = T)
#   myp = formatC(myn/length(covars$Country)*100, format = "f", digits = 1)
#   out = paste0(myn, " (", myp, "%)")
#   for (k in levels(covars$Gender)){
#     myn = sum(covars$Country[covars$Gender==k]=="Luxembourg", na.rm = T)
#     myp = formatC(myn/sum(covars$Gender==k)*100, format = "f", digits = 1)
#     out=c(out, paste0(myn, " (", myp, "%)"))
#   }
#   mytable=rbind(mytable, out)
#   rownames(mytable)[nrow(mytable)]="Luxembourg"
#   myn = sum(is.na(covars$Country))
#   if(myn > 0){
#     myp = formatC(myn/length(covars$Country)*100, format = "f", digits = 1)
#     out = paste0(myn, " (", myp, "%)")
#     for (k in levels(covars$Gender)){
#       myn = sum(is.na(covars$Country[covars$Gender==k]))
#       myp = formatC(myn/sum(covars$Gender==k)*100, format = "f", digits = 1)
#       out=c(out, paste0(myn, " (", myp, "%)"))
#     }
#     mytable=rbind(mytable, out)
#     rownames(mytable)[nrow(mytable)]="Missing, N (%)"
#   }
#   # Smoking
#   mytable=rbind(mytable, rep(NA, ncol(mytable)))
#   rownames(mytable)[nrow(mytable)]="Smoking status, N (%)"
#   myn = sum(covars$Smoking=="Never smoker", na.rm = T)
#   myp = formatC(myn/length(covars$Smoking)*100, format = "f", digits = 1)
#   out = paste0(myn, " (", myp, "%)")
#   for (k in levels(covars$Gender)){
#     myn = sum(covars$Smoking[covars$Gender==k]=="Never smoker", na.rm = T)
#     myp = formatC(myn/sum(covars$Gender==k)*100, format = "f", digits = 1)
#     out=c(out, paste0(myn, " (", myp, "%)"))
#   }
#   mytable=rbind(mytable, out)
#   rownames(mytable)[nrow(mytable)]="Never smoker"
#   myn = sum(covars$Smoking=="Former smoker", na.rm = T)
#   myp = formatC(myn/length(covars$Smoking)*100, format = "f", digits = 1)
#   out = paste0(myn, " (", myp, "%)")
#   for (k in levels(covars$Gender)){
#     myn = sum(covars$Smoking[covars$Gender==k]=="Former smoker", na.rm = T)
#     myp = formatC(myn/sum(covars$Gender==k)*100, format = "f", digits = 1)
#     out=c(out, paste0(myn, " (", myp, "%)"))
#   }
#   mytable=rbind(mytable, out)
#   rownames(mytable)[nrow(mytable)]="Former smoker"
#   myn = sum(covars$Smoking=="Current smoker", na.rm = T)
#   myp = formatC(myn/length(covars$Smoking)*100, format = "f", digits = 1)
#   out = paste0(myn, " (", myp, "%)")
#   for (k in levels(covars$Gender)){
#     myn = sum(covars$Smoking[covars$Gender==k]=="Current smoker", na.rm = T)
#     myp = formatC(myn/sum(covars$Gender==k)*100, format = "f", digits = 1)
#     out=c(out, paste0(myn, " (", myp, "%)"))
#   }
#   mytable=rbind(mytable, out)
#   rownames(mytable)[nrow(mytable)]="Current smoker"
#   myn = sum(is.na(covars$Smoking))
#   if(myn > 0){
#     myp = formatC(myn/length(covars$Smoking)*100, format = "f", digits = 1)
#     out = paste0(myn, " (", myp, "%)")
#     for (k in levels(covars$Gender)){
#       myn = sum(is.na(covars$Smoking[covars$Gender==k]))
#       myp = formatC(myn/sum(covars$Gender==k)*100, format = "f", digits = 1)
#       out=c(out, paste0(myn, " (", myp, "%)"))
#     }
#     mytable=rbind(mytable, out)
#     rownames(mytable)[nrow(mytable)]="Missing, N (%)"
#   }
#   # Energy
#   mymean=formatC(mean(covars$Energy, na.rm=TRUE), format="f", digits=1)
#   mysd=formatC(sd(covars$Energy, na.rm=TRUE), format="f", digits=1)
#   out=paste0(mymean, " (", mysd, ")")
#   for (k in levels(covars$Gender)){
#     mymean=formatC(mean(covars$Energy[covars$Gender==k], na.rm=TRUE), format="f", digits=1)
#     mysd=formatC(sd(covars$Energy[covars$Gender==k], na.rm=TRUE), format="f", digits=1)
#     out=c(out, paste0(mymean, " (", mysd, ")"))
#   }
#   mytable=rbind(mytable, out)
#   rownames(mytable)[nrow(mytable)]="Energy (kcal/day), Mean (SD)"
#   myn = sum(is.na(covars$Energy))
#   if(myn > 0){
#     myp = formatC(myn/length(covars$Energy)*100, format = "f", digits = 1)
#     out = paste0(myn, " (", myp, "%)")
#     for (k in levels(covars$Gender)){
#       myn = sum(is.na(covars$Energy[covars$Gender==k]))
#       myp = formatC(myn/sum(covars$Gender==k)*100, format = "f", digits = 1)
#       out=c(out, paste0(myn, " (", myp, "%)"))
#     }
#     mytable=rbind(mytable, out)
#     rownames(mytable)[nrow(mytable)]="Missing, N (%)"
#   }
#   # Proteins
#   mymean=formatC(mean(covars$Proteins, na.rm=TRUE), format="f", digits=1)
#   mysd=formatC(sd(covars$Proteins, na.rm=TRUE), format="f", digits=1)
#   out=paste0(mymean, " (", mysd, ")")
#   for (k in levels(covars$Gender)){
#     mymean=formatC(mean(covars$Proteins[covars$Gender==k], na.rm=TRUE), format="f", digits=1)
#     mysd=formatC(sd(covars$Proteins[covars$Gender==k], na.rm=TRUE), format="f", digits=1)
#     out=c(out, paste0(mymean, " (", mysd, ")"))
#   }
#   mytable=rbind(mytable, out)
#   rownames(mytable)[nrow(mytable)]="Proteins (g/day), Mean (SD)"
#   myn = sum(is.na(covars$Proteins))
#   if(myn > 0){
#     myp = formatC(myn/length(covars$Proteins)*100, format = "f", digits = 1)
#     out = paste0(myn, " (", myp, "%)")
#     for (k in levels(covars$Gender)){
#       myn = sum(is.na(covars$Proteins[covars$Gender==k]))
#       myp = formatC(myn/sum(covars$Gender==k)*100, format = "f", digits = 1)
#       out=c(out, paste0(myn, " (", myp, "%)"))
#     }
#     mytable=rbind(mytable, out)
#     rownames(mytable)[nrow(mytable)]="Missing, N (%)"
#   }
#   # Fats
#   mymean=formatC(mean(covars$Fats, na.rm=TRUE), format="f", digits=1)
#   mysd=formatC(sd(covars$Fats, na.rm=TRUE), format="f", digits=1)
#   out=paste0(mymean, " (", mysd, ")")
#   for (k in levels(covars$Gender)){
#     mymean=formatC(mean(covars$Fats[covars$Gender==k], na.rm=TRUE), format="f", digits=1)
#     mysd=formatC(sd(covars$Fats[covars$Gender==k], na.rm=TRUE), format="f", digits=1)
#     out=c(out, paste0(mymean, " (", mysd, ")"))
#   }
#   mytable=rbind(mytable, out)
#   rownames(mytable)[nrow(mytable)]="Fats (g/day), Mean (SD)"
#   myn = sum(is.na(covars$Fats))
#   if(myn > 0){
#     myp = formatC(myn/length(covars$Fats)*100, format = "f", digits = 1)
#     out = paste0(myn, " (", myp, "%)")
#     for (k in levels(covars$Gender)){
#       myn = sum(is.na(covars$Fats[covars$Gender==k]))
#       myp = formatC(myn/sum(covars$Gender==k)*100, format = "f", digits = 1)
#       out=c(out, paste0(myn, " (", myp, "%)"))
#     }
#     mytable=rbind(mytable, out)
#     rownames(mytable)[nrow(mytable)]="Missing, N (%)"
#   }
#   # Carbohydrates
#   mymean=formatC(mean(covars$Carbohydrates, na.rm=TRUE), format="f", digits=1)
#   mysd=formatC(sd(covars$Carbohydrates, na.rm=TRUE), format="f", digits=1)
#   out=paste0(mymean, " (", mysd, ")")
#   for (k in levels(covars$Gender)){
#     mymean=formatC(mean(covars$Carbohydrates[covars$Gender==k], na.rm=TRUE), format="f", digits=1)
#     mysd=formatC(sd(covars$Carbohydrates[covars$Gender==k], na.rm=TRUE), format="f", digits=1)
#     out=c(out, paste0(mymean, " (", mysd, ")"))
#   }
#   mytable=rbind(mytable, out)
#   rownames(mytable)[nrow(mytable)]="Carbohydrates (g/day), Mean (SD)"
#   myn = sum(is.na(covars$Carbohydrates))
#   if(myn > 0){
#     myp = formatC(myn/length(covars$Carbohydrates)*100, format = "f", digits = 1)
#     out = paste0(myn, " (", myp, "%)")
#     for (k in levels(covars$Gender)){
#       myn = sum(is.na(covars$Carbohydrates[covars$Gender==k]))
#       myp = formatC(myn/sum(covars$Gender==k)*100, format = "f", digits = 1)
#       out=c(out, paste0(myn, " (", myp, "%)"))
#     }
#     mytable=rbind(mytable, out)
#     rownames(mytable)[nrow(mytable)]="Missing, N (%)"
#   }
#   # Fibres
#   mymean=formatC(mean(covars$Fibers, na.rm=TRUE), format="f", digits=1)
#   mysd=formatC(sd(covars$Fibers, na.rm=TRUE), format="f", digits=1)
#   out=paste0(mymean, " (", mysd, ")")
#   for (k in levels(covars$Gender)){
#     mymean=formatC(mean(covars$Fibers[covars$Gender==k], na.rm=TRUE), format="f", digits=1)
#     mysd=formatC(sd(covars$Fibers[covars$Gender==k], na.rm=TRUE), format="f", digits=1)
#     out=c(out, paste0(mymean, " (", mysd, ")"))
#   }
#   mytable=rbind(mytable, out)
#   rownames(mytable)[nrow(mytable)]="Fibres (g/day), Mean (SD)"
#   myn = sum(is.na(covars$Fibers))
#   if(myn > 0){
#     myp = formatC(myn/length(covars$Fibers)*100, format = "f", digits = 1)
#     out = paste0(myn, " (", myp, "%)")
#     for (k in levels(covars$Gender)){
#       myn = sum(is.na(covars$Fibers[covars$Gender==k]))
#       myp = formatC(myn/sum(covars$Gender==k)*100, format = "f", digits = 1)
#       out=c(out, paste0(myn, " (", myp, "%)"))
#     }
#     mytable=rbind(mytable, out)
#     rownames(mytable)[nrow(mytable)]="Missing, N (%)"
#   }
#   out = nrow(covars)
#   for (k in levels(covars$Gender)){
#     out=c(out, sum(covars$Gender==k))
#   }
#   colnames(mytable)=paste0(c("Total",levels(covars$Gender))," (N=",out,")")
#   SaveExcelWithSuperscripts(cbind(rownames(mytable),mytable), paste0("../Exports/Descriptive_statistics/","Characteristics_by_","Gender",".xlsx"))
# }

## Overview of population characteristics: by data_type ----
{mytable=NULL
  # Age
  mymean=formatC(mean(covars$Age, na.rm=TRUE), format="f", digits=1)
  mysd=formatC(sd(covars$Age, na.rm=TRUE), format="f", digits=1)
  out=paste0(mymean, " (", mysd, ")")
  for (k in levels(covars$data_type)){
    mymean=formatC(mean(covars$Age[covars$data_type==k], na.rm=TRUE), format="f", digits=1)
    mysd=formatC(sd(covars$Age[covars$data_type==k], na.rm=TRUE), format="f", digits=1)
    out=c(out, paste0(mymean, " (", mysd, ")"))
  }
  mytable=rbind(mytable, out)
  rownames(mytable)[nrow(mytable)]="Age (years), Mean (SD)"
  myn = sum(is.na(covars$Age))
  if(myn > 0){
    myp = formatC(myn/length(covars$Age)*100, format = "f", digits = 1)
    out = paste0(myn, " (", myp, "%)")
    for (k in levels(covars$data_type)){
      myn = sum(is.na(covars$Age[covars$data_type==k]))
      myp = formatC(myn/sum(covars$data_type==k)*100, format = "f", digits = 1)
      out=c(out, paste0(myn, " (", myp, "%)"))
    }
    mytable=rbind(mytable, out)
    rownames(mytable)[nrow(mytable)]="Missing, N (%)"
  }
  # Country
  mytable=rbind(mytable, rep(NA, ncol(mytable)))
  rownames(mytable)[nrow(mytable)]="Country, N (%)"
  myn = sum(covars$Country=="Belgium", na.rm = T)
  myp = formatC(myn/length(covars$Country)*100, format = "f", digits = 1)
  out = paste0(myn, " (", myp, "%)")
  for (k in levels(covars$data_type)){
    myn = sum(covars$Country[covars$data_type==k]=="Belgium", na.rm = T)
    myp = formatC(myn/sum(covars$data_type==k)*100, format = "f", digits = 1)
    out=c(out, paste0(myn, " (", myp, "%)"))
  }
  mytable=rbind(mytable, out)
  rownames(mytable)[nrow(mytable)]="Belgium"
  myn = sum(covars$Country=="Luxembourg", na.rm = T)
  myp = formatC(myn/length(covars$Country)*100, format = "f", digits = 1)
  out = paste0(myn, " (", myp, "%)")
  for (k in levels(covars$data_type)){
    myn = sum(covars$Country[covars$data_type==k]=="Luxembourg", na.rm = T)
    myp = formatC(myn/sum(covars$data_type==k)*100, format = "f", digits = 1)
    out=c(out, paste0(myn, " (", myp, "%)"))
  }
  mytable=rbind(mytable, out)
  rownames(mytable)[nrow(mytable)]="Luxembourg"
  myn = sum(is.na(covars$Country))
  if(myn > 0){
    myp = formatC(myn/length(covars$Country)*100, format = "f", digits = 1)
    out = paste0(myn, " (", myp, "%)")
    for (k in levels(covars$data_type)){
      myn = sum(is.na(covars$Country[covars$data_type==k]))
      myp = formatC(myn/sum(covars$data_type==k)*100, format = "f", digits = 1)
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
  for (k in levels(covars$data_type)){
    myn = sum(covars$Gender[covars$data_type==k]=="Male", na.rm = T)
    myp = formatC(myn/sum(covars$data_type==k)*100, format = "f", digits = 1)
    out=c(out, paste0(myn, " (", myp, "%)"))
  }
  mytable=rbind(mytable, out)
  rownames(mytable)[nrow(mytable)]="Male"
  myn = sum(covars$Gender=="Female", na.rm = T)
  myp = formatC(myn/length(covars$Gender)*100, format = "f", digits = 1)
  out = paste0(myn, " (", myp, "%)")
  for (k in levels(covars$data_type)){
    myn = sum(covars$Gender[covars$data_type==k]=="Female", na.rm = T)
    myp = formatC(myn/sum(covars$data_type==k)*100, format = "f", digits = 1)
    out=c(out, paste0(myn, " (", myp, "%)"))
  }
  mytable=rbind(mytable, out)
  rownames(mytable)[nrow(mytable)]="Female"
  myn = sum(is.na(covars$Gender))
  if(myn > 0){
    myp = formatC(myn/length(covars$Gender)*100, format = "f", digits = 1)
    out = paste0(myn, " (", myp, "%)")
    for (k in levels(covars$data_type)){
      myn = sum(is.na(covars$Gender[covars$data_type==k]))
      myp = formatC(myn/sum(covars$data_type==k)*100, format = "f", digits = 1)
      out=c(out, paste0(myn, " (", myp, "%)"))
    }
    mytable=rbind(mytable, out)
    rownames(mytable)[nrow(mytable)]="Missing, N (%)"
  }
  # Education
  mytable=rbind(mytable, rep(NA, ncol(mytable)))
  rownames(mytable)[nrow(mytable)]="Education, N (%)"
  myn = sum(covars$Education=="Low", na.rm = T)
  myp = formatC(myn/length(covars$Education)*100, format = "f", digits = 1)
  out = paste0(myn, " (", myp, "%)")
  for (k in levels(covars$data_type)){
    myn = sum(covars$Education[covars$data_type==k]=="Low", na.rm = T)
    myp = formatC(myn/sum(covars$data_type==k)*100, format = "f", digits = 1)
    out=c(out, paste0(myn, " (", myp, "%)"))
  }
  mytable=rbind(mytable, out)
  rownames(mytable)[nrow(mytable)]="Low (before high school)"
  myn = sum(covars$Education=="Intermediate", na.rm = T)
  myp = formatC(myn/length(covars$Education)*100, format = "f", digits = 1)
  out = paste0(myn, " (", myp, "%)")
  for (k in levels(covars$data_type)){
    myn = sum(covars$Education[covars$data_type==k]=="Intermediate", na.rm = T)
    myp = formatC(myn/sum(covars$data_type==k)*100, format = "f", digits = 1)
    out=c(out, paste0(myn, " (", myp, "%)"))
  }
  mytable=rbind(mytable, out)
  rownames(mytable)[nrow(mytable)]="Intermediate (high school)"
  myn = sum(covars$Education=="High", na.rm = T)
  myp = formatC(myn/length(covars$Education)*100, format = "f", digits = 1)
  out = paste0(myn, " (", myp, "%)")
  for (k in levels(covars$data_type)){
    myn = sum(covars$Education[covars$data_type==k]=="High", na.rm = T)
    myp = formatC(myn/sum(covars$data_type==k)*100, format = "f", digits = 1)
    out=c(out, paste0(myn, " (", myp, "%)"))
  }
  mytable=rbind(mytable, out)
  rownames(mytable)[nrow(mytable)]="High (above high school)"
  myn = sum(is.na(covars$Education))
  if(myn > 0){
    myp = formatC(myn/length(covars$Education)*100, format = "f", digits = 1)
    out = paste0(myn, " (", myp, "%)")
    for (k in levels(covars$data_type)){
      myn = sum(is.na(covars$Education[covars$data_type==k]))
      myp = formatC(myn/sum(covars$data_type==k)*100, format = "f", digits = 1)
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
  for (k in levels(covars$data_type)){
    myn = sum(covars$Smoking[covars$data_type==k]=="Never smoker", na.rm = T)
    myp = formatC(myn/sum(covars$data_type==k)*100, format = "f", digits = 1)
    out=c(out, paste0(myn, " (", myp, "%)"))
  }
  mytable=rbind(mytable, out)
  rownames(mytable)[nrow(mytable)]="Never smoker"
  myn = sum(covars$Smoking=="Former smoker", na.rm = T)
  myp = formatC(myn/length(covars$Smoking)*100, format = "f", digits = 1)
  out = paste0(myn, " (", myp, "%)")
  for (k in levels(covars$data_type)){
    myn = sum(covars$Smoking[covars$data_type==k]=="Former smoker", na.rm = T)
    myp = formatC(myn/sum(covars$data_type==k)*100, format = "f", digits = 1)
    out=c(out, paste0(myn, " (", myp, "%)"))
  }
  mytable=rbind(mytable, out)
  rownames(mytable)[nrow(mytable)]="Former smoker"
  myn = sum(covars$Smoking=="Current smoker", na.rm = T)
  myp = formatC(myn/length(covars$Smoking)*100, format = "f", digits = 1)
  out = paste0(myn, " (", myp, "%)")
  for (k in levels(covars$data_type)){
    myn = sum(covars$Smoking[covars$data_type==k]=="Current smoker", na.rm = T)
    myp = formatC(myn/sum(covars$data_type==k)*100, format = "f", digits = 1)
    out=c(out, paste0(myn, " (", myp, "%)"))
  }
  mytable=rbind(mytable, out)
  rownames(mytable)[nrow(mytable)]="Current smoker"
  myn = sum(is.na(covars$Smoking))
  if(myn > 0){
    myp = formatC(myn/length(covars$Smoking)*100, format = "f", digits = 1)
    out = paste0(myn, " (", myp, "%)")
    for (k in levels(covars$data_type)){
      myn = sum(is.na(covars$Smoking[covars$data_type==k]))
      myp = formatC(myn/sum(covars$data_type==k)*100, format = "f", digits = 1)
      out=c(out, paste0(myn, " (", myp, "%)"))
    }
    mytable=rbind(mytable, out)
    rownames(mytable)[nrow(mytable)]="Missing, N (%)"
  }
  # Energy
  mymean=formatC(mean(covars$Energy, na.rm=TRUE), format="f", digits=1)
  mysd=formatC(sd(covars$Energy, na.rm=TRUE), format="f", digits=1)
  out=paste0(mymean, " (", mysd, ")")
  for (k in levels(covars$data_type)){
    mymean=formatC(mean(covars$Energy[covars$data_type==k], na.rm=TRUE), format="f", digits=1)
    mysd=formatC(sd(covars$Energy[covars$data_type==k], na.rm=TRUE), format="f", digits=1)
    out=c(out, paste0(mymean, " (", mysd, ")"))
  }
  mytable=rbind(mytable, out)
  rownames(mytable)[nrow(mytable)]="Energy (kcal/day), Mean (SD)"
  myn = sum(is.na(covars$Energy))
  if(myn > 0){
    myp = formatC(myn/length(covars$Energy)*100, format = "f", digits = 1)
    out = paste0(myn, " (", myp, "%)")
    for (k in levels(covars$data_type)){
      myn = sum(is.na(covars$Energy[covars$data_type==k]))
      myp = formatC(myn/sum(covars$data_type==k)*100, format = "f", digits = 1)
      out=c(out, paste0(myn, " (", myp, "%)"))
    }
    mytable=rbind(mytable, out)
    rownames(mytable)[nrow(mytable)]="Missing, N (%)"
  }
  # Proteins
  mymean=formatC(mean(covars$Proteins, na.rm=TRUE), format="f", digits=1)
  mysd=formatC(sd(covars$Proteins, na.rm=TRUE), format="f", digits=1)
  out=paste0(mymean, " (", mysd, ")")
  for (k in levels(covars$data_type)){
    mymean=formatC(mean(covars$Proteins[covars$data_type==k], na.rm=TRUE), format="f", digits=1)
    mysd=formatC(sd(covars$Proteins[covars$data_type==k], na.rm=TRUE), format="f", digits=1)
    out=c(out, paste0(mymean, " (", mysd, ")"))
  }
  mytable=rbind(mytable, out)
  rownames(mytable)[nrow(mytable)]="Proteins (g/day), Mean (SD)"
  myn = sum(is.na(covars$Proteins))
  if(myn > 0){
    myp = formatC(myn/length(covars$Proteins)*100, format = "f", digits = 1)
    out = paste0(myn, " (", myp, "%)")
    for (k in levels(covars$data_type)){
      myn = sum(is.na(covars$Proteins[covars$data_type==k]))
      myp = formatC(myn/sum(covars$data_type==k)*100, format = "f", digits = 1)
      out=c(out, paste0(myn, " (", myp, "%)"))
    }
    mytable=rbind(mytable, out)
    rownames(mytable)[nrow(mytable)]="Missing, N (%)"
  }
  # Fats
  mymean=formatC(mean(covars$Fats, na.rm=TRUE), format="f", digits=1)
  mysd=formatC(sd(covars$Fats, na.rm=TRUE), format="f", digits=1)
  out=paste0(mymean, " (", mysd, ")")
  for (k in levels(covars$data_type)){
    mymean=formatC(mean(covars$Fats[covars$data_type==k], na.rm=TRUE), format="f", digits=1)
    mysd=formatC(sd(covars$Fats[covars$data_type==k], na.rm=TRUE), format="f", digits=1)
    out=c(out, paste0(mymean, " (", mysd, ")"))
  }
  mytable=rbind(mytable, out)
  rownames(mytable)[nrow(mytable)]="Fats (g/day), Mean (SD)"
  myn = sum(is.na(covars$Fats))
  if(myn > 0){
    myp = formatC(myn/length(covars$Fats)*100, format = "f", digits = 1)
    out = paste0(myn, " (", myp, "%)")
    for (k in levels(covars$data_type)){
      myn = sum(is.na(covars$Fats[covars$data_type==k]))
      myp = formatC(myn/sum(covars$data_type==k)*100, format = "f", digits = 1)
      out=c(out, paste0(myn, " (", myp, "%)"))
    }
    mytable=rbind(mytable, out)
    rownames(mytable)[nrow(mytable)]="Missing, N (%)"
  }
  # Carbohydrates
  mymean=formatC(mean(covars$Carbohydrates, na.rm=TRUE), format="f", digits=1)
  mysd=formatC(sd(covars$Carbohydrates, na.rm=TRUE), format="f", digits=1)
  out=paste0(mymean, " (", mysd, ")")
  for (k in levels(covars$data_type)){
    mymean=formatC(mean(covars$Carbohydrates[covars$data_type==k], na.rm=TRUE), format="f", digits=1)
    mysd=formatC(sd(covars$Carbohydrates[covars$data_type==k], na.rm=TRUE), format="f", digits=1)
    out=c(out, paste0(mymean, " (", mysd, ")"))
  }
  mytable=rbind(mytable, out)
  rownames(mytable)[nrow(mytable)]="Carbohydrates (g/day), Mean (SD)"
  myn = sum(is.na(covars$Carbohydrates))
  if(myn > 0){
    myp = formatC(myn/length(covars$Carbohydrates)*100, format = "f", digits = 1)
    out = paste0(myn, " (", myp, "%)")
    for (k in levels(covars$data_type)){
      myn = sum(is.na(covars$Carbohydrates[covars$data_type==k]))
      myp = formatC(myn/sum(covars$data_type==k)*100, format = "f", digits = 1)
      out=c(out, paste0(myn, " (", myp, "%)"))
    }
    mytable=rbind(mytable, out)
    rownames(mytable)[nrow(mytable)]="Missing, N (%)"
  }
  # Fibres
  mymean=formatC(mean(covars$Fibers, na.rm=TRUE), format="f", digits=1)
  mysd=formatC(sd(covars$Fibers, na.rm=TRUE), format="f", digits=1)
  out=paste0(mymean, " (", mysd, ")")
  for (k in levels(covars$data_type)){
    mymean=formatC(mean(covars$Fibers[covars$data_type==k], na.rm=TRUE), format="f", digits=1)
    mysd=formatC(sd(covars$Fibers[covars$data_type==k], na.rm=TRUE), format="f", digits=1)
    out=c(out, paste0(mymean, " (", mysd, ")"))
  }
  mytable=rbind(mytable, out)
  rownames(mytable)[nrow(mytable)]="Fibres (g/day), Mean (SD)"
  myn = sum(is.na(covars$Fibers))
  if(myn > 0){
    myp = formatC(myn/length(covars$Fibers)*100, format = "f", digits = 1)
    out = paste0(myn, " (", myp, "%)")
    for (k in levels(covars$data_type)){
      myn = sum(is.na(covars$Fibers[covars$data_type==k]))
      myp = formatC(myn/sum(covars$data_type==k)*100, format = "f", digits = 1)
      out=c(out, paste0(myn, " (", myp, "%)"))
    }
    mytable=rbind(mytable, out)
    rownames(mytable)[nrow(mytable)]="Missing, N (%)"
  }
  # Physical activity
  mytable=rbind(mytable, rep(NA, ncol(mytable)))
  rownames(mytable)[nrow(mytable)]="Physical activity, N (%)"
  myn = sum(covars$Physical_activity=="Inactive", na.rm = T)
  myp = formatC(myn/length(covars$Physical_activity)*100, format = "f", digits = 1)
  out = paste0(myn, " (", myp, "%)")
  for (k in levels(covars$data_type)){
    myn = sum(covars$Physical_activity[covars$data_type==k]=="Inactive", na.rm = T)
    myp = formatC(myn/sum(covars$data_type==k)*100, format = "f", digits = 1)
    out=c(out, paste0(myn, " (", myp, "%)"))
  }
  mytable=rbind(mytable, out)
  rownames(mytable)[nrow(mytable)]="Inactive"
  myn = sum(covars$Physical_activity=="Moderately active", na.rm = T)
  myp = formatC(myn/length(covars$Physical_activity)*100, format = "f", digits = 1)
  out = paste0(myn, " (", myp, "%)")
  for (k in levels(covars$data_type)){
    myn = sum(covars$Physical_activity[covars$data_type==k]=="Moderately active", na.rm = T)
    myp = formatC(myn/sum(covars$data_type==k)*100, format = "f", digits = 1)
    out=c(out, paste0(myn, " (", myp, "%)"))
  }
  mytable=rbind(mytable, out)
  rownames(mytable)[nrow(mytable)]="Moderately active"
  myn = sum(covars$Physical_activity=="Active", na.rm = T)
  myp = formatC(myn/length(covars$Physical_activity)*100, format = "f", digits = 1)
  out = paste0(myn, " (", myp, "%)")
  for (k in levels(covars$data_type)){
    myn = sum(covars$Physical_activity[covars$data_type==k]=="Active", na.rm = T)
    myp = formatC(myn/sum(covars$data_type==k)*100, format = "f", digits = 1)
    out=c(out, paste0(myn, " (", myp, "%)"))
  }
  mytable=rbind(mytable, out)
  rownames(mytable)[nrow(mytable)]="Active"
  myn = sum(is.na(covars$Physical_activity))
  if(myn > 0){
    myp = formatC(myn/length(covars$Physical_activity)*100, format = "f", digits = 1)
    out = paste0(myn, " (", myp, "%)")
    for (k in levels(covars$data_type)){
      myn = sum(is.na(covars$Physical_activity[covars$data_type==k]))
      myp = formatC(myn/sum(covars$data_type==k)*100, format = "f", digits = 1)
      out=c(out, paste0(myn, " (", myp, "%)"))
    }
    mytable=rbind(mytable, out)
    rownames(mytable)[nrow(mytable)]="Missing, N (%)"
  }
  # Alcohol
  mytable=rbind(mytable, rep(NA, ncol(mytable)))
  rownames(mytable)[nrow(mytable)]="Alcohol consumption, N (%)"
  myn = sum(covars$Alcohol=="Low", na.rm = T)
  myp = formatC(myn/length(covars$Alcohol)*100, format = "f", digits = 1)
  out = paste0(myn, " (", myp, "%)")
  for (k in levels(covars$data_type)){
    myn = sum(covars$Alcohol[covars$data_type==k]=="Low", na.rm = T)
    myp = formatC(myn/sum(covars$data_type==k)*100, format = "f", digits = 1)
    out=c(out, paste0(myn, " (", myp, "%)"))
  }
  mytable=rbind(mytable, out)
  rownames(mytable)[nrow(mytable)]="Low (<0.1g/day)"
  myn = sum(covars$Alcohol=="Moderate", na.rm = T)
  myp = formatC(myn/length(covars$Alcohol)*100, format = "f", digits = 1)
  out = paste0(myn, " (", myp, "%)")
  for (k in levels(covars$data_type)){
    myn = sum(covars$Alcohol[covars$data_type==k]=="Moderate", na.rm = T)
    myp = formatC(myn/sum(covars$data_type==k)*100, format = "f", digits = 1)
    out=c(out, paste0(myn, " (", myp, "%)"))
  }
  mytable=rbind(mytable, out)
  rownames(mytable)[nrow(mytable)]="Moderate (0.1-10g/day)"
  myn = sum(covars$Alcohol=="High", na.rm = T)
  myp = formatC(myn/length(covars$Alcohol)*100, format = "f", digits = 1)
  out = paste0(myn, " (", myp, "%)")
  for (k in levels(covars$data_type)){
    myn = sum(covars$Alcohol[covars$data_type==k]=="High", na.rm = T)
    myp = formatC(myn/sum(covars$data_type==k)*100, format = "f", digits = 1)
    out=c(out, paste0(myn, " (", myp, "%)"))
  }
  mytable=rbind(mytable, out)
  rownames(mytable)[nrow(mytable)]="High (>10g/day)"
  myn = sum(is.na(covars$Alcohol))
  if(myn > 0){
    myp = formatC(myn/length(covars$Alcohol)*100, format = "f", digits = 1)
    out = paste0(myn, " (", myp, "%)")
    for (k in levels(covars$data_type)){
      myn = sum(is.na(covars$Alcohol[covars$data_type==k]))
      myp = formatC(myn/sum(covars$data_type==k)*100, format = "f", digits = 1)
      out=c(out, paste0(myn, " (", myp, "%)"))
    }
    mytable=rbind(mytable, out)
    rownames(mytable)[nrow(mytable)]="Missing, N (%)"
  }
  out = nrow(covars)
  for (k in levels(covars$data_type)){
    out=c(out, sum(covars$data_type==k))
  }
  colnames(mytable)=paste0(c("Total",levels(covars$data_type))," (N=",out,")")
  SaveExcelWithSuperscripts(cbind(rownames(mytable),mytable), paste0("../Exports/Descriptive_statistics/","Characteristics_by_","data_type",".xlsx"))
}

# ## Overview of population characteristics (related to outcome): by Country----
# { data = readRDS(paste0("../Data/","All_participants_info",".rds"))
#   covars = data[,c("Country",
#                    "BMI_cat",
#                    "SBP", "DBP",
#                    "FPG",
#                    "TC", "HDLC", "LDLC", "TG",
#                    "WC_cat",
#                    "Diabetes_medication","Hypertension_medication","Lipid_disorder_medication")]
#   mytable=NULL
#   # BMI
#   mytable=rbind(mytable, rep(NA, length(levels(covars$Country))+1))
#   rownames(mytable)[nrow(mytable)]="BMI, N (%)"
#   myn = sum(covars$BMI_cat=="Underweight", na.rm = T)
#   myp = formatC(myn/length(covars$BMI_cat)*100, format = "f", digits = 1)
#   out = paste0(myn, " (", myp, "%)")
#   for (k in levels(covars$Country)){
#     myn = sum(covars$BMI_cat[covars$Country==k]=="Underweight", na.rm = T)
#     myp = formatC(myn/sum(covars$Country==k)*100, format = "f", digits = 1)
#     out=c(out, paste0(myn, " (", myp, "%)"))
#   }
#   mytable=rbind(mytable, out)
#   rownames(mytable)[nrow(mytable)]="Underweight"
#   myn = sum(covars$BMI_cat=="Normal", na.rm = T)
#   myp = formatC(myn/length(covars$BMI_cat)*100, format = "f", digits = 1)
#   out = paste0(myn, " (", myp, "%)")
#   for (k in levels(covars$Country)){
#     myn = sum(covars$BMI_cat[covars$Country==k]=="Normal", na.rm = T)
#     myp = formatC(myn/sum(covars$Country==k)*100, format = "f", digits = 1)
#     out=c(out, paste0(myn, " (", myp, "%)"))
#   }
#   mytable=rbind(mytable, out)
#   rownames(mytable)[nrow(mytable)]="Normal"
#   myn = sum(covars$BMI_cat=="Pre-obesity", na.rm = T)
#   myp = formatC(myn/length(covars$BMI_cat)*100, format = "f", digits = 1)
#   out = paste0(myn, " (", myp, "%)")
#   for (k in levels(covars$Country)){
#     myn = sum(covars$BMI_cat[covars$Country==k]=="Pre-obesity", na.rm = T)
#     myp = formatC(myn/sum(covars$Country==k)*100, format = "f", digits = 1)
#     out=c(out, paste0(myn, " (", myp, "%)"))
#   }
#   mytable=rbind(mytable, out)
#   rownames(mytable)[nrow(mytable)]="Pre-obesity"
#   myn = sum(covars$BMI_cat=="Obesity class I", na.rm = T)
#   myp = formatC(myn/length(covars$BMI_cat)*100, format = "f", digits = 1)
#   out = paste0(myn, " (", myp, "%)")
#   for (k in levels(covars$Country)){
#     myn = sum(covars$BMI_cat[covars$Country==k]=="Obesity class I", na.rm = T)
#     myp = formatC(myn/sum(covars$Country==k)*100, format = "f", digits = 1)
#     out=c(out, paste0(myn, " (", myp, "%)"))
#   }
#   mytable=rbind(mytable, out)
#   rownames(mytable)[nrow(mytable)]="Obesity class I"
#   myn = sum(covars$BMI_cat=="Obesity class II", na.rm = T)
#   myp = formatC(myn/length(covars$BMI_cat)*100, format = "f", digits = 1)
#   out = paste0(myn, " (", myp, "%)")
#   for (k in levels(covars$Country)){
#     myn = sum(covars$BMI_cat[covars$Country==k]=="Obesity class II", na.rm = T)
#     myp = formatC(myn/sum(covars$Country==k)*100, format = "f", digits = 1)
#     out=c(out, paste0(myn, " (", myp, "%)"))
#   }
#   mytable=rbind(mytable, out)
#   rownames(mytable)[nrow(mytable)]="Obesity class II"
#   myn = sum(covars$BMI_cat=="Obesity class III", na.rm = T)
#   myp = formatC(myn/length(covars$BMI_cat)*100, format = "f", digits = 1)
#   out = paste0(myn, " (", myp, "%)")
#   for (k in levels(covars$Country)){
#     myn = sum(covars$BMI_cat[covars$Country==k]=="Obesity class III", na.rm = T)
#     myp = formatC(myn/sum(covars$Country==k)*100, format = "f", digits = 1)
#     out=c(out, paste0(myn, " (", myp, "%)"))
#   }
#   mytable=rbind(mytable, out)
#   rownames(mytable)[nrow(mytable)]="Obesity class III"
#   myn = sum(is.na(covars$BMI_cat))
#   if(myn > 0){
#     myp = formatC(myn/length(covars$BMI_cat)*100, format = "f", digits = 1)
#     out = paste0(myn, " (", myp, "%)")
#     for (k in levels(covars$Country)){
#       myn = sum(is.na(covars$BMI_cat[covars$Country==k]))
#       myp = formatC(myn/sum(covars$Country==k)*100, format = "f", digits = 1)
#       out=c(out, paste0(myn, " (", myp, "%)"))
#     }
#     mytable=rbind(mytable, out)
#     rownames(mytable)[nrow(mytable)]="Missing, N (%)"
#   }
#   # Systolic blood pressure
#   mymean=formatC(mean(covars$SBP, na.rm=TRUE), format="f", digits=1)
#   mysd=formatC(sd(covars$SBP, na.rm=TRUE), format="f", digits=1)
#   out=paste0(mymean, " (", mysd, ")")
#   for (k in levels(covars$Country)){
#     mymean=formatC(mean(covars$SBP[covars$Country==k], na.rm=TRUE), format="f", digits=1)
#     mysd=formatC(sd(covars$SBP[covars$Country==k], na.rm=TRUE), format="f", digits=1)
#     out=c(out, paste0(mymean, " (", mysd, ")"))
#   }
#   mytable=rbind(mytable, out)
#   rownames(mytable)[nrow(mytable)]="Systolic blood pressure (mmHg), Mean (SD)"
#   myn = sum(is.na(covars$SBP))
#   if(myn > 0){
#     myp = formatC(myn/length(covars$SBP)*100, format = "f", digits = 1)
#     out = paste0(myn, " (", myp, "%)")
#     for (k in levels(covars$Country)){
#       myn = sum(is.na(covars$SBP[covars$Country==k]))
#       myp = formatC(myn/sum(covars$Country==k)*100, format = "f", digits = 1)
#       out=c(out, paste0(myn, " (", myp, "%)"))
#     }
#     mytable=rbind(mytable, out)
#     rownames(mytable)[nrow(mytable)]="Missing, N (%)"
#   }
#   # Diastolic blood pressure
#   mymean=formatC(mean(covars$DBP, na.rm=TRUE), format="f", digits=1)
#   mysd=formatC(sd(covars$DBP, na.rm=TRUE), format="f", digits=1)
#   out=paste0(mymean, " (", mysd, ")")
#   for (k in levels(covars$Country)){
#     mymean=formatC(mean(covars$DBP[covars$Country==k], na.rm=TRUE), format="f", digits=1)
#     mysd=formatC(sd(covars$DBP[covars$Country==k], na.rm=TRUE), format="f", digits=1)
#     out=c(out, paste0(mymean, " (", mysd, ")"))
#   }
#   mytable=rbind(mytable, out)
#   rownames(mytable)[nrow(mytable)]="Diastolic blood pressure (mmHg), Mean (SD)"
#   myn = sum(is.na(covars$DBP))
#   if(myn > 0){
#     myp = formatC(myn/length(covars$DBP)*100, format = "f", digits = 1)
#     out = paste0(myn, " (", myp, "%)")
#     for (k in levels(covars$Country)){
#       myn = sum(is.na(covars$DBP[covars$Country==k]))
#       myp = formatC(myn/sum(covars$Country==k)*100, format = "f", digits = 1)
#       out=c(out, paste0(myn, " (", myp, "%)"))
#     }
#     mytable=rbind(mytable, out)
#     rownames(mytable)[nrow(mytable)]="Missing, N (%)"
#   }
#   # Fasting blood glucose
#   mymean=formatC(mean(covars$FPG, na.rm=TRUE), format="f", digits=1)
#   mysd=formatC(sd(covars$FPG, na.rm=TRUE), format="f", digits=1)
#   out=paste0(mymean, " (", mysd, ")")
#   for (k in levels(covars$Country)){
#     mymean=formatC(mean(covars$FPG[covars$Country==k], na.rm=TRUE), format="f", digits=1)
#     mysd=formatC(sd(covars$FPG[covars$Country==k], na.rm=TRUE), format="f", digits=1)
#     out=c(out, paste0(mymean, " (", mysd, ")"))
#   }
#   mytable=rbind(mytable, out)
#   rownames(mytable)[nrow(mytable)]="Fasting blood glucose (mg/dl), Mean (SD)"
#   myn = sum(is.na(covars$FPG))
#   if(myn > 0){
#     myp = formatC(myn/length(covars$FPG)*100, format = "f", digits = 1)
#     out = paste0(myn, " (", myp, "%)")
#     for (k in levels(covars$Country)){
#       myn = sum(is.na(covars$FPG[covars$Country==k]))
#       myp = formatC(myn/sum(covars$Country==k)*100, format = "f", digits = 1)
#       out=c(out, paste0(myn, " (", myp, "%)"))
#     }
#     mytable=rbind(mytable, out)
#     rownames(mytable)[nrow(mytable)]="Missing, N (%)"
#   }
#   # Total cholesterol
#   mymean=formatC(mean(covars$TC, na.rm=TRUE), format="f", digits=1)
#   mysd=formatC(sd(covars$TC, na.rm=TRUE), format="f", digits=1)
#   out=paste0(mymean, " (", mysd, ")")
#   for (k in levels(covars$Country)){
#     mymean=formatC(mean(covars$TC[covars$Country==k], na.rm=TRUE), format="f", digits=1)
#     mysd=formatC(sd(covars$TC[covars$Country==k], na.rm=TRUE), format="f", digits=1)
#     out=c(out, paste0(mymean, " (", mysd, ")"))
#   }
#   mytable=rbind(mytable, out)
#   rownames(mytable)[nrow(mytable)]="Total cholesterol (mg/dl), Mean (SD)"
#   myn = sum(is.na(covars$TC))
#   if(myn > 0){
#     myp = formatC(myn/length(covars$TC)*100, format = "f", digits = 1)
#     out = paste0(myn, " (", myp, "%)")
#     for (k in levels(covars$Country)){
#       myn = sum(is.na(covars$TC[covars$Country==k]))
#       myp = formatC(myn/sum(covars$Country==k)*100, format = "f", digits = 1)
#       out=c(out, paste0(myn, " (", myp, "%)"))
#     }
#     mytable=rbind(mytable, out)
#     rownames(mytable)[nrow(mytable)]="Missing, N (%)"
#   }
#   # HDL cholesterol
#   mymean=formatC(mean(covars$HDLC, na.rm=TRUE), format="f", digits=1)
#   mysd=formatC(sd(covars$HDLC, na.rm=TRUE), format="f", digits=1)
#   out=paste0(mymean, " (", mysd, ")")
#   for (k in levels(covars$Country)){
#     mymean=formatC(mean(covars$HDLC[covars$Country==k], na.rm=TRUE), format="f", digits=1)
#     mysd=formatC(sd(covars$HDLC[covars$Country==k], na.rm=TRUE), format="f", digits=1)
#     out=c(out, paste0(mymean, " (", mysd, ")"))
#   }
#   mytable=rbind(mytable, out)
#   rownames(mytable)[nrow(mytable)]="HDL cholesterol (mg/dl), Mean (SD)"
#   myn = sum(is.na(covars$HDLC))
#   if(myn > 0){
#     myp = formatC(myn/length(covars$HDLC)*100, format = "f", digits = 1)
#     out = paste0(myn, " (", myp, "%)")
#     for (k in levels(covars$Country)){
#       myn = sum(is.na(covars$HDLC[covars$Country==k]))
#       myp = formatC(myn/sum(covars$Country==k)*100, format = "f", digits = 1)
#       out=c(out, paste0(myn, " (", myp, "%)"))
#     }
#     mytable=rbind(mytable, out)
#     rownames(mytable)[nrow(mytable)]="Missing, N (%)"
#   }
#   # LDL cholesterol
#   mymean=formatC(mean(covars$LDLC, na.rm=TRUE), format="f", digits=1)
#   mysd=formatC(sd(covars$LDLC, na.rm=TRUE), format="f", digits=1)
#   out=paste0(mymean, " (", mysd, ")")
#   for (k in levels(covars$Country)){
#     mymean=formatC(mean(covars$LDLC[covars$Country==k], na.rm=TRUE), format="f", digits=1)
#     mysd=formatC(sd(covars$LDLC[covars$Country==k], na.rm=TRUE), format="f", digits=1)
#     out=c(out, paste0(mymean, " (", mysd, ")"))
#   }
#   mytable=rbind(mytable, out)
#   rownames(mytable)[nrow(mytable)]="LDL cholesterol (mg/dl), Mean (SD)"
#   myn = sum(is.na(covars$LDLC))
#   if(myn > 0){
#     myp = formatC(myn/length(covars$LDLC)*100, format = "f", digits = 1)
#     out = paste0(myn, " (", myp, "%)")
#     for (k in levels(covars$Country)){
#       myn = sum(is.na(covars$LDLC[covars$Country==k]))
#       myp = formatC(myn/sum(covars$Country==k)*100, format = "f", digits = 1)
#       out=c(out, paste0(myn, " (", myp, "%)"))
#     }
#     mytable=rbind(mytable, out)
#     rownames(mytable)[nrow(mytable)]="Missing, N (%)"
#   }
#   # Waist circumference
#   mytable=rbind(mytable, rep(NA, ncol(mytable)))
#   rownames(mytable)[nrow(mytable)]="Waist circumference, N (%)"
#   myn = sum(covars$WC_cat=="High", na.rm = T)
#   myp = formatC(myn/length(covars$WC_cat)*100, format = "f", digits = 1)
#   out = paste0(myn, " (", myp, "%)")
#   for (k in levels(covars$Country)){
#     myn = sum(covars$WC_cat[covars$Country==k]=="High", na.rm = T)
#     myp = formatC(myn/sum(covars$Country==k)*100, format = "f", digits = 1)
#     out=c(out, paste0(myn, " (", myp, "%)"))
#   }
#   mytable=rbind(mytable, out)
#   rownames(mytable)[nrow(mytable)]="High"
#   myn = sum(covars$WC_cat=="Normal", na.rm = T)
#   myp = formatC(myn/length(covars$WC_cat)*100, format = "f", digits = 1)
#   out = paste0(myn, " (", myp, "%)")
#   for (k in levels(covars$Country)){
#     myn = sum(covars$WC_cat[covars$Country==k]=="Normal", na.rm = T)
#     myp = formatC(myn/sum(covars$Country==k)*100, format = "f", digits = 1)
#     out=c(out, paste0(myn, " (", myp, "%)"))
#   }
#   mytable=rbind(mytable, out)
#   rownames(mytable)[nrow(mytable)]="Normal"
#   myn = sum(is.na(covars$WC_cat))
#   if(myn > 0){
#     myp = formatC(myn/length(covars$WC_cat)*100, format = "f", digits = 1)
#     out = paste0(myn, " (", myp, "%)")
#     for (k in levels(covars$Country)){
#       myn = sum(is.na(covars$WC_cat[covars$Country==k]))
#       myp = formatC(myn/sum(covars$Country==k)*100, format = "f", digits = 1)
#       out=c(out, paste0(myn, " (", myp, "%)"))
#     }
#     mytable=rbind(mytable, out)
#     rownames(mytable)[nrow(mytable)]="Missing, N (%)"
#   }
#   # Triglyceride
#   mytable=rbind(mytable, rep(NA, ncol(mytable)))
#   rownames(mytable)[nrow(mytable)]="Triglyceride, N (%)"
#   myn = sum(covars$TG>=150, na.rm = T)
#   myp = formatC(myn/length(covars$TG)*100, format = "f", digits = 1)
#   out = paste0(myn, " (", myp, "%)")
#   for (k in levels(covars$Country)){
#     myn = sum(covars$TG[covars$Country==k]>=150, na.rm = T)
#     myp = formatC(myn/sum(covars$Country==k)*100, format = "f", digits = 1)
#     out=c(out, paste0(myn, " (", myp, "%)"))
#   }
#   mytable=rbind(mytable, out)
#   rownames(mytable)[nrow(mytable)]=">=150mg/dl"
#   myn = sum(covars$TG<150, na.rm = T)
#   myp = formatC(myn/length(covars$TG)*100, format = "f", digits = 1)
#   out = paste0(myn, " (", myp, "%)")
#   for (k in levels(covars$Country)){
#     myn = sum(covars$TG[covars$Country==k]<150, na.rm = T)
#     myp = formatC(myn/sum(covars$Country==k)*100, format = "f", digits = 1)
#     out=c(out, paste0(myn, " (", myp, "%)"))
#   }
#   mytable=rbind(mytable, out)
#   rownames(mytable)[nrow(mytable)]="<150mg/dl"
#   myn = sum(is.na(covars$TG))
#   if(myn > 0){
#     myp = formatC(myn/length(covars$TG)*100, format = "f", digits = 1)
#     out = paste0(myn, " (", myp, "%)")
#     for (k in levels(covars$Country)){
#       myn = sum(is.na(covars$WC_cat[covars$Country==k]))
#       myp = formatC(myn/sum(covars$Country==k)*100, format = "f", digits = 1)
#       out=c(out, paste0(myn, " (", myp, "%)"))
#     }
#     mytable=rbind(mytable, out)
#     rownames(mytable)[nrow(mytable)]="Missing, N (%)"
#   }
#   # Medication
#   mytable=rbind(mytable, rep(NA, ncol(mytable)))
#   rownames(mytable)[nrow(mytable)]="Type of medication, N (%)"
#   myn = sum(covars$Diabetes_medication=="Yes", na.rm = T)
#   myp = formatC(myn/length(covars$Diabetes_medication)*100, format = "f", digits = 1)
#   out = paste0(myn, " (", myp, "%)")
#   for (k in levels(covars$Country)){
#     myn = sum(covars$Diabetes_medication[covars$Country==k]=="Yes", na.rm = T)
#     myp = formatC(myn/sum(covars$Country==k)*100, format = "f", digits = 1)
#     out=c(out, paste0(myn, " (", myp, "%)"))
#   }
#   mytable=rbind(mytable, out)
#   rownames(mytable)[nrow(mytable)]="Diabetes"
#   myn = sum(is.na(covars$Diabetes_medication))
#   if(myn > 0){
#     myp = formatC(myn/length(covars$Diabetes_medication)*100, format = "f", digits = 1)
#     out = paste0(myn, " (", myp, "%)")
#     for (k in levels(covars$Country)){
#       myn = sum(is.na(covars$WC_cat[covars$Country==k]))
#       myp = formatC(myn/sum(covars$Country==k)*100, format = "f", digits = 1)
#       out=c(out, paste0(myn, " (", myp, "%)"))
#     }
#     mytable=rbind(mytable, out)
#     rownames(mytable)[nrow(mytable)]="Diabetes medication: Missing, N (%)"
#   }
#   myn = sum(covars$Hypertension_medication=="Yes", na.rm = T)
#   myp = formatC(myn/length(covars$Hypertension_medication)*100, format = "f", digits = 1)
#   out = paste0(myn, " (", myp, "%)")
#   for (k in levels(covars$Country)){
#     myn = sum(covars$Hypertension_medication[covars$Country==k]=="Yes", na.rm = T)
#     myp = formatC(myn/sum(covars$Country==k)*100, format = "f", digits = 1)
#     out=c(out, paste0(myn, " (", myp, "%)"))
#   }
#   mytable=rbind(mytable, out)
#   rownames(mytable)[nrow(mytable)]="Hypertension"
#   myn = sum(is.na(covars$Hypertension_medication))
#   if(myn > 0){
#     myp = formatC(myn/length(covars$Hypertension_medication)*100, format = "f", digits = 1)
#     out = paste0(myn, " (", myp, "%)")
#     for (k in levels(covars$Country)){
#       myn = sum(is.na(covars$WC_cat[covars$Country==k]))
#       myp = formatC(myn/sum(covars$Country==k)*100, format = "f", digits = 1)
#       out=c(out, paste0(myn, " (", myp, "%)"))
#     }
#     mytable=rbind(mytable, out)
#     rownames(mytable)[nrow(mytable)]="Hypertension medication: Missing, N (%)"
#   }
#   myn = sum(covars$Lipid_disorder_medication=="Yes", na.rm = T)
#   myp = formatC(myn/length(covars$Lipid_disorder_medication)*100, format = "f", digits = 1)
#   out = paste0(myn, " (", myp, "%)")
#   for (k in levels(covars$Country)){
#     myn = sum(covars$Lipid_disorder_medication[covars$Country==k]=="Yes", na.rm = T)
#     myp = formatC(myn/sum(covars$Country==k)*100, format = "f", digits = 1)
#     out=c(out, paste0(myn, " (", myp, "%)"))
#   }
#   mytable=rbind(mytable, out)
#   rownames(mytable)[nrow(mytable)]="Lipid disorder"
#   myn = sum(is.na(covars$Lipid_disorder_medication))
#   if(myn > 0){
#     myp = formatC(myn/length(covars$Lipid_disorder_medication)*100, format = "f", digits = 1)
#     out = paste0(myn, " (", myp, "%)")
#     for (k in levels(covars$Country)){
#       myn = sum(is.na(covars$WC_cat[covars$Country==k]))
#       myp = formatC(myn/sum(covars$Country==k)*100, format = "f", digits = 1)
#       out=c(out, paste0(myn, " (", myp, "%)"))
#     }
#     mytable=rbind(mytable, out)
#     rownames(mytable)[nrow(mytable)]="Lipid disorder: Missing, N (%)"
#   }
#   out = nrow(covars)
#   for (k in levels(covars$Country)){
#     out=c(out, sum(covars$Country==k))
#   }
#   colnames(mytable)=paste0(c("Total",levels(covars$Country))," (N=",out,")")
#   SaveExcelWithSuperscripts(cbind(rownames(mytable),mytable), paste0("../Exports/Descriptive_statistics/","Characteristics_outcome_by_","Country",".xlsx"))
# }

## Overview of population characteristics (related to outcome): by Gender----
{ data = readRDS(paste0("../Data/","All_participants_info",".rds"))
covars = data[,c("Gender",
                 "BMI_cat",
                 "SBP", "DBP",
                 "FPG",
                 "TC", "HDLC", "LDLC", "TG",
                 "WC_cat",
                 "Diabetes_medication","Hypertension_medication","Lipid_disorder_medication")]
mytable=NULL
# BMI
mytable=rbind(mytable, rep(NA, length(levels(covars$Gender))+1))
rownames(mytable)[nrow(mytable)]="BMI, N (%)"
myn = sum(covars$BMI_cat=="Underweight", na.rm = T)
myp = formatC(myn/length(covars$BMI_cat)*100, format = "f", digits = 1)
out = paste0(myn, " (", myp, "%)")
for (k in levels(covars$Gender)){
  myn = sum(covars$BMI_cat[covars$Gender==k]=="Underweight", na.rm = T)
  myp = formatC(myn/sum(covars$Gender==k)*100, format = "f", digits = 1)
  out=c(out, paste0(myn, " (", myp, "%)"))
}
mytable=rbind(mytable, out)
rownames(mytable)[nrow(mytable)]="Underweight"
myn = sum(covars$BMI_cat=="Normal", na.rm = T)
myp = formatC(myn/length(covars$BMI_cat)*100, format = "f", digits = 1)
out = paste0(myn, " (", myp, "%)")
for (k in levels(covars$Gender)){
  myn = sum(covars$BMI_cat[covars$Gender==k]=="Normal", na.rm = T)
  myp = formatC(myn/sum(covars$Gender==k)*100, format = "f", digits = 1)
  out=c(out, paste0(myn, " (", myp, "%)"))
}
mytable=rbind(mytable, out)
rownames(mytable)[nrow(mytable)]="Normal"
myn = sum(covars$BMI_cat=="Pre-obesity", na.rm = T)
myp = formatC(myn/length(covars$BMI_cat)*100, format = "f", digits = 1)
out = paste0(myn, " (", myp, "%)")
for (k in levels(covars$Gender)){
  myn = sum(covars$BMI_cat[covars$Gender==k]=="Pre-obesity", na.rm = T)
  myp = formatC(myn/sum(covars$Gender==k)*100, format = "f", digits = 1)
  out=c(out, paste0(myn, " (", myp, "%)"))
}
mytable=rbind(mytable, out)
rownames(mytable)[nrow(mytable)]="Pre-obesity"
myn = sum(covars$BMI_cat=="Obesity class I", na.rm = T)
myp = formatC(myn/length(covars$BMI_cat)*100, format = "f", digits = 1)
out = paste0(myn, " (", myp, "%)")
for (k in levels(covars$Gender)){
  myn = sum(covars$BMI_cat[covars$Gender==k]=="Obesity class I", na.rm = T)
  myp = formatC(myn/sum(covars$Gender==k)*100, format = "f", digits = 1)
  out=c(out, paste0(myn, " (", myp, "%)"))
}
mytable=rbind(mytable, out)
rownames(mytable)[nrow(mytable)]="Obesity class I"
myn = sum(covars$BMI_cat=="Obesity class II", na.rm = T)
myp = formatC(myn/length(covars$BMI_cat)*100, format = "f", digits = 1)
out = paste0(myn, " (", myp, "%)")
for (k in levels(covars$Gender)){
  myn = sum(covars$BMI_cat[covars$Gender==k]=="Obesity class II", na.rm = T)
  myp = formatC(myn/sum(covars$Gender==k)*100, format = "f", digits = 1)
  out=c(out, paste0(myn, " (", myp, "%)"))
}
mytable=rbind(mytable, out)
rownames(mytable)[nrow(mytable)]="Obesity class II"
myn = sum(covars$BMI_cat=="Obesity class III", na.rm = T)
myp = formatC(myn/length(covars$BMI_cat)*100, format = "f", digits = 1)
out = paste0(myn, " (", myp, "%)")
for (k in levels(covars$Gender)){
  myn = sum(covars$BMI_cat[covars$Gender==k]=="Obesity class III", na.rm = T)
  myp = formatC(myn/sum(covars$Gender==k)*100, format = "f", digits = 1)
  out=c(out, paste0(myn, " (", myp, "%)"))
}
mytable=rbind(mytable, out)
rownames(mytable)[nrow(mytable)]="Obesity class III"
myn = sum(is.na(covars$BMI_cat))
if(myn > 0){
  myp = formatC(myn/length(covars$BMI_cat)*100, format = "f", digits = 1)
  out = paste0(myn, " (", myp, "%)")
  for (k in levels(covars$Gender)){
    myn = sum(is.na(covars$BMI_cat[covars$Gender==k]))
    myp = formatC(myn/sum(covars$Gender==k)*100, format = "f", digits = 1)
    out=c(out, paste0(myn, " (", myp, "%)"))
  }
  mytable=rbind(mytable, out)
  rownames(mytable)[nrow(mytable)]="Missing, N (%)"
}
# Systolic blood pressure
mymean=formatC(mean(covars$SBP, na.rm=TRUE), format="f", digits=1)
mysd=formatC(sd(covars$SBP, na.rm=TRUE), format="f", digits=1)
out=paste0(mymean, " (", mysd, ")")
for (k in levels(covars$Gender)){
  mymean=formatC(mean(covars$SBP[covars$Gender==k], na.rm=TRUE), format="f", digits=1)
  mysd=formatC(sd(covars$SBP[covars$Gender==k], na.rm=TRUE), format="f", digits=1)
  out=c(out, paste0(mymean, " (", mysd, ")"))
}
mytable=rbind(mytable, out)
rownames(mytable)[nrow(mytable)]="Systolic blood pressure (mmHg), Mean (SD)"
myn = sum(is.na(covars$SBP))
if(myn > 0){
  myp = formatC(myn/length(covars$SBP)*100, format = "f", digits = 1)
  out = paste0(myn, " (", myp, "%)")
  for (k in levels(covars$Gender)){
    myn = sum(is.na(covars$SBP[covars$Gender==k]))
    myp = formatC(myn/sum(covars$Gender==k)*100, format = "f", digits = 1)
    out=c(out, paste0(myn, " (", myp, "%)"))
  }
  mytable=rbind(mytable, out)
  rownames(mytable)[nrow(mytable)]="Missing, N (%)"
}
# Diastolic blood pressure
mymean=formatC(mean(covars$DBP, na.rm=TRUE), format="f", digits=1)
mysd=formatC(sd(covars$DBP, na.rm=TRUE), format="f", digits=1)
out=paste0(mymean, " (", mysd, ")")
for (k in levels(covars$Gender)){
  mymean=formatC(mean(covars$DBP[covars$Gender==k], na.rm=TRUE), format="f", digits=1)
  mysd=formatC(sd(covars$DBP[covars$Gender==k], na.rm=TRUE), format="f", digits=1)
  out=c(out, paste0(mymean, " (", mysd, ")"))
}
mytable=rbind(mytable, out)
rownames(mytable)[nrow(mytable)]="Diastolic blood pressure (mmHg), Mean (SD)"
myn = sum(is.na(covars$DBP))
if(myn > 0){
  myp = formatC(myn/length(covars$DBP)*100, format = "f", digits = 1)
  out = paste0(myn, " (", myp, "%)")
  for (k in levels(covars$Gender)){
    myn = sum(is.na(covars$DBP[covars$Gender==k]))
    myp = formatC(myn/sum(covars$Gender==k)*100, format = "f", digits = 1)
    out=c(out, paste0(myn, " (", myp, "%)"))
  }
  mytable=rbind(mytable, out)
  rownames(mytable)[nrow(mytable)]="Missing, N (%)"
}
# Fasting blood glucose
mymean=formatC(mean(covars$FPG, na.rm=TRUE), format="f", digits=1)
mysd=formatC(sd(covars$FPG, na.rm=TRUE), format="f", digits=1)
out=paste0(mymean, " (", mysd, ")")
for (k in levels(covars$Gender)){
  mymean=formatC(mean(covars$FPG[covars$Gender==k], na.rm=TRUE), format="f", digits=1)
  mysd=formatC(sd(covars$FPG[covars$Gender==k], na.rm=TRUE), format="f", digits=1)
  out=c(out, paste0(mymean, " (", mysd, ")"))
}
mytable=rbind(mytable, out)
rownames(mytable)[nrow(mytable)]="Fasting blood glucose (mg/dl), Mean (SD)"
myn = sum(is.na(covars$FPG))
if(myn > 0){
  myp = formatC(myn/length(covars$FPG)*100, format = "f", digits = 1)
  out = paste0(myn, " (", myp, "%)")
  for (k in levels(covars$Gender)){
    myn = sum(is.na(covars$FPG[covars$Gender==k]))
    myp = formatC(myn/sum(covars$Gender==k)*100, format = "f", digits = 1)
    out=c(out, paste0(myn, " (", myp, "%)"))
  }
  mytable=rbind(mytable, out)
  rownames(mytable)[nrow(mytable)]="Missing, N (%)"
}
# Total cholesterol
mymean=formatC(mean(covars$TC, na.rm=TRUE), format="f", digits=1)
mysd=formatC(sd(covars$TC, na.rm=TRUE), format="f", digits=1)
out=paste0(mymean, " (", mysd, ")")
for (k in levels(covars$Gender)){
  mymean=formatC(mean(covars$TC[covars$Gender==k], na.rm=TRUE), format="f", digits=1)
  mysd=formatC(sd(covars$TC[covars$Gender==k], na.rm=TRUE), format="f", digits=1)
  out=c(out, paste0(mymean, " (", mysd, ")"))
}
mytable=rbind(mytable, out)
rownames(mytable)[nrow(mytable)]="Total cholesterol (mg/dl), Mean (SD)"
myn = sum(is.na(covars$TC))
if(myn > 0){
  myp = formatC(myn/length(covars$TC)*100, format = "f", digits = 1)
  out = paste0(myn, " (", myp, "%)")
  for (k in levels(covars$Gender)){
    myn = sum(is.na(covars$TC[covars$Gender==k]))
    myp = formatC(myn/sum(covars$Gender==k)*100, format = "f", digits = 1)
    out=c(out, paste0(myn, " (", myp, "%)"))
  }
  mytable=rbind(mytable, out)
  rownames(mytable)[nrow(mytable)]="Missing, N (%)"
}
# HDL cholesterol
mymean=formatC(mean(covars$HDLC, na.rm=TRUE), format="f", digits=1)
mysd=formatC(sd(covars$HDLC, na.rm=TRUE), format="f", digits=1)
out=paste0(mymean, " (", mysd, ")")
for (k in levels(covars$Gender)){
  mymean=formatC(mean(covars$HDLC[covars$Gender==k], na.rm=TRUE), format="f", digits=1)
  mysd=formatC(sd(covars$HDLC[covars$Gender==k], na.rm=TRUE), format="f", digits=1)
  out=c(out, paste0(mymean, " (", mysd, ")"))
}
mytable=rbind(mytable, out)
rownames(mytable)[nrow(mytable)]="HDL cholesterol (mg/dl), Mean (SD)"
myn = sum(is.na(covars$HDLC))
if(myn > 0){
  myp = formatC(myn/length(covars$HDLC)*100, format = "f", digits = 1)
  out = paste0(myn, " (", myp, "%)")
  for (k in levels(covars$Gender)){
    myn = sum(is.na(covars$HDLC[covars$Gender==k]))
    myp = formatC(myn/sum(covars$Gender==k)*100, format = "f", digits = 1)
    out=c(out, paste0(myn, " (", myp, "%)"))
  }
  mytable=rbind(mytable, out)
  rownames(mytable)[nrow(mytable)]="Missing, N (%)"
}
# LDL cholesterol
mymean=formatC(mean(covars$LDLC, na.rm=TRUE), format="f", digits=1)
mysd=formatC(sd(covars$LDLC, na.rm=TRUE), format="f", digits=1)
out=paste0(mymean, " (", mysd, ")")
for (k in levels(covars$Gender)){
  mymean=formatC(mean(covars$LDLC[covars$Gender==k], na.rm=TRUE), format="f", digits=1)
  mysd=formatC(sd(covars$LDLC[covars$Gender==k], na.rm=TRUE), format="f", digits=1)
  out=c(out, paste0(mymean, " (", mysd, ")"))
}
mytable=rbind(mytable, out)
rownames(mytable)[nrow(mytable)]="LDL cholesterol (mg/dl), Mean (SD)"
myn = sum(is.na(covars$LDLC))
if(myn > 0){
  myp = formatC(myn/length(covars$LDLC)*100, format = "f", digits = 1)
  out = paste0(myn, " (", myp, "%)")
  for (k in levels(covars$Gender)){
    myn = sum(is.na(covars$LDLC[covars$Gender==k]))
    myp = formatC(myn/sum(covars$Gender==k)*100, format = "f", digits = 1)
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
for (k in levels(covars$Gender)){
  myn = sum(covars$WC_cat[covars$Gender==k]=="High", na.rm = T)
  myp = formatC(myn/sum(covars$Gender==k)*100, format = "f", digits = 1)
  out=c(out, paste0(myn, " (", myp, "%)"))
}
mytable=rbind(mytable, out)
rownames(mytable)[nrow(mytable)]="High"
myn = sum(covars$WC_cat=="Normal", na.rm = T)
myp = formatC(myn/length(covars$WC_cat)*100, format = "f", digits = 1)
out = paste0(myn, " (", myp, "%)")
for (k in levels(covars$Gender)){
  myn = sum(covars$WC_cat[covars$Gender==k]=="Normal", na.rm = T)
  myp = formatC(myn/sum(covars$Gender==k)*100, format = "f", digits = 1)
  out=c(out, paste0(myn, " (", myp, "%)"))
}
mytable=rbind(mytable, out)
rownames(mytable)[nrow(mytable)]="Normal"
myn = sum(is.na(covars$WC_cat))
if(myn > 0){
  myp = formatC(myn/length(covars$WC_cat)*100, format = "f", digits = 1)
  out = paste0(myn, " (", myp, "%)")
  for (k in levels(covars$Gender)){
    myn = sum(is.na(covars$WC_cat[covars$Gender==k]))
    myp = formatC(myn/sum(covars$Gender==k)*100, format = "f", digits = 1)
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
for (k in levels(covars$Gender)){
  myn = sum(covars$TG[covars$Gender==k]>=150, na.rm = T)
  myp = formatC(myn/sum(covars$Gender==k)*100, format = "f", digits = 1)
  out=c(out, paste0(myn, " (", myp, "%)"))
}
mytable=rbind(mytable, out)
rownames(mytable)[nrow(mytable)]=">=150mg/dl"
myn = sum(covars$TG<150, na.rm = T)
myp = formatC(myn/length(covars$TG)*100, format = "f", digits = 1)
out = paste0(myn, " (", myp, "%)")
for (k in levels(covars$Gender)){
  myn = sum(covars$TG[covars$Gender==k]<150, na.rm = T)
  myp = formatC(myn/sum(covars$Gender==k)*100, format = "f", digits = 1)
  out=c(out, paste0(myn, " (", myp, "%)"))
}
mytable=rbind(mytable, out)
rownames(mytable)[nrow(mytable)]="<150mg/dl"
myn = sum(is.na(covars$TG))
if(myn > 0){
  myp = formatC(myn/length(covars$TG)*100, format = "f", digits = 1)
  out = paste0(myn, " (", myp, "%)")
  for (k in levels(covars$Gender)){
    myn = sum(is.na(covars$WC_cat[covars$Gender==k]))
    myp = formatC(myn/sum(covars$Gender==k)*100, format = "f", digits = 1)
    out=c(out, paste0(myn, " (", myp, "%)"))
  }
  mytable=rbind(mytable, out)
  rownames(mytable)[nrow(mytable)]="Missing, N (%)"
}
# Medication
mytable=rbind(mytable, rep(NA, ncol(mytable)))
rownames(mytable)[nrow(mytable)]="Type of medication, N (%)"
myn = sum(covars$Diabetes_medication=="Yes", na.rm = T)
myp = formatC(myn/length(covars$Diabetes_medication)*100, format = "f", digits = 1)
out = paste0(myn, " (", myp, "%)")
for (k in levels(covars$Gender)){
  myn = sum(covars$Diabetes_medication[covars$Gender==k]=="Yes", na.rm = T)
  myp = formatC(myn/sum(covars$Gender==k)*100, format = "f", digits = 1)
  out=c(out, paste0(myn, " (", myp, "%)"))
}
mytable=rbind(mytable, out)
rownames(mytable)[nrow(mytable)]="Diabetes"
myn = sum(is.na(covars$Diabetes_medication))
if(myn > 0){
  myp = formatC(myn/length(covars$Diabetes_medication)*100, format = "f", digits = 1)
  out = paste0(myn, " (", myp, "%)")
  for (k in levels(covars$Gender)){
    myn = sum(is.na(covars$WC_cat[covars$Gender==k]))
    myp = formatC(myn/sum(covars$Gender==k)*100, format = "f", digits = 1)
    out=c(out, paste0(myn, " (", myp, "%)"))
  }
  mytable=rbind(mytable, out)
  rownames(mytable)[nrow(mytable)]="Diabetes medication: Missing, N (%)"
}
myn = sum(covars$Hypertension_medication=="Yes", na.rm = T)
myp = formatC(myn/length(covars$Hypertension_medication)*100, format = "f", digits = 1)
out = paste0(myn, " (", myp, "%)")
for (k in levels(covars$Gender)){
  myn = sum(covars$Hypertension_medication[covars$Gender==k]=="Yes", na.rm = T)
  myp = formatC(myn/sum(covars$Gender==k)*100, format = "f", digits = 1)
  out=c(out, paste0(myn, " (", myp, "%)"))
}
mytable=rbind(mytable, out)
rownames(mytable)[nrow(mytable)]="Hypertension"
myn = sum(is.na(covars$Hypertension_medication))
if(myn > 0){
  myp = formatC(myn/length(covars$Hypertension_medication)*100, format = "f", digits = 1)
  out = paste0(myn, " (", myp, "%)")
  for (k in levels(covars$Gender)){
    myn = sum(is.na(covars$WC_cat[covars$Gender==k]))
    myp = formatC(myn/sum(covars$Gender==k)*100, format = "f", digits = 1)
    out=c(out, paste0(myn, " (", myp, "%)"))
  }
  mytable=rbind(mytable, out)
  rownames(mytable)[nrow(mytable)]="Hypertension medication: Missing, N (%)"
}
myn = sum(covars$Lipid_disorder_medication=="Yes", na.rm = T)
myp = formatC(myn/length(covars$Lipid_disorder_medication)*100, format = "f", digits = 1)
out = paste0(myn, " (", myp, "%)")
for (k in levels(covars$Gender)){
  myn = sum(covars$Lipid_disorder_medication[covars$Gender==k]=="Yes", na.rm = T)
  myp = formatC(myn/sum(covars$Gender==k)*100, format = "f", digits = 1)
  out=c(out, paste0(myn, " (", myp, "%)"))
}
mytable=rbind(mytable, out)
rownames(mytable)[nrow(mytable)]="Lipid disorder"
myn = sum(is.na(covars$Lipid_disorder_medication))
if(myn > 0){
  myp = formatC(myn/length(covars$Lipid_disorder_medication)*100, format = "f", digits = 1)
  out = paste0(myn, " (", myp, "%)")
  for (k in levels(covars$Gender)){
    myn = sum(is.na(covars$WC_cat[covars$Gender==k]))
    myp = formatC(myn/sum(covars$Gender==k)*100, format = "f", digits = 1)
    out=c(out, paste0(myn, " (", myp, "%)"))
  }
  mytable=rbind(mytable, out)
  rownames(mytable)[nrow(mytable)]="Lipid disorder: Missing, N (%)"
}
out = nrow(covars)
for (k in levels(covars$Gender)){
  out=c(out, sum(covars$Gender==k))
}
colnames(mytable)=paste0(c("Total",levels(covars$Gender))," (N=",out,")")
SaveExcelWithSuperscripts(cbind(rownames(mytable),mytable), paste0("../Exports/Descriptive_statistics/","Characteristics_outcome_by_","Gender",".xlsx"))
}

## Overview of population characteristics (related to outcome): by data_type----
{ data = readRDS(paste0("../Data/","All_participants_info",".rds"))
covars = data[,c("data_type",
                 "BMI_cat",
                 "SBP", "DBP",
                 "FPG",
                 "TC", "HDLC", "LDLC", "TG",
                 "WC_cat",
                 "Diabetes_medication","Hypertension_medication","Lipid_disorder_medication")]
mytable=NULL
# BMI
mytable=rbind(mytable, rep(NA, length(levels(covars$data_type))+1))
rownames(mytable)[nrow(mytable)]="BMI, N (%)"
myn = sum(covars$BMI_cat=="Underweight", na.rm = T)
myp = formatC(myn/length(covars$BMI_cat)*100, format = "f", digits = 1)
out = paste0(myn, " (", myp, "%)")
for (k in levels(covars$data_type)){
  myn = sum(covars$BMI_cat[covars$data_type==k]=="Underweight", na.rm = T)
  myp = formatC(myn/sum(covars$data_type==k)*100, format = "f", digits = 1)
  out=c(out, paste0(myn, " (", myp, "%)"))
}
mytable=rbind(mytable, out)
rownames(mytable)[nrow(mytable)]="Underweight"
myn = sum(covars$BMI_cat=="Normal", na.rm = T)
myp = formatC(myn/length(covars$BMI_cat)*100, format = "f", digits = 1)
out = paste0(myn, " (", myp, "%)")
for (k in levels(covars$data_type)){
  myn = sum(covars$BMI_cat[covars$data_type==k]=="Normal", na.rm = T)
  myp = formatC(myn/sum(covars$data_type==k)*100, format = "f", digits = 1)
  out=c(out, paste0(myn, " (", myp, "%)"))
}
mytable=rbind(mytable, out)
rownames(mytable)[nrow(mytable)]="Normal"
myn = sum(covars$BMI_cat=="Pre-obesity", na.rm = T)
myp = formatC(myn/length(covars$BMI_cat)*100, format = "f", digits = 1)
out = paste0(myn, " (", myp, "%)")
for (k in levels(covars$data_type)){
  myn = sum(covars$BMI_cat[covars$data_type==k]=="Pre-obesity", na.rm = T)
  myp = formatC(myn/sum(covars$data_type==k)*100, format = "f", digits = 1)
  out=c(out, paste0(myn, " (", myp, "%)"))
}
mytable=rbind(mytable, out)
rownames(mytable)[nrow(mytable)]="Pre-obesity"
myn = sum(covars$BMI_cat=="Obesity class I", na.rm = T)
myp = formatC(myn/length(covars$BMI_cat)*100, format = "f", digits = 1)
out = paste0(myn, " (", myp, "%)")
for (k in levels(covars$data_type)){
  myn = sum(covars$BMI_cat[covars$data_type==k]=="Obesity class I", na.rm = T)
  myp = formatC(myn/sum(covars$data_type==k)*100, format = "f", digits = 1)
  out=c(out, paste0(myn, " (", myp, "%)"))
}
mytable=rbind(mytable, out)
rownames(mytable)[nrow(mytable)]="Obesity class I"
myn = sum(covars$BMI_cat=="Obesity class II", na.rm = T)
myp = formatC(myn/length(covars$BMI_cat)*100, format = "f", digits = 1)
out = paste0(myn, " (", myp, "%)")
for (k in levels(covars$data_type)){
  myn = sum(covars$BMI_cat[covars$data_type==k]=="Obesity class II", na.rm = T)
  myp = formatC(myn/sum(covars$data_type==k)*100, format = "f", digits = 1)
  out=c(out, paste0(myn, " (", myp, "%)"))
}
mytable=rbind(mytable, out)
rownames(mytable)[nrow(mytable)]="Obesity class II"
myn = sum(covars$BMI_cat=="Obesity class III", na.rm = T)
myp = formatC(myn/length(covars$BMI_cat)*100, format = "f", digits = 1)
out = paste0(myn, " (", myp, "%)")
for (k in levels(covars$data_type)){
  myn = sum(covars$BMI_cat[covars$data_type==k]=="Obesity class III", na.rm = T)
  myp = formatC(myn/sum(covars$data_type==k)*100, format = "f", digits = 1)
  out=c(out, paste0(myn, " (", myp, "%)"))
}
mytable=rbind(mytable, out)
rownames(mytable)[nrow(mytable)]="Obesity class III"
myn = sum(is.na(covars$BMI_cat))
if(myn > 0){
  myp = formatC(myn/length(covars$BMI_cat)*100, format = "f", digits = 1)
  out = paste0(myn, " (", myp, "%)")
  for (k in levels(covars$data_type)){
    myn = sum(is.na(covars$BMI_cat[covars$data_type==k]))
    myp = formatC(myn/sum(covars$data_type==k)*100, format = "f", digits = 1)
    out=c(out, paste0(myn, " (", myp, "%)"))
  }
  mytable=rbind(mytable, out)
  rownames(mytable)[nrow(mytable)]="Missing, N (%)"
}
# Systolic blood pressure
mymean=formatC(mean(covars$SBP, na.rm=TRUE), format="f", digits=1)
mysd=formatC(sd(covars$SBP, na.rm=TRUE), format="f", digits=1)
out=paste0(mymean, " (", mysd, ")")
for (k in levels(covars$data_type)){
  mymean=formatC(mean(covars$SBP[covars$data_type==k], na.rm=TRUE), format="f", digits=1)
  mysd=formatC(sd(covars$SBP[covars$data_type==k], na.rm=TRUE), format="f", digits=1)
  out=c(out, paste0(mymean, " (", mysd, ")"))
}
mytable=rbind(mytable, out)
rownames(mytable)[nrow(mytable)]="Systolic blood pressure (mmHg), Mean (SD)"
myn = sum(is.na(covars$SBP))
if(myn > 0){
  myp = formatC(myn/length(covars$SBP)*100, format = "f", digits = 1)
  out = paste0(myn, " (", myp, "%)")
  for (k in levels(covars$data_type)){
    myn = sum(is.na(covars$SBP[covars$data_type==k]))
    myp = formatC(myn/sum(covars$data_type==k)*100, format = "f", digits = 1)
    out=c(out, paste0(myn, " (", myp, "%)"))
  }
  mytable=rbind(mytable, out)
  rownames(mytable)[nrow(mytable)]="Missing, N (%)"
}
# Diastolic blood pressure
mymean=formatC(mean(covars$DBP, na.rm=TRUE), format="f", digits=1)
mysd=formatC(sd(covars$DBP, na.rm=TRUE), format="f", digits=1)
out=paste0(mymean, " (", mysd, ")")
for (k in levels(covars$data_type)){
  mymean=formatC(mean(covars$DBP[covars$data_type==k], na.rm=TRUE), format="f", digits=1)
  mysd=formatC(sd(covars$DBP[covars$data_type==k], na.rm=TRUE), format="f", digits=1)
  out=c(out, paste0(mymean, " (", mysd, ")"))
}
mytable=rbind(mytable, out)
rownames(mytable)[nrow(mytable)]="Diastolic blood pressure (mmHg), Mean (SD)"
myn = sum(is.na(covars$DBP))
if(myn > 0){
  myp = formatC(myn/length(covars$DBP)*100, format = "f", digits = 1)
  out = paste0(myn, " (", myp, "%)")
  for (k in levels(covars$data_type)){
    myn = sum(is.na(covars$DBP[covars$data_type==k]))
    myp = formatC(myn/sum(covars$data_type==k)*100, format = "f", digits = 1)
    out=c(out, paste0(myn, " (", myp, "%)"))
  }
  mytable=rbind(mytable, out)
  rownames(mytable)[nrow(mytable)]="Missing, N (%)"
}
# Fasting blood glucose
mymean=formatC(mean(covars$FPG, na.rm=TRUE), format="f", digits=1)
mysd=formatC(sd(covars$FPG, na.rm=TRUE), format="f", digits=1)
out=paste0(mymean, " (", mysd, ")")
for (k in levels(covars$data_type)){
  mymean=formatC(mean(covars$FPG[covars$data_type==k], na.rm=TRUE), format="f", digits=1)
  mysd=formatC(sd(covars$FPG[covars$data_type==k], na.rm=TRUE), format="f", digits=1)
  out=c(out, paste0(mymean, " (", mysd, ")"))
}
mytable=rbind(mytable, out)
rownames(mytable)[nrow(mytable)]="Fasting blood glucose (mg/dl), Mean (SD)"
myn = sum(is.na(covars$FPG))
if(myn > 0){
  myp = formatC(myn/length(covars$FPG)*100, format = "f", digits = 1)
  out = paste0(myn, " (", myp, "%)")
  for (k in levels(covars$data_type)){
    myn = sum(is.na(covars$FPG[covars$data_type==k]))
    myp = formatC(myn/sum(covars$data_type==k)*100, format = "f", digits = 1)
    out=c(out, paste0(myn, " (", myp, "%)"))
  }
  mytable=rbind(mytable, out)
  rownames(mytable)[nrow(mytable)]="Missing, N (%)"
}
# Total cholesterol
mymean=formatC(mean(covars$TC, na.rm=TRUE), format="f", digits=1)
mysd=formatC(sd(covars$TC, na.rm=TRUE), format="f", digits=1)
out=paste0(mymean, " (", mysd, ")")
for (k in levels(covars$data_type)){
  mymean=formatC(mean(covars$TC[covars$data_type==k], na.rm=TRUE), format="f", digits=1)
  mysd=formatC(sd(covars$TC[covars$data_type==k], na.rm=TRUE), format="f", digits=1)
  out=c(out, paste0(mymean, " (", mysd, ")"))
}
mytable=rbind(mytable, out)
rownames(mytable)[nrow(mytable)]="Total cholesterol (mg/dl), Mean (SD)"
myn = sum(is.na(covars$TC))
if(myn > 0){
  myp = formatC(myn/length(covars$TC)*100, format = "f", digits = 1)
  out = paste0(myn, " (", myp, "%)")
  for (k in levels(covars$data_type)){
    myn = sum(is.na(covars$TC[covars$data_type==k]))
    myp = formatC(myn/sum(covars$data_type==k)*100, format = "f", digits = 1)
    out=c(out, paste0(myn, " (", myp, "%)"))
  }
  mytable=rbind(mytable, out)
  rownames(mytable)[nrow(mytable)]="Missing, N (%)"
}
# HDL cholesterol
mymean=formatC(mean(covars$HDLC, na.rm=TRUE), format="f", digits=1)
mysd=formatC(sd(covars$HDLC, na.rm=TRUE), format="f", digits=1)
out=paste0(mymean, " (", mysd, ")")
for (k in levels(covars$data_type)){
  mymean=formatC(mean(covars$HDLC[covars$data_type==k], na.rm=TRUE), format="f", digits=1)
  mysd=formatC(sd(covars$HDLC[covars$data_type==k], na.rm=TRUE), format="f", digits=1)
  out=c(out, paste0(mymean, " (", mysd, ")"))
}
mytable=rbind(mytable, out)
rownames(mytable)[nrow(mytable)]="HDL cholesterol (mg/dl), Mean (SD)"
myn = sum(is.na(covars$HDLC))
if(myn > 0){
  myp = formatC(myn/length(covars$HDLC)*100, format = "f", digits = 1)
  out = paste0(myn, " (", myp, "%)")
  for (k in levels(covars$data_type)){
    myn = sum(is.na(covars$HDLC[covars$data_type==k]))
    myp = formatC(myn/sum(covars$data_type==k)*100, format = "f", digits = 1)
    out=c(out, paste0(myn, " (", myp, "%)"))
  }
  mytable=rbind(mytable, out)
  rownames(mytable)[nrow(mytable)]="Missing, N (%)"
}
# LDL cholesterol
mymean=formatC(mean(covars$LDLC, na.rm=TRUE), format="f", digits=1)
mysd=formatC(sd(covars$LDLC, na.rm=TRUE), format="f", digits=1)
out=paste0(mymean, " (", mysd, ")")
for (k in levels(covars$data_type)){
  mymean=formatC(mean(covars$LDLC[covars$data_type==k], na.rm=TRUE), format="f", digits=1)
  mysd=formatC(sd(covars$LDLC[covars$data_type==k], na.rm=TRUE), format="f", digits=1)
  out=c(out, paste0(mymean, " (", mysd, ")"))
}
mytable=rbind(mytable, out)
rownames(mytable)[nrow(mytable)]="LDL cholesterol (mg/dl), Mean (SD)"
myn = sum(is.na(covars$LDLC))
if(myn > 0){
  myp = formatC(myn/length(covars$LDLC)*100, format = "f", digits = 1)
  out = paste0(myn, " (", myp, "%)")
  for (k in levels(covars$data_type)){
    myn = sum(is.na(covars$LDLC[covars$data_type==k]))
    myp = formatC(myn/sum(covars$data_type==k)*100, format = "f", digits = 1)
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
for (k in levels(covars$data_type)){
  myn = sum(covars$WC_cat[covars$data_type==k]=="High", na.rm = T)
  myp = formatC(myn/sum(covars$data_type==k)*100, format = "f", digits = 1)
  out=c(out, paste0(myn, " (", myp, "%)"))
}
mytable=rbind(mytable, out)
rownames(mytable)[nrow(mytable)]="High"
myn = sum(covars$WC_cat=="Normal", na.rm = T)
myp = formatC(myn/length(covars$WC_cat)*100, format = "f", digits = 1)
out = paste0(myn, " (", myp, "%)")
for (k in levels(covars$data_type)){
  myn = sum(covars$WC_cat[covars$data_type==k]=="Normal", na.rm = T)
  myp = formatC(myn/sum(covars$data_type==k)*100, format = "f", digits = 1)
  out=c(out, paste0(myn, " (", myp, "%)"))
}
mytable=rbind(mytable, out)
rownames(mytable)[nrow(mytable)]="Normal"
myn = sum(is.na(covars$WC_cat))
if(myn > 0){
  myp = formatC(myn/length(covars$WC_cat)*100, format = "f", digits = 1)
  out = paste0(myn, " (", myp, "%)")
  for (k in levels(covars$data_type)){
    myn = sum(is.na(covars$WC_cat[covars$data_type==k]))
    myp = formatC(myn/sum(covars$data_type==k)*100, format = "f", digits = 1)
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
for (k in levels(covars$data_type)){
  myn = sum(covars$TG[covars$data_type==k]>=150, na.rm = T)
  myp = formatC(myn/sum(covars$data_type==k)*100, format = "f", digits = 1)
  out=c(out, paste0(myn, " (", myp, "%)"))
}
mytable=rbind(mytable, out)
rownames(mytable)[nrow(mytable)]=">=150mg/dl"
myn = sum(covars$TG<150, na.rm = T)
myp = formatC(myn/length(covars$TG)*100, format = "f", digits = 1)
out = paste0(myn, " (", myp, "%)")
for (k in levels(covars$data_type)){
  myn = sum(covars$TG[covars$data_type==k]<150, na.rm = T)
  myp = formatC(myn/sum(covars$data_type==k)*100, format = "f", digits = 1)
  out=c(out, paste0(myn, " (", myp, "%)"))
}
mytable=rbind(mytable, out)
rownames(mytable)[nrow(mytable)]="<150mg/dl"
myn = sum(is.na(covars$TG))
if(myn > 0){
  myp = formatC(myn/length(covars$TG)*100, format = "f", digits = 1)
  out = paste0(myn, " (", myp, "%)")
  for (k in levels(covars$data_type)){
    myn = sum(is.na(covars$WC_cat[covars$data_type==k]))
    myp = formatC(myn/sum(covars$data_type==k)*100, format = "f", digits = 1)
    out=c(out, paste0(myn, " (", myp, "%)"))
  }
  mytable=rbind(mytable, out)
  rownames(mytable)[nrow(mytable)]="Missing, N (%)"
}
# Medication
mytable=rbind(mytable, rep(NA, ncol(mytable)))
rownames(mytable)[nrow(mytable)]="Type of medication, N (%)"
myn = sum(covars$Diabetes_medication=="Yes", na.rm = T)
myp = formatC(myn/length(covars$Diabetes_medication)*100, format = "f", digits = 1)
out = paste0(myn, " (", myp, "%)")
for (k in levels(covars$data_type)){
  myn = sum(covars$Diabetes_medication[covars$data_type==k]=="Yes", na.rm = T)
  myp = formatC(myn/sum(covars$data_type==k)*100, format = "f", digits = 1)
  out=c(out, paste0(myn, " (", myp, "%)"))
}
mytable=rbind(mytable, out)
rownames(mytable)[nrow(mytable)]="Diabetes"
myn = sum(is.na(covars$Diabetes_medication))
if(myn > 0){
  myp = formatC(myn/length(covars$Diabetes_medication)*100, format = "f", digits = 1)
  out = paste0(myn, " (", myp, "%)")
  for (k in levels(covars$data_type)){
    myn = sum(is.na(covars$WC_cat[covars$data_type==k]))
    myp = formatC(myn/sum(covars$data_type==k)*100, format = "f", digits = 1)
    out=c(out, paste0(myn, " (", myp, "%)"))
  }
  mytable=rbind(mytable, out)
  rownames(mytable)[nrow(mytable)]="Diabetes medication: Missing, N (%)"
}
myn = sum(covars$Hypertension_medication=="Yes", na.rm = T)
myp = formatC(myn/length(covars$Hypertension_medication)*100, format = "f", digits = 1)
out = paste0(myn, " (", myp, "%)")
for (k in levels(covars$data_type)){
  myn = sum(covars$Hypertension_medication[covars$data_type==k]=="Yes", na.rm = T)
  myp = formatC(myn/sum(covars$data_type==k)*100, format = "f", digits = 1)
  out=c(out, paste0(myn, " (", myp, "%)"))
}
mytable=rbind(mytable, out)
rownames(mytable)[nrow(mytable)]="Hypertension"
myn = sum(is.na(covars$Hypertension_medication))
if(myn > 0){
  myp = formatC(myn/length(covars$Hypertension_medication)*100, format = "f", digits = 1)
  out = paste0(myn, " (", myp, "%)")
  for (k in levels(covars$data_type)){
    myn = sum(is.na(covars$WC_cat[covars$data_type==k]))
    myp = formatC(myn/sum(covars$data_type==k)*100, format = "f", digits = 1)
    out=c(out, paste0(myn, " (", myp, "%)"))
  }
  mytable=rbind(mytable, out)
  rownames(mytable)[nrow(mytable)]="Hypertension medication: Missing, N (%)"
}
myn = sum(covars$Lipid_disorder_medication=="Yes", na.rm = T)
myp = formatC(myn/length(covars$Lipid_disorder_medication)*100, format = "f", digits = 1)
out = paste0(myn, " (", myp, "%)")
for (k in levels(covars$data_type)){
  myn = sum(covars$Lipid_disorder_medication[covars$data_type==k]=="Yes", na.rm = T)
  myp = formatC(myn/sum(covars$data_type==k)*100, format = "f", digits = 1)
  out=c(out, paste0(myn, " (", myp, "%)"))
}
mytable=rbind(mytable, out)
rownames(mytable)[nrow(mytable)]="Lipid disorder"
myn = sum(is.na(covars$Lipid_disorder_medication))
if(myn > 0){
  myp = formatC(myn/length(covars$Lipid_disorder_medication)*100, format = "f", digits = 1)
  out = paste0(myn, " (", myp, "%)")
  for (k in levels(covars$data_type)){
    myn = sum(is.na(covars$WC_cat[covars$data_type==k]))
    myp = formatC(myn/sum(covars$data_type==k)*100, format = "f", digits = 1)
    out=c(out, paste0(myn, " (", myp, "%)"))
  }
  mytable=rbind(mytable, out)
  rownames(mytable)[nrow(mytable)]="Lipid disorder: Missing, N (%)"
}
out = nrow(covars)
for (k in levels(covars$data_type)){
  out=c(out, sum(covars$data_type==k))
}
colnames(mytable)=paste0(c("Total",levels(covars$data_type))," (N=",out,")")
SaveExcelWithSuperscripts(cbind(rownames(mytable),mytable), paste0("../Exports/Descriptive_statistics/","Characteristics_outcome_by_","data_type",".xlsx"))
}

## Overview of outcome prevalence: by Country ----
{
  data = readRDS(paste0("../Data/","All_participants_info",".rds"))
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
  SaveExcelWithSuperscripts(cbind(rownames(mytable),mytable), paste0("../Exports/Descriptive_statistics/","Outcome_prevalence_by_Country.xlsx"))
}
## Overview of outcome prevalence: by Gender ----
{
  data = readRDS(paste0("../Data/","All_participants_info",".rds"))
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
  SaveExcelWithSuperscripts(cbind(rownames(mytable),mytable), paste0("../Exports/Descriptive_statistics/","Outcome_prevalence_by_Gender.xlsx"))
}
## Overview of outcome prevalence: by data_type ----
{
  data = readRDS(paste0("../Data/","All_participants_info",".rds"))
  outcomes = data[,c("Obesity","Hypertension","Diabetes","Lipid_disorder","Metabolic_syndrome")]
  mytable=NULL
  for (j in 1:ncol(outcomes)){
    vars = outcomes[,j]
    myn = sum(vars=="Yes", na.rm = T)
    myp = formatC(myn/length(vars)*100, format = "f", digits = 1)
    out = paste0(myn, " (", myp, "%)")
    for (k in levels(data$data_type)){
      myn = sum(vars[data$data_type==k]=="Yes", na.rm = T)
      myp = formatC(myn/sum(data$data_type==k)*100, format = "f", digits = 1)
      out=c(out, paste0(myn, " (", myp, "%)"))
    }
    tmp=formatC(chisq.test(table(vars,data$data_type))$p.value, format="e", digits=2)
    out = c(out,tmp)
    mytable=rbind(mytable, out)
    rownames(mytable)[nrow(mytable)]=colnames(outcomes)[j]
  }
  out = nrow(data)
  for (k in levels(data$data_type)){
    out=c(out, sum(data$data_type==k))
  }
  colnames(mytable)=c(paste0(c("Total",levels(data$data_type))," (N=",out,")"),"P-value")
  SaveExcelWithSuperscripts(cbind(rownames(mytable),mytable), paste0("../Exports/Descriptive_statistics/","Outcome_prevalence_by_data_type.xlsx"))
}

## Co-occurence of CVDRFs ----

rm(list=ls())
project_path=dirname(rstudioapi::getActiveDocumentContext()$path)
setwd(project_path)

# install.packages("VennDiagram")
library(VennDiagram)
library(tidyverse)
library(colorspace)
source("graph_colours.R")

ifelse(dir.exists("../Figures/"), "", dir.create("../Figures/"))
ifelse(dir.exists("../Figures/Descriptive_statistics/"), "", dir.create("../Figures/Descriptive_statistics/"))


{
  data = readRDS(paste0("../Data/","All_participants_info",".rds"))
  
  obesity = data$ID[which(data$Obesity == "Yes")]
  hypertension = data$ID[which(data$Hypertension == "Yes")]
  diabetes = data$ID[which(data$Diabetes == "Yes")]
  ld = data$ID[which(data$Lipid_disorder == "Yes")]
  ms = data$ID[which(data$Metabolic_syndrome == "Yes")]
  
  mylist = list(obesity, hypertension, diabetes, ld, ms)
  mynames = paste0(c("Obesity", "Hypertension", "Diabetes", "Lipid disorder", "Metabolic syndrome"), 
                   " (N=", sapply(mylist, length), ")")
  venn.diagram(mylist, filename = paste0("../Figures/Descriptive_statistics/Venn_diagram_outcome_","All_participants",".tiff"),
               fill = outcome.colours, fontfamily ="sans", cat.fontfamily ="sans",
               print.mode=c("raw"), cex = 0.8, cat.cex = 0.8,
               category.names = mynames, imagetype = "tiff", 
               cat.just = list(c(0.5, 1), c(0, -6), c(0, 0), c(0.5, 0.5), c(1, -6)))
}

