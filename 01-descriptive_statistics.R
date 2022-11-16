## Descriptive statistics: covariates and outcomes

rm(list=ls())
project_path=dirname(rstudioapi::getActiveDocumentContext()$path)
setwd(project_path)

source("functions.R")
library(tidyverse)

ifelse(dir.exists("../Exports/Descriptive_statistics/"), "", dir.create("../Exports/Descriptive_statistics/"))

# Overview of population characteristics ----
filepath = "PesticidesPCBBDEs"
covar = readRDS(paste0("../Data/",filepath,"_covar.rds"))

## Overview of population characteristics: by Centre ----
group.factor.name = "Centre"
data = covar %>% select(Age, Gender, Education, Smoking,
                        Energy, Proteins, Fats, Carbohydrates, Fibers,
                        Physical_activity, Alcohol)
labels = c("Age","Gender", "Education","Smoking status", "Energy", "Proteins", "Fats",
           "Carbohydrates", "Fibers", "Physical activity",
           "Alcohol consumption")
units = c("years",NA, NA, NA, "kcal/day", "g/day", "g/day",
          "g/day", "g/day", NA, NA)
{
  group.factor = covar[,group.factor.name]
  mytable=NULL
  for (v in 1:ncol(data)){
    label = labels[v]
    unit = units[v]
    if (is.numeric(data[,v])){
      mymean=formatC(mean(data[,v], na.rm=TRUE), format="f", digits=1)
      mysd=formatC(sd(data[,v], na.rm=TRUE), format="f", digits=1)
      out = paste0(mymean, " (", mysd, ")")
      for (k in levels(group.factor)){
        mymean=formatC(mean(data[,v][group.factor==k], na.rm=TRUE), format="f", digits=1)
        mysd=formatC(sd(data[,v][group.factor==k], na.rm=TRUE), format="f", digits=1)
        out=c(out, paste0(mymean, " (", mysd, ")"))
      }
      mytable=rbind(mytable, out)
      rownames(mytable)[nrow(mytable)]= paste0(label," (",unit,"), ","Mean (SD)")
      myn = sum(is.na(data[,v]))
    } else if(is.factor(data[,v])){
      mytable=rbind(mytable, rep(NA, length(levels(group.factor))+1))
      rownames(mytable)[nrow(mytable)]=paste0(label, ", N (%)")
      for (l in 1:length(levels(data[,v]))){
        myn = sum(data[,v]==levels(data[,v])[l], na.rm = T)
        myp = formatC(myn/length(data[,v])*100, format = "f", digits = 1)
        out = paste0(myn, " (", myp, "%)")
        for (k in levels(group.factor)){
          myn = sum(data[,v][group.factor==k]==levels(data[,v])[l], na.rm = T)
          myp = formatC(myn/sum(group.factor==k)*100, format = "f", digits = 1)
          out=c(out, paste0(myn, " (", myp, "%)"))
        }
        mytable=rbind(mytable, out)
        rownames(mytable)[nrow(mytable)]=paste0(levels(data[,v])[l]) 
      }
    }
    myn = sum(is.na(data[,v]))
    if(myn > 0){
      myp = formatC(myn/length(data[,v])*100, format = "f", digits = 1)
      out = paste0(myn, " (", myp, "%)")
      for (k in levels(group.factor)){
        myn = sum(is.na(data[,v][group.factor==k]))
        myp = formatC(myn/sum(group.factor==k)*100, format = "f", digits = 1)
        out=c(out, paste0(myn, " (", myp, "%)"))
      }
      mytable=rbind(mytable, out)
      rownames(mytable)[nrow(mytable)]="Missing, N (%)"
    }
  }
  out = nrow(data)
  for (k in levels(group.factor)){
    out=c(out, sum(group.factor==k))
  }
  colnames(mytable)=paste0(c("Total",levels(group.factor))," (N=",out,")")
  SaveExcelWithSuperscripts(cbind(rownames(mytable),mytable), paste0("../Exports/Descriptive_statistics/","Characteristics_by_",group.factor.name,".xlsx"))
}
## Overview of population characteristics: by Gender ----
group.factor.name = "Gender"
data = covar %>% select(Age, Education, Smoking,
                        Energy, Proteins, Fats, Carbohydrates, Fibers,
                        Physical_activity, Alcohol, Centre)
labels = c("Age", "Education", "Smoking status", "Energy", "Proteins", "Fats",
           "Carbohydrates", "Fibers","Physical activity",
           "Alcohol consumption", "Centre")
units = c("years", NA, NA, "kcal/day", "g/day", "g/day",
          "g/day", "g/day", NA, NA, NA)
{
  group.factor = covar[,group.factor.name]
  mytable=NULL
  for (v in 1:ncol(data)){
    label = labels[v]
    unit = units[v]
    if (is.numeric(data[,v])){
      mymean=formatC(mean(data[,v], na.rm=TRUE), format="f", digits=1)
      mysd=formatC(sd(data[,v], na.rm=TRUE), format="f", digits=1)
      out = paste0(mymean, " (", mysd, ")")
      for (k in levels(group.factor)){
        mymean=formatC(mean(data[,v][group.factor==k], na.rm=TRUE), format="f", digits=1)
        mysd=formatC(sd(data[,v][group.factor==k], na.rm=TRUE), format="f", digits=1)
        out=c(out, paste0(mymean, " (", mysd, ")"))
      }
      mytable=rbind(mytable, out)
      rownames(mytable)[nrow(mytable)]= paste0(label," (",unit,"), ","Mean (SD)")
      myn = sum(is.na(data[,v]))
    } else if(is.factor(data[,v])){
      mytable=rbind(mytable, rep(NA, length(levels(group.factor))+1))
      rownames(mytable)[nrow(mytable)]=paste0(label, ", N (%)")
      for (l in 1:length(levels(data[,v]))){
        myn = sum(data[,v]==levels(data[,v])[l], na.rm = T)
        myp = formatC(myn/length(data[,v])*100, format = "f", digits = 1)
        out = paste0(myn, " (", myp, "%)")
        for (k in levels(group.factor)){
          myn = sum(data[,v][group.factor==k]==levels(data[,v])[l], na.rm = T)
          myp = formatC(myn/sum(group.factor==k)*100, format = "f", digits = 1)
          out=c(out, paste0(myn, " (", myp, "%)"))
        }
        mytable=rbind(mytable, out)
        rownames(mytable)[nrow(mytable)]=paste0(levels(data[,v])[l]) 
      }
    }
    myn = sum(is.na(data[,v]))
    if(myn > 0){
      myp = formatC(myn/length(data[,v])*100, format = "f", digits = 1)
      out = paste0(myn, " (", myp, "%)")
      for (k in levels(group.factor)){
        myn = sum(is.na(data[,v][group.factor==k]))
        myp = formatC(myn/sum(group.factor==k)*100, format = "f", digits = 1)
        out=c(out, paste0(myn, " (", myp, "%)"))
      }
      mytable=rbind(mytable, out)
      rownames(mytable)[nrow(mytable)]="Missing, N (%)"
    }
  }
  out = nrow(data)
  for (k in levels(group.factor)){
    out=c(out, sum(group.factor==k))
  }
  colnames(mytable)=paste0(c("Total",levels(group.factor))," (N=",out,")")
  SaveExcelWithSuperscripts(cbind(rownames(mytable),mytable), paste0("../Exports/Descriptive_statistics/","Characteristics_by_",group.factor.name,".xlsx"))
}

# Overview of outcomes ----
covar = readRDS(paste0("../Data/",filepath,"_covar.rds"))
trait = readRDS(paste0("../Data/",filepath,"_trait.rds"))

## Overview of outcomes: by Centre----
group.factor.name = "Centre"
data = trait %>% select(BMI_cat, WC_cat, TG_cat, TC, HDLC, LDLC, SBP, DBP, FPG)
labels = c("Body Mass Index","Waist circumference","Triglyceride", "Total cholesterol",
           "HDL-C", "LDL-C", "Systolic blood pressue", "Diastolic blood pressure",
           "Fasting plasma glucose")
units = c(NA, NA, NA, "mg/dL", "mg/dL", "mg/dL", "mmHg", "mmHg","mg/dL")
{
  group.factor = covar[,group.factor.name]
  mytable=NULL
  for (v in 1:ncol(data)){
    label = labels[v]
    unit = units[v]
    if (is.numeric(data[,v])){
      mymean=formatC(mean(data[,v], na.rm=TRUE), format="f", digits=1)
      mysd=formatC(sd(data[,v], na.rm=TRUE), format="f", digits=1)
      out = paste0(mymean, " (", mysd, ")")
      for (k in levels(group.factor)){
        mymean=formatC(mean(data[,v][group.factor==k], na.rm=TRUE), format="f", digits=1)
        mysd=formatC(sd(data[,v][group.factor==k], na.rm=TRUE), format="f", digits=1)
        out=c(out, paste0(mymean, " (", mysd, ")"))
      }
      mytable=rbind(mytable, out)
      rownames(mytable)[nrow(mytable)]= paste0(label," (",unit,"), ","Mean (SD)")
      myn = sum(is.na(data[,v]))
    } else if(is.factor(data[,v])){
      mytable=rbind(mytable, rep(NA, length(levels(group.factor))+1))
      rownames(mytable)[nrow(mytable)]=paste0(label, ", N (%)")
      for (l in 1:length(levels(data[,v]))){
        myn = sum(data[,v]==levels(data[,v])[l], na.rm = T)
        myp = formatC(myn/length(data[,v])*100, format = "f", digits = 1)
        out = paste0(myn, " (", myp, "%)")
        for (k in levels(group.factor)){
          myn = sum(data[,v][group.factor==k]==levels(data[,v])[l], na.rm = T)
          myp = formatC(myn/sum(group.factor==k)*100, format = "f", digits = 1)
          out=c(out, paste0(myn, " (", myp, "%)"))
        }
        mytable=rbind(mytable, out)
        rownames(mytable)[nrow(mytable)]=paste0(levels(data[,v])[l]) 
      }
    }
    myn = sum(is.na(data[,v]))
    if(myn > 0){
      myp = formatC(myn/length(data[,v])*100, format = "f", digits = 1)
      out = paste0(myn, " (", myp, "%)")
      for (k in levels(group.factor)){
        myn = sum(is.na(data[,v][group.factor==k]))
        myp = formatC(myn/sum(group.factor==k)*100, format = "f", digits = 1)
        out=c(out, paste0(myn, " (", myp, "%)"))
      }
      mytable=rbind(mytable, out)
      rownames(mytable)[nrow(mytable)]="Missing, N (%)"
    }
  }
  out = nrow(data)
  for (k in levels(group.factor)){
    out=c(out, sum(group.factor==k))
  }
  colnames(mytable)=paste0(c("Total",levels(group.factor))," (N=",out,")")
  SaveExcelWithSuperscripts(cbind(rownames(mytable),mytable), paste0("../Exports/Descriptive_statistics/","Outcome_by_",group.factor.name,".xlsx"))
}

## Overview of outcomes: by Gender----
group.factor.name = "Gender"
data = trait %>% select(BMI_cat, WC_cat, TG_cat, TC, HDLC, LDLC, SBP, DBP, FPG)
labels = c("Body Mass Index","Waist circumference","Triglyceride", "Total cholesterol",
           "HDL-C", "LDL-C", "Systolic blood pressue", "Diastolic blood pressure",
           "Fasting plasma glucose")
units = c(NA, NA, NA, "mg/dL", "mg/dL", "mg/dL", "mmHg", "mmHg","mg/dL")
{
  group.factor = covar[,group.factor.name]
  mytable=NULL
  for (v in 1:ncol(data)){
    label = labels[v]
    unit = units[v]
    if (is.numeric(data[,v])){
      mymean=formatC(mean(data[,v], na.rm=TRUE), format="f", digits=1)
      mysd=formatC(sd(data[,v], na.rm=TRUE), format="f", digits=1)
      out = paste0(mymean, " (", mysd, ")")
      for (k in levels(group.factor)){
        mymean=formatC(mean(data[,v][group.factor==k], na.rm=TRUE), format="f", digits=1)
        mysd=formatC(sd(data[,v][group.factor==k], na.rm=TRUE), format="f", digits=1)
        out=c(out, paste0(mymean, " (", mysd, ")"))
      }
      mytable=rbind(mytable, out)
      rownames(mytable)[nrow(mytable)]= paste0(label," (",unit,"), ","Mean (SD)")
      myn = sum(is.na(data[,v]))
    } else if(is.factor(data[,v])){
      mytable=rbind(mytable, rep(NA, length(levels(group.factor))+1))
      rownames(mytable)[nrow(mytable)]=paste0(label, ", N (%)")
      for (l in 1:length(levels(data[,v]))){
        myn = sum(data[,v]==levels(data[,v])[l], na.rm = T)
        myp = formatC(myn/length(data[,v])*100, format = "f", digits = 1)
        out = paste0(myn, " (", myp, "%)")
        for (k in levels(group.factor)){
          myn = sum(data[,v][group.factor==k]==levels(data[,v])[l], na.rm = T)
          myp = formatC(myn/sum(group.factor==k)*100, format = "f", digits = 1)
          out=c(out, paste0(myn, " (", myp, "%)"))
        }
        mytable=rbind(mytable, out)
        rownames(mytable)[nrow(mytable)]=paste0(levels(data[,v])[l]) 
      }
    }
    myn = sum(is.na(data[,v]))
    if(myn > 0){
      myp = formatC(myn/length(data[,v])*100, format = "f", digits = 1)
      out = paste0(myn, " (", myp, "%)")
      for (k in levels(group.factor)){
        myn = sum(is.na(data[,v][group.factor==k]))
        myp = formatC(myn/sum(group.factor==k)*100, format = "f", digits = 1)
        out=c(out, paste0(myn, " (", myp, "%)"))
      }
      mytable=rbind(mytable, out)
      rownames(mytable)[nrow(mytable)]="Missing, N (%)"
    }
  }
  out = nrow(data)
  for (k in levels(group.factor)){
    out=c(out, sum(group.factor==k))
  }
  colnames(mytable)=paste0(c("Total",levels(group.factor))," (N=",out,")")
  SaveExcelWithSuperscripts(cbind(rownames(mytable),mytable), paste0("../Exports/Descriptive_statistics/","Outcome_by_",group.factor.name,".xlsx"))
}

# Co-occurence of CVDRFs ----
rm(list=ls())
project_path=dirname(rstudioapi::getActiveDocumentContext()$path)
setwd(project_path)

# install.packages("VennDiagram")
library(VennDiagram)
library(tidyverse)
library(colorspace)
source("graph_colours.R")

{
  trait = readRDS(paste0("../Data/",filepath,"_trait.rds"))
  
  OB = rownames(trait)[which(trait$Obesity == "1")]
  DL = rownames(trait)[which(trait$Dyslipidemia == "1")]
  HT = rownames(trait)[which(trait$Hypertension == "1")]
  DB = rownames(trait)[which(trait$Diabetes == "1")]
  
  mylist = list(OB, DL, HT, DB)
  mynames = paste0(c("Obesity", "Dyslipidemia", "Hypertension", "Diabetes"), 
                   " (N=", sapply(mylist, length), ")")
  venn.diagram(mylist, filename = "../Figures/Supplementary/Venn_diagram_disease_outcome.tiff",
               fill = c("#ef7c10","#00722a", "#dc251f","#009ee2"), fontfamily ="sans", cat.fontfamily ="sans",
               print.mode=c("raw"), cex = 0.8, cat.cex = 0.8,
               category.names = mynames, imagetype = "tiff", 
               cat.just = list(c(0.3, 0.5), c(0.65, 0.5), c(0.5, 0.5), c(0.5, 0.5)))
}

