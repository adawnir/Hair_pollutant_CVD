## Data cleaning

rm(list=ls())
project_path=dirname(rstudioapi::getActiveDocumentContext()$path)
setwd(project_path)

source("functions.R")
library(tidyverse)
library(readxl)

ifelse(dir.exists("../Processed/"), "", dir.create("../Processed/"))

## Data cleaning ----
filepaths = c("PesticidesPCBBDEs","PAHs")
for (m in 1:2){
  data = read_excel("../Data/Dataset_Pesticides_PAHs_05242022.xlsx", sheet = m)
  str(data)
  dim(data)
  colnames(data)[38:ncol(data)] = readLines(paste0("../Dictionaries/",filepaths[m],"_names.txt"))
  newdata = as.data.frame(data)
  rownames(newdata) = newdata$identifiant
  dim(newdata)
  colnames(newdata)
  
  # Check if all participants are Fasting
  newdata %>% filter(Fasting==0) %>% nrow
  # Check for participants missing >50% diet information
  remove = newdata %>% select(Energy, Proteins, Fats, Carbohydrates, Fibers) %>%
    .[rowSums(is.na(.))/ncol(.) > 0.5,] %>% rownames
  print(paste0("Removing ",length(remove)," participants missing >50% diet information"))
  newdata = newdata[-which(rownames(newdata) %in% remove),]
  
  # Check for participants missing >50% covariate information
  newdata %>% select(Age, Gender, Smoking,
                      Energy, Proteins, Fats, Carbohydrates, Fibers,
                      Education, Physical_activity, Alcohol) %>%
    .[rowSums(is.na(.))/ncol(.) > 0.5,] %>% nrow
  
  # Rename columns
  newdata = newdata %>% rename(ID = "identifiant",
                               Centre = "Country",
                               Diabetes_medication = "Diabetic_MD",
                               Hypertension_medication = "Antihypertensive_MD",
                               Lipid_medication = "Lipid_MD")
  
  newdata = newdata %>% mutate_at(vars(ends_with("_medication")),
                                  function(x) factor(x, 0:1, labels = c("No","Yes")))
  
  # Covariate transformation
  table(newdata$Centre)
  newdata$Centre = factor(newdata$Centre)
  
  table(newdata$Gender)
  newdata$Gender = factor(ifelse(newdata$Gender=="F", "Female", "Male"))
  newdata$Gender = relevel(newdata$Gender, "Male") # Male as reference
  
  table(newdata$Smoking)
  newdata$Smoking = factor(newdata$Smoking, 1:3, labels = c("Never smoker","Former smoker","Current smoker"))
  
  table(newdata$Physical_activity)
  newdata$Physical_activity = factor(newdata$Physical_activity, 1:3, labels = c("Inactive","Moderately active","Active"))

  table(newdata$Alcohol)
  newdata$Alcohol = factor(newdata$Alcohol, 1:3, labels = c("Low","Moderate","High"))
  
  table(newdata$Education)
  newdata$Education = factor(newdata$Education, 1:3, labels = c("Low","Intermediate","High"))
  
  # Outcome ascertainment
  # Categorise BMI
  newdata$BMI_cat = ifelse(newdata$BMI <18.5,"Underweight",
                            ifelse(newdata$BMI >=18.5 & newdata$BMI <25, "Normal",
                                   ifelse(newdata$BMI >=25 & newdata$BMI <30, "Pre-obesity",
                                          ifelse(newdata$BMI >=30 & newdata$BMI <35, "Obesity class I",
                                                 ifelse(newdata$BMI >=35 & newdata$BMI <40, "Obesity class II",
                                                        "Obesity class III")))))
  newdata$BMI_cat = factor(newdata$BMI_cat,
                           levels = c("Underweight","Normal", "Pre-obesity",
                                      "Obesity class I","Obesity class II",
                                      "Obesity class III"))
  
  # Dichotomous WC
  newdata = newdata %>% mutate(WC_cat =  factor(ifelse(newdata$WC >=102 & newdata$Gender == "Male", 1,
                                                       ifelse(newdata$WC >=88 & newdata$Gender == "Female", 1, 0)),
                                                labels = c("Normal","High")))

  # Dichotomous TG
  newdata = newdata %>% mutate(TG_cat = factor(ifelse(newdata$TG >= 150, 1, 0), labels = c("Normal","High")))
  newdata$TG_cat = factor(newdata$TG_cat)
  
  # Subset relevant columns
  expo = colnames(data)[38:ncol(data)]
  expo = newdata %>% select(all_of(expo))
  
  covar = newdata %>% select(Centre, Age, Education, Gender, Smoking,
                             Energy, Proteins, Fats, Carbohydrates, Fibers,
                             Physical_activity, Alcohol)
  response = newdata %>% select(BMI, WC, TG, TC, LDLC, HDLC, SBP, DBP, FPG,
                                BMI_cat, WC_cat, TG_cat, ends_with("_medication"),
                                Obesity, Dyslipidemia, Hypertension, Diabetes)
  all(rownames(expo) == rownames(covar))
  all(rownames(covar) == rownames(response))
  
  ifelse(dir.exists("../Exports/"), "", dir.create("../Exports/"))
  ifelse(dir.exists("../Exports/Data/"), "", dir.create("../Exports/Data/"))
  saveRDS(expo, paste0("../Data/",filepaths[m],"_expo.rds"))
  write.csv(expo, paste0("../Exports/Data/",filepaths[m],"_expo.csv"))
  saveRDS(covar, paste0("../Data/",filepaths[m],"_covar.rds"))
  write.csv(covar, paste0("../Exports/Data/",filepaths[m],"_covar.csv"))
  saveRDS(response, paste0("../Data/",filepaths[m],"_response.rds"))
  write.csv(response, paste0("../Exports/Data/",filepaths[m],"_response.csv"))
  
  ### Proportion of NAs per chemical compounds ----
  naprop = apply(expo, 2, function(x) sum(is.na(x))/nrow(expo))
  prop = sort(naprop, decreasing = TRUE)
  
  pdf(paste0("../Figures/Descriptive_statistics/Missing_prop_",filepaths[m],".pdf"), width = 9, height = 4.75)
  par(mar=c(9,5,2,1))
  plot(prop, type = "h", ylab="Proportion missing", xlab="", xaxt = "n", lwd=1, ylim = c(0,1),
       col=ifelse(prop>=0.5, yes="tomato", no="black"), cex.lab=1.5)
  abline(h=0.5, lty=2, col="darkred", lwd = 1)
  for (i in 1:length(prop)){
    axis(side=1, at=i, labels=names(prop)[i], las=2,
         col=ifelse(prop[i]>=0.5, yes="tomato", no="black"),
         col.axis=ifelse(prop[i]>=0.5, yes="tomato", no="black"))
  }
  dev.off()
  
  ### Proportion of NAs per participant ----
  summary(apply(expo, 1, function(x) sum(is.na(x))/ncol(expo)))

  ## Proportion of detected per chemical compounds ----
  ndprop = apply(expo, 2, function(x) sum(x=="nd", na.rm = TRUE)/nrow(expo))
  prop =sort(1-(ndprop+naprop), decreasing = TRUE)
  
  pdf(paste0("../Figures/Descriptive_statistics/Detection_prop_",filepaths[m],".pdf"), width = 9, height = 4.75)
  par(mar=c(9,5,2,1))
  plot(prop, type = "h", ylab="Proportion detected", xlab="", xaxt = "n", lwd=1, ylim = c(0,1),
       col=ifelse(prop<=0.1, yes="tomato", no="black"), cex.lab=1.5)
  abline(h=0.1, lty=2, col="darkred", lwd = 1)
  for (i in 1:length(prop)){
    axis(side=1, at=i, labels=names(prop)[i], las=2,
         col=ifelse(prop[i]<=0.1, yes="tomato", no="black"),
         col.axis=ifelse(prop[i]<=0.1, yes="tomato", no="black"))
  }
  dev.off()
  
  ### Proportion detected per participant ----
  summary(1-(apply(expo, 1, function(x) sum(x=="nd", na.rm = TRUE)/ncol(expo))+
               apply(expo, 1, function(x) sum(is.na(x))/ncol(expo))))
  
  ### Recoding nd ----
  # Extract LOD
  lod = apply(expo, 2,function(x) suppressWarnings(min(as.numeric(unlist(x)), na.rm = T)))
  
  # Save exposure information
  info = data.frame(lod = lod, ndprop = ndprop, naprop = naprop,row.names = colnames(expo))
  
  saveRDS(info, paste0("../Processed/",filepaths[m],"_info.rds"))
  
  ### Replace nd with random values from 0 to minimum detection (Gaussian)
  for(k in 1:ncol(expo)){
    expo[which(expo[,k]=="nd"),k] = rtruncnorm(sum(expo[,k]=="nd", na.rm = T),min=0,max=lod[k]) 
  }
  expo_ndimp = apply(expo, 2, as.numeric)
  rownames(expo_ndimp) = rownames(expo)
  summary(expo_ndimp)
  
  saveRDS(expo_ndimp, paste0("../Processed/",filepaths[m],"_expo_ndimp.rds"))
  write.csv(expo_ndimp, paste0("../Exports/Data/",filepaths[m],"_expo_ndimp.csv"))
}

## Train/Test split----
pest = readRDS("../Data/PesticidesPCBBDEs_covar.rds")
pahs = readRDS("../Data/PAHs_covar.rds")

# Unique participants for training
train_pest = setdiff(rownames(pest), rownames(pahs))
train_pahs = setdiff(rownames(pahs), rownames(pest))

# Overlapping participants for validation
test = intersect(rownames(pest), rownames(pahs))

# Check if covariate information is consistent
all.equal(pest[test,],pahs[test,])

# Covariates data
train_pest_covar = pest %>% .[train_pest,] %>% mutate_if(is.factor,droplevels)
train_pahs_covar = pahs %>% .[train_pahs,] %>% mutate_if(is.factor,droplevels)
test_covar = pest %>% .[test,] %>% mutate_if(is.factor,droplevels)
str(test_covar)

saveRDS(train_pest_covar, "../Data/PesticidesPCBBDEs_train_covar.rds")
saveRDS(train_pahs_covar, "../Data/PAHs_train_covar.rds")
saveRDS(test_covar, "../Data/Overlap_test_covar.rds")

# Check if response information is consistent
pestY = readRDS("../Data/PesticidesPCBBDEs_response.rds")
pahsY = readRDS("../Data/PAHs_response.rds")
all.equal(pestY[test,],pahsY[test,])

# Response data
train_pestY = pestY %>% .[train_pest,] %>% mutate_if(is.factor,droplevels)
train_pahsY = pahsY %>% .[train_pahs,] %>% mutate_if(is.factor,droplevels)
testY = pestY %>% .[test,] %>% mutate_if(is.factor,droplevels)
str(testY)

saveRDS(train_pestY, "../Data/PesticidesPCBBDEs_train_response.rds")
saveRDS(train_pahsY, "../Data/PAHs_train_response.rds")
saveRDS(testY, "../Data/Overlap_test_response.rds")

# Exposure data
pest = readRDS("../Processed/PesticidesPCBBDEs_expo_ndimp.rds")
pahs = readRDS("../Processed/PAHs_expo_ndimp.rds")

train_expo = pest %>% .[train_pest,]
train_pahs_expo = pahs %>% .[train_pahs,]
test_expo = cbind(pest[test,],pahs[test,])
str(test_expo)

saveRDS(train_expo, "../Processed/PesticidesPCBBDEs_train_expo_ndimp.rds")
saveRDS(train_pahs_expo, "../Processed/PAHs_train_expo_ndimp.rds")
saveRDS(test_expo, "../Processed/Overlap_test_expo_ndimp.rds")


### Combine all participants (covariate and outcome information)----
outer = c(train_pest,train_pahs,test)
covar = rbind(train_pest_covar, train_pahs_covar, test_covar)
response = rbind(train_pestY, train_pahsY, testY)

str(covar)
str(response)

covar$data_type = factor(rep(1:3,
                             c(length(train_pest),length(train_pahs),length(test))),
                         labels = c("train_pest","train_pahs","test"))
table(covar$data_type)
table(covar$Centre,covar$data_type)

saveRDS(covar, "../Data/All_participants_covar.rds")
write.csv(covar, "../Exports/Data/All_participants_covar.csv")

saveRDS(response, "../Data/All_participants_response.rds")
write.csv(response, "../Exports/Data/All_participants_response.csv")

