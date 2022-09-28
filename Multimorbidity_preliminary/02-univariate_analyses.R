## Univariate analyses: association between exposures and number of CVDRF outcomes

rm(list=ls())
project_path=dirname(rstudioapi::getActiveDocumentContext()$path)
setwd(project_path)
setwd("../")

library(tidyverse)

filepaths = c("PesticidesPCBBDEs_train","PAHs_train","Overlap_test")
for (i in 1:3){
  # Load data
  data = readRDS(paste0("../Processed/",filepaths[i],"_XY_Multimorbidity.rds"))
  
  # Extract data
  expo = data[c(15:ncol(data))]
  Y = data$OutcomeScore
  
  # Scale exposures
  expo = scale(expo)
  
  # Logistic regression
  ncol(expo) # number of tests
  betas0 = se0 = pvals0 = betas1 = se1 = pvals1 = betas2 = se2 = pvals2 = NULL
  f0='Y ~ expo[,j] + data$Age + data$Gender'
  f1='Y ~ expo[,j] + data$Age + data$Gender + data$Smoking + data$DietScore1 + data$DietScore2 + data$DietScore3'
  f2='Y ~ expo[,j] + data$Age + data$Gender + data$Smoking + data$DietScore1 + data$DietScore2  + data$DietScore3+ data$Country'
  t0=Sys.time()
  for (j in 1:ncol(expo)){
    model0=lm(as.formula(f0))
    betas0=c(betas0, summary(model0)$coefficients[2,1])
    se0=c(se0, summary(model0)$coefficients[2,2])
    pvals0=c(pvals0, summary(model0)$coefficients[2,4])
    
    model1=lm(as.formula(f1))
    betas1=c(betas1, summary(model1)$coefficients[2,1])
    se1=c(se1, summary(model1)$coefficients[2,2])
    pvals1=c(pvals1, summary(model1)$coefficients[2,4])
    
    model2=glm(as.formula(f2))
    betas2=c(betas2, summary(model2)$coefficients[2,1])
    se2=c(se2, summary(model2)$coefficients[2,2])
    pvals2=c(pvals2, summary(model2)$coefficients[2,4]) 
  }
  t1=Sys.time()
  print(t1-t0)
  names(pvals0) = names(betas0) = names(se0) = colnames(expo)
  names(pvals1) = names(betas1) = names(se1) = colnames(expo)
  names(pvals2) = names(betas2) = names(se2) = colnames(expo)
  
  ifelse(dir.exists("../Results"),"",dir.create("../Results"))
  ifelse(dir.exists("../Results/Multimorbidity"),"",dir.create("../Results/Multimorbidity"))
  saveRDS(pvals0, paste0("../Results/Multimorbidity/",filepaths[i],"_univariate_base_pvals.rds"))
  saveRDS(betas0, paste0("../Results/Multimorbidity/",filepaths[i],"_univariate_base_betas.rds"))
  saveRDS(se0, paste0("../Results/Multimorbidity/",filepaths[i],"_univariate_base_se.rds"))
  
  saveRDS(pvals1, paste0("../Results/Multimorbidity/",filepaths[i],"_univariate_adjust_pvals.rds"))
  saveRDS(betas1, paste0("../Results/Multimorbidity/",filepaths[i],"_univariate_adjust_betas.rds"))
  saveRDS(se1, paste0("../Results/Multimorbidity/",filepaths[i],"_univariate_adjust_se.rds"))
  
  saveRDS(pvals2, paste0("../Results/Multimorbidity/",filepaths[i],"_univariate_country_pvals.rds"))
  saveRDS(betas2, paste0("../Results/Multimorbidity/",filepaths[i],"_univariate_country_betas.rds"))
  saveRDS(se2, paste0("../Results/Multimorbidity/",filepaths[i],"_univariate_country_se.rds"))
  
}
