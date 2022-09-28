## Univariate analyses: association between exposures and clustered outcome

rm(list=ls())
project_path=dirname(rstudioapi::getActiveDocumentContext()$path)
setwd(project_path)
setwd("../")

library(tidyverse)
library(nnet)

filepaths = c("PesticidesPCBBDEs_train","PAHs_train","Overlap_test")
for (i in 1:2){
  # Load data
  data = readRDS(paste0("../Processed/",filepaths[i],"_XY_Comorbidity.rds"))
  
  # Extract data
  expo = data[c(15:ncol(data))]
  Y = data$OutcomeCluster
  
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
    model0=multinom(as.formula(f0))
    z = summary(model0)$coefficients/summary(model0)$standard.errors
    p = (1 - pnorm(abs(z), 0, 1))*2 # we are using two-tailed z test
    betas0=cbind(betas0, summary(model0)$coefficients[,2])
    se0=cbind(se0, summary(model0)$standard.errors[,2])
    pvals0=cbind(pvals0, p[,2])
    
    model1=multinom(as.formula(f1))
    z = summary(model1)$coefficients/summary(model1)$standard.errors
    p = (1 - pnorm(abs(z), 0, 1))*2 # we are using two-tailed z test
    betas1=cbind(betas1, summary(model1)$coefficients[,2])
    se1=cbind(se1, summary(model1)$standard.errors[,2])
    pvals1=cbind(pvals1, p[,2])
    
    model2=multinom(as.formula(f2))
    z = summary(model2)$coefficients/summary(model2)$standard.errors
    p = (1 - pnorm(abs(z), 0, 1))*2 # we are using two-tailed z test
    betas2=cbind(betas2, summary(model2)$coefficients[,2])
    se2=cbind(se2, summary(model2)$standard.errors[,2])
    pvals2=cbind(pvals2, p[,2])
  }
  t1=Sys.time()
  print(t1-t0)
  colnames(pvals0) = colnames(betas0) = colnames(se0) = colnames(expo)
  colnames(pvals1) = colnames(betas1) = colnames(se1) = colnames(expo)
  colnames(pvals2) = colnames(betas2) = colnames(se2) = colnames(expo)
  
  ifelse(dir.exists("../Results"),"",dir.create("../Results"))
  ifelse(dir.exists("../Results/Comorbidity"),"",dir.create("../Results/Comorbidity"))
  saveRDS(pvals0, paste0("../Results/Comorbidity/",filepaths[i],"_univariate_base_pvals.rds"))
  saveRDS(betas0, paste0("../Results/Comorbidity/",filepaths[i],"_univariate_base_betas.rds"))
  saveRDS(se0, paste0("../Results/Comorbidity/",filepaths[i],"_univariate_base_se.rds"))
  
  saveRDS(pvals1, paste0("../Results/Comorbidity/",filepaths[i],"_univariate_adjust_pvals.rds"))
  saveRDS(betas1, paste0("../Results/Comorbidity/",filepaths[i],"_univariate_adjust_betas.rds"))
  saveRDS(se1, paste0("../Results/Comorbidity/",filepaths[i],"_univariate_adjust_se.rds"))
  
  saveRDS(pvals2, paste0("../Results/Comorbidity/",filepaths[i],"_univariate_country_pvals.rds"))
  saveRDS(betas2, paste0("../Results/Comorbidity/",filepaths[i],"_univariate_country_betas.rds"))
  saveRDS(se2, paste0("../Results/Comorbidity/",filepaths[i],"_univariate_country_se.rds"))
  
}
