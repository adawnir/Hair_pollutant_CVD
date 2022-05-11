## Univariate analyses: association between exposures and CVDRF outcomes

rm(list=ls())
project_path=dirname(rstudioapi::getActiveDocumentContext()$path)
setwd(project_path)

library(tidyverse)

filepaths = c("PesticidesPCBBDEs","PAHs","Combined")
for (i in 1:3){
  # Load data
  data = readRDS(paste0("../Processed/",filepaths[i],"_XY.rds"))
  
  # Extract data
  expo = data[c(21:ncol(data))]
  Y = data[,c(16:20)]
  str(Y)
  
  Y = Y %>% mutate_all(as.integer)-1
  str(Y)
  
  # Scale exposures
  expo = scale(expo)
  
  # Logistic regession
  ncol(Y) * ncol(expo) # number of tests
  betas0 = pvals0 = betas1 = pvals1 = NULL
  f0='Y[,k] ~ expo[,j] + data$Age + data$Gender'
  f1='Y[,k] ~ expo[,j] + data$Age + data$Gender + data$Country'
  t0=Sys.time()
  for (j in 1:ncol(expo)){
    for (k in 1:ncol(Y)){
      model0=glm(as.formula(f0))
      betas0=c(betas0, coefficients(model0)[2])
      pvals0=c(pvals0, summary(model0)$coefficients[2,4])
      
      model1=glm(as.formula(f1))
      betas1=c(betas1, coefficients(model1)[2])
      pvals1=c(pvals1, summary(model1)$coefficients[2,4])
    }
    }
  t1=Sys.time()
  print(t1-t0)
  betas0 = matrix(betas0, nrow = ncol(Y), ncol = ncol(expo))
  pvals0 = matrix(pvals0, nrow = ncol(Y), ncol = ncol(expo))
  rownames(pvals0) = rownames(betas0) = colnames(Y)
  colnames(pvals0) = colnames(betas0) = colnames(expo)
  
  betas1 = matrix(betas1, nrow = ncol(Y), ncol = ncol(expo))
  pvals1 = matrix(pvals1, nrow = ncol(Y), ncol = ncol(expo))
  rownames(pvals1) = rownames(betas1) = colnames(Y)
  colnames(pvals1) = colnames(betas1) = colnames(expo)
  
  ifelse(dir.exists("../Results"),"",dir.create("../Results"))
  saveRDS(pvals0, paste0("../Results/",filepaths[i],"_univariate_age_gender_pvals.rds"))
  saveRDS(betas0, paste0("../Results/",filepaths[i],"_univariate__age_gender_country_betas.rds"))
  saveRDS(pvals1, paste0("../Results/",filepaths[i],"_univariate_age_gender_country_pvals.rds"))
  saveRDS(betas1, paste0("../Results/",filepaths[i],"_univariate__age_gender_country_betas.rds"))
}
