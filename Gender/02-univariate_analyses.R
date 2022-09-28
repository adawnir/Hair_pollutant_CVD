## Univariate analyses: association between exposures and CVDRF outcomes

rm(list=ls())
project_path=dirname(rstudioapi::getActiveDocumentContext()$path)
setwd(project_path)
setwd("../")

library(tidyverse)

filepaths = c("PesticidesPCBBDEs_train","PAHs_train","Overlap_test")
strata = c("Female","Male")

for (i in 1:3){
  for (h in 1:2){
    # Load data
    data = readRDS(paste0("../Processed/",filepaths[i],"_XY_",strata[h],".rds"))
    
    # Extract data
    expo = data[c(14:ncol(data))]
    Y = data[,c(9:13)]
    str(Y)
    
    Y = Y %>% mutate_all(as.integer)-1
    str(Y)
    
    # Scale exposures
    expo = scale(expo)
    
    # Logistic regression
    ncol(Y) * ncol(expo) # number of tests
    betas0 = se0 = pvals0 = betas1 = se1 = pvals1 = betas2 = se2 = pvals2 = NULL
    f0='Y[,k] ~ expo[,j] + data$Age'
    f1='Y[,k] ~ expo[,j] + data$Age + data$Smoking + data$DietScore1 + data$DietScore2 + data$DietScore3'
    f2='Y[,k] ~ expo[,j] + data$Age + data$Smoking + data$DietScore1 + data$DietScore2  + data$DietScore3+ data$Country'
    t0=Sys.time()
    for (k in 1:ncol(Y)){
      for (j in 1:ncol(expo)){
        model0=glm(as.formula(f0))
        betas0=c(betas0, summary(model0)$coefficients[2,1])
        se0=c(se0, summary(model0)$coefficients[2,2])
        pvals0=c(pvals0, summary(model0)$coefficients[2,4])
        
        model1=glm(as.formula(f1))
        betas1=c(betas1, summary(model1)$coefficients[2,1])
        se1=c(se1, summary(model1)$coefficients[2,2])
        pvals1=c(pvals1, summary(model1)$coefficients[2,4])
        
        model2=glm(as.formula(f2))
        betas2=c(betas2, summary(model2)$coefficients[2,1])
        se2=c(se2, summary(model2)$coefficients[2,2])
        pvals2=c(pvals2, summary(model2)$coefficients[2,4]) 
      }
    }
    t1=Sys.time()
    print(t1-t0)
    betas0 = matrix(betas0, nrow = ncol(Y), ncol = ncol(expo), byrow = TRUE)
    se0 = matrix(se0, nrow = ncol(Y), ncol = ncol(expo), byrow = TRUE)
    pvals0 = matrix(pvals0, nrow = ncol(Y), ncol = ncol(expo), byrow = TRUE)
    rownames(pvals0) = rownames(betas0) = rownames(se0) = colnames(Y)
    colnames(pvals0) = colnames(betas0) = colnames(se0) = colnames(expo)
    
    betas1 = matrix(betas1, nrow = ncol(Y), ncol = ncol(expo), byrow = TRUE)
    se1 = matrix(se1, nrow = ncol(Y), ncol = ncol(expo), byrow = TRUE)
    pvals1 = matrix(pvals1, nrow = ncol(Y), ncol = ncol(expo), byrow = TRUE)
    rownames(pvals1) = rownames(betas1) = rownames(se1) = colnames(Y)
    colnames(pvals1) = colnames(betas1) = colnames(se1) = colnames(expo)
    
    betas2 = matrix(betas2, nrow = ncol(Y), ncol = ncol(expo), byrow = TRUE)
    se2 = matrix(se2, nrow = ncol(Y), ncol = ncol(expo), byrow = TRUE)
    pvals2 = matrix(pvals2, nrow = ncol(Y), ncol = ncol(expo), byrow = TRUE)
    rownames(pvals2) = rownames(betas2) = rownames(se2) = colnames(Y)
    colnames(pvals2) = colnames(betas2) = colnames(se2) = colnames(expo)
    
    ifelse(dir.exists("../Results"),"",dir.create("../Results"))
    ifelse(dir.exists("../Results/Gender"),"",dir.create("../Results/Gender"))
    saveRDS(pvals0, paste0("../Results/Gender/",filepaths[i],"_univariate_base_pvals_",strata[h],".rds"))
    saveRDS(betas0, paste0("../Results/Gender/",filepaths[i],"_univariate_base_betas_",strata[h],".rds"))
    saveRDS(se0, paste0("../Results/Gender/",filepaths[i],"_univariate_base_se_",strata[h],".rds"))
    
    saveRDS(pvals1, paste0("../Results/Gender/",filepaths[i],"_univariate_adjust_pvals_",strata[h],".rds"))
    saveRDS(betas1, paste0("../Results/Gender/",filepaths[i],"_univariate_adjust_betas_",strata[h],".rds"))
    saveRDS(se1, paste0("../Results/Gender/",filepaths[i],"_univariate_adjust_se_",strata[h],".rds"))
    
    saveRDS(pvals2, paste0("../Results/Gender/",filepaths[i],"_univariate_country_pvals_",strata[h],".rds"))
    saveRDS(betas2, paste0("../Results/Gender/",filepaths[i],"_univariate_country_betas_",strata[h],".rds"))
    saveRDS(se2, paste0("../Results/Gender/",filepaths[i],"_univariate_country_se_",strata[h],".rds"))
    
  }
}
