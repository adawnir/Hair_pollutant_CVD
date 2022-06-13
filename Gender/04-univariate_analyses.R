## Univariate analyses: association between exposures and CVDRF outcomes

rm(list=ls())
project_path=dirname(rstudioapi::getActiveDocumentContext()$path)
setwd(project_path)
setwd("../")

library(tidyverse)

filepaths = c("PesticidesPCBBDEs","PAHs","Combined")
strata = c("Female","Male")
for (i in 1:3){
  for (h in 1:2){
    # Load data
    data = readRDS(paste0("../Processed/",filepaths[i],"_XY_",strata[h],".rds"))
    
    # Extract data
    expo = data[c(21:ncol(data))]
    Y = data[,c(16:20)]
    str(Y)
    
    Y = Y %>% mutate_all(as.integer)-1
    str(Y)
    
    # Scale exposures
    expo = scale(expo)
    
    # Logistic regression
    ncol(Y) * ncol(expo) # number of tests
    betas0 = pvals0 = betas1 = pvals1 = NULL
    f0='Y[,k] ~ expo[,j] + data$Age'
    t0=Sys.time()
    for (j in 1:ncol(expo)){
      for (k in 1:ncol(Y)){
        model0=glm(as.formula(f0))
        betas0=c(betas0, coefficients(model0)[2])
        pvals0=c(pvals0, summary(model0)$coefficients[2,4])
      }
    }
    t1=Sys.time()
    print(t1-t0)
    betas0 = matrix(betas0, nrow = ncol(Y), ncol = ncol(expo))
    pvals0 = matrix(pvals0, nrow = ncol(Y), ncol = ncol(expo))
    rownames(pvals0) = rownames(betas0) = colnames(Y)
    colnames(pvals0) = colnames(betas0) = colnames(expo)

    ifelse(dir.exists("../Results/Gender"),"",dir.create("../Results/Gender"))
    saveRDS(pvals0, paste0("../Results/Gender/",filepaths[i],"_univariate_age_pvals_",strata[h],".rds"))
    saveRDS(betas0, paste0("../Results/Gender/",filepaths[i],"_univariate_age_betas_",strata[h],".rds"))
  }
}
