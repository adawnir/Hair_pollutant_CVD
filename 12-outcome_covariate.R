## Univariate analyses: association between outcomes and covariates

rm(list=ls())
project_path=dirname(rstudioapi::getActiveDocumentContext()$path)
setwd(project_path)

library(tidyverse)
source("functions.R")
filepaths = c("PesticidesPCBBDEs_train","PAHs_train","Overlap_test")

## Response ~ Social and Behavioural risk factors ----
for (i in 1:3){
  # Load data
  Z = readRDS(paste0("../Processed/",filepaths[i],"_Z.rds"))
  Y = readRDS(paste0("../Data/",filepaths[i],"_response.rds"))
  
  Y = Y %>% select(BMI, WC, TG, TC, LDLC, HDLC, SBP, DBP, FPG)
  
  Z = Z %>% select(Age, Gender, Country, Education, Smoking, starts_with("Diet"),
                   Physical_activity, Alcohol)
  Z_mat = model.matrix(~ ., Z)[,-1]
  
  # Linear regression
  f0='Y[,k] ~ Z[,j]'
  
  t0=Sys.time()
  betas = se = pvals = NULL
  for (k in 1:ncol(Y)){
    for (j in 1:ncol(Z)){
      model=lm(as.formula(f0), data = Z)
      if(is.factor(Z[,j])){
        n.levels = length(levels(Z[,j]))-1
        betas=c(betas, summary(model)$coefficients[seq(2,by = 1,length = n.levels),1])
        se=c(se, summary(model)$coefficients[seq(2,by = 1,length = n.levels),2])
        pvals=c(pvals, summary(model)$coefficients[seq(2,by = 1,length = n.levels),4])
      } else if (is.numeric(Z[,j])){
        betas=c(betas, summary(model)$coefficients[2,1])
        se=c(se, summary(model)$coefficients[2,2])
        pvals=c(pvals, summary(model)$coefficients[2,4])
      }
    }
  }
  betas = matrix(betas, nrow = ncol(Y), ncol = ncol(Z_mat), byrow = TRUE)
  se = matrix(se, nrow = ncol(Y), ncol = ncol(Z_mat), byrow = TRUE)
  pvals = matrix(pvals, nrow = ncol(Y), ncol = ncol(Z_mat), byrow = TRUE)
  rownames(pvals) = rownames(betas) = rownames(se) = colnames(Y)
  colnames(pvals) = colnames(betas) = colnames(se) = colnames(Z_mat)
  
  saveRDS(pvals, paste0("../Results/Univariate/Y_Z_pvals_",filepaths[i],".rds"))
  saveRDS(betas, paste0("../Results/Univariate/Y_Z_betas_",filepaths[i],".rds"))
  saveRDS(se, paste0("../Results/Univariate/Y_Z_se_",filepaths[i],".rds"))
  
  bonf = 0.05/nrow(pvals)
  mytable = NULL
  for (c in 1:ncol(pvals)){
    beta = paste0(formatC(betas[,c], format="f", digits=2),
                  " (",formatC(betas[,c]-qnorm(0.975)*se[,c], format="f", digits=2),
                  ",",formatC(betas[,c]+qnorm(0.975)*se[,c], format="f", digits=2),")")
    pval = formatC(pvals[,c], format = "e", digits = 2)
    signif = ifelse(pvals[,c] < bonf, "*","")
    mytable = cbind(mytable,beta,pval,signif)
  }
  dim(mytable)
  mytable = rbind(rep(c("Beta coefficient (95% CI)","P-value","Bonf."), ncol(pvals)), mytable)
  colnames(mytable) = rep(colnames(pvals), each = 3)
  SaveExcelWithSuperscripts(cbind(c("",rownames(pvals)), mytable), paste0("../Exports/Univariate/Y_Z_summary_",filepaths[i],".xlsx"))
}

