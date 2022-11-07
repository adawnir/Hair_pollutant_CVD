## Univariate analyses: association between exposures and covariates

rm(list=ls())
project_path=dirname(rstudioapi::getActiveDocumentContext()$path)
setwd(project_path)

library(tidyverse)
source("functions.R")
filepaths = c("PesticidesPCBBDEs_train","PAHs_train","Overlap_test")

## Exposure ~ Social and Behavioural risk factors----
for (i in 1:3){
  # Load data
  Z = readRDS(paste0("../Processed/",filepaths[i],"_Z.rds"))
  X = readRDS(paste0("../Processed/",filepaths[i],"_expo_ndimp_naimp_log.rds"))
  Z = Z %>% select(Age, Gender, Centre, Education, Smoking, starts_with("Diet"),
                   Physical_activity, Alcohol)
  Z_mat = model.matrix(~ ., Z)[,-1]
  
  # Linear regression
  f0='X[,k] ~ Z[,j]'
  
  t0=Sys.time()
  betas = se = pvals = NULL
  for (k in 1:ncol(X)){
    for (j in 1:ncol(Z)){
      model=lm(as.formula(f0), data = Z)
      if(is.factor(Z[,j])){
        n.levels = length(levels(Z[,j]))-1
        betas=c(betas, summary(model)$coefficients[seq(2,bX = 1,length = n.levels),1])
        se=c(se, summary(model)$coefficients[seq(2,bX = 1,length = n.levels),2])
        pvals=c(pvals, summary(model)$coefficients[seq(2,bX = 1,length = n.levels),4])
      } else if (is.numeric(Z[,j])){
        betas=c(betas, summary(model)$coefficients[2,1])
        se=c(se, summary(model)$coefficients[2,2])
        pvals=c(pvals, summary(model)$coefficients[2,4])
      }
    }
  }
  betas = matrix(betas, nrow = ncol(X), ncol = ncol(Z_mat), byrow = TRUE)
  se = matrix(se, nrow = ncol(X), ncol = ncol(Z_mat), byrow = TRUE)
  pvals = matrix(pvals, nrow = ncol(X), ncol = ncol(Z_mat), byrow = TRUE)
  rownames(pvals) = rownames(betas) = rownames(se) = colnames(X)
  colnames(pvals) = colnames(betas) = colnames(se) = colnames(Z_mat)
  
  saveRDS(pvals, paste0("../Results/Univariate/X_Z_pvals_",filepaths[i],".rds"))
  saveRDS(betas, paste0("../Results/Univariate/X_Z_betas_",filepaths[i],".rds"))
  saveRDS(se, paste0("../Results/Univariate/X_Z_se_",filepaths[i],".rds"))
  
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
  SaveExcelWithSuperscripts(cbind(c("",rownames(pvals)), mytable), paste0("../Exports/Univariate/X_Z_summary_",filepaths[i],".xlsx"))
  
}


## Exposure and Education ----
for (i in 1:3){
  # Load data
  X = readRDS(paste0("../Processed/",filepaths[i],"_expo_ndimp_naimp_log.rds"))
  Z = readRDS(paste0("../Processed/",filepaths[i],"_Z.rds"))
  colnames(Z)
  
  Z = Z %>% select(Education, Age, Gender, Centre)
  Z = model.matrix(~ ., Z)[,-1]
  
  # Linear regression
  f0='X[,j] ~ Z[,1:4]'
  f1='X[,j] ~ Z[,1:5]'
  t0=Sys.time()
  mytable = NULL
  for (f in 0:1){
    betas = se = pvals = NULL
      for (j in 1:ncol(X)){
        formula = eval(parse(text = paste0("f",f)))
        model=lm(as.formula(formula))
        betas=c(betas, summary(model)$coefficients[2:3,1])
        se=c(se, summary(model)$coefficients[2:3,2])
        pvals=c(pvals, summary(model)$coefficients[2:3,4])
      }
    betas = matrix(betas, nrow = ncol(X), ncol = 2, byrow = TRUE)
    se = matrix(se, nrow = ncol(X), ncol = 2, byrow = TRUE)
    pvals = matrix(pvals, nrow = ncol(X), ncol = 2, byrow = TRUE)
    rownames(pvals) = rownames(betas) = rownames(se) = colnames(X)
    colnames(pvals) = colnames(betas) = colnames(se) = colnames(Z)[1:2]
    
    saveRDS(pvals, paste0("../Results/Univariate/X_Education_f",f,"_pvals_",filepaths[i],".rds"))
    saveRDS(betas, paste0("../Results/Univariate/X_Education_f",f,"_betas_",filepaths[i],".rds"))
    saveRDS(se, paste0("../Results/Univariate/X_Education_f",f,"_se_",filepaths[i],".rds"))
  
    bonf = 0.05/nrow(pvals)
    for (c in 1:ncol(pvals)){
      beta = paste0(formatC(betas[,c], format="f", digits=2),
                    " (",formatC(betas[,c]-qnorm(0.975)*se[,c], format="f", digits=2),
                    ",",formatC(betas[,c]+qnorm(0.975)*se[,c], format="f", digits=2),")")
      pval = formatC(pvals[,c], format = "e", digits = 2)
      signif = ifelse(pvals[,c] < bonf, "*","")
      mytable = cbind(mytable,beta,pval,signif)
    }
  }
  t1=Sys.time()
  print(t1-t0)
  colnames(mytable) = c(rep(colnames(pvals), each = 3), paste0(rep(colnames(pvals), each = 3), "*"))
  mytable = rbind(rep(c("Beta coefficient (95% CI)","P-value","Bonf."), 4), mytable)
  SaveExcelWithSuperscripts(cbind(c("",rownames(pvals)), mytable), paste0("../Exports/Univariate/X_Education_summary_",filepaths[i],".xlsx"))
}

## Exposure and Education + Covariates ----
for (i in 1:3){
  # Load data
  X = readRDS(paste0("../Processed/",filepaths[i],"_expo_ndimp_naimp_log.rds"))
  Z = readRDS(paste0("../Processed/",filepaths[i],"_Z.rds"))
  colnames(Z)
  
  Z = Z %>% select(Education, Age, Gender, Smoking, starts_with("Diet"),
                   Physical_activity, Alcohol)
  Z = model.matrix(~ ., Z)[,-1]
  
  # Linear regression
  f0='X[,j] ~ Z[,1:6]'
  f1='X[,j] ~ Z[,1:9]'
  f2='X[,j] ~ Z[,1:11]'
  f3='X[,j] ~ Z[,1:13]'
  t0=Sys.time()
  mytable = NULL
  for (f in 0:3){
    betas = se = pvals = NULL
    for (j in 1:ncol(X)){
      formula = eval(parse(text = paste0("f",f)))
      model=lm(as.formula(formula))
      betas=c(betas, summary(model)$coefficients[2:3,1])
      se=c(se, summary(model)$coefficients[2:3,2])
      pvals=c(pvals, summary(model)$coefficients[2:3,4])
    }
    betas = matrix(betas, nrow = ncol(X), ncol = 2, byrow = TRUE)
    se = matrix(se, nrow = ncol(X), ncol = 2, byrow = TRUE)
    pvals = matrix(pvals, nrow = ncol(X), ncol = 2, byrow = TRUE)
    rownames(pvals) = rownames(betas) = rownames(se) = colnames(X)
    colnames(pvals) = colnames(betas) = colnames(se) = colnames(Z)[1:2]
    
    saveRDS(pvals, paste0("../Results/Univariate/X_Education_Z_f",f,"_pvals_",filepaths[i],".rds"))
    saveRDS(betas, paste0("../Results/Univariate/X_Education_Z_f",f,"_betas_",filepaths[i],".rds"))
    saveRDS(se, paste0("../Results/Univariate/X_Education_Z_f",f,"_se_",filepaths[i],".rds"))
    
    bonf = 0.05/nrow(pvals)
    for (c in 1:ncol(pvals)){
      beta = paste0(formatC(betas[,c], format="f", digits=2),
                    " (",formatC(betas[,c]-qnorm(0.975)*se[,c], format="f", digits=2),
                    ",",formatC(betas[,c]+qnorm(0.975)*se[,c], format="f", digits=2),")")
      pval = formatC(pvals[,c], format = "e", digits = 2)
      signif = ifelse(pvals[,c] < bonf, "*","")
      mytable = cbind(mytable,beta,pval,signif)
    }
  }
  t1=Sys.time()
  print(t1-t0)
  colnames(mytable) =  rep(paste0(rep(colnames(pvals),4),rep(c(" (f0)"," (f1)"," (f2)"," (f3)"),each = 2)),each = 3)
  mytable = rbind(rep(c("Beta coefficient (95% CI)","P-value","Bonf."), 4), mytable)
  SaveExcelWithSuperscripts(cbind(c("",rownames(pvals)), mytable), paste0("../Exports/Univariate/X_Education_Z_summary_",filepaths[i],".xlsx"))
}
