## Univariate analyses: association between exposures and CVDRF outcomes

rm(list=ls())
project_path=dirname(rstudioapi::getActiveDocumentContext()$path)
setwd(project_path)

library(tidyverse)
source("functions.R")
filepaths = c("PesticidesPCBBDEs_train","PAHs_train","Overlap_test")

## Exploratory: Potential confounders ----
for (i in 1:3){
  # Load data
  Z = readRDS(paste0("../Processed/",filepaths[i],"_Z.rds"))
  X = readRDS(paste0("../Processed/",filepaths[i],"_expo_ndimp_naimp_log.rds"))
  
  Z = model.matrix(~ ., Z)[,-1]

  # Scale exposures
  X = scale(X)
  
  # Linear regression
  ncol(Z) * ncol(X) # number of tests
  betas0 = se0 = pvals0 = NULL
  f0='X[,j] ~ Z[,k]'
  t0=Sys.time()
  for (k in 1:ncol(Z)){
    for (j in 1:ncol(X)){
      model0=lm(as.formula(f0))
      betas0=c(betas0, summary(model0)$coefficients[2,1])
      se0=c(se0, summary(model0)$coefficients[2,2])
      pvals0=c(pvals0, summary(model0)$coefficients[2,4])
    }
  }
  t1=Sys.time()
  print(t1-t0)
  betas0 = matrix(betas0, nrow = ncol(Z), ncol = ncol(X))
  se0 = matrix(se0, nrow = ncol(Z), ncol = ncol(X))
  pvals0 = matrix(pvals0, nrow = ncol(Z), ncol = ncol(X))
  rownames(pvals0) = rownames(betas0) = rownames(se0) = colnames(Z)
  colnames(pvals0) = colnames(betas0) = colnames(se0) = colnames(X)
  
  ifelse(dir.exists("../Results/Univariate/"),"",dir.create("../Results/Univariate/"))
  ifelse(dir.exists("../Exports/Univariate/"),"",dir.create("../Exports/Univariate/"))
  
  saveRDS(pvals0, paste0("../Results/Univariate/Expo_covar_pvals_",filepaths[i],".rds"))
  saveRDS(betas0, paste0("../Results/Univariate/Expo_covar_betas_",filepaths[i],".rds"))
  saveRDS(se0, paste0("../Results/Univariate/Expo_covar_se_",filepaths[i],".rds"))
  
  bonf = 0.05/ncol(pvals0)
  mytable = NULL
  for (r in 1:nrow(pvals0)){
    beta = paste0(formatC(betas0[r,], format="f", digits=2),
                " (",formatC(betas0[r,]-qnorm(0.975)*se0[r,], format="f", digits=2),
                ",",formatC(betas0[r,]+qnorm(0.975)*se0[r,], format="f", digits=2),")")
    pval = formatC(pvals0[r,], format = "e", digits = 2)
    signif = ifelse(pvals0[r,] < bonf, "*","")
    mytable = cbind(mytable,beta,pval,signif)
  }
  dim(mytable)
  mytable = rbind(rep(c("Beta coefficient (95% CI)","P-value","Bonf."), ncol(pvals0)), mytable)
  colnames(mytable) = rep(rownames(pvals0), each = 3)
  SaveExcelWithSuperscripts(cbind(c("",colnames(pvals0)), mytable), paste0("../Exports/Univariate/Expo_covar_summary_",filepaths[i],".xlsx"))
}
## Response ----
for (i in 1:3){
  # Load data
  X = readRDS(paste0("../Processed/",filepaths[i],"_expo_ndimp_naimp_log.rds"))
  Z = readRDS(paste0("../Processed/",filepaths[i],"_Z.rds"))
  
  Y = readRDS(paste0("../Processed/",filepaths[i],"_Y.rds"))
  levels = Y %>% select(where(is.factor)) %>% sapply(function(x) length(levels(x)))
  keep = names(levels)[levels == 2]
  Y = Y %>% select(where(is.numeric), all_of(keep))
  
  # Scale exposures
  X = scale(X)
  
  # Logistic regression
  ncol(Y) * ncol(X) # number of tests
  f0='Y[,k] ~ X[,j] + Age + Gender'
  f1='Y[,k] ~ X[,j] + Age + Gender + Smoking + DietScore1 + DietScore2 + DietScore3'
  f2='Y[,k] ~ X[,j] + Age + Gender + Smoking + DietScore1 + DietScore2  + DietScore3 + Education'
  f3='Y[,k] ~ X[,j] + Age + Gender + Smoking + DietScore1 + DietScore2  + DietScore3 + Education + Physical_activity + Alcohol'
  
  f4='Y[,k] ~ X[,j] + Age + Gender + Country'
  f5='Y[,k] ~ X[,j] + Age + Gender + Smoking + DietScore1 + DietScore2 + DietScore3 + Country'
  f6='Y[,k] ~ X[,j] + Age + Gender + Smoking + DietScore1 + DietScore2  + DietScore3 + Education + Country'
  f7='Y[,k] ~ X[,j] + Age + Gender + Smoking + DietScore1 + DietScore2  + DietScore3 + Education + Physical_activity + Alcohol + Country'
  
  t0=Sys.time()
  for (f in 0:7){
    betas = se = pvals = NULL
    for (k in 1:ncol(Y)){
      for (j in 1:ncol(X)){
        formula = eval(parse(text = paste0("f",f)))
        if(is.factor(Y[,k])){
          model=glm(as.formula(formula), data = Z, family = "binomial")
        } else if (is.numeric(Y[,k])){
          model=lm(as.formula(formula), data = Z)
        }
        betas=c(betas, summary(model)$coefficients[2,1])
        se=c(se, summary(model)$coefficients[2,2])
        pvals=c(pvals, summary(model)$coefficients[2,4])
      }
    }
    betas = matrix(betas, nrow = ncol(Y), ncol = ncol(X), byrow = TRUE)
    se = matrix(se, nrow = ncol(Y), ncol = ncol(X), byrow = TRUE)
    pvals = matrix(pvals, nrow = ncol(Y), ncol = ncol(X), byrow = TRUE)
    rownames(pvals) = rownames(betas) = rownames(se) = colnames(Y)
    colnames(pvals) = colnames(betas) = colnames(se) = colnames(X)
    
    saveRDS(pvals, paste0("../Results/Univariate/Response_X_f",f,"_pvals_",filepaths[i],".rds"))
    saveRDS(betas, paste0("../Results/Univariate/Response_outcome_X_f",f,"_betas_",filepaths[i],".rds"))
    saveRDS(se, paste0("../Results/Univariate/Response_outcome_X_f",f,"_se_",filepaths[i],".rds"))
    
    bonf = 0.05/ncol(pvals)
    mytable = NULL
    for (r in 1:nrow(pvals)){
      beta = paste0(formatC(betas[r,], format="f", digits=2),
                  " (",formatC(betas[r,]-qnorm(0.975)*se[r,], format="f", digits=2),
                  ",",formatC(betas[r,]+qnorm(0.975)*se[r,], format="f", digits=2),")")
      pval = formatC(pvals[r,], format = "e", digits = 2)
      signif = ifelse(pvals[r,] < bonf, "*","")
      mytable = cbind(mytable,beta,pval,signif)
    }
    dim(mytable)
    mytable = rbind(rep(c("Beta coefficient (95% CI)","P-value","Bonf."), nrow(pvals)), mytable)
    colnames(mytable) = rep(rownames(pvals), each = 3)
    SaveExcelWithSuperscripts(cbind(c("",colnames(pvals)), mytable), paste0("../Exports/Univariate/Response_expo_f",f,"_summary_",filepaths[i],".xlsx"))
    
    # assign(paste0("betas",f),betas)
    # assign(paste0("se",f),se)
    # assign(paste0("pvals",f),pvals)
  }
  t1=Sys.time()
  print(t1-t0)
}
