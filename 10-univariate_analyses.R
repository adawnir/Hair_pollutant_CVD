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
  data = readRDS(paste0("../Processed/",filepaths[i],"_XY.rds"))
  
  # Extract data
  expo = data[c(27:ncol(data))]
  X = data[,c(4,3,5,7)] # Age, Gender, Smoking and Education
  str(X)
  X = cbind(X[,1:2],model.matrix(~ Smoking + Education, X)[,-1])
  X = X %>% mutate_if(is.factor, function(x) as.integer(x)-1)
  
  # Scale exposures
  expo = scale(expo)
  
  # Linear regression
  ncol(X) * ncol(expo) # number of tests
  betas0 = se0 = pvals0 = NULL
  f0='expo[,j] ~ X[,k]'
  t0=Sys.time()
  for (k in 1:ncol(X)){
    for (j in 1:ncol(expo)){
      model0=lm(as.formula(f0))
      betas0=c(betas0, summary(model0)$coefficients[2,1])
      se0=c(se0, summary(model0)$coefficients[2,2])
      pvals0=c(pvals0, summary(model0)$coefficients[2,4])
    }
  }
  t1=Sys.time()
  print(t1-t0)
  betas0 = matrix(betas0, nrow = ncol(X), ncol = ncol(expo), byrow = TRUE)
  se0 = matrix(se0, nrow = ncol(X), ncol = ncol(expo), byrow = TRUE)
  pvals0 = matrix(pvals0, nrow = ncol(X), ncol = ncol(expo), byrow = TRUE)
  rownames(pvals0) = rownames(betas0) = rownames(se0) = colnames(X)
  colnames(pvals0) = colnames(betas0) = colnames(se0) = colnames(expo)
  
  ifelse(dir.exists("../Results/Univariate/"),"",dir.create("../Results/Univariate/"))
  ifelse(dir.exists("../Exports/Univariate/"),"",dir.create("../Exports/Univariate/"))
  
  saveRDS(pvals0, paste0("../Results/Univariate/Expo_covar_pvals_",filepaths[i],".rds"))
  saveRDS(betas0, paste0("../Results/Univariate/Expo_covar_betas_",filepaths[i],".rds"))
  saveRDS(se0, paste0("../Results/Univariate/Expo_covar_se_",filepaths[i],".rds"))
  
  bonf = 0.05/ncol(pvals0)
  mytable = NULL
  for (r in 1:nrow(pvals0)){
    or = paste0(formatC(exp(betas0[r,]), format="f", digits=2),
                " (",formatC(exp(betas0[r,]-qnorm(0.975)*se0[r,]), format="f", digits=2),
                ",",formatC(exp(betas0[r,]+qnorm(0.975)*se0[r,]), format="f", digits=2),")")
    pval = formatC(pvals0[r,], format = "e", digits = 2)
    signif = ifelse(pvals0[r,] < bonf, "*","")
    mytable = cbind(mytable,or,pval,signif)
  }
  dim(mytable)
  mytable = rbind(rep(c("OR (95% CI)","P-value","Bonf."), nrow(pvals0)), mytable)
  colnames(mytable) = rep(rownames(pvals0), each = 3)
  SaveExcelWithSuperscripts(cbind(c("",colnames(pvals0)), mytable), paste0("../Exports/Univariate/Expo_covar_summary_",filepaths[i],".xlsx"))
}
## Binary outcome ----
for (i in 1:3){
  # Load data
  data = readRDS(paste0("../Processed/",filepaths[i],"_XY.rds"))
  
  # Extract data
  expo = data[c(27:ncol(data))]
  Y = data[,c(12:16)]
  str(Y)
  
  Y = Y %>% mutate_if(is.factor, function(x) as.integer(x)-1)
  str(Y)
  
  # Scale exposures
  expo = scale(expo)
  
  # Logistic regression
  ncol(Y) * ncol(expo) # number of tests
  f0='Y[,k] ~ expo[,j] + data$Age + data$Gender'
  f1='Y[,k] ~ expo[,j] + data$Age + data$Gender + data$Smoking + data$DietScore1 + data$DietScore2 + data$DietScore3'
  f2='Y[,k] ~ expo[,j] + data$Age + data$Gender + data$Smoking + data$DietScore1 + data$DietScore2  + data$DietScore3 + data$Country'
  f3='Y[,k] ~ expo[,j] + data$Age + data$Gender + data$Smoking + data$DietScore1 + data$DietScore2  + data$DietScore3 + data$Education'
  f4='Y[,k] ~ expo[,j] + data$Age + data$Gender + data$Smoking + data$DietScore1 + data$DietScore2  + data$DietScore3 + data$Education + data$Physical_activity + data$Alcohol'
  
  t0=Sys.time()
  for (f in 0:4){
    betas = se = pvals = NULL
    for (k in 1:ncol(Y)){
      for (j in 1:ncol(expo)){
        formula = eval(parse(text = paste0("f",f)))
        model=glm(as.formula(formula))
        betas=c(betas, summary(model)$coefficients[2,1])
        se=c(se, summary(model)$coefficients[2,2])
        pvals=c(pvals, summary(model)$coefficients[2,4])
      }
    }
    betas = matrix(betas, nrow = ncol(Y), ncol = ncol(expo), byrow = TRUE)
    se = matrix(se, nrow = ncol(Y), ncol = ncol(expo), byrow = TRUE)
    pvals = matrix(pvals, nrow = ncol(Y), ncol = ncol(expo), byrow = TRUE)
    rownames(pvals) = rownames(betas) = rownames(se) = colnames(Y)
    colnames(pvals) = colnames(betas) = colnames(se) = colnames(expo)
    
    saveRDS(pvals, paste0("../Results/Univariate/Binary_outcome_expo_f",f,"_pvals_",filepaths[i],".rds"))
    saveRDS(betas, paste0("../Results/Univariate/Binary_outcome_expo_f",f,"_betas_",filepaths[i],".rds"))
    saveRDS(se, paste0("../Results/Univariate/Binary_outcome_expo_f",f,"_se_",filepaths[i],".rds"))
    
    bonf = 0.05/ncol(pvals)
    mytable = NULL
    for (r in 1:nrow(pvals)){
      or = paste0(formatC(exp(betas[r,]), format="f", digits=2),
                  " (",formatC(exp(betas[r,]-qnorm(0.975)*se[r,]), format="f", digits=2),
                  ",",formatC(exp(betas[r,]+qnorm(0.975)*se[r,]), format="f", digits=2),")")
      pval = formatC(pvals[r,], format = "e", digits = 2)
      signif = ifelse(pvals[r,] < bonf, "*","")
      mytable = cbind(mytable,or,pval,signif)
    }
    dim(mytable)
    mytable = rbind(rep(c("OR (95% CI)","P-value","Bonf."), nrow(pvals)), mytable)
    colnames(mytable) = rep(rownames(pvals), each = 3)
    SaveExcelWithSuperscripts(cbind(c("",colnames(pvals)), mytable), paste0("../Exports/Univariate/Binary_outcome_expo_f",f,"_summary_",filepaths[i],".xlsx"))
    
    # assign(paste0("betas",f),betas)
    # assign(paste0("se",f),se)
    # assign(paste0("pvals",f),pvals)
  }
  t1=Sys.time()
  print(t1-t0)
}
## Continuous outcome ----
for (i in 1:3){
  # Load data
  data = readRDS(paste0("../Processed/",filepaths[i],"_XY.rds"))
  
  # Extract data
  expo = data[c(27:ncol(data))]
  Y = data[,c(17:25)]
  str(Y)
  
  # Scale outcomes
  Y = scale(Y)
  
  # Scale exposures
  expo = scale(expo)
  
  # Linear regression
  ncol(Y) * ncol(expo) # number of tests
  f0='Y[,k] ~ expo[,j] + data$Age + data$Gender'
  f1='Y[,k] ~ expo[,j] + data$Age + data$Gender + data$Smoking + data$DietScore1 + data$DietScore2 + data$DietScore3'
  f2='Y[,k] ~ expo[,j] + data$Age + data$Gender + data$Smoking + data$DietScore1 + data$DietScore2  + data$DietScore3 + data$Country'
  f3='Y[,k] ~ expo[,j] + data$Age + data$Gender + data$Smoking + data$DietScore1 + data$DietScore2  + data$DietScore3 + data$Education'
  f4='Y[,k] ~ expo[,j] + data$Age + data$Gender + data$Smoking + data$DietScore1 + data$DietScore2  + data$DietScore3 + data$Education + data$Physical_activity + data$Alcohol'
  
  t0=Sys.time()
  for (f in 0:4){
    betas = se = pvals = NULL
    for (k in 1:ncol(Y)){
      for (j in 1:ncol(expo)){
        formula = eval(parse(text = paste0("f",f)))
        model=lm(as.formula(formula))
        betas=c(betas, summary(model)$coefficients[2,1])
        se=c(se, summary(model)$coefficients[2,2])
        pvals=c(pvals, summary(model)$coefficients[2,4])
      }
    }
    betas = matrix(betas, nrow = ncol(Y), ncol = ncol(expo), byrow = TRUE)
    se = matrix(se, nrow = ncol(Y), ncol = ncol(expo), byrow = TRUE)
    pvals = matrix(pvals, nrow = ncol(Y), ncol = ncol(expo), byrow = TRUE)
    rownames(pvals) = rownames(betas) = rownames(se) = colnames(Y)
    colnames(pvals) = colnames(betas) = colnames(se) = colnames(expo)
    
    saveRDS(pvals, paste0("../Results/Univariate/Continuous_outcome_expo_f",f,"_pvals_",filepaths[i],".rds"))
    saveRDS(betas, paste0("../Results/Univariate/Continuous_outcome_expo_f",f,"_betas_",filepaths[i],".rds"))
    saveRDS(se, paste0("../Results/Univariate/Continuous_outcome_expo_f",f,"_se_",filepaths[i],".rds"))
    
    bonf = 0.05/ncol(pvals)
    mytable = NULL
    for (r in 1:nrow(pvals)){
      or = paste0(formatC(betas[r,], format="f", digits=2),
                  " (",formatC(betas[r,]-qnorm(0.975)*se[r,], format="f", digits=2),
                  ",",formatC(betas[r,]+qnorm(0.975)*se[r,], format="f", digits=2),")")
      pval = formatC(pvals[r,], format = "e", digits = 2)
      signif = ifelse(pvals[r,] < bonf, "*","")
      mytable = cbind(mytable,or,pval,signif)
    }
    dim(mytable)
    mytable = rbind(rep(c("Beta (95% CI)","P-value","Bonf."), nrow(pvals)), mytable)
    colnames(mytable) = rep(rownames(pvals), each = 3)
    SaveExcelWithSuperscripts(cbind(c("",colnames(pvals)), mytable), paste0("../Exports/Univariate/Continuous_outcome_expo_f",f,"_summary_",filepaths[i],".xlsx"))
    
    # assign(paste0("betas",f),betas)
    # assign(paste0("se",f),se)
    # assign(paste0("pvals",f),pvals)
  }
  t1=Sys.time()
  print(t1-t0)
}
