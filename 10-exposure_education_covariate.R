## Univariate analyses: association between exposures and covariates

rm(list=ls())
project_path=dirname(rstudioapi::getActiveDocumentContext()$path)
setwd(project_path)

library(tidyverse)
source("functions.R")
filepaths = c("PesticidesPCBBDEs_train","PAHs_train","Overlap_test")

## Exploratory----
for (i in 1:3){
  # Load data
  Z = readRDS(paste0("../Processed/",filepaths[i],"_Z.rds"))
  X = readRDS(paste0("../Processed/",filepaths[i],"_expo_ndimp_naimp_log.rds"))
  colnames(Z)
  
  Z = model.matrix(~ ., Z)[,-1]
  
  colnames(Z)
  
  # Linear regression
  f0='X[,j] ~  Z[,k]'
  
  t0=Sys.time()
  betas = se = pvals = NULL
  for (j in 1:ncol(X)){
    for (k in 1:ncol(Z)){
      model=lm(as.formula(f0))
      betas=c(betas, summary(model)$coefficients[2,1])
      se=c(se, summary(model)$coefficients[2,2])
      pvals=c(pvals, summary(model)$coefficients[2,4])
    }
  }
  t1=Sys.time()
  print(t1-t0)
  betas = matrix(betas, nrow = ncol(X), ncol = ncol(Z), byrow = TRUE)
  se = matrix(se, nrow = ncol(X), ncol = ncol(Z), byrow = TRUE)
  pvals = matrix(pvals, nrow = ncol(X), ncol = ncol(Z), byrow = TRUE)
  rownames(pvals) = rownames(betas) = rownames(se) = colnames(X)
  colnames(pvals) = colnames(betas) = colnames(se) = colnames(Z)
  
  saveRDS(pvals, paste0("../Results/Univariate/Exploratory_X_Z_pvals_",filepaths[i],".rds"))
  saveRDS(betas, paste0("../Results/Univariate/Exploratory_X_Z_betas_",filepaths[i],".rds"))
  saveRDS(se, paste0("../Results/Univariate/Exploratory_X_Z_se_",filepaths[i],".rds"))
  
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
  SaveExcelWithSuperscripts(cbind(c("",rownames(pvals)), mytable), paste0("../Exports/Univariate/Exploratory_X_Z_summary_",filepaths[i],".xlsx"))
}

## Exposure and Education ----
for (i in 1:3){
  # Load data
  X = readRDS(paste0("../Processed/",filepaths[i],"_expo_ndimp_naimp_log.rds"))
  Z = readRDS(paste0("../Processed/",filepaths[i],"_Z.rds"))
  colnames(Z)
  
  Z = Z %>% select(Education, Age, Gender, Country)
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

# ## Response ----
# for (i in 1:3){
#   # Load data
#   X = readRDS(paste0("../Processed/",filepaths[i],"_expo_ndimp_naimp_log.rds"))
#   Z = readRDS(paste0("../Processed/",filepaths[i],"_Z.rds"))
#   
#   Y = readRDS(paste0("../Processed/",filepaths[i],"_Y.rds"))
#   levels = Y %>% select(where(is.factor)) %>% sapply(function(x) length(levels(x)))
#   keep = names(levels)[levels == 2]
#   Y = Y %>% select(where(is.numeric), all_of(keep))
#   
#   # Scale exposures
#   X = scale(X)
#   
#   # Logistic regression
#   ncol(Y) * ncol(X) # number of tests
#   f0='Y[,k] ~ X[,j] + Age + Gender'
#   f1='Y[,k] ~ X[,j] + Age + Gender + Smoking + DietScore1 + DietScore2 + DietScore3'
#   f2='Y[,k] ~ X[,j] + Age + Gender + Smoking + DietScore1 + DietScore2  + DietScore3 + Education'
#   f3='Y[,k] ~ X[,j] + Age + Gender + Smoking + DietScore1 + DietScore2  + DietScore3 + Education + Physical_activity + Alcohol'
#   
#   f4='Y[,k] ~ X[,j] + Age + Gender + Country'
#   f5='Y[,k] ~ X[,j] + Age + Gender + Smoking + DietScore1 + DietScore2 + DietScore3 + Country'
#   f6='Y[,k] ~ X[,j] + Age + Gender + Smoking + DietScore1 + DietScore2  + DietScore3 + Education + Country'
#   f7='Y[,k] ~ X[,j] + Age + Gender + Smoking + DietScore1 + DietScore2  + DietScore3 + Education + Physical_activity + Alcohol + Country'
#   
#   t0=Sys.time()
#   for (f in 0:7){
#     betas = se = pvals = NULL
#     for (k in 1:ncol(Y)){
#       for (j in 1:ncol(X)){
#         formula = eval(parse(text = paste0("f",f)))
#         if(is.factor(Y[,k])){
#           model=glm(as.formula(formula), data = Z, family = "binomial")
#         } else if (is.numeric(Y[,k])){
#           model=lm(as.formula(formula), data = Z)
#         }
#         betas=c(betas, summary(model)$coefficients[2,1])
#         se=c(se, summary(model)$coefficients[2,2])
#         pvals=c(pvals, summary(model)$coefficients[2,4])
#       }
#     }
#     betas = matrix(betas, nrow = ncol(Y), ncol = ncol(X), byrow = TRUE)
#     se = matrix(se, nrow = ncol(Y), ncol = ncol(X), byrow = TRUE)
#     pvals = matrix(pvals, nrow = ncol(Y), ncol = ncol(X), byrow = TRUE)
#     rownames(pvals) = rownames(betas) = rownames(se) = colnames(Y)
#     colnames(pvals) = colnames(betas) = colnames(se) = colnames(X)
#     
#     saveRDS(pvals, paste0("../Results/Univariate/Response_X_f",f,"_pvals_",filepaths[i],".rds"))
#     saveRDS(betas, paste0("../Results/Univariate/Response_outcome_X_f",f,"_betas_",filepaths[i],".rds"))
#     saveRDS(se, paste0("../Results/Univariate/Response_outcome_X_f",f,"_se_",filepaths[i],".rds"))
#     
#     bonf = 0.05/ncol(pvals)
#     mytable = NULL
#     for (r in 1:nrow(pvals)){
#       beta = paste0(formatC(betas[r,], format="f", digits=2),
#                   " (",formatC(betas[r,]-qnorm(0.975)*se[r,], format="f", digits=2),
#                   ",",formatC(betas[r,]+qnorm(0.975)*se[r,], format="f", digits=2),")")
#       pval = formatC(pvals[r,], format = "e", digits = 2)
#       signif = ifelse(pvals[r,] < bonf, "*","")
#       mytable = cbind(mytable,beta,pval,signif)
#     }
#     dim(mytable)
#     mytable = rbind(rep(c("Beta coefficient (95% CI)","P-value","Bonf."), nrow(pvals)), mytable)
#     colnames(mytable) = rep(rownames(pvals), each = 3)
#     SaveExcelWithSuperscripts(cbind(c("",colnames(pvals)), mytable), paste0("../Exports/Univariate/Response_expo_f",f,"_summary_",filepaths[i],".xlsx"))
#     
#     # assign(paste0("betas",f),betas)
#     # assign(paste0("se",f),se)
#     # assign(paste0("pvals",f),pvals)
#   }
#   t1=Sys.time()
#   print(t1-t0)
# }
