## Stability-based LASSO logistic regression per CVDRF outcome

rm(list=ls())
project_path=dirname(rstudioapi::getActiveDocumentContext()$path)
setwd(project_path)

library(sharp)
library(tidyverse)

# args=commandArgs(trailingOnly=TRUE)
# m=as.numeric(args[1])

filepaths = c("PesticidesPCBBDEs_train","PAHs_train")

ifelse(dir.exists("../Results/Multivariate/"),"",dir.create("../Results/Multivariate/"))

for (m in 1:2){
  # Load data
  X = readRDS(paste0("../Processed/",filepaths[m],"_expo_ndimp_naimp_log.rds"))
  Y = readRDS(paste0("../Processed/",filepaths[m],"_Y.rds"))
  levels = Y %>% select(where(is.factor)) %>% sapply(function(x) length(levels(x)))
  keep = names(levels)[levels == 2]
  Y = Y %>% select(where(is.numeric), all_of(keep)) %>%
    mutate_if(is.factor, function(x) factor(x, labels = 0:1))
  
  # Scale exposures
  X = scale(X)
  
  for (f in 0:7){
    Z = readRDS(paste0("../Processed/",filepaths[m],"_Z.rds"))
    if (f == 0){
      Z = Z %>% select(Age, Gender)
    }
    if (f == 1){
      Z = Z %>% select(Age, Gender, Smoking, starts_with("DietScore"))
    }
    if (f == 2){
      Z = Z %>% select(Age, Gender, Smoking, starts_with("DietScore"), Education)
    }
    if (f == 3){
      Z = Z %>% select(Age, Gender, Smoking, starts_with("DietScore"), Education,
                       Physical_activity, Alcohol)
    }
    if (f == 4){
      Z = Z %>% select(Age, Gender, Country)
    }
    if (f == 5){
      Z = Z %>% select(Age, Gender, Smoking, starts_with("DietScore"), Country)
    }
    if (f == 6){
      Z = Z %>% select(Age, Gender, Smoking, starts_with("DietScore"), Education, Country)
    }
    if (f == 7){
      Z = Z %>% select(Age, Gender, Smoking, starts_with("DietScore"), Education,
                       Physical_activity, Alcohol, Country)
    }
    Z = model.matrix(~ ., Z)[,-1] 
    for (k in 1:ncol(Y)){
      print(colnames(Y)[k])
      
      if(is.factor(Y[,k])){
        if(table(Y[,k])[2]<16){
          next
        } else{
          t0=Sys.time()
          out = VariableSelection(cbind(Z,X), Y[,k], family = "binomial",
                                  penalty.factor = c(rep(0,ncol(Z)),rep(1,ncol(X))))
          t1=Sys.time()
          print(t1-t0)
        } 
      } else if(is.numeric(Y[,k])){
        t0=Sys.time()
        out = VariableSelection(cbind(Z,X), Y[,k], family = "gaussian",
                                penalty.factor = c(rep(0,ncol(Z)),rep(1,ncol(X))))
        t1=Sys.time()
        print(t1-t0)
      }
      # Extracting ID of calibrated lambda
      hat_lambda_id=ArgmaxId(out)[1]
      
      # Computing average beta coefficients from models with calibrated lambda
      beta_mu=apply(out$Beta[hat_lambda_id,,],1,FUN=function(x){mean(x[x!=0])})
      
      # Calibrated selection proportions 
      selprop=SelectionProportions(out)
      
      # Save
      saveRDS(out, paste0("../Results/Multivariate/Response_expo_f",f,"_output_",colnames(Y)[k],"_",filepaths[m],".rds"))
      saveRDS(beta_mu, paste0("../Results/Multivariate/Response_expo_f",f,"_beta_mu_",colnames(Y)[k],"_",filepaths[m],".rds"))
      saveRDS(selprop, paste0("../Results/Multivariate/Response_expo_f",f,"_selprop",colnames(Y)[k],"_",filepaths[m],".rds"))
      }
  }
}