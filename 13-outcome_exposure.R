## Multivariate LASSO analyses: association between outcomes and exposures adjusted for covariates

rm(list=ls())
project_path=dirname(rstudioapi::getActiveDocumentContext()$path)
setwd(project_path)

library(tidyverse)
source("functions.R")
filepaths = c("PesticidesPCBBDEs_train","PAHs_train","Overlap_test")
ifelse(dir.exists("../Results/Multivariate/"),"",dir.create("../Results/Multivariate/"))

## One outcome ~ Exposures ----
i = 1

  # Load data
  X = readRDS(paste0("../Processed/",filepaths[i],"_expo_ndimp_naimp_log.rds"))
  Y = readRDS(paste0("../Data/",filepaths[i],"_response.rds"))
  
  Y = Y %>% select(BMI, WC, TG, TC, LDLC, HDLC, SBP, DBP, FPG)

  for (f in 0:6){
    Z = readRDS(paste0("../Processed/",filepaths[i],"_Z.rds"))
    if (f==0){
      Z = Z %>% select(Age, Gender)
    }
    if (f==1){
      Z = Z %>% select(Age, Gender, Centre)
    }
    if (f==2){
      Z = Z %>% select(Age, Gender, Education)
    }
    if (f==3){
      Z = Z %>% select(Age, Gender, Education, Smoking)
    }
    if (f==4){
      Z = Z %>% select(Age, Gender, Education, Smoking, starts_with("Diet"))
    }
    if (f==5){
      Z = Z %>% select(Age, Gender, Education, Smoking, starts_with("Diet"), Physical_activity)
    }
    if (f==6){
      Z = Z %>% select(Age, Gender, Education, Smoking, starts_with("Diet"), Physical_activity, Alcohol)
    }
    
    Z_mat = model.matrix(~ ., Z)[,-1]

    for (k in 1:ncol(Y)){
      # LASSO linear regression
      stab = VariableSelection(cbind(Z_mat,X), Y[,k], family = "gaussian",
                               penalty.factor = c(rep(0,ncol(Z_mat)),rep(1,ncol(X))))
      
      pdf(paste0("../Figures/Multivariate/",colnames(Y)[k],"_X_f",f,"_output_",filepaths[i],".pdf"))
      par(mar = c(7,5,7,6))
      CalibrationPlot(stab)
      dev.off()
      
      # Extracting ID of calibrated lambda
      hat_lambda_id = ArgmaxId(stab)[1]
      
      # Computing average beta coefficients from models with calibrated lambda
      beta_mu = apply(stab$Beta[hat_lambda_id,-c(1:ncol(Z)),], 1,
                      FUN=function(x){mean(x[x!=0])})
      
      # Calibrated selection proportions 
      selprop = SelectionProportions(stab)
      
      # Save
      saveRDS(stab, paste0("../Results/Multivariate/",colnames(Y)[k],"_X_f",f,"_output_",filepaths[i],".rds"))
      saveRDS(beta_mu, paste0("../Results/Multivariate/",colnames(Y)[k],"_X_f",f,"_beta_mu_",filepaths[i],".rds"))
      saveRDS(selprop, paste0("../Results/Multivariate/",colnames(Y)[k],"_X_f",f,"_selprop_",filepaths[i],".rds"))
    }
  }


## Multiple outcomes ~ Exposures ----
i = 1

  for (j in 1:3){
    # Load data
    X = readRDS(paste0("../Processed/",filepaths[i],"_expo_ndimp_naimp_log.rds"))
    Y = readRDS(paste0("../Data/",filepaths[i],"_response.rds"))
    
    if (j==1){
      Y = Y %>% select(BMI, WC, TG, HDLC, SBP, DBP)
    }
    if (j==2){
      Y = Y %>% select(BMI, WC, TG, TC, LDLC, HDLC, SBP, DBP)
    }
    if (j==3){
      Y = Y %>% select(BMI, WC, TG, TC, LDLC, HDLC, SBP, DBP, FPG)
    }
    
    for (f in 0:6){
      Z = readRDS(paste0("../Processed/",filepaths[i],"_Z.rds"))
      if (f==0){
        Z = Z %>% select(Age, Gender)
      }
      if (f==1){
        Z = Z %>% select(Age, Gender, Centre)
      }
      if (f==2){
        Z = Z %>% select(Age, Gender, Education)
      }
      if (f==3){
        Z = Z %>% select(Age, Gender, Education, Smoking)
      }
      if (f==4){
        Z = Z %>% select(Age, Gender, Education, Smoking, starts_with("Diet"))
      }
      if (f==5){
        Z = Z %>% select(Age, Gender, Education, Smoking, starts_with("Diet"), Physical_activity)
      }
      if (f==6){
        Z = Z %>% select(Age, Gender, Education, Smoking, starts_with("Diet"), Physical_activity, Alcohol)
      }
      
      Z_mat = model.matrix(~ ., Z)[,-1]

      # LASSO linear regression
      stab = VariableSelection(cbind(Z_mat,X), Y, family = "mgaussian",
                               penalty.factor = c(rep(0,ncol(Z_mat)),rep(1,ncol(X))))
      
      pdf(paste0("../Figures/Multivariate/MLASSO",j,"_Y_X_f",f,"_output_",filepaths[i],".pdf"))
      par(mar = c(7,5,7,6))
      CalibrationPlot(stab)
      dev.off()
      
      # Extracting ID of calibrated lambda
      hat_lambda_id = ArgmaxId(stab)[1]
      
      # Calibrated selection proportions 
      selprop = SelectionProportions(stab)
      
      # Save
      saveRDS(stab, paste0("../Results/Multivariate/MLASSO",j,"_Y_X_f",f,"_output_",filepaths[i],".rds"))
      saveRDS(selprop, paste0("../Results/Multivariate/MLASSO",j,"_Y_X_f",f,"_selprop_",filepaths[i],".rds"))
    } 
  }

