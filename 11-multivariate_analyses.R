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

## Binary outcome----
for (m in 1:2){
  # Load data
  data = readRDS(paste0("../Processed/",filepaths[m],"_XY.rds"))
  
  # Extract data
  Y = data[,c(12:14,16)] # exclude lipid disorder
  Y = Y %>% mutate_all(as.integer)-1
  outcome =  sub("_", " ", colnames(Y))
  
  tmp = model.matrix(~ Smoking + Education + Physical_activity + Alcohol, data)[,-1]
  X = cbind(data$Age,
            data$Gender,
            tmp[,1:2],
            data[,c("DietScore1","DietScore2","DietScore3")],
            data[,c(27:ncol(data))])
  X = X %>% mutate_if(is.factor, function(x) as.integer(x)-1)
  
  
  # Model 1: Adjusted for age, gender, smoking and diet
  for (k in 1:ncol(Y)){
    print(outcome[k])

    if(table(Y[,k])[2]<16){
      next
    } else{
      t0=Sys.time()
      out = VariableSelection(X, Y[,k], family = "binomial",
                              penalty.factor = c(rep(0,7),rep(1,ncol(X)-7)))
      t1=Sys.time()
      print(t1-t0)
      
      # Extracting ID of calibrated lambda
      hat_lambda_id=ArgmaxId(out)[1]
      
      # Computing average beta coefficients from models with calibrated lambda
      beta_mu=apply(out$Beta[hat_lambda_id,,],1,FUN=function(x){mean(x[x!=0])})
      
      # Calibrated selection proportions 
      selprop=SelectionProportions(out)
      
      # Save
      saveRDS(out, paste0("../Results/Multivariate/Binary_outcome_expo_f1_output_",colnames(Y)[k],"_",filepaths[m],".rds"))
      saveRDS(beta_mu, paste0("../Results/Multivariate/Binary_outcome_expo_f1_beta_mu_",colnames(Y)[k],"_",filepaths[m],".rds"))
      saveRDS(selprop, paste0("../Results/Multivariate/Binary_outcome_expo_f1_selprop",colnames(Y)[k],"_",filepaths[m],".rds"))
    }
  }
  
  # Model 2: Model 1 + education
  
  X = cbind(data$Age,
            data$Gender,
            tmp[,1:2],
            data[,c("DietScore1","DietScore2","DietScore3")],
            tmp[,3:4],
            data[,c(27:ncol(data))])
  X = X %>% mutate_if(is.factor, function(x) as.integer(x)-1)
  
  for (k in 1:ncol(Y)){
    print(outcome[k])
    
    if(table(Y[,k])[2]<16){
      next
    } else{
      t0=Sys.time()
      out = VariableSelection(X, Y[,k], family = "binomial",
                              penalty.factor = c(rep(0,9),rep(1,ncol(X)-9)))
      t1=Sys.time()
      print(t1-t0)
      
      # Extracting ID of calibrated lambda
      hat_lambda_id=ArgmaxId(out)[1]
      
      # Computing average beta coefficients from models with calibrated lambda
      beta_mu=apply(out$Beta[hat_lambda_id,,],1,FUN=function(x){mean(x[x!=0])})
      
      # Calibrated selection proportions 
      selprop=SelectionProportions(out)
      
      # Save
      saveRDS(out, paste0("../Results/Multivariate/Binary_outcome_expo_f3_output_",colnames(Y)[k],"_",filepaths[m],".rds"))
      saveRDS(beta_mu, paste0("../Results/Multivariate/Binary_outcome_expo_f3_beta_mu_",colnames(Y)[k],"_",filepaths[m],".rds"))
      saveRDS(selprop, paste0("../Results/Multivariate/Binary_outcome_expo_f3_selprop_",colnames(Y)[k],"_",filepaths[m],".rds"))
    }
  }
  
  # Model 3: Model 2 + physical activity and alcohol consumption
  X = cbind(data$Age,
            data$Gender,
            tmp[,1:2],
            data[,c("DietScore1","DietScore2","DietScore3")],
            tmp[,3:ncol(tmp)],
            data[,c(27:ncol(data))])
  X = X %>% mutate_if(is.factor, function(x) as.integer(x)-1)
  
  for (k in 1:ncol(Y)){
    print(outcome[k])
    
    if(table(Y[,k])[2]<16){
      next
    } else{
      t0=Sys.time()
      out = VariableSelection(X, Y[,k], family = "binomial",
                              penalty.factor = c(rep(0,13),rep(1,ncol(X)-13)))
      t1=Sys.time()
      print(t1-t0)
      
      # Extracting ID of calibrated lambda
      hat_lambda_id=ArgmaxId(out)[1]
      
      # Computing average beta coefficients from models with calibrated lambda
      beta_mu=apply(out$Beta[hat_lambda_id,,],1,FUN=function(x){mean(x[x!=0])})
      
      # Calibrated selection proportions 
      selprop=SelectionProportions(out)
      
      # Save
      saveRDS(out, paste0("../Results/Multivariate/Binary_outcome_expo_f4_output_",colnames(Y)[k],"_",filepaths[m],".rds"))
      saveRDS(beta_mu, paste0("../Results/Multivariate/Binary_outcome_f4_expo_beta_mu_",colnames(Y)[k],"_",filepaths[m],".rds"))
      saveRDS(selprop, paste0("../Results/Multivariate/Binary_outcome_expo_f4_selprop_",colnames(Y)[k],"_",filepaths[m],".rds"))
    }
  }
}

## Continuous outcome----
for (m in 1:2){
  # Load data
  data = readRDS(paste0("../Processed/",filepaths[m],"_XY.rds"))
  
  # Extract data
  Y = data[,c(17:25)]
  # Y = scale(Y)
  outcome = colnames(Y)
  
  tmp = model.matrix(~ Smoking + Education + Physical_activity + Alcohol, data)[,-1]
  X = cbind(data$Age,
            data$Gender,
            tmp[,1:2],
            data[,c("DietScore1","DietScore2","DietScore3")],
            data[,c(27:ncol(data))])
  X = X %>% mutate_if(is.factor, function(x) as.integer(x)-1)
  
  
  # Model 1: Adjusted for age, gender, smoking and diet
  for (k in 1:ncol(Y)){
    print(outcome[k])
    
    t0=Sys.time()
    out = VariableSelection(X, Y[,k], family = "gaussian",
                            penalty.factor = c(rep(0,7),rep(1,ncol(X)-7)))
    t1=Sys.time()
    print(t1-t0)
    
    # Extracting ID of calibrated lambda
    hat_lambda_id=ArgmaxId(out)[1]
    
    # Computing average beta coefficients from models with calibrated lambda
    beta_mu=apply(out$Beta[hat_lambda_id,,],1,FUN=function(x){mean(x[x!=0])})
    
    # Calibrated selection proportions 
    selprop=SelectionProportions(out)
    
    # Save
    saveRDS(out, paste0("../Results/Multivariate/Continuous_outcome_expo_f1_output_",colnames(Y)[k],"_",filepaths[m],".rds"))
    saveRDS(beta_mu, paste0("../Results/Multivariate/Continuous_outcome_expo_f1_beta_mu_",colnames(Y)[k],"_",filepaths[m],".rds"))
    saveRDS(selprop, paste0("../Results/Multivariate/Continuous_outcome_expo_f1_selprop_",colnames(Y)[k],"_",filepaths[m],".rds"))
  }
    
  
  # Model 2: Model 1 + education
  
  X = cbind(data$Age,
            data$Gender,
            tmp[,1:2],
            data[,c("DietScore1","DietScore2","DietScore3")],
            tmp[,3:4],
            data[,c(27:ncol(data))])
  X = X %>% mutate_if(is.factor, function(x) as.integer(x)-1)
  
  for (k in 1:ncol(Y)){
    print(outcome[k])
    
    t0=Sys.time()
    out = VariableSelection(X, Y[,k], family = "gaussian",
                            penalty.factor = c(rep(0,9),rep(1,ncol(X)-9)))
    t1=Sys.time()
    print(t1-t0)
    
    # Extracting ID of calibrated lambda
    hat_lambda_id=ArgmaxId(out)[1]
    
    # Computing average beta coefficients from models with calibrated lambda
    beta_mu=apply(out$Beta[hat_lambda_id,,],1,FUN=function(x){mean(x[x!=0])})
    
    # Calibrated selection proportions 
    selprop=SelectionProportions(out)
    
    # Save
    saveRDS(out, paste0("../Results/Multivariate/Continuous_outcome_expo_f3_output_",colnames(Y)[k],"_",filepaths[m],".rds"))
    saveRDS(beta_mu, paste0("../Results/Multivariate/Continuous_outcome_expo_f3_beta_mu_",colnames(Y)[k],"_",filepaths[m],".rds"))
    saveRDS(selprop, paste0("../Results/Multivariate/Continuous_outcome_expo_f3_selprop_",colnames(Y)[k],"_",filepaths[m],".rds"))
  }
  
  # Model 3: Model 2 + physical activity and alcohol consumption
  X = cbind(data$Age,
            data$Gender,
            tmp[,1:2],
            data[,c("DietScore1","DietScore2","DietScore3")],
            tmp[,3:ncol(tmp)],
            data[,c(27:ncol(data))])
  X = X %>% mutate_if(is.factor, function(x) as.integer(x)-1)
  
  for (k in 1:ncol(Y)){
    print(outcome[k])
    
    t0=Sys.time()
    out = VariableSelection(X, Y[,k], family = "gaussian",
                            penalty.factor = c(rep(0,13),rep(1,ncol(X)-13)))
    t1=Sys.time()
    print(t1-t0)
    
    # Extracting ID of calibrated lambda
    hat_lambda_id=ArgmaxId(out)[1]
    
    # Computing average beta coefficients from models with calibrated lambda
    beta_mu=apply(out$Beta[hat_lambda_id,,],1,FUN=function(x){mean(x[x!=0])})
    
    # Calibrated selection proportions 
    selprop=SelectionProportions(out)
    
    # Save
    saveRDS(out, paste0("../Results/Multivariate/Continuous_outcome_expo_f4_output_",colnames(Y)[k],"_",filepaths[m],".rds"))
    saveRDS(beta_mu, paste0("../Results/Multivariate/Continuous_outcome_expo_f4_beta_mu_",colnames(Y)[k],"_",filepaths[m],".rds"))
    saveRDS(selprop, paste0("../Results/Multivariate/Continuous_outcome_expo_f4_selprop_",colnames(Y)[k],"_",filepaths[m],".rds"))
  }
}