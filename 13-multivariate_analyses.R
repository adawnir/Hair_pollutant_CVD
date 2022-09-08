## Stability-based LASSO logistic regression per CVDRF outcome

## Variable selection----
rm(list=ls())
project_path=dirname(rstudioapi::getActiveDocumentContext()$path)
setwd(project_path)

library(sharp)
library(tidyverse)

# args=commandArgs(trailingOnly=TRUE)
# m=as.numeric(args[1])

filepaths = c("PesticidesPCBBDEs","PAHs","Combined")

for (m in 1:3){
  # Load data
  data = readRDS(paste0("../Processed/",filepaths[m],"_XY.rds"))
  
  # Extract data
  genderfemale = ifelse(data$Gender == "Female", 1, 0)
  X = cbind(data$Age,
            genderfemale,
            data[,c(28:ncol(data))])
  
  Y = data[,c(23:27)]
  Y = Y %>% mutate_all(as.integer)-1
  
  suffix =  sub(" ", "_", colnames(Y))
  
  # LASSO logistic regression
  for (k in 1:ncol(Y)){
    print(colnames(Y)[k])
    
    ## Split training and test
    set.seed(100)
    s0=sample(which(Y[,k]=="0"), size=0.7*sum(Y[,k]=="0"), replace=FALSE)
    s1=sample(which(Y[,k]=="1"), size=0.7*sum(Y[,k]=="1"), replace=FALSE)
    train=c(s0,s1)
    Y_train=Y[train,k]
    X_train=X[train,]
    
    if(table(Y_train)[2]<16){
      next
    } else{
      t0=Sys.time()
      out = VariableSelection(X_train, Y_train, family = "binomial",
                              penalty.factor = c(rep(0,2),rep(1,ncol(X_train)-2)))
      t1=Sys.time()
      print(t1-t0)
      
      pdf(paste0("../Figures/Multivariate_analyses_base_output_",suffix[k],"_",filepaths[m],".pdf"))
      CalibrationPlot(out)
      dev.off()
      
      # Extracting ID of calibrated lambda
      hat_lambda_id=ArgmaxId(out)[1]
      
      # Computing average beta coefficients from models with calibrated lambda
      beta_mu=apply(out$Beta[hat_lambda_id,,],1,FUN=function(x){mean(x[x!=0])})
      
      # Calibrated selection proportions 
      selprop=SelectionProportions(out)
      
      # Save
      saveRDS(out, paste0("../Results/Multivariate_analysis_base_output_",suffix[k],"_",filepaths[m],".rds"))
      saveRDS(beta_mu, paste0("../Results/Multivariate_analysis_base_beta_mu_",suffix[k],"_",filepaths[m],".rds"))
      saveRDS(selprop, paste0("../Results/Multivariate_analysis_base_selprop_",suffix[k],"_",filepaths[m],".rds"))
    }
  }
}

for (m in 1:3){
  # Load data
  data = readRDS(paste0("../Processed/",filepaths[m],"_XY.rds"))
  
  # Extract data
  genderfemale = ifelse(data$Gender == "Female", 1, 0)
  formersmoker = ifelse(data$Smoking == "Former smoker", 1, 0)
  currentsmoker = ifelse(data$Smoking == "Current smoker", 1, 0)
  X = cbind(data$Age,
            genderfemale,
            formersmoker,
            currentsmoker,
            data[,c("NutriScore1","NutriScore2","NutriScore3")],
            data[,c(28:ncol(data))])
  
  Y = data[,c(23:27)]
  Y = Y %>% mutate_all(as.integer)-1
  
  suffix =  sub(" ", "_", colnames(Y))
  
  # LASSO logistic regression
  for (k in 1:ncol(Y)){
    print(colnames(Y)[k])
    
    ## Split training and test
    set.seed(100)
    s0=sample(which(Y[,k]=="0"), size=0.7*sum(Y[,k]=="0"), replace=FALSE)
    s1=sample(which(Y[,k]=="1"), size=0.7*sum(Y[,k]=="1"), replace=FALSE)
    train=c(s0,s1)
    Y_train=Y[train,k]
    X_train=X[train,]
    
    if(table(Y_train)[2]<16){
      next
    } else{
      t0=Sys.time()
      out = VariableSelection(X_train, Y_train, family = "binomial",
                              penalty.factor = c(rep(0,7),rep(1,ncol(X_train)-7)))
      t1=Sys.time()
      print(t1-t0)
      
      pdf(paste0("../Figures/Multivariate_analyses_adjust_output_",suffix[k],"_",filepaths[m],".pdf"))
      CalibrationPlot(out)
      dev.off()
      
      # Extracting ID of calibrated lambda
      hat_lambda_id=ArgmaxId(out)[1]
      
      # Computing average beta coefficients from models with calibrated lambda
      beta_mu=apply(out$Beta[hat_lambda_id,,],1,FUN=function(x){mean(x[x!=0])})
      
      # Calibrated selection proportions 
      selprop=SelectionProportions(out)
      
      # Save
      saveRDS(out, paste0("../Results/Multivariate_analysis_adjust_output_",suffix[k],"_",filepaths[m],".rds"))
      saveRDS(beta_mu, paste0("../Results/Multivariate_analysis_adjust_beta_mu_",suffix[k],"_",filepaths[m],".rds"))
      saveRDS(selprop, paste0("../Results/Multivariate_analysis_adjust_selprop_",suffix[k],"_",filepaths[m],".rds"))
    }
  }
}