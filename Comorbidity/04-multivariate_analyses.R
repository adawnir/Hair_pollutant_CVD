## Stability-based LASSO logistic regression for multinomial outcome

## Variable selection----
rm(list=ls())
project_path=dirname(rstudioapi::getActiveDocumentContext()$path)
setwd(project_path)
setwd("../")

library(sharp)
library(tidyverse)

# args=commandArgs(trailingOnly=TRUE)
# m=as.numeric(args[1])

filepaths = c("PesticidesPCBBDEs_train","PAHs_train")

for (m in 1:2){
  # Load data
  data = readRDS(paste0("../Processed/",filepaths[m],"_XY_Comorbidity.rds"))
  
  # Extract data
  genderfemale = ifelse(data$Gender == "Female", 1, 0)
  formersmoker = ifelse(data$Smoking == "Former smoker", 1, 0)
  currentsmoker = ifelse(data$Smoking == "Current smoker", 1, 0)
  X = cbind(data$Age,
            genderfemale,
            formersmoker,
            currentsmoker,
            data[,c("DietScore1","DietScore2","DietScore3")],
            data[,c(15:ncol(data))])
  
  Y = data$OutcomeCluster
  

  # LASSO logistic regression
  t0=Sys.time()
  out = VariableSelection(X, Y, family = "multinomial",
                          penalty.factor = c(rep(0,7),rep(1,ncol(X)-7)))
  t1=Sys.time()
  print(t1-t0)
  
  # Extracting ID of calibrated lambda
  hat_lambda_id=ArgmaxId(out)[1]
  
  # Computing average beta coefficients from models with calibrated lambda
  beta_mu=apply(out$Beta[hat_lambda_id,,,1:length(levels(Y))],c(1,3),
                FUN=function(x){mean(x[x!=0])})
  
  # Calibrated selection proportions 
  selprop=SelectionProportions(out)
  
  # Save
  saveRDS(out, paste0("../Results/Comorbidity/Multivariate_analysis_adjust_output_",filepaths[m],".rds"))
  saveRDS(beta_mu, paste0("../Results/Comorbidity/Multivariate_analysis_adjust_beta_mu_",filepaths[m],".rds"))
  saveRDS(selprop, paste0("../Results/Comorbidity/Multivariate_analysis_adjust_selprop_",filepaths[m],".rds"))
  
}