## Stability-based LASSO logistic regression for multinomial outcome

rm(list=ls())
project_path=dirname(rstudioapi::getActiveDocumentContext()$path)
setwd(project_path)

library(sharp)
library(tidyverse)

# args=commandArgs(trailingOnly=TRUE)
# m=as.numeric(args[1])

filepaths = c("PesticidesPCBBDEs_train","PAHs_train")

## Variable selection----
for (m in 1:2){
  # Load data
  data = readRDS(paste0("../Processed/",filepaths[m],"_XY.rds"))
  
  # Extract data
  expo = data[c(27:ncol(data))]
  Y = as.integer(data$OutcomeCluster)-1
  
  # Scale exposures
  expo = scale(expo)
  # Multinomial logistic regression
  ncol(expo) # number of tests

  for (f in c(1,3,4)){
    tmp = model.matrix(~ Smoking + Education + Physical_activity + Alcohol, data)[,-1]
    if(f==1){
      X = cbind(data$Age,
                data$Gender,
                tmp[,1:2],
                data[,c("DietScore1","DietScore2","DietScore3")],
                data[,c(27:ncol(data))])
      X = X %>% mutate_if(is.factor, function(x) as.integer(x)-1)
      z = 7
    }
    if(f==3){
      X = cbind(data$Age,
                data$Gender,
                tmp[,1:2],
                data[,c("DietScore1","DietScore2","DietScore3")],
                tmp[,3:4],
                data[,c(27:ncol(data))])
      X = X %>% mutate_if(is.factor, function(x) as.integer(x)-1)
      z = 9
    }
    if(f==4){
      X = cbind(data$Age,
                data$Gender,
                tmp[,1:2],
                data[,c("DietScore1","DietScore2","DietScore3")],
                tmp[,3:ncol(tmp)],
                data[,c(27:ncol(data))])
      X = X %>% mutate_if(is.factor, function(x) as.integer(x)-1)
      z = 13
    }
    # LASSO logistic regression
    t0=Sys.time()
    out = VariableSelection(X, Y, family = "multinomial",
                            penalty.factor = c(rep(0,z),rep(1,ncol(X)-z)))
    t1=Sys.time()
    print(t1-t0)
    
    # Extracting ID of calibrated lambda
    hat_lambda_id=ArgmaxId(out)[1]
    
    # Computing average beta coefficients from models with calibrated lambda
    beta_mu=apply(out$Beta[hat_lambda_id,,,1:length(levels(data$OutcomeCluster))],
                  c(1,3),
                  FUN=function(x){mean(x[x!=0])})
    
    # Calibrated selection proportions 
    selprop=SelectionProportions(out)
    # Save
    saveRDS(out, paste0("../Results/Comorbidity/Multivariate_cluster_outcome_expo_f",f,"_output_",filepaths[m],".rds"))
    saveRDS(beta_mu, paste0("../Results/Comorbidity/Multivariate_cluster_outcome_expo_f",f,"_beta_mu_",filepaths[m],".rds"))
    saveRDS(selprop, paste0("../Results/Comorbidity/Multivariate_cluster_outcome_expo_f",f,"_selprop_",filepaths[m],".rds"))
  }
}