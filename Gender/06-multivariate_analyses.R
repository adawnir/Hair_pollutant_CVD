## Stability-based LASSO logistic regression per CVDRF outcome

## Variable selection----
rm(list=ls())
project_path=dirname(rstudioapi::getActiveDocumentContext()$path)
setwd(project_path)
setwd("../")

library(sharp)
library(tidyverse)

# args=commandArgs(trailingOnly=TRUE)
# m=as.numeric(args[1])

filepaths = c("PesticidesPCBBDEs","PAHs","Combined")
strata = c("Female","Male")

for (m in 1:2){
  for (h in 1:2){
    # Load data
    data = readRDS(paste0("../Processed/",filepaths[m],"_XY_",strata[h],".rds"))
    
    # Extract data
    X = data[,c(4,21:ncol(data))]
    X = X %>% mutate_if(is.factor, as.integer)-1
    Y = data[,c(16:20)]
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
                                penalty.factor = c(rep(0,1),rep(1,ncol(X_train)-1)))
        t1=Sys.time()
        print(t1-t0)
        
        pdf(paste0("../Figures/Gender/Multivariate_analyses_output_",suffix[k],"_",filepaths[m],"_",strata[h],".pdf"))
        CalibrationPlot(out)
        dev.off()
        
        # Extracting ID of calibrated lambda
        hat_lambda_id=ArgmaxId(out)[1]
        
        # Computing average beta coefficients from models with calibrated lambda
        beta_mu=apply(out$Beta[hat_lambda_id,,],1,FUN=function(x){mean(x[x!=0])})
        
        # Calibrated selection proportions 
        selprop=SelectionProportions(out)
        
        # Save
        saveRDS(out, paste0("../Results/Gender/Multivariate_analysis_output_",suffix[k],"_",filepaths[m],"_",strata[h],".rds"))
        saveRDS(beta_mu, paste0("../Results/Gender/Multivariate_analysis_beta_mu_",suffix[k],"_",filepaths[m],"_",strata[h],".rds"))
        saveRDS(selprop, paste0("../Results/Gender/Multivariate_analysis_selprop_",suffix[k],"_",filepaths[m],"_",strata[h],".rds"))
      }
    }
  }
}
