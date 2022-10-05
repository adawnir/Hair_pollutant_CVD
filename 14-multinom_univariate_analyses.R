## Univariate analyses: association between exposures and clustered outcome

rm(list=ls())
project_path=dirname(rstudioapi::getActiveDocumentContext()$path)
setwd(project_path)

library(tidyverse)
library(nnet)
source("functions.R")

filepaths = c("PesticidesPCBBDEs_train","PAHs_train","Overlap_test")
ifelse(dir.exists("../Exports/Comorbidity/"),"",dir.create("../Exports/Comorbidity/"))
ifelse(dir.exists("../Results/Comorbidity/"),"",dir.create("../Results/Comorbidity/"))

for (i in 1:2){
  # Load data
  data = readRDS(paste0("../Processed/",filepaths[i],"_XY.rds"))
  
  # Extract data
  expo = data[c(27:ncol(data))]
  Y = data$OutcomeCluster
  
  # Scale exposures
  expo = scale(expo)
  
  # Multinomial logistic regression
  ncol(expo) # number of tests
  f0='Y ~ expo[,j] + data$Age + data$Gender'
  f1='Y ~ expo[,j] + data$Age + data$Gender + data$Smoking + data$DietScore1 + data$DietScore2 + data$DietScore3'
  f2='Y ~ expo[,j] + data$Age + data$Gender + data$Smoking + data$DietScore1 + data$DietScore2  + data$DietScore3 + data$Country'
  f3='Y ~ expo[,j] + data$Age + data$Gender + data$Smoking + data$DietScore1 + data$DietScore2  + data$DietScore3 + data$Education'
  f4='Y ~ expo[,j] + data$Age + data$Gender + data$Smoking + data$DietScore1 + data$DietScore2  + data$DietScore3 + data$Education + data$Physical_activity + data$Alcohol'
  
  t0=Sys.time()
  for (f in 0:4){
    mytable = NULL
    betas = se = pvals = NULL
    for (j in 1:ncol(expo)){
      formula = eval(parse(text = paste0("f",f)))
      model=multinom(as.formula(formula))
      z = summary(model)$coefficients/summary(model)$standard.errors
      p = (1 - pnorm(abs(z), 0, 1))*2 # we are using two-tailed z test
      betas=cbind(betas, summary(model)$coefficients[,2])
      se=cbind(se, summary(model)$standard.errors[,2])
      pvals=cbind(pvals, p[,2])
    }
    colnames(pvals) = colnames(betas) = colnames(se) = colnames(expo)
    
    saveRDS(pvals, paste0("../Results/Comorbidity/Univariate_cluster_outcome_expo_f",f,"_pvals_",filepaths[i],".rds"))
    saveRDS(betas, paste0("../Results/Comorbidity/Univariate_cluster_outcome_expo_f",f,"_betas_",filepaths[i],".rds"))
    saveRDS(se, paste0("../Results/Comorbidity/Univariate_cluster_outcome_expo_f",f,"_se_",filepaths[i],".rds"))
    
    bonf = 0.05/ncol(pvals)
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
    SaveExcelWithSuperscripts(cbind(c("",colnames(pvals)), mytable), paste0("../Exports/Comorbidity/Univariate_cluster_outcome_expo_f",f,"_summary_",filepaths[i],".xlsx"))
  }
  t1=Sys.time()
  print(t1-t0)
}
