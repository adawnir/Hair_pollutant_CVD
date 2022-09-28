## Inspection of stability-based LASSO logistic regression

rm(list=ls())
project_path=dirname(rstudioapi::getActiveDocumentContext()$path)
setwd(project_path)
setwd("../")

library(tidyverse)
library(sharp)

## Extract calibrated parameters----

myfiles = list.files(path = "../Results/Multimorbidity/", pattern = "Multivariate_analysis_adjust_output_*")
param = NULL
for (i in 1:length(myfiles)){
  # Load data
  out = readRDS(paste0("../Results/Multimorbidity/",myfiles[i]))
  tmp = Argmax(out)
  pdf(paste0("../Figures/Multimorbidity/",myfiles[i],".pdf"))
  par(mar = c(7,5,7,6))
  CalibrationPlot(out)
  dev.off()
  param = rbind(param, tmp)
}
rownames(param) = gsub(".rds","", myfiles) %>% gsub("_output_","_", .)
saveRDS(param, paste0("../Results/Multimorbidity/Multivariate_analysis_adjust_param.rds"))

## Inspect empty models and direction of betas of stably selected variables ----
myfiles = list.files(path = "../Results/Multimorbidity/", pattern = "Multivariate_analysis_adjust_output_*")
for (i in 1:length(myfiles)){
  # Load data
  out = readRDS(paste0("../Results/Multimorbidity/",myfiles[i]))
  if(sum(SelectedVariables(out))<1){
    print(paste0(myfiles[i]," is an empty model"))
  } else{
    selprop = SelectionProportions(out)
    all(colnames(out$Beta)[-c(1:7)] == names(selprop))
    a=apply(out$Beta[ArgmaxId(out)[1],-c(1:7),],1,FUN=function(x){sum(x>0)})
    b=apply(out$Beta[ArgmaxId(out)[1],-c(1:7),],1,FUN=function(x){sum(x<0)})
    
    newpath = gsub("output","inspect",myfiles[i]) %>% gsub(".rds",".pdf",.)
    pdf(paste0("../Figures/Multimorbidity/",newpath))
    par(mar=c(5,5,1,1))
    plot(a/(a+b), selprop, las=1, pch=19, col="navy", cex.lab=1.5,
         xlab="Proportion of positive loadings among non-zero loadings", 
         ylab="Selection Proportion") # Ideally no point around 0.5 with high selection proportion
    dev.off()
  }
}

## Recalibration and Predictive performance----
myfiles = list.files(path = "../Results/Multimorbidity/", pattern = "Multivariate_analysis_adjust_output_*")

for (i in 1:length(myfiles)){
  # Load data
  split = gsub("Multivariate_analysis_adjust_output_","",myfiles[i])
  data_type = gsub(".rds","",split)
  
  data = readRDS(paste0("../Processed/","Overlap_test","_XY_Multimorbidity.rds"))
  out = readRDS(paste0("../Results/Multimorbidity/",myfiles[i]))
  
  # Extract data
  genderfemale = ifelse(data$Gender == "Female", 1, 0)
  formersmoker = ifelse(data$Smoking == "Former smoker", 1, 0)
  currentsmoker = ifelse(data$Smoking == "Current smoker", 1, 0)
  X_test = cbind(data$Age,
                 genderfemale,
                 formersmoker,
                 currentsmoker,
                 data[,c("DietScore1","DietScore2","DietScore3")],
                 data[,c(15:ncol(data))])
  Y_test = data$OutcomeScore
  
  train = readRDS(paste0("../Processed/",data_type,"_XY_Multimorbidity.rds"))
  genderfemale = ifelse(train$Gender == "Female", 1, 0)
  formersmoker = ifelse(train$Smoking == "Former smoker", 1, 0)
  currentsmoker = ifelse(train$Smoking == "Current smoker", 1, 0)
  X_train = cbind(train$Age,
                  genderfemale,
                  formersmoker,
                  currentsmoker,
                  train[,c("DietScore1","DietScore2","DietScore3")],
                  train[,c(15:ncol(train))])
  Y_train = train$OutcomeScore
  
  # Scale data
  X_test = scale(X_test)
  X_train = scale(X_train)
  
  # Computing AUCs on the test set from models where predictors are added in order of decreasing selection proportion
  selprop = SelectionProportions(out)
  selprop_nonzero = selprop[SelectedVariables(out)==1]
  myorder = names(selprop_nonzero)[sort.list(selprop_nonzero, decreasing = TRUE)]
  calib = sum(SelectedVariables(out)==1)
  
  ## NULL model
  tmp = X_train[,c(colnames(X_train)[1:7],myorder[1:k]),drop=FALSE]
  mymodel_recalib=lm(Y_train ~., data = as.data.frame(tmp))
  # Prediction using a logistic model with recalibrated beta coefficients
  tmp = as.matrix(X_test[,c(colnames(X_test)[1:7],myorder[1:k]),drop=FALSE])%*%as.matrix(coef(mymodel_recalib)[-1], ncol=1)
  mymodel=lm(Y_test ~., data = as.data.frame(offset(tmp)))
  rmse = sqrt(crossprod(mymodel$residuals)/length(mymodel$residuals))
  
  myrmse = NULL
  for (k in 1:length(myorder)){
    # Recalibration of the beta coefficients on the training set
    tmp = X_train[,c(colnames(X_train)[1:7],myorder[1:k]),drop=FALSE]
    mymodel_recalib=lm(Y_train ~., data = as.data.frame(tmp))
    # Prediction using a logistic model with recalibrated beta coefficients
    tmp = as.matrix(X_test[,c(colnames(X_test)[1:7],myorder[1:k]),drop=FALSE])%*%as.matrix(coef(mymodel_recalib)[-1], ncol=1)
    mymodel=lm(Y_test ~., data = as.data.frame(offset(tmp)))
    rmse = sqrt(crossprod(mymodel$residuals)/length(mymodel$residuals))
    if(k==calib){
      saveRDS(mymodel$residuals, paste0("../Results/Multimorbidity/",gsub("output","residuals",myfiles[i])))
      recalib = data.frame(beta = summary(mymodel_recalib)$coefficients[-1,1],
                           se = summary(mymodel_recalib)$coefficients[-1,2])
      saveRDS(recalib, paste0("../Results/Multimorbidity/",gsub("output","recalib_beta",myfiles[i])))
    }
    if(length(myorder)==1){
      saveRDS(rmse, paste0("../Results/Multimorbidity/",gsub("output","performance",myfiles[i])))
    } else{
      myrmse=c(myrmse,rmse)
    }
  }
  if(!is.null(myrmse)){
    names(myrmse) = myorder
    saveRDS(myrmse, paste0("../Results/Multimorbidity/",gsub("output","performance",myfiles[i])))
  }
}
