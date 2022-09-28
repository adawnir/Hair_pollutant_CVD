## Inspection of stability-based LASSO logistic regression

rm(list=ls())
project_path=dirname(rstudioapi::getActiveDocumentContext()$path)
setwd(project_path)
setwd("../")

library(tidyverse)
library(sharp)
library(nnet)

## Extract calibrated parameters----

myfiles = list.files(path = "../Results/Comorbidity/", pattern = "Multivariate_analysis_adjust_output_*")
param = NULL
for (i in 1:length(myfiles)){
  # Load data
  out = readRDS(paste0("../Results/Comorbidity/",myfiles[i]))
  tmp = Argmax(out)
  pdf(paste0("../Figures/Comorbidity/",myfiles[i],".pdf"))
  par(mar = c(7,5,7,6))
  CalibrationPlot(out)
  dev.off()
  param = rbind(param, tmp)
}
rownames(param) = gsub(".rds","", myfiles) %>% gsub("_output_","_", .)
saveRDS(param, paste0("../Results/Comorbidity/Multivariate_analysis_adjust_param.rds"))

## Inspect empty models and direction of betas of stably selected variables ----
myfiles = list.files(path = "../Results/Comorbidity/", pattern = "Multivariate_analysis_adjust_output_*")
for (i in 1:length(myfiles)){
  # Load data
  out = readRDS(paste0("../Results/Comorbidity/",myfiles[i]))
  if(sum(SelectedVariables(out))<1){
    print(paste0(myfiles[i]," is an empty model"))
  } else{
    selprop = SelectionProportions(out)
    all(colnames(out$Beta)[-c(1:7)] == names(selprop))
    a=apply(out$Beta[ArgmaxId(out)[1],-c(1:7),,],c(1,3),FUN=function(x){sum(x>0)})
    b=apply(out$Beta[ArgmaxId(out)[1],-c(1:7),,],c(1,3),FUN=function(x){sum(x<0)})
    
    newpath = gsub("output","inspect",myfiles[i]) %>% gsub(".rds",".pdf",.)
    pdf(paste0("../Figures/Comorbidity/",newpath))
    par(mar=c(5,5,1,1))
    plot(a/(a+b), rep(selprop,each=ncol(a)), las=1, pch=19, col="navy", cex.lab=1.5,
         xlab="Proportion of positive loadings among non-zero loadings", 
         ylab="Selection Proportion") # Ideally no point around 0.5 with high selection proportion
    dev.off()
  }
}

## Recalibration and Predictive performance----
myfiles = list.files(path = "../Results/Comorbidity/", pattern = "Multivariate_analysis_adjust_output_*")

for (i in 1:length(myfiles)){
  # Load data
  split = gsub("Multivariate_analysis_adjust_output_","",myfiles[i])
  data_type = gsub(".rds","",split)
  
  data = readRDS(paste0("../Processed/","Overlap_test","_XY_Comorbidity.rds"))
  out = readRDS(paste0("../Results/Comorbidity/",myfiles[i]))
  
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
  Y_test = data$OutcomeCluster
  
  train = readRDS(paste0("../Processed/",data_type,"_XY_Comorbidity.rds"))
  genderfemale = ifelse(train$Gender == "Female", 1, 0)
  formersmoker = ifelse(train$Smoking == "Former smoker", 1, 0)
  currentsmoker = ifelse(train$Smoking == "Current smoker", 1, 0)
  X_train = cbind(train$Age,
                  genderfemale,
                  formersmoker,
                  currentsmoker,
                  train[,c("DietScore1","DietScore2","DietScore3")],
                  train[,c(15:ncol(train))])
  Y_train = train$OutcomeCluster
  
  # Scale data
  X_test = scale(X_test)
  X_train = scale(X_train)
  
  colnames(X_train)[1:7] = colnames(X_test)[1:7]
  
  # Computing AUCs on the test set from models where predictors are added in order of decreasing selection proportion
  selprop = SelectionProportions(out)
  selprop_nonzero = selprop[SelectedVariables(out)==1]
  myorder = names(selprop_nonzero)[sort.list(selprop_nonzero, decreasing = TRUE)]
  calib = sum(SelectedVariables(out)==1)
  
  myf1 = NULL
  for (k in 1:length(myorder)){
    # Recalibration of the beta coefficients on the training set
    tmp1 = X_train[,c(colnames(X_train)[1:7],myorder[1:k]),drop=FALSE]
    mymodel_recalib=multinom(Y_train ~., data = as.data.frame(tmp1))
    # Prediction using a logistic model with recalibrated beta coefficients
    tmp2 = X_test[,c(colnames(X_test)[1:7],myorder[1:k]),drop=FALSE]
    Y_pred = predict(mymodel_recalib, newdata = tmp2, "class")
    Y_predp = predict(mymodel_recalib, newdata = tmp2, "probs")
    pred = table(Y_pred, Y_test)
    p = diag(pred)/rowSums(pred)
    r = diag(pred)/colSums(pred)
    f1 = 2*p*r/(p+r)
    if(k==calib){
      saveRDS(Y_predp, paste0("../Results/Comorbidity/",gsub("output","predictions",myfiles[i])))
      recalib = list(beta = summary(mymodel_recalib)$coefficients[,-1],
                     se = summary(mymodel_recalib)$standard.errors[,-1])
      saveRDS(recalib, paste0("../Results/Comorbidity/",gsub("output","recalib_beta",myfiles[i])))
    }
    if(length(myorder)==1){
      saveRDS(f1, paste0("../Results/Comorbidity/",gsub("output","performance",myfiles[i])))
    } else{
      myf1=cbind(myf1,f1)
    }
  }
  if(!is.null(myf1)){
    colnames(myf1) = myorder
    saveRDS(myf1, paste0("../Results/Comorbidity/",gsub("output","performance",myfiles[i])))
  }
}
