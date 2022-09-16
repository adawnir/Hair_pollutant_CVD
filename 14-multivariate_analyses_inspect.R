## Inspection of stability-based LASSO logistic regression

rm(list=ls())
project_path=dirname(rstudioapi::getActiveDocumentContext()$path)
setwd(project_path)

library(tidyverse)
library(sharp)
library(ROCR)

## Extract calibrated parameters----

myfiles = list.files(path = "../Results", pattern = "Multivariate_analysis_adjust_output_*")
param = NULL
for (i in 1:length(myfiles)){
  # Load data
  out = readRDS(paste0("../Results/",myfiles[i]))
  tmp = Argmax(out)
  pdf(paste0("../Figures/",myfiles[i],".pdf"))
  par(mar = c(7,5,7,6))
  CalibrationPlot(out)
  dev.off()
  param = rbind(param, tmp)
}
rownames(param) = gsub(".rds","", myfiles) %>% gsub("_output_","_", .)
saveRDS(param, paste0("../Results/Multivariate_analysis_adjust_param.rds"))

## Inspect empty models and direction of betas of stably selected variables ----
myfiles = list.files(path = "../Results", pattern = "Multivariate_analysis_adjust_output_*")
for (i in 1:length(myfiles)){
  # Load data
  out = readRDS(paste0("../Results/",myfiles[i]))
  if(sum(SelectedVariables(out))<1){
    print(paste0(myfiles[i]," is an empty model"))
  } else{
    selprop = SelectionProportions(out)
    all(colnames(out$Beta)[-c(1:7)] == names(selprop))
    a=apply(out$Beta[ArgmaxId(out)[1],-c(1:7),],1,FUN=function(x){sum(x>0)})
    b=apply(out$Beta[ArgmaxId(out)[1],-c(1:7),],1,FUN=function(x){sum(x<0)})
    
    newpath = gsub("output","inspect",myfiles[i]) %>% gsub(".rds",".pdf",.)
    pdf(paste0("../Figures/",newpath))
    par(mar=c(5,5,1,1))
    plot(a/(a+b), selprop, las=1, pch=19, col="navy", cex.lab=1.5,
         xlab="Proportion of positive loadings among non-zero loadings", 
         ylab="Selection Proportion") # Ideally no point around 0.5 with high selection proportion
    dev.off()
  }
}

## Recalibration and Predictive performance----
myfiles = list.files(path = "../Results", pattern = "Multivariate_analysis_adjust_output_*")

for (i in 1:length(myfiles)){
  # Load data
  split = gsub("Multivariate_analysis_adjust_output_","",myfiles[i])
  outcome_type = gsub("_PesticidesPCBBDEs_train.rds|_PAHs_train.rds","", split)
  data_type = gsub(paste0(outcome_type,"_"),"",split) %>% gsub(".rds","",.)
  
  data = readRDS(paste0("../Processed/","Overlap_test","_XY.rds"))
  out = readRDS(paste0("../Results/",myfiles[i]))
  
  # Extract data
  genderfemale = ifelse(data$Gender == "Female", 1, 0)
  formersmoker = ifelse(data$Smoking == "Former smoker", 1, 0)
  currentsmoker = ifelse(data$Smoking == "Current smoker", 1, 0)
  X_test = cbind(data$Age,
                 genderfemale,
                 formersmoker,
                 currentsmoker,
                 data[,c("DietScore1","DietScore2","DietScore3")],
                 data[,c(14:ncol(data))])
  Y_test = as.integer(data[,outcome_type])-1
  
  train = readRDS(paste0("../Processed/",data_type,"_XY.rds"))
  genderfemale = ifelse(train$Gender == "Female", 1, 0)
  formersmoker = ifelse(train$Smoking == "Former smoker", 1, 0)
  currentsmoker = ifelse(train$Smoking == "Current smoker", 1, 0)
  X_train = cbind(train$Age,
                  genderfemale,
                  formersmoker,
                  currentsmoker,
                  train[,c("DietScore1","DietScore2","DietScore3")],
                  train[,c(14:ncol(train))])
  Y_train = as.integer(train[,outcome_type])-1
    
  # Scale data
  X_test = scale(X_test)
  X_train = scale(X_train)
  
  # Computing AUCs on the test set from models where predictors are added in order of decreasing selection proportion
  selprop = SelectionProportions(out)
  selprop_nonzero = selprop[SelectedVariables(out)==1]
  myorder = names(selprop_nonzero)[sort.list(selprop_nonzero, decreasing = TRUE)]
  calib = sum(SelectedVariables(out)==1)
  
  myf1 = NULL
  for (k in 1:length(myorder)){
    # Recalibration of the beta coefficients on the training set
    tmp = X_train[,c(colnames(X_train)[1:7],myorder[1:k]),drop=FALSE]
    mymodel_recalib=glm(Y_train ~., data = as.data.frame(tmp), family="binomial")
    # Prediction using a logistic model with recalibrated beta coefficients
    tmp = as.matrix(X_test[,c(colnames(X_test)[1:7],myorder[1:k]),drop=FALSE])%*%as.matrix(coef(mymodel_recalib)[-1], ncol=1)
    mymodel=glm(Y_test ~., data = as.data.frame(offset(tmp)), family="binomial")
    pred = prediction(mymodel$fitted.values, Y_test)
    f1 = performance(pred,"f")
    curve = data.frame(cutoffs = unlist(f1@x.values), f1 =unlist(f1@y.values))
    if(k==calib){
      saveRDS(mymodel$fitted.values, paste0("../Results/",gsub("output","predictions",myfiles[i])))
      recalib = data.frame(beta = summary(mymodel_recalib)$coefficients[-1,1],
                           se = summary(mymodel_recalib)$coefficients[-1,2])
      saveRDS(recalib, paste0("../Results/",gsub("output","recalib_beta",myfiles[i])))
    }
    if(length(myorder)==1){
      saveRDS(curve, paste0("../Results/",gsub("output","performance",myfiles[i])))
    } else{
      myf1[[k]]=curve
    }
  }
  if(!is.null(myf1)){
    names(myf1) = myorder
    saveRDS(myf1, paste0("../Results/",gsub("output","performance",myfiles[i])))
  }
}
