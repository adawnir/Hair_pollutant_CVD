## Inspection of stability-based LASSO logistic regression

rm(list=ls())
project_path=dirname(rstudioapi::getActiveDocumentContext()$path)
setwd(project_path)

library(tidyverse)
library(sharp)
library(ROCR)

## Extract calibrated parameters----
ifelse(dir.exists("../Figures/Multivariate/"),"",dir.create("../Figures/Multivariate/"))

myfiles = list.files(path = "../Results/Multivariate/", pattern = "*_output_*")
param = NULL
for (i in 1:length(myfiles)){
  # Load data
  out = readRDS(paste0("../Results/Multivariate/",myfiles[i]))
  tmp = Argmax(out)
  pdf(paste0("../Figures/Multivariate/",myfiles[i],".pdf"))
  par(mar = c(7,5,7,6))
  CalibrationPlot(out)
  dev.off()
  param = rbind(param, tmp)
}
rownames(param) = gsub(".rds","", myfiles) %>% gsub("_output_","_", .)
saveRDS(param, paste0("../Results/Multivariate/Model_parameter_summary.rds"))

## Inspect empty models and direction of betas of stably selected variables ----
myfiles = list.files(path = "../Results/Multivariate/", pattern = "*f1_output_*")
for (i in 1:length(myfiles)){
  # Load data
  out = readRDS(paste0("../Results/Multivariate/",myfiles[i]))
  if(sum(SelectedVariables(out))<1){
    print(paste0(myfiles[i]," is an empty model"))
  } else{
    selprop = SelectionProportions(out)
    all(colnames(out$Beta)[-c(1:7)] == names(selprop))
    a=apply(out$Beta[ArgmaxId(out)[1],-c(1:7),],1,FUN=function(x){sum(x>0)})
    b=apply(out$Beta[ArgmaxId(out)[1],-c(1:7),],1,FUN=function(x){sum(x<0)})
    
    newpath = gsub("output","inspect",myfiles[i]) %>% gsub(".rds",".pdf",.)
    pdf(paste0("../Figures/Multivariate/",newpath))
    par(mar=c(5,5,1,1))
    plot(a/(a+b), selprop, las=1, pch=19, col="navy", cex.lab=1.5,
         xlab="Proportion of positive loadings among non-zero loadings", 
         ylab="Selection Proportion") # Ideally no point around 0.5 with high selection proportion
    dev.off()
  }
}

myfiles = list.files(path = "../Results/Multivariate/", pattern = "*f3_output_*")
for (i in 1:length(myfiles)){
  # Load data
  out = readRDS(paste0("../Results/Multivariate/",myfiles[i]))
  if(sum(SelectedVariables(out))<1){
    print(paste0(myfiles[i]," is an empty model"))
  } else{
    selprop = SelectionProportions(out)
    all(colnames(out$Beta)[-c(1:9)] == names(selprop))
    a=apply(out$Beta[ArgmaxId(out)[1],-c(1:9),],1,FUN=function(x){sum(x>0)})
    b=apply(out$Beta[ArgmaxId(out)[1],-c(1:9),],1,FUN=function(x){sum(x<0)})
    
    newpath = gsub("output","inspect",myfiles[i]) %>% gsub(".rds",".pdf",.)
    pdf(paste0("../Figures/Multivariate/",newpath))
    par(mar=c(5,5,1,1))
    plot(a/(a+b), selprop, las=1, pch=19, col="navy", cex.lab=1.5,
         xlab="Proportion of positive loadings among non-zero loadings", 
         ylab="Selection Proportion") # Ideally no point around 0.5 with high selection proportion
    dev.off()
  }
}

myfiles = list.files(path = "../Results/Multivariate/", pattern = "*f4_output_*")
for (i in 1:length(myfiles)){
  # Load data
  out = readRDS(paste0("../Results/Multivariate/",myfiles[i]))
  if(sum(SelectedVariables(out))<1){
    print(paste0(myfiles[i]," is an empty model"))
  } else{
    selprop = SelectionProportions(out)
    all(colnames(out$Beta)[-c(1:13)] == names(selprop))
    a=apply(out$Beta[ArgmaxId(out)[1],-c(1:13),],1,FUN=function(x){sum(x>0)})
    b=apply(out$Beta[ArgmaxId(out)[1],-c(1:13),],1,FUN=function(x){sum(x<0)})
    
    newpath = gsub("output","inspect",myfiles[i]) %>% gsub(".rds",".pdf",.)
    pdf(paste0("../Figures/Multivariate/",newpath))
    par(mar=c(5,5,1,1))
    plot(a/(a+b), selprop, las=1, pch=19, col="navy", cex.lab=1.5,
         xlab="Proportion of positive loadings among non-zero loadings", 
         ylab="Selection Proportion") # Ideally no point around 0.5 with high selection proportion
    dev.off()
  }
}

## Recalibration and Predictive performance: Binary outcome----
myfiles = list.files(path = "../Results/Multivariate/", pattern = "Binary_outcome_expo_f1_output_*")
for (i in 1:length(myfiles)){
  # Load data
  split = gsub("Binary_outcome_expo_f1_output_","",myfiles[i])
  outcome_type = gsub("_PesticidesPCBBDEs_train.rds|_PAHs_train.rds","", split)
  data_type = gsub(paste0(outcome_type,"_"),"",split) %>% gsub(".rds","",.)
  
  data = readRDS(paste0("../Processed/","Overlap_test","_XY.rds"))
  out = readRDS(paste0("../Results/Multivariate/",myfiles[i]))
  if(sum(SelectedVariables(out))<1){
    print(paste0(myfiles[i]," is an empty model"))
  } else{
    # Extract data
    Y_test = as.integer(data[,outcome_type])-1
    
    tmp = model.matrix(~ Smoking + Education + Physical_activity + Alcohol, data)[,-1]
    X_test = cbind(data$Age,
                   data$Gender,
                   tmp[,1:2],
                   data[,c("DietScore1","DietScore2","DietScore3")],
                   data[,c(27:ncol(data))])
    X_test = X_test %>% mutate_if(is.factor, function(x) as.integer(x)-1)
    
    train = readRDS(paste0("../Processed/",data_type,"_XY.rds"))
    Y_train = as.integer(train[,outcome_type])-1
    
    tmp = model.matrix(~ Smoking + Education + Physical_activity + Alcohol, train)[,-1]
    X_train = cbind(train$Age,
                    train$Gender,
                    tmp[,1:2],
                    train[,c("DietScore1","DietScore2","DietScore3")],
                    train[,c(27:ncol(train))])
    X_train = X_train %>% mutate_if(is.factor, function(x) as.integer(x)-1)
    
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
      mymodel=glm(Y_test ~offset(tmp), family="binomial")
      pred = prediction(mymodel$fitted.values, Y_test)
      f1 = performance(pred,"f")
      curve = data.frame(cutoffs = unlist(f1@x.values), f1 =unlist(f1@y.values))
      if(k==calib){
        saveRDS(mymodel$fitted.values, paste0("../Results/Multivariate/",gsub("output","predictions",myfiles[i])))
        recalib = data.frame(beta = summary(mymodel_recalib)$coefficients[-1,1],
                             se = summary(mymodel_recalib)$coefficients[-1,2])
        saveRDS(recalib, paste0("../Results/Multivariate/",gsub("output","recalib_beta",myfiles[i])))
      }
      if(length(myorder)==1){
        saveRDS(curve, paste0("../Results/Multivariate/",gsub("output","performance",myfiles[i])))
      } else{
        myf1[[k]]=curve
      }
    }
    if(!is.null(myf1)){
      names(myf1) = myorder
      saveRDS(myf1, paste0("../Results/Multivariate/",gsub("output","performance",myfiles[i])))
    } 
  }
}

myfiles = list.files(path = "../Results/Multivariate/", pattern = "Binary_outcome_expo_f3_output_*")
for (i in 1:length(myfiles)){
  # Load data
  split = gsub("Binary_outcome_expo_f3_output_","",myfiles[i])
  outcome_type = gsub("_PesticidesPCBBDEs_train.rds|_PAHs_train.rds","", split)
  data_type = gsub(paste0(outcome_type,"_"),"",split) %>% gsub(".rds","",.)
  
  data = readRDS(paste0("../Processed/","Overlap_test","_XY.rds"))
  out = readRDS(paste0("../Results/Multivariate/",myfiles[i]))
  if(sum(SelectedVariables(out))<1){
    print(paste0(myfiles[i]," is an empty model"))
  } else{
    # Extract data
    Y_test = as.integer(data[,outcome_type])-1
    
    tmp = model.matrix(~ Smoking + Education + Physical_activity + Alcohol, data)[,-1]
    X_test = cbind(data$Age,
                   data$Gender,
                   tmp[,1:2],
                   data[,c("DietScore1","DietScore2","DietScore3")],
                   tmp[,3:4],
                   data[,c(27:ncol(data))])
    X_test = X_test %>% mutate_if(is.factor, function(x) as.integer(x)-1)
    
    train = readRDS(paste0("../Processed/",data_type,"_XY.rds"))
    Y_train = as.integer(train[,outcome_type])-1
    
    tmp = model.matrix(~ Smoking + Education + Physical_activity + Alcohol, train)[,-1]
    X_train = cbind(train$Age,
                    train$Gender,
                    tmp[,1:2],
                    train[,c("DietScore1","DietScore2","DietScore3")],
                    tmp[,3:4],
                    train[,c(27:ncol(train))])
    X_train = X_train %>% mutate_if(is.factor, function(x) as.integer(x)-1)
    
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
      tmp = X_train[,c(colnames(X_train)[1:9],myorder[1:k]),drop=FALSE]
      mymodel_recalib=glm(Y_train ~., data = as.data.frame(tmp), family="binomial")
      # Prediction using a logistic model with recalibrated beta coefficients
      tmp = as.matrix(X_test[,c(colnames(X_test)[1:9],myorder[1:k]),drop=FALSE])%*%as.matrix(coef(mymodel_recalib)[-1], ncol=1)
      mymodel=glm(Y_test ~offset(tmp), family="binomial")
      pred = prediction(mymodel$fitted.values, Y_test)
      f1 = performance(pred,"f")
      curve = data.frame(cutoffs = unlist(f1@x.values), f1 =unlist(f1@y.values))
      if(k==calib){
        saveRDS(mymodel$fitted.values, paste0("../Results/Multivariate/",gsub("output","predictions",myfiles[i])))
        recalib = data.frame(beta = summary(mymodel_recalib)$coefficients[-1,1],
                             se = summary(mymodel_recalib)$coefficients[-1,2])
        saveRDS(recalib, paste0("../Results/Multivariate/",gsub("output","recalib_beta",myfiles[i])))
      }
      if(length(myorder)==1){
        saveRDS(curve, paste0("../Results/Multivariate/",gsub("output","performance",myfiles[i])))
      } else{
        myf1[[k]]=curve
      }
    }
    if(!is.null(myf1)){
      names(myf1) = myorder
      saveRDS(myf1, paste0("../Results/Multivariate/",gsub("output","performance",myfiles[i])))
    } 
  }
}

myfiles = list.files(path = "../Results/Multivariate/", pattern = "Binary_outcome_expo_f4_output_*")
for (i in 1:length(myfiles)){
  # Load data
  split = gsub("Binary_outcome_expo_f4_output_","",myfiles[i])
  outcome_type = gsub("_PesticidesPCBBDEs_train.rds|_PAHs_train.rds","", split)
  data_type = gsub(paste0(outcome_type,"_"),"",split) %>% gsub(".rds","",.)
  
  data = readRDS(paste0("../Processed/","Overlap_test","_XY.rds"))
  out = readRDS(paste0("../Results/Multivariate/",myfiles[i]))
  if(sum(SelectedVariables(out))<1){
    print(paste0(myfiles[i]," is an empty model"))
  } else{
    # Extract data
    Y_test = as.integer(data[,outcome_type])-1
    
    tmp = model.matrix(~ Smoking + Education + Physical_activity + Alcohol, data)[,-1]
    X_test = cbind(data$Age,
                   data$Gender,
                   tmp[,1:2],
                   data[,c("DietScore1","DietScore2","DietScore3")],
                   tmp[,3:ncol(tmp)],
                   data[,c(27:ncol(data))])
    X_test = X_test %>% mutate_if(is.factor, function(x) as.integer(x)-1)
    
    train = readRDS(paste0("../Processed/",data_type,"_XY.rds"))
    Y_train = as.integer(train[,outcome_type])-1
    
    tmp = model.matrix(~ Smoking + Education + Physical_activity + Alcohol, train)[,-1]
    X_train = cbind(train$Age,
                    train$Gender,
                    tmp[,1:2],
                    train[,c("DietScore1","DietScore2","DietScore3")],
                    tmp[,3:ncol(tmp)],
                    train[,c(27:ncol(train))])
    X_train = X_train %>% mutate_if(is.factor, function(x) as.integer(x)-1)
    
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
      tmp = X_train[,c(colnames(X_train)[1:13],myorder[1:k]),drop=FALSE]
      mymodel_recalib=glm(Y_train ~., data = as.data.frame(tmp), family="binomial")
      # Prediction using a logistic model with recalibrated beta coefficients
      tmp = as.matrix(X_test[,c(colnames(X_test)[1:13],myorder[1:k]),drop=FALSE])%*%as.matrix(coef(mymodel_recalib)[-1], ncol=1)
      mymodel=glm(Y_test ~offset(tmp), family="binomial")
      pred = prediction(mymodel$fitted.values, Y_test)
      f1 = performance(pred,"f")
      curve = data.frame(cutoffs = unlist(f1@x.values), f1 =unlist(f1@y.values))
      if(k==calib){
        saveRDS(mymodel$fitted.values, paste0("../Results/Multivariate/",gsub("output","predictions",myfiles[i])))
        recalib = data.frame(beta = summary(mymodel_recalib)$coefficients[-1,1],
                             se = summary(mymodel_recalib)$coefficients[-1,2])
        saveRDS(recalib, paste0("../Results/Multivariate/",gsub("output","recalib_beta",myfiles[i])))
      }
      if(length(myorder)==1){
        saveRDS(curve, paste0("../Results/Multivariate/",gsub("output","performance",myfiles[i])))
      } else{
        myf1[[k]]=curve
      }
    }
    if(!is.null(myf1)){
      names(myf1) = myorder
      saveRDS(myf1, paste0("../Results/Multivariate/",gsub("output","performance",myfiles[i])))
    } 
  }
}


## Recalibration and Predictive performance: Continuous outcome----
myfiles = list.files(path = "../Results/Multivariate/", pattern = "Continuous_outcome_expo_f1_output_*")
for (i in 1:length(myfiles)){
  # Load data
  split = gsub("Continuous_outcome_expo_f1_output_","",myfiles[i])
  outcome_type = gsub("_PesticidesPCBBDEs_train.rds|_PAHs_train.rds","", split)
  data_type = gsub(paste0(outcome_type,"_"),"",split) %>% gsub(".rds","",.)
  
  data = readRDS(paste0("../Processed/","Overlap_test","_XY.rds"))
  out = readRDS(paste0("../Results/Multivariate/",myfiles[i]))
  if(sum(SelectedVariables(out))<1){
    print(paste0(myfiles[i]," is an empty model"))
  } else{
    # Extract data
    Y_test = data[,outcome_type]
    Y_test = scale(Y_test)
    tmp = model.matrix(~ Smoking + Education + Physical_activity + Alcohol, data)[,-1]
    X_test = cbind(data$Age,
                   data$Gender,
                   tmp[,1:2],
                   data[,c("DietScore1","DietScore2","DietScore3")],
                   data[,c(27:ncol(data))])
    X_test = X_test %>% mutate_if(is.factor, function(x) as.integer(x)-1)
    
    train = readRDS(paste0("../Processed/",data_type,"_XY.rds"))
    Y_train = train[,outcome_type]
    Y_train = scale(Y_train)
    tmp = model.matrix(~ Smoking + Education + Physical_activity + Alcohol, train)[,-1]
    X_train = cbind(train$Age,
                    train$Gender,
                    tmp[,1:2],
                    train[,c("DietScore1","DietScore2","DietScore3")],
                    train[,c(27:ncol(train))])
    X_train = X_train %>% mutate_if(is.factor, function(x) as.integer(x)-1)
    
    # Scale data
    X_test = scale(X_test)
    X_train = scale(X_train)
    
    # Computing AUCs on the test set from models where predictors are added in order of decreasing selection proportion
    selprop = SelectionProportions(out)
    selprop_nonzero = selprop[SelectedVariables(out)==1]
    myorder = names(selprop_nonzero)[sort.list(selprop_nonzero, decreasing = TRUE)]
    calib = sum(SelectedVariables(out)==1)
    
    myrmse = NULL
    for (k in 1:length(myorder)){
      # Recalibration of the beta coefficients on the training set
      tmp = X_train[,c(colnames(X_train)[1:7],myorder[1:k]),drop=FALSE]
      mymodel_recalib=lm(Y_train ~., data = as.data.frame(tmp))
      # Prediction using a logistic model with recalibrated beta coefficients
      tmp = as.matrix(X_test[,c(colnames(X_test)[1:7],myorder[1:k]),drop=FALSE])%*%as.matrix(coef(mymodel_recalib)[-1], ncol=1)
      mymodel=lm(Y_test ~offset(tmp))
      rmse = sqrt(crossprod(mymodel$residuals)/length(mymodel$residuals))
      if(k==calib){
        saveRDS(mymodel$residuals, paste0("../Results/Multivariate/",gsub("output","residuals",myfiles[i])))
        recalib = data.frame(beta = summary(mymodel_recalib)$coefficients[-1,1],
                             se = summary(mymodel_recalib)$coefficients[-1,2])
        saveRDS(recalib, paste0("../Results/Multivariate/",gsub("output","recalib_beta",myfiles[i])))
      }
      if(length(myorder)==1){
        saveRDS(rmse, paste0("../Results/Multivariate/",gsub("output","performance",myfiles[i])))
      } else{
        myrmse=c(myrmse,rmse)
      }
    }
    if(!is.null(myrmse)){
      names(myrmse) = myorder
      saveRDS(myrmse, paste0("../Results/Multivariate/",gsub("output","performance",myfiles[i])))
    } 
  }
}

myfiles = list.files(path = "../Results/Multivariate/", pattern = "Continuous_outcome_expo_f3_output_*")
for (i in 1:length(myfiles)){
  # Load data
  split = gsub("Continuous_outcome_expo_f3_output_","",myfiles[i])
  outcome_type = gsub("_PesticidesPCBBDEs_train.rds|_PAHs_train.rds","", split)
  data_type = gsub(paste0(outcome_type,"_"),"",split) %>% gsub(".rds","",.)
  
  data = readRDS(paste0("../Processed/","Overlap_test","_XY.rds"))
  out = readRDS(paste0("../Results/Multivariate/",myfiles[i]))
  if(sum(SelectedVariables(out))<1){
    print(paste0(myfiles[i]," is an empty model"))
  } else{
    # Extract data
    Y_test = data[,outcome_type]
    Y_test = scale(Y_test)
    tmp = model.matrix(~ Smoking + Education + Physical_activity + Alcohol, data)[,-1]
    X_test = cbind(data$Age,
                   data$Gender,
                   tmp[,1:2],
                   data[,c("DietScore1","DietScore2","DietScore3")],
                   tmp[,3:4],
                   data[,c(27:ncol(data))])
    X_test = X_test %>% mutate_if(is.factor, function(x) as.integer(x)-1)
    
    train = readRDS(paste0("../Processed/",data_type,"_XY.rds"))
    Y_train = train[,outcome_type]
    Y_train = scale(Y_train)
    tmp = model.matrix(~ Smoking + Education + Physical_activity + Alcohol, train)[,-1]
    X_train = cbind(train$Age,
                    train$Gender,
                    tmp[,1:2],
                    train[,c("DietScore1","DietScore2","DietScore3")],
                    tmp[,3:4],
                    train[,c(27:ncol(train))])
    X_train = X_train %>% mutate_if(is.factor, function(x) as.integer(x)-1)
    
    # Scale data
    X_test = scale(X_test)
    X_train = scale(X_train)
    
    # Computing AUCs on the test set from models where predictors are added in order of decreasing selection proportion
    selprop = SelectionProportions(out)
    selprop_nonzero = selprop[SelectedVariables(out)==1]
    myorder = names(selprop_nonzero)[sort.list(selprop_nonzero, decreasing = TRUE)]
    calib = sum(SelectedVariables(out)==1)
    
    myrmse = NULL
    for (k in 1:length(myorder)){
      # Recalibration of the beta coefficients on the training set
      tmp = X_train[,c(colnames(X_train)[1:9],myorder[1:k]),drop=FALSE]
      mymodel_recalib=lm(Y_train ~., data = as.data.frame(tmp))
      # Prediction using a logistic model with recalibrated beta coefficients
      tmp = as.matrix(X_test[,c(colnames(X_test)[1:9],myorder[1:k]),drop=FALSE])%*%as.matrix(coef(mymodel_recalib)[-1], ncol=1)
      mymodel=lm(Y_test ~offset(tmp))
      rmse = sqrt(crossprod(mymodel$residuals)/length(mymodel$residuals))
      if(k==calib){
        saveRDS(mymodel$residuals, paste0("../Results/Multivariate/",gsub("output","residuals",myfiles[i])))
        recalib = data.frame(beta = summary(mymodel_recalib)$coefficients[-1,1],
                             se = summary(mymodel_recalib)$coefficients[-1,2])
        saveRDS(recalib, paste0("../Results/Multivariate/",gsub("output","recalib_beta",myfiles[i])))
      }
      if(length(myorder)==1){
        saveRDS(rmse, paste0("../Results/Multivariate/",gsub("output","performance",myfiles[i])))
      } else{
        myrmse=c(myrmse,rmse)
      }
    }
    if(!is.null(myrmse)){
      names(myrmse) = myorder
      saveRDS(myrmse, paste0("../Results/Multivariate/",gsub("output","performance",myfiles[i])))
    } 
  }
}

myfiles = list.files(path = "../Results/Multivariate/", pattern = "Continuous_outcome_expo_f4_output_*")
for (i in 1:length(myfiles)){
  # Load data
  split = gsub("Continuous_outcome_expo_f4_output_","",myfiles[i])
  outcome_type = gsub("_PesticidesPCBBDEs_train.rds|_PAHs_train.rds","", split)
  data_type = gsub(paste0(outcome_type,"_"),"",split) %>% gsub(".rds","",.)
  
  data = readRDS(paste0("../Processed/","Overlap_test","_XY.rds"))
  out = readRDS(paste0("../Results/Multivariate/",myfiles[i]))
  if(sum(SelectedVariables(out))<1){
    print(paste0(myfiles[i]," is an empty model"))
  } else{
    # Extract data
    Y_test = data[,outcome_type]
    Y_test = scale(Y_test)
    tmp = model.matrix(~ Smoking + Education + Physical_activity + Alcohol, data)[,-1]
    X_test = cbind(data$Age,
                   data$Gender,
                   tmp[,1:2],
                   data[,c("DietScore1","DietScore2","DietScore3")],
                   tmp[,3:ncol(tmp)],
                   data[,c(27:ncol(data))])
    X_test = X_test %>% mutate_if(is.factor, function(x) as.integer(x)-1)
    
    train = readRDS(paste0("../Processed/",data_type,"_XY.rds"))
    Y_train = train[,outcome_type]
    Y_train = scale(Y_train)
    tmp = model.matrix(~ Smoking + Education + Physical_activity + Alcohol, train)[,-1]
    X_train = cbind(train$Age,
                    train$Gender,
                    tmp[,1:2],
                    train[,c("DietScore1","DietScore2","DietScore3")],
                    tmp[,3:ncol(tmp)],
                    train[,c(27:ncol(train))])
    X_train = X_train %>% mutate_if(is.factor, function(x) as.integer(x)-1)
    
    # Scale data
    X_test = scale(X_test)
    X_train = scale(X_train)
    
    # Computing AUCs on the test set from models where predictors are added in order of decreasing selection proportion
    selprop = SelectionProportions(out)
    selprop_nonzero = selprop[SelectedVariables(out)==1]
    myorder = names(selprop_nonzero)[sort.list(selprop_nonzero, decreasing = TRUE)]
    calib = sum(SelectedVariables(out)==1)
    
    myrmse = NULL
    for (k in 1:length(myorder)){
      # Recalibration of the beta coefficients on the training set
      tmp = X_train[,c(colnames(X_train)[1:13],myorder[1:k]),drop=FALSE]
      mymodel_recalib=lm(Y_train ~., data = as.data.frame(tmp))
      # Prediction using a logistic model with recalibrated beta coefficients
      tmp = as.matrix(X_test[,c(colnames(X_test)[1:13],myorder[1:k]),drop=FALSE])%*%as.matrix(coef(mymodel_recalib)[-1], ncol=1)
      mymodel=lm(Y_test ~offset(tmp))
      rmse = sqrt(crossprod(mymodel$residuals)/length(mymodel$residuals))
      if(k==calib){
        saveRDS(mymodel$residuals, paste0("../Results/Multivariate/",gsub("output","residuals",myfiles[i])))
        recalib = data.frame(beta = summary(mymodel_recalib)$coefficients[-1,1],
                             se = summary(mymodel_recalib)$coefficients[-1,2])
        saveRDS(recalib, paste0("../Results/Multivariate/",gsub("output","recalib_beta",myfiles[i])))
      }
      if(length(myorder)==1){
        saveRDS(rmse, paste0("../Results/Multivariate/",gsub("output","performance",myfiles[i])))
      } else{
        myrmse=c(myrmse,rmse)
      }
    }
    if(!is.null(myrmse)){
      names(myrmse) = myorder
      saveRDS(myrmse, paste0("../Results/Multivariate/",gsub("output","performance",myfiles[i])))
    } 
  }
}

