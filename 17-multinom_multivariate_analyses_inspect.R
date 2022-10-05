## Inspection of stability-based LASSO logistic regression

rm(list=ls())
project_path=dirname(rstudioapi::getActiveDocumentContext()$path)
setwd(project_path)

library(tidyverse)
library(sharp)
library(nnet)

## Extract calibrated parameters----

myfiles = list.files(path = "../Results/Comorbidity/", pattern = "*_output_*")
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
saveRDS(param, paste0("../Results/Comorbidity/Multivariate_cluster_outcome_expo_param_summary.rds"))

## Inspect empty models and direction of betas of stably selected variables ----
myfiles = list.files(path = "../Results/Comorbidity/", pattern = "*_f1_output_*")
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

myfiles = list.files(path = "../Results/Comorbidity/", pattern = "*_f3_output_*")
for (i in 1:length(myfiles)){
  # Load data
  out = readRDS(paste0("../Results/Comorbidity/",myfiles[i]))
  if(sum(SelectedVariables(out))<1){
    print(paste0(myfiles[i]," is an empty model"))
  } else{
    selprop = SelectionProportions(out)
    all(colnames(out$Beta)[-c(1:9)] == names(selprop))
    a=apply(out$Beta[ArgmaxId(out)[1],-c(1:9),,],c(1,3),FUN=function(x){sum(x>0)})
    b=apply(out$Beta[ArgmaxId(out)[1],-c(1:9),,],c(1,3),FUN=function(x){sum(x<0)})
    
    newpath = gsub("output","inspect",myfiles[i]) %>% gsub(".rds",".pdf",.)
    pdf(paste0("../Figures/Comorbidity/",newpath))
    par(mar=c(5,5,1,1))
    plot(a/(a+b), rep(selprop,each=ncol(a)), las=1, pch=19, col="navy", cex.lab=1.5,
         xlab="Proportion of positive loadings among non-zero loadings", 
         ylab="Selection Proportion") # Ideally no point around 0.5 with high selection proportion
    dev.off()
  }
}


myfiles = list.files(path = "../Results/Comorbidity/", pattern = "*f4_output_*")
for (i in 1:length(myfiles)){
  # Load data
  out = readRDS(paste0("../Results/Comorbidity/",myfiles[i]))
  if(sum(SelectedVariables(out))<1){
    print(paste0(myfiles[i]," is an empty model"))
  } else{
    selprop = SelectionProportions(out)
    all(colnames(out$Beta)[-c(1:13)] == names(selprop))
    a=apply(out$Beta[ArgmaxId(out)[1],-c(1:13),,],c(1,3),FUN=function(x){sum(x>0)})
    b=apply(out$Beta[ArgmaxId(out)[1],-c(1:13),,],c(1,3),FUN=function(x){sum(x<0)})
    
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
myfiles = list.files(path = "../Results/Comorbidity/", pattern = "*_f1_output_*")
for (i in 1:length(myfiles)){
  # Load data
  split = gsub("Multivariate_cluster_outcome_expo_f1_output_","",myfiles[i])
  data_type = gsub(".rds","",split)
  
  data = readRDS(paste0("../Processed/Overlap_test_XY.rds"))
  out = readRDS(paste0("../Results/Comorbidity/",myfiles[i]))
  
  if(sum(SelectedVariables(out))<1){
    print(paste0(myfiles[i]," is an empty model"))
  } else{
    # Extract data
    Y_test = data$OutcomeCluster
    
    tmp = model.matrix(~ Smoking + Education + Physical_activity + Alcohol, data)[,-1]
    X_test = cbind(data$Age,
                   data$Gender,
                   tmp[,1:2],
                   data[,c("DietScore1","DietScore2","DietScore3")],
                   data[,c(27:ncol(data))])
    X_test = X_test %>% mutate_if(is.factor, function(x) as.integer(x)-1)
    
    train = readRDS(paste0("../Processed/",data_type,"_XY.rds"))
    Y_train = train$OutcomeCluster
    
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
}

myfiles = list.files(path = "../Results/Comorbidity/", pattern = "*_f3_output_*")
for (i in 1:length(myfiles)){
  # Load data
  split = gsub("Multivariate_cluster_outcome_expo_f3_output_","",myfiles[i])
  data_type = gsub(".rds","",split)
  
  data = readRDS(paste0("../Processed/Overlap_test_XY.rds"))
  out = readRDS(paste0("../Results/Comorbidity/",myfiles[i]))
  
  if(sum(SelectedVariables(out))<1){
    print(paste0(myfiles[i]," is an empty model"))
  } else{
    # Extract data
    Y_test = data$OutcomeCluster
    
    tmp = model.matrix(~ Smoking + Education + Physical_activity + Alcohol, data)[,-1]
    X_test = cbind(data$Age,
                   data$Gender,
                   tmp[,1:2],
                   data[,c("DietScore1","DietScore2","DietScore3")],
                   tmp[,3:4],
                   data[,c(27:ncol(data))])
    X_test = X_test %>% mutate_if(is.factor, function(x) as.integer(x)-1)
    
    train = readRDS(paste0("../Processed/",data_type,"_XY.rds"))
    Y_train = train$OutcomeCluster
    
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
    
    colnames(X_train)[1:9] = colnames(X_test)[1:9]
    
    # Computing AUCs on the test set from models where predictors are added in order of decreasing selection proportion
    selprop = SelectionProportions(out)
    selprop_nonzero = selprop[SelectedVariables(out)==1]
    myorder = names(selprop_nonzero)[sort.list(selprop_nonzero, decreasing = TRUE)]
    calib = sum(SelectedVariables(out)==1)
    
    myf1 = NULL
    for (k in 1:length(myorder)){
      # Recalibration of the beta coefficients on the training set
      tmp1 = X_train[,c(colnames(X_train)[1:9],myorder[1:k]),drop=FALSE]
      mymodel_recalib=multinom(Y_train ~., data = as.data.frame(tmp1))
      # Prediction using a logistic model with recalibrated beta coefficients
      tmp2 = X_test[,c(colnames(X_test)[1:9],myorder[1:k]),drop=FALSE]
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
}

myfiles = list.files(path = "../Results/Comorbidity/", pattern = "*_f4_output_*")
for (i in 1:length(myfiles)){
  # Load data
  split = gsub("Multivariate_cluster_outcome_expo_f4_output_","",myfiles[i])
  data_type = gsub(".rds","",split)
  
  data = readRDS(paste0("../Processed/Overlap_test_XY.rds"))
  out = readRDS(paste0("../Results/Comorbidity/",myfiles[i]))
  
  if(sum(SelectedVariables(out))<1){
    print(paste0(myfiles[i]," is an empty model"))
  } else{
    # Extract data
    Y_test = data$OutcomeCluster
    
    tmp = model.matrix(~ Smoking + Education + Physical_activity + Alcohol, data)[,-1]
    X_test = cbind(data$Age,
                   data$Gender,
                   tmp[,1:2],
                   data[,c("DietScore1","DietScore2","DietScore3")],
                   tmp[,3:ncol(tmp)],
                   data[,c(27:ncol(data))])
    X_test = X_test %>% mutate_if(is.factor, function(x) as.integer(x)-1)
    
    train = readRDS(paste0("../Processed/",data_type,"_XY.rds"))
    Y_train = train$OutcomeCluster
    
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
    
    colnames(X_train)[1:13] = colnames(X_test)[1:13]
    
    # Computing AUCs on the test set from models where predictors are added in order of decreasing selection proportion
    selprop = SelectionProportions(out)
    selprop_nonzero = selprop[SelectedVariables(out)==1]
    myorder = names(selprop_nonzero)[sort.list(selprop_nonzero, decreasing = TRUE)]
    calib = sum(SelectedVariables(out)==1)
    
    myf1 = NULL
    for (k in 1:length(myorder)){
      # Recalibration of the beta coefficients on the training set
      tmp1 = X_train[,c(colnames(X_train)[1:13],myorder[1:k]),drop=FALSE]
      mymodel_recalib=multinom(Y_train ~., data = as.data.frame(tmp1))
      # Prediction using a logistic model with recalibrated beta coefficients
      tmp2 = X_test[,c(colnames(X_test)[1:13],myorder[1:k]),drop=FALSE]
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
}
