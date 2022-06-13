## Inspection of stability-based LASSO logistic regression

rm(list=ls())
project_path=dirname(rstudioapi::getActiveDocumentContext()$path)
setwd(project_path)

library(tidyverse)
library(sharp)
library(pROC)

## Extract calibrated parameters----
filepaths = c("PesticidesPCBBDEs","PAHs","Combined")
for (i in 1:3){
  data = readRDS(paste0("../Processed/",filepaths[i],"_XY.rds"))
  Y = data[,c(16:20)]
  suffix =  gsub(" ", "_", colnames(Y))
  param = NULL
  if (i == 3){
    for (k in c(1,2,4,5)){
      out = readRDS(paste0("../Results/Multivariate_analysis_output_",suffix[k],"_",filepaths[i],".rds"))
      tmp = Argmax(out)
      param = rbind(param, tmp)
    }
    rownames(param) = colnames(Y)[c(1,2,4,5)]
  } else {
    for (k in 1:5){
      out = readRDS(paste0("../Results/Multivariate_analysis_output_",suffix[k],"_",filepaths[i],".rds"))
      tmp = Argmax(out)
      param = rbind(param, tmp)
    }
    rownames(param) = colnames(Y)
  }
  saveRDS(param, paste0("../Results/Multivariate_analysis_param_",filepaths[i],".rds"))
}

## Inspect non-convergence warning for diabetes outcome----
filepaths = c("PesticidesPCBBDEs","PAHs","Combined")
out = readRDS(paste0("../Results/Multivariate_analysis_output_Diabetes_",filepaths[1],".rds"))
ArgmaxId(out)
out = readRDS(paste0("../Results/Multivariate_analysis_output_Diabetes_",filepaths[2],".rds"))
ArgmaxId(out)

## Inspect direction of betas of stably selected variables ----
myfiles = list.files(path = "../Results", pattern = "Multivariate_analysis_output_*")
for (i in 1:length(myfiles)){
  # Load data
  out = readRDS(paste0("../Results/",myfiles[i]))
  selprop = SelectionProportions(out)
  all(colnames(out$Beta)[-c(1:2)] == names(selprop))
  a=apply(out$Beta[ArgmaxId(out)[1],-c(1:2),],1,FUN=function(x){sum(x>0)})
  b=apply(out$Beta[ArgmaxId(out)[1],-c(1:2),],1,FUN=function(x){sum(x<0)})
  
  newpath = gsub("output","inspect",myfiles[i]) %>% gsub(".rds",".pdf",.)
  pdf(paste0("../Figures/",newpath))
  par(mar=c(5,5,1,1))
  plot(a/(a+b), selprop, las=1, pch=19, col="navy", cex.lab=1.5,
       xlab="Proportion of positive loadings among non-zero loadings", 
       ylab="Selection Proportion") # Ideally no point around 0.5 with high selection proportion
  dev.off()
}

## Recalibration and Predictive performance----
myfiles = list.files(path = "../Results", pattern = "Multivariate_analysis_output_*")

for (i in 1:length(myfiles)){
  # Load data
  split = strsplit(myfiles[i], "_(?!.*_)", perl=TRUE)[[1]]
  data_type = split[2] %>% gsub(".rds","",.)
  outcome_type = gsub("Multivariate_analysis_output_","",split[1])
  
  data = readRDS(paste0("../Processed/",data_type,"_XY.rds"))
  out = readRDS(paste0("../Results/",myfiles[i]))
  
  # Extract data
  X = cbind(data[,c("Age","Gender")],data[,c(21:ncol(data))])
  X = X %>% mutate_if(is.factor, as.integer)-1
  Y = as.integer(data[,gsub("_"," ",outcome_type)])-1

  ## Split training and test
  set.seed(100)
  s0=sample(which(Y=="0"), size=0.7*sum(Y=="0"), replace=FALSE)
  s1=sample(which(Y=="1"), size=0.7*sum(Y=="1"), replace=FALSE)
  train=c(s0,s1)
  Y_train=Y[train]
  X_train=X[train,]
  Y_test=Y[-train]
  X_test=X[-train,]
  
  # Scale data
  X_train = scale(X_train)
  X_test = scale(X_test)
  
  vars=c("Age","Gender",names(SelectedVariables(out)[SelectedVariables(out)==1]))
  
  # Computing AUCs on the test set from models where predictors are added in order of decreasing selection proportion
  selprop = SelectionProportions(out)
  selprop_nonzero = selprop[SelectedVariables(out)==1]
  myorder = names(selprop_nonzero)[sort.list(selprop_nonzero, decreasing = TRUE)]
  myorder = c("Age","Gender",myorder)
  calib = sum(SelectedVariables(out)==1)+2
  
  myaucs = NULL
  myaucs_table = NULL
  for (k in 1:length(myorder)){
    # Recalibration of the beta coefficients on the training set
    tmp = X_train[,myorder[1:k],drop=FALSE]
    mymodel_recalib=glm(Y_train ~., data = as.data.frame(tmp), family="binomial")
    # Prediction using a logistic model with recalibrated beta coefficients
    tmp = as.matrix(X_test[,myorder[1:k],drop=FALSE])%*%as.matrix(coef(mymodel_recalib)[-1], ncol=1)
    mymodel=glm(Y_test ~., data = as.data.frame(offset(tmp)), family="binomial")
    myroc=roc(response=Y_test, predictor=mymodel$fitted.values)
    if(k==calib){
      saveRDS(myroc, paste0("../Results/",gsub("output","ROC",myfiles[i])))
      beta = coef(mymodel_recalib)[-1]
      saveRDS(beta, paste0("../Results/",gsub("output","beta",myfiles[i])))
    }
    myaucs=c(myaucs, myroc$auc)
    myaucs_table=rbind(myaucs_table, formatC(as.numeric(ci.auc(myroc)), format="f", digits=2))
  }
  rownames(myaucs_table)=myorder
  colnames(myaucs_table)=c("li","auc","ui")
  saveRDS(myaucs_table, paste0("../Results/",gsub("output","AUC",myfiles[i])))
}
