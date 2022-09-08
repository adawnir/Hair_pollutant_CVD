## Inspection of stability-based LASSO logistic regression

rm(list=ls())
project_path=dirname(rstudioapi::getActiveDocumentContext()$path)
setwd(project_path)
setwd("../")

library(tidyverse)
library(sharp)
library(pROC)

## Extract calibrated parameters----
filepaths = c("PesticidesPCBBDEs","PAHs","Combined")
strata = c("Female","Male")
for (i in 1:2){
  for (h in 1:2){
    data = readRDS(paste0("../Processed/",filepaths[i],"_XY.rds"))
    Y = data[,c(23:27)]
    suffix =  gsub(" ", "_", colnames(Y))
    param = NULL
    for (k in c(1,2,4,5)){
      out = readRDS(paste0("../Results/Gender/Multivariate_analysis_adjust_output_",suffix[k],"_",filepaths[i],"_",strata[h],".rds"))
      tmp = Argmax(out)
      param = rbind(param, tmp)
    }
    rownames(param) = colnames(Y)[c(1,2,4,5)]
    saveRDS(param, paste0("../Results/Gender/Multivariate_analysis_adjust_param_",filepaths[i],"_",strata[h],".rds")) 
  }
}

## Inspect direction of betas of stably selected variables ----
myfiles = list.files(path = "../Results/Gender/", pattern = "Multivariate_analysis_adjust_output_*")
for (i in 1:length(myfiles)){
  # Load data
  out = readRDS(paste0("../Results/Gender/",myfiles[i]))
  selprop = SelectionProportions(out)
  all(colnames(out$Beta)[-c(1:2)] == names(selprop))
  a=apply(out$Beta[ArgmaxId(out)[1],-c(1:2),],1,FUN=function(x){sum(x>0)})
  b=apply(out$Beta[ArgmaxId(out)[1],-c(1:2),],1,FUN=function(x){sum(x<0)})
  
  newpath = gsub("output","inspect",myfiles[i]) %>% gsub(".rds",".pdf",.)
  pdf(paste0("../Figures/Gender/",newpath))
  par(mar=c(5,5,1,1))
  plot(a/(a+b), selprop, las=1, pch=19, col="navy", cex.lab=1.5,
       xlab="Proportion of positive loadings among non-zero loadings", 
       ylab="Selection Proportion") # Ideally no point around 0.5 with high selection proportion
  dev.off()
}

## Recalibration and Predictive performance----
myfiles = list.files(path = "../Results/Gender/", pattern = "Multivariate_analysis_adjust_output_*")
for (i in 1:length(myfiles)){
  # Load data
  split = strsplit(myfiles[i], "_(?!.*_)", perl=TRUE)[[1]]
  splitb = gsub("Multivariate_analysis_adjust_output_","",split[1]) %>% strsplit(., "_(?!.*_)", perl=TRUE) %>% .[[1]]
  strata_type = gsub(".rds","",split[2])
  data_type = splitb[2]
  outcome_type = splitb[1] 
  
  data = readRDS(paste0("../Processed/",data_type,"_XY_",strata_type,".rds"))
  out = readRDS(paste0("../Results/Gender/",myfiles[i]))
  
  # Extract data
  genderfemale = ifelse(data$Gender == "Female", 1, 0)
  formersmoker = ifelse(data$Smoking == "Former smoker", 1, 0)
  currentsmoker = ifelse(data$Smoking == "Current smoker", 1, 0)
  X = cbind(data$Age,
            genderfemale,
            formersmoker,
            currentsmoker,
            data[,c("NutriScore1","NutriScore2","NutriScore3")],
            data[,c(28:ncol(data))])
  
  Y = data[,c(23:27)]
  Y = Y %>% mutate_all(as.integer)-1

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
  
  vars=c("Age",names(SelectedVariables(out)[SelectedVariables(out)==1]))
  
  # Computing AUCs on the test set from models where predictors are added in order of decreasing selection proportion
  selprop = SelectionProportions(out)
  selprop_nonzero = selprop[SelectedVariables(out)==1]
  myorder = names(selprop_nonzero)[sort.list(selprop_nonzero, decreasing = TRUE)]
  myorder = c("Age",myorder)
  calib = sum(SelectedVariables(out)==1)+1
  
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
      saveRDS(myroc, paste0("../Results/Gender/",gsub("output","ROC",myfiles[i])))
      beta = coef(mymodel_recalib)[-1]
      saveRDS(beta, paste0("../Results/Gender/",gsub("output","beta",myfiles[i])))
    }
    myaucs=c(myaucs, myroc$auc)
    myaucs_table=rbind(myaucs_table, formatC(as.numeric(ci.auc(myroc)), format="f", digits=2))
  }
  rownames(myaucs_table)=myorder
  colnames(myaucs_table)=c("li","auc","ui")
  saveRDS(myaucs_table, paste0("../Results/Gender/",gsub("output","AUC",myfiles[i])))
}
