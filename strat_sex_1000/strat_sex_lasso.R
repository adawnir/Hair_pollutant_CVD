
### Sumemr Project --  Stratified analysis by sex: Stability selection LASSO Logistic Regression 1000 iterations (RUN ON HPC)
## Created by Vivian on 04 August

rm(list=ls())
project_path="~/hair/scripts"
setwd(project_path)

##### Load packages --
LoadPackages=function(packages){
  for (i in 1:length(packages)){
    suppressPackageStartupMessages(library(packages[i], character.only=TRUE))
  }
}

LoadPackages(c("pheatmap","glasso","glmnet","igraph","pROC","caret"))
source("penalisation_functions.R")

##### Parameters --
args=commandArgs(trailingOnly=TRUE)
m=as.numeric(args[1])


##### Load data set 
arr <- paste0(rep(c("pest_de", "pahs_de"), each = 5), "_", 
              c("obesity", "hypertension", "diabetes", "Lipid_disorder", "MS"), rep(c(".f",".m"),each=10))[m]
dat=readRDS(paste0("../results/strat_sex_denoised/",arr,"_denoised.rds")) 


##### Create directories 
ifelse(dir.exists("../figures/strat_sex_lasso_1000"),"",dir.create("../figures/strat_sex_lasso_1000")) 
ifelse(dir.exists("../results/strat_sex_lasso_1000"),"",dir.create("../results/strat_sex_lasso_1000")) 


##### Run stability selection on full sample --
y=dat[, ncol(dat)]
x=as.matrix(dat[,-c(1, ncol(dat))])

out=CalibrateRegression(xdata=x, ydata=y, K=100, tau=0.5, verbose=FALSE,
                        family="binomial")




### Save plot
pdf(paste0("../figures/strat_sex_lasso_1000/out_",arr,".pdf"), height=5, width=12) 
CalibrationPlot(out)
dev.off()

### Extracting ID of calibrated lambda
hat_lambda_id=GetArgmaxId(out)[1]

### Computing average beta coefficients from models with calibrated lambda
average_beta=apply(out$Beta[hat_lambda_id,,],1,FUN=function(x){mean(x[x!=0])})

### Calibrated selection proportions 
selprop=out$selprop[hat_lambda_id,]

### Save
saveRDS(out, paste0("../results/strat_sex_lasso_1000/out_",arr,".rds")) 
saveRDS(average_beta, paste0("../results/strat_sex_lasso_1000/average_beta_",arr,".rds")) 
saveRDS(selprop, paste0("../results/strat_sex_lasso_1000/selprop_",arr,".rds")) 


##### Recalibration using multiple splits --
selprop_nonzero=selprop[selprop>0]
myorder=names(selprop_nonzero)[sort.list(selprop_nonzero, decreasing = TRUE)]

sen <- NULL
spe <- NULL
mr_con <- NULL
mr_cas <- NULL
myauc <- NULL

for (k in 1:1000) {
  
  # Split data
  set.seed(k)
  train <- createDataPartition(y, p=0.7, list=F)
  y_train <- y[train]
  x_train <- x[train,]
  y_test <- y[-train]
  x_test <- x[-train, ]
  
  myaucs=NULL
  myaucs_table=NULL
  
  calib=sum(CalibratedStableRegression(out)) # Number of selected variables
  
  for (i in 1:length(myorder)){
    # Recalibration of the beta coefficients on the training set
    mymodel_recalib=glm(y_train~x_train[,myorder[1:i],drop=FALSE], family="binomial")
    # Prediction using a logistic model with recalibrated beta coefficients
    mymodel=glm(y_test~offset(x_test[,myorder[1:i],drop=FALSE]%*%matrix(coef(mymodel_recalib)[-1], ncol=1)), family="binomial")
    myroc=roc(response=y_test, predictor=mymodel$fitted.values)
    if(i==calib){
      roc_tmp <- myroc 
    }
    myaucs=c(myaucs, myroc$auc)
    myaucs_table=rbind(myaucs_table, formatC(as.numeric(ci.auc(myroc)), format="f", digits=4))
  }

  rownames(myaucs_table)=myorder
  colnames(myaucs_table)=c("li","auc","ui")
  
  sen <- cbind (sen, roc_tmp$sensitivities)
  spe <- cbind(spe, roc_tmp$specificities)
  auc_tmp <- myaucs_table[calib,]
  myauc <- rbind(myauc, auc_tmp)
  
  # Misclassification rates
  mr_controls=mr_cases=NULL
  thr_list=seq(0.1,0.9,by=0.1)
  for (thr in thr_list){
    predicted_cat=ifelse(mymodel$fitted.values[y_test==0]>=thr, yes=1, no=0)
    mr_controls=c(mr_controls, sum(predicted_cat==1)/length(predicted_cat))
    predicted_cat=ifelse(mymodel$fitted.values[y_test==1]>=thr, yes=1, no=0)
    mr_cases=c(mr_cases, sum(predicted_cat==0)/length(predicted_cat))
  }
  
  mr_con <- cbind(mr_con, mr_controls)
  mr_cas <- cbind(mr_cas, mr_cases)
}  

saveRDS(sen, paste0("../results/strat_sex_lasso_1000/sensitivities_",arr,".rds"))
saveRDS(spe, paste0("../results/strat_sex_lasso_1000/specificities_",arr,".rds"))
saveRDS(myauc, paste0("../results/strat_sex_lasso_1000/auc_",arr,".rds"))

saveRDS(mr_con, paste0("../results/strat_sex_lasso_1000/mr_controls_",arr,".rds"))
saveRDS(mr_cas, paste0("../results/strat_sex_lasso_1000/mr_cases_",arr,".rds"))



