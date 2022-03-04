
### Sumemr Project --  Stratified analysis by Belgium: Stability selection LASSO Logistic Regression ARRAY JOB (RUN ON HPC)
## Created by Vivian on 24 July

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
source("penalisation_functions_1.R")

##### Parameters --
args=commandArgs(trailingOnly=TRUE)
m=as.numeric(args[1])


##### Load data set --
arr <- c(paste0(rep(c("pest_de"), each = 10), "_", 
                c("obesity", "hypertension", "diabetes", "Lipid_disorder", "MS"), 
                rep(c(".low",".up"),each=5)))[m]

dat=readRDS(paste0("../results/strat_belgium_denoised/",arr,"_denoised.rds")) 


##### Make data set --
y=dat[, ncol(dat)]
x=as.matrix(dat[,-c(1, ncol(dat))])
set.seed(100)
train=createDataPartition(y,p=0.7,list=F)
y_train=y[train]
x_train=x[train,]
y_test=y[-train]
x_test=x[-train, ]


##### Create directories
ifelse(dir.exists("../figures/strat_belgium_lasso"),"",dir.create("../figures/strat_belgium_lasso")) 
ifelse(dir.exists("../results/strat_belgium_lasso"),"",dir.create("../results/strat_belgium_lasso")) 


##### Run stability selection --
out=CalibrateRegression(xdata=x_train, ydata=y_train, K=100, tau=0.7, verbose=FALSE,
                        family="binomial")
### Save plot
pdf(paste0("../figures/strat_belgium_lasso/out_",arr,".pdf"), height=5, width=12) 
CalibrationPlot(out)
dev.off()

### Extracting ID of calibrated lambda
hat_lambda_id=GetArgmaxId(out)[1]

### Computing average beta coefficients from models with calibrated lambda
average_beta=apply(out$Beta[hat_lambda_id,,],1,FUN=function(x){mean(x[x!=0])})

### Calibrated selection proportions 
selprop=out$selprop[hat_lambda_id,]

### Save
saveRDS(out, paste0("../results/strat_belgium_lasso/out_",arr,".rds")) 
saveRDS(average_beta, paste0("../results/strat_belgium_lasso/average_beta_",arr,".rds")) 
saveRDS(selprop, paste0("../results/strat_belgium_lasso/selprop_",arr,".rds")) 

# Computing AUCs on the test set from models where predictors are added in order of decreasing selection proportion
selprop_nonzero=selprop[selprop>0]
myorder=names(selprop_nonzero)[sort.list(selprop_nonzero, decreasing = TRUE)]
myaucs=NULL
myaucs_table=NULL

calib=sum(CalibratedStableRegression(out)) # Number of selected variables

t0=Sys.time()
for (k in 1:length(myorder)){
  # Recalibration of the beta coefficients on the training set
  mymodel_recalib=glm(y_train~x_train[,myorder[1:k],drop=FALSE], family="binomial")
  # Prediction using a logistic model with recalibrated beta coefficients
  mymodel=glm(y_test~offset(x_test[,myorder[1:k],drop=FALSE]%*%matrix(coef(mymodel_recalib)[-1], ncol=1)), family="binomial")
  myroc=roc(response=y_test, predictor=mymodel$fitted.values)
  if(k==calib){
    saveRDS(myroc, paste0("../results/strat_belgium_lasso/roc_",arr,".rds")) 
  }
  myaucs=c(myaucs, myroc$auc)
  myaucs_table=rbind(myaucs_table, formatC(as.numeric(ci.auc(myroc)), format="f", digits=4))
}
t1=Sys.time()
print(t1-t0)
rownames(myaucs_table)=myorder
colnames(myaucs_table)=c("li","auc","ui")
saveRDS(myaucs_table, paste0("../results/strat_belgium_lasso/auc_",arr,".rds")) 



