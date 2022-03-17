
### Sumemr Project -- Stability selection sPLS with univariate outcome 1000 iteration (HPC)
## Created by Vivian on 02 August

rm(list=ls())
project_path="~/hair/scripts"
setwd(project_path)

##### Load packages --
library(pheatmap)
library(parallel)
library(mixOmics)
library(tidyverse)
library(sgPLS)
library(pROC)


source("penalisation_functions.R")
source("pls_functions.R")


##### Parameters
args=commandArgs(trailingOnly=TRUE)
m=as.numeric(args[1])


##### Load data set --
arr <- paste0(rep(c("pest_de", "pahs_de"), each = 5), "_", 
              c("obesity", "hypertension", "diabetes", "Lipid_disorder", "MS"))[m]
dat=readRDS(paste0("../results/denoised/",arr,".rds"))


##### Create directories --
ifelse(dir.exists("../figures/spls_1000_uni"),"",dir.create("../figures/spls_1000_uni"))
ifelse(dir.exists("../results/spls_1000_uni"),"",dir.create("../results/spls_1000_uni"))


##### Rub stability selection --
y=dat[, ncol(dat)]
x=as.matrix(dat[,-c(1, ncol(dat))])

out <- CalibrateRegression(xdata = x, ydata = y, Lambda=1:ncol(x), family="binomial", 
                           implementation="SparsePLS", K=100)

### save plot
pdf(paste0("../figures/spls_1000_uni/out_",arr,".pdf"), height=5, width=12)
CalibrationPlot(out)
dev.off()

### Extracting ID of calibrated lambda
hat_lambda_id <- GetArgmaxId(out)[1]

### Calibrated selection proportions 
selprop <- out$selprop[hat_lambda_id,]

### Computing average beta coefficients from models with calibrated lambda
average_load <- apply(out$Beta[hat_lambda_id,,],1,FUN=function(x){mean(x[x!=0])})

### Visualising selection proportion paths
stab_mat=out$selprop[,sort.list(apply(out$selprop,2,sum), decreasing=TRUE)]
rownames(stab_mat)=1:nrow(stab_mat)

pdf(paste0("../figures/spls_1000_uni/stab_mat_",arr,".pdf"), height=8, width=12) ### Change path for other stratification
pheatmap(stab_mat, cluster_rows=FALSE, cluster_cols=FALSE, border=NA)
dev.off()

### save
saveRDS(out, paste0("../results/spls_1000_uni/out_",arr, ".rds"))
saveRDS(average_load, paste0("../results/spls_1000_uni/average_load_",arr, ".rds"))
saveRDS(selprop, paste0("../results/spls_1000_uni/selprop_",arr,".rds"))



##### Make data --
selected <- CalibratedStableRegression(out)
selected <- names(selected[selected == 1])

#selectprop <- NULL
#ave_load <- NULL
sen <- NULL
spe <- NULL
mr_con <- NULL
mr_cas <- NULL
myauc <- NULL

for (k in 1:1000) {

  train=readRDS(paste0("../results/denoised/split_1000/", arr, "_split_", k, ".rds"))
  y_train=y[train]
  x_train=x[train,]
  y_test=y[-train]
  x_test=x[-train, ]
  
  if(length(selected) == 1){
    mypls <- plsda(X=as.matrix(x_train[,selected]), Y=y_train, ncomp=1)
    colnames(mypls$X) <- selected
    x_tmp <- data.frame(x_test[,selected])
    colnames(x_tmp) <- selected
    predictions_re <- predict(mypls, newdata=x_tmp)
    predictions_raw <- predictions_re$predict[,2,1]
    myroc_re <- roc(response=y_test, predictor=predictions_raw)
    
    sen <- cbind(sen, myroc_re$sensitivities)
    spe <- cbind(spe, myroc_re$specificities)
    myauc <- rbind(myauc, round(as.numeric(ci.auc(myroc_re)),2))
  } else if(length(selected) > 1){
    mypls <- plsda(X=x_train[,selected], Y=y_train, ncomp=1)
    predictions_re <- predict(mypls, newdata=x_test[,selected])
    predictions_raw <- predictions_re$predict[,2,1]
    myroc_re <- roc(response=y_test, predictor=predictions_raw)
    
    sen <- cbind(sen, myroc_re$sensitivities)
    spe <- cbind(spe, myroc_re$specificities)
    myauc <- rbind(myauc, round(as.numeric(ci.auc(myroc_re)),2))
  } else {
    myroc_re$sensitivities <- rep(NA, nrow(sen))
    myroc_re$specificities <- rep(NA, nrow(spe))
    
    sen <- cbind(sen, myroc_re$sensitivities)
    spe <- cbind(spe, myroc_re$specificities)
  }
  

  
  # Misclassification rates based on max dist (with recalibration)
  predictions_class <- predictions_re$class$max.dist
  mr_controls <- sum(predictions_class == 1)/length(predictions_class)
  mr_cases <- sum(predictions_class == 0)/length(predictions_class)
  
  mr_con <- cbind(mr_con, mr_controls)
  mr_cas <- cbind(mr_cas, mr_cases)
}



##### Save

saveRDS(sen, paste0("../results/spls_1000_uni/sensitivities_",arr,".rds"))
saveRDS(spe, paste0("../results/spls_1000_uni/specificities_",arr,".rds"))
saveRDS(myauc, paste0("../results/spls_1000_uni/auc_",arr,".rds"))

saveRDS(mr_con, paste0("../results/spls_1000_uni/mr_controls_",arr,".rds"))
saveRDS(mr_cas, paste0("../results/spls_1000_uni/mr_cases_",arr,".rds"))

