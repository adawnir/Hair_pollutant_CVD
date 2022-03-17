
### Sumemr Project --  Stratified analysis by country: Stability selection PLS 1000 iterations (RUN ON HPC)
## Created by Vivian on 04 August

rm(list=ls())
project_path="~/hair/scripts"
setwd(project_path)

## Load packages
LoadPackages=function(packages){
  for (i in 1:length(packages)){
    suppressPackageStartupMessages(library(packages[i], character.only=TRUE))
  }
}

LoadPackages(c("pheatmap","glasso","glmnet","igraph","pROC","sgPLS"))

source("penalisation_functions.R")
source("pls_functions.R")


##### Parameters --
args=commandArgs(trailingOnly=TRUE)
m=as.numeric(args[1])


##### Load data set --
arr <- c(paste0(rep(c("pest_de"), each = 15), "_", 
                c("obesity", "hypertension", "diabetes", "Lipid_disorder", "MS"), 
                rep(c(".f",".b",".l"),each=5)), 
         paste0(rep(c("pahs_de"), each = 9), "_", 
                c("obesity", "hypertension", "Lipid_disorder", "MS", "diabetes"), 
                rep(c(".b",".l"), times = c(4,5))))[m]

dat=readRDS(paste0("../results/strat_country_denoised/",arr,"_denoised.rds")) 
 

##### Create directories --
ifelse(dir.exists("../figures/strat_country_spls_1000"),"",dir.create("../figures/strat_country_spls_1000")) 
ifelse(dir.exists("../results/strat_country_spls_1000"),"",dir.create("../results/strat_country_spls_1000")) 


##### Running stability selection --
y=dat[, ncol(dat)]
x=dat[,-c(1, ncol(dat))]
out <- CalibrateRegression(xdata = x, ydata = y, Lambda=1:ncol(x), family="binomial",
                           implementation="SparsePLS", K=100)

### Save plots
pdf(paste0("../figures/strat_country_spls_1000/out_",arr,".pdf"), height=5, width=12)
CalibrationPlot(out)
dev.off()

### stability score as a function of lambda (showing stability score for the best threshold in selection proportion for each lambda)
pdf(paste0("../figures/strat_country_spls_1000/out_unconstr_2d_",arr,".pdf"), height=5, width=12) ### Change path for other stratification
CalibrationPlot(out,bi_dim=FALSE)
dev.off()

### Visualising selection proportion paths
stab_mat=out$selprop[,sort.list(apply(out$selprop,2,sum), decreasing=TRUE)]
rownames(stab_mat)=1:nrow(stab_mat)

pdf(paste0("../figures/strat_country_spls_1000/stab_mat_",arr,".pdf"), height=8, width=12) ### Change path for other stratification
pheatmap(stab_mat, cluster_rows=FALSE, cluster_cols=FALSE, border=NA)
dev.off()

### Extracting ID of calibrated lambda
hat_lambda_id=GetArgmaxId(out)[1]

selprop=out$selprop[hat_lambda_id,]

### Computing average beta coefficients from models with calibrated lambda
average_beta=apply(out$Beta[hat_lambda_id,,],1,FUN=function(x){mean(x[x!=0])})

### Save data sets
saveRDS(out, paste0("../results/strat_country_spls_1000/out_",arr,".rds")) 
saveRDS(selprop, paste0("../results/strat_country_spls_1000/selprop_",arr,".rds")) 
saveRDS(average_beta, paste0("../results/strat_country_spls_1000/beta_",arr,".rds")) 


##### Prediction performance (with recalibration)
selected=CalibratedStableRegression(out)
selected=names(selected[selected==1])

sen <- NULL
spe <- NULL
mr_con <- NULL
mr_cas <- NULL
myauc <- NULL

for (k in 1:1000) {
  train=readRDS(paste0("../results/strat_country_denoised/split_1000/",arr, "_split_", k, ".rds"))
  y_train=y[train]
  x_train=x[train,]
  y_test=y[-train]
  x_test=x[-train, ]
  
  if(length(selected) == 1){
    tmp <- as.matrix(x_train[,selected], ncol=1)
    colnames(tmp) <- selected
    rownames(tmp) <- train
    mypls <- plsda(X=tmp, Y=y_train, ncomp=1)
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




saveRDS(sen, paste0("../results/strat_country_spls_1000/sensitivities_",arr,".rds"))
saveRDS(spe, paste0("../results/strat_country_spls_1000/specificities_",arr,".rds"))
saveRDS(myauc, paste0("../results/strat_country_spls_1000/auc_",arr,".rds"))

saveRDS(mr_con, paste0("../results/strat_country_spls_1000/mr_controls_",arr,".rds"))
saveRDS(mr_cas, paste0("../results/strat_country_spls_1000/mr_cases_",arr,".rds"))





