
### Sumemr Project --  Stratified analysis by sex (same obs.): Stability selection PLS ARRAY JOB (RUN ON HPC)
## Created by Vivian on 19 July

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
arr <- paste0(rep(c("pest_de", "pahs_de"), each = 5), "_", 
              c("obesity", "hypertension", "diabetes", "Lipid_disorder", "MS"), rep(c(".f",".m"),each=10))[m]
dat=readRDS(paste0("../results/strat_sex_denoised_same/",arr,"_denoised.rds")) 
 

##### Make dataset --
y=dat[, ncol(dat)]
x=dat[,-c(1, ncol(dat))]
train=readRDS(paste0("../results/strat_sex_denoised_same/",arr,"_split.rds"))
y_train=y[train]
x_train=x[train,]
y_test=y[-train]
x_test=x[-train, ]


##### Create directories --
ifelse(dir.exists("../figures/strat_sex_spls_same"),"",dir.create("../figures/strat_sex_spls_same")) 
ifelse(dir.exists("../results/strat_sex_spls_same"),"",dir.create("../results/strat_sex_spls_same")) 


##### Running stability selection --
out <- CalibrateRegression(xdata = x_train, ydata = y_train, Lambda=1:ncol(x), family="binomial",
                           implementation="SparsePLS", K=100)


### Save plots
pdf(paste0("../figures/strat_sex_spls_same/out_",arr,".pdf"), height=5, width=12)
CalibrationPlot(out)
dev.off()

### stability score as a function of lambda (showing stability score for the best threshold in selection proportion for each lambda)
pdf(paste0("../figures/strat_sex_spls_same/out_unconstr_2d_",arr,".pdf"), height=5, width=12) ### Change path for other stratification
CalibrationPlot(out,bi_dim=FALSE)
dev.off()

### Visualising selection proportion paths
stab_mat=out$selprop[,sort.list(apply(out$selprop,2,sum), decreasing=TRUE)]
rownames(stab_mat)=1:nrow(stab_mat)

pdf(paste0("../figures/strat_sex_spls_same/stab_mat_",arr,".pdf"), height=8, width=12) ### Change path for other stratification
pheatmap(stab_mat, cluster_rows=FALSE, cluster_cols=FALSE, border=NA)
dev.off()

### Extracting ID of calibrated lambda
hat_lambda_id=GetArgmaxId(out)[1]

selprop=out$selprop[hat_lambda_id,]

### Computing average beta coefficients from models with calibrated lambda
average_beta=apply(out$Beta[hat_lambda_id,,],1,FUN=function(x){mean(x[x!=0])})

### Save data sets
saveRDS(out, paste0("../results/strat_sex_spls_same/out_",arr,".rds")) 
saveRDS(selprop, paste0("../results/strat_sex_spls_same/selprop_",arr,".rds")) 
saveRDS(average_beta, paste0("../results/strat_sex_spls_same/beta_",arr,".rds")) 


### Prediction performance (with recalibration)
selected=CalibratedStableRegression(out)
selected=names(selected[selected==1])
mypls=plsda(X=x_train[,selected], Y=y_train, ncomp=1)
predictions=predict(mypls, newdata=x_test[,selected])
y_pred=predictions$predict[,2,1]
myroc=roc(response=y_test, predictor=y_pred)
saveRDS(myroc, paste0("../results/strat_sex_spls_same/roc_",arr,".rds"))


