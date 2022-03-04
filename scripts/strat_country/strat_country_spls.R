
### Sumemr Project --  Stratified analysis by country: Stability selection PLS ARRAY JOB (RUN ON HPC)
## Created by Vivian on 13 July

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
 

##### Make dataset --
y=dat[, ncol(dat)]
x=dat[,-c(1, ncol(dat))]
train=readRDS(paste0("../results/strat_country_denoised/",arr,"_split.rds"))
y_train=y[train]
x_train=x[train,]
y_test=y[-train]
x_test=x[-train, ]


##### Create directories --
ifelse(dir.exists("../figures/strat_country_spls"),"",dir.create("../figures/strat_country_spls")) 
ifelse(dir.exists("../results/strat_country_spls"),"",dir.create("../results/strat_country_spls")) 


##### Running stability selection --
out <- CalibrateRegression(xdata = x_train, ydata = y_train, Lambda=1:ncol(x), family="binomial",
                           implementation="SparsePLS", K=100)


### Save plots
pdf(paste0("../figures/strat_country_spls/out_",arr,".pdf"), height=5, width=12)
CalibrationPlot(out)
dev.off()

### stability score as a function of lambda (showing stability score for the best threshold in selection proportion for each lambda)
pdf(paste0("../figures/strat_country_spls/out_unconstr_2d_",arr,".pdf"), height=5, width=12) ### Change path for other stratification
CalibrationPlot(out,bi_dim=FALSE)
dev.off()

### Visualising selection proportion paths
stab_mat=out$selprop[,sort.list(apply(out$selprop,2,sum), decreasing=TRUE)]
rownames(stab_mat)=1:nrow(stab_mat)

pdf(paste0("../figures/strat_country_spls/stab_mat_",arr,".pdf"), height=8, width=12) ### Change path for other stratification
pheatmap(stab_mat, cluster_rows=FALSE, cluster_cols=FALSE, border=NA)
dev.off()

### Extracting ID of calibrated lambda
hat_lambda_id=GetArgmaxId(out)[1]

selprop=out$selprop[hat_lambda_id,]

### Computing average beta coefficients from models with calibrated lambda
average_beta=apply(out$Beta[hat_lambda_id,,],1,FUN=function(x){mean(x[x!=0])})

### Save data sets
saveRDS(out, paste0("../results/strat_country_spls/out_",arr,".rds")) 
saveRDS(selprop, paste0("../results/strat_country_spls/selprop_",arr,".rds")) 
saveRDS(average_beta, paste0("../results/strat_country_spls/beta_",arr,".rds")) 


### Prediction performance (with recalibration)
selected=CalibratedStableRegression(out)
selected=names(selected[selected==1])
mypls=plsda(X=x_train[,selected], Y=y_train, ncomp=1)
predictions=predict(mypls, newdata=x_test[,selected])
y_pred=predictions$predict[,2,1]
myroc=roc(response=y_test, predictor=y_pred)
saveRDS(myroc, paste0("../results/strat_country_spls/roc_",arr,".rds"))


