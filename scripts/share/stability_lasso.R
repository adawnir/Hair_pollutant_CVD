

### Stability selection LASSO script
## Created by Chia-An

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

##### Parameters
args=commandArgs(trailingOnly=TRUE)
m=as.numeric(args[1])


##### Load data set --
arr <- paste0(rep(c("pest_de", "pahs_de"), each = 5), "_", 
              c("obesity", "hypertension", "diabetes", "Lipid_disorder", "MS"))[m]
dat=readRDS(paste0("../results/denoised/",arr,".rds"))


##### Run stability selection on full sample --
y <- dat[, ncol(dat)]
x <- as.matrix(dat[,-c(1,ncol(dat))])


out=CalibrateRegression(xdata=x, ydata=y, K=100, tau=0.5, 
                        verbose=FALSE, family="binomial")

# Save plot
CalibrationPlot(out)


# Extracting ID of calibrated lambda
hat_lambda_id=GetArgmaxId(out)[1]

# Computing average beta coefficients from models with calibrated lambda
average_beta=apply(out$Beta[hat_lambda_id,,],1,FUN=function(x){mean(x[x!=0])})


# Calibrated selection proportions 
selprop=out$selprop[hat_lambda_id,]




