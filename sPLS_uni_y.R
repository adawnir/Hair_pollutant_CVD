
### Sumemr Project -- Stability selection sPLS with univariate outcome (HPC)
## Created by Vivian on 07 July

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
              c("obesity", "hypertension", "diabetes", "Lipid_disorder", "MS"), rep("_TT", 10))[m]
dat=readRDS(paste0("../results/Denoised/split/",arr,".rds"))


##### Split data --
y_train <- dat[[1]][,1]
x_train <- dat[[1]][,-1]
y_test <- dat[[2]][,1]
x_test <- dat[[2]][,-1]


##### Create directories --
ifelse(dir.exists("../figures/spls_uni"),"",dir.create("../figures/spls_uni"))
ifelse(dir.exists("../results/spls_uni"),"",dir.create("../results/spls_uni"))


##### Run sPLS stability models--

t0=Sys.time()
out <- CalibrateRegression(xdata = x_train, ydata = y_train, Lambda=1:ncol(x_train), family="binomial", 
                             implementation="SparsePLS", K=100)
t1=Sys.time()
print(t1-t0)

# Save plots
pdf(paste0("../figures/spls_uni/out_",arr,".pdf"), height=5, width=12)
CalibrationPlot(out)
dev.off()

# Calibrated selection proportions 
selprop <- CalibratedSelectionProportionsRegression(out, plot=FALSE)

# Extracting ID of calibrated lambda
hat_lambda_id <- GetArgmaxId(out)[1]

# Computing average beta coefficients from models with calibrated lambda
average_load <- apply(out$Beta[hat_lambda_id,,],1,FUN=function(x){mean(x[x!=0])})

# Save
saveRDS(out, paste0("../results/spls_uni/out_",arr, ".rds"))
saveRDS(average_load, paste0("../results/spls_uni/average_load_",arr, ".rds"))
saveRDS(selprop, paste0("../results/spls_uni/selprop_",arr,".rds"))


# Extracting average loadings (without recalibration)
myloadings <- out$Beta[which.max(out$S),,]
myloadings <- apply(myloadings,1,FUN=function(x){mean(x[x!=0])})
myloadings[is.na(myloadings)]=0
predictions <- as.vector(as.matrix(x_test)%*%cbind(myloadings))
myroc=roc(response=y_test, predictor=predictions)

saveRDS(myroc, paste0("../results/spls_uni/roc_",arr,".rds"))

# Extracting average loadings (with recalibration)
selected <- CalibratedStableRegression(out)
selected <- names(selected[selected == 1])
mypls <- plsda(X=x_train[,selected], Y=y_train, ncomp=1)
predictions_re <- predict(mypls, newdata=x_test[,selected])
predictions_raw <- predictions_re$predict[,2,1]
myroc_re <- roc(response=y_test, predictor=predictions_raw)

saveRDS(myroc_re, paste0("../results/spls_uni/roc_recali_",arr,".rds"))


# Misclassification rates based on max dist (with recalibration)
predictions_class <- predictions_re$class$max.dist
mr_controls <- sum(predictions_class == 1)/length(predictions_class)
mr_cases <- sum(predictions_class == 0)/length(predictions_class)

saveRDS(mr_controls, paste0("../results/spls_uni/mr_controls_",arr,".rds"))
saveRDS(mr_cases, paste0("../results/spls_uni/mr_cases_",arr,".rds"))




