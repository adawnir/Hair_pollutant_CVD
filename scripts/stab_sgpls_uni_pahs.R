
### Sumemr Project -- Stability selection sgPLS with univariate outcome 1000 iteration (HPC)
## Created by Vivian on 06 August

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
arr <- paste0(rep(c("pahs_de"), each = 5), "_", 
              c("obesity", "hypertension", "diabetes", "Lipid_disorder", "MS"))[m]
dat=readRDS(paste0("../results/denoised/",arr,".rds"))


##### Create directories --
ifelse(dir.exists("../figures/sgpls_uni"),"",dir.create("../figures/sgpls_uni"))
ifelse(dir.exists("../results/sgpls_uni"),"",dir.create("../results/sgpls_uni"))


##### Randomly generated groups
group_ind=c(1,3)


##### Rub stability selection --
y=dat[, ncol(dat)]
x=as.matrix(dat[,-c(1, ncol(dat))])


alpha_list=seq(0.05,0.95,by=0.05)
stability_alpha=NULL
for (k in 1:length(alpha_list)){
  myalpha=alpha_list[k]
  tmp=CalibrateRegression(xdata=x, ydata=y, Lambda=1:(length(group_ind)+1), K=20,
                          family="binomial", implementation="SparseGroupPLS", ind.block.x=group_ind, alpha.x=myalpha)
  assign(paste0("out_", k), tmp)
  stability_alpha=cbind(stability_alpha, tmp$S)
}
rownames(stability_alpha)=tmp$Lambda

colnames(stability_alpha)=alpha_list

pdf(paste0("../figures/sgpls_uni/stab_alpha_",arr,".pdf"), height=5, width=12)
pheatmap(stability_alpha, cluster_rows=FALSE, cluster_cols=FALSE, border=NA)
dev.off()

# Calibrating the parameter alpha
hat_alpha_id=which.max(apply(stability_alpha,2,max))
hat_alpha=alpha_list[hat_alpha_id]

# Calibrating the number of groups and threshold in selection proportion (pi)
out=eval(parse(text=paste0("out_", hat_alpha_id)))

pdf(paste0("../figures/sgpls_uni/out_",arr,".pdf"), height=5, width=12)
CalibrationPlot(out)
dev.off()


# Selection proportions for best alpha over different numbers of groups
mygroups=NULL
group_size=c(group_ind[1],diff(c(group_ind,ncol(x))))
for (i in 1:(length(group_size))){
  mygroups=c(mygroups,rep(i, group_size[i]))
}
names(mygroups)=colnames(x)
new=SeparateMatrix(out$selprop, groups=mygroups)
rownames(new)=1:nrow(new)

pdf(paste0("../figures/sgpls_uni/selprop_heatmap_",arr,".pdf"), height=5, width=12)
pheatmap(new, cluster_rows=FALSE, cluster_cols=FALSE, border=NA, na_col="white")
dev.off()

# Extracting average loadings from sgPLS-DA calibrated by stability
myselprop=out$selprop[which.max(out$S),]
myloadings=out$Beta[which.max(out$S),,]
myloadings=apply(myloadings,1,FUN=function(x){mean(x[x!=0])})
myloadings[is.na(myloadings)]=0

myloadings=myloadings[colnames(new)]
names(myloadings)[is.na(names(myloadings))]=""
myselprop=myselprop[names(myloadings)]


### save
saveRDS(out, paste0("../results/sgpls_uni/out_",arr, ".rds"))
saveRDS(myloadings, paste0("../results/sgpls_uni/average_load_",arr, ".rds"))
saveRDS(myselprop, paste0("../results/sgpls_uni/selprop_",arr,".rds"))


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

for (k in 1:1) {

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

saveRDS(sen, paste0("../results/sgpls_uni/sensitivities_",arr,".rds"))
saveRDS(spe, paste0("../results/sgpls_uni/specificities_",arr,".rds"))
saveRDS(myauc, paste0("../results/sgpls_uni/auc_",arr,".rds"))

saveRDS(mr_con, paste0("../results/sgpls_uni/mr_controls_",arr,".rds"))
saveRDS(mr_cas, paste0("../results/sgpls_uni/mr_cases_",arr,".rds"))

