## Inspection of stability-based LASSO logistic regression

rm(list=ls())
project_path=dirname(rstudioapi::getActiveDocumentContext()$path)
setwd(project_path)

library(tidyverse)
library(sharp)
library(ROCR)

## Extract calibrated parameters----
ifelse(dir.exists("../Figures/Multivariate/"),"",dir.create("../Figures/Multivariate/"))

myfiles = list.files(path = "../Results/Multivariate/", pattern = "*_f[0-3]_output_*")
param = NULL
for (i in 1:length(myfiles)){
  # Load data
  out = readRDS(paste0("../Results/Multivariate/",myfiles[i]))
  tmp = Argmax(out)
  pdf(paste0("../Figures/Multivariate/",myfiles[i],".pdf"))
  par(mar = c(7,5,7,6))
  CalibrationPlot(out)
  dev.off()
  param = rbind(param, tmp)
}
rownames(param) = gsub(".rds","", myfiles) %>% gsub("_output_","_", .)
saveRDS(param, paste0("../Results/Multivariate/Model_parameter_summary.rds"))

## Inspect empty models and direction of betas of stably selected variables ----
myfiles = list.files(path = "../Results/Multivariate/", pattern = "*_f[0-3]_output_*")
for (i in 1:length(myfiles)){
  # Load data
  out = readRDS(paste0("../Results/Multivariate/",myfiles[i]))
  if(sum(SelectedVariables(out))<1){
    print(paste0(myfiles[i]," is an empty model"))
  } else{
    selprop = SelectionProportions(out)
    a=apply(out$Beta[ArgmaxId(out)[1],names(selprop),],1,FUN=function(x){sum(x>0)})
    b=apply(out$Beta[ArgmaxId(out)[1],names(selprop),],1,FUN=function(x){sum(x<0)})
    
    newpath = gsub("output","inspect",myfiles[i]) %>% gsub(".rds",".pdf",.)
    pdf(paste0("../Figures/Multivariate/",newpath))
    par(mar=c(5,5,1,1))
    plot(a/(a+b), selprop, las=1, pch=19, col="navy", cex.lab=1.5,
         xlab="Proportion of positive loadings among non-zero loadings", 
         ylab="Selection Proportion") # Ideally no point around 0.5 with high selection proportion
    dev.off()
  }
}

## Plot selection proportion----
ifelse(dir.exists("../Figures/Multivariate/"),"",dir.create("../Figures/Multivariate/"))

myfiles = list.files(path = "../Results/Multivariate/", pattern = "*_f[0-3]_output_*")
param = NULL
for (i in 1:length(myfiles)){
  # Load data
  out = readRDS(paste0("../Results/Multivariate/",myfiles[i]))
  if(sum(SelectedVariables(out))<1){
    print(paste0(myfiles[i]," is an empty model"))
  } else{
    pdf(paste0("../Figures/Multivariate/",gsub("output","selprop",myfiles[i]),".pdf"), width = 9)
    par(mar = c(15,5,1,1))
    plot(out)
    dev.off()
  }
}

