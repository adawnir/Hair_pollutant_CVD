# R2GUESS

rm(list=ls())
project_path="/rds/general/user/rw1317/home/Hair_pollutant_CVD/Scripts"
setwd(project_path)


library(R2GUESS)
library(tidyverse)

## Parameters
filepaths = c("PesticidesPCBBDEs_train","PAHs_train")
paths = c("Option1","Option2","Option3")

args=commandArgs(trailingOnly=TRUE)
array=as.numeric(args[1])

mat = matrix(rep(0:7,length(paths)), ncol=length(paths))
f = mat[array]
i = arrayInd(array, dim(mat))[2]

m = 1

model = readRDS(paste0("../Results/R2GUESS/",paths[i],"/Model",f,".rds"))

set.seed(3)
n.sweep = get.sweep.best.model(model, models = 1)
res.g = get.g.sweep(model, n.sweep$result, 1)
beta = sample.beta(model, res.g, Nmonte = 2, Nmonte.sigma = 2)

saveRDS(res.g, paste0("../Results/R2GUESS/",paths[i],"/Model",f,"_res.rds"))
saveRDS(beta, paste0("../Results/R2GUESS/",paths[i],"/Model",f,"_beta.rds"))
