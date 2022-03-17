
### Sumemr Project -- Split data 1000 times for stability selection PLS
## Created by Vivian on 04 August

rm(list=ls())
project_path="~/hair/scripts"
setwd(project_path)

library(caret)

##### Parameters
args=commandArgs(trailingOnly=TRUE)
m=as.numeric(args[1])


for (k in 1:1000) {
  arr <- c(paste0(rep(c("pest_de"), each = 15), "_", 
                c("obesity", "hypertension", "diabetes", "Lipid_disorder", "MS"), 
                rep(c(".f",".b",".l"),each=5)),
           paste0(rep(c("pahs_de"), each = 10), "_", 
                  c("obesity", "hypertension", "diabetes", "Lipid_disorder", "MS"), 
                  rep(c(".b",".l"),each=5)))[m]
  dat=readRDS(paste0("../results/strat_country_denoised/",arr,"_denoised.rds"))
  y=dat[, ncol(dat)]
  x=dat[,-c(1, ncol(dat))]
  set.seed(k)
  train=createDataPartition(y,p=0.7,list=F)
  saveRDS(train, paste0("../results/strat_country_denoised/split_1000/",arr,"_split_", k, ".rds"))
}

