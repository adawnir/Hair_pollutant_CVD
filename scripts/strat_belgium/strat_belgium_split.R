
### Sumemr Project -- Split data for stability selection PLS
## Created by Vivian on 24 July

rm(list=ls())
project_path="~/hair/scripts"
setwd(project_path)

library(caret)


for(m in 1:10){
  arr <- paste0(rep(c("pest_de"), each = 10), "_", 
                c("obesity", "hypertension", "diabetes", "Lipid_disorder", "MS"), rep(c(".low",".up"),each=5))[m]
  dat=readRDS(paste0("../results/strat_belgium_denoised/",arr,"_denoised.rds"))
  y=dat[, ncol(dat)]
  x=dat[,-c(1, ncol(dat))]
  set.seed(100)
  train=createDataPartition(y,p=0.7,list=F)
  saveRDS(train, paste0("../results/strat_belgium_denoised/",arr,"_split.rds"))
}
