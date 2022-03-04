
### Sumemr Project -- Split data for stability selection PLS on overlapping
## Created by Vivian on 28 July

rm(list=ls())
project_path="~/hair/scripts"
setwd(project_path)

library(caret)

for(m in 1:5){
  arr <- paste0(rep("over_de", times = 5), "_", 
                c("obesity", "hypertension", "diabetes", "Lipid_disorder", "MS"))[m]
  dat=readRDS(paste0("../results/Denoised/",arr,".rds"))
  y=dat[, ncol(dat)]
  x=dat[,-c(1, ncol(dat))]
  set.seed(100)
  train=createDataPartition(y,p=0.7,list=F)
  saveRDS(train, paste0("../results/Denoised/",arr,"_split.rds"))
}

