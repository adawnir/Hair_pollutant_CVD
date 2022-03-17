
### Sumemr Project -- Split data for stability selection PLS (same obs.)
## Created by Vivian on 19 July

rm(list=ls())
project_path="~/hair/scripts"
setwd(project_path)

library(caret)

for(m in 1:20){
  arr <- paste0(rep(c("pest_de", "pahs_de"), each = 5), "_", 
                c("obesity", "hypertension", "diabetes", "Lipid_disorder", "MS"), rep(c(".f",".m"),each=10))[m]
  dat=readRDS(paste0("../results/strat_sex_denoised_same/",arr,"_denoised.rds"))
  y=dat[, ncol(dat)]
  x=dat[,-c(1, ncol(dat))]
  set.seed(100)
  train=createDataPartition(y,p=0.8,list=F)
  saveRDS(train, paste0("../results/strat_sex_denoised_same/",arr,"_split.rds"))
}

