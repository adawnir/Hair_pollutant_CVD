
### Sumemr Project -- Stability selection sPLS splitting train/test dataset 
## Created by Vivian on 21 July

rm(list=ls())
project_path="~/hair/scripts"
setwd(project_path)

library(caret)

for(m in 1:20){
  arr <- paste0(rep(c("pest_de", "pahs_de", "pest_comb_de","pahs_comb_de"), each = 5), "_", 
                c("obesity", "hypertension", "diabetes", "LD", "MS"))[m]
  dat=readRDS(paste0("../cat_results/denoised/",arr,".rds"))
  y=dat[, ncol(dat)]
  x=dat[,-ncol(dat)]
  set.seed(100)
  train=createDataPartition(y,p=0.8,list=F)
  saveRDS(train, paste0("../cat_results/denoised/",arr,"_split.rds"))
}



