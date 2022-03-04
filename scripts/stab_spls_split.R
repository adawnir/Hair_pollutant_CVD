
### Sumemr Project -- Split data for stability selection PLS 
## Created by Vivian on 02 August

rm(list=ls())
project_path="~/hair/scripts"
setwd(project_path)

library(caret)


ifelse(dir.exists("../results/denoised/split_1000"),"",dir.create("../results/denoised/split_1000"))


for(m in 1:10){
  for (k in 1:1000) {
    arr <- paste0(rep(c("pest_de", "pahs_de"), each = 5), "_", 
                  c("obesity", "hypertension", "diabetes", "Lipid_disorder", "MS"))[m]
    dat=readRDS(paste0("../results/denoised/",arr,".rds"))
    y=dat[, ncol(dat)]
    x=dat[,-c(1, ncol(dat))]
    set.seed(k)
    train=createDataPartition(y,p=0.7,list=F)
    saveRDS(train, paste0("../results/denoised/split_1000/",arr,"_split_", k, ".rds"))
  }
}

