
### Sumemr Project -- Stability selection sPLS splitting train/test dataset 
## Created by Vivian on 07 July

rm(list=ls())
project_path="~/hair/scripts"
setwd(project_path)

##### Load data set --
for (m in 1:10){
  arr <- paste0(rep(c("pest_de", "pahs_de"), each = 5), "_", 
             c("obesity", "hypertension", "diabetes", "Lipid_disorder", "MS"))[m]
  dat <- readRDS(paste0("../results/Denoised/",arr,".rds")) # Load output
  assign(arr, dat) # Assign name
  
}


##### Split train and test --
### function
foo <- function(dat){
  y <- dat[, ncol(dat)]
  x <- dat[,-c(1, ncol(dat))]
  set.seed(100)
  ids=caret::createDataPartition(y,p=0.8,list=F) # Directly call caret do not load package
  y_train=y[ids]
  x_train=x[ids,]
  train <- data.frame(cbind(y_train, x_train))
  y_test=y[-ids]
  x_test=x[-ids, ]
  test <- data.frame(cbind(y_test, x_test))
  df <- list(training = train, 
             testing = test)
}


### run
pest_de_diabetes_TT <- foo(pest_de_diabetes)
pest_de_obesity_TT <- foo(pest_de_obesity)
pest_de_hypertension_TT <- foo(pest_de_hypertension)
pest_de_MS_TT <- foo(pest_de_MS)
pest_de_Lipid_disorder_TT <- foo(pest_de_Lipid_disorder)

pahs_de_diabetes_TT <- foo(pahs_de_diabetes)
pahs_de_obesity_TT <- foo(pahs_de_obesity)
pahs_de_hypertension_TT <- foo(pahs_de_hypertension)
pahs_de_MS_TT <- foo(pahs_de_MS)
pahs_de_Lipid_disorder_TT <- foo(pahs_de_Lipid_disorder)

### save 
saveRDS(pest_de_diabetes_TT, "../results/Denoised/split/pest_de_diabetes_TT.rds")
saveRDS(pest_de_obesity_TT, "../results/Denoised/split/pest_de_obesity_TT.rds")
saveRDS(pest_de_hypertension_TT, "../results/Denoised/split/pest_de_hypertension_TT.rds")
saveRDS(pest_de_MS_TT, "../results/Denoised/split/pest_de_MS_TT.rds")
saveRDS(pest_de_Lipid_disorder_TT, "../results/Denoised/split/pest_de_Lipid_disorder_TT.rds")

saveRDS(pahs_de_diabetes_TT, "../results/Denoised/split/pahs_de_diabetes_TT.rds")
saveRDS(pahs_de_obesity_TT, "../results/Denoised/split/pahs_de_obesity_TT.rds")
saveRDS(pahs_de_hypertension_TT, "../results/Denoised/split/pahs_de_hypertension_TT.rds")
saveRDS(pahs_de_MS_TT, "../results/Denoised/split/pahs_de_MS_TT.rds")
saveRDS(pahs_de_Lipid_disorder_TT, "../results/Denoised/split/pahs_de_Lipid_disorder_TT.rds")

