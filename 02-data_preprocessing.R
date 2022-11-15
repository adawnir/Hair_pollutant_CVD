### Pre-processing: Overview of missing valyes and non-detects, Recoding non-detects, Splitting data, imputing missing values

rm(list=ls())
project_path=dirname(rstudioapi::getActiveDocumentContext()$path)
setwd(project_path)

source("functions.R")
# library(imputeLCMD)
library(missForest)

filepaths = c("PesticidesPCBBDEs","PAHs")
m = 1

expo = readRDS(paste0("../Processed/",filepaths[m],"_expo_ndimp.rds"))
covar = readRDS(paste0("../Data/",filepaths[m],"_covar.rds"))
response = readRDS(paste0("../Data/",filepaths[m],"_response.rds"))

## Imputing missing values
# Exposures
set.seed(202204)
print("Imputing exposures")
imp = missForest(expo)
print(imp$OOBerror)
expo_imp = imp$ximp

# Covariate information
set.seed(202204)
print("Imputing covariates")
imp = missForest(covar)
print(imp$OOBerror)
covar_imp = imp$ximp[,1:ncol(covar)]

all(colnames(expo_imp)==colnames(expo))
all(colnames(covar_imp)==colnames(covar))
all(rownames(covar_imp)==rownames(expo_imp))

saveRDS(expo_imp, paste0("../Processed/",filepaths[m],"_expo_ndimp_naimp.rds"))
saveRDS(covar_imp, paste0("../Processed/",filepaths[m],"_covar_naimp.rds"))

## Log10 transformation
expo_log = log10(expo_imp)

saveRDS(expo_log, paste0("../Processed/",filepaths[m],"_expo_ndimp_naimp_log.rds"))



