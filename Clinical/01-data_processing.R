### Data processing: Reconstructing data sets

rm(list=ls())
project_path=dirname(rstudioapi::getActiveDocumentContext()$path)
setwd(project_path)
setwd("../")

# Load packages
library(tidyverse)
source("functions.R")

## Load data ----
filepaths = c("PesticidesPCBBDEs_train","PAHs_train","Overlap_test")
prefix = c("train_pest","train_pahs","test")
for (i in 1:3){
  data = readRDS(paste0("../Processed/",filepaths[i],"_XY.rds"))
  assign(prefix[i], data)
}

data = readRDS("../Data/All_participants_info.rds")

## Reconstruct dataset ----
pest_XY = cbind(train_pest[,1:8], data[rownames(train_pest),c(5,12:19)],train_pest[,14:ncol(train_pest)])
pahs_XY = cbind(train_pahs[,1:8], data[rownames(train_pahs),c(5,12:19)],train_pahs[,14:ncol(train_pahs)])
test_XY = cbind(test[,1:8], data[rownames(test),c(5,12:19)],test[,14:ncol(test)])

saveRDS(pest_XY, "../Processed/PesticidesPCBBDEs_train_XY_Clinical.rds")
saveRDS(pahs_XY, "../Processed/PAHs_train_XY_Clinical.rds")
saveRDS(test_XY, "../Processed/Overlap_test_XY_Clinical.rds")
