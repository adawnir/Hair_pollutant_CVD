### Data processing: Composite score and reconstructing data sets

rm(list=ls())
project_path=dirname(rstudioapi::getActiveDocumentContext()$path)
setwd(project_path)
setwd("../")

# Load packages
library(tidyverse)
library(RColorBrewer)
library(FactoMineR)
source("functions.R")
source("graph_colours.R")

## Load data ----
filepaths = c("PesticidesPCBBDEs_train","PAHs_train","Overlap_test")
for (i in 1:3){
  data = readRDS(paste0("../Processed/",filepaths[i],"_XY.rds"))
  tmp = data[,9:13 ]%>% mutate_all(as.integer)-1
  OutcomeScore = rowSums(tmp)
  data = cbind(data[,1:13],OutcomeScore,data[,14:ncol(data)])
  
  saveRDS(data, paste0("../Processed/",filepaths[i],"_XY_Multimorbidity.rds"))
}

