## Graphical Lasso Network

rm(list=ls())
project_path="/rds/general/user/rw1317/home/Hair_pollutant_CVD/Scripts"
setwd(project_path)

# options(warn=0)

# Load packages
library(tidyverse)
library(igraph)
library(colorspace)
library(sharp)

# Load custom
source("functions.R")
source("graph_colours.R")

## Parameters
args=commandArgs(trailingOnly=TRUE)
m=as.numeric(args[1])

# Load data sets
annot = readRDS("../Data/Compound_family_annotation.rds")

filepaths = c("PesticidesPCBBDEs_train","PAHs_train","Overlap_test")
# Load data
data = readRDS(paste0("../Processed/",filepaths[m],"_info_expo_ndimp_naimp_log.rds"))

# Standardisaiton
expo = data[,c(10:14,34:ncol(data))]
expo = scale(expo)

# Running stability selection
for (p in c(seq(10,50,10))){
  print(paste("PFER",p))
  t0=Sys.time()
  out=GraphicalModel(xdata=expo, pk=c(5,ncol(expo)-5), PFER_thr=p)
  t1=Sys.time()
  print(t1-t0)
  ifelse(dir.exists("../Results/"),"",dir.create("../Results/"))
  ifelse(dir.exists("../Results/Graphical_network/"),"",dir.create("../Results/Graphical_network/"))
  saveRDS(out, paste0("../Results/Graphical_network/",filepaths[m],"_exposures_network_output_PFER",p,".rds"))
}
