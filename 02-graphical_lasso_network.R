## Graphical Lasso Network

rm(list=ls())
project_path="/rds/general/user/rw1317/home/Hair_pollutant_CVD/Scripts"
setwd(project_path)

# options(warn=0)

# Load packages
library(tidyverse)
library(igraph)
library(colorspace)
library(focus)

# Load custom
source("functions.R")
source("graph_param.R")

## Parameters
args=commandArgs(trailingOnly=TRUE)
m=as.numeric(args[1])

# Load data sets
annot = readRDS("../Data/Compound_family_annotation.rds")

filepaths = c("PesticidesPCBBDEs","PAHs","Combined")
for (i in 1:3){
  # Load data
  expo = readRDS(paste0("../Processed/",filepaths[m],"_ndimp_naimp_log.rds"))
  
  # Standardisaiton
  expo = scale(expo)
  
  # Running stability selection
  t0=Sys.time()
  out=GraphicalModel(xdata=expo, PFER_thr=20)
  t1=Sys.time()
  print(t1-t0)
  ifelse(dir.exists("../Results/"),"",dir.create("../Results/"))
  saveRDS(out, paste0("../Results/",filepaths[m],"_graphical_network_exposures_output.rds"))
  
  ifelse(dir.exists("../Figures/"),"",dir.create("../Figures/"))
  pdf(paste0("../Figures/Graphical_network_exposures_output_",filepaths[m],".pdf"))
  par(mar = c(7,5,7,6))
  CalibrationPlot(out)
  dev.off()
  
  # Adjacency matrix of the calibrated network
  A=Adjacency(out)
  saveRDS(A, paste0("../Results/",filepaths[m],"_graphical_network_exposures_adjacency_matrix.rds"))
}

