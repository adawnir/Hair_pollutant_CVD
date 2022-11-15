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

filepaths = c("PesticidesPCBBDEs","PAHs")
m = 1

annot = readRDS("../Data/Compound_family_annotation.rds")

# Load data
expo = readRDS(paste0("../Processed/",filepaths[m],"_expo_ndimp_naimp_log.rds"))
covar = readRDS(paste0("../Processed/",filepaths[m],"_covar_naimp.rds"))
covar = covar %>% select(Energy, Proteins, Fats, Carbohydrates, Fibers)
response  = readRDS(paste0("../Data/",filepaths[m],"_response.rds"))
response = response[,1:9]

# Running stability selection
for (p in c(seq(10,50,10))){
  print(paste("Exposure network: PFER",p))
  t0=Sys.time()
  out=GraphicalModel(xdata=cbind(covar,expo), pk=c(ncol(covar),ncol(expo)), PFER_thr=p)
  t1=Sys.time()
  print(t1-t0)
  ifelse(dir.exists("../Results/"),"",dir.create("../Results/"))
  ifelse(dir.exists("../Results/Graphical_network/"),"",dir.create("../Results/Graphical_network/"))
  saveRDS(out, paste0("../Results/Graphical_network/",filepaths[m],"_exposures_network_output_PFER",p,".rds"))
}

for (p in c(seq(10,50,10))){
  print(paste("Outcome network: PFER",p))
  t0=Sys.time()
  out=GraphicalModel(xdata=response, PFER_thr=p)
  t1=Sys.time()
  print(t1-t0)
  ifelse(dir.exists("../Results/"),"",dir.create("../Results/"))
  ifelse(dir.exists("../Results/Graphical_network/"),"",dir.create("../Results/Graphical_network/"))
  saveRDS(out, paste0("../Results/Graphical_network/",filepaths[m],"_outcome_network_output_PFER",p,".rds"))
}
