### Data processing: Composite score and reconstructing data sets

rm(list=ls())
project_path=dirname(rstudioapi::getActiveDocumentContext()$path)
setwd(project_path)

# Load packages
library(tidyverse)
library(RColorBrewer)
library(FactoMineR)
source("functions.R")
source("graph_colours.R")

## Load data ----
filepaths = c("PesticidesPCBBDEs_train","PAHs_train","Overlap_test")
prefix = c("train_pest","train_pahs","test")
for (i in 1:3){
  data = readRDS(paste0("../Processed/",filepaths[i],"_info_expo_ndimp_naimp_log.rds"))
  assign(prefix[i], data)
}

## Diet information ----
for (i in 1:3){
  x = eval(parse(text = prefix[i]))[,7:11]
  mypca = PCA(x, graph = FALSE)
  saveRDS(mypca, paste0("../Results/",filepaths[i],"_diet_info_pca.rds"))
  
  scores = mypca$ind$coord
  colnames(scores) = paste0("DietScore",1:ncol(scores))
  assign(paste0(prefix[i],"_scores"), scores)
  
  loadings = mypca$var$coord
  ev=mypca$eig[,2]/100
  
  # Scree plot
  pdf(paste0("../Figures/Diet_info_pca_scree_plot_",filepaths[i],".pdf"), width=5, height=5)
  par(mar=c(5,5,1,1))
  plot(ev, pch=19, col="navy", las=1, type="b", ylim=c(0,1),
       ylab="Proportion of explained variance", xlab="PCs")
  points(cumsum(ev), pch=19, col="red", type="b")
  legend("right", pch=19, col=c("navy", "red"),
         legend=c("Proportion of e.v.", "Cumulative proportion of e.v."))
  dev.off()
}

## Reconstruct dataset ----
pest_XY = cbind(train_pest[,c(1:4,6)], train_pest_scores[,1:3],train_pest[,c(23:27,31:ncol(train_pest))])
pahs_XY = cbind(train_pahs[,c(1:4,6)], train_pahs_scores[,1:3],train_pahs[,c(23:27,31:ncol(train_pahs))])
test_XY = cbind(test[,c(1:4,6)], test_scores[,1:3],test[,c(23:27,31:ncol(test))])

saveRDS(pest_XY, "../Processed/PesticidesPCBBDEs_train_XY.rds")
write.csv(pest_XY, paste0("../Exports/PesticidesPCBBDEs_train_XY.csv"))
saveRDS(pahs_XY, "../Processed/PAHs_train_XY.rds")
write.csv(pahs_XY, paste0("../Exports/PAHs_train_XY.csv"))
saveRDS(test_XY, "../Processed/Overlap_test_XY.rds")
write.csv(test_XY, paste0("../Exports/Overlap_test_XY.csv"))
