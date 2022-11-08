## Multivariate analyses: association between outcomes and exposures adjusted for covariates

rm(list=ls())
project_path=dirname(rstudioapi::getActiveDocumentContext()$path)
setwd(project_path)

library(tidyverse)
library(colorspace)
library(sharp)
source("graph_colours.R")
filepaths = c("PesticidesPCBBDEs_train","PAHs_train","Overlap_test")

i = 1

# Load data
Y = readRDS(paste0("../Data/",filepaths[i],"_response.rds"))
model = c("Model 0: + Age + Gender", "Model 0 + Centre", "Model 0 + Education",
          "+ Smoking status", "+ Dietary intake",
          "+ Physical activity", "+ Alcohol consumption")

for (k in 1:9){
  pdf(paste0("../Figures/Multivariate/",colnames(Y)[k],"_X_summary_selprop_",filepaths[i],".pdf"), height = 8)
  par(mar=c(1,1,0,1), oma = c(6,13,3,5), mfrow = c(7,1))
  # Load data
  for (f in 0:6){
    stab = readRDS(paste0("../Results/Multivariate/",colnames(Y)[k],"_X_f",f,"_output_",filepaths[i],".pdf"))
    selprop = readRDS(paste0("../Results/Multivariate/",colnames(Y)[k],"_X_f",f,"_selprop_",filepaths[i],".pdf"))
    # assign(paste0("selprop",f),selprop)
    annot_sub = annot[names(selprop)]
    pi = Argmax(stab)[2]
    plot(selprop, type = "h", ylim = c(0,1),
         col = ifelse(SelectedVariables(stab)==1, darken(annot.colours[annot_sub],0.5),"black"),
         xaxt = "n", yaxt = "n", lwd=2,
         xlab = "", ylab = "")
    axis(side=4, at=axTicks(2), cex.axis = 0.8)
    abline(h = pi, col = "darkred", lty = 2)
    coord = par("usr")
    mtext(model[f+1], cex = 0.8, side = 2, line = 1, outer = FALSE, las = 1)
    text(coord[2], pi, paste0("Threshold = ",round(pi, 2)), col = "darkred", adj = c(1,1), cex = 0.8)
    if(f==6){
      mtext("Selection Proportion", cex = 1.2, side = 4, line = 2, outer = TRUE)
      mtext(colnames(Y)[k], cex = 1.5, side = 3, line = 1, outer = TRUE)
      for (j in 1:length(selprop)){
        axis(side=1, at=j, labels=names(selprop)[j],
             col.axis = ifelse(SelectedVariables(stab)[j]==1, darken(annot.colours[annot_sub[j]],0.5),"black"),
             las=2, cex.axis = 0.8)
      }
    }
  }
  dev.off() 
}
for (j in 1:3){
  pdf(paste0("../Figures/Multivariate/MLASSO",j,"_X_summary_selprop_",filepaths[i],".pdf"), height = 8)
  par(mar=c(1,1,0,1), oma = c(6,13,3,5), mfrow = c(7,1))
  # Load data
  for (f in 0:6){
    stab = readRDS(paste0("../Results/Multivariate/MLASSO",j,"_X_f",f,"_output_",filepaths[i],".pdf"))
    selprop = readRDS(paste0("../Results/Multivariate/MLASSO",j,"_X_f",f,"_selprop_",filepaths[i],".pdf"))
    # assign(paste0("selprop",f),selprop)
    annot_sub = annot[names(selprop)]
    pi = Argmax(stab)[2]
    plot(selprop, type = "h", ylim = c(0,1),
         col = ifelse(SelectedVariables(stab)==1, darken(annot.colours[annot_sub],0.5),"black"),
         xaxt = "n", yaxt = "n", lwd=2,
         xlab = "", ylab = "")
    axis(side=4, at=axTicks(2), cex.axis = 0.8)
    abline(h = pi, col = "darkred", lty = 2)
    coord = par("usr")
    mtext(model[f+1], cex = 0.8, side = 2, line = 1, outer = FALSE, las = 1)
    text(coord[2], pi, paste0("Threshold = ",round(pi, 2)), col = "darkred", adj = c(1,1), cex = 0.8)
    if(f==6){
      mtext("Selection Proportion", cex = 1.2, side = 4, line = 2, outer = TRUE)
      mtext("Multiple LASSO", cex = 1.5, side = 3, line = 1, outer = TRUE)
      for (j in 1:length(selprop)){
        axis(side=1, at=j, labels=names(selprop)[j],
             col.axis = ifelse(SelectedVariables(stab)[j]==1, darken(annot.colours[annot_sub[j]],0.5),"black"),
             las=2, cex.axis = 0.8)
      }
    }
  }
  dev.off() 
}


