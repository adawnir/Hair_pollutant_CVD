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
pest = readRDS("../Data/PesticidesPCBBDEs.rds")
pahs = readRDS("../Data/PAHs.rds")
comb = readRDS("../Data/Combined.rds")

filepaths = c("PesticidesPCBBDEs","PAHs","Combined")
prefix = c("pest","pahs","comb")
for (i in 1:3){
  expo = readRDS(paste0("../Processed/",filepaths[i],"_ndimp_naimp_log.rds"))
  assign(paste0(prefix[i],"_expo"), expo)
}

## Nutrient information ----
nutri = readRDS("../Processed/All_participants_nutri_naimp.rds")
mypca = PCA(nutri, graph = FALSE)
saveRDS(mypca, paste0("../Results/All_participants_nutri_pca.rds"))
scores = mypca$ind$coord
colnames(scores) = paste0("NutriScore",1:ncol(scores))
loadings = mypca$var$coord
ev=mypca$eig[,2]/100

# Scree plot
pdf(paste0("../Figures/Nutrition_PCA_scree_plot.pdf"), width=5, height=5)
par(mar=c(5,5,1,1))
plot(ev, pch=19, col="navy", las=1, type="b", ylim=c(0,1),
     ylab="Proportion of explained variance", xlab="PCs")
points(cumsum(ev), pch=19, col="red", type="b")
legend("right", pch=19, col=c("navy", "red"),
       legend=c("Proportion of e.v.", "Cumulative proportion of e.v."))
dev.off()

## Reconstruct dataset ----
pest_XY = cbind(pest[,c(1:4,6:11)], scores[pest$ID,1:3],pest[,c(5,12:19,23:27)],pest_expo)
pahs_XY = cbind(pahs[,c(1:4,6:11)], scores[pahs$ID,1:3],pahs[,c(5,12:19,23:27)],pahs_expo)
comb_XY = cbind(comb[,c(1:4,6:11)], scores[comb$ID,1:3],comb[,c(5,12:19,23:27)],comb_expo)

saveRDS(pest_XY, "../Processed/PesticidesPCBBDEs_XY.rds")
write.csv(pest_XY, paste0("../Exports/PesticidesPCBBDEs_XY.csv"))
saveRDS(pahs_XY, "../Processed/PAHs_XY.rds")
write.csv(pahs_XY, paste0("../Exports/PAHs_XY.csv"))
saveRDS(comb_XY, "../Processed/Combined_XY.rds")
write.csv(comb_XY, paste0("../Exports/Combined_XY.csv"))
