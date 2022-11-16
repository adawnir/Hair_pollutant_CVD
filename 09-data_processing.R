### Data processing: Composite score and reconstructing data sets

rm(list=ls())
project_path=dirname(rstudioapi::getActiveDocumentContext()$path)
setwd(project_path)

# Load packages
library(tidyverse)
library(RColorBrewer)
library(FactoMineR)
library(pheatmap)
source("functions.R")
source("graph_colours.R")

## Load data ----
filepath = "PesticidesPCBBDEs"
m = 1

data = readRDS(paste0("../Processed/",filepath,"_covar_naimp.rds"))

## Diet information ----
ifelse(dir.exists("../Results/"),"",dir.create("../Results/"))
ifelse(dir.exists("../Results/DietScore/"),"",dir.create("../Results/DietScore/"))

x = data %>% select(Energy, Proteins, Fats, Carbohydrates, Fibers)
mypca = PCA(x, graph = FALSE)
saveRDS(mypca, paste0("../Results/DietScore/Diet_info_pca_",filepath,".rds"))
  
scores = mypca$ind$coord
colnames(scores) = paste0("DietScore",1:ncol(scores))

loadings = mypca$var$coord
ev=mypca$eig[,2]/100
print(ev)
print(mypca$var$coord[,1:3])
print(mypca$var$contrib[,1:3])
  
# Scree plot
pdf(paste0("../Figures/Supplementary/Diet_info_pca_scree_plot.pdf"), width=5, height=5)
par(mar=c(5,5,1,1))
plot(ev, pch=19, col="#0019a8", las=1, type="b", ylim=c(0,1),
     ylab="Proportion of explained variance", xlab="PCs")
points(cumsum(ev), pch=19, col="#dc251f", type="b")
legend("right", pch=19, col=c("#0019a8", "#dc251f"),
       legend=c("Proportion of e.v.", "Cumulative proportion of e.v."))
dev.off()

covar = data %>% mutate(DietScore1 = as.numeric(scores[,1]),
                        DietScore2 = as.numeric(scores[,2]),
                        DietScore3 = as.numeric(scores[,3])) %>%
  select(Centre, Age, Gender, Smoking, starts_with("DietScore"),
         Education, Physical_activity, Alcohol)
covar$Education = relevel(covar$Education, "High")
covar$Physical_activity = relevel(covar$Physical_activity, "Active")
covar$Alcohol = factor(covar$Alcohol, levels = c("Low","High","Moderate"))

saveRDS(covar, paste0("../Processed/",filepath,"_Z.rds"))
write.csv(covar, paste0("../Exports/Data/",filepath,"_Z.csv"))
