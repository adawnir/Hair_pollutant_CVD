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
filepaths = c("PesticidesPCBBDEs_train","PAHs_train","Overlap_test")
prefix = c("train_pest","train_pahs","test")
for (i in 1:3){
  data = readRDS(paste0("../Processed/",filepaths[i],"_info_expo_ndimp_naimp_log.rds"))
  assign(prefix[i], data)
}

## Diet information ----
ifelse(dir.exists("../Results/"),"",dir.create("../Results/"))
ifelse(dir.exists("../Results/DietScore/"),"",dir.create("../Results/DietScore/"))

for (i in 1:3){
  x = eval(parse(text = prefix[i]))[,10:14]
  mypca = PCA(x, graph = FALSE)
  saveRDS(mypca, paste0("../Results/DietScore/",filepaths[i],"_diet_info_pca.rds"))
  
  scores = mypca$ind$coord
  colnames(scores) = paste0("DietScore",1:ncol(scores))
  assign(paste0(prefix[i],"_scores"), scores)
  
  loadings = mypca$var$coord
  ev=mypca$eig[,2]/100
  
  # Scree plot
  pdf(paste0("../Figures/Descriptive_statistics/Diet_info_pca_scree_plot_",filepaths[i],".pdf"), width=5, height=5)
  par(mar=c(5,5,1,1))
  plot(ev, pch=19, col="#0019a8", las=1, type="b", ylim=c(0,1),
       ylab="Proportion of explained variance", xlab="PCs")
  points(cumsum(ev), pch=19, col="#dc251f", type="b")
  legend("right", pch=19, col=c("#0019a8", "#dc251f"),
         legend=c("Proportion of e.v.", "Cumulative proportion of e.v."))
  dev.off()
}

## Clustering binary outcomes ----
data = readRDS(paste0("../Data/","All_participants_info.rds"))
tmp = data[,26:30 ]%>% mutate_all(as.integer)-1

# Jaccard distance
d = dist(tmp, method = "binary")

# Hierarchical clustering
h = hclust(d, "ward.D2")

klist = NULL
for (k in 2:6){
  myclusters = cutree(h, k = k)
  if (min(table(myclusters))>nrow(tmp)*0.05){
    klist = c(klist, k)
  }
}

k = 5
myclusters = factor(cutree(h, k = k))

# pheatmap(t(tmp),
#          border_color = NA,
#          color = c(lighten("#00afac",0.5),"#f4a9be"),
#          treeheight_row = 0,
#          cluster_rows = FALSE,
#          labels_row = gsub("_"," ",colnames(tmp)),
#          show_colnames = FALSE,
#          clustering_distance_cols = "binary",
#          clustering_method = "ward.D2",
#          treeheight_col = 50,
#          cutree_cols = k,
#          legend_breaks = c(0,1),
#          legend_labels = c("No","Yes"))
myclusters = factor(myclusters, levels = c(1,4,3,5,2),
                    labels = c("No disease", "A","B","C","D"))

mycolours = c("#a1a5a7","#93ceba","#9b0058","#994517","#000000")
mat_annot = data.frame(Cluster = myclusters)
rownames(mat_annot) = rownames(tmp)
mat_colors = list(Cluster = mycolours)
names(mat_colors$Cluster) = levels(myclusters)

ifelse(dir.exists("../Figures/Comorbidity"),"",dir.create("../Figures/Comorbidity"))
{pdf(paste0("../Figures/Comorbidity/Heatmap_hclust_wardd2_All_participants.pdf"))
  pheatmap(t(tmp),
           border_color = NA,
           color = c(lighten("#00afac",0.5),"#f4a9be"),
           treeheight_row = 0,
           cluster_rows = FALSE,
           labels_row = gsub("_"," ",colnames(tmp)),
           show_colnames = FALSE,
           clustering_distance_cols = "binary",
           clustering_method = "ward.D2",
           treeheight_col = 50,
           cutree_cols = 5,
           annotation_col = mat_annot,
           annotation_colors = mat_colors,
           annotation_legend = TRUE,
           legend_breaks = c(0,1),
           legend_labels = c("No","Yes"))
  dev.off()
}

OutcomeScore = myclusters
names(OutcomeScore) = rownames(tmp)

## Reconstruct dataset ----
pest_XY = cbind(train_pest[,c(1:4,6:9)],
                train_pest_scores[,1:3],
                train_pest[,c(26:30,5,15:22)],
                OutcomeScore[train_pest$ID],
                train_pest[,c(34:ncol(train_pest))])
colnames(pest_XY)[26] = "OutcomeCluster"
pahs_XY = cbind(train_pahs[,c(1:4,6:9)],
                train_pahs_scores[,1:3],
                train_pahs[,c(26:30,5,15:22)],
                OutcomeScore[train_pahs$ID],
                train_pahs[,c(34:ncol(train_pahs))])
colnames(pahs_XY)[26] = "OutcomeCluster"
test_XY = cbind(test[,c(1:4,6:9)],
                test_scores[,1:3],
                test[,c(26:30,5,15:22)],
                OutcomeScore[test$ID],
                test[,c(34:ncol(test))])
colnames(test_XY)[26] = "OutcomeCluster"

saveRDS(pest_XY, "../Processed/PesticidesPCBBDEs_train_XY.rds")
write.csv(pest_XY, paste0("../Exports/Data/PesticidesPCBBDEs_train_XY.csv"))
saveRDS(pahs_XY, "../Processed/PAHs_train_XY.rds")
write.csv(pahs_XY, paste0("../Exports/Data/PAHs_train_XY.csv"))
saveRDS(test_XY, "../Processed/Overlap_test_XY.rds")
write.csv(test_XY, paste0("../Exports/Data/Overlap_test_XY.csv"))
