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
  data = readRDS(paste0("../Processed/",filepaths[i],"_covar_naimp.rds"))
  assign(prefix[i], data)
}

## Diet information ----
ifelse(dir.exists("../Results/"),"",dir.create("../Results/"))
ifelse(dir.exists("../Results/DietScore/"),"",dir.create("../Results/DietScore/"))

for (i in 1:3){
  x = eval(parse(text = prefix[i]))
  x = x %>% select(Energy, Proteins, Fats, Carbohydrates, Fibers)
  mypca = PCA(x, graph = FALSE)
  saveRDS(mypca, paste0("../Results/DietScore/",filepaths[i],"_diet_info_pca.rds"))
  
  scores = mypca$ind$coord
  colnames(scores) = paste0("DietScore",1:ncol(scores))
  assign(paste0(prefix[i],"_scores"), scores)
  
  loadings = mypca$var$coord
  ev=mypca$eig[,2]/100
  print(ev)
  
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

for(i in 1:3){
  mypca = readRDS(paste0("../Results/DietScore/",filepaths[i],"_diet_info_pca.rds"))
  print(mypca$var$coord[,1:3])
}

for(i in 1:3){
  covar = eval(parse(text = prefix[i]))
  scores = eval(parse(text = paste0(prefix[i],"_scores")))
  covar = covar %>% mutate(DietScore1 = as.numeric(scores[,1]),
                           DietScore2 = as.numeric(scores[,2]),
                           DietScore3 = as.numeric(scores[,3])) %>%
    select(Country, Age, Gender, Smoking, starts_with("DietScore"),
           Education, Physical_activity, Alcohol)
  
  saveRDS(covar, paste0("../Processed/",filepaths[i],"_Z.rds"))
  write.csv(covar, paste0("../Exports/Data/",filepaths[i],"_Z.csv"))
}

## Clustering binary outcomes ----
data = readRDS(paste0("../Data/","All_participants_response.rds"))
tmp = data[,11:15 ]%>% mutate_all(as.integer)-1

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
#          labels_row = c(names(outcome.colours)),
#          show_colnames = FALSE,
#          clustering_distance_cols = "binary",
#          clustering_method = "ward.D2",
#          treeheight_col = 50,
#          cutree_cols = k,
#          legend_breaks = c(0,1),
#          legend_labels = c("Normal","Abnormal"))
myclusters = factor(myclusters, levels = c(1,4,3,2,5),
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
           labels_row = names(outcome.colours),
           show_colnames = FALSE,
           clustering_distance_cols = "binary",
           clustering_method = "ward.D2",
           treeheight_col = 50,
           cutree_cols = 5,
           annotation_col = mat_annot,
           annotation_colors = mat_colors,
           annotation_legend = TRUE,
           legend_breaks = c(0,1),
           legend_labels = c("Normal","Abnormal"))
  dev.off()
}

OutcomeCluster = myclusters
names(OutcomeCluster) = rownames(tmp)

for(i in 1:3){
  response = readRDS(paste0("../Data/",filepaths[i],"_response.rds"))
  response$OutcomeCluster = factor(OutcomeCluster[rownames(response)],
                                   labels = c("No disease", "A","B","C","D"))
  saveRDS(response, paste0("../Processed/",filepaths[i],"_Y.rds"))
  write.csv(response, paste0("../Exports/Data/",filepaths[i],"_Y.csv"))
}
