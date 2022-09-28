### Data processing: Clustering binary outcomes

rm(list=ls())
project_path=dirname(rstudioapi::getActiveDocumentContext()$path)
setwd(project_path)
setwd("../")

# Load packages
library(tidyverse)
library(RColorBrewer)
library(networkD3)
library(pheatmap)
library(cluster)

## Load data ----

data = readRDS(paste0("../Data/","All_participants_info.rds"))
tmp = data[,23:27 ]%>% mutate_all(as.integer)-1
# # Subset disease group (vs control)
# tmp = tmp[apply(tmp,1,function(x) sum(x)>0),]

# tmp = data %>% select(BMI, WC, SBP, DBP, TC, TG, LDLC, HDLC, FPG,
#                       ends_with("medication")) %>%
#   mutate_if(is.factor, function(x) as.logical(as.integer(x)-1))

# # calculate gower distance metric
# d = daisy(tmp, metric = "gower")

# Jaccard distance
d = dist(tmp, method = "binary")
# 
# # Dice coefficient
# d = 2*d/(1+d)

# Hierarchical clustering
h = hclust(d, "ward.D2")

klist = NULL
for (k in 2:6){
  myclusters = cutree(h, k = k)
  if (min(table(myclusters))>nrow(tmp)*0.05){
    klist = c(klist, k)
  }
}
myclusters = factor(cutree(h, k = max(klist)))

# # Create an incidence matrix. Usually the flow goes from the row names to the column names.
# # Remember that our connection are directed since we are working with a flow.
# data = matrix(table(interaction(tmp),myclusters),
#               ncol = max(myclusters))
# rownames(data) = names(table(interaction(tmp)))
# colnames(data) = names(table(myclusters))
# 
# # Transform it to connection data frame with tidyr from the tidyverse:
# links = data %>% 
#   as.data.frame() %>% 
#   rownames_to_column(var="source") %>% 
#   gather(key="target", value="value", -1) %>%
#   filter(value != 0)
# 
# # From these flows we need to create a node data frame: it lists every entities involved in the flow
# nodes = data.frame(
#   name=c(as.character(links$source), as.character(links$target)) %>% 
#     unique()
# )
# 
# # With networkD3, connection must be provided using id, not using real name like in the links dataframe.. So we need to reformat it.
# links$IDsource = match(links$source, nodes$name)-1 
# links$IDtarget = match(links$target, nodes$name)-1
# 
# # Make the Network
# p = sankeyNetwork(Links = links, Nodes = nodes,
#                    Source = "IDsource", Target = "IDtarget",
#                    Value = "value", NodeID = "name", 
#                    sinksRight=FALSE)
# 
# p

mycolours = c("#a1a5a7","#93ceba","#9b0058","#994517","#0019a8","#000000")
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
           cutree_cols = 6,
           annotation_col = mat_annot,
           annotation_colors = mat_colors,
           annotation_legend = TRUE,
           legend_breaks = c(0,1),
           legend_labels = c("No","Yes"))
  
  dev.off()
}

OutcomeScore = myclusters
names(OutcomeScore) = rownames(tmp)

filepaths = c("PesticidesPCBBDEs_train","PAHs_train","Overlap_test")
for (i in 1:3){
  data = readRDS(paste0("../Processed/",filepaths[i],"_XY.rds"))
  OutcomeCluster = OutcomeScore[rownames(data)]
  data = cbind(data[,1:13],OutcomeCluster,data[,14:ncol(data)])
  saveRDS(data, paste0("../Processed/",filepaths[i],"_XY_Comorbidity.rds"))
}




