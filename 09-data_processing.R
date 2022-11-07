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

x = rbind(train_pest,train_pahs)
x = x %>% select(Energy, Proteins, Fats, Carbohydrates, Fibers)
mypca = PCA(x, graph = FALSE)
saveRDS(mypca, paste0("../Results/DietScore/Training_diet_info_pca.rds"))
  
scores = mypca$ind$coord
colnames(scores) = paste0("DietScore",1:ncol(scores))

loadings = mypca$var$coord
ev=mypca$eig[,2]/100
print(ev)
  
# Scree plot
pdf(paste0("../Figures/Descriptive_statistics/Diet_info_pca_scree_plot.pdf"), width=5, height=5)
par(mar=c(5,5,1,1))
plot(ev, pch=19, col="#0019a8", las=1, type="b", ylim=c(0,1),
     ylab="Proportion of explained variance", xlab="PCs")
points(cumsum(ev), pch=19, col="#dc251f", type="b")
legend("right", pch=19, col=c("#0019a8", "#dc251f"),
       legend=c("Proportion of e.v.", "Cumulative proportion of e.v."))
dev.off()

## S3 method for class 'PCA'
predict = predict(mypca, test %>% select(Energy, Proteins, Fats, Carbohydrates, Fibers))
test_scores = predict$coord

train_pest_scores = scores[rownames(train_pest),]
train_pahs_scores = scores[rownames(train_pahs),]

mypca$var$coord[,1:3]
mypca$var$contrib[,1:3]

for(i in 1:3){
  covar = eval(parse(text = prefix[i]))
  scores = eval(parse(text = paste0(prefix[i],"_scores")))
  covar = covar %>% mutate(DietScore1 = as.numeric(scores[,1]),
                           DietScore2 = as.numeric(scores[,2]),
                           DietScore3 = as.numeric(scores[,3])) %>%
    select(Centre, Age, Gender, Smoking, starts_with("DietScore"),
           Education, Physical_activity, Alcohol)
  covar$Education = relevel(covar$Education, "High")
  covar$Physical_activity = relevel(covar$Physical_activity, "Active")
  covar$Alcohol = factor(covar$Alcohol, levels = c("Low","High","Moderate"))

  saveRDS(covar, paste0("../Processed/",filepaths[i],"_Z.rds"))
  write.csv(covar, paste0("../Exports/Data/",filepaths[i],"_Z.csv"))
}
