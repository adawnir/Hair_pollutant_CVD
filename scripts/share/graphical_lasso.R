
### Graphical lasso model script
## Created by Chia-An

rm(list=ls())
project_path="~/hair/scripts"
setwd(project_path)

##### Load packages and source functions

LoadPackages=function(packages){
  for (i in 1:length(packages)){
    suppressPackageStartupMessages(library(packages[i], character.only=TRUE))
  }
}

LoadPackages(c("pheatmap","corpcor","abind","parallel",
               "RColorBrewer","igraph","ppcor","mvtnorm",
               "pROC","glasso","stabs","huge","pulsar",
               "QUIC","glassoFast","colorspace","glmnet","tidyverse"))

source("penalisation_functions.R")

##### Load data 
pest <- readRDS("../processed/uni_multi_pest.rds")
pahs <- readRDS("../processed/uni_multi_pahs.rds")

pest1 <- readRDS("../processed/imputation/imp_pest.rds")
pahs1 <- readRDS("../processed/imputation/imp_pahs.rds")


##### settings --
colours1 <- RColorBrewer::brewer.pal(8, "Dark2")
colours2 <- RColorBrewer::brewer.pal(12, "Paired")
mycolours <- c(colours1[1:3], colours2[c(1:2, 5:6, 3, 11)], colours1[6], colours2[9:10])

colnames(pest)[17:49] <- colnames(pest1)
colnames(pest)[17:49] <- c(colnames(pest1)[1:6], "Alpha-endosulfan", "Beta-endosulfan",
                           "Beta-HCH", "Gamma-HCH", colnames(pest1)[11:33])
colnames(pahs)[17:29] <- c("Cotinine", "Nicotine", colnames(pahs1)[3:13])

pest_net <- pest[,17:49]
pahs_net <- pahs[,17:29]


##### Running stability selection --
### Pest
t0=Sys.time()
#pk=c(4, 2, 4, 1, 2, 2, 1, 1, 1)
out_pest <- CalibrateNetwork(data=pest_net, K=100, tau=0.5, PFER_thr=20,
                             refine_calib_grid=FALSE, verbose=FALSE)
t1=Sys.time()
print(t1-t0)

saveRDS(out_pest, "../results/Network/calib_network_obj_pest.rds")

CalibrationPlot(out_pest)
A_pest <- CalibratedAdjacency(out_pest)

saveRDS(A_pest, "../results/Network/adj_mat_pest.rds")

mynode_colours_pest <- c(rep(mycolours[1], 3),
                         rep(mycolours[2], 1),
                         rep(mycolours[3], 9),
                         rep(mycolours[4], 9),
                         rep(mycolours[5], 4),
                         rep(mycolours[6], 2),
                         rep(mycolours[7], 2),
                         rep(mycolours[8], 1),
                         rep(mycolours[9], 1),
                         rep(mycolours[10], 1))

mygraph_pest <- GetGraph(adjacency=A_pest, node_color=mynode_colours_pest, satellites=T)
V(mygraph_pest)$size =  degree(mygraph_pest) + 2
V(mygraph_pest)$label.font = 2
V(mygraph_pest)$label.cex = 0.6

cairo_pdf("../figures/Network/network_pest.pdf", height = 7, width = 7)
par(mar=rep(0,4))
set.seed(1)
plot(mygraph_pest, layout=layout_with_fr(mygraph_pest))
legend("bottomright",pch=19,
       col = mycolours, 
       legend = c("PCB", "BDE", "Organochlorines", "Organophosphorus", 
                  "Pyrethroid", "Phenylpyrazole", "Carbamates", 
                  "Carboxamide", "Dinitroaniline", "Oxadiazole"), cex = 0.7, bty = "n")
dev.off()

print(sort(betweenness(mygraph_pest), decreasing=TRUE))
degree(mygraph_pest) %>% sort(decreasing=TRUE)


