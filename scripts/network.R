
### Sumemr Project -- Graphical Lasso Network
## Created by Vivian on 29 June added on 30 July

rm(list=ls())
project_path="~/hair/scripts"
setwd(project_path)

##### Load packages and source functions

#install.packages("ppcor")
#install.packages("stabs")
#install.packages("huge")
#install.packages("pulsar")
#install.packages("glassoFast")

LoadPackages=function(packages){
  for (i in 1:length(packages)){
    suppressPackageStartupMessages(library(packages[i], character.only=TRUE))
  }
}

LoadPackages(c("pheatmap","corpcor","abind","parallel",
               "RColorBrewer","igraph","ppcor","mvtnorm",
               "pROC","glasso","stabs","huge","pulsar",
               "QUIC","glassoFast","colorspace","glmnet","tidyverse"))

#library(gt)
source("penalisation_functions.R")

##### Load data 
pest <- readRDS("../processed/uni_multi_pest.rds")
pahs <- readRDS("../processed/uni_multi_pahs.rds")
#over <- readRDS("../processed/uni_multi_overlap.rds")

pest1 <- readRDS("../processed/imputation/imp_pest.rds")
pahs1 <- readRDS("../processed/imputation/imp_pahs.rds")

#over_net <- over[,17:62]
#over_net <- over_net[,c(7:24, 1:6)]


##### settings --
colours1 <- RColorBrewer::brewer.pal(8, "Dark2")
colours2 <- RColorBrewer::brewer.pal(12, "Paired")
mycolours <- c(colours1[1:3], colours2[c(1:2, 5:6, 3, 11)], colours1[6], colours2[9:10])

#mycolours <- darken(mycolours, amount = 0.2)
#mycolours  <- mycolours %>% adjust_luminance(-0.5)

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
#Lambda=LambdaGridNetwork(data=pest_net, pk=pk, lambda_path_refined_cardinal=50)
#out=CalibrateNetwork(data=pest_net, pk=pk, Lambda=Lambda, lambda_other_blocks=apply(Lambda,2,min), 
#                     K=100, tau=0.5, PFER_thr=5,
#                     refine_calib_grid=FALSE, verbose=FALSE)
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


### Pest Lux
pest_L <- pest[which(pest$Country == "Luxembourg"),]
pest_net_L <- pest_L[,17:49]

t0=Sys.time()
out_pest_L <- CalibrateNetwork(data=pest_net_L, K=100, tau=0.5, PFER_thr=20,
                             refine_calib_grid=FALSE, verbose=FALSE)
t1=Sys.time()
print(t1-t0)

saveRDS(out_pest_L, "../results/Network/calib_network_obj_pest_L.rds")

CalibrationPlot(out_pest_L)
A_pest_L <- CalibratedAdjacency(out_pest_L)

saveRDS(A_pest_L, "../results/Network/adj_mat_pest_L.rds")


mygraph_pest_L <- GetGraph(adjacency=A_pest_L, node_color=mynode_colours_pest, satellites=T)
V(mygraph_pest_L)$size =  degree(mygraph_pest_L) + 2
V(mygraph_pest_L)$label.font = 2
V(mygraph_pest_L)$label.cex = 0.6

cairo_pdf("../figures/Network/network_pest_L.pdf", height = 7, width = 7)
par(mar=rep(0,4))
set.seed(1)
plot(mygraph_pest_L, layout=layout_with_fr(mygraph_pest_L))
legend("bottomleft",pch=19,
       col = mycolours,
       legend = c("PCB", "BDE", "Organochlorines", "Organophosphorus", 
                  "Pyrethroid", "Phenylpyrazole", "Carbamates", 
                  "Carboxamide", "Dinitroaniline", "Oxadiazole"), cex = 0.7, bty = "n")
dev.off()

print(sort(betweenness(mygraph_pest_L), decreasing=TRUE))
degree(mygraph_pest_L) %>% sort(decreasing=TRUE)


### Pest Bel
pest_B <- pest[which(pest$Country == "Belgium"),]
pest_net_B <- pest_B[,17:49]

t0=Sys.time()
out_pest_B <- CalibrateNetwork(data=pest_net_B, K=100, tau=0.5, PFER_thr=20,
                               refine_calib_grid=FALSE, verbose=FALSE)
t1=Sys.time()
print(t1-t0)

saveRDS(out_pest_B, "../results/Network/calib_network_obj_pest_B.rds")

CalibrationPlot(out_pest_B)
A_pest_B <- CalibratedAdjacency(out_pest_B)

saveRDS(A_pest_B, "../results/Network/adj_mat_pest_B.rds")


mygraph_pest_B <- GetGraph(adjacency=A_pest_B, node_color=mynode_colours_pest, satellites=T)
V(mygraph_pest_B)$size =  degree(mygraph_pest_B) + 2
V(mygraph_pest_B)$label.font = 2
V(mygraph_pest_B)$label.cex = 0.6

cairo_pdf("../figures/Network/network_pest_B.pdf", height = 7, width = 7)
par(mar=rep(0,4))
set.seed(1)
plot(mygraph_pest_B, layout=layout_with_fr(mygraph_pest_B))
legend("bottomleft",pch=19,
       col = mycolours,
       legend = c("PCB", "BDE", "Organochlorines", "Organophosphorus", 
                  "Pyrethroid", "Phenylpyrazole", "Carbamates", 
                  "Carboxamide", "Dinitroaniline", "Oxadiazole"), cex = 0.7, bty = "n")
dev.off()

print(sort(betweenness(mygraph_pest_B), decreasing=TRUE))
degree(mygraph_pest_B) %>% sort(decreasing=TRUE)


### Pest France
pest_F <- pest[which(pest$Country == "France"),]
pest_net_F <- pest_F[,17:49]

t0=Sys.time()
out_pest_F <- CalibrateNetwork(data=pest_net_F, K=100, tau=0.5, PFER_thr=20,
                               refine_calib_grid=FALSE, verbose=FALSE)
t1=Sys.time()
print(t1-t0)

saveRDS(out_pest_F, "../results/Network/calib_network_obj_pest_F.rds")

CalibrationPlot(out_pest_F)
A_pest_F <- CalibratedAdjacency(out_pest_F)

saveRDS(A_pest_F, "../results/Network/adj_mat_pest_F.rds")


mygraph_pest_F <- GetGraph(adjacency=A_pest_F, node_color=mynode_colours_pest, satellites=T)
V(mygraph_pest_F)$size =  degree(mygraph_pest_F) + 2
V(mygraph_pest_F)$label.font = 2
V(mygraph_pest_F)$label.cex = 0.6

cairo_pdf("../figures/Network/network_pest_F.pdf", height = 7, width = 7)
par(mar=rep(0,4))
set.seed(2)
plot(mygraph_pest_F, layout=layout_with_fr(mygraph_pest_F))
legend("bottomleft",pch=19,
       col = mycolours,
       legend = c("PCB", "BDE", "Organochlorines", "Organophosphorus", 
                  "Pyrethroid", "Carbamates", 
                  "Carboxamide", "Dinitroaniline", "Oxadiazole"), cex = 0.7, bty = "n")
dev.off()




### PAHs
t0=Sys.time()
out_pahs <- CalibrateNetwork(data=pahs_net, K=100, tau=0.5, PFER_thr=5,
                             refine_calib_grid=FALSE, verbose=FALSE)
t1=Sys.time()
print(t1-t0)

saveRDS(out_pahs, "../results/Network/calib_network_obj_pahs.rds")

CalibrationPlot(out_pahs)
A_pahs <- CalibratedAdjacency(out_pahs)

saveRDS(A_pahs, "../results/Network/adj_mat_pahs.rds")


mynode_colours_pahs <- c(rep(mycolours[1],2),
                         rep(mycolours[3],11))

mygraph_pahs <- GetGraph(adjacency=A_pahs, node_color=mynode_colours_pahs, satellites = T)
V(mygraph_pahs)$size =  degree(mygraph_pahs) + 2
V(mygraph_pahs)$label.font = 2
V(mygraph_pahs)$label.cex = 0.6

pdf("../figures/Network/network_pahs.pdf", height = 7, width = 7)
par(mar=rep(0,4))
set.seed(4)
plot(mygraph_pahs, layout=layout_with_fr(mygraph_pahs))
legend("bottomright",pch=19,
       col = mycolours[c(1,3)],
       legend = c("Tobacco-related chemicals", "Metabolites of PAHs"), cex = 0.8, bty = "n")
dev.off()

print(sort(betweenness(mygraph_pahs), decreasing=TRUE))
degree(mygraph_pahs) %>% sort(decreasing=TRUE)


### PAHs Lux
pahs_L <- pahs[which(pahs$Country == "Luxembourg"),]
pahs_net_L <- pahs_L[,17:29]

t0=Sys.time()
out_pahs_L <- CalibrateNetwork(data=pahs_net_L, K=100, tau=0.5, PFER_thr=5,
                             refine_calib_grid=FALSE, verbose=FALSE)
t1=Sys.time()
print(t1-t0)

saveRDS(out_pahs_L, "../results/Network/calib_network_obj_pahs_L.rds")

CalibrationPlot(out_pahs_L)
A_pahs_L <- CalibratedAdjacency(out_pahs_L)

saveRDS(A_pahs_L, "../results/Network/adj_mat_pahs_L.rds")


mygraph_pahs_L <- GetGraph(adjacency=A_pahs_L, node_color=mynode_colours_pahs, satellites = T)
V(mygraph_pahs_L)$size =  degree(mygraph_pahs_L) + 2
V(mygraph_pahs_L)$label.font = 2
V(mygraph_pahs_L)$label.cex = 0.6

pdf("../figures/Network/network_pahs_L.pdf",height = 7, width = 7)
par(mar=rep(0,4))
set.seed(4)
plot(mygraph_pahs_L, layout=layout_with_fr(mygraph_pahs_L))
legend("topright",pch=19,
       col = mycolours[c(1,3)],
       legend = c("Tobacco-related chemicals", "Metabolites of PAHs"), cex = 0.7, bty = "n")
dev.off()


### PAHs Bel
pahs_B <- pahs[which(pahs$Country == "Belgium"),]
pahs_net_B <- pahs_B[,17:29]

t0=Sys.time()
out_pahs_B <- CalibrateNetwork(data=pahs_net_B, K=100, tau=0.5, PFER_thr=5,
                               refine_calib_grid=FALSE, verbose=FALSE)
t1=Sys.time()
print(t1-t0)

saveRDS(out_pahs_B, "../results/Network/calib_network_obj_pahs_B.rds")

CalibrationPlot(out_pahs_B)
A_pahs_B <- CalibratedAdjacency(out_pahs_B)

saveRDS(A_pahs_B, "../results/Network/adj_mat_pahs_B.rds")


mygraph_pahs_B <- GetGraph(adjacency=A_pahs_B, node_color=mynode_colours_pahs, satellites = T)
V(mygraph_pahs_B)$size =  degree(mygraph_pahs_B) + 2
V(mygraph_pahs_B)$label.font = 2
V(mygraph_pahs_B)$label.cex = 0.6

pdf("../figures/Network/network_pahs_B.pdf", height = 7, width = 7)
par(mar=rep(0,4))
set.seed(4)
plot(mygraph_pahs_B, layout=layout_with_fr(mygraph_pahs_B))
legend("topright",pch=19,
       col = mycolours[c(1,3)],
       legend = c("Tobacco-related chemicals", "Metabolites of PAHs"), cex = 0.7, bty = "n")
dev.off()


### Overlap
t0=Sys.time()
#pk_over <- c(2, 3, 1, 4, 2, 4, 1, 2, 2, 1, 1, 1)
out_over <- CalibrateNetwork(data=over_net, K=100, tau=0.5, PFER_thr=40,
                             refine_calib_grid=FALSE, verbose=FALSE)
#Lambda_over <- LambdaGridNetwork(data=over_net, pk=pk_over, lambda_path_refined_cardinal=50)
#out=CalibrateNetwork(data=pest_net, pk=pk, Lambda=Lambda, lambda_other_blocks=apply(Lambda,2,min), 
#                     K=100, tau=0.5, PFER_thr=5,
#                     refine_calib_grid=FALSE, verbose=FALSE)
t1=Sys.time()
print(t1-t0)

saveRDS(out_over, "../results/Network/calib_network_obj_overlap.rds")

CalibrationPlot(out_over)
A_over <- CalibratedAdjacency(out_over)

saveRDS(A_over, "../results/Network/adj_mat_overlap.rds")


mynode_colours_over <- c(rep("#FF000080", 2),
                         rep("#FF6D0080", 11),
                         rep("#FFDB0080", 3),
                         rep("#B6FF0080", 1),
                         rep("#49FF0080", 9),
                         rep("#00FF2480", 3),
                         rep("#00FF9280", 6),
                         rep("#00FFFF80", 2),
                         rep("#0092FF80", 2),
                         rep("#0024FF80", 2),
                         rep("#4900FF80", 2),
                         rep("#B600FF80", 1),
                         rep("#FF00DB80", 1),
                         rep("#FF006D80"), 1)
mygraph_over <- GetGraph(adjacency=A_over, node_color=mynode_colours_over)
V(mygraph_over)$size = 0.5 * (degree(mygraph_over)) + 5
V(mygraph_over)$label.font = 2
V(mygraph_over)$label.cex = 0.45

pdf("../figures/Network/network_over.pdf")
par(mar=rep(0,4))
set.seed(1)
plot(mygraph_over, layout=layout_with_fr(mygraph_over))
legend("topleft",pch=19,
       col = c("#FF000080", "#FF6D0080", "#FFDB0080", "#B6FF0080", "#49FF0080", "#00FF2480",
               "#00FF9280", "#00FFFF80", "#0092FF80", "#0024FF80", "#4900FF80",
               "#B600FF80", "#FF00DB80", "#FF006D80"),
       legend = c("Tobacco related", "PAHs", 
                  "PCB", "BDE", "Organochlorines", "Organophosphorus", 
                  "Metabolites_of_Organophosphorus", 
                  "Pyrethroid", "Metabolites_of_Pyrethroid", "Phenylpyrazole", "Carbamates", 
                  "Carboxamide", "Dinitroaniline", "Oxadiazole"), 
       cex = 0.6, bty = "n")
dev.off()

print(sort(betweenness(mygraph_over), decreasing=TRUE))
degree(mygraph_over) %>% sort(decreasing=TRUE)
