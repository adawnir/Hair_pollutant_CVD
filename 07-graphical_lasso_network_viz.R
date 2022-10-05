## Graphical Lasso Network Visualisation

rm(list=ls())
project_path=dirname(rstudioapi::getActiveDocumentContext()$path)
setwd(project_path)

library(tidyverse)
library(igraph)
library(colorspace)
library(sharp)
library(grid)

source("graph_colours.R")

annot = readRDS("../Data/Compound_family_annotation.rds")

filepaths = c("PesticidesPCBBDEs_train","PAHs_train","Overlap_test")
for (m in 1:length(filepaths)){
  for (p in seq(10,50,10)){
    out = readRDS(paste0("../Results/Graphical_lasso_network/",filepaths[m],"_exposures_network_output_PFER",p,".rds"))
    ifelse(dir.exists("../Figures/"),"",dir.create("../Figures/"))
    ifelse(dir.exists("../Figures/Descriptive_statistics/"),"",dir.create("../Figures/Descriptive_statistics/"))
    pdf(paste0("../Figures/Descriptive_statistics/Graphical_network_exposures_output_",filepaths[m],"_PFER",p,".pdf"), width = 15)
    par(mar = c(7,5,7,6), mfrow = c(1, 3), pty = "s")
    CalibrationPlot(out)
    dev.off()
    
    # Adjacency matrix of the calibrated network
    A=Adjacency(out)
    saveRDS(A, paste0("../Results/Graphical_network/",filepaths[m],"_exposures_network_adjacency_matrix_PFER",p,".rds"))
    
    expo = colnames(A)[6:ncol(A)]
    
    annot_sub = annot[expo]
    mynode_colour = c("#ffcc00","#9b0058","#00afac","#994517","#00722a",
                      annot.colours[annot_sub])
    names(mynode_colour) = colnames(A)
    
    # Getting igraph object
    mygraph=Graph(adjacency=A,node_colour=mynode_colour, satellites = TRUE)
    
    ## Community detection
    # Louvain
    lc = cluster_louvain(mygraph)
    
    # Set cluster colour
    cluster_colour = rainbow(length(unique(lc$membership)), alpha = 0.1)[membership(lc)]
    names(cluster_colour) = membership(lc)
    
    E(mygraph)$color = ifelse(igraph::crossing(lc, mygraph),
                              alpha('grey',0.6),
                              darken(cluster_colour[as.character(membership(lc)[ends(mygraph, E(mygraph))[,1]])],0.5))  
    
    # Increase vertex size
    V(mygraph)$size = V(mygraph)$size + 5
    V(mygraph)$shape = rep(c("square","circle"),c(5,length(expo)))
    
    # Plot clusters in graph
    pdf(paste0("../Figures/Descriptive_statistics/Graphical_network_exposures_community_",filepaths[m],"_PFER",p,".pdf"))
    par(mar=rep(1,4), xpd = TRUE)
    set.seed(1)
    plot(mygraph, layout=layout_with_fr(mygraph),
         vertex.label.cex = 0.7,
         mark.groups = communities(lc),
         mark.col = unique(cluster_colour[order(names(cluster_colour))]),
         mark.border = unique(cluster_colour[order(names(cluster_colour))]))
    dev.off()
  }
}

{pdf("../Figures/Descriptive_statistics/Graphical_network_exposures_legend.pdf", height = 3, width = 3)
  grid.newpage()
  for(e in 1:length(annot.colours)) {
    grid.circle(
      2,
      e*0.55,
      0.25,
      default.units = "cm",
      gp=gpar( 
        fill=rev(annot.colours)[e],
        lwd=0
      )
    )
    grid.text(
      label = rev(names(annot.colours))[e],
      x = 2.5,
      y = e*0.55,
      default.units = "cm",
      hjust = 0
    )
  }
  dev.off()
}

