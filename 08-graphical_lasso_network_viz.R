## Graphical Lasso Network Visualisation

rm(list=ls())
project_path=dirname(rstudioapi::getActiveDocumentContext()$path)
setwd(project_path)

library(tidyverse)
library(igraph)
library(colorspace)
library(focus) # library(sharp)
library(grid)

source("graph_colours.R")

annot = readRDS("../Data/Compound_family_annotation.rds")

filepaths = c("PesticidesPCBBDEs","PAHs","Combined")
for (m in 1:length(filepaths)){
  A = readRDS(paste0("../Results/",filepaths[m],"_graphical_network_exposures_output.rds"))
  expo = readRDS(paste0("../Processed/",filepaths[m],"_ndimp_naimp_log.rds"))
  
  annot_sub = annot[colnames(expo)]
  mynode_colour = annot.colours[annot_sub]
  names(mynode_colour) = colnames(expo)
  
  # Getting igraph object
  mygraph=Graph(adjacency=A,node_colour=mynode_colour)
  
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
  
  # Plot clusters in graph
  pdf(paste0("../Figures/Graphical_network_exposures_community_",filepaths[m],".pdf"))
  par(mar=rep(1,4), xpd = TRUE)
  set.seed(1)
  plot(mygraph, layout=layout_with_fr(mygraph),
       vertex.label.cex = 0.8,
       mark.groups = communities(lc),
       mark.col = unique(cluster_colour[order(names(cluster_colour))]),
       mark.border = unique(cluster_colour[order(names(cluster_colour))]))
  dev.off()
}

{pdf("../Figures/Graphical_network_exposures_legend.pdf", height = 3, width = 3)
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

