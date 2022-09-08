### PCA of population based on exposure

rm(list=ls())
project_path=dirname(rstudioapi::getActiveDocumentContext()$path)
setwd(project_path)

# Load packages
library(tidyverse)
library(RColorBrewer)
library(FactoMineR)
source("functions.R")
source("graph_colours.R")

## Exposures ----
annot = readRDS("../Data/Compound_family_annotation.rds")

filepaths = c("PesticidesPCBBDEs","PAHs","Combined")
for (i in 1:3){
  # Load data
  data = readRDS(paste0("../Data/",filepaths[i],".rds"))
  expo = readRDS(paste0("../Processed/",filepaths[i],"_ndimp_naimp_log.rds"))
  print(ncol(expo))
  
  mypca=PCA(expo, graph = FALSE)
  ev=mypca$eig[,2]
  
  CreateScorePlot(mypca=mypca, type=as.character(data$Country),
                  mycolours=country.colours[levels(data$Country)],
                  filename=paste0("../Figures/PCA_score_plot_",filepaths[i],".pdf"),
                  ellipse = TRUE)
  mycor=cor(expo, mypca$ind$coord)
  
  annot_sub = annot[colnames(expo)]
  {pdf(paste0("../Figures/PCA_correlation_circle_",filepaths[i],".pdf"), width=17.5, height=5)
    par(mar = c(5,5,1,1))
    layout(matrix(c(1,2,3,4), 1, 4, byrow = TRUE), widths=c(2,2,2,1))
    comp=matrix(c(1,2,1,3,2,3), byrow=TRUE, ncol=2)
    for (k in 1:nrow(comp)){
      xcomp=comp[k,1]
      ycomp=comp[k,2]
      plot(mycor[,c(xcomp,ycomp)], xlim=c(-1.25,1.25), ylim=c(-1.25,1.25), cex=0.1, pch=19, las=1, cex.lab=1.5,
           xlab=paste0("Comp ",xcomp," (", round(ev[xcomp], digits=2), "% e.v.)"),
           ylab=paste0("Comp ",ycomp," (", round(ev[ycomp], digits=2), "% e.v.)"),
           col=annot.colours[annot_sub])
      arrows(x0=rep(0, nrow(mycor)), y0=rep(0, nrow(mycor)),
             x1=mycor[,xcomp], y1=mycor[,ycomp], length=0.1, col=annot.colours[annot_sub])
      abline(h=0, lty=2)
      abline(v=0, lty=2)
      xseq=seq(-1,1,length.out=10000)
      lines(xseq, sqrt(1-xseq^2))
      lines(xseq, -sqrt(1-xseq^2))
      text(mycor[,xcomp]+sign(mycor[,xcomp])*0.25, mycor[,ycomp],
           labels=colnames(expo), cex=0.8,
           col=darken(annot.colours[annot_sub]),0.5)
    }
    par(mar = c(0,1,1,0))
    plot(0, 0, type = 'l', bty = 'n', xaxt = 'n', yaxt = 'n', xlab = "", ylab = "")
    legend("left", col=annot.colours[unique(annot_sub)],
           legend=names(annot.colours[unique(annot_sub)]),
           ncol = ceiling(length(annot.colours[unique(annot_sub)])/25),
           lty = 1, lwd = 2, bty = "n", cex = 1.5)
    dev.off()
  }
}
