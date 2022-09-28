rm(list=ls())
project_path=dirname(rstudioapi::getActiveDocumentContext()$path)
setwd(project_path)
setwd("../")

library(plotrix)

source("graph_colours.R")
library(colorspace)

annot = readRDS("../Data/Compound_family_annotation.rds")

filepaths = c("PesticidesPCBBDEs_train","PAHs_train","Overlap_test")

## Base model vs Adjusted model ----
for (i in 1:2){
  pvals0 = readRDS(paste0("../Results/Comorbidity/",filepaths[i],"_univariate_base_pvals.rds"))
  betas0 = readRDS(paste0("../Results/Comorbidity/",filepaths[i],"_univariate_base_betas.rds"))
  se0 = readRDS(paste0("../Results/Comorbidity/",filepaths[i],"_univariate_base_se.rds"))
  
  pvals1 = readRDS(paste0("../Results/Comorbidity/",filepaths[i],"_univariate_adjust_pvals.rds"))
  betas1 = readRDS(paste0("../Results/Comorbidity/",filepaths[i],"_univariate_adjust_betas.rds"))
  se1 = readRDS(paste0("../Results/Comorbidity/",filepaths[i],"_univariate_adjust_se.rds"))
  
  annot_sub = rep(annot[colnames(betas0)],each=nrow(betas0))
  # out_sub = rep(names(outcome.colours)[c(4,2,1,3,5)],times=ncol(betas0))
  out_sub = rep(c(19,17,21,18,15),times=ncol(betas0))
  bonf = 0.05/ncol(pvals0)
  bonf = -log10(bonf)
  x1 = -log10(pvals0)
  y1 = -log10(pvals1)
  
  x2 = betas0
  y2 = betas1
  
  xui = betas0+qnorm(0.975)*se0
  xli = betas0-qnorm(0.975)*se0
  yui = betas1+qnorm(0.975)*se1
  yli = betas1-qnorm(0.975)*se1
  
  cluster.colours = c("#a1a5a7","#93ceba","#9b0058","#994517","#0019a8","#000000")[-1]
  
  ifelse(dir.exists("../Figures/Comorbidity"),"",dir.create("../Figures/Comorbidity"))
  pdf(paste0("../Figures/Comorbidity/Univariate_base_vs_adjust_",filepaths[i],".pdf"), width = 9, height = 5)
  par(mar=c(5,5,4,1), mfrow=c(1,2), pty="s", xpd=FALSE)
  plot(1, type = "n", xaxt = "n", yaxt = "n", xlab = "", ylab = "")
  rect(par("usr")[1], par("usr")[3],
       par("usr")[2], par("usr")[4],
       col = "#f7f7f7")
  par(new = TRUE)
  plot(x1, y1,
       pch=out_sub,
       cex=ifelse(x1 > bonf | y1 > bonf, 3, 0.5),
       col=ifelse(x1 > bonf | y1 > bonf, annot.colours[annot_sub], "grey"),
       ylim = c(0, max(y1,bonf)+0.3),
       xlim = c(0, max(x1,bonf)+0.3),
       cex.lab=1.2,
       xlab=substitute(paste(-log[10](italic(p))," ",x), list(x = "Model 1")),
       ylab=substitute(paste(-log[10](italic(p))," ",x), list(x = "Model 2")),
       panel.first = c(abline(v=axTicks(1), col="grey", lty=3),
                       abline(h=axTicks(2), col="grey", lty=3),
                       abline(0, 1, col="grey", lty=3)))
  text(x = bonf, y = bonf,labels="Bonferroni threshold", adj = c(1,0),col = "darkred", cex = 0.5)
  abline(h = bonf, lty = 2, col = "darkred")
  text(x = bonf, y = bonf, labels="Bonferroni threshold", adj = c(0,1),col = "darkred", cex = 0.5, srt = 270)
  abline(v = bonf, lty = 2, col = "darkred")
  for (j in 1:length(x1)){
    if(x1[j] > bonf| y1[j] > bonf){
      text(x1[j], y1[j],
           labels = which(x1[j] == x1[x1 > bonf| y1 > bonf | y1 < -bonf]),
           cex = 0.8)
    }
  }
  xlim=c(min(xli),max(xui))
  ylim=c(min(yli),max(yui))
  par(xpd=FALSE)
  plot(1, type = "n", xaxt = "n", yaxt = "n", xlab = "", ylab = "")
  rect(par("usr")[1], par("usr")[3],
       par("usr")[2], par("usr")[4],
       col = "#f7f7f7")
  par(new = TRUE)
  plotCI(x2, y2,
         li=ifelse(x1 > bonf | y1 > bonf, xli, NA),
         ui=ifelse(x1 > bonf | y1 > bonf, xui, NA),
         err="x",
         pch=out_sub,
         cex=ifelse(x1 > bonf | y1 > bonf, 3, 0.5),
         col=ifelse(x1 > bonf | y1 > bonf, annot.colours[annot_sub], "grey"),
         xlim=xlim, ylim=ylim,
         cex.lab=1.2,
         xlab="Beta Coefficient Model 1",
         ylab="Beta Coefficient Model 2")
  abline(v=axTicks(1), col="grey", lty=3)
  abline(h=axTicks(2), col="grey", lty=3)
  abline(0, 1, col="grey", lty=3)
  plotCI(x2, y2,
         li=ifelse(x1 > bonf | y1 > bonf, yli, NA),
         ui=ifelse(x1 > bonf | y1 > bonf, yui, NA),
         err="y",
         pch=out_sub,
         cex=ifelse(x1 > bonf | y1 > bonf, 3, 0.5),
         col=ifelse(x1 > bonf | y1 > bonf, annot.colours[annot_sub], "grey"),
         add=TRUE)
  abline(v = 0, lty = 2, col = "darkred")
  abline(h = 0, lty = 2, col = "darkred")
  for (j in 1:length(x1)){
    if(x1[j] > bonf| y1[j] > bonf){
      text(x2[j], y2[j],
           labels = which(x1[j] == x1[x1 > bonf| y1 > bonf | y1 < -bonf]),
           cex = 0.8)
    }
  }
  if(sum(x1 > bonf | y1 > bonf)>0){
    legend("topright",
           inset = c(0,-0.1),
           legend = paste0(1:sum(x1 > bonf | y1 > bonf),". ",
                           colnames(x1)[which(x1 > bonf | y1 > bonf,arr.ind = T)[,2]]),
           text.col = darken(annot.colours[annot_sub[colnames(x1)[which(x1 > bonf | y1 > bonf,arr.ind = T)[,2]]]],0.5),
           bty="n", xpd=NA, horiz=TRUE, cex=0.8, x.intersp=0)
    legend("topright",
           inset = c(0,-0.2),
           pch = out_sub,
           legend = paste0("Cluster ", 2:6),
           col = cluster.colours,
           text.col = cluster.colours,
           bty="n", xpd=NA, horiz=TRUE, cex=0.8, x.intersp=1)
  }
  dev.off()
}

## Sensitivity analysis: Comparison of adjusted model and model further adjusted for country ----

for (i in 1:2){
  pvals0 = readRDS(paste0("../Results/Comorbidity/",filepaths[i],"_univariate_adjust_pvals.rds"))
  betas0 = readRDS(paste0("../Results/Comorbidity/",filepaths[i],"_univariate_adjust_betas.rds"))
  se0 = readRDS(paste0("../Results/Comorbidity/",filepaths[i],"_univariate_adjust_se.rds"))
  
  pvals1 = readRDS(paste0("../Results/Comorbidity/",filepaths[i],"_univariate_country_pvals.rds"))
  betas1 = readRDS(paste0("../Results/Comorbidity/",filepaths[i],"_univariate_country_betas.rds"))
  se1 = readRDS(paste0("../Results/Comorbidity/",filepaths[i],"_univariate_country_se.rds"))
  
  annot_sub = rep(annot[colnames(betas0)],each=nrow(betas0))
  # out_sub = rep(names(outcome.colours)[c(4,2,1,3,5)],times=ncol(betas0))
  out_sub = rep(c(19,17,21,18,15),times=ncol(betas0))
  bonf = 0.05/ncol(pvals0)
  bonf = -log10(bonf)
  x1 = -log10(pvals0)
  y1 = -log10(pvals1)
  
  x2 = betas0
  y2 = betas1
  
  xui = betas0+qnorm(0.975)*se0
  xli = betas0-qnorm(0.975)*se0
  yui = betas1+qnorm(0.975)*se1
  yli = betas1-qnorm(0.975)*se1
  
  cluster.colours = c("#a1a5a7","#93ceba","#9b0058","#994517","#0019a8","#000000")[-1]
  
  ifelse(dir.exists("../Figures/Comorbidity"),"",dir.create("../Figures/Comorbidity"))
  pdf(paste0("../Figures/Comorbidity/Univariate_adjust_vs_country_",filepaths[i],".pdf"), width = 9, height = 5)
  par(mar=c(5,5,4,1), mfrow=c(1,2), pty="s", xpd=FALSE)
  plot(1, type = "n", xaxt = "n", yaxt = "n", xlab = "", ylab = "")
  rect(par("usr")[1], par("usr")[3],
       par("usr")[2], par("usr")[4],
       col = "#f7f7f7")
  par(new = TRUE)
  plot(x1, y1,
       pch=out_sub,
       cex=ifelse(x1 > bonf | y1 > bonf, 3, 0.5),
       col=ifelse(x1 > bonf | y1 > bonf, annot.colours[annot_sub], "grey"),
       ylim = c(0, max(y1,bonf)+0.3),
       xlim = c(0, max(x1,bonf)+0.3),
       cex.lab=1.2,
       xlab=substitute(paste(-log[10](italic(p))," ",x), list(x = "Model 2")),
       ylab=substitute(paste(-log[10](italic(p))," ",x), list(x = "Model 3")),
       panel.first = c(abline(v=axTicks(1), col="grey", lty=3),
                       abline(h=axTicks(2), col="grey", lty=3),
                       abline(0, 1, col="grey", lty=3)))
  text(x = bonf, y = bonf,labels="Bonferroni threshold", adj = c(1,0),col = "darkred", cex = 0.5)
  abline(h = bonf, lty = 2, col = "darkred")
  text(x = bonf, y = bonf, labels="Bonferroni threshold", adj = c(0,1),col = "darkred", cex = 0.5, srt = 270)
  abline(v = bonf, lty = 2, col = "darkred")
  for (j in 1:length(x1)){
    if(x1[j] > bonf| y1[j] > bonf){
      text(x1[j], y1[j],
           labels = which(x1[j] == x1[x1 > bonf| y1 > bonf | y1 < -bonf]),
           cex = 0.8)
    }
  }
  xlim=c(min(xli),max(xui))
  ylim=c(min(yli),max(yui))
  par(xpd=FALSE)
  plot(1, type = "n", xaxt = "n", yaxt = "n", xlab = "", ylab = "")
  rect(par("usr")[1], par("usr")[3],
       par("usr")[2], par("usr")[4],
       col = "#f7f7f7")
  par(new = TRUE)
  plotCI(x2, y2,
         li=ifelse(x1 > bonf | y1 > bonf, xli, NA),
         ui=ifelse(x1 > bonf | y1 > bonf, xui, NA),
         err="x",
         pch=out_sub,
         cex=ifelse(x1 > bonf | y1 > bonf, 3, 0.5),
         col=ifelse(x1 > bonf | y1 > bonf, annot.colours[annot_sub], "grey"),
         xlim=xlim, ylim=ylim,
         cex.lab=1.2,
         xlab="Beta Coefficient Model 2",
         ylab="Beta Coefficient Model 3")
  abline(v=axTicks(1), col="grey", lty=3)
  abline(h=axTicks(2), col="grey", lty=3)
  abline(0, 1, col="grey", lty=3)
  plotCI(x2, y2,
         li=ifelse(x1 > bonf | y1 > bonf, yli, NA),
         ui=ifelse(x1 > bonf | y1 > bonf, yui, NA),
         err="y",
         pch=out_sub,
         cex=ifelse(x1 > bonf | y1 > bonf, 3, 0.5),
         col=ifelse(x1 > bonf | y1 > bonf, annot.colours[annot_sub], "grey"),
         add=TRUE)
  abline(v = 0, lty = 2, col = "darkred")
  abline(h = 0, lty = 2, col = "darkred")
  for (j in 1:length(x1)){
    if(x1[j] > bonf| y1[j] > bonf){
      text(x2[j], y2[j],
           labels = which(x1[j] == x1[x1 > bonf| y1 > bonf | y1 < -bonf]),
           cex = 0.8)
    }
  }
  if(sum(x1 > bonf | y1 > bonf)>0){
    legend("topright",
           inset = c(0,-0.1),
           legend = paste0(1:sum(x1 > bonf | y1 > bonf),". ",
                           colnames(x1)[which(x1 > bonf | y1 > bonf,arr.ind = T)[,2]]),
           text.col = darken(annot.colours[annot_sub[colnames(x1)[which(x1 > bonf | y1 > bonf,arr.ind = T)[,2]]]],0.5),
           bty="n", xpd=NA, horiz=TRUE, cex=0.8, x.intersp=0)
    legend("topright",
           inset = c(0,-0.2),
           pch = out_sub,
           legend = paste0("Cluster ", 2:6),
           col = cluster.colours,
           text.col = cluster.colours,
           bty="n", xpd=NA, horiz=TRUE, cex=0.8, x.intersp=1)
  }
  dev.off()
}
