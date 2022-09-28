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
for (i in 1:3){
  pvals0 = readRDS(paste0("../Results/Multimorbidity/",filepaths[i],"_univariate_base_pvals.rds"))
  betas0 = readRDS(paste0("../Results/Multimorbidity/",filepaths[i],"_univariate_base_betas.rds"))
  se0 = readRDS(paste0("../Results/Multimorbidity/",filepaths[i],"_univariate_base_se.rds"))
  
  pvals1 = readRDS(paste0("../Results/Multimorbidity/",filepaths[i],"_univariate_adjust_pvals.rds"))
  betas1 = readRDS(paste0("../Results/Multimorbidity/",filepaths[i],"_univariate_adjust_betas.rds"))
  se1 = readRDS(paste0("../Results/Multimorbidity/",filepaths[i],"_univariate_adjust_se.rds"))
  
  annot_sub = annot[names(betas0)]
  bonf = 0.05/length(pvals0)
  bonf = -log10(bonf)
  x1 = -log10(pvals0)
  y1 = -log10(pvals1)
  
  x2 = betas0
  y2 = betas1
  
  xui = betas0+qnorm(0.975)*se0
  xli = betas0-qnorm(0.975)*se0
  yui = betas1+qnorm(0.975)*se1
  yli = betas1-qnorm(0.975)*se1
  
  ifelse(dir.exists("../Figures/Multimorbidity"),"",dir.create("../Figures/Multimorbidity"))
  pdf(paste0("../Figures/Multimorbidity/Univariate_base_vs_adjust_",filepaths[i],".pdf"), width = 9, height = 5)
  par(mar=c(5,5,4,1), mfrow=c(1,2), pty="s", xpd=FALSE)
  plot(x1, y1,
       pch=19,
       cex=ifelse(x1 > bonf | y1 > bonf, 3, 1),
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
           labels = which(names(x1)[j] == names(x1)[x1 > bonf| y1 > bonf | y1 < -bonf]),
           cex = 0.8)
    }
  }
  xlim=c(min(xli),max(xui))
  ylim=c(min(yli),max(yui))
  par(xpd=FALSE)
  plotCI(x2, y2, li=xli, ui=xui, err="x",
         pch=19,
         cex=ifelse(x1 > bonf | y1 > bonf, 3, 1),
         col=ifelse(x1 > bonf | y1 > bonf, annot.colours[annot_sub], "grey"),
         xlim=xlim, ylim=ylim,
         cex.lab=1.2,
         xlab="Beta Coefficient Model 1",
         ylab="Beta Coefficient Model 2")
  abline(v=axTicks(1), col="grey", lty=3)
  abline(h=axTicks(2), col="grey", lty=3)
  abline(0, 1, col="grey", lty=3)
  plotCI(x2, y2, li=yli, ui=yui, err="y",
         pch=19,
         cex=ifelse(x1 > bonf | y1 > bonf, 3, 1),
         col=ifelse(x1 > bonf | y1 > bonf, annot.colours[annot_sub], "grey"),
         add=TRUE)
  abline(v = 0, lty = 2, col = "darkred")
  abline(h = 0, lty = 2, col = "darkred")
  for (j in 1:length(x1)){
    if(x1[j] > bonf| y1[j] > bonf){
      text(x2[j], y2[j],
           labels = which(names(x1)[j] == names(x1)[x1 > bonf| y1 > bonf | y1 < -bonf]),
           cex = 0.8)
    }
  }
  if(sum(x1 > bonf | y1 > bonf)>0){
    legend("top",
           adj = c(1,0),
           inset = c(0,-0.1),
           legend = paste0(1:sum(x1 > bonf | y1 > bonf),". ",
                           names(x1)[x1 > bonf | y1 > bonf]),
           text.col = darken(annot.colours[annot_sub[names(x1)[x1 > bonf | y1 > bonf]]],0.5),
           bty="n", xpd=NA, horiz=TRUE, cex=0.8, x.intersp=0)
  }
  dev.off()
}

## Sensitivity analysis: Comparison of adjusted model and model further adjusted for country ----

for (i in 1:3){
  pvals0 = readRDS(paste0("../Results/Multimorbidity/",filepaths[i],"_univariate_adjust_pvals.rds"))
  betas0 = readRDS(paste0("../Results/Multimorbidity/",filepaths[i],"_univariate_adjust_betas.rds"))
  se0 = readRDS(paste0("../Results/Multimorbidity/",filepaths[i],"_univariate_adjust_se.rds"))
  
  pvals1 = readRDS(paste0("../Results/Multimorbidity/",filepaths[i],"_univariate_country_pvals.rds"))
  betas1 = readRDS(paste0("../Results/Multimorbidity/",filepaths[i],"_univariate_country_betas.rds"))
  se1 = readRDS(paste0("../Results/Multimorbidity/",filepaths[i],"_univariate_country_se.rds"))
  
  annot_sub = annot[names(betas0)]
  bonf = 0.05/length(pvals0)
  bonf = -log10(bonf)
  x1 = -log10(pvals0)
  y1 = -log10(pvals1)
  
  x2 = betas0
  y2 = betas1
  
  xui = betas0+qnorm(0.975)*se0
  xli = betas0-qnorm(0.975)*se0
  yui = betas1+qnorm(0.975)*se1
  yli = betas1-qnorm(0.975)*se1
  
  ifelse(dir.exists("../Figures/Multimorbidity"),"",dir.create("../Figures/Multimorbidity"))
  pdf(paste0("../Figures/Multimorbidity/Univariate_adjust_vs_country_",filepaths[i],".pdf"), width = 9, height = 5)
  par(mar=c(5,5,4,1), mfrow=c(1,2), pty="s", xpd=FALSE)
  plot(x1, y1,
       pch=19,
       cex=ifelse(x1 > bonf | y1 > bonf, 3, 1),
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
           labels = which(names(x1)[j] == names(x1)[x1 > bonf| y1 > bonf | y1 < -bonf]),
           cex = 0.8)
    }
  }
  xlim=c(min(xli),max(xui))
  ylim=c(min(yli),max(yui))
  par(xpd=FALSE)
  plotCI(x2, y2, li=xli, ui=xui, err="x",
         pch=19,
         cex=ifelse(x1 > bonf | y1 > bonf, 3, 1),
         col=ifelse(x1 > bonf | y1 > bonf, annot.colours[annot_sub], "grey"),
         xlim=xlim, ylim=ylim,
         cex.lab=1.2,
         xlab="Beta Coefficient Model 2",
         ylab="Beta Coefficient Model 3")
  abline(v=axTicks(1), col="grey", lty=3)
  abline(h=axTicks(2), col="grey", lty=3)
  abline(0, 1, col="grey", lty=3)
  plotCI(x2, y2, li=yli, ui=yui, err="y",
         pch=19,
         cex=ifelse(x1 > bonf | y1 > bonf, 3, 1),
         col=ifelse(x1 > bonf | y1 > bonf, annot.colours[annot_sub], "grey"),
         add=TRUE)
  abline(v = 0, lty = 2, col = "darkred")
  abline(h = 0, lty = 2, col = "darkred")
  for (j in 1:length(x1)){
    if(x1[j] > bonf| y1[j] > bonf){
      text(x2[j], y2[j],
           labels = which(names(x1)[j] == names(x1)[x1 > bonf| y1 > bonf | y1 < -bonf]),
           cex = 0.8)
    }
  }
  if(sum(x1 > bonf | y1 > bonf)>0){
    legend("top",
           adj = c(1,0),
           inset = c(0,-0.1),
           legend = paste0(1:sum(x1 > bonf | y1 > bonf),". ",
                           names(x1)[x1 > bonf | y1 > bonf]),
           text.col = darken(annot.colours[annot_sub[names(x1)[x1 > bonf | y1 > bonf]]],0.5),
           bty="n", xpd=NA, horiz=TRUE, cex=0.8, x.intersp=0)
  }
  dev.off()
}
