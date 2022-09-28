## Visualisation of univariate analyses

rm(list=ls())
project_path=dirname(rstudioapi::getActiveDocumentContext()$path)
setwd(project_path)

library(plotrix)

source("graph_colours.R")
library(colorspace)

## Base model vs Adjusted model ----
annot = readRDS("../Data/Compound_family_annotation.rds")

filepaths = c("PesticidesPCBBDEs_train","PAHs_train","Overlap_test")
for (i in 1:3){
  pvals0 = readRDS(paste0("../Results/",filepaths[i],"_univariate_base_pvals.rds"))
  betas0 = readRDS(paste0("../Results/",filepaths[i],"_univariate_base_betas.rds"))
  se0 = readRDS(paste0("../Results/",filepaths[i],"_univariate_base_se.rds"))
  
  pvals1 = readRDS(paste0("../Results/",filepaths[i],"_univariate_adjust_pvals.rds"))
  betas1 = readRDS(paste0("../Results/",filepaths[i],"_univariate_adjust_betas.rds"))
  se1 = readRDS(paste0("../Results/",filepaths[i],"_univariate_adjust_se.rds"))
  
  outcome =  sub("_", " ", rownames(pvals0))
  annot_sub = annot[colnames(betas0)]
  bonf = 0.05/ncol(pvals0)
  bonf = -log10(bonf)
  
  for (k in 1:nrow(pvals0)){
    print(outcome[k])
    
    x1 = -log10(pvals0[k,])
    y1 = -log10(pvals1[k,])
    
    x2 = exp(betas0[k,])
    y2 = exp(betas1[k,])
    
    xui = exp(betas0[k,]+qnorm(0.975)*se0[k,])
    xli = exp(betas0[k,]-qnorm(0.975)*se0[k,])
    yui = exp(betas1[k,]+qnorm(0.975)*se1[k,])
    yli = exp(betas1[k,]-qnorm(0.975)*se1[k,])
   
    pdf(paste0("../Figures/Univariate_base_vs_adjust_",rownames(pvals0)[k],"_",filepaths[i],".pdf"), width = 9, height = 5)
    par(mar=c(5,5,4,1), mfrow=c(1,2), pty="s", xpd=FALSE)
    plot(1, type = "n", xaxt = "n", yaxt = "n", xlab = "", ylab = "")
    rect(par("usr")[1], par("usr")[3],
         par("usr")[2], par("usr")[4],
         col = "#f7f7f7")
    par(new = TRUE)
    plot(x1, y1,
         pch=19,
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
    text(x = bonf, y = bonf,labels="Bonferroni threshold", adj = c(1,0),col = outcome.colours[k], cex = 0.5)
    abline(h = bonf, lty = 2, col = darken(outcome.colours[k], 0.5))
    text(x = bonf, y = bonf, labels="Bonferroni threshold", adj = c(0,1),col = outcome.colours[k], cex = 0.5, srt = 270)
    abline(v = bonf, lty = 2, col = darken(outcome.colours[k], 0.5))
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
    plot(1, type = "n", xaxt = "n", yaxt = "n", xlab = "", ylab = "")
    rect(par("usr")[1], par("usr")[3],
         par("usr")[2], par("usr")[4],
         col = "#f7f7f7")
    par(new = TRUE)
    plotCI(x2, y2,
           li=ifelse(x1 > bonf | y1 > bonf, xli, NA),
           ui=ifelse(x1 > bonf | y1 > bonf, xui, NA),
           err="x",
           pch=19,
           cex=ifelse(x1 > bonf | y1 > bonf, 3, 0.5),
           col=ifelse(x1 > bonf | y1 > bonf, annot.colours[annot_sub], "grey"),
           xlim=xlim, ylim=ylim,
           cex.lab=1.2,
           xlab="Odds Ratio Model 1",
           ylab="Odds Ratio Model 2")
    abline(v=axTicks(1), col="grey", lty=3)
    abline(h=axTicks(2), col="grey", lty=3)
    abline(0, 1, col="grey", lty=3)
    plotCI(x2, y2,
           li=ifelse(x1 > bonf | y1 > bonf, yli, NA),
           ui=ifelse(x1 > bonf | y1 > bonf, yui, NA),
           err="y",
           pch=19,
           cex=ifelse(x1 > bonf | y1 > bonf, 3, 0.5),
           col=ifelse(x1 > bonf | y1 > bonf, annot.colours[annot_sub], "grey"),
           add=TRUE)
    abline(v = 1, lty = 2, col = darken(outcome.colours[k], 0.5))
    abline(h = 1, lty = 2, col = darken(outcome.colours[k], 0.5))
    for (j in 1:length(x1)){
      if(x1[j] > bonf| y1[j] > bonf){
        text(x2[j], y2[j],
             labels = which(names(x1)[j] == names(x1)[x1 > bonf| y1 > bonf | y1 < -bonf]),
             cex = 0.8)
      }
    }
    
    mtext(outcome[k], side = 3, line = -2, font = 2, outer = TRUE, cex = 1.5)
    if(sum(x1 > bonf | y1 > bonf)>0){
      legend("topright",
             inset = c(0,-0.1),
             legend = paste0(1:sum(x1 > bonf | y1 > bonf),". ",
                             names(x1)[x1 > bonf | y1 > bonf]),
             text.col = darken(annot.colours[annot_sub[names(x1)[x1 > bonf | y1 > bonf]]],0.5),
             bty="n", xpd=NA, horiz=TRUE, cex=0.8, x.intersp=0)
    }
    dev.off()
  }
}

## Sensitivity analysis: Comparison of adjusted model and model further adjusted for country ----
annot = readRDS("../Data/Compound_family_annotation.rds")

filepaths = c("PesticidesPCBBDEs_train","PAHs_train","Overlap_test")
for (i in 1:3){
  pvals0 = readRDS(paste0("../Results/",filepaths[i],"_univariate_adjust_pvals.rds"))
  betas0 = readRDS(paste0("../Results/",filepaths[i],"_univariate_adjust_betas.rds"))
  se0 = readRDS(paste0("../Results/",filepaths[i],"_univariate_adjust_se.rds"))
  
  pvals1 = readRDS(paste0("../Results/",filepaths[i],"_univariate_country_pvals.rds"))
  betas1 = readRDS(paste0("../Results/",filepaths[i],"_univariate_country_betas.rds"))
  se1 = readRDS(paste0("../Results/",filepaths[i],"_univariate_country_se.rds"))
  
  outcome =  sub("_", " ", rownames(pvals0))
  annot_sub = annot[colnames(betas0)]
  bonf = 0.05/ncol(pvals0)
  bonf = -log10(bonf)
  
  for (k in 1:nrow(pvals0)){
    print(outcome[k])
    
    x1 = -log10(pvals0[k,])
    y1 = -log10(pvals1[k,])
    
    x2 = exp(betas0[k,])
    y2 = exp(betas1[k,])
    
    xui = exp(betas0[k,]+qnorm(0.975)*se0[k,])
    xli = exp(betas0[k,]-qnorm(0.975)*se0[k,])
    yui = exp(betas1[k,]+qnorm(0.975)*se1[k,])
    yli = exp(betas1[k,]-qnorm(0.975)*se1[k,])

    pdf(paste0("../Figures/Univariate_adjust_vs_country_",rownames(pvals0)[k],"_",filepaths[i],".pdf"), width = 9, height = 5)
    par(mar=c(5,5,4,1), mfrow=c(1,2), pty="s", xpd=FALSE)
    plot(1, type = "n", xaxt = "n", yaxt = "n", xlab = "", ylab = "")
    rect(par("usr")[1], par("usr")[3],
         par("usr")[2], par("usr")[4],
         col = "#f7f7f7")
    par(new = TRUE)
    plot(x1, y1,
         pch=19,
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
    text(x = bonf, y = bonf,labels="Bonferroni threshold", adj = c(1,0),col = outcome.colours[k], cex = 0.5)
    abline(h = bonf, lty = 2, col = darken(outcome.colours[k], 0.5))
    text(x = bonf, y = bonf, labels="Bonferroni threshold", adj = c(0,1),col = outcome.colours[k], cex = 0.5, srt = 270)
    abline(v = bonf, lty = 2, col = darken(outcome.colours[k], 0.5))
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
    plot(1, type = "n", xaxt = "n", yaxt = "n", xlab = "", ylab = "")
    rect(par("usr")[1], par("usr")[3],
         par("usr")[2], par("usr")[4],
         col = "#f7f7f7")
    par(new = TRUE)
    plotCI(x2, y2,
           li=ifelse(x1 > bonf | y1 > bonf, xli, NA),
           ui=ifelse(x1 > bonf | y1 > bonf, xui, NA),
           err="x",
           pch=19,
           cex=ifelse(x1 > bonf | y1 > bonf, 3, 0.5),
           col=ifelse(x1 > bonf | y1 > bonf, annot.colours[annot_sub], "grey"),
           xlim=xlim, ylim=ylim,
           cex.lab=1.2,
           xlab="Odds Ratio Model 2",
           ylab="Odds Ratio Model 3")
    abline(v=axTicks(1), col="grey", lty=3)
    abline(h=axTicks(2), col="grey", lty=3)
    abline(0, 1, col="grey", lty=3)
    plotCI(x2, y2,
           li=ifelse(x1 > bonf | y1 > bonf, yli, NA),
           ui=ifelse(x1 > bonf | y1 > bonf, yui, NA),
           err="y",
           pch=19,
           cex=ifelse(x1 > bonf | y1 > bonf, 3, 0.5),
           col=ifelse(x1 > bonf | y1 > bonf, annot.colours[annot_sub], "grey"),
           add=TRUE)
    abline(v = 1, lty = 2, col = darken(outcome.colours[k], 0.5))
    abline(h = 1, lty = 2, col = darken(outcome.colours[k], 0.5))
    for (j in 1:length(x1)){
      if(x1[j] > bonf| y1[j] > bonf){
        text(x2[j], y2[j],
             labels = which(names(x1)[j] == names(x1)[x1 > bonf| y1 > bonf | y1 < -bonf]),
             cex = 0.8)
      }
    }
    
    mtext(outcome[k], side = 3, line = -2, font = 2, outer = TRUE, cex = 1.5)
    if(sum(x1 > bonf | y1 > bonf)>0){
      legend("topright",
             inset = c(0,-0.1),
             legend = paste0(1:sum(x1 > bonf | y1 > bonf),". ",
                             names(x1)[x1 > bonf | y1 > bonf]),
             text.col = darken(annot.colours[annot_sub[names(x1)[x1 > bonf | y1 > bonf]]],0.5),
             bty="n", xpd=NA, horiz=TRUE, cex=0.8, x.intersp=0)
    }
    dev.off()
  }
}
     