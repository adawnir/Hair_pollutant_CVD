## Visulisation of stability-based LASSO logistic regression

rm(list=ls())
project_path=dirname(rstudioapi::getActiveDocumentContext()$path)
setwd(project_path)
setwd("../")

source("graph_colours.R")
library(colorspace)
library(tidyverse)
library(sharp)
library(plotrix)
library(RColorBrewer)

annot = readRDS("../Data/Compound_family_annotation.rds")

## Compare results from multivariate analyses to univariate analyses ----
myfiles = list.files(path = "../Results/Multimorbidity/", pattern = "Multivariate_analysis_adjust_output_*")
for (i in 1:length(myfiles)){
  split = gsub("Multivariate_analysis_adjust_output_","", myfiles[i])
  data_type =gsub(".rds","",split)

  # betas = readRDS(paste0("../Results/",data_type,"_univariate_adjust_betas.rds"))
  # betas = betas[outcome_type,]
  pvals = readRDS(paste0("../Results/Multimorbidity/",data_type,"_univariate_adjust_pvals.rds"))
  bonf = 0.05/length(pvals)
  x = -log10(pvals)
  
  selprop = readRDS(paste0("../Results/Multimorbidity/",gsub("output","selprop",myfiles[i])))
  recalib = readRDS(paste0("../Results/Multimorbidity/",gsub("output","recalib_beta",myfiles[i])))
  perf = readRDS(paste0("../Results/Multimorbidity/",gsub("output","performance",myfiles[i])))
  
  y = recalib[-c(1:7),1]
  names(y) = rownames(recalib)[-c(1:7)]
  
  yui = recalib[-c(1:7),1]+qnorm(0.975)*recalib[-c(1:7),2]
  yli = recalib[-c(1:7),1]-qnorm(0.975)*recalib[-c(1:7),2]
  
  # betas_m = readRDS(paste0("../Results/",gsub("output","beta",myfiles[i])))
  # betas_m = betas_m[-c(1:2)]
  # names(betas_m) = gsub("`","",names(betas_m))
  
  param = readRDS(paste0("../Results/Multimorbidity/Multivariate_analysis_adjust_param.rds"))
  pi = param[gsub(".rds","", myfiles[i]) %>% gsub("_output_","_", .),2]
  
  all(names(selprop) == names(pvals))
  
  annot_sub = annot[names(selprop)]
  mycolours = annot.colours[annot_sub]
  
  annot_sub = annot[names(sort(-selprop))]
  mycolours2 = annot.colours[annot_sub]
  
  condition  = selprop >= pi
  
  # unit = ifelse(sum(condition)>0,10,1)
  # legcol = ifelse(sum(condition)>0,ceiling(sum(condition)/25),1)
  # width_extra = ceiling(sum(condition)/25)*2
  
  # ## Check consistency of beta coefficients
  # print(all(sign(betas[selprop >= pi]) == sign(betas_m[names(betas[selprop >= pi])])))
  
  pdf(paste0("../Figures/Multimorbidity/Multivariate_adjust_",data_type,".pdf"), 6, height = 4)
  layout(matrix(c(1,1,2), nrow = 1, ncol = 3))
  par(mar=c(5,5,1,1), pty="s", xpd=FALSE)
  plot(x, selprop, pch=19,
       col=ifelse(condition, mycolours, "grey"),
       cex = ifelse(condition, 3, 1),
       cex.lab=1.2,
       ylim = c(0, 1),
       xlim = c(0, max(c(-log10(bonf), x))),
       ylab="Selection Proportion",
       xlab=expression(-log[10](italic(p))),
       panel.first = c(abline(v=axTicks(1), col="grey", lty=3),
                       abline(h=axTicks(2), col="grey", lty=3)))
  
  for (k in 1:length(pvals)){
    if(selprop[k] >= pi){
      text(x[k],selprop[k],labels = which(names(selprop)[k] == names(sort(-selprop[condition]))),cex = 0.8)
    }
  }
  abline(h = pi, lty = 2, col = "darkred")
  abline(v= -log10(bonf), lty = 2, col = "darkred")
  coord = par("usr")
  text(coord[1], pi, "Selection proportion threshold", adj = c(0,1), col = "darkred", cex = 0.5)
  text(-log10(bonf), 0, "Bonferroni threshold", adj = c(1,1), srt = 270, col = "darkred", cex = 0.5)
  
  par(mar=c(9,5,1,1), xpd=FALSE, pty="m")
  xseq=0:length(y)+0.5
  plot(1:length(y),y,
       xlim=range(0:length(y)+0.5),
       ylim=c(-0.5,0.5),
       xaxt="n", yaxt="n",
       cex.lab=1.2,
       ylab="Beta Coefficient Recalibrated Model",
       xlab="",
       xaxt="n")
  coord = par("usr")
  background.colours = brewer.pal(n = 8, name = "RdPu")[-c(1:3)]
  for (k in 1:length(xseq)){
    polygon(x=c(xseq[k],xseq[k+1],xseq[k+1],xseq[k]),
            y=c(-30,-30,30,30), col=alpha(background.colours[k],0.3), border=NA)
    box()
  }
  plotCI(1:length(y),y, li=yli, ui=yui, err="y",
         pch=19,
         cex=3,
         col=mycolours2,
         add=TRUE)
  axis(side=2, at=axTicks(2))
  axis(side=1, at=1:length(y), labels=FALSE)
  text(1:length(y), coord[3]-0.05, labels=gsub("`","",names(y)),
       srt=30, adj=1, xpd=NA)
  abline(h=0, lty=2, col="darkred")
  text(1:length(y),y,labels = 1:length(y),cex = 0.8)
  text(1:length(y), yui, adj=c(0.5,0), srt=30,
       labels=paste0("RMSE=",round(perf,2)),cex=0.8)
  dev.off()
}
