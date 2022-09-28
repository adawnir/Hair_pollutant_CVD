## Visulisation of stability-based LASSO logistic regression

rm(list=ls())
project_path=dirname(rstudioapi::getActiveDocumentContext()$path)
setwd(project_path)

source("graph_colours.R")
library(colorspace)
library(tidyverse)
library(sharp)
library(plotrix)
library(RColorBrewer)

annot = readRDS("../Data/Compound_family_annotation.rds")

## Compare results from multivariate analyses to univariate analyses ----
myfiles = list.files(path = "../Results", pattern = "Multivariate_analysis_adjust_output_*")
for (i in 1:length(myfiles)){
  split = gsub("Multivariate_analysis_adjust_output_","", myfiles[i])
  outcome_type = gsub("_PesticidesPCBBDEs_train.rds|_PAHs_train.rds","", split)
  data_type = gsub(paste0(outcome_type,"_"),"",split) %>% gsub(".rds","",.)

  # betas = readRDS(paste0("../Results/",data_type,"_univariate_adjust_betas.rds"))
  # betas = betas[outcome_type,]
  pvals = readRDS(paste0("../Results/",data_type,"_univariate_adjust_pvals.rds"))
  pvals = pvals[outcome_type,]
  bonf = 0.05/length(pvals)
  x = -log10(pvals)
  
  selprop = readRDS(paste0("../Results/",gsub("output","selprop",myfiles[i])))
  recalib = readRDS(paste0("../Results/",gsub("output","recalib_beta",myfiles[i])))
  perf = readRDS(paste0("../Results/",gsub("output","performance",myfiles[i])))
  
  y = exp(recalib[-c(1:7),1])
  names(y) = rownames(recalib)[-c(1:7)]
  
  yui = exp(recalib[-c(1:7),1]+qnorm(0.975)*recalib[-c(1:7),2])
  yli = exp(recalib[-c(1:7),1]-qnorm(0.975)*recalib[-c(1:7),2])
  
  # betas_m = readRDS(paste0("../Results/",gsub("output","beta",myfiles[i])))
  # betas_m = betas_m[-c(1:2)]
  # names(betas_m) = gsub("`","",names(betas_m))
  
  param = readRDS(paste0("../Results/Multivariate_analysis_adjust_param.rds"))
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
  
  pdf(paste0("../Figures/Multivariate_adjust_",outcome_type,"_",data_type,".pdf"), width = 9, height = 5)
  layout(matrix(c(1,1,2,3,3), nrow = 1, ncol = 5))
  par(mar=c(5,5,3,1), pty="s", xpd=FALSE)
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
  abline(h = pi, lty = 2, col = darken(outcome.colours[gsub("_"," ",outcome_type)],0.5))
  abline(v= -log10(bonf), lty = 2, col = darken(outcome.colours[gsub("_"," ",outcome_type)],0.5))
  coord = par("usr")
  text(coord[1], pi, "Selection proportion threshold", adj = c(0,1), col = darken(outcome.colours[gsub("_"," ",outcome_type)],0.5), cex = 0.5)
  text(-log10(bonf), 0, "Bonferroni threshold", adj = c(1,1), srt = 270, col = darken(outcome.colours[gsub("_"," ",outcome_type)],0.5), cex = 0.5)
  
  par(mar=c(9,5,6,1), xpd=FALSE, pty="m")
  xseq=0:length(y)+0.5
  plot(1:length(y),y,
       xlim=range(0:length(y)+0.5),
       ylim=c(0.1,10),
       xaxt="n", yaxt="n",
       cex.lab=1.2,
       ylab="Odds Ratio Recalibrated Model",
       xlab="",
       xaxt="n",log="y")
  coord = par("usr")
  background.colours = brewer.pal(n = 8, name = "RdPu")[-c(1:3)]
  for (k in 1:length(xseq)){
    polygon(x=c(xseq[k],xseq[k+1],xseq[k+1],xseq[k]),
            y=10^(c(-30,-30,30,30)), col=alpha(background.colours[k],0.3), border=NA)
    box()
  }
  plotCI(1:length(y),y, li=yli, ui=yui, err="y",
         pch=19,
         cex=3,
         col=mycolours2,
         add=TRUE)
  axis(side=2, at=axTicks(2))
  axis(side=1, at=1:length(y), labels=FALSE)
  text(1:length(y), 10^coord[3]-0.015, labels=gsub("`","",names(y)),
       srt=30, adj=1, xpd=NA)
  abline(h=1, lty=2, col=darken(outcome.colours[gsub("_"," ",outcome_type)],0.5))
  text(1:length(y),y,labels = 1:length(y),cex = 0.8)
  par(mar=c(5,5,3,1), pty="s", xpd=FALSE)
  plot(1, 1, type = "n",
       ylab="F1 Score", xlab="Threshold", cex.lab=1.2,
       ylim=c(0,1),xlim=c(0,1))
  if(length(y)==1){
    lines(perf, col = background.colours[1])
    text(perf[which.max(perf[,2]),1],max(perf[,2],na.rm=T)+0.05,
         labels=round(max(perf[,2],na.rm=T),2),cex=0.8)
  } else{
    for (k in 1:length(perf)){
      lines(perf[[k]], col = background.colours[k])
    }
    max.list = round(unlist(lapply(perf,function(x) max(x[,2],na.rm=T))),2)
    for (j in which(max.list==max(max.list))){
      abline(v=perf[[j]][which.max(perf[[j]][,2]),1], lty = 2, col=background.colours[j])
      abline(h=max(perf[[j]][,2],na.rm=T), lty=2, col=background.colours[j])
    }
    legend("topright", legend = paste0("+ ",gsub("`","",names(y)),": F1=",max.list),
           lty = 1, col = background.colours[1:length(perf)], bg="white", cex=0.8)
  }
  mtext(gsub("_"," ",outcome_type), side = 3, line = -3, font = 2, outer = TRUE, cex = 1.5)
  dev.off()
}
