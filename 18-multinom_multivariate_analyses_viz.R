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
myfiles = list.files(path = "../Results/Comorbidity/", pattern = "Multivariate_cluster_outcome_expo_f[0-4]_recalib_beta_*")
for (i in 1:length(myfiles)){
  split = gsub("Multivariate_cluster_outcome_expo_f[0-4]_recalib_beta_","", myfiles[i])
  data_type = gsub(".rds","",split)
  model_type = regmatches(myfiles[i], regexpr("Multivariate_cluster_outcome_expo_f[0-4]", myfiles[i]))
  
  pvals = readRDS(paste0("../Results/Comorbidity/",gsub("Multi","Uni",model_type),"_pvals_",data_type,".rds"))
  bonf = 0.05/ncol(pvals)
  x = -log10(pvals)
  
  selprop = readRDS(paste0("../Results/Comorbidity/",gsub("recalib_beta","selprop",myfiles[i])))
  recalib = readRDS(paste0("../Results/Comorbidity/",myfiles[i]))
  perf = readRDS(paste0("../Results/Comorbidity/",gsub("recalib_beta","performance",myfiles[i])))
  
  f = gsub("Multivariate_cluster_outcome_expo_f", "", model_type)
  if(f == 1){
    formula = "Model 1 (Adjusted for age + gender smoking + diet)"
    z = 7
  }
  if(f == 3){
    formula = "Model 2 (Model 1 + education)"
    z = 9
  }
  if(f == 4){
    formula = "Model 3 (Model 2 + physical activity + alcohol)"
    z = 13
  }
  
  y = exp(recalib[[1]][,-c(1:z)])
  yui = exp(recalib[[1]][,-c(1:z)]+qnorm(0.975)*recalib[[2]][,-c(1:z)])
  yli = exp(recalib[[1]][,-c(1:z)]-qnorm(0.975)*recalib[[2]][,-c(1:z)])
  
  param = readRDS(paste0("../Results/Comorbidity/Multivariate_cluster_outcome_expo_param_summary.rds"))
  pi = param[gsub(".rds","", myfiles[i]) %>% gsub("_recalib_beta_","_", .),2]
  
  all(names(selprop) == colnames(pvals))
  
  annot_sub = annot[names(selprop)]
  mycolours = rep(annot.colours[annot_sub],each=nrow(pvals))
  
  annot_sub = annot[names(sort(-selprop))]
  mycolours2 = rep(annot.colours[annot_sub],each=nrow(pvals))
  
  cluster.colours = c("#a1a5a7","#93ceba","#9b0058","#994517","#000000") 
  
  condition  = rep(selprop,each=nrow(pvals)) >= pi
  out_sub = rep(21:24,times=ncol(pvals))
  out_sub2 = rep(1:4,times=ncol(pvals))
  
  # unit = ifelse(sum(condition)>0,10,1)
  # legcol = ifelse(sum(condition)>0,ceiling(sum(condition)/25),1)
  # width_extra = ceiling(sum(condition)/25)*2
  
  # ## Check consistency of beta coefficients
  # print(all(sign(betas[selprop >= pi]) == sign(betas_m[names(betas[selprop >= pi])])))
  
  pdf(paste0("../Figures/Comorbidity/",model_type,"_",data_type,".pdf"), width = 9, height = 5)
  layout(matrix(c(1,2,3), nrow = 1, ncol = 3))
  par(mar=c(5,5,4,1), pty="s", xpd=FALSE)
  plot(1, type = "n", xaxt = "n", yaxt = "n", xlab = "", ylab = "")
  rect(par("usr")[1], par("usr")[3],
       par("usr")[2], par("usr")[4],
       col = "#f7f7f7")
  par(new = TRUE)
  plot(x, rep(selprop,each=nrow(pvals)), pch=out_sub,
       bg=ifelse(condition, mycolours, "grey"),
       col=ifelse(condition, cluster.colours[-1][out_sub2], "grey"),
       cex = ifelse(condition, 2, 0.5),
       cex.lab=1.2,
       ylim = c(0, 1),
       xlim = c(0, max(c(-log10(bonf), x))),
       ylab="Selection Proportion",
       xlab=expression(-log[10](italic(p))),
       panel.first = c(abline(v=axTicks(1), col="grey", lty=3),
                       abline(h=axTicks(2), col="grey", lty=3)))
  for (k in 1:ncol(pvals)){
    if(selprop[k] >= pi){
      text(x[,k],selprop[k],labels = which(names(selprop)[k] == names(sort(-selprop[selprop >= pi]))),cex = 0.8)
    }
  }
  abline(h = pi, lty = 2, col = "darkred")
  abline(v= -log10(bonf), lty = 2, col = "darkred")
  coord = par("usr")
  text(coord[1], pi, "Selection proportion threshold", adj = c(0,1), col = "darkred", cex = 0.5)
  text(-log10(bonf), 0, "Bonferroni threshold", adj = c(1,1), srt = 270, col = "darkred", cex = 0.5)
  legend("topleft",
         inset = c(0,-0.2),
         pch = out_sub,
         legend = paste0("Cluster ", rownames(pvals)),
         col = cluster.colours[-1],
         text.col = cluster.colours[-1],
         bty="n", xpd=NA, horiz=TRUE, cex=1, x.intersp=1)
  
  par(mar=c(5,5,4,1), xpd=FALSE, pty="s")
  xseq=0:ncol(y)+0.5
  xseq[1] = xseq[1]-0.25
  xseq[length(xseq)] = xseq[length(xseq)]+0.25
  xseq_in = rep(1:ncol(y)-0.6,each=nrow(y))+cumsum(rep(1/nrow(y),nrow(y)))
  plot(xseq_in,y,
       xlim=range(0:ncol(y)+0.5),
       ylim=c(0.1,10),
       type = "n",
       xaxt="n", yaxt="n",
       cex.lab=1.2,
       ylab="Beta Coefficient Recalibrated Model",
       xlab="",log = "y")
  coord = par("usr")
  background.colours = colorRampPalette(brewer.pal(n = 8, name = "RdPu")[-c(1:3)])(sum(selprop >= pi))
  for (k in 1:length(xseq)){
    polygon(x=c(xseq[k],xseq[k+1],xseq[k+1],xseq[k]),
            y=10^c(-30,-30,30,30), col=alpha(background.colours[k],0.3), border=NA)
    box()
  }
  par(new=TRUE)
  plotCI(xseq_in,y,
         li=yli, ui=yui, err="y",
         pch=out_sub,
         cex=2,
         pt.bg= mycolours2,
         col=cluster.colours[-1][out_sub2],
         add=TRUE)
  abline(v=xseq, lty = 3, col = "black")
  axis(side=2, at=axTicks(2))
  axis(side=1, at=1:length(y), labels=FALSE)
  text(1:ncol(y), 10^(coord[3]*1.1), labels=gsub("`","",colnames(y)),
       srt=30, adj=1, xpd=NA)
  abline(h=1, lty=2, col="darkred")
  text(xseq_in,y,labels = rep(1:ncol(y),each=nrow(y)),cex = 0.8)

  par(mar=c(5,5,4,1), xpd=FALSE, pty="s")
  xseq=0:ncol(perf)+0.5
  xseq[1] = xseq[1]-0.25
  xseq[length(xseq)] = xseq[length(xseq)]+0.25
  xseq_in = rep(1:ncol(perf)-0.6,each=nrow(perf))+cumsum(rep(1/nrow(perf),nrow(perf)))
  plot(1,
       xlim=range(0:ncol(perf)+0.5),
       ylim=c(0,1),
       type = "n",
       xaxt="n", yaxt="n",
       ylab="",
       xlab="")
  coord = par("usr")
  background.colours = colorRampPalette(brewer.pal(n = 8, name = "RdPu")[-c(1:3)])(sum(selprop >= pi))
  for (k in 1:length(xseq)){
    polygon(x=c(xseq[k],xseq[k+1],xseq[k+1],xseq[k]),
            y=c(-30,-30,30,30), col=alpha(background.colours[k],0.3), border=NA)
    box()
  }
  par(new=TRUE)
  plot(xseq_in,perf,
       xlim=range(0:ncol(perf)+0.5),
       ylim=c(0,1),
       type="h",
       lwd=3,
       cex.lab=1.2,
       col=cluster.colours,
       xaxt="n", yaxt="n",
       ylab="F1 Score",xlab="")
  abline(v=xseq, lty = 3, col = "black")
  axis(side=2, at=axTicks(2))
  axis(side=1, at=1:ncol(perf), labels=FALSE)
  coord = par("usr")
  text(1:ncol(perf), coord[3]-0.05, labels=paste0("+ ",gsub("`","",colnames(y))),
       srt=30, adj=1, xpd=NA)
  legend("topright",
         inset = c(0,-0.4),
         lty = 1,
         lwd = 3,
         legend = c("No disease", paste0("Cluster ",rownames(pvals))),
         col = cluster.colours,
         text.col = cluster.colours,
         bty="n", xpd=NA, cex=1, x.intersp=1)
  mtext(formula, side = 3, line = -3, font = 2, outer = TRUE, cex = 1.5)
  dev.off()
}
