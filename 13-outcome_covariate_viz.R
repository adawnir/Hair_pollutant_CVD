## Univariate analyses: association between exposures and covariates

rm(list=ls())
project_path=dirname(rstudioapi::getActiveDocumentContext()$path)
setwd(project_path)

library(tidyverse)
library(plotrix)
library(colorspace)
source("graph_colours.R")
filepaths = c("PesticidesPCBBDEs_train","PAHs_train","Overlap_test")

# Load data ----
i = 1

# Load data
pvals = readRDS(paste0("../Results/Univariate/Y_Z_pvals_",filepaths[i],".rds"))
betas = readRDS(paste0("../Results/Univariate/Y_Z_betas_",filepaths[i],".rds"))
se = readRDS(paste0("../Results/Univariate/Y_Z_se_",filepaths[i],".rds"))
colnames(betas)
bonf = 0.05/nrow(betas)

VocanoPlot = function(j, label, path, mycolour){
  pvals_sub = pvals[,j]
  betas_sub = betas[,j]
  names(betas_sub) = rep(rownames(betas),times = length(j))
  se_sub = se[,j]
  
  li = betas_sub-qnorm(0.975)*se_sub
  ui = betas_sub+qnorm(0.975)*se_sub
  condition = pvals_sub < bonf
  leg = ifelse(sum(condition)>0,10*ceiling(sum(condition)/25),1)
  wid = ifelse(sum(condition)>0,6+2*ceiling(sum(condition)/25),6)
  
  ifelse(dir.exists("../Figures/Univariate/"),"",dir.create("../Figures/Univariate/"))
  
  pdf(paste0("../Figures/Univariate/X_Z_",path,"_",filepaths[i],".pdf"), width = wid, height = 6)
  par(mar=c(5,5,5,leg), pty="s")
  plot(0,
       ylab=expression(-log[10](italic(p))),
       xlab=expression(beta),
       main=label,
       cex.lab=1.2,
       ylim = c(0, max(-log10(pvals_sub), bonf)*1.25),
       xlim = c(-1,1)*max(abs(c(li,ui)))*1.05,type = "n")
  abline(h=axTicks(2), col="grey", lty=3)
  abline(v=axTicks(1), col="grey", lty=3)
  plotCI(betas_sub, -log10(pvals_sub),
         li = ifelse(condition, li, betas_sub),
         ui = ifelse(condition, ui, betas_sub),
         err="x",
         pch=21,
         col=mycolour,
         pt.bg=ifelse(condition, annot.colours[annot_sub], "grey"),
         cex = ifelse(condition, 3, 1), add = T)
  abline(h = -log10(bonf), lty = 2, col = "darkred")
  coord = par("usr")
  text(coord[1], -log10(bonf),  labels = "Bonferroni threshold", adj=c(0,1), col = "darkred", cex = 0.8)
  if(length(j)>1){
    legend("topright",
           legend = gsub(path,"",colnames(betas)[j]),
           pch = 1,
           col = unique(mycolour), cex = 0.8)
  }
  if(sum(condition)>0){
    unique_names = names(betas_sub)
    unique_names = ifelse(duplicated(names(betas_sub)),
                          paste0(names(betas_sub),"*"),
                          names(betas_sub))
    for (k in 1:length(betas_sub)){
      if(pvals_sub[k] < bonf){
        text(betas_sub[k], -log10(pvals_sub[k]),
             labels = which(unique_names[k] == unique_names[condition]),
             cex = 0.8)
      }
    }
    par(xpd = TRUE)
    if(length(mycolour)>1){
      text.col = mycolour[pvals[,j]<bonf]
    } else{
      text.col = mycolour
    }
    legend(x = coord[2]*1.05, y = coord[4],
           legend = paste0(1:sum(condition),". ",
                           names(betas_sub)[condition]),
           text.col = text.col,
           bty = "n",  x.intersp = 0)
  }
  dev.off()
}

## Age ----
j = "Age"
label = "Age"
path = "Age"
mycolour ="#dc251f"
VocanoPlot(j, label, path, mycolour)

## Gender ----
j = "GenderFemale"
label = "Gender (ref. Male)"
path = "Gender"
mycolour ="#dc251f"
VocanoPlot(j, label, path, mycolour)

## Education ----
j = grep("Education",colnames(betas))
label = "Education (ref. High)"
path = "Education"
mycolour = rep(c("#dc251f","#994517"),each=nrow(betas))

VocanoPlot(j, label, path, mycolour)

## Smoking ----
j = grep("Smoking",colnames(betas))
label = "Smoking status (ref. Never smoker)"
path = "Smoking"
mycolour = rep(c("#dc251f","#994517"),each=nrow(betas))

VocanoPlot(j, label, path, mycolour)

## Diet ----
j = grep("DietScore",colnames(betas))
label = "Dietary intake"
path = "DietScore"
mycolour = rep(c("#ffcc00","#00722a", "#9b0058"),each=nrow(betas))

VocanoPlot(j, label, path, mycolour)

## Physical activity ----
j = grep("Physical_activity",colnames(betas))
label = "Physical activity (ref. Active)"
path = "Physical_activity"
mycolour = rep(c("#dc251f","#994517"),each=nrow(betas))

VocanoPlot(j, label, path, mycolour)

## Alcohol----
j = grep("Alcohol",colnames(betas))
label = "Alcohol consumption (ref. Low)"
path = "Alcohol"
mycolour = rep(c("#dc251f","#994517"),each=nrow(betas))

VocanoPlot(j, label, path, mycolour)
