## Visulisation of stability-based LASSO logistic regression

rm(list=ls())
project_path=dirname(rstudioapi::getActiveDocumentContext()$path)
setwd(project_path)

source("graph_colours.R")
library(colorspace)
library(tidyverse)
library(sharp)
library(pROC)

annot = readRDS("../Data/Compound_family_annotation.rds")

## Compare results from multivariate analyses to univariate analyses ----
myfiles = list.files(path = "../Results", pattern = "Multivariate_analysis_output_*")
for (i in 1:length(myfiles)){
  split = strsplit(myfiles[i], "_(?!.*_)", perl=TRUE)[[1]]
  data_type = split[2] %>% gsub(".rds","",.)
  outcome_path = gsub("Multivariate_analysis_output_","",split[1])
  outcome_type = gsub("_"," ", outcome_path)
  
  betas = readRDS(paste0("../Results/",data_type,"_univariate_age_gender_betas.rds"))
  betas = betas[outcome_type,]
  pvals = readRDS(paste0("../Results/",data_type,"_univariate_age_gender_pvals.rds"))
  pvals = pvals[outcome_type,]
  bonf = 0.05/length(pvals)
  x = sign(betas)*-log10(pvals)
    
  selprop = readRDS(paste0("../Results/",gsub("output","selprop",myfiles[i])))
  betas_m = readRDS(paste0("../Results/",gsub("output","beta",myfiles[i])))
  betas_m = betas_m[-c(1:2)]
  names(betas_m) = gsub("`","",names(betas_m))
  param = readRDS(paste0("../Results/Multivariate_analysis_param_",data_type,".rds"))
  pi = param[outcome_type,2]
  
  all(names(selprop) == names(pvals))
  
  annot_sub = annot[names(selprop)]
  mycolours = annot.colours[annot_sub]
  
  condition  = pvals < bonf | selprop >= pi
  
  unit = ifelse(sum(condition)>0,10,1)
  legcol = ifelse(sum(condition)>0,ceiling(sum(condition)/25),1)
  width_extra = ceiling(sum(condition)/25)*2

  ## Check consistency of beta coefficients
  print(all(sign(betas[selprop >= pi]) == sign(betas_m[names(betas[selprop >= pi])])))
  
  pdf(paste0("../Figures/Multivariate_vs_univariate_",outcome_path,"_",data_type,".pdf"), 5+width_extra, height = 5.5)
  par(mar=c(5,5,3,unit*legcol))
  plot(x, selprop, pch=19,
       col=ifelse(condition, mycolours, "grey"),
       cex.lab=1.5, cex = 3,
       main = outcome_type,
       ylim = c(0, 1),
       xlim = c(min(log10(bonf),x), max(c(-log10(bonf), x))),
       ylab="Selection Proportion",
       xlab=expression(Signed~-log[10](p)))
  
  for (k in 1:length(pvals)){
    if(pvals[k] < bonf|selprop[k] >= pi){
      text(x[k],selprop[k],labels = which(names(pvals)[k] == names(pvals)[condition]),cex = 0.8)
    }
  }
  abline(h = pi, lty = 2, col = outcome.colours[outcome_type])
  abline(v= -log10(bonf), lty = 2, col = outcome.colours[outcome_type])
  abline(v= log10(bonf), lty = 2, col = outcome.colours[outcome_type])
  coord = par("usr")
  text(coord[1]+abs(coord[1])*0.1, pi, "Selection proportion threshold", adj = c(0,1), col = outcome.colours[outcome_type], cex = 0.8)
  text(-log10(bonf), 0, "Bonferroni threshold", adj = c(1,1), srt = 270, col = outcome.colours[outcome_type], cex = 0.8)
  text(log10(bonf), 0, "Bonferroni threshold", adj = c(0,1), srt = 90, col = outcome.colours[outcome_type], cex = 0.8)
  if(sum(condition)>0){
    par(xpd = TRUE)
    legend(x = coord[2], y = coord[4],
           legend = paste0(1:sum(condition),". ", gsub("\\+","\\+\n",names(pvals)[condition])),
           text.col = darken(mycolours[condition],0.5),
           bty = "n",  x.intersp = 0)
  }
  dev.off()
}

## Compare predictive performance between outcomes per data set ----
myfiles = list.files(path = "../Results", pattern = "Multivariate_analysis_ROC_*")
myfiles = myfiles[-7] # exclude empty model
filepaths = c("PesticidesPCBBDEs","PAHs","Combined")
title = c("POPs and non-persistent pesticides","Tobacco-related chemicals and PAH metabolites",
          "Combined")
for (i in 1:3){
  myfiles_sub = myfiles[grepl(filepaths[i],myfiles)]
  pdf(paste0("../Figures/Multivariate_analysis_ROC_",filepaths[i],".pdf"), width = 5, height = 5.5)
  par(mar=c(5,5,3,1))
  plot(0, type = "n",
       ylim = c(0,1),
       xlim = c(0,1),
       xlab = "False Positive Rate", ylab = "True Positive Rate",
       main = title[i], cex.main = 1)
  myauc = NULL
  for (k in 1:length(myfiles_sub)){
    myroc = readRDS(paste0("../Results/",myfiles_sub[k]))
    myauc = c(myauc, round(as.numeric(myroc$auc),2))
    coords = pROC::coords(myroc, methods = "all")[,-1]
    coords[,1] = 1-coords[,1]
    
    split = strsplit(myfiles_sub[k], "_(?!.*_)", perl=TRUE)[[1]]
    outcome_type = gsub("Multivariate_analysis_ROC_","",split[1]) %>% gsub("_"," ",.)
    lines(coords, col = outcome.colours[outcome_type])
  }
  split = sapply(myfiles_sub, function(x) strsplit(x, "_(?!.*_)", perl=TRUE)[[1]])
  outcome_type = gsub("Multivariate_analysis_ROC_","",split[1,]) %>% gsub("_"," ",.)
  label = outcome_type[match(names(outcome.colours), outcome_type, nomatch = 0)]
  myauc = myauc[match(names(outcome.colours), outcome_type, nomatch = 0)]
  abline(0,1, lty = 3, col = "grey")
  legend("bottomright",
         legend = c(paste0(label,": AUC = ",myauc)),
         lty = 1, col = outcome.colours[label], cex = 0.8)
  dev.off()
}

