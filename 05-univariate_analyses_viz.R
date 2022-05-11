## Visualisation of univariate analyses

rm(list=ls())
project_path=dirname(rstudioapi::getActiveDocumentContext()$path)
setwd(project_path)

source("graph_colours.R")

## Main analysis: Using all available data ----
annot = readRDS("../Data/Compound_family_annotation.rds")

filepaths = c("PesticidesPCBBDEs","PAHs","Combined")
for (i in 1:2){
  pvals = readRDS(paste0("../Results/",filepaths[i],"_univariate_age_gender_pvals.rds"))
  betas = readRDS(paste0("../Results/",filepaths[i],"_univariate_age_gender_betas.rds"))
  
  suffix =  sub(" ", "_", rownames(pvals))
  
  annot_sub = annot[colnames(betas)]
  
  for (k in 1:nrow(pvals)){
    print(suffix[k])
    unit = ifelse(sum(pvals[k,] < 0.05/ncol(pvals))>0,10,1)
    legcol = ifelse(sum(pvals[k,] < 0.05/ncol(pvals))>0,ceiling(sum(pvals[k,] < 0.05/ncol(pvals))/25),1)
    width_extra = ceiling(sum(pvals[k,] < 0.05/ncol(pvals))/25)*2
    
    pdf(paste0("../Figures/Univariate_age_gender_",suffix[k],"_",filepaths[i],".pdf"), width = 5+width_extra, height = 5.5)
    par(mar=c(5,5,3,unit*legcol))
    plot(betas[k,], -log10(pvals[k,]), pch=19,
         col=ifelse(pvals[k,] < 0.05/ncol(betas), annot.colours[annot_sub], "grey"),
         cex.lab=1.5, cex = 3,
         main = rownames(pvals)[k],
         ylim = c(0, max(-log10(pvals[k,])-log10(0.05/ncol(betas)))+0.3),
         xlim = c(-max(abs(betas[k,]))-0.2, max(abs(betas[k,]))+0.2),
         ylab=expression(-log[10](italic(p))),
         xlab=expression(beta))
    abline(h = -log10(0.05/ncol(betas)), lty = 2, col = outcome.colours[k])
    coord = par("usr")
    text(coord[1]+0.01, -log10(0.05/ncol(betas)),  labels = "Bonferroni threshold", adj=c(0,1), col = outcome.colours[k], cex = 0.8)
    abline(h=axTicks(2), col="grey", lty=3)
    abline(v=axTicks(1), col="grey", lty=3)
    for (j in 1:ncol(betas)){
      if(pvals[k,j] < 0.05/ncol(betas)){
        text(betas[k,j], -log10(pvals[k,j]),
             labels = which(colnames(betas)[j] == colnames(betas)[pvals[k,] < 0.05/ncol(betas)]),
             cex = 0.8)
      }
    }
    if(sum(pvals[k,] < 0.05/ncol(betas))>0){
      par(xpd = TRUE)
      legend(x = coord[2]*1.05, y = coord[4],
             legend = paste0(1:sum(pvals[k,] < 0.05/ncol(betas)),". ",
                             colnames(betas)[pvals[k,] < 0.05/ncol(betas)]),
             text.col = darken(annot.colours[annot_sub[pvals[k,] < 0.05/ncol(betas)]],0.5),
             bty = "n",  x.intersp = 0, ncol = legcol)
    }
    dev.off()
  }
}

## Sensitivity analysis: same sample size for all exposures ----
annot = readRDS("../Data/Compound_family_annotation.rds")

filepaths = c("PesticidesPCBBDEs","PAHs","Combined")
pvals = readRDS(paste0("../Results/",filepaths[3],"_univariate_age_gender_pvals.rds"))
betas = readRDS(paste0("../Results/",filepaths[3],"_univariate_age_gender_betas.rds"))

suffix =  sub(" ", "_", rownames(pvals))

annot_sub = annot[colnames(betas)]

for (k in 1:nrow(pvals)){
  print(suffix[k])
  unit = ifelse(sum(pvals[k,] < 0.05/ncol(pvals))>0,10,1)
  legcol = ifelse(sum(pvals[k,] < 0.05/ncol(pvals))>0,ceiling(sum(pvals[k,] < 0.05/ncol(pvals))/25),1)
  width_extra = ceiling(sum(pvals[k,] < 0.05/ncol(pvals))/25)*2
  
  pdf(paste0("../Figures/Univariate_age_gender_nequal_",suffix[k],".pdf"), width = 5+width_extra, height = 5.5)
  par(mar=c(5,5,3,unit*legcol))
  plot(betas[k,], -log10(pvals[k,]), pch=19,
       col=ifelse(pvals[k,] < 0.05/ncol(betas), annot.colours[annot_sub], "grey"),
       cex.lab=1.5, cex = 3,
       main = rownames(pvals)[k],
       ylim = c(0, max(-log10(pvals[k,]),-log10(0.05/ncol(betas)))+0.3),
       xlim = c(-max(abs(betas[k,]))-0.2, max(abs(betas[k,]))+0.2),
       ylab=expression(-log[10](italic(p))),
       xlab=expression(beta))
  abline(h = -log10(0.05/ncol(betas)), lty = 2, col = outcome.colours[k])
  coord = par("usr")
  text(coord[1]+0.01, -log10(0.05/ncol(betas)),  labels = "Bonferroni threshold", adj=c(0,1), col = outcome.colours[k], cex = 0.8)
  abline(h=axTicks(2), col="grey", lty=3)
  abline(v=axTicks(1), col="grey", lty=3)
  for (j in 1:ncol(betas)){
    if(pvals[k,j] < 0.05/ncol(betas)){
      text(betas[k,j], -log10(pvals[k,j]),
           labels = which(colnames(betas)[j] == colnames(betas)[pvals[k,] < 0.05/ncol(betas)]),
           cex = 0.8)
    }
  }
  if(sum(pvals[k,] < 0.05/ncol(betas))>0){
    par(xpd = TRUE)
    legend(x = coord[2]*1.05, y = coord[4],
           legend = paste0(1:sum(pvals[k,] < 0.05/ncol(betas)),". ",
                           colnames(betas)[pvals[k,] < 0.05/ncol(betas)]),
           text.col = darken(annot.colours[annot_sub[pvals[k,] < 0.05/ncol(betas)]],0.5),
           bty = "n",  x.intersp = 0, ncol = legcol)
  }
  dev.off()
}


## Sensitivity analysis: Comparison with model adjusted for country ----
annot = readRDS("../Data/Compound_family_annotation.rds")

filepaths = c("PesticidesPCBBDEs","PAHs","Combined")
for (i in 1:2){
  pvals0 = readRDS(paste0("../Results/",filepaths[i],"_univariate_age_gender_pvals.rds"))
  betas0 = readRDS(paste0("../Results/",filepaths[i],"_univariate_age_gender_betas.rds"))
  
  pvals1 = readRDS(paste0("../Results/",filepaths[i],"_univariate_age_gender_country_pvals.rds"))
  betas1 = readRDS(paste0("../Results/",filepaths[i],"_univariate_age_gender_country_betas.rds"))
  
  # Inspect
  diff = NULL
  for (k in 1:nrow(pvals0)){
    tmp = ifelse(sign(betas0[k,])==sign(betas1[k,]),0,1)
    diff = rbind(diff, tmp)
  }
  rownames(diff) = rownames(pvals0)
  saveRDS(diff, "../Results/Univariate_compare_inconsistent_beta.rds")
  write.csv(diff, "../Exports/Univariate_compare_inconsistent_beta.csv")
  
  suffix =  sub(" ", "_", rownames(pvals0))
  bonf = 0.05/ncol(pvals0)
  bonf = -log10(bonf)
  annot_sub = annot[colnames(betas0)]
  
  for (k in 1:nrow(pvals0)){
    print(suffix[k])
    
    x = -log10(pvals0[k,])
    y = -log10(pvals1[k,])
    
    unit = ifelse(sum(x > bonf | y > bonf)>0,10,1)
    legcol = ifelse(sum(x > bonf | y > bonf)>0,ceiling(sum(x > bonf | y > bonf)/25),1)
    width_extra = ceiling(sum(x > bonf | y > bonf)/25)*2
    
    pdf(paste0("../Figures/Univariate_compare_",suffix[k],"_",filepaths[i],".pdf"), width = 5+width_extra, height = 5.5)
    par(mar=c(5,5,3,unit*legcol))
    plot(x, y, pch=19,
         col=ifelse(x > bonf | y > bonf, annot.colours[annot_sub], "grey"),
         cex.lab=1.5, cex = 3,
         main = rownames(pvals0)[k],
         ylim = c(0, max(y,bonf)+0.3),
         xlim = c(0, max(x,bonf)+0.3),
         xlab=substitute(paste(-log[10](italic(p))," ",x), list(x = "Base model")),
         ylab=substitute(paste(-log[10](italic(p))," ",x), list(x = "Model adjusted for country")))
    coord = par("usr")
    text(x = coord[1]+0.01, y = bonf,labels="Bonferroni threshold", adj = c(0,1),col = outcome.colours[k], cex = 0.8)
    abline(h = bonf, lty = 2, col = darken(outcome.colours[k], 0.5))
    text(x = bonf, y = coord[3]+0.01,labels="Bonferroni threshold", adj = c(1,1),col = outcome.colours[k], cex = 0.8, srt = 270)
    abline(v = bonf, lty = 2, col = darken(outcome.colours[k], 0.5))
    abline(h=axTicks(2), col="grey", lty=3)
    abline(v=axTicks(1), col="grey", lty=3)
    abline(0, 1, col="grey", lty=3)
    for (j in 1:length(x)){
      if(x[j] > bonf| y[j] > bonf){
        text(x[j], y[j],
             labels = which(names(x)[j] == names(x)[x > bonf| y > bonf | y < -bonf]),
             cex = 0.8)
      }
    }
    if(sum(x > bonf | y > bonf)>0){
      par(xpd = TRUE)
      legend(x = coord[2]+0.05, y = coord[4],
             legend = paste0(1:sum(x > bonf | y > bonf),". ",
                             names(x)[x > bonf | y > bonf]),
             text.col = darken(annot.colours[annot_sub[names(x)[x > bonf | y > bonf]]],0.5),
             bty = "n",  x.intersp = 0, ncol = legcol)
    }
    dev.off()
  }  
}
     