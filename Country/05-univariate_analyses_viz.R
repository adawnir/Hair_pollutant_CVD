## Visualisation of univariate analyses

rm(list=ls())
project_path=dirname(rstudioapi::getActiveDocumentContext()$path)
setwd(project_path)
setwd("../")
source("graph_colours.R")
library(colorspace)

filepaths = c("PesticidesPCBBDEs","PAHs","Combined")
strata = c("Belgium","Luxembourg","France")

for (i in 1:2){
  n = ifelse(i==2,2,3)
  for (h in 1:n){
    data = readRDS(paste0("../Processed/",filepaths[i],"_XY_",strata[h],".rds"))
    pvals = readRDS(paste0("../Results/Country/",filepaths[i],"_univariate_age_gender_pvals_",strata[h],".rds"))
    betas = readRDS(paste0("../Results/Country/",filepaths[i],"_univariate_age_gender_betas_",strata[h],".rds"))
    assign(paste0("data",strata[h]),data)
    assign(paste0("pvals",strata[h]),pvals)
    assign(paste0("betas",strata[h]),betas)
    
  }
  suffix =  sub(" ", "_", rownames(pvalsBelgium))
  bonf = 0.05/ncol(betasBelgium)
  if (i == 1){
    krow = 1:5
  }
  if (i == 2){
    krow = c(1,2,4,5)
  }
  for (k in krow){
    print(suffix[k])
    if (i == 1){
      sum = sum(pvalsBelgium[k,] < 0.05/ncol(pvalsBelgium),
                pvalsLuxembourg[k,] < 0.05/ncol(pvalsLuxembourg),
                pvalsFrance[k,] < 0.05/ncol(pvalsFrance))
      xlim = c(-1,1) * (max(abs(betasBelgium[k,]),
                            abs(betasLuxembourg[k,]),
                            abs(betasFrance[k,]))+0.2)
      ylim = c(0, max(-log10(pvalsBelgium[k,]),
                      -log10(pvalsLuxembourg[k,]),
                      -log10(pvalsFrance[k,]),
                      -log10(bonf))+0.3)
    }
    if (i == 2){
      sum = sum(pvalsBelgium[k,] < 0.05/ncol(pvalsBelgium),
                pvalsLuxembourg[k,] < 0.05/ncol(pvalsLuxembourg))
      xlim = c(-1,1) * (max(abs(betasBelgium[k,]),abs(betasLuxembourg[k,]))+0.2)
      ylim = c(0, max(-log10(pvalsBelgium[k,]),
                      -log10(pvalsLuxembourg[k,]),
                      -log10(bonf))+0.3)
    }
    unit = ifelse(sum > 0, 10, 1)
    legcol = ifelse(sum > 0, ceiling(sum/25),1)
    width_extra = ceiling(sum/25)*2
    
    ifelse(dir.exists("../Figures/Country"),"",dir.create("../Figures/Country"))
    pdf(paste0("../Figures/Country/Univariate_age_gender_",suffix[k],"_",filepaths[i],".pdf"), width = 5+width_extra, height = 5.5)
    par(mar=c(5,5,3,unit*legcol))
    plot(betasBelgium[k,], -log10(pvalsBelgium[k,]), pch=19,
         col=ifelse(pvalsBelgium[k,] < bonf, country.colours[1], "grey"),
         cex.lab=1.5, cex = 3,
         main = rownames(pvalsBelgium)[k],
         ylim = ylim,
         xlim = xlim,
         ylab=expression(-log[10](italic(p))),
         xlab=expression(beta))
    points(betasLuxembourg[k,], -log10(pvalsLuxembourg[k,]), pch=19,
           col=ifelse(pvalsLuxembourg[k,] < bonf, country.colours[3], "grey"),
           cex = 3)
    if (i == 2){
      points(betasFrance[k,], -log10(pvalsFrance[k,]), pch=19,
             col=ifelse(pvalsFrance[k,] < bonf, country.colours[2], "grey"),
             cex = 3)
    }
    abline(h = -log10(bonf), lty = 2, col = outcome.colours[k])
    coord = par("usr")
    text(coord[1]+0.01, -log10(bonf),  labels = "Bonferroni threshold", adj=c(0,1), col = outcome.colours[k], cex = 0.8)
    abline(h=axTicks(2), col="grey", lty=3)
    abline(v=axTicks(1), col="grey", lty=3)
    for (j in 1:ncol(betasBelgium)){
      if(pvalsBelgium[k,j] < bonf){
        text(betasBelgium[k,j], -log10(pvalsBelgium[k,j]),
             labels = which(colnames(betasBelgium)[j] == colnames(betasBelgium)[pvalsBelgium[k,] < bonf]),
             cex = 0.8)
      }
      if(pvalsLuxembourg[k,j] < bonf){
        text(betasLuxembourg[k,j], -log10(pvalsLuxembourg[k,j]),
             labels = which(colnames(betasLuxembourg)[j] == colnames(betasLuxembourg)[pvalsLuxembourg[k,] < bonf]),
             cex = 0.8)
      }
      if (i == 2){
        if(pvalsFrance[k,j] < bonf){
          text(betasFrance[k,j], -log10(pvalsFrance[k,j]),
               labels = which(colnames(betasFrance)[j] == colnames(betasFrance)[pvalsFrance[k,] < bonf]),
               cex = 0.8)
        }
      }
    }
    if(sum(pvalsBelgium[k,] < bonf)>0){
      par(xpd = TRUE)
      legend(x = coord[2]*1.01, y = coord[4],
             legend = c(paste0("Belgium (N=",nrow(dataBelgium),")"),
                        paste0(1:sum(pvalsBelgium[k,] < bonf),". ",gsub("\\+","\\+\n",colnames(betasBelgium)[pvalsBelgium[k,] < bonf]))),
             text.col = darken(country.colours[1],0.5),
             bty = "n",  x.intersp = 0, ncol = legcol)
    }
    if(sum(pvalsLuxembourg[k,] < bonf)>0){
      par(xpd = TRUE)
      legend(x = coord[2]*1.01, y = (coord[4]-coord[3])/n,
             legend = c(paste0("Luxembourg (N=",nrow(dataLuxembourg),")"),
                        paste0(1:sum(pvalsLuxembourg[k,] < bonf),". ",gsub("\\+","\\+\n",colnames(betasLuxembourg)[pvalsLuxembourg[k,] < bonf]))),
             text.col = darken(country.colours[3],0.5),
             bty = "n",  x.intersp = 0, ncol = legcol)
    }
    if (i == 2){
      if(sum(pvalsFrance[k,] < bonf)>0){
        par(xpd = TRUE)
        legend(x = coord[2]*1.01, y = (coord[4]-coord[3])*2/n,
               legend = c(paste0("France (N=",nrow(dataFrance),")"),
                          paste0(1:sum(pvalsFrance[k,] < bonf),". ",gsub("\\+","\\+\n",colnames(betasFrance)[pvalsFrance[k,] < bonf]))),
               text.col = darken(country.colours[2],0.5),
               bty = "n",  x.intersp = 0, ncol = legcol)
      }
    }
    dev.off()
  }
}
