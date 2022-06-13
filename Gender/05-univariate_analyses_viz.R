## Visualisation of univariate analyses

rm(list=ls())
project_path=dirname(rstudioapi::getActiveDocumentContext()$path)
setwd(project_path)
setwd("../")
source("graph_colours.R")

filepaths = c("PesticidesPCBBDEs","PAHs","Combined")
strata = c("Female","Male")

for (i in 1:2){
  dataf = readRDS(paste0("../Processed/",filepaths[i],"_XY_",strata[1],".rds"))
  datam = readRDS(paste0("../Processed/",filepaths[i],"_XY_",strata[2],".rds"))
  
  pvalsf = readRDS(paste0("../Results/Gender/",filepaths[i],"_univariate_age_pvals_",strata[1],".rds"))
  betasf = readRDS(paste0("../Results/Gender/",filepaths[i],"_univariate_age_betas_",strata[1],".rds"))
  
  pvalsm = readRDS(paste0("../Results/Gender/",filepaths[i],"_univariate_age_pvals_",strata[2],".rds"))
  betasm = readRDS(paste0("../Results/Gender/",filepaths[i],"_univariate_age_betas_",strata[2],".rds"))
  
  suffix =  sub(" ", "_", rownames(pvalsf))
  bonf = 0.05/ncol(betasf)
  
  for (k in 1:nrow(pvalsf)){
    print(suffix[k])
    sumf = sum(pvalsf[k,] < 0.05/ncol(pvalsf))
    summ = sum(pvalsm[k,] < 0.05/ncol(pvalsm))
    unit = ifelse(sumf+summ > 0, 10, 1)
    legcol = ifelse(sumf+summ > 0, ceiling((sumf+summ)/25),1)
    width_extra = ceiling((sumf+summ)/25)*2
    
    ifelse(dir.exists("../Figures/Gender"),"",dir.create("../Figures/Gender"))
    pdf(paste0("../Figures/Gender/Univariate_age_",suffix[k],"_",filepaths[i],".pdf"), width = 5+width_extra, height = 5.5)
    par(mar=c(5,5,3,unit*legcol))
    plot(betasf[k,], -log10(pvalsf[k,]), pch=19,
         col=ifelse(pvalsf[k,] < bonf, "hotpink", "grey"),
         cex.lab=1.5, cex = 3,
         main = rownames(pvalsf)[k],
         ylim = c(0, max(-log10(pvalsf[k,]),-log10(pvalsm[k,]),-log10(bonf))+0.3),
         xlim = c(-max(abs(betasf[k,]),abs(betasm[k,]))-0.2,
                  max(abs(betasf[k,]),abs(betasm[k,]))+0.2),
         ylab=expression(-log[10](italic(p))),
         xlab=expression(beta))
    points(betasm[k,], -log10(pvalsm[k,]), pch=19,
           col=ifelse(pvalsm[k,] < bonf, "deepskyblue", "grey"),
           cex = 3)
    abline(h = -log10(bonf), lty = 2, col = outcome.colours[k])
    coord = par("usr")
    text(coord[1]+0.01, -log10(bonf),  labels = "Bonferroni threshold", adj=c(0,1), col = outcome.colours[k], cex = 0.8)
    abline(h=axTicks(2), col="grey", lty=3)
    abline(v=axTicks(1), col="grey", lty=3)
    for (j in 1:ncol(betasf)){
      if(pvalsf[k,j] < bonf){
        text(betasf[k,j], -log10(pvalsf[k,j]),
             labels = which(colnames(betasf)[j] == colnames(betasf)[pvalsf[k,] < bonf]),
             cex = 0.8)
      }
      if(pvalsm[k,j] < bonf){
        text(betasm[k,j], -log10(pvalsm[k,j]),
             labels = which(colnames(betasm)[j] == colnames(betasm)[pvalsm[k,] < bonf]),
             cex = 0.8)
      }
    }
    if(sum(pvalsf[k,] < bonf)>0){
      par(xpd = TRUE)
      legend(x = coord[2]*1.01, y = coord[4],
             legend = c(paste0("Female (N=",nrow(dataf),")"),
                        paste0(1:sum(pvalsf[k,] < bonf),". ",gsub("\\+","\\+\n",colnames(betasf)[pvalsf[k,] < bonf]))),
             text.col = darken("hotpink",0.5),
             bty = "n",  x.intersp = 0, ncol = legcol)
    }
    if(sum(pvalsm[k,] < bonf)>0){
      par(xpd = TRUE)
      legend(x = coord[2]*1.01, y = (coord[4]-coord[3])/2,
             legend = c(paste0("Male (N=",nrow(datam),")"),
                        paste0(1:sum(pvalsm[k,] < bonf),". ",gsub("\\+","\\+\n",colnames(betasm)[pvalsm[k,] < bonf]))),
             text.col = darken("deepskyblue",0.5),
             bty = "n",  x.intersp = 0, ncol = legcol)
    }
    dev.off()
  }
}