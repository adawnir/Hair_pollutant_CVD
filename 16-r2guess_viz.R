# R2GUESS

rm(list=ls())
project_path=dirname(rstudioapi::getActiveDocumentContext()$path)
setwd(project_path)


library(R2GUESS)
library(sgPLS)

source("graph_colours.R")
library(colorspace)

trait_group = c("Core","Core+FPG","All_traits")
filepath = "PesticidesPCBBDEs"

X = readRDS(paste0("../Processed/",filepath,"_expo_ndimp_naimp_log.rds"))
annot_sub = annot[colnames(X)]
model.names = c("Model 0: + Age + Gender", "+ Centre", "+ Education",
                "+ Smoking status", "+ Dietary intake")

for (j in 1:3){
  pdf(paste0("../Figures/R2GUESS/",trait_group[j],"/Y_X_f0-1_MPPI.pdf"), height = 6, width = 8)
  par(mar=c(1,1,0,1), oma = c(7,9,3,4), mfrow = c(2,1))
  # Load data
  for (f in 0:1){
    model = readRDS(paste0("../Results/R2GUESS/",trait_group[j],"/Model",f,".rds"))
    # Extract MPPI
    NameMarg = file.path(model$path.output, paste(model$root.file.output,"output_marg_prob_incl.txt",sep="_"))
    Marg  = read.table(NameMarg,header=TRUE)
    Marg$Predictor = colnames(X)

    bestmodel = strsplit(model$BestModels$modelName[1], " ")[[1]]
    
    ## MPPI plots
    plot(Marg$Marg_Prob_Incl, type = "h", ylim = c(0,1),
         xaxt = "n", yaxt = "n", lwd=2,
         col = ifelse(Marg$Marg_Prob_Incl>0.1, darken(annot.colours[annot_sub],0.5),"black"),
         xlab = "", ylab = "")
    points(x = as.numeric(bestmodel),
           y = Marg$Marg_Prob_Incl[as.numeric(bestmodel)], pch = 19, col = "#dc251f")
    abline(h = 0.1, col = "darkred", lty = 2)
    axis(side=4, at=axTicks(2), cex.axis = 0.8)
    coord = par("usr")
    text(coord[2], 0.1, "MPPI = 0.1", col = "darkred", adj = c(1,0), cex = 0.6)
    mtext(model.names[f+1], cex = 0.8, side = 2, line = 1, outer = FALSE, las = 1)
    if(f==1){
      legend("topleft", legend = "Best model", pch = 19, col = "#dc251f", cex = 0.8)
      mtext("Marginal Posterior Probability of Inclusion", cex = 1.2, side = 4, line = 2, outer = TRUE)
      mtext(paste(model[["label.Y"]],collapse = "/"), cex = 1.5, side = 3, line = 1, outer = TRUE)
      for (k in 1:nrow(Marg)){
        axis(side=1, at=k, labels=Marg$Predictor[k],
             col.axis = ifelse(k %in% as.numeric(bestmodel), darken(annot.colours[annot_sub[k]],0.5),"black"),
             las=2, cex.axis = 0.8)
      }
    }
  }
  dev.off() 
}

for (j in 1:3){
  pdf(paste0("../Figures/R2GUESS/",trait_group[j],"/Y_X_f0-4_MPPI.pdf"), height = 7, width = 8)
  par(mar=c(1,1,0,1), oma = c(7,9,3,4), mfrow = c(4,1))
  # Load data
  for (f in c(0,2:4)){
    model = readRDS(paste0("../Results/R2GUESS/",trait_group[j],"/Model",f,".rds"))
    # Extract MPPI
    NameMarg = file.path(model$path.output, paste(model$root.file.output,"output_marg_prob_incl.txt",sep="_"))
    Marg  = read.table(NameMarg,header=TRUE)
    Marg$Predictor = colnames(X)
    
    bestmodel = strsplit(model$BestModels$modelName[1], " ")[[1]]
    
    ## MPPI plots
    plot(Marg$Marg_Prob_Incl, type = "h", ylim = c(0,1),
         xaxt = "n", yaxt = "n", lwd=2,
         col = ifelse(Marg$Marg_Prob_Incl>0.1, darken(annot.colours[annot_sub],0.5),"black"),
         xlab = "", ylab = "")
    points(x = as.numeric(bestmodel),
           y = Marg$Marg_Prob_Incl[as.numeric(bestmodel)], pch = 19, col = "#dc251f")
    abline(h = 0.1, col = "darkred", lty = 2)
    axis(side=4, at=axTicks(2), cex.axis = 0.8)
    coord = par("usr")
    text(coord[2], 0.1, "MPPI = 0.1", col = "darkred", adj = c(1,0), cex = 0.6)
    mtext(model.names[f+1], cex = 0.8, side = 2, line = 1, outer = FALSE, las = 1)
    if(f==4){
      legend("topleft", legend = "Best model", pch = 19, col = "#dc251f", cex = 0.8)
      mtext("Marginal Posterior Probability of Inclusion", cex = 1.2, side = 4, line = 2, outer = TRUE)
      mtext(paste(model$label.Y,collapse = "/"), cex = 1.5, side = 3, line = 1, outer = TRUE)
      for (k in 1:nrow(Marg)){
        axis(side=1, at=k, labels=Marg$Predictor[k],
             col.axis = ifelse(k %in% as.numeric(bestmodel), darken(annot.colours[annot_sub[k]],0.5),"black"),
             las=2, cex.axis = 0.8)
      }
    }
  }
  dev.off() 
}

## Boxplots summarising the posterior distribution of the
## regression coefficients for selected variables

f = 4

res.g = readRDS(paste0("../Results/R2GUESS/",trait_group[i],"/Model",f,"_res.rds"))
beta = readRDS(paste0("../Results/R2GUESS/",trait_group[i],"/Model",f,"_beta.rds"))

for(i in 1:length(res.g$predictors)){
  pdf(paste0("../Figures/R2GUESS/Model",f,"_",colnames(X)[as.numeric(res.g$predictors[i])],"_estimated_beta_",trait_group[j],".pdf"), height = 4)
  par(mar=c(5,5,3,1))
  variable <- as.character(res.g$predictors[i])
  indice <- match(variable,beta$predictors)
  indix1 <- (model$q)*(indice-1)+1
  matbeta <- as.matrix(beta$res.beta[,indix1:(indix1+model$q-1)])
  colnames(matbeta) <- model$label.Y 
  
  boxplot(matbeta,
          main = colnames(X)[as.numeric(res.g$predictors[i])],
          xlab = expression(beta),
          col = alpha(c("#ef7c10","#ef7c10","#00722a","#00722a","#00722a",
                        "#00722a","#dc251f","#dc251f","#009ee2"),0.5),
          horizontal = TRUE, outline = FALSE, las = 1)
  abline(v=0, lty = 2)
  abline(h=c(1:(ncol(matbeta)-1))+0.5, col = "grey", lty = 3)
  dev.off()
}




