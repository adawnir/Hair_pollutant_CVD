# R2GUESS

rm(list=ls())
project_path=dirname(rstudioapi::getActiveDocumentContext()$path)
setwd(project_path)


library(R2GUESS)
library(sgPLS)

source("graph_colours.R")
library(colorspace)

filepaths = c("PesticidesPCBBDEs_train","PAHs_train")
i = 1
paths = c("Option1","Option2","Option3")
j = 1
myfiles = list.files(path = paste0("../Results/R2GUESS/",paths[j]), pattern = "Model")

X = readRDS(paste0("../Processed/",filepaths[i],"_expo_ndimp_naimp_log.rds"))
annot_sub = annot[colnames(X)]
model.names = c("Model 0: + Age + Gender", "Model 0 + Centre", "Model 0 + Education",
                "+ Smoking status", "+ Dietary intake",
                "+ Physical activity", "+ Alcohol consumption","+ Medication")

for (j in 1:3){
  pdf(paste0("../Figures/R2GUESS/",paths[j],"/_Y_X_f0-1_MPPI_",filepaths[i],".pdf"), height = 4)
  par(mar=c(1,1,0,1), oma = c(6,13,3,5), mfrow = c(2,1))
  # Load data
  for (f in 0:1){
    model = readRDS(paste0("../Results/R2GUESS/",paths[j],"/Model",f,"_",filepaths[i],".rds"))
    # Extract MPPI
    NameMarg = file.path(model$path.output, paste(model$root.file.output,"output_marg_prob_incl.txt",sep="_"))
    Marg  = read.table(NameMarg,header=TRUE)
    Marg$Predictor = colnames(X)

    bestmodel = strsplit(model$BestModels$modelName[1], " ")[[1]]
    
    ## MPPI plots
    plot(Marg$Marg_Prob_Incl, type = "h", ylim = c(0,1),
         xaxt = "n", yaxt = "n", lwd=2,
         col = ifelse(Marg$Marg_Prob_Incl[i]>0.1, darken(annot.colours[annot_sub],0.5),"black"),
         xlab = "", ylab = "")
    points(x = as.numeric(bestmodel),
           y = Marg$Marg_Prob_Incl[as.numeric(bestmodel)], pch = 19, col = "#dc251f")
    abline(h = 0.1, col = "darkred", lty = 2)
    axis(side=4, at=axTicks(2), cex.axis = 0.8)
    coord = par("usr")
    text(coord[2], 0.1, "MPPI = 0.1", col = "darkred", adj = c(1,1), cex = 0.8)
    mtext(model.names[f+1], cex = 0.8, side = 2, line = 1, outer = FALSE, las = 1)
    if(f==0){
      legend("topleft", legend = "Best model", pch = 19, col = "#dc251f")
    }
    if(f==1){
      mtext("Marginal Posterior Probability of Inclusion", cex = 1.2, side = 4, line = 2, outer = TRUE)
      mtext(paste(model[["label.Y"]],collapse = "/"), cex = 1.5, side = 3, line = 1, outer = TRUE)
      for (k in 1:nrow(Marg)){
        axis(side=1, at=k, labels=Marg$Predictor[k],
             col.axis = ifelse(Marg$Marg_Prob_Incl[k]>0.1, darken(annot.colours[annot_sub[k]],0.5),"black"),
             las=2, cex.axis = 0.8)
      }
    }
  }
  dev.off() 
}

for (j in 1:3){
  pdf(paste0("../Figures/R2GUESS/",paths[j],"/_Y_X_f2-7_MPPI_",filepaths[i],".pdf"), height = 9)
  par(mar=c(1,1,0,1), oma = c(6,13,3,5), mfrow = c(7,1))
  # Load data
  for (f in 2:7){
    model = readRDS(paste0("../Results/R2GUESS/",paths[j],"/Model",f,"_",filepaths[i],".rds"))
    # Extract MPPI
    NameMarg = file.path(model$path.output, paste(model$root.file.output,"output_marg_prob_incl.txt",sep="_"))
    Marg  = read.table(NameMarg,header=TRUE)
    Marg$Predictor = colnames(X)
    
    bestmodel = strsplit(model$BestModels$modelName[1], " ")[[1]]
    
    ## MPPI plots
    plot(Marg$Marg_Prob_Incl, type = "h", ylim = c(0,1),
         xaxt = "n", yaxt = "n", lwd=2,
         col = ifelse(Marg$Marg_Prob_Incl[i]>0.1, darken(annot.colours[annot_sub],0.5),"black"),
         xlab = "", ylab = "")
    points(x = as.numeric(bestmodel),
           y = Marg$Marg_Prob_Incl[as.numeric(bestmodel)], pch = 19, col = "#dc251f")
    abline(h = 0.1, col = "darkred", lty = 2)
    axis(side=4, at=axTicks(2), cex.axis = 0.8)
    coord = par("usr")
    text(coord[2], 0.1, "MPPI = 0.1", col = "darkred", adj = c(1,1), cex = 0.8)
    mtext(model.names[f+1], cex = 0.8, side = 2, line = 1, outer = FALSE, las = 1)
    if(f==2){
      legend("topleft", legend = "Best model", pch = 19, col = "#dc251f")
    }
    if(f==7){
      mtext("Marginal Posterior Probability of Inclusion", cex = 1.2, side = 4, line = 2, outer = TRUE)
      mtext(paste(model[["label.Y"]],collapse = "/"), cex = 1.5, side = 3, line = 1, outer = TRUE)
      for (k in 1:nrow(Marg)){
        axis(side=1, at=k, labels=Marg$Predictor[k],
             col.axis = ifelse(Marg$Marg_Prob_Incl[k]>0.1, darken(annot.colours[annot_sub[k]],0.5),"black"),
             las=2, cex.axis = 0.8)
      }
    }
  }
  dev.off() 
}

## Boxplots summarising the posterior distribution of the
## regression coefficients for selected variables
pdf(paste0("../Figures/R2GUESS/",paths[j],"/Model",f,"_","_estimated_beta_",filepaths[i],".pdf"), height = 4)
for(i in 1:length(res.g$predictors)){
  res.boxplot = boxplotbeta(model, beta, variable = res.g$predictors[i])
}
dev.off()



