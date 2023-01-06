# R2GUESS

rm(list=ls())
project_path=dirname(rstudioapi::getActiveDocumentContext()$path)
setwd(project_path)


library(R2GUESS)

source("graph_colours.R")
library(colorspace)

## Diagnostic plots ----
## Diagnostic plots to assess the convergence of the algorithm
jpeg(paste0("../Figures/R2GUESS/",trait_group[i],"/Model",f,"_monitor_App.jpg"), width = 20, height = 17, units = "cm", res = 300)
plot(model)
dev.off()

## Diagnostic plots to assess the convergence of the algorithm
pdf(paste0("../Figures/R2GUESS/",trait_group[i],"/Model",f,"_conv1.pdf"))
check.convergence(model, 1)
dev.off()

pdf(paste0("../Figures/R2GUESS/",trait_group[i],"/Model",f,"_conv2.pdf"))
check.convergence(model, 2)
dev.off()

### MPPI multi-trait ----
trait_group = c("Core","Core+FPG","All_traits")
filepath = "PesticidesPCBBDEs"

X = readRDS(paste0("../Processed/",filepath,"_expo_ndimp_naimp_log.rds"))
annot_sub = annot[colnames(X)]
model.names = c("Model 0: + Age + Gender", "+ Centre", "+ Education",
                "+ Smoking status", "+ Dietary intake")

for (i in 1:3){
  pdf(paste0("../Figures/R2GUESS/",trait_group[i],"/Y_X_f0-1_MPPI.pdf"), height = 5, width = 8)
  par(mai=c(0.1,0.1,0,0.1), omi = c(1.4,1.8,0.6,0.8), mfrow = c(2,1))
  # Load data
  for (f in 0:1){
    model = readRDS(paste0("../Results/R2GUESS/",trait_group[i],"/Model",f,".rds"))
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

for (i in 1:3){
  pdf(paste0("../Figures/R2GUESS/",trait_group[i],"/Y_X_f0-4_MPPI.pdf"), height = 7, width = 8)
  par(mai=c(0.1,0.1,0,0.1), omi = c(1.4,1.8,0.6,0.8), mfrow = c(4,1))
  # Load data
  for (f in c(0,2:4)){
    model = readRDS(paste0("../Results/R2GUESS/",trait_group[i],"/Model",f,".rds"))
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
    axis(side=4, at=axTicks(2), cex.axis = 1.2)
    coord = par("usr")
    text(coord[2], 0.1, "MPPI = 0.1", col = "darkred", adj = c(1,0), cex = 0.9)
    mtext(model.names[f+1], cex = 0.8, side = 2, line = 1, outer = FALSE, las = 1)
    if(f==4){
      legend("topleft", legend = "Best model", pch = 19, col = "#dc251f", cex = 1.2)
      mtext("Marginal Posterior Probability of Inclusion", cex = 1.2, side = 4, line = 3, outer = TRUE)
      mtext(paste(model$label.Y,collapse = "/"), cex = 1.5, side = 3, line = 1, outer = TRUE)
      for (k in 1:nrow(Marg)){
        axis(side=1, at=k, labels=Marg$Predictor[k],
             col.axis = ifelse(k %in% as.numeric(bestmodel), darken(annot.colours[annot_sub[k]],0.5),"black"),
             las=2, cex.axis = 1.2)
      }
    }
  }
  dev.off() 
}

pdf(paste0("../Figures/R2GUESS/Multi_trait_Y_X_MPPI.pdf"), height = 5, width = 8)
par(mai=c(0.1,0.1,0,0.1), omi = c(1.4,0.6,0.1,0.8), mfrow = c(3,1))
# Load data
for (i in 1:3){
  model = readRDS(paste0("../Results/R2GUESS/",trait_group[i],"/Model4.rds"))
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
  axis(side=4, at=axTicks(2), cex.axis = 1.2)
  coord = par("usr")
  text(coord[2], 0.1, "MPPI = 0.1", col = "darkred", adj = c(1,0), cex = 0.9)
  mtext(c("6 traits","7 traits","All traits")[i], cex = 0.8, side = 2, line = 1, outer = FALSE, las = 1)
  if(i==3){
    legend("topleft", legend = "Best model", pch = 19, col = "#dc251f", cex = 1.2)
    mtext("Marginal Posterior Probability of Inclusion", cex = 1.2, side = 4, line = 3, outer = TRUE)
    axis(side=1, at=1:nrow(Marg), labels=Marg$Predictor,las=2, cex.axis = 1.2)
  }
}
dev.off()

### MPPI single-trait ----
filepath = "PesticidesPCBBDEs"
Y = readRDS(paste0("../Data/",filepath,"_trait.rds"))
trait_group = colnames(Y)[1:9]
X = readRDS(paste0("../Processed/",filepath,"_expo_ndimp_naimp_log.rds"))
annot_sub = annot[colnames(X)]
model.names = c("Model 0: + Age + Gender", "+ Centre", "+ Education",
                "+ Smoking status", "+ Dietary intake")

for (i in 1:9){
  pdf(paste0("../Figures/R2GUESS/",trait_group[i],"/Y_X_f0-1_MPPI.pdf"), height = 5, width = 8)
  par(mai=c(0.1,0.1,0,0.1), omi = c(1.4,1.8,0.6,0.8), mfrow = c(2,1))
  # Load data
  for (f in 0:1){
    model = readRDS(paste0("../Results/R2GUESS/",trait_group[i],"/Model",f,".rds"))
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

for (i in 1:9){
  pdf(paste0("../Figures/R2GUESS/",trait_group[i],"/Y_X_f0-4_MPPI.pdf"), height = 7, width = 8)
  par(mai=c(0.1,0.1,0,0.1), omi = c(1.4,1.8,0.6,0.8), mfrow = c(4,1))
  # Load data
  for (f in c(0,2:4)){
    model = readRDS(paste0("../Results/R2GUESS/",trait_group[i],"/Model",f,".rds"))
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
    axis(side=4, at=axTicks(2), cex.axis = 1.2)
    coord = par("usr")
    text(coord[2], 0.1, "MPPI = 0.1", col = "darkred", adj = c(1,0), cex = 0.9)
    mtext(model.names[f+1], cex = 0.8, side = 2, line = 1, outer = FALSE, las = 1)
    if(f==4){
      legend("topleft", legend = "Best model", pch = 19, col = "#dc251f", cex = 1.2)
      mtext("Marginal Posterior Probability of Inclusion", cex = 1.2, side = 4, line = 3, outer = TRUE)
      mtext(paste(model$label.Y,collapse = "/"), cex = 1.5, side = 3, line = 1, outer = TRUE)
      for (k in 1:nrow(Marg)){
        axis(side=1, at=k, labels=Marg$Predictor[k],
             col.axis = ifelse(k %in% as.numeric(bestmodel), darken(annot.colours[annot_sub[k]],0.5),"black"),
             las=2, cex.axis = 1.2)
      }
    }
  }
  dev.off() 
}

pdf(paste0("../Figures/R2GUESS/Single_trait_Y_X_MPPI.pdf"), height = 10, width = 8)
par(mai=c(0.1,0.1,0,0.1), omi = c(1.4,0.6,0.1,0.8), mfrow = c(9,1))
# Load data
for (i in c(1:3,6:9,4:5)){
  model = readRDS(paste0("../Results/R2GUESS/",trait_group[i],"/Model4.rds"))
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
  axis(side=4, at=axTicks(2), cex.axis = 1.2)
  coord = par("usr")
  text(coord[2], 0.1, "MPPI = 0.1", col = "darkred", adj = c(1,0), cex = 0.9)
  mtext(trait_group[i], cex = 0.8, side = 2, line = 1, outer = FALSE, las = 1)
  if(i==5){
    legend("topleft", legend = "Best model", pch = 19, col = "#dc251f", cex = 1.2)
    mtext("Marginal Posterior Probability of Inclusion", cex = 1.2, side = 4, line = 3, outer = TRUE)
    axis(side=1, at=1:nrow(Marg), labels=Marg$Predictor,las=2, cex.axis = 1.2)
  }
}
dev.off()


