# sPLS for selecting outcomes and prior for R2GUESS

rm(list=ls())
project_path=dirname(rstudioapi::getActiveDocumentContext()$path)
setwd(project_path)

source("functions.R")
source("graph_colours.R")

library(sharp)
library(mixOmics)
library(tidyverse)
library(FactoMineR)
library(colorspace)

filepaths = c("PesticidesPCBBDEs_train","PAHs_train")

## m = 1 ----
m = 1

# Load data
data = readRDS(paste0("../Processed/",filepaths[m],"_XY.rds"))
Y = data[,c(17:25)]
X = data[,27:ncol(data)]

mypca=PCA(Y, graph = FALSE)
ev=mypca$eig[,2]

mycor=cor(Y, mypca$ind$coord)

CreateScorePlot(mypca=mypca, type=as.character(data$Country),
                mycolours=country.colours[levels(data$Country)],
                filename=paste0("../Figures/Descriptive_statistics/PCA_outcome_score_plot_by_Country_",filepaths[m],".pdf"),
                ellipse = TRUE)

pdf(paste0("../Figures/Descriptive_statistics/PCA_outcome_correlation_circle_",filepaths[m],".pdf"), width=15, height=5)
layout(matrix(c(1,2,3), 1, 3, byrow = TRUE), widths=c(2,2,2))
par(mar = c(5,5,1,1), pty = "s")
comp=matrix(c(1,2,1,3,2,3), byrow=TRUE, ncol=2)
for (k in 1:nrow(comp)){
  xcomp=comp[k,1]
  ycomp=comp[k,2]
  plot(mycor[,c(xcomp,ycomp)], xlim=c(-1.25,1.25), ylim=c(-1.25,1.25), cex=0.1, pch=19, las=1, cex.lab=1.5,
       xlab=paste0("Comp ",xcomp," (", round(ev[xcomp], digits=2), "% e.v.)"),
       ylab=paste0("Comp ",ycomp," (", round(ev[ycomp], digits=2), "% e.v.)"),
       col=outcome.colours)
  arrows(x0=rep(0, nrow(mycor)), y0=rep(0, nrow(mycor)),
         x1=mycor[,xcomp], y1=mycor[,ycomp], length=0.1, col=outcome.colours)
  abline(h=0, lty=2)
  abline(v=0, lty=2)
  xseq=seq(-1,1,length.out=10000)
  lines(xseq, sqrt(1-xseq^2))
  lines(xseq, -sqrt(1-xseq^2))
  text(mycor[,xcomp]+sign(mycor[,xcomp])*0.1, mycor[,ycomp],
       labels=colnames(Y), cex=0.8,
       col=darken(outcome.colours,0.5))
}
dev.off()

tmp = model.matrix(~ Smoking + Education + Physical_activity + Alcohol, data)[,-1]
Z = cbind(data$Age,
          data$Gender,
          tmp[,1:2],
          data[,c("DietScore1","DietScore2","DietScore3")],
          tmp[,3:ncol(tmp)])
Z = Z %>% mutate_if(is.factor, function(x) as.integer(x)-1)

f1='X[,k] ~ data$Age + data$Gender + `SmokingFormer smoker` + `SmokingCurrent smoker` + DietScore1 + DietScore2 + DietScore3'
f3='X[,k] ~ data$Age + data$Gender + `SmokingFormer smoker` + `SmokingCurrent smoker` + DietScore1 + DietScore2 + DietScore3 + EducationIntermediate + EducationHigh'
f4='X[,k] ~ data$Age + data$Gender + `SmokingFormer smoker` + `SmokingCurrent smoker` + DietScore1 + DietScore2 + DietScore3 + EducationIntermediate + EducationHigh + `Physical_activityModerately active` + Physical_activityActive + AlcoholModerate + AlcoholHigh'

for (f in c(1,3,4)){
  X_denoised=NULL
  for (k in seq(1:ncol(X))){
    print(colnames(X)[k])
    formula = eval(parse(text = paste0("f",f)))
    model=lm(as.formula(formula), data=Z)
    X_denoised = cbind(X_denoised, model$residuals)
  }
  colnames(X_denoised)=colnames(X)
  rownames(X_denoised)=rownames(X)
  
  stab = readRDS(paste0("../Results/R2GUESS/sPLS_XY_f",f,"_output_",filepaths[m],".rds"))
  
  myspls = spls(X_denoised, Y, ncomp = 3,
                keepX = colSums(stab$selectedX),
                keepY = colSums(stab$selectedY))
  w = myspls$explained_variance$Y/sum(myspls$explained_variance$Y) # weight per component based on ev
  # tmp = stab$selpropY%*%w # rank outcomes by importance (proportion)
  tmp = stab$selectedY%*%w # rank outcomes by importance (binary)
  rank = c(tmp)
  names(rank) = rownames(tmp)
  rank = sort(rank, decreasing = T)
  assign(paste0("rank_f",f), rank)
}

rank_f1
rank_f3
rank_f4

## m = 2 ----
m = 2

# Load data
data = readRDS(paste0("../Processed/",filepaths[m],"_XY.rds"))
Y = data[,c(17:25)]
X = data[,27:ncol(data)]

mypca=PCA(Y, graph = FALSE)
ev=mypca$eig[,2]

mycor=cor(Y, mypca$ind$coord)

CreateScorePlot(mypca=mypca, type=as.character(data$Country),
                mycolours=country.colours[levels(data$Country)],
                filename=paste0("../Figures/Descriptive_statistics/PCA_outcome_score_plot_by_Country_",filepaths[m],".pdf"),
                ellipse = TRUE)

pdf(paste0("../Figures/Descriptive_statistics/PCA_outcome_correlation_circle_",filepaths[m],".pdf"), width=15, height=5)
layout(matrix(c(1,2,3), 1, 3, byrow = TRUE), widths=c(2,2,2))
par(mar = c(5,5,1,1), pty = "s")
comp=matrix(c(1,2,1,3,2,3), byrow=TRUE, ncol=2)
for (k in 1:nrow(comp)){
  xcomp=comp[k,1]
  ycomp=comp[k,2]
  plot(mycor[,c(xcomp,ycomp)], xlim=c(-1.25,1.25), ylim=c(-1.25,1.25), cex=0.1, pch=19, las=1, cex.lab=1.5,
       xlab=paste0("Comp ",xcomp," (", round(ev[xcomp], digits=2), "% e.v.)"),
       ylab=paste0("Comp ",ycomp," (", round(ev[ycomp], digits=2), "% e.v.)"),
       col=outcome.colours)
  arrows(x0=rep(0, nrow(mycor)), y0=rep(0, nrow(mycor)),
         x1=mycor[,xcomp], y1=mycor[,ycomp], length=0.1, col=outcome.colours)
  abline(h=0, lty=2)
  abline(v=0, lty=2)
  xseq=seq(-1,1,length.out=10000)
  lines(xseq, sqrt(1-xseq^2))
  lines(xseq, -sqrt(1-xseq^2))
  text(mycor[,xcomp]+sign(mycor[,xcomp])*0.1, mycor[,ycomp],
       labels=colnames(Y), cex=0.8,
       col=darken(outcome.colours,0.5))
}
dev.off()

tmp = model.matrix(~ Smoking + Education + Physical_activity + Alcohol, data)[,-1]
Z = cbind(data$Age,
          data$Gender,
          tmp[,1:2],
          data[,c("DietScore1","DietScore2","DietScore3")],
          tmp[,3:ncol(tmp)])
Z = Z %>% mutate_if(is.factor, function(x) as.integer(x)-1)

f1='X[,k] ~ data$Age + data$Gender + `SmokingFormer smoker` + `SmokingCurrent smoker` + DietScore1 + DietScore2 + DietScore3'
f3='X[,k] ~ data$Age + data$Gender + `SmokingFormer smoker` + `SmokingCurrent smoker` + DietScore1 + DietScore2 + DietScore3 + EducationIntermediate + EducationHigh'
f4='X[,k] ~ data$Age + data$Gender + `SmokingFormer smoker` + `SmokingCurrent smoker` + DietScore1 + DietScore2 + DietScore3 + EducationIntermediate + EducationHigh + `Physical_activityModerately active` + Physical_activityActive + AlcoholModerate + AlcoholHigh'

for (f in c(1,3,4)){
  X_denoised=NULL
  for (k in seq(1:ncol(X))){
    print(colnames(X)[k])
    formula = eval(parse(text = paste0("f",f)))
    model=lm(as.formula(formula), data=Z)
    X_denoised = cbind(X_denoised, model$residuals)
  }
  colnames(X_denoised)=colnames(X)
  rownames(X_denoised)=rownames(X)
  
  stab = readRDS(paste0("../Results/R2GUESS/sPLS_XY_f",f,"_output_",filepaths[m],".rds"))
  
  myspls = spls(X_denoised, Y, ncomp = 2,
                keepX = colSums(stab$selectedX),
                keepY = colSums(stab$selectedY))
  w = myspls$explained_variance$Y/sum(myspls$explained_variance$Y) # weight per component based on ev
  tmp = stab$selectedY[,1:2]%*%w # rank outcomes by importance
  rank = c(tmp)
  names(rank) = rownames(tmp)
  rank = sort(rank, decreasing = T)
  assign(paste0("rank_f",f), rank)
}
rank_f1
rank_f3
rank_f4
