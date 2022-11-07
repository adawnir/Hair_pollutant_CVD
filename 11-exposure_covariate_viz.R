## Univariate analyses: association between exposures and covariates

rm(list=ls())
project_path=dirname(rstudioapi::getActiveDocumentContext()$path)
setwd(project_path)

library(tidyverse)
library(plotrix)
library(colorspace)
source("graph_colours.R")
filepaths = c("PesticidesPCBBDEs_train","PAHs_train","Overlap_test")

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
  
  pdf(paste0("../Figures/Univariate/",root,path,"_",filepaths[i],".pdf"), width = wid, height = 6)
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

# Unadjusted: exposures and covariates ----
i = 1

# Load data
pvals = readRDS(paste0("../Results/Univariate/X_Z_pvals_",filepaths[i],".rds"))
betas = readRDS(paste0("../Results/Univariate/X_Z_betas_",filepaths[i],".rds"))
se = readRDS(paste0("../Results/Univariate/X_Z_se_",filepaths[i],".rds"))
colnames(betas)
bonf = 0.05/nrow(betas)
annot_sub = annot[rownames(betas)]
root = "X_Z_"


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

# Education (+ Age + Gender) ----
i = 1

# Load data
pvals = readRDS(paste0("../Results/Univariate/X_Education_f0_pvals_",filepaths[i],".rds"))
betas = readRDS(paste0("../Results/Univariate/X_Education_f0_betas_",filepaths[i],".rds"))
se = readRDS(paste0("../Results/Univariate/X_Education_f0_se_",filepaths[i],".rds"))
colnames(betas)
bonf = 0.05/nrow(betas)
annot_sub = annot[rownames(betas)]
root = "X_Education_f0_"

j = grep("Education",colnames(betas))
label = "Education (ref. High)"
path = "Education"
mycolour = rep(c("#dc251f","#994517"),each=nrow(betas))

VocanoPlot(j, label, path, mycolour)

## Education (+ Age + Gender + Centre) ----
# Load data
pvals1 = readRDS(paste0("../Results/Univariate/X_Education_f1_pvals_",filepaths[i],".rds"))
betas1 = readRDS(paste0("../Results/Univariate/X_Education_f1_betas_",filepaths[i],".rds"))
se1 = readRDS(paste0("../Results/Univariate/X_Education_f1_se_",filepaths[i],".rds"))
colnames(betas1)
bonf = 0.05/nrow(betas)
annot_sub = annot[rownames(betas)]
root = "X_Education_f1_"

j = grep("Education",colnames(betas1))
label = "Education (ref. High)"
path = "Education"
mycolour = rep(c("#dc251f","#994517"),each=nrow(betas1))

pvals_sub = pvals[,j]
betas_sub = betas[,j]
names(betas_sub) = rep(rownames(betas),times = length(j))

condition = pvals_sub < bonf | pvals_sub1 < bonf 
leg = ifelse(sum(condition)>0,10*ceiling(sum(condition)/25),1)
wid = ifelse(sum(condition)>0,6+2*ceiling(sum(condition)/25),6)

pvals_sub1 = pvals1[,j]
betas_sub1 = betas1[,j]
names(betas_sub1) = rep(rownames(betas1),times = length(j))
se_sub1 = se1[,j]

li1 = betas_sub1-qnorm(0.975)*se_sub1
ui1 = betas_sub1+qnorm(0.975)*se_sub1

pdf(paste0("../Figures/Univariate/",root,path,"_",filepaths[i],".pdf"), width = wid, height = 6)
par(mar=c(5,5,5,leg), pty="s")
plot(0,
     ylab=expression(-log[10](italic(p))),
     xlab=expression(beta),
     main=label,
     cex.lab=1.2,
     ylim = c(0, max(-log10(pvals_sub1), bonf)*1.25),
     xlim = c(-1,1)*max(abs(c(li1,ui1)))*1.05,type = "n")
abline(h=axTicks(2), col="grey", lty=3)
abline(v=axTicks(1), col="grey", lty=3)
plotCI(betas_sub1, -log10(pvals_sub1),
       li = ifelse(condition, li1, betas_sub1),
       ui = ifelse(condition, ui1, betas_sub1),
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
    if(pvals_sub[k] < bonf | pvals_sub1[k] < bonf){
      text(betas_sub1[k], -log10(pvals_sub1[k]),
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

# Education (+ Age + Gender + ...) ----
i = 1

# Load data
pvals1 = readRDS(paste0("../Results/Univariate/X_Education_f0_pvals_",filepaths[i],".rds"))
betas1 = readRDS(paste0("../Results/Univariate/X_Education_f0_betas_",filepaths[i],".rds"))
se1 = readRDS(paste0("../Results/Univariate/X_Education_f0_se_",filepaths[i],".rds"))

for (f in 0:3){
  pvals = readRDS(paste0("../Results/Univariate/X_Education_Z_f",f,"_pvals_",filepaths[i],".rds"))
  betas = readRDS(paste0("../Results/Univariate/X_Education_Z_f",f,"_betas_",filepaths[i],".rds"))
  se = readRDS(paste0("../Results/Univariate/X_Education_Z_f",f,"_se_",filepaths[i],".rds"))
  
  assign(paste0("pvals",f+2), pvals)
  assign(paste0("betas",f+2), betas)
  assign(paste0("se",f+2), se)
}


colnames(betas)
bonf = 0.05/nrow(betas)
# annot_sub = annot[rownames(betas)]
root = "X_Education_Z_"
model = c("Education (+ Age + Gender)", "+ Smoking status", "+ Dietary intake",
          "+ Physical activity", "+ Alcohol consumption")
mycolour = c("#dc251f","#994517")

ForestPlot = function(f){
  betas_sub = betas[j,]
  se_sub = se[j,]
  li = betas_sub-qnorm(0.975)*se_sub
  ui = betas_sub+qnorm(0.975)*se_sub
  
  plotCI(x = betas_sub, y = c(0.25, 0.75), li = li, ui = ui, err = "x",
         pch = 19, col = mycolour, cex = 3,
         xlim = c(-1,1)*max(abs(c(li,ui))),
         ylim = c(0,1),
         xaxt = "n", yaxt = "n", xlab = "", ylab = "")
  abline(v = 0, col = "black", lty = 2)
  coord = par("usr")
  mtext(model[f], cex = 0.8, side = 2, line = 1, outer = FALSE, las = 1)
}

ManhattPlot = function(f){
  pvals_sub = pvals[j,]
  plot(x = -log10(pvals_sub), y = c(0.25, 0.75),
       pch = 19, col = mycolour, cex = 3,
       xlim = c(0, 5),
       ylim = c(0,1),
       xaxt = "n", yaxt = "n", xlab = "", ylab = "")
  abline(v = -log10(bonf), col = "darkred", lty = 2)
}

## HCB ----
j = grep("HCB",rownames(betas))
label = "HCB"


ifelse(dir.exists("../Figures/Univariate/"),"",dir.create("../Figures/Univariate/"))
pdf(paste0("../Figures/Univariate/",root,label,"_",filepaths[i],".pdf"), height = 6)
par(mar=c(0,0,0,0), oma=c(3,15,6,1), mfrow = c(5,2), pty="m")
for (f in 1:5){
  pvals = eval(parse(text = paste0("pvals",f)))
  betas = eval(parse(text = paste0("betas",f)))
  se = eval(parse(text = paste0("se",f)))
  ForestPlot(f)
  if(f == 1){
    mtext("Beta coeficient (95% CI)", cex = 0.8, side = 3, line = 1, outer = FALSE)
  }
  if(f==5){
    axis(side=1, at=axTicks(1))
  }
  ManhattPlot(f)
  if(f == 1){
    mtext(expression(-log[10](italic(p))), cex = 0.8, side = 3, line = 1, outer = FALSE)
    legend("topright",
           legend = gsub("Education","",colnames(betas)),
           pch = 19,
           col = mycolour)
  }
  if(f==5){
    axis(side=1, at=axTicks(1))
  }
  mtext(label, cex = 1.2, side = 3, line = 3, outer = TRUE)
}
dev.off()

## Chlorpyrifos ----
j = grep("Chlorpyrifos",rownames(betas))
label = "Chlorpyrifos"

ifelse(dir.exists("../Figures/Univariate/"),"",dir.create("../Figures/Univariate/"))
pdf(paste0("../Figures/Univariate/",root,label,"_",filepaths[i],".pdf"), height = 6)
par(mar=c(0,0,0,0), oma=c(3,15,6,1), mfrow = c(5,2), pty="m")
for (f in 1:5){
  pvals = eval(parse(text = paste0("pvals",f)))
  betas = eval(parse(text = paste0("betas",f)))
  se = eval(parse(text = paste0("se",f)))
  ForestPlot(f)
  if(f == 1){
    mtext("Beta coeficient (95% CI)", cex = 0.8, side = 3, line = 1, outer = FALSE)
  }
  if(f==5){
    axis(side=1, at=axTicks(1))
  }
  ManhattPlot(f)
  if(f == 1){
    mtext(expression(-log[10](italic(p))), cex = 0.8, side = 3, line = 1, outer = FALSE)
    legend("topright",
           legend = gsub("Education","",colnames(betas)),
           pch = 19,
           col = mycolour)
  }
  if(f==5){
    axis(side=1, at=axTicks(1))
  }
  mtext(label, cex = 1.2, side = 3, line = 3, outer = TRUE)
}
dev.off()

## gamma-HCH ----
j = grep("gamma-HCH",rownames(betas))
label = "gamma-HCH"

ifelse(dir.exists("../Figures/Univariate/"),"",dir.create("../Figures/Univariate/"))
pdf(paste0("../Figures/Univariate/",root,label,"_",filepaths[i],".pdf"), height = 6)
par(mar=c(0,0,0,0), oma=c(3,15,6,1), mfrow = c(5,2), pty="m")
for (f in 1:5){
  pvals = eval(parse(text = paste0("pvals",f)))
  betas = eval(parse(text = paste0("betas",f)))
  se = eval(parse(text = paste0("se",f)))
  ForestPlot(f)
  if(f == 1){
    mtext("Beta coeficient (95% CI)", cex = 0.8, side = 3, line = 1, outer = FALSE)
  }
  if(f==5){
    axis(side=1, at=axTicks(1))
  }
  ManhattPlot(f)
  if(f == 1){
    mtext(expression(-log[10](italic(p))), cex = 0.8, side = 3, line = 1, outer = FALSE)
    legend("topright",
           legend = gsub("Education","",colnames(betas)),
           pch = 19,
           col = mycolour)
  }
  if(f==5){
    axis(side=1, at=axTicks(1))
  }
  mtext(label, cex = 1.2, side = 3, line = 3, outer = TRUE)
}
dev.off()

# Tobacco: Education (+ Age + Gender + ...) ----
i = 2

# Load data
pvals1 = readRDS(paste0("../Results/Univariate/X_Education_f0_pvals_",filepaths[i],".rds"))
betas1 = readRDS(paste0("../Results/Univariate/X_Education_f0_betas_",filepaths[i],".rds"))
se1 = readRDS(paste0("../Results/Univariate/X_Education_f0_se_",filepaths[i],".rds"))

for (f in 0:3){
  pvals = readRDS(paste0("../Results/Univariate/X_Education_Z_f",f,"_pvals_",filepaths[i],".rds"))
  betas = readRDS(paste0("../Results/Univariate/X_Education_Z_f",f,"_betas_",filepaths[i],".rds"))
  se = readRDS(paste0("../Results/Univariate/X_Education_Z_f",f,"_se_",filepaths[i],".rds"))
  
  assign(paste0("pvals",f+2), pvals)
  assign(paste0("betas",f+2), betas)
  assign(paste0("se",f+2), se)
}

bonf = 0.05/2
# annot_sub = annot[rownames(betas)]
root = "X_Education_Z_"
model = c("Education (+ Age + Gender)", "+ Smoking status", "+ Dietary intake",
          "+ Physical activity", "+ Alcohol consumption")
mycolour = c("#dc251f","#994517")

ForestPlot = function(f){
  betas_sub = betas[j,]
  se_sub = se[j,]
  li = betas_sub-qnorm(0.975)*se_sub
  ui = betas_sub+qnorm(0.975)*se_sub
  
  plotCI(x = betas_sub, y = c(0.25, 0.75), li = li, ui = ui, err = "x",
         pch = 19, col = mycolour, cex = 3,
         xlim = c(-1,1)*max(abs(c(li,ui))),
         ylim = c(0,1),
         xaxt = "n", yaxt = "n", xlab = "", ylab = "")
  abline(v = 0, col = "black", lty = 2)
  coord = par("usr")
  mtext(model[f], cex = 0.8, side = 2, line = 1, outer = FALSE, las = 1)
}

ManhattPlot = function(f){
  pvals_sub = pvals[j,]
  plot(x = -log10(pvals_sub), y = c(0.25, 0.75),
       pch = 19, col = mycolour, cex = 3,
       xlim = c(0, 7),
       ylim = c(0,1),
       xaxt = "n", yaxt = "n", xlab = "", ylab = "")
  abline(v = -log10(bonf), col = "darkred", lty = 2)
}

## Cotinine ----
j = grep("Cotinine",rownames(betas))
label = "Cotinine"


ifelse(dir.exists("../Figures/Univariate/"),"",dir.create("../Figures/Univariate/"))
pdf(paste0("../Figures/Univariate/",root,label,"_",filepaths[i],".pdf"), height = 6)
par(mar=c(0,0,0,0), oma=c(3,15,6,1), mfrow = c(5,2), pty="m")
for (f in 1:5){
  pvals = eval(parse(text = paste0("pvals",f)))
  betas = eval(parse(text = paste0("betas",f)))
  se = eval(parse(text = paste0("se",f)))
  ForestPlot(f)
  if(f == 1){
    mtext("Beta coeficient (95% CI)", cex = 0.8, side = 3, line = 1, outer = FALSE)
  }
  if(f==5){
    axis(side=1, at=axTicks(1))
  }
  ManhattPlot(f)
  if(f == 1){
    mtext(expression(-log[10](italic(p))), cex = 0.8, side = 3, line = 1, outer = FALSE)
    legend("topright",
           legend = gsub("Education","",colnames(betas)),
           pch = 19,
           col = mycolour)
  }
  if(f==5){
    axis(side=1, at=axTicks(1))
  }
  mtext(label, cex = 1.2, side = 3, line = 3, outer = TRUE)
}
dev.off()

## Nicotine ----
j = grep("Nicotine",rownames(betas))
label = "Nicotine"

ifelse(dir.exists("../Figures/Univariate/"),"",dir.create("../Figures/Univariate/"))
pdf(paste0("../Figures/Univariate/",root,label,"_",filepaths[i],".pdf"), height = 6)
par(mar=c(0,0,0,0), oma=c(3,15,6,1), mfrow = c(5,2), pty="m")
for (f in 1:5){
  pvals = eval(parse(text = paste0("pvals",f)))
  betas = eval(parse(text = paste0("betas",f)))
  se = eval(parse(text = paste0("se",f)))
  ForestPlot(f)
  if(f == 1){
    mtext("Beta coeficient (95% CI)", cex = 0.8, side = 3, line = 1, outer = FALSE)
  }
  if(f==5){
    axis(side=1, at=axTicks(1))
  }
  ManhattPlot(f)
  if(f == 1){
    mtext(expression(-log[10](italic(p))), cex = 0.8, side = 3, line = 1, outer = FALSE)
    legend("topright",
           legend = gsub("Education","",colnames(betas)),
           pch = 19,
           col = mycolour)
  }
  if(f==5){
    axis(side=1, at=axTicks(1))
  }
  mtext(label, cex = 1.2, side = 3, line = 3, outer = TRUE)
}
dev.off()
