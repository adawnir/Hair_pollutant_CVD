
### Sumemr Project -- Stratified analysis by belgium Prediction Performance
## Created by Vivian on 24 July

rm(list=ls())
project_path="~/hair/scripts"
setwd(project_path)

### Loading packages----
library(RColorBrewer)
library(tidyverse)
library(plotrix)
library(colorspace)
library(pROC)

source("penalisation_functions.R")
forname <- readRDS("../processed/imputation/imp_pahs.rds")
colnames(forname) 

##### Load data --
for (m in 1:8){
  arr <- c(paste0(rep(c("pest_de"), times = 8), "_", 
                  c("obesity", "hypertension", "Lipid_disorder", "MS"), 
                  rep(c(".up",".low"), each=4)))[m]
  ### Lasso
  lasso_out=readRDS(paste0("../results/strat_belgium_lasso/out_",arr,".rds")) # Load output
  assign(paste0("lasso_out_",arr),lasso_out) # Assign name
  lasso_calib=sum(CalibratedStableRegression(lasso_out)) # Number of selected variables
  assign(paste0("lasso_calib_",arr),lasso_calib) # Assign name
  # lasso_auc_avg=readRDS(paste0("../Results/lasso/auc_average_beta_",arr,".rds")) # Load auc (average beta)
  # assign(paste0("lasso_auc_avg_",arr),lasso_auc_avg) # Assign name
  lasso_roc=readRDS(paste0("../results/strat_belgium_lasso/roc_",arr,".rds")) # Load roc (recalibrated)
  assign(paste0("lasso_roc_",arr),lasso_roc) # Assign name
  lasso_auc=readRDS(paste0("../results/strat_belgium_lasso/auc_",arr,".rds")) # Load auc (recalibrated)
  assign(paste0("lasso_auc_",arr),lasso_auc) # Assign name
  
}




##### Main analyses: LASSO (not run)
### Recalibrate beta: pest + obesity
colnames=names(lasso_out_pest_de_obesity$Beta[1,,1])
auc=lasso_auc_pest_de_obesity
calib=lasso_calib_pest_de_obesity
file_path="lasso_auc_recalib_pest_obesity"

pdf(paste0("../figures/lasso/",file_path,".pdf"), width=10, height=5)
par(mar=c(11,5,1,1))
plotCI(x=1:length(auc[,2]), y=as.numeric(auc[,2]),
       li=as.numeric(auc[,1]), ui=as.numeric(auc[,3]), sfrac = 0.001,
       xaxt="n", xlab="", pch=19, cex=0.5,
       ylab="AUC", ylim=c(0.4,1))
for (k in 1:length(auc[,2])){
  mytext=rownames(auc)[k]
  labfont=ifelse(k==calib,2,1)
  axis(side=1, at=k, labels=mytext, las=2, cex.axis=0.7,font=labfont)
}
abline(v=calib, col="darkred", lty=4, lwd=1)
abline(h=as.numeric(auc[calib,2]), col="darkred", lty=4, lwd=1)
legend("topright", lty=c(4,3), lwd=2, bty = "n",
       legend = paste0("Calibrated model; AUC = ",auc[calib,2]))
dev.off()


### Recalibrate beta: pest + LD
colnames=names(lasso_out_pest_de_Lipid_disorder$Beta[1,,1])
auc=lasso_auc_pest_de_Lipid_disorder
calib=lasso_calib_pest_de_Lipid_disorder
file_path="lasso_auc_recalib_pest_Lipid_disorder"

pdf(paste0("../figures/lasso/",file_path,".pdf"), width=10, height=5)
par(mar=c(11,5,1,1))
plotCI(x=1:length(auc[,2]), y=as.numeric(auc[,2]),
       li=as.numeric(auc[,1]), ui=as.numeric(auc[,3]), sfrac = 0.001,
       xaxt="n", xlab="", pch=19, cex=0.5,
       ylab="AUC", ylim=c(0.4,1))
for (k in 1:length(auc[,2])){
  mytext=rownames(auc)[k]
  labfont=ifelse(k==calib,2,1)
  axis(side=1, at=k, labels=mytext, las=2, cex.axis=0.7,font=labfont)
}
abline(v=calib, col="darkred", lty=4, lwd=1)
abline(h=as.numeric(auc[calib,2]), col="darkred", lty=4, lwd=1)
legend("topright", lty=c(4,3), lwd=2, bty = "n",
       legend = paste0("Calibrated model; AUC = ",auc[calib,2]))
dev.off()

### Recalibrate beta: pest + hypertension
colnames=names(lasso_out_pest_de_hypertension$Beta[1,,1])
auc=lasso_auc_pest_de_hypertension
calib=lasso_calib_pest_de_hypertension
file_path="lasso_auc_recalib_pest_hypertension"

pdf(paste0("../figures/lasso/",file_path,".pdf"), width=10, height=5)
par(mar=c(11,5,1,1))
plotCI(x=1:length(auc[,2]), y=as.numeric(auc[,2]),
       li=as.numeric(auc[,1]), ui=as.numeric(auc[,3]), sfrac = 0.001,
       xaxt="n", xlab="", pch=19, cex=0.5,
       ylab="AUC", ylim=c(0.4,1))
for (k in 1:length(auc[,2])){
  mytext=rownames(auc)[k]
  labfont=ifelse(k==calib,2,1)
  axis(side=1, at=k, labels=mytext, las=2, cex.axis=0.7,font=labfont)
}
abline(v=calib, col="darkred", lty=4, lwd=1)
abline(h=as.numeric(auc[calib,2]), col="darkred", lty=4, lwd=1)
legend("topright", lty=c(4,3), lwd=2, bty = "n",
       legend = paste0("Calibrated model; AUC = ",auc[calib,2]))
dev.off()

### Recalibrate beta: pest + diabetes
colnames=names(lasso_out_pest_de_diabetes$Beta[1,,1])
auc=lasso_auc_pest_de_diabetes
calib=lasso_calib_pest_de_diabetes
file_path="lasso_auc_recalib_pest_diabetes"

pdf(paste0("../figures/lasso/",file_path,".pdf"), width=10, height=5)
par(mar=c(11,5,1,1))
plotCI(x=1:length(auc[,2]), y=as.numeric(auc[,2]),
       li=as.numeric(auc[,1]), ui=as.numeric(auc[,3]), sfrac = 0.001,
       xaxt="n", xlab="", pch=19, cex=0.5,
       ylab="AUC", ylim=c(0,1))
for (k in 1:length(auc[,2])){
  mytext=rownames(auc)[k]
  labfont=ifelse(k==calib,2,1)
  axis(side=1, at=k, labels=mytext, las=2, cex.axis=0.7,font=labfont)
}
abline(v=calib, col="darkred", lty=4, lwd=1)
abline(h=as.numeric(auc[calib,2]), col="darkred", lty=4, lwd=1)
legend("bottomright", lty=c(4,3), lwd=2, bty = "n",
       legend = paste0("Calibrated model; AUC = ",auc[calib,2]))
dev.off()

### Recalibrate beta: pest + MS
colnames=names(lasso_out_pest_de_MS$Beta[1,,1])
auc=lasso_auc_pest_de_MS
calib=lasso_calib_pest_de_MS
file_path="lasso_auc_recalib_pest_MS"

pdf(paste0("../figures/lasso/",file_path,".pdf"), width=10, height=5)
par(mar=c(11,5,1,1))
plotCI(x=1:length(auc[,2]), y=as.numeric(auc[,2]),
       li=as.numeric(auc[,1]), ui=as.numeric(auc[,3]), sfrac = 0.001,
       xaxt="n", xlab="", pch=19, cex=0.5,
       ylab="AUC", ylim=c(0.4,1))
for (k in 1:length(auc[,2])){
  mytext=rownames(auc)[k]
  labfont=ifelse(k==calib,2,1)
  axis(side=1, at=k, labels=mytext, las=2, cex.axis=0.7,font=labfont)
}
abline(v=calib, col="darkred", lty=4, lwd=1)
abline(h=as.numeric(auc[calib,2]), col="darkred", lty=4, lwd=1)
legend("topright", lty=c(4,3), lwd=2, bty = "n",
       legend = paste0("Calibrated model; AUC = ",auc[calib,2]))
dev.off()

### Recalibrate beta: pahs + obesity
auc=lasso_auc_pahs_de_obesity
calib=lasso_calib_pahs_de_obesity
file_path="lasso_auc_recalib_pahs_obesity"

pdf(paste0("../figures/lasso/",file_path,".pdf"), width=10, height=5)
par(mar=c(11,5,1,1))
plotCI(x=1:length(auc[,2]), y=as.numeric(auc[,2]),
       li=as.numeric(auc[,1]), ui=as.numeric(auc[,3]), sfrac = 0.001,
       xaxt="n", xlab="", pch=19, cex=0.5,
       ylab="AUC", ylim=c(0.3,1))
for (k in 1:length(auc[,2])){
  mytext=rownames(auc)[k]
  labfont=ifelse(k==calib,2,1)
  axis(side=1, at=k, labels=mytext, las=2, cex.axis=0.7,font=labfont)
}
abline(v=calib, col="darkred", lty=4, lwd=1)
abline(h=as.numeric(auc[calib,2]), col="darkred", lty=4, lwd=1)
legend("topright", lty=c(4,3), lwd=2, bty = "n",
       legend = paste0("Calibrated model; AUC = ",auc[calib,2]))
dev.off()


### Recalibrate beta: pahs + hypertension
colnames=names(lasso_out_pahs_de_hypertension$Beta[1,,1])
auc=lasso_auc_pahs_de_hypertension
calib=lasso_calib_pahs_de_hypertension
file_path="lasso_auc_recalib_pahs_hypertension"

pdf(paste0("../figures/lasso/",file_path,".pdf"), width=10, height=5)
par(mar=c(11,5,1,1))
plotCI(x=1:length(auc[,2]), y=as.numeric(auc[,2]),
       li=as.numeric(auc[,1]), ui=as.numeric(auc[,3]), sfrac = 0.001,
       xaxt="n", xlab="", pch=19, cex=0.5,
       ylab="AUC", ylim=c(0.4,1))
for (k in 1:length(auc[,2])){
  mytext=rownames(auc)[k]
  labfont=ifelse(k==calib,2,1)
  axis(side=1, at=k, labels=mytext, las=2, cex.axis=0.7,font=labfont)
}
abline(v=calib, col="darkred", lty=4, lwd=1)
abline(h=as.numeric(auc[calib,2]), col="darkred", lty=4, lwd=1)
legend("topright", lty=c(4,3), lwd=2, bty = "n",
       legend = paste0("Calibrated model; AUC = ",auc[calib,2]))
dev.off()

### Recalibrate beta: pahs + diabetes
colnames=names(lasso_out_pahs_de_diabetes$Beta[1,,1])
auc=lasso_auc_pahs_de_diabetes
calib=lasso_calib_pahs_de_diabetes
file_path="lasso_auc_recalib_pahs_diabetes"

pdf(paste0("../figures/lasso/",file_path,".pdf"), width=10, height=5)
par(mar=c(11,5,1,1))
plotCI(x=1:length(auc[,2]), y=as.numeric(auc[,2]),
       li=as.numeric(auc[,1]), ui=as.numeric(auc[,3]), sfrac = 0.001,
       xaxt="n", xlab="", pch=19, cex=0.5,
       ylab="AUC", ylim=c(0,1))
for (k in 1:length(auc[,2])){
  mytext=rownames(auc)[k]
  labfont=ifelse(k==calib,2,1)
  axis(side=1, at=k, labels=mytext, las=2, cex.axis=0.7,font=labfont)
}
abline(v=calib, col="darkred", lty=4, lwd=1)
abline(h=as.numeric(auc[calib,2]), col="darkred", lty=4, lwd=1)
legend("bottomright", lty=c(4,3), lwd=2, bty = "n",
       legend = paste0("Calibrated model; AUC = ",auc[calib,2]))
dev.off()

### Recalibrate beta: pahs + MS
colnames=names(lasso_out_pahs_de_MS$Beta[1,,1])
auc=lasso_auc_pahs_de_MS
calib=lasso_calib_pahs_de_MS
file_path="lasso_auc_recalib_pahs_MS"

pdf(paste0("../figures/lasso/",file_path,".pdf"), width=10, height=5)
par(mar=c(11,5,1,1))
plotCI(x=1:length(auc[,2]), y=as.numeric(auc[,2]),
       li=as.numeric(auc[,1]), ui=as.numeric(auc[,3]), sfrac = 0.001,
       xaxt="n", xlab="", pch=19, cex=0.5,
       ylab="AUC", ylim=c(0.3,1))
for (k in 1:length(auc[,2])){
  mytext=rownames(auc)[k]
  labfont=ifelse(k==calib,2,1)
  axis(side=1, at=k, labels=mytext, las=2, cex.axis=0.7,font=labfont)
}
abline(v=calib, col="darkred", lty=4, lwd=1)
abline(h=as.numeric(auc[calib,2]), col="darkred", lty=4, lwd=1)
legend("topright", lty=c(4,3), lwd=2, bty = "n",
       legend = paste0("Calibrated model; AUC = ",auc[calib,2]))
dev.off()



##### Main analyses: LASSO --
### function
foo = function(l1, l2, l3, l4, l5, l6, l7, l8, l9, l10, l11, l12, l13, l14, l15){
  l1.coord = data.frame(fpr=1-l1$specificities,tpr=l1$sensitivities)
  l2.coord = data.frame(fpr=1-l2$specificities,tpr=l2$sensitivities)
  l3.coord = data.frame(fpr=1-l3$specificities,tpr=l3$sensitivities)
  l4.coord = data.frame(fpr=1-l4$specificities,tpr=l4$sensitivities)
  l5.coord = data.frame(fpr=1-l5$specificities,tpr=l5$sensitivities)
  l6.coord = data.frame(fpr=1-l6$specificities,tpr=l6$sensitivities)
  l7.coord = data.frame(fpr=1-l7$specificities,tpr=l7$sensitivities)
  l8.coord = data.frame(fpr=1-l8$specificities,tpr=l8$sensitivities)
  l9.coord = data.frame(fpr=1-l9$specificities,tpr=l9$sensitivities)
  l10.coord = data.frame(fpr=1-l10$specificities,tpr=l10$sensitivities)
  l11.coord = data.frame(fpr=1-l11$specificities,tpr=l11$sensitivities)
  l12.coord = data.frame(fpr=1-l12$specificities,tpr=l12$sensitivities)
  l13.coord = data.frame(fpr=1-l13$specificities,tpr=l13$sensitivities)
  l14.coord = data.frame(fpr=1-l14$specificities,tpr=l14$sensitivities)
  l15.coord = data.frame(fpr=1-l15$specificities,tpr=l15$sensitivities)
  x=list(l1.coord, l2.coord, l3.coord, l4.coord, l5.coord, 
         l6.coord, l7.coord, l8.coord, l9.coord, l10.coord,
         l11.coord, l12.coord, l13.coord, l14.coord, l15.coord)
  names(x) = c("l1","l2","l3","l4","l5","l6","l7","l8","l9","l10", "l11","l12","l13","l14","l15")
  return(x)
}

foo = function(l1, l2, l3, l4, l5, l6, l9, l10){
  l1.coord = data.frame(fpr=1-l1$specificities,tpr=l1$sensitivities)
  l2.coord = data.frame(fpr=1-l2$specificities,tpr=l2$sensitivities)
  l3.coord = data.frame(fpr=1-l3$specificities,tpr=l3$sensitivities)
  l4.coord = data.frame(fpr=1-l4$specificities,tpr=l4$sensitivities)
  l5.coord = data.frame(fpr=1-l5$specificities,tpr=l5$sensitivities)
  l6.coord = data.frame(fpr=1-l6$specificities,tpr=l6$sensitivities)

  l9.coord = data.frame(fpr=1-l9$specificities,tpr=l9$sensitivities)
  l10.coord = data.frame(fpr=1-l10$specificities,tpr=l10$sensitivities)
  x=list(l1.coord, l2.coord, l3.coord, l4.coord, l5.coord, 
         l6.coord,  l9.coord, l10.coord)
  names(x) = c("l1","l2","l3","l4","l5","l6", "l9","l10")
  return(x)
}

### Calculate AUC and confidence intervals
foo2 = function(l1, l2, l3, l4, l5, l6, l7, l8, l9, l10, l11, l12, l13, l14, l15
                , l1_ca, l2_ca, l3_ca, l4_ca, l5_ca, l6_ca, l7_ca, l8_ca, l9_ca, l10_ca,
                l11_ca, l12_ca, l13_ca, l14_ca, l15_ca){
  l1 = round(as.numeric(l1[l1_ca,]),2)
  l2 = round(as.numeric(l2[l2_ca,]),2)
  l3 = round(as.numeric(l3[l3_ca,]),2)
  l4 = round(as.numeric(l4[l4_ca,]),2)
  l5 = round(as.numeric(l5[l5_ca,]),2)
  l6 = round(as.numeric(l6[l6_ca,]),2)
  l7 = round(as.numeric(l7[l7_ca,]),2)
  l8 = round(as.numeric(l8[l8_ca,]),2)
  l9 = round(as.numeric(l9[l9_ca,]),2)
  l10 = round(as.numeric(l10[l10_ca,]),2)
  l11 = round(as.numeric(l11[l11_ca,]),2)
  l12 = round(as.numeric(l12[l12_ca,]),2)
  l13 = round(as.numeric(l13[l13_ca,]),2)
  l14 = round(as.numeric(l14[l14_ca,]),2)
  l15 = round(as.numeric(l15[l15_ca,]),2)
  x=c(paste0(l1[2]," [",l1[1],"-",l1[3],"]"),
      paste0(l2[2]," [",l2[1],"-",l2[3],"]"),
      paste0(l3[2]," [",l3[1],"-",l3[3],"]"),
      paste0(l4[2]," [",l4[1],"-",l4[3],"]"),
      paste0(l5[2]," [",l5[1],"-",l5[3],"]"),
      paste0(l6[2]," [",l6[1],"-",l6[3],"]"),
      paste0(l7[2]," [",l7[1],"-",l7[3],"]"),
      paste0(l8[2]," [",l8[1],"-",l8[3],"]"),
      paste0(l9[2]," [",l9[1],"-",l9[3],"]"),
      paste0(l10[2]," [",l10[1],"-",l10[3],"]"),
      paste0(l11[2]," [",l11[1],"-",l11[3],"]"),
      paste0(l12[2]," [",l12[1],"-",l12[3],"]"),
      paste0(l13[2]," [",l13[1],"-",l13[3],"]"),
      paste0(l14[2]," [",l14[1],"-",l14[3],"]"),
      paste0(l15[2]," [",l15[1],"-",l15[3],"]"))
}

foo2 = function(l1, l2, l3, l4, l5, l6, l9, l10,
                l1_ca, l2_ca, l3_ca, l4_ca, l5_ca, l6_ca, l9_ca, l10_ca){
  l1 = round(as.numeric(l1[l1_ca,]),2)
  l2 = round(as.numeric(l2[l2_ca,]),2)
  l3 = round(as.numeric(l3[l3_ca,]),2)
  l4 = round(as.numeric(l4[l4_ca,]),2)
  l5 = round(as.numeric(l5[l5_ca,]),2)
  l6 = round(as.numeric(l6[l6_ca,]),2)

  l9 = round(as.numeric(l9[l9_ca,]),2)
  l10 = round(as.numeric(l10[l10_ca,]),2)
  x=c(paste0(l1[2]," [",l1[1],"-",l1[3],"]"),
      paste0(l2[2]," [",l2[1],"-",l2[3],"]"),
      paste0(l3[2]," [",l3[1],"-",l3[3],"]"),
      paste0(l4[2]," [",l4[1],"-",l4[3],"]"),
      paste0(l5[2]," [",l5[1],"-",l5[3],"]"),
      paste0(l6[2]," [",l6[1],"-",l6[3],"]"),
      
      paste0(l9[2]," [",l9[1],"-",l9[3],"]"),
      paste0(l10[2]," [",l10[1],"-",l10[3],"]"))
}


### pest
roc_pest=foo(lasso_roc_pest_de_obesity.low, lasso_roc_pest_de_obesity.up, 
             lasso_roc_pest_de_Lipid_disorder.low, lasso_roc_pest_de_Lipid_disorder.up, 
             lasso_roc_pest_de_hypertension.low, lasso_roc_pest_de_hypertension.up, 
             lasso_roc_pest_de_MS.low, lasso_roc_pest_de_MS.up)

auc_pest=foo2(lasso_auc_pest_de_obesity.low, lasso_auc_pest_de_obesity.up,
              lasso_auc_pest_de_Lipid_disorder.low, lasso_auc_pest_de_Lipid_disorder.up, 
              lasso_auc_pest_de_hypertension.low, lasso_auc_pest_de_hypertension.up, 
              lasso_auc_pest_de_MS.low, lasso_auc_pest_de_MS.up,
              lasso_calib_pest_de_obesity.low, lasso_calib_pest_de_obesity.up, 
              lasso_calib_pest_de_Lipid_disorder.low, lasso_calib_pest_de_Lipid_disorder.up, 
              lasso_calib_pest_de_hypertension.low, lasso_calib_pest_de_hypertension.up, 
              lasso_calib_pest_de_MS.low, lasso_calib_pest_de_MS.up)


mycolours = brewer.pal(10, "Paired")
colors1 <- RColorBrewer::brewer.pal(10, "RdBu")[c(7, 9:10)]
colors2 <- RColorBrewer::brewer.pal(10, "PRGn")[c(7, 9:10)]
colors3 <- colorspace::diverge_hsv(12)[c(9, 11:12)]
colors4 <-  RColorBrewer::brewer.pal(5, "Oranges")[c(2,4:5)]
colors5 <-  RColorBrewer::brewer.pal(10, "PuOr")[c(7, 9:10)]
mycolours <- c(colors1, colors2, colors3, colors4, colors5)

pdf("../figures/strat_belgium_lasso/roc_pest.pdf", width = 7, height = 7)
par(mar=c(5,5,1,1))
plot(roc_pest[[1]], ylim = c(0,1),
     col = mycolours[1], type="n", lwd = 2,
     xlab = "False Positive Rate", ylab = "True Positive Rate")
abline(0,1, lty = 3, col = "grey")
lines(roc_pest[[1]], col = mycolours[1], type="l", lwd = 2)
lines(roc_pest[[2]], col = mycolours[2], type="l", lwd = 2)
lines(roc_pest[[3]], col = mycolours[3], type="l", lwd = 2)
lines(roc_pest[[4]], col = mycolours[4], type="l", lwd = 2)
lines(roc_pest[[5]], col = mycolours[5], type="l", lwd = 2)
lines(roc_pest[[6]], col = mycolours[6], type="l", lwd = 2)
lines(roc_pest[[7]], col = mycolours[9], type="l", lwd = 2)
lines(roc_pest[[8]], col = mycolours[10], type="l", lwd = 2)
legend("bottomright",
       legend = c(paste0("Belgium population 1: Obesity; AUC = ",
                         auc_pest[1]),
                  paste0("Belgium population 2: Obesity; AUC = ",
                         auc_pest[2]),
                  paste0("Belgium population 1: Lipid disorder; AUC = ",
                         auc_pest[3]),
                  paste0("Belgium population 2: Lipid disorder; AUC = ",
                         auc_pest[4]),
                  paste0("Belgium population 1: Hypertension; AUC = ",
                         auc_pest[5]),
                  paste0("Belgium population 2: Hypertension; AUC = ",
                         auc_pest[6]),
                  paste0("Belgium population 1: Metabolic symdrone; AUC = ",
                         auc_pest[7]),
                  paste0("Belgium population 2: Metabolic symdrone; AUC = ",
                         auc_pest[8])),
       lty = 1, lwd = 2, col = mycolours[c(1:6, 9:10)], cex = 0.6, bty = "n")
dev.off()


