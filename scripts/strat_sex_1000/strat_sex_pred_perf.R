
### Sumemr Project -- Stratified analysis by sex Prediction Performance
## Created by Vivian on 19 July

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
##### Load data --
for (m in 1:20){
  arr=paste0(rep(c("pahs_de", "pest_de"), each = 5), "_", 
             c("obesity", "hypertension", "diabetes","MS", "Lipid_disorder"),
             rep(c(".f",".m"),each=10))[m]
  ### Lasso
  lasso_out=readRDS(paste0("../results/strat_sex_lasso_1000/out_",arr,".rds")) # Load output
  assign(paste0("lasso_out_",arr),lasso_out) # Assign name
  lasso_calib=sum(CalibratedStableRegression(lasso_out)) # Number of selected variables
  assign(paste0("lasso_calib_",arr),lasso_calib) # Assign name
  
  lasso_sensitivities <- readRDS(paste0("../results/strat_sex_lasso_1000/sensitivities_",arr,".rds")) # Load roc (recalibrated)
  lasso_ave_sen <- rowMeans(lasso_sensitivities)
  lasso_sd_sen <- apply(lasso_sensitivities, 1, function(x) sd(x))
  lasso_u95_sen <- lasso_ave_sen + qnorm(0.975)*lasso_sd_sen
  lasso_u95_sen <- ifelse(lasso_u95_sen > 1, 1, lasso_u95_sen)
  lasso_l95_sen <- lasso_ave_sen - qnorm(0.975)*lasso_sd_sen
  lasso_l95_sen <- ifelse(lasso_l95_sen < 0, 0, lasso_l95_sen)
  lasso_sen <- cbind(lasso_ave_sen, lasso_sd_sen, lasso_u95_sen, lasso_l95_sen)
  assign(paste0("lasso_sen_",arr),lasso_sen) # Assign name
  
  lasso_specificities <- readRDS(paste0("../results/strat_sex_lasso_1000/specificities_",arr,".rds")) # Load roc (recalibrated)
  lasso_ave_fpr <- 1- rowMeans(lasso_specificities)
  lasso_sd_fpr <- apply(lasso_specificities, 1, function(x) sd(x))
  lasso_u95_fpr <- lasso_ave_fpr + qnorm(0.975)*lasso_sd_fpr
  lasso_u95_fpr <- ifelse(lasso_u95_fpr > 1, 1, lasso_u95_fpr)
  lasso_l95_fpr <- lasso_ave_fpr - qnorm(0.975)*lasso_sd_fpr
  lasso_l95_fpr <- ifelse(lasso_l95_fpr < 0, 0, lasso_l95_fpr)
  lasso_fpr <- cbind(lasso_ave_fpr, lasso_sd_fpr, lasso_u95_fpr, lasso_l95_fpr)
  assign(paste0("lasso_fpr_",arr),lasso_fpr) # Assign name
  
  lasso_myaucs <- readRDS(paste0("../results/strat_sex_lasso_1000/auc_",arr,".rds")) # Load roc (recalibrated)
  assign(paste0("lasso_auc_",arr),lasso_myaucs)
  
}

for (m in 1:20){
  arr <- paste0(rep(c("pahs_de", "pest_de"), each = 5), "_", 
                c("obesity", "hypertension", "diabetes","MS", "Lipid_disorder"),
                rep(c(".f",".m"),each=10))[m]
  ### sPLS
  spls_out <- readRDS(paste0("../results/strat_sex_spls_1000/out_",arr,".rds")) # Load output
  assign(paste0("spls_out_",arr),spls_out) # Assign name
  
  spls_calib <- sum(CalibratedStableRegression(spls_out)) # Number of selected variables
  assign(paste0("spls_calib_",arr),spls_calib) # Assign name
  
  spls_sensitivities <- readRDS(paste0("../results/strat_sex_spls_1000/sensitivities_",arr,".rds")) # Load roc (recalibrated)
  spls_ave_sen <- rowMeans(spls_sensitivities)
  spls_sd_sen <- apply(spls_sensitivities, 1, function(x) sd(x))
  spls_u95_sen <- spls_ave_sen + qnorm(0.975)*spls_sd_sen
  spls_u95_sen <- ifelse(spls_u95_sen > 1, 1, spls_u95_sen)
  spls_l95_sen <- spls_ave_sen - qnorm(0.975)*spls_sd_sen
  spls_l95_sen <- ifelse(spls_l95_sen < 0, 0, spls_l95_sen)
  spls_sen <- cbind(spls_ave_sen, spls_sd_sen, spls_u95_sen, spls_l95_sen)
  assign(paste0("spls_sen_",arr),spls_sen) # Assign name
  
  spls_specificities <- readRDS(paste0("../results/strat_sex_spls_1000/specificities_",arr,".rds")) # Load roc (recalibrated)
  spls_ave_fpr <- 1- rowMeans(spls_specificities)
  spls_sd_fpr <- apply(spls_specificities, 1, function(x) sd(x))
  spls_u95_fpr <- spls_ave_fpr + qnorm(0.975)*spls_sd_fpr
  spls_u95_fpr <- ifelse(spls_u95_fpr > 1, 1, spls_u95_fpr)
  spls_l95_fpr <- spls_ave_fpr - qnorm(0.975)*spls_sd_fpr
  spls_l95_fpr <- ifelse(spls_l95_fpr < 0, 0, spls_l95_fpr)
  spls_fpr <- cbind(spls_ave_fpr, spls_sd_fpr, spls_u95_fpr, spls_l95_fpr)
  assign(paste0("spls_fpr_",arr),spls_fpr) # Assign name
  
  spls_myaucs <- readRDS(paste0("../results/strat_sex_spls_1000/auc_",arr,".rds")) # Load roc (recalibrated)
  assign(paste0("spls_auc_",arr),spls_myaucs)
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
foo = function(sen1, sen2, sen3, sen4, sen5, sen6, sen7, sen8, sen9, sen10, 
               spe1, spe2, spe3, spe4, spe5, spe6, spe7, spe8, spe9, spe10){
  a1.coord = data.frame(fpr=spe1[,1], tpr=sen1[,1], fpr_l95=spe1[,4], tpr_l95=sen1[,4], fpr_u95=spe1[,3], tpr_u95=sen1[,3])
  a2.coord = data.frame(fpr=spe2[,1], tpr=sen2[,1], fpr_l95=spe2[,4], tpr_l95=sen2[,4], fpr_u95=spe2[,3], tpr_u95=sen2[,3])
  a3.coord = data.frame(fpr=spe3[,1], tpr=sen3[,1], fpr_l95=spe3[,4], tpr_l95=sen3[,4], fpr_u95=spe3[,3], tpr_u95=sen3[,3])
  a4.coord = data.frame(fpr=spe4[,1], tpr=sen4[,1], fpr_l95=spe4[,4], tpr_l95=sen4[,4], fpr_u95=spe4[,3], tpr_u95=sen4[,3])
  a5.coord = data.frame(fpr=spe5[,1], tpr=sen5[,1], fpr_l95=spe5[,4], tpr_l95=sen5[,4], fpr_u95=spe5[,3], tpr_u95=sen5[,3])
  a6.coord = data.frame(fpr=spe6[,1], tpr=sen6[,1], fpr_l95=spe6[,4], tpr_l95=sen6[,4], fpr_u95=spe6[,3], tpr_u95=sen6[,3])
  a7.coord = data.frame(fpr=spe7[,1], tpr=sen7[,1], fpr_l95=spe7[,4], tpr_l95=sen7[,4], fpr_u95=spe7[,3], tpr_u95=sen7[,3])
  a8.coord = data.frame(fpr=spe8[,1], tpr=sen8[,1], fpr_l95=spe8[,4], tpr_l95=sen8[,4], fpr_u95=spe8[,3], tpr_u95=sen8[,3])
  a9.coord = data.frame(fpr=spe9[,1], tpr=sen9[,1], fpr_l95=spe9[,4], tpr_l95=sen9[,4], fpr_u95=spe9[,3], tpr_u95=sen9[,3])
  a10.coord = data.frame(fpr=spe10[,1], tpr=sen10[,1], fpr_l95=spe10[,4], tpr_l95=sen10[,4], fpr_u95=spe10[,3], tpr_u95=sen10[,3])
  
  x=list(a1.coord, a2.coord, a3.coord, a4.coord, a5.coord, a6.coord, a7.coord, a8.coord, a9.coord, a10.coord)
  names(x) = c("a1","a2","a3","a4","a5","a6","a7","a8","a9","a10")
  return(x)
}

foo2 = function(l1, l2, l3, l4, l5, l6, l7, l8, l9, l10){
  l1.coord = data.frame(auc=round(mean(l1[,2]),2), auc_l95=round(mean(l1[,2])-qnorm(0.975)*sd(l1[,2]),2), auc_u95=round(mean(l1[,2])+qnorm(0.975)*sd(l1[,2]),2))
  l2.coord = data.frame(auc=round(mean(l2[,2]),2), auc_l95=round(mean(l2[,2])-qnorm(0.975)*sd(l2[,2]),2), auc_u95=round(mean(l2[,2])+qnorm(0.975)*sd(l2[,2]),2))
  l3.coord = data.frame(auc=round(mean(l3[,2]),2), auc_l95=round(mean(l3[,2])-qnorm(0.975)*sd(l3[,2]),2), auc_u95=round(mean(l3[,2])+qnorm(0.975)*sd(l3[,2]),2))
  l4.coord = data.frame(auc=round(mean(l4[,2]),2), auc_l95=round(mean(l4[,2])-qnorm(0.975)*sd(l4[,2]),2), auc_u95=round(mean(l4[,2])+qnorm(0.975)*sd(l4[,2]),2))
  l5.coord = data.frame(auc=round(mean(l5[,2]),2), auc_l95=round(mean(l5[,2])-qnorm(0.975)*sd(l5[,2]),2), auc_u95=round(mean(l5[,2])+qnorm(0.975)*sd(l5[,2]),2))
  l6.coord = data.frame(auc=round(mean(l6[,2]),2), auc_l95=round(mean(l6[,2])-qnorm(0.975)*sd(l6[,2]),2), auc_u95=round(mean(l6[,2])+qnorm(0.975)*sd(l6[,2]),2))
  l7.coord = data.frame(auc=round(mean(l7[,2]),2), auc_l95=round(mean(l7[,2])-qnorm(0.975)*sd(l7[,2]),2), auc_u95=round(mean(l7[,2])+qnorm(0.975)*sd(l7[,2]),2))
  l8.coord = data.frame(auc=round(mean(l8[,2]),2), auc_l95=round(mean(l8[,2])-qnorm(0.975)*sd(l8[,2]),2), auc_u95=round(mean(l8[,2])+qnorm(0.975)*sd(l8[,2]),2))
  l9.coord = data.frame(auc=round(mean(l9[,2]),2), auc_l95=round(mean(l8[,2])-qnorm(0.975)*sd(l9[,2]),2), auc_u95=round(mean(l9[,2])+qnorm(0.975)*sd(l9[,2]),2))
  l10.coord = data.frame(auc=round(mean(l10[,2]),2), auc_l95=round(mean(l10[,2])-qnorm(0.975)*sd(l10[,2]),2), auc_u95=round(mean(l10[,2])+qnorm(0.975)*sd(l10[,2]),2))
  x = c(paste0(l1.coord$auc," [",l1.coord$auc_l95,"-",l1.coord$auc_u95,"]"),
        paste0(l2.coord$auc," [",l2.coord$auc_l95,"-",l2.coord$auc_u95,"]"),
        paste0(l3.coord$auc," [",l3.coord$auc_l95,"-",l3.coord$auc_u95,"]"),
        paste0(l4.coord$auc," [",l4.coord$auc_l95,"-",l4.coord$auc_u95,"]"),
        paste0(l5.coord$auc," [",l5.coord$auc_l95,"-",l5.coord$auc_u95,"]"),
        paste0(l6.coord$auc," [",l6.coord$auc_l95,"-",l6.coord$auc_u95,"]"),
        paste0(l7.coord$auc," [",l7.coord$auc_l95,"-",l7.coord$auc_u95,"]"),
        paste0(l8.coord$auc," [",l8.coord$auc_l95,"-",l8.coord$auc_u95,"]"),
        paste0(l9.coord$auc," [",l9.coord$auc_l95,"-",l9.coord$auc_u95,"]"),
        paste0(l10.coord$auc," [",l10.coord$auc_l95,"-",l10.coord$auc_u95,"]"))
  return(x)
}

foo3 = function(l1, l2, l3, l4, l5, l6, l7, l8, l9, l10){
  l1.coord = data.frame(auc=round(mean(as.numeric(l1[,2])),2), 
                        auc_l95=round(mean(as.numeric(l1[,2]))-qnorm(0.975)*sd(as.numeric(l1[,2])),2), 
                        auc_u95=round(mean(as.numeric(l1[,2]))+qnorm(0.975)*sd(as.numeric(l1[,2])),2))
  l2.coord = data.frame(auc=round(mean(as.numeric(l2[,2])),2), 
                        auc_l95=round(mean(as.numeric(l2[,2]))-qnorm(0.975)*sd(as.numeric(l2[,2])),2), 
                        auc_u95=round(mean(as.numeric(l2[,2]))+qnorm(0.975)*sd(as.numeric(l2[,2])),2))
  l3.coord = data.frame(auc=round(mean(as.numeric(l3[,2])),2), 
                        auc_l95=round(mean(as.numeric(l3[,2]))-qnorm(0.975)*sd(as.numeric(l3[,2])),2), 
                        auc_u95=round(mean(as.numeric(l3[,2]))+qnorm(0.975)*sd(as.numeric(l3[,2])),2))
  l4.coord = data.frame(auc=round(mean(as.numeric(l4[,2])),2), 
                        auc_l95=round(mean(as.numeric(l4[,2]))-qnorm(0.975)*sd(as.numeric(l4[,2])),2), 
                        auc_u95=round(mean(as.numeric(l4[,2]))+qnorm(0.975)*sd(as.numeric(l4[,2])),2))
  l5.coord = data.frame(auc=round(mean(as.numeric(l5[,2])),2), 
                        auc_l95=round(mean(as.numeric(l5[,2]))-qnorm(0.975)*sd(as.numeric(l5[,2])),2), 
                        auc_u95=round(mean(as.numeric(l5[,2]))+qnorm(0.975)*sd(as.numeric(l5[,2])),2))
  l6.coord = data.frame(auc=round(mean(as.numeric(l6[,2])),2), 
                        auc_l95=round(mean(as.numeric(l6[,2]))-qnorm(0.975)*sd(as.numeric(l6[,2])),2), 
                        auc_u95=round(mean(as.numeric(l6[,2]))+qnorm(0.975)*sd(as.numeric(l6[,2])),2))
  l7.coord = data.frame(auc=round(mean(as.numeric(l7[,2])),2), 
                        auc_l95=round(mean(as.numeric(l7[,2]))-qnorm(0.975)*sd(as.numeric(l7[,2])),2), 
                        auc_u95=round(mean(as.numeric(l7[,2]))+qnorm(0.975)*sd(as.numeric(l7[,2])),2))
  l8.coord = data.frame(auc=round(mean(as.numeric(l8[,2])),2), 
                        auc_l95=round(mean(as.numeric(l8[,2]))-qnorm(0.975)*sd(as.numeric(l8[,2])),2), 
                        auc_u95=round(mean(as.numeric(l8[,2]))+qnorm(0.975)*sd(as.numeric(l8[,2])),2))
  l9.coord = data.frame(auc=round(mean(as.numeric(l9[,2])),2), 
                        auc_l95=round(mean(as.numeric(l9[,2]))-qnorm(0.975)*sd(as.numeric(l9[,2])),2), 
                        auc_u95=round(mean(as.numeric(l9[,2]))+qnorm(0.975)*sd(as.numeric(l9[,2])),2))
  l10.coord = data.frame(auc=round(mean(as.numeric(l10[,2])),2), 
                        auc_l95=round(mean(as.numeric(l10[,2]))-qnorm(0.975)*sd(as.numeric(l10[,2])),2), 
                        auc_u95=round(mean(as.numeric(l10[,2]))+qnorm(0.975)*sd(as.numeric(l10[,2])),2))
  
  x = c(paste0(l1.coord$auc," [",l1.coord$auc_l95,"-",l1.coord$auc_u95,"]"),
        paste0(l2.coord$auc," [",l2.coord$auc_l95,"-",l2.coord$auc_u95,"]"),
        paste0(l3.coord$auc," [",l3.coord$auc_l95,"-",l3.coord$auc_u95,"]"),
        paste0(l4.coord$auc," [",l4.coord$auc_l95,"-",l4.coord$auc_u95,"]"),
        paste0(l5.coord$auc," [",l5.coord$auc_l95,"-",l5.coord$auc_u95,"]"),
        paste0(l6.coord$auc," [",l6.coord$auc_l95,"-",l6.coord$auc_u95,"]"),
        paste0(l7.coord$auc," [",l7.coord$auc_l95,"-",l7.coord$auc_u95,"]"),
        paste0(l8.coord$auc," [",l8.coord$auc_l95,"-",l8.coord$auc_u95,"]"),
        paste0(l9.coord$auc," [",l9.coord$auc_l95,"-",l9.coord$auc_u95,"]"),
        paste0(l10.coord$auc," [",l10.coord$auc_l95,"-",l10.coord$auc_u95,"]"))
  return(x)
}


### lasso pest
roc_lasso_pest <- foo(lasso_sen_pest_de_obesity.f, lasso_sen_pest_de_obesity.m,
                      lasso_sen_pest_de_Lipid_disorder.f, lasso_sen_pest_de_Lipid_disorder.m, 
                      lasso_sen_pest_de_hypertension.f, lasso_sen_pest_de_hypertension.m,
                      lasso_sen_pest_de_diabetes.f, lasso_sen_pest_de_diabetes.m,
                      lasso_sen_pest_de_MS.f, lasso_sen_pest_de_MS.m,
                      lasso_fpr_pest_de_obesity.f, lasso_fpr_pest_de_obesity.m,
                      lasso_fpr_pest_de_Lipid_disorder.f, lasso_fpr_pest_de_Lipid_disorder.m, 
                      lasso_fpr_pest_de_hypertension.f,  lasso_fpr_pest_de_hypertension.m, 
                      lasso_fpr_pest_de_diabetes.f, lasso_fpr_pest_de_diabetes.m,
                      lasso_fpr_pest_de_MS.f, lasso_fpr_pest_de_MS.m)

auc_lasso_pest <- foo3(lasso_auc_pest_de_obesity.f, lasso_auc_pest_de_obesity.m, 
                       lasso_auc_pest_de_Lipid_disorder.f, lasso_auc_pest_de_Lipid_disorder.m, 
                       lasso_auc_pest_de_hypertension.f, lasso_auc_pest_de_hypertension.m, 
                       lasso_auc_pest_de_diabetes.f,  lasso_auc_pest_de_diabetes.m,
                       lasso_auc_pest_de_MS.f,  lasso_auc_pest_de_MS.m)




mycolours = brewer.pal(10, "Paired")

pdf("../figures/strat_sex_lasso_1000/roc_pest_f.pdf", width = 7, height = 7)
par(mar=c(5,5,2,1))
plot(roc_lasso_pest[[1]][,1],roc_lasso_pest[[1]][,2], ylim = c(0,1),
     col = mycolours[1], type="n", lwd = 2,
     xlab = "False Positive Rate", ylab = "True Positive Rate", cex.lab = 0.8, cex.axis = 0.8)
mtext("POPs and non-persistent pesticides, LASSO models", side=3, adj=0, line=0.5, cex=0.8, font=2)
abline(0,1, lty = 3, col = "grey")
lines(roc_lasso_pest[[1]][,1],roc_lasso_pest[[1]][,2], col = mycolours[1], type="l", lwd = 2)
lines(roc_lasso_pest[[3]][,1],roc_lasso_pest[[3]][,2], col = mycolours[3], type="l", lwd = 2)
lines(roc_lasso_pest[[5]][,1],roc_lasso_pest[[5]][,2], col = mycolours[5], type="l", lwd = 2)
lines(roc_lasso_pest[[7]][,1],roc_lasso_pest[[7]][,2], col = mycolours[7], type="l", lwd = 2)
lines(roc_lasso_pest[[9]][,1],roc_lasso_pest[[9]][,2], col = mycolours[9], type="l", lwd = 2)
legend("bottomright",
       legend = c(paste0("Obesity; AUC = ",
                         auc_lasso_pest[1]),
                  paste0("Lipid disorder; AUC = ",
                         auc_lasso_pest[3]),
                  paste0("Hypertension; AUC = ",
                         auc_lasso_pest[5]),
                  paste0("Diabetes; AUC = ",
                         auc_lasso_pest[7]),
                  paste0("Metabolic syndrome; AUC = ",
                         auc_lasso_pest[9])),
       lty = 1, lwd = 2, col = mycolours[c(1,3,5,7,9)], cex = 0.7, bty = "n")
dev.off()


pdf("../figures/strat_sex_lasso_1000/roc_pest_m.pdf", width = 7, height = 7)
par(mar=c(5,5,2,1))
plot(roc_lasso_pest[[2]][,1],roc_lasso_pest[[2]][,2], ylim = c(0,1),
     col = mycolours[2], type="n", lwd = 2,
     xlab = "False Positive Rate", ylab = "True Positive Rate", cex.lab = 0.8, cex.axis = 0.8)
mtext("POPs and non-persistent pesticides, LASSO models", side=3, adj=0, line=0.5, cex=0.8, font=2)
abline(0,1, lty = 3, col = "grey")
lines(roc_lasso_pest[[2]][,1],roc_lasso_pest[[2]][,2], col = mycolours[2], type="l", lwd = 2)
lines(roc_lasso_pest[[4]][,1][1:95],roc_lasso_pest[[4]][,2][1:95], col = mycolours[4], type="l", lwd = 2)
lines(roc_lasso_pest[[6]][,1],roc_lasso_pest[[6]][,2], col = mycolours[6], type="l", lwd = 2)
lines(roc_lasso_pest[[8]][,1],roc_lasso_pest[[8]][,2], col = mycolours[8], type="l", lwd = 2)
lines(roc_lasso_pest[[10]][,1],roc_lasso_pest[[10]][,2], col = mycolours[10], type="l", lwd = 2)
legend("bottomright",
       legend = c(paste0("Obesity; AUC = ",
                         auc_lasso_pest[2]),
                  paste0("Lipid disorder; AUC = ",
                         auc_lasso_pest[4]),
                  paste0("Hypertension; AUC = ",
                         auc_lasso_pest[6]),
                  paste0("Diabetes; AUC = ",
                         auc_lasso_pest[8]),
                  paste0("Metabolic syndrome; AUC = ",
                         auc_lasso_pest[10])),
       lty = 1, lwd = 2, col = mycolours[c(2,4,6,8,10)], cex = 0.7, bty = "n")
dev.off()




### lasso pahs
roc_lasso_pahs <- foo(lasso_sen_pahs_de_obesity.f, lasso_sen_pahs_de_obesity.m,
                      lasso_sen_pahs_de_Lipid_disorder.f, lasso_sen_pahs_de_Lipid_disorder.m, 
                      lasso_sen_pahs_de_hypertension.f, lasso_sen_pahs_de_hypertension.m,
                      lasso_sen_pahs_de_diabetes.f, lasso_sen_pahs_de_diabetes.m,
                      lasso_sen_pahs_de_MS.f, lasso_sen_pahs_de_MS.m,
                      lasso_fpr_pahs_de_obesity.f, lasso_fpr_pahs_de_obesity.m,
                      lasso_fpr_pahs_de_Lipid_disorder.f, lasso_fpr_pahs_de_Lipid_disorder.m, 
                      lasso_fpr_pahs_de_hypertension.f,  lasso_fpr_pahs_de_hypertension.m, 
                      lasso_fpr_pahs_de_diabetes.f, lasso_fpr_pahs_de_diabetes.m,
                      lasso_fpr_pahs_de_MS.f, lasso_fpr_pahs_de_MS.m)

auc_lasso_pahs <- foo3(lasso_auc_pahs_de_obesity.f, lasso_auc_pahs_de_obesity.m, 
                       lasso_auc_pahs_de_Lipid_disorder.f, lasso_auc_pahs_de_Lipid_disorder.m, 
                       lasso_auc_pahs_de_hypertension.f, lasso_auc_pahs_de_hypertension.m, 
                       lasso_auc_pahs_de_diabetes.f,  lasso_auc_pahs_de_diabetes.m,
                       lasso_auc_pahs_de_MS.f,  lasso_auc_pahs_de_MS.m)




pdf("../figures/strat_sex_lasso_1000/roc_pahs_f.pdf", width = 7, height = 7)
par(mar=c(5,5,2,1))
plot(roc_lasso_pahs[[1]][,1],roc_lasso_pahs[[1]][,2], ylim = c(0,1),
     col = mycolours[1], type="n", lwd = 2,
     xlab = "False Positive Rate", ylab = "True Positive Rate", cex.lab = 0.8, cex.axis = 0.8)
mtext("Metabolites of PAHs and tobacco-related chemicals, LASSO models", side=3, adj=0, line=0.5, cex=0.8, font=2)
abline(0,1, lty = 3, col = "grey")
lines(roc_lasso_pahs[[1]][,1],roc_lasso_pahs[[1]][,2], col = mycolours[1], type="l", lwd = 2)
lines(roc_lasso_pahs[[3]][,1],roc_lasso_pahs[[3]][,2], col = mycolours[3], type="l", lwd = 2)
lines(roc_lasso_pahs[[5]][,1],roc_lasso_pahs[[5]][,2], col = mycolours[5], type="l", lwd = 2)
lines(roc_lasso_pahs[[7]][,1],roc_lasso_pahs[[7]][,2], col = mycolours[7], type="l", lwd = 2)
lines(roc_lasso_pahs[[9]][,1],roc_lasso_pahs[[9]][,2], col = mycolours[9], type="l", lwd = 2)
legend("bottomright",
       legend = c(paste0("Obesity; AUC = ",
                         auc_lasso_pahs[1]),
                  paste0("Lipid disorder; AUC = ",
                         auc_lasso_pahs[3]),
                  paste0("Hypertension; AUC = ",
                         auc_lasso_pahs[5]),
                  paste0("Diabetes; AUC = ",
                         auc_lasso_pahs[7]),
                  paste0("Metabolic syndrome; AUC = ",
                         auc_lasso_pahs[9])),
       lty = 1, lwd = 2, col = mycolours[c(1,3,5,7,9)], cex = 0.7, bty = "n")
dev.off()


pdf("../figures/strat_sex_lasso_1000/roc_pahs_m.pdf", width = 7, height = 7)
par(mar=c(5,5,2,1))
plot(roc_lasso_pahs[[2]][,1],roc_lasso_pahs[[2]][,2], ylim = c(0,1),
     col = mycolours[2], type="n", lwd = 2,
     xlab = "False Positive Rate", ylab = "True Positive Rate", cex.lab = 0.8, cex.axis = 0.8)
mtext("Metabolites of PAHs and tobacco-related chemicals, LASSO models", side=3, adj=0, line=0.5, cex=0.8, font=2)
abline(0,1, lty = 3, col = "grey")
lines(roc_lasso_pahs[[2]][,1],roc_lasso_pahs[[2]][,2], col = mycolours[2], type="l", lwd = 2)
lines(roc_lasso_pahs[[4]][,1][1:95],roc_lasso_pahs[[4]][,2][1:95], col = mycolours[4], type="l", lwd = 2)
lines(roc_lasso_pahs[[6]][,1],roc_lasso_pahs[[6]][,2], col = mycolours[6], type="l", lwd = 2)
lines(roc_lasso_pahs[[8]][,1],roc_lasso_pahs[[8]][,2], col = mycolours[8], type="l", lwd = 2)
lines(roc_lasso_pahs[[10]][,1],roc_lasso_pahs[[10]][,2], col = mycolours[10], type="l", lwd = 2)
legend("bottomright",
       legend = c(paste0("Obesity; AUC = ",
                         auc_lasso_pahs[2]),
                  paste0("Lipid disorder; AUC = ",
                         auc_lasso_pahs[4]),
                  paste0("Hypertension; AUC = ",
                         auc_lasso_pahs[6]),
                  paste0("Diabetes; AUC = ",
                         auc_lasso_pahs[8]),
                  paste0("Metabolic syndrome; AUC = ",
                         auc_lasso_pahs[10])),
       lty = 1, lwd = 2, col = mycolours[c(2,4,6,8,10)], cex = 0.7, bty = "n")
dev.off()

