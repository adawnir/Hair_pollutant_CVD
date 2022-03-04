
### Sumemr Project -- Multivariate analyses Prediction Performance
## Created by Vivian on 22 July

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
for (m in 1:5){
  arr=paste0(rep(c("pest_comb_de"), each = 5), "_", 
             c("obesity", "hypertension", "diabetes","MS", "LD"))[m]
  ### Lasso
  lasso_out=readRDS(paste0("../cat_results/lasso/comb/out_",arr,".rds")) # Load output
  assign(paste0("lasso_out_",arr),lasso_out) # Assign name
  lasso_calib=sum(CalibratedStableRegression(lasso_out)) # Number of selected variables
  assign(paste0("lasso_calib_",arr),lasso_calib) # Assign name
  # lasso_auc_avg=readRDS(paste0("../Results/lasso/auc_average_beta_",arr,".rds")) # Load auc (average beta)
  # assign(paste0("lasso_auc_avg_",arr),lasso_auc_avg) # Assign name
  lasso_roc=readRDS(paste0("../cat_results/lasso/comb/roc_recalib_beta_",arr,".rds")) # Load roc (recalibrated)
  assign(paste0("lasso_roc_",arr),lasso_roc) # Assign name
  lasso_auc=readRDS(paste0("../cat_results/lasso/comb/auc_recalib_beta_",arr,".rds")) # Load auc (recalibrated)
  assign(paste0("lasso_auc_",arr),lasso_auc) # Assign name
  
}

for (m in 1:4){
  arr=paste0(rep(c("pahs_comb_de"), each = 4), "_", 
             c("obesity", "hypertension", "diabetes","MS"))[m]
  ### Lasso
  lasso_out=readRDS(paste0("../cat_results/lasso/comb/out_",arr,".rds")) # Load output
  assign(paste0("lasso_out_",arr),lasso_out) # Assign name
  lasso_calib=sum(CalibratedStableRegression(lasso_out)) # Number of selected variables
  assign(paste0("lasso_calib_",arr),lasso_calib) # Assign name
  # lasso_auc_avg=readRDS(paste0("../Results/lasso/auc_average_beta_",arr,".rds")) # Load auc (average beta)
  # assign(paste0("lasso_auc_avg_",arr),lasso_auc_avg) # Assign name
  lasso_roc=readRDS(paste0("../cat_results/lasso/comb/roc_recalib_beta_",arr,".rds")) # Load roc (recalibrated)
  assign(paste0("lasso_roc_",arr),lasso_roc) # Assign name
  lasso_auc=readRDS(paste0("../cat_results/lasso/comb/auc_recalib_beta_",arr,".rds")) # Load auc (recalibrated)
  assign(paste0("lasso_auc_",arr),lasso_auc) # Assign name
}


for (m in 1:5){
  arr=paste0(rep(c("pest_comb_de"), each = 5), "_", 
             c("obesity", "hypertension", "diabetes", "LD", "MS"))[m]
  
  ### sPLS
  spls_out=readRDS(paste0("../cat_results/spls_uni/comb/out_",arr,".rds")) # Load output
  assign(paste0("spls_out_",arr),spls_out) # Assign name
  spls_calib=sum(CalibratedStableRegression(spls_out)) # Number of selected variables
  assign(paste0("spls_calib_",arr),spls_calib) # Assign name
  spls_roc=readRDS(paste0("../cat_results/spls_uni/comb/roc_recali_",arr,".rds")) # Load roc (recalibrated)
  assign(paste0("spls_roc_",arr),spls_roc) # Assign name
}

for (m in 1:4){
  arr=paste0(rep(c("pahs_comb_de"), each = 4), "_", 
             c("hypertension", "diabetes", "LD", "MS"))[m]
  
  ### sPLS
  spls_out=readRDS(paste0("../cat_results/spls_uni/comb/out_",arr,".rds")) # Load output
  assign(paste0("spls_out_",arr),spls_out) # Assign name
  spls_calib=sum(CalibratedStableRegression(spls_out)) # Number of selected variables
  assign(paste0("spls_calib_",arr),spls_calib) # Assign name
  spls_roc=readRDS(paste0("../cat_results/spls_uni/comb/roc_recali_",arr,".rds")) # Load roc (recalibrated)
  assign(paste0("spls_roc_",arr),spls_roc) # Assign name
}


##### Main analyses: LASSO 
### Recalibrate beta: pest + obesity
colnames=names(lasso_out_pest_comb_de_obesity$Beta[1,,1])
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



##### Main analyses: LASSO vs sPLS --
### function
foo = function(l1, l2, l3, l4, l5, s1, s2, s3, s4, s5){
  l1.coord = data.frame(fpr=1-l1$specificities,tpr=l1$sensitivities)
  l2.coord = data.frame(fpr=1-l2$specificities,tpr=l2$sensitivities)
  l3.coord = data.frame(fpr=1-l3$specificities,tpr=l3$sensitivities)
  l4.coord = data.frame(fpr=1-l4$specificities,tpr=l4$sensitivities)
  l5.coord = data.frame(fpr=1-l5$specificities,tpr=l5$sensitivities)
  s1.coord = data.frame(fpr=1-s1$specificities,tpr=s1$sensitivities)
  s2.coord = data.frame(fpr=1-s2$specificities,tpr=s2$sensitivities)
  s3.coord = data.frame(fpr=1-s3$specificities,tpr=s3$sensitivities)
  s4.coord = data.frame(fpr=1-s4$specificities,tpr=s4$sensitivities)
  s5.coord = data.frame(fpr=1-s5$specificities,tpr=s5$sensitivities)
  x=list(l1.coord, l2.coord, l3.coord, l4.coord, l5.coord, 
         s1.coord, s2.coord, s3.coord, s4.coord, s5.coord)
  names(x) = c("l1","l2","l3","l4","l5","s1","s2","s3","s4","s5")
  return(x)
}

foo_pahs = function(l1, l2, l3, l5, s2, s3, s4, s5){
  l1.coord = data.frame(fpr=1-l1$specificities,tpr=l1$sensitivities)
  l2.coord = data.frame(fpr=1-l2$specificities,tpr=l2$sensitivities)
  l3.coord = data.frame(fpr=1-l3$specificities,tpr=l3$sensitivities)
  l5.coord = data.frame(fpr=1-l5$specificities,tpr=l5$sensitivities)
  s2.coord = data.frame(fpr=1-s2$specificities,tpr=s2$sensitivities)
  s3.coord = data.frame(fpr=1-s3$specificities,tpr=s3$sensitivities)
  s4.coord = data.frame(fpr=1-s4$specificities,tpr=s4$sensitivities)
  s5.coord = data.frame(fpr=1-s5$specificities,tpr=s5$sensitivities)
  x=list(l1.coord, l2.coord, l3.coord, l5.coord, 
         s2.coord, s3.coord, s4.coord, s5.coord)
  names(x) = c("l1","l2","l3","l5", "s2","s3","s4","s5")
  return(x)
}

### Calculate AUC and confidence intervals
foo2 = function(l1, l2, l3, l4, l5, s1, s2, s3, s4, s5, l1_ca, l2_ca, l3_ca, l4_ca, l5_ca){
  l1 = round(as.numeric(l1[l1_ca,]),2)
  l2 = round(as.numeric(l2[l2_ca,]),2)
  l3 = round(as.numeric(l3[l3_ca,]),2)
  l4 = round(as.numeric(l4[l4_ca,]),2)
  l5 = round(as.numeric(l5[l5_ca,]),2)
  s1 = round(as.numeric(ci.auc(s1)),2)
  s2 = round(as.numeric(ci.auc(s2)),2)
  s3 = round(as.numeric(ci.auc(s3)),2)
  s4 = round(as.numeric(ci.auc(s4)),2)
  s5 = round(as.numeric(ci.auc(s5)),2)
  x=c(paste0(l1[2]," [",l1[1],"-",l1[3],"]"),
      paste0(l2[2]," [",l2[1],"-",l2[3],"]"),
      paste0(l3[2]," [",l3[1],"-",l3[3],"]"),
      paste0(l4[2]," [",l4[1],"-",l4[3],"]"),
      paste0(l5[2]," [",l5[1],"-",l5[3],"]"),
      paste0(s1[2]," [",s1[1],"-",s1[3],"]"),
      paste0(s2[2]," [",s2[1],"-",s2[3],"]"),
      paste0(s3[2]," [",s3[1],"-",s3[3],"]"),
      paste0(s4[2]," [",s4[1],"-",s4[3],"]"),
      paste0(s5[2]," [",s5[1],"-",s5[3],"]"))
}

foo2_pahs = function(l1, l2, l3, l5, s2, s3, s4, s5, l1_ca, l2_ca, l3_ca, l5_ca){
  l1 = round(as.numeric(l1[l1_ca,]),2)
  l2 = round(as.numeric(l2[l2_ca,]),2)
  l3 = round(as.numeric(l3[l3_ca,]),2)
  l5 = round(as.numeric(l5[l5_ca,]),2)
  s2 = round(as.numeric(ci.auc(s2)),2)
  s3 = round(as.numeric(ci.auc(s3)),2)
  s4 = round(as.numeric(ci.auc(s4)),2)
  s5 = round(as.numeric(ci.auc(s5)),2)
  x=c(paste0(l1[2]," [",l1[1],"-",l1[3],"]"),
      paste0(l2[2]," [",l2[1],"-",l2[3],"]"),
      paste0(l3[2]," [",l3[1],"-",l3[3],"]"),
      paste0(l5[2]," [",l5[1],"-",l5[3],"]"),
      paste0(s2[2]," [",s2[1],"-",s2[3],"]"),
      paste0(s3[2]," [",s3[1],"-",s3[3],"]"),
      paste0(s4[2]," [",s4[1],"-",s4[3],"]"),
      paste0(s5[2]," [",s5[1],"-",s5[3],"]"))
}


### pest
roc_pest=foo(lasso_roc_pest_comb_de_obesity, lasso_roc_pest_comb_de_LD, 
             lasso_roc_pest_comb_de_hypertension, lasso_roc_pest_comb_de_diabetes,
             lasso_roc_pest_comb_de_MS, 
             spls_roc_pest_comb_de_obesity, spls_roc_pest_comb_de_LD,
             spls_roc_pest_comb_de_hypertension, spls_roc_pest_comb_de_diabetes,
             spls_roc_pest_comb_de_MS)
auc_pest=foo2(lasso_auc_pest_comb_de_obesity, lasso_auc_pest_comb_de_LD, 
              lasso_auc_pest_comb_de_hypertension, lasso_auc_pest_comb_de_diabetes,
              lasso_auc_pest_comb_de_MS, 
              spls_roc_pest_comb_de_obesity, spls_roc_pest_comb_de_LD,
              spls_roc_pest_comb_de_hypertension, spls_roc_pest_comb_de_diabetes,
              spls_roc_pest_comb_de_MS,
              lasso_calib_pest_comb_de_obesity, lasso_calib_pest_comb_de_LD,
              lasso_calib_pest_comb_de_hypertension, lasso_calib_pest_comb_de_diabetes,
              lasso_calib_pest_comb_de_MS)

mycolours = brewer.pal(10, "Paired")

pdf("../cat_figures/lasso_spls_comb/roc_pest.pdf", width = 7, height = 7)
par(mar=c(5,5,1,1))
plot(roc_pest[[1]], ylim = c(0,1),
     col = mycolours[1], type="n", lwd = 2,
     xlab = "False Positive Rate", ylab = "True Positive Rate")
abline(0,1, lty = 3, col = "grey")
lines(roc_pest[[1]], col = mycolours[1], type="l", lwd = 2)
lines(roc_pest[[6]], col = mycolours[2], type="l", lwd = 2)
lines(roc_pest[[2]], col = mycolours[3], type="l", lwd = 2)
lines(roc_pest[[7]], col = mycolours[4], type="l", lwd = 2)
lines(roc_pest[[3]], col = mycolours[5], type="l", lwd = 2)
lines(roc_pest[[8]], col = mycolours[6], type="l", lwd = 2)
lines(roc_pest[[4]], col = mycolours[7], type="l", lwd = 2)
lines(roc_pest[[9]], col = mycolours[8], type="l", lwd = 2)
lines(roc_pest[[5]], col = mycolours[9], type="l", lwd = 2)
lines(roc_pest[[10]], col = mycolours[10], type="l", lwd = 2)
legend("bottomright",
       legend = c(paste0("Obesity with LASSO Model; AUC = ",
                         auc_pest[1]),
                  paste0("Obesity with sPLS-DA Mode; AUC = ",
                         auc_pest[6]),
                  paste0("Lipid disorder with LASSO Model; AUC = ",
                         auc_pest[2]),
                  paste0("Lipid disorder with sPLS-DA Mode; AUC = ",
                         auc_pest[7]),
                  paste0("Hypertension with LASSO Model; AUC = ",
                         auc_pest[3]),
                  paste0("Hypertension with sPLS-DA Mode; AUC = ",
                         auc_pest[8]),
                  paste0("Diabetes with LASSO Model; AUC = ",
                         auc_pest[4]),
                  paste0("Diabetes with sPLS-DA Mode; AUC = ",
                         auc_pest[9]),
                  paste0("Metabolic syndrome with LASSO Model; AUC = ",
                         auc_pest[5]),
                  paste0("Metabolic syndrome with sPLS-DA Mode; AUC = ",
                         auc_pest[10])),
       lty = 1, lwd = 2, col = mycolours, cex = 0.6, bty = "n")
dev.off()


### pahs
roc_pahs=foo_pahs(lasso_roc_pahs_comb_de_obesity,
                  lasso_roc_pahs_comb_de_hypertension, lasso_roc_pahs_comb_de_diabetes,
                  lasso_roc_pahs_comb_de_MS, 
                  spls_roc_pahs_comb_de_LD,
                  spls_roc_pahs_comb_de_hypertension, spls_roc_pahs_comb_de_diabetes,
                  spls_roc_pahs_comb_de_MS)
auc_pahs=foo2_pahs(lasso_auc_pahs_comb_de_obesity, 
                   lasso_auc_pahs_comb_de_hypertension, lasso_auc_pahs_comb_de_diabetes,
                   lasso_auc_pahs_comb_de_MS, 
                   spls_roc_pahs_comb_de_LD,
                   spls_roc_pahs_comb_de_hypertension, spls_roc_pahs_comb_de_diabetes,
                   spls_roc_pahs_comb_de_MS,
                   lasso_calib_pahs_comb_de_obesity,
                   lasso_calib_pahs_comb_de_hypertension, lasso_calib_pahs_comb_de_diabetes,
                   lasso_calib_pahs_comb_de_MS)

pdf("../cat_figures/lasso_spls_comb/roc_pahs.pdf", width = 7, height = 7)
par(mar=c(5,5,1,1))
plot(roc_pahs[[1]], ylim = c(0,1),
     col = mycolours[1], type="n", lwd = 2,
     xlab = "False Positive Rate", ylab = "True Positive Rate")
abline(0,1, lty = 3, col = "grey")
lines(roc_pahs[[1]], col = mycolours[1], type="l", lwd = 2)
lines(roc_pahs[[5]], col = mycolours[4], type="l", lwd = 2)
lines(roc_pahs[[2]], col = mycolours[5], type="l", lwd = 2)
lines(roc_pahs[[6]], col = mycolours[6], type="l", lwd = 2)
lines(roc_pahs[[3]], col = mycolours[7], type="l", lwd = 2)
lines(roc_pahs[[7]], col = mycolours[8], type="l", lwd = 2)
lines(roc_pahs[[4]], col = mycolours[9], type="l", lwd = 2)
lines(roc_pahs[[8]], col = mycolours[10], type="l", lwd = 2)
legend("bottomright",
       legend = c(paste0("Obesity with LASSO Model; AUC = ",
                         auc_pahs[1]),
                  paste0("Lipid disorder with sPLS-DA Mode; AUC = ",
                         auc_pahs[5]),
                  paste0("Hypertension with LASSO Model; AUC = ",
                         auc_pahs[2]),
                  paste0("Hypertension with sPLS-DA Model; AUC = ",
                         auc_pahs[6]),
                  paste0("Diabetes with LASSO Model; AUC = ",
                         auc_pahs[3]),
                  paste0("Diabetes with sPLS-DA Model; AUC = ",
                         auc_pahs[7]),
                  paste0("Metabolic syndrome with LASSO Model; AUC = ",
                         auc_pahs[4]),
                  paste0("Metabolic syndrome with sPLS-DA Model; AUC = ",
                         auc_pahs[8])),
       lty = 1, lwd = 2, col = mycolours[c(1,4,5,6,7:10)], cex = 0.6, bty = "n")
dev.off()
