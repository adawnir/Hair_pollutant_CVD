
### Sumemr Project -- Stratified analysis by sex (same obs.) Prediction Performance
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
# No pahs_diabetes.f & pahs_Lipid_disorder.m
for (m in 1:18){
  arr <- c(paste0(rep(c("pest_de"), each = 10), "_", 
                  c("obesity", "hypertension", "diabetes", "Lipid_disorder", "MS"), 
                  rep(c(".m",".f"), each=5)), 
           paste0(rep(c("pahs_de"), each = 4), "_", 
                  c("obesity", "hypertension", "Lipid_disorder", "MS"), 
                  rep(c(".f"), times = 4)),
           paste0(rep(c("pahs_de"), each = 4), "_", 
                  c("obesity", "hypertension", "MS", "diabetes"), 
                  rep(c(".m"), times = 4)))[m]
  
  ### Lasso
  lasso_out=readRDS(paste0("../results/strat_sex_lasso_same/out_",arr,".rds")) # Load output
  assign(paste0("lasso_out_",arr),lasso_out) # Assign name
  lasso_calib=sum(CalibratedStableRegression(lasso_out)) # Number of selected variables
  assign(paste0("lasso_calib_",arr),lasso_calib) # Assign name
  # lasso_auc_avg=readRDS(paste0("../Results/lasso/auc_average_beta_",arr,".rds")) # Load auc (average beta)
  # assign(paste0("lasso_auc_avg_",arr),lasso_auc_avg) # Assign name
  lasso_roc=readRDS(paste0("../results/strat_sex_lasso_same/roc_",arr,".rds")) # Load roc (recalibrated)
  assign(paste0("lasso_roc_",arr),lasso_roc) # Assign name
  lasso_auc=readRDS(paste0("../results/strat_sex_lasso_same/auc_",arr,".rds")) # Load auc (recalibrated)
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
foo = function(l1, l2, l3, l4, l5, l6, l7, l8, l9, l10){
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
  x=list(l1.coord, l2.coord, l3.coord, l4.coord, l5.coord, 
         l6.coord, l7.coord, l8.coord, l9.coord, l10.coord)
  names(x) = c("l1","l2","l3","l4","l5","l6","l7","l8","l9","l10")
  return(x)
}

foo_pahs = function(l1, l2, l3, l5, l6, l7, l9, l10){
  l1.coord = data.frame(fpr=1-l1$specificities,tpr=l1$sensitivities)
  l2.coord = data.frame(fpr=1-l2$specificities,tpr=l2$sensitivities)
  l3.coord = data.frame(fpr=1-l3$specificities,tpr=l3$sensitivities)

  l5.coord = data.frame(fpr=1-l5$specificities,tpr=l5$sensitivities)
  l6.coord = data.frame(fpr=1-l6$specificities,tpr=l6$sensitivities)
  l7.coord = data.frame(fpr=1-l7$specificities,tpr=l7$sensitivities)

  l9.coord = data.frame(fpr=1-l9$specificities,tpr=l9$sensitivities)
  l10.coord = data.frame(fpr=1-l10$specificities,tpr=l10$sensitivities)
  x=list(l1.coord, l2.coord, l3.coord, l5.coord, 
         l6.coord, l7.coord, l9.coord, l10.coord)
  names(x) = c("l1","l2","l3", "l5","l6","l7","l9","l10")
  return(x)
}

### Calculate AUC and confidence intervals
foo2 = function(l1, l2, l3, l4, l5, l6, l7, l8, l9, l10,
                l1_ca, l2_ca, l3_ca, l4_ca, l5_ca, l6_ca, l7_ca, l8_ca, l9_ca, l10_ca){
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
  x=c(paste0(l1[2]," [",l1[1],"-",l1[3],"]"),
      paste0(l2[2]," [",l2[1],"-",l2[3],"]"),
      paste0(l3[2]," [",l3[1],"-",l3[3],"]"),
      paste0(l4[2]," [",l4[1],"-",l4[3],"]"),
      paste0(l5[2]," [",l5[1],"-",l5[3],"]"),
      paste0(l6[2]," [",l6[1],"-",l6[3],"]"),
      paste0(l7[2]," [",l7[1],"-",l7[3],"]"),
      paste0(l8[2]," [",l8[1],"-",l8[3],"]"),
      paste0(l9[2]," [",l9[1],"-",l9[3],"]"),
      paste0(l10[2]," [",l10[1],"-",l10[3],"]"))
}

foo2_pahs = function(l1, l2, l3, l5, l6, l7, l9, l10,
                     l1_ca, l2_ca, l3_ca, l5_ca, l6_ca, l7_ca, l9_ca, l10_ca){
  l1 = round(as.numeric(l1[l1_ca,]),2)
  l2 = round(as.numeric(l2[l2_ca,]),2)
  l3 = round(as.numeric(l3[l3_ca,]),2)

  l5 = round(as.numeric(l5[l5_ca,]),2)
  l6 = round(as.numeric(l6[l6_ca,]),2)
  l7 = round(as.numeric(l7[l7_ca,]),2)

  l9 = round(as.numeric(l9[l9_ca,]),2)
  l10 = round(as.numeric(l10[l10_ca,]),2)
  x=c(paste0(l1[2]," [",l1[1],"-",l1[3],"]"),
      paste0(l2[2]," [",l2[1],"-",l2[3],"]"),
      paste0(l3[2]," [",l3[1],"-",l3[3],"]"),

      paste0(l5[2]," [",l5[1],"-",l5[3],"]"),
      paste0(l6[2]," [",l6[1],"-",l6[3],"]"),
      paste0(l7[2]," [",l7[1],"-",l7[3],"]"),

      paste0(l9[2]," [",l9[1],"-",l9[3],"]"),
      paste0(l10[2]," [",l10[1],"-",l10[3],"]"))
}

### pest
roc_pest=foo(lasso_roc_pest_de_obesity.f, lasso_roc_pest_de_obesity.m,
             lasso_roc_pest_de_Lipid_disorder.f, lasso_roc_pest_de_Lipid_disorder.m, 
             lasso_roc_pest_de_hypertension.f, lasso_roc_pest_de_hypertension.m, 
             lasso_roc_pest_de_diabetes.f, lasso_roc_pest_de_diabetes.m,
             lasso_roc_pest_de_MS.f, lasso_roc_pest_de_MS.m)

auc_pest=foo2(lasso_auc_pest_de_obesity.f, lasso_auc_pest_de_obesity.m, 
              lasso_auc_pest_de_Lipid_disorder.f, lasso_auc_pest_de_Lipid_disorder.m, 
              lasso_auc_pest_de_hypertension.f, lasso_auc_pest_de_hypertension.m, 
              lasso_auc_pest_de_diabetes.f, lasso_auc_pest_de_diabetes.m, 
              lasso_auc_pest_de_MS.f, lasso_auc_pest_de_MS.m,
              lasso_calib_pest_de_obesity.f, lasso_calib_pest_de_obesity.m,  
              lasso_calib_pest_de_Lipid_disorder.f, lasso_calib_pest_de_Lipid_disorder.m, 
              lasso_calib_pest_de_hypertension.f, lasso_calib_pest_de_hypertension.m, 
              lasso_calib_pest_de_diabetes.f, lasso_calib_pest_de_diabetes.m, 
              lasso_calib_pest_de_MS.f, lasso_calib_pest_de_MS.m)


mycolours = brewer.pal(10, "Paired")

pdf("../figures/strat_sex_lasso_same/roc_pest.pdf", width = 7, height = 7)
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
lines(roc_pest[[7]], col = mycolours[7], type="l", lwd = 2)
lines(roc_pest[[8]], col = mycolours[8], type="l", lwd = 2)
lines(roc_pest[[9]], col = mycolours[9], type="l", lwd = 2)
lines(roc_pest[[10]], col = mycolours[10], type="l", lwd = 2)
legend("bottomright",
       legend = c(paste0("Female: Obesity; AUC = ",
                         auc_pest[1]),
                  paste0("Male: Obesity; AUC = ",
                         auc_pest[2]),
                  paste0("Female: Lipid disorder; AUC = ",
                         auc_pest[3]),
                  paste0("Male: Lipid disorder; AUC = ",
                         auc_pest[4]),
                  paste0("Female: Hypertension; AUC = ",
                         auc_pest[5]),
                  paste0("Male: Hypertension; AUC = ",
                         auc_pest[6]),
                  paste0("Female: Diabetes; AUC = ",
                         auc_pest[7]),
                  paste0("Male: Diabetes; AUC = ",
                         auc_pest[8]),
                  paste0("Female: Metabolic symdrone; AUC = ",
                         auc_pest[9]),
                  paste0("Male: Metabolic symdrone; AUC = ",
                         auc_pest[10])),
       lty = 1, lwd = 2, col = mycolours, cex = 0.6, bty = "n")
dev.off()


### pahs
roc_pahs=foo_pahs(lasso_roc_pahs_de_obesity.f, lasso_roc_pahs_de_obesity.m,
                  lasso_roc_pahs_de_Lipid_disorder.f,
                  lasso_roc_pahs_de_hypertension.f, lasso_roc_pahs_de_hypertension.m,
                  lasso_roc_pahs_de_diabetes.m,
                  lasso_roc_pahs_de_MS.f, lasso_roc_pahs_de_MS.m)
auc_pahs=foo2_pahs(lasso_auc_pahs_de_obesity.f, lasso_auc_pahs_de_obesity.m,
                   lasso_auc_pahs_de_Lipid_disorder.f,
                   lasso_auc_pahs_de_hypertension.f, lasso_auc_pahs_de_hypertension.m,
                   lasso_auc_pahs_de_diabetes.m,
                   lasso_auc_pahs_de_MS.f, lasso_auc_pahs_de_MS.m,
                   lasso_calib_pahs_de_obesity.f, lasso_calib_pahs_de_obesity.m,
                   lasso_calib_pahs_de_Lipid_disorder.f,
                   lasso_calib_pahs_de_hypertension.f, lasso_calib_pahs_de_hypertension.m,
                   lasso_calib_pahs_de_diabetes.m,
                   lasso_calib_pahs_de_MS.f, lasso_calib_pahs_de_MS.m)

pdf("../figures/strat_sex_lasso_same/roc_pahs.pdf", width = 7, height = 7)
par(mar=c(5,5,1,1))
plot(roc_pahs[[1]], ylim = c(0,1),
     col = mycolours[1], type="n", lwd = 2,
     xlab = "False Positive Rate", ylab = "True Positive Rate")
abline(0,1, lty = 3, col = "grey")
lines(roc_pahs[[1]], col = mycolours[1], type="l", lwd = 2)
lines(roc_pahs[[2]], col = mycolours[2], type="l", lwd = 2)
lines(roc_pahs[[3]], col = mycolours[3], type="l", lwd = 2)
lines(roc_pahs[[4]], col = mycolours[5], type="l", lwd = 2)
lines(roc_pahs[[5]], col = mycolours[6], type="l", lwd = 2)
lines(roc_pahs[[6]], col = mycolours[8], type="l", lwd = 2)
lines(roc_pahs[[7]], col = mycolours[9], type="l", lwd = 2)
lines(roc_pahs[[8]], col = mycolours[10], type="l", lwd = 2)
legend("bottomright",
       legend = c(paste0("Female: Obesity; AUC = ",
                         auc_pahs[1]),
                  paste0("Male: Obesity; AUC = ",
                         auc_pahs[2]),
                  paste0("Female: Lipid disorder; AUC = ",
                         auc_pahs[3]),
                  paste0("Female: Hypertension; AUC = ",
                         auc_pahs[4]),
                  paste0("Male: Hypertension; AUC = ",
                         auc_pahs[5]),
                  paste0("Male: Diabetes; AUC = ",
                         auc_pahs[6]),
                  paste0("Female: Metabolic symdrone; AUC = ",
                         auc_pahs[7]),
                  paste0("Male: Metabolic symdrone; AUC = ",
                         auc_pahs[8])),
       lty = 1, lwd = 2, col = mycolours[c(1:3, 5:6, 8:10)], cex = 0.6, bty = "n")
dev.off()
