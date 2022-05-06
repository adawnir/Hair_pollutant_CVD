### Pre-processing: Imputing non-detects and missing values

rm(list=ls())
project_path=dirname(rstudioapi::getActiveDocumentContext()$path)
setwd(project_path)

source("functions.R")
# library(imputeLCMD)
library(missForest)

### Load data ----
pest = readRDS("../Data/PesticidesPCBBDEs.rds")
pahs = readRDS("../Data/PAHs.csv")

# Subset exposures
pest_expo = pest[,16:48]
pahs_expo = pahs[,16:28]

##### Proportion of NAs per chemical compounds ----
pest_na_prop = apply(pest_expo, 2, function(x) sum(is.na(x))/nrow(pest_expo))
pest_prop = sort(pest_na_prop, decreasing = TRUE)

ifelse(dir.exists("../Figures/"), "", dir.create("../Figures/"))
pdf("../Figures/Missing_prop_PesticidesPCBBDEs.pdf", width = 9, height = 4.75)
par(mar=c(9,5,2,1))
plot(pest_prop, type = "h", ylab="Proportion missing", xlab="", xaxt = "n", lwd=1, ylim = c(0,1),
     col=ifelse(pest_prop>=0.5, yes="tomato", no="black"), cex.lab=1.5)
abline(h=0.5, lty=2, col="darkred", lwd = 1)
for (i in 1:length(pest_prop)){
  axis(side=1, at=i, labels=names(pest_prop)[i], las=2,
       col=ifelse(pest_prop[i]>=0.5, yes="tomato", no="black"),
       col.axis=ifelse(pest_prop[i]>=0.5, yes="tomato", no="black"))
}
dev.off()

pahs_na_prop = apply(pahs_expo, 2, function(x) sum(is.na(x))/nrow(pahs_expo))
pahs_prop = sort(pahs_na_prop, decreasing = TRUE)

ifelse(dir.exists("../Figures/"), "", dir.create("../Figures/"))
pdf("../Figures/Missing_prop_PAHs.pdf", width = 9, height = 6)
par(mar=c(16,5,2,1))
plot(pahs_prop, type = "h", ylab="Proportion missing", xlab="", xaxt = "n", lwd=1, ylim = c(0,1),
     col=ifelse(pahs_prop>=0.5, yes="tomato", no="black"), cex.lab=1.5)
abline(h=0.5, lty=2, col="darkred", lwd = 1)
for (i in 1:length(pahs_prop)){
  axis(side=1, at=i, labels=names(pahs_prop)[i], las=2,
       col=ifelse(pahs_prop[i]>=0.5, yes="tomato", no="black"),
       col.axis=ifelse(pahs_prop[i]>=0.5, yes="tomato", no="black"))
}
dev.off()


##### Proportion of detected per chemical compounds ----
pest_nd_prop = apply(pest_expo, 2, function(x) sum(x=="nd", na.rm = TRUE)/nrow(pest_expo))
pest_prop =sort(1-(pest_nd_prop+pest_na_prop), decreasing = TRUE)

ifelse(dir.exists("../Figures/"), "", dir.create("../Figures/"))
pdf("../Figures/Detection_prop_PesticidesPCBBDEs.pdf", width = 9, height = 4.75)
par(mar=c(9,5,2,1))
plot(pest_prop, type = "h", ylab="Proportion detected", xlab="", xaxt = "n", lwd=1, ylim = c(0,1),
     col=ifelse(pest_prop<=0.1, yes="tomato", no="black"), cex.lab=1.5)
abline(h=0.1, lty=2, col="darkred", lwd = 1)
for (i in 1:length(pest_prop)){
  axis(side=1, at=i, labels=names(pest_prop)[i], las=2,
       col=ifelse(pest_prop[i]<=0.1, yes="tomato", no="black"),
       col.axis=ifelse(pest_prop[i]<=0.1, yes="tomato", no="black"))
}
dev.off()

pahs_nd_prop = apply(pahs_expo, 2, function(x) sum(x=="nd", na.rm = TRUE)/nrow(pahs_expo))
pahs_prop = sort(1-(pahs_nd_prop+pahs_na_prop), decreasing = TRUE)

ifelse(dir.exists("../Figures/"), "", dir.create("../Figures/"))
pdf("../Figures/Detection_prop_PAHs.pdf", width = 9, height = 6)
par(mar=c(16,5,2,1))
plot(pahs_prop, type = "h", ylab="Proportion detected", xlab="", xaxt = "n", lwd=1, ylim = c(0,1),
     col=ifelse(pahs_prop<=0.1, yes="tomato", no="black"), cex.lab=1.5)
abline(h=0.1, lty=2, col="darkred", lwd = 1)
for (i in 1:length(pahs_prop)){
  axis(side=1, at=i, labels=names(pahs_prop)[i], las=2,
       col=ifelse(pahs_prop[i]<=0.1, yes="tomato", no="black"),
       col.axis=ifelse(pahs_prop[i]<=0.1, yes="tomato", no="black"))
}
dev.off()

##### Recoding nd ----

### Extract LOD
pest_lod = apply(pest_expo, 2,function(x) suppressWarnings(min(as.numeric(unlist(x)), na.rm = T)))
pahs_lod = apply(pahs_expo, 2,function(x) suppressWarnings(min(as.numeric(unlist(x)), na.rm = T)))

### Replace nd with random values from 0 to minimum detection (Gaussian)
for(k in 1:ncol(pest_expo)){
  pest_expo[which(pest_expo[,k]=="nd"),k] = rtruncnorm(sum(pest_expo[,k]=="nd", na.rm = T),min=0,max=pest_lod[k]) 
}
for(k in 1:ncol(pahs_expo)){
  pahs_expo[which(pahs_expo[,k]=="nd"),k] = rtruncnorm(sum(pahs_expo[,k]=="nd", na.rm = T),min=0,max=pahs_lod[k]) 
}

pest_expo = apply(pest_expo, 2, as.numeric)
rownames(pest_expo) = rownames(pest)
pahs_expo = apply(pahs_expo, 2, as.numeric)
rownames(pahs_expo) = rownames(pahs)

summary(pest_expo)
summary(pahs_expo)

saveRDS(pest_expo, "../Processed/PesticidesPCBBDEs_ndimp.rds")
write.csv(pest_expo, paste0("../Exports/PesticidesPCBBDEs_ndimp.csv"))
saveRDS(pahs_expo, "../Processed/PAHs_ndimp.rds")
write.csv(pahs_expo, paste0("../Exports/PAHs_ndimp.csv"))


##### Log10 transformation ----
pest_expo_log = log10(pest_expo)
pahs_expo_log = log10(pahs_expo)

saveRDS(pest_expo_log, "../Processed/PesticidesPCBBDEs_ndimp_log.rds")
saveRDS(pahs_expo_log, "../Processed/PAHs_ndimp_log.rds")

##### Imputation of missing data----
# set.seed(7)
# pest_expo_imp = impute.QRILC(t(pest_expo_log))
# pest_expo_imp = data.frame(t(pest_expo_imp[[1]]))
# colnames(pest_expo_imp) = colnames(pest_expo)
# str(pest_expo_imp)
# summary(pest_expo_imp)
# 
# set.seed(7)
# pahs_expo_imp = impute.QRILC(t(pahs_expo_log))
# pahs_expo_imp = data.frame(t(pahs_expo_imp[[1]]))
# colnames(pahs_expo_imp) = colnames(pahs_expo)
# summary(pahs_expo_imp)

set.seed(202204)
imp = missForest(pest_expo_log)
imp$OOBerror
pest_expo_imp = imp$ximp
colnames(pest_expo_imp) = colnames(pest_expo_log)
rownames(pest_expo_imp) = rownames(pest_expo_log)

set.seed(202204)
imp = missForest(pahs_expo_log)
imp$OOBerror
pahs_expo_imp = imp$ximp
colnames(pahs_expo_imp) = colnames(pahs_expo_log)
rownames(pahs_expo_imp) = rownames(pahs_expo_log)


saveRDS(pest_expo_imp, "../Processed/PesticidesPCBBDEs_ndimp_naimp.rds")
write.csv(pest_expo_imp, paste0("../Exports/PesticidesPCBBDEs_ndimp_naimp.csv"))
saveRDS(pahs_expo_imp, "../Processed/PAHs_ndimp_naimp.rds")
write.csv(pahs_expo_imp, paste0("../Exports/PAHs_ndimp_naimp.csv"))
