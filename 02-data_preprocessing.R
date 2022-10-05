### Pre-processing: Overview of missing valyes and non-detects, Recoding non-detects, Splitting data, imputing missing values

rm(list=ls())
project_path=dirname(rstudioapi::getActiveDocumentContext()$path)
setwd(project_path)

source("functions.R")
# library(imputeLCMD)
library(missForest)

ifelse(dir.exists("../Processed/"), "", dir.create("../Processed/"))

## Load data ----
pest = readRDS("../Data/PesticidesPCBBDEs.rds")
pahs = readRDS("../Data/PAHs.rds")

# Subset exposures
colnames(pest)
pest_expo = pest[,33:ncol(pest)]
colnames(pahs)
pahs_expo = pahs[,33:ncol(pahs)]

## Proportion of NAs per chemical compounds ----
pest_na_prop = apply(pest_expo, 2, function(x) sum(is.na(x))/nrow(pest_expo))
pest_prop = sort(pest_na_prop, decreasing = TRUE)

pdf("../Figures/Descriptive_statistics/Missing_prop_PesticidesPCBBDEs.pdf", width = 9, height = 4.75)
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

pdf("../Figures/Descriptive_statistics/Missing_prop_PAHs.pdf", width = 9, height = 6)
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

## Proportion of NAs per participant ----
summary(apply(pest_expo, 1, function(x) sum(is.na(x))/ncol(pest_expo)))
summary(apply(pahs_expo, 1, function(x) sum(is.na(x))/ncol(pahs_expo)))

## Proportion of detected per chemical compounds ----
pest_nd_prop = apply(pest_expo, 2, function(x) sum(x=="nd", na.rm = TRUE)/nrow(pest_expo))
pest_prop =sort(1-(pest_nd_prop+pest_na_prop), decreasing = TRUE)

pdf("../Figures/Descriptive_statistics/Detection_prop_PesticidesPCBBDEs.pdf", width = 9, height = 4.75)
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

pdf("../Figures/Descriptive_statistics/Detection_prop_PAHs.pdf", width = 9, height = 6)
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

## Proportion detected per participant ----
summary(1-(apply(pest_expo, 1, function(x) sum(x=="nd", na.rm = TRUE)/ncol(pest_expo))+
             apply(pest_expo, 1, function(x) sum(is.na(x))/ncol(pest_expo))))
summary(1-(apply(pahs_expo, 1, function(x) sum(x=="nd", na.rm = TRUE)/ncol(pahs_expo))+
             apply(pahs_expo, 1, function(x) sum(is.na(x))/ncol(pahs_expo))))

## Recoding nd ----
### Extract LOD
pest_lod = apply(pest_expo, 2,function(x) suppressWarnings(min(as.numeric(unlist(x)), na.rm = T)))
pahs_lod = apply(pahs_expo, 2,function(x) suppressWarnings(min(as.numeric(unlist(x)), na.rm = T)))

### Save exposure information
pest_info = data.frame(lod = pest_lod, ndprop = pest_nd_prop, naprop = pest_na_prop,
                       row.names = colnames(pest_expo))
pahs_info = data.frame(lod = pahs_lod, ndprop = pahs_nd_prop, naprop = pahs_na_prop,
                       row.names = colnames(pahs_expo))

saveRDS(pest_info, "../Processed/PesticidesPCBBDEs_info.rds")
saveRDS(pahs_info, "../Processed/PAHs_info.rds")

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
write.csv(pest_expo, paste0("../Exports/Data/PesticidesPCBBDEs_ndimp.csv"))
saveRDS(pahs_expo, "../Processed/PAHs_ndimp.rds")
write.csv(pahs_expo, paste0("../Exports/Data/PAHs_ndimp.csv"))

## Splitting data sets ----
outer = readRDS("../Data/All_participants_info.rds")
train_id = outer$ID[outer$data_type=="train_pest"]
train_pest = cbind(outer[train_id,],
                   pest_expo[train_id,])
dim(train_pest)

train_id = outer$ID[outer$data_type=="train_pahs"]
train_pahs = cbind(outer[train_id,],
                   pahs_expo[train_id,])
dim(train_pahs)

## Overlapping participants
test_id = outer$ID[outer$data_type=="test"]
test = cbind(outer[test_id,],pest_expo[test_id,],pahs_expo[test_id,])
dim(test)

saveRDS(train_pest, "../Processed/PesticidesPCBBDEs_train_info_expo_ndimp.rds")
saveRDS(train_pahs, "../Processed/PAHs_train_info_expo_ndimp.rds")
saveRDS(test, "../Processed/Overlap_test_info_expo_ndimp.rds")

## Imputing missing values ----

## Exposures
set.seed(202204)
imp = missForest(train_pest[,34:ncol(train_pest)])
imp$OOBerror
train_pest[,34:ncol(train_pest)] = imp$ximp

set.seed(202204)
imp = missForest(train_pahs[,34:ncol(train_pahs)])
imp$OOBerror
train_pahs[,34:ncol(train_pahs)] = imp$ximp

set.seed(202204)
imp = missForest(test[,34:ncol(test)])
imp$OOBerror
test[,34:ncol(test)] = imp$ximp

## Covariate information
set.seed(202204)
imp = missForest(train_pest[,2:32])
imp$OOBerror
train_pest[,2:32] = imp$ximp

set.seed(202204)
imp = missForest(train_pahs[,2:32])
imp$OOBerror
train_pahs[,2:32] = imp$ximp

set.seed(202204)
imp = missForest(test[,2:32])
imp$OOBerror
test[,2:32] = imp$ximp

saveRDS(train_pest, "../Processed/PesticidesPCBBDEs_train_info_expo_ndimp_naimp.rds")
saveRDS(train_pahs, "../Processed/PAHs_train_info_expo_ndimp_naimp.rds")
saveRDS(test, "../Processed/Overlap_test_info_expo_ndimp_naimp.rds")

## Log10 transformation ----
train_pest[,34:ncol(train_pest)] = log10(train_pest[,34:ncol(train_pest)])
train_pahs[,34:ncol(train_pahs)] = log10(train_pahs[,34:ncol(train_pahs)])
test[,34:ncol(test)] = log10(test[,34:ncol(test)])

saveRDS(train_pest, "../Processed/PesticidesPCBBDEs_train_info_expo_ndimp_naimp_log.rds")
saveRDS(train_pahs, "../Processed/PAHs_train_info_expo_ndimp_naimp_log.rds")
saveRDS(test, "../Processed/Overlap_test_info_expo_ndimp_naimp_log.rds")

