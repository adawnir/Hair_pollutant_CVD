### Pre-processing: Imputing non-detects and missing values

rm(list=ls())
project_path=dirname(rstudioapi::getActiveDocumentContext()$path)
setwd(project_path)

source("functions.R")
# library(imputeLCMD)
library(missForest)

## Load data ----
pest = readRDS("../Data/PesticidesPCBBDEs.rds")
pahs = readRDS("../Data/PAHs.rds")
comb = readRDS("../Data/Combined.rds")

# Subset exposures
colnames(pest)
pest_expo = pest[,30:ncol(pest)]
colnames(pahs)
pahs_expo = pahs[,30:ncol(pahs)]

## Proportion of NAs per chemical compounds ----
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

## Proportion of NAs per participant ----
summary(apply(pest_expo, 1, function(x) sum(is.na(x))/ncol(pest_expo)))
summary(apply(pahs_expo, 1, function(x) sum(is.na(x))/ncol(pahs_expo)))

## Proportion of detected per chemical compounds ----
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

## Proportion of NAs per participant ----
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

## Overlapping participants
comb_expo = cbind(pest_expo[comb$ID,],pahs_expo[comb$ID,])
colnames(comb_expo)
dim(comb_expo)

saveRDS(pest_expo, "../Processed/PesticidesPCBBDEs_ndimp.rds")
write.csv(pest_expo, paste0("../Exports/PesticidesPCBBDEs_ndimp.csv"))
saveRDS(pahs_expo, "../Processed/PAHs_ndimp.rds")
write.csv(pahs_expo, paste0("../Exports/PAHs_ndimp.csv"))
saveRDS(comb_expo, "../Processed/Combined_ndimp.rds")
write.csv(comb_expo, paste0("../Exports/Combined_ndimp.csv"))


## Imputation of missing data----
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
imp = missForest(pest_expo)
imp$OOBerror
pest_expo_imp = imp$ximp
colnames(pest_expo_imp) = colnames(pest_expo)
rownames(pest_expo_imp) = rownames(pest_expo)

set.seed(202204)
imp = missForest(pahs_expo)
imp$OOBerror
pahs_expo_imp = imp$ximp
colnames(pahs_expo_imp) = colnames(pahs_expo)
rownames(pahs_expo_imp) = rownames(pahs_expo)

## Overlapping participants
comb_expo_imp = cbind(pest_expo_imp[comb$ID,],pahs_expo_imp[comb$ID,])
colnames(comb_expo_imp)
dim(comb_expo_imp)

saveRDS(pest_expo_imp, "../Processed/PesticidesPCBBDEs_ndimp_naimp.rds")
write.csv(pest_expo_imp, paste0("../Exports/PesticidesPCBBDEs_ndimp_naimp.csv"))
saveRDS(pahs_expo_imp, "../Processed/PAHs_ndimp_naimp.rds")
write.csv(pahs_expo_imp, paste0("../Exports/PAHs_ndimp_naimp.csv"))
saveRDS(comb_expo_imp, "../Processed/Combined_ndimp_naimp.rds")
write.csv(comb_expo_imp, paste0("../Exports/Combined_ndimp_naimp.csv"))

## Imputation of missing data (nutrient intake information)----
# Combine all participants
outer = rbind(pest[setdiff(pest$ID, pahs$ID),7:11],
              pahs[setdiff(pahs$ID, pest$ID),7:11],
              comb[,7:11])
colnames(outer)

set.seed(202204)
imp = missForest(outer)
imp$OOBerror
outer_imp = imp$ximp
colnames(outer_imp) = colnames(outer)
rownames(outer_imp) = rownames(outer)

saveRDS(outer_imp, "../Processed/All_participants_nutri_naimp.rds")
write.csv(outer_imp, paste0("../Exports/All_participants_nutri_naimp.csv"))

## Log10 transformation ----
pest_expo_imp_log = log10(pest_expo_imp)
pahs_expo_imp_log = log10(pahs_expo_imp)

## Overlapping participants
comb_expo_imp_log = cbind(pest_expo_imp_log[comb$ID,],pahs_expo_imp_log[comb$ID,])
colnames(comb_expo_imp_log)
dim(comb_expo_imp_log)

saveRDS(pest_expo_imp_log, "../Processed/PesticidesPCBBDEs_ndimp_naimp_log.rds")
write.csv(pest_expo_imp_log, paste0("../Exports/PesticidesPCBBDEs_ndimp_naimp_log.csv"))
saveRDS(pahs_expo_imp_log, "../Processed/PAHs_ndimp_naimp_log.rds")
write.csv(pahs_expo_imp_log, paste0("../Exports/PAHs_ndimp_naimp_log.csv"))
saveRDS(comb_expo_imp_log, "../Processed/Combined_ndimp_naimp_log.rds")
write.csv(comb_expo_imp_log, paste0("../Exports/Combined_ndimp_naimp_log.csv"))
