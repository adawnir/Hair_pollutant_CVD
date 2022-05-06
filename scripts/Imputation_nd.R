
### Sumemr Project -- Imputation Guassian
## Created by Vivian on 1 July

rm(list=ls())
project_path="~/hair/scripts"
setwd(project_path)

library(imputeLCMD)

##### Load data
pest <- read.csv("../data/Dataset_PesticidesPCBBDEs.csv")
names(pest) <- pest %>% slice(1) %>% unlist()
pest <- pest %>% slice(-c(1:2))
dim(pest)    # 1096   48
str(pest)
 
pahs <- read.csv("../data/Dataset_PAHs.csv")
names(pahs) <- pahs %>% slice(1) %>% unlist()
pahs <- pahs %>% slice(-c(1:2))
dim(pahs)    # 682   28
str(pahs)


##### Proportion of NAs per chemical compounds --
pest <- pest[complete.cases(pest[ , 6:15]),]
pest <- pest[, 16:48]
pest_NA_prop = apply(pest, 2, function(x) sum(is.na(x))/nrow(pest))

pest_NA_prop_1 <- data.frame(unlist(pest_NA_prop))
pdf("../figures/PesticidesPCBBDE/NA_rate_pest.pdf", width = 12, height = 7.5)
par(mar=c(15,6,2,1))
plot(pest_NA_prop_1$unlist.pest_NA_prop., type = "h", ylab="Missing rate", 
     xlab="", xaxt="n", lwd=5, 
     col=ifelse(pest_NA_prop_1$unlist.pest_NA_prop. > 0.5, yes="lightsalmon2", no="gray70"),
     cex.lab=2, cex.axis = 2)
abline(h=0.5, lty=2, col="darkred", lwd = 6)
for (i in 1:33){
  axis(side=1, at=i, labels=rownames(pest_NA_prop_1)[i], las=2, cex.axis = 1, font = 2,
       col=ifelse(pest_NA_prop_1$unlist.pest_NA_prop.[i] > 0.5, yes="lightsalmon2", no="gray70"),
       col.axis=ifelse(pest_NA_prop_1$unlist.pest_NA_prop.[i] > 0.5, yes="lightsalmon2", no="gray70"))
}
dev.off()


pahs <- pahs[complete.cases(pahs[ , 6:15]),]
pahs <- pahs[, 16:28]
pahs_NA_prop = apply(pahs, 2, function(x) sum(is.na(x))/nrow(pahs)) %>% sort()

pahs_NA_prop_1 <- data.frame(unlist(pahs_NA_prop))
pdf("../figures/PAHs/NA_rate_pahs.pdf", width = 12, height = 7.5)
par(mar=c(15,6,2,1))
plot(pahs_NA_prop_1$unlist.pahs_NA_prop., type = "h", ylab="Missing rate", 
     xlab="", xaxt="n", lwd=5, 
     col=ifelse(pahs_NA_prop_1$unlist.pahs_NA_prop. > 0.5, yes="lightsalmon2", no="gray70"),
     cex.lab=2, cex.axis = 2)
abline(h=0.5, lty=2, col="darkred", lwd = 6)
for (i in 1:13){
  axis(side=1, at=i, labels=rownames(pahs_NA_prop_1)[i], las=2, cex.axis = 1, font = 2,
       col=ifelse(pahs_NA_prop_1$unlist.pahs_NA_prop.[i] > 0.5, yes="lightsalmon2", no="gray70"),
       col.axis=ifelse(pahs_NA_prop_1$unlist.pahs_NA_prop.[i] > 0.5, yes="lightsalmon2", no="gray70"))
}
dev.off()




##### Proportion of detected per chemical compounds
pest_nd_prop = apply(pest, 2, function(x) sum(x=="nd", na.rm = TRUE)/nrow(pest)) %>% sort()
pest_nd_prop_1 <- data.frame(unlist(pest_nd_prop))

pdf("../figures/PesticidesPCBBDE/nd_rate_pest.pdf", width = 12, height = 7.5)
par(mar=c(15,6,2,1))
plot(pest_nd_prop_1$unlist.pest_nd_prop., type = "h", ylab="Missing rate", 
     xlab="", xaxt="n", lwd=5, 
     col=ifelse(pest_nd_prop_1$unlist.pest_nd_prop. > 0.5, yes="lightsalmon2", no="gray70"),
     cex.lab=2, cex.axis = 2)
abline(h=0.5, lty=2, col="darkred", lwd = 6)
for (i in 1:33){
  axis(side=1, at=i, labels=rownames(pest_nd_prop_1)[i], las=2, cex.axis = 1, font = 2,
       col=ifelse(pest_nd_prop_1$unlist.pest_nd_prop.[i] > 0.5, yes="lightsalmon2", no="gray70"),
       col.axis=ifelse(pest_nd_prop_1$unlist.pest_nd_prop.[i] > 0.5, yes="lightsalmon2", no="gray70"))
}
dev.off()


pahs_nd_prop = apply(pahs, 2, function(x) sum(x=="nd", na.rm = TRUE)/nrow(pahs)) %>% sort()
pahs_nd_prop_1 <- data.frame(unlist(pahs_nd_prop))

pdf("../figures/PAHs/nd_rate_pahs.pdf", width = 12, height = 7.5)
par(mar=c(15,6,2,1))
plot(pahs_nd_prop_1$unlist.pahs_nd_prop., type = "h", ylab="Missing rate", 
     xlab="", xaxt="n", lwd=5, 
     col=ifelse(pahs_nd_prop_1$unlist.pahs_nd_prop. > 0.5, yes="lightsalmon2", no="gray70"),
     cex.lab=2, cex.axis = 2)
abline(h=0.5, lty=2, col="darkred", lwd = 6)
for (i in 1:13){
  axis(side=1, at=i, labels=rownames(pahs_nd_prop_1)[i], las=2, cex.axis = 1, font = 2,
       col=ifelse(pahs_nd_prop_1$unlist.pahs_nd_prop.[i] > 0.5, yes="lightsalmon2", no="gray70"),
       col.axis=ifelse(pahs_nd_prop_1$unlist.pahs_nd_prop.[i] > 0.5, yes="lightsalmon2", no="gray70"))
}
dev.off()


##### Number of chemical compounds with 90% or more nd or NA -- 0!! 
sum(apply(pest, 2, function(x) sum(is.na(x))/nrow(pest)) + apply(pest, 2, function(x) sum(x=="nd", na.rm = TRUE)/nrow(pest)) >=0.9)

sum(apply(pahs, 2, function(x) sum(is.na(x))/nrow(pahs)) + apply(pahs, 2, function(x) sum(x=="nd", na.rm = TRUE)/nrow(pahs)) >=0.9)



##### Recoding nd --
### funciton
rtruncnorm = function(n, mean = 0, sd = 1, min = -Inf, max = Inf){
  if (min > max) stop('Error: Truncation range is empty')
  x = runif(n, pnorm(min, mean, sd), pnorm(max, mean, sd))
  qnorm(x, mean, sd)
}


### set up LOD from data
# PEST
pest_tmp1 = NULL
for (i in 1:33) {
  tmp <- pest[,i]
  tmp1 <- as.numeric(as.character(tmp))
  pest_tmp1 <- cbind(pest_tmp1, tmp1)
}
pest_tmp1 <- data.frame(pest_tmp1)
colnames(pest_tmp1) <- colnames(pest)

LOD <- apply(pest_tmp1, 2, function(x) min(x, na.rm = T))

# PAHs
pahs_tmp1 = NULL
for (i in 1:13) {
  tmp <- pahs[,i]
  tmp1 <- as.numeric(as.character(tmp))
  pahs_tmp1 <- cbind(pahs_tmp1, tmp1)
}
pahs_tmp1 <- data.frame(pahs_tmp1)
colnames(pahs_tmp1) <- colnames(pahs)

LOD_pahs <- apply(pahs_tmp1, 2, function(x) min(x, na.rm = T))


### Replace nd with random values from 0 to minimum detection (Gaussian)
# PEST
pest_tmp2  <- data.frame(lapply(pest, as.character), stringsAsFactors=FALSE)

for(k in 1:ncol(pest_tmp2)){
  set.seed(150621)
  pest_tmp2[pest_tmp2[,k]=="nd"&!is.na(pest_tmp2[,k]),k] = rtruncnorm(sum(pest_tmp2[,k]=="nd"&!is.na(pest_tmp2[,k])), min=0, max=LOD[k]) 
}

pest_tmp2  = apply(pest_tmp2 , 2, as.numeric) %>% data.frame()
colnames(pest_tmp2) <- colnames(pest)

# PAHs
pahs_tmp2  <- data.frame(lapply(pahs, as.character), stringsAsFactors=FALSE)

for(k in 1:ncol(pahs_tmp2)){
  set.seed(150621)
  pahs_tmp2[pahs_tmp2[,k]=="nd"&!is.na(pahs_tmp2[,k]),k] = rtruncnorm(sum(pahs_tmp2[,k]=="nd"&!is.na(pahs_tmp2[,k])), min=0, max=LOD[k]) 
}

pahs_tmp2  = apply(pahs_tmp2 , 2, as.numeric) %>% data.frame()
colnames(pahs_tmp2) <- colnames(pahs)



##### Log10 transformation--
pest_tmp2_log10 = log10(pest_tmp2)
pahs_tmp2_log10 = log10(pahs_tmp2)

saveRDS(pest_tmp2_log10, "../processed/imputation/log10_pest.rds")
saveRDS(pahs_tmp2_log10, "../processed/imputation/log10_pahs.rds")


##### Imputation --
# PEST
set.seed(7)
pest_tmp2_imp=impute.QRILC(t(pest_tmp2_log10))
pest_tmp2_imp <- data.frame(t(pest_tmp2_imp[[1]]))
colnames(pest_tmp2_imp) = colnames(pest)
str(pest_tmp2_imp)

# PAHs
set.seed(7)
pahs_tmp2_imp=impute.QRILC(t(pahs_tmp2_log10))
pahs_tmp2_imp <- data.frame(t(pahs_tmp2_imp[[1]]))
colnames(pahs_tmp2_imp) = colnames(pahs)
str(pahs_tmp2_imp)


# Save data sets
saveRDS(pest_tmp2_imp, "../processed/imputation/imp_pest.rds")
saveRDS(pahs_tmp2_imp, "../processed/imputation/imp_pahs.rds")

