## Data exploration and pre-processing
## Rin Wada 15 June

# Initialisation
rm(list=ls())
path=dirname(rstudioapi::getActiveDocumentContext()$path)
setwd(path)

# Load packages
library(tidyverse)
library(colorspace)
library(imputeLCMD)

rtruncnorm = function(n, mean = 0, sd = 1, min = -Inf, max = Inf){
  if (min > max) stop('Error: Truncation range is empty')
  x = runif(n, pnorm(min, mean, sd), pnorm(max, mean, sd))
  qnorm(x, mean, sd)
}

### Detection frequency per chemical----
# Minimum detection per chemical compound
all(colnames(mat_lux)==chem_lux$Compound)
chem_lux$LOD = apply(mat_lux, 2, function(x) min(as.numeric(unlist(x)), na.rm = T))
chem_lux$LOD = ifelse(is.infinite(chem_lux$LOD),0,chem_lux$LOD)

# Proportion of NAs per chemical compounds
all(colnames(mat_lux)==chem_lux$Compound)
chem_lux$NA_prop = apply(mat_lux, 2, function(x) sum(is.na(x))/nrow(mat_lux))

# Proportion of detected per chemical compounds
all(colnames(mat_lux)==chem_lux$Compound)
chem_lux$nd_prop = apply(mat_lux, 2, function(x) sum(x=="nd", na.rm = TRUE)/nrow(mat_lux))

### Inclusion/exclusion criteria (Chemical compound) ----
# Number of chemical compounds with 50% or more NA
nrow(chem_lux[which(chem_lux$NA_prop>=0.5),])

# Filter out >=50% NA
mat_lux = mat_lux[,which(chem_lux$NA_prop<0.5)]
chem_lux = chem_lux[which(chem_lux$NA_prop<0.5),]

# Number of chemical compounds with 90% or more nd or NA
sum(chem_lux$nd_prop + chem_lux$NA_prop>=0.9)

# Filter out >=90% nd or NA
mat_lux = mat_lux[,which(chem_lux$nd_prop + chem_lux$NA_prop<0.9)]
chem_lux = chem_lux[which(chem_lux$nd_prop + chem_lux$NA_prop<0.9),]

ncol(mat_lux)

### Recoding nd ----
# Replace nd with random values from 0 to minimum detection (Gaussian)
for(k in 1:ncol(mat_lux)){
  set.seed(150621)
  mat_lux[mat_lux[,k]=="nd"&!is.na(mat_lux[,k]),k] = rtruncnorm(sum(mat_lux[,k]=="nd"&!is.na(mat_lux[,k])), min=0, max=chem_lux$LOD[k]) 
}

# Convert to numeric (Replace any characters with NaN and add rownames)
mat_lux = apply(mat_lux, 2, as.numeric)
rownames(mat_lux) = covar_lux$Indiv.ID

### Transformation ----
# Density plots
ncol(mat_lux)
ifelse(dir.exists("../Figures/Luxembourg"),"",dir.create("../Figures/Luxembourg"))
pdf("../Figures/Luxembourg/Compound_dist.pdf", height = 11, width = 8)
par(mfrow = c(7, 5), oma = c(0.5,0.5,0,0.5), mar = c(1,1,1,1), mgp=c(0,0.2,0))
for (k in 1:ncol(mat_lux)){
  plot(density(mat_lux[,k], na.rm = T),
       col="navy", xlab="", ylab="", xaxt="n", main=colnames(mat_lux)[k],
       cex.main = 0.5, cex.axis=0.5, tck=-0.05)
}
dev.off()

# Log10 Transformation
mat_lux = log10(mat_lux)

# Density plots
ncol(mat_lux)
pdf("../Figures/Luxembourg/Compound_dist_trans.pdf", height = 11, width = 8)
par(mfrow = c(7, 5), oma = c(0.5,0.5,0,0.5), mar = c(1,1,1,1), mgp=c(0,0.2,0))
for (k in 1:ncol(mat_lux)){
  plot(density(mat_lux[,k], na.rm = T),
       col="navy", xlab="", ylab="", xaxt="n", main=colnames(mat_lux)[k],
       cex.main = 0.5, cex.axis=0.5, tck=-0.05)
}
dev.off()

ncol(mat_gs)
pdf("../Figures/GrandeSynthe/Compound_dist_trans.pdf", height = 11, width = 8)
par(mfrow = c(7, 5), oma = c(0.5,0.5,0,0.5), mar = c(1,1,1,1), mgp=c(0,0.2,0))
for (k in 1:ncol(mat_gs)){
  plot(density(mat_gs[,k], na.rm = T),
       col="navy", xlab="", ylab="", xaxt="n", main=colnames(mat_gs)[k],
       cex.main = 0.5, cex.axis=0.5, tck=-0.05)
}
dev.off()

# Save data sets
saveRDS(mat_lux, "../Processed/Luxembourg/Chemical_compound_matrix_subset_trans.rds")

### Imputation ----
# impute.QRILC
set.seed(7)
mat_lux_imp=impute.QRILC(t(mat_lux))
mat_lux_imp=t(mat_lux_imp[[1]])
rownames(mat_lux_imp) = rownames(mat_lux)
colnames(mat_lux_imp) = colnames(mat_lux)

# Save data sets
saveRDS(mat_lux_imp, "../Processed/Luxembourg/Chemical_compound_matrix_subset_trans_imp.rds")
