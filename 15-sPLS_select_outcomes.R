# sPLS for selecting outcomes and prior for R2GUESS

rm(list=ls())
project_path="/rds/general/user/rw1317/home/Hair_pollutant_CVD/Scripts"
setwd(project_path)


library(sharp)
library(tidyverse)

## Parameters
args=commandArgs(trailingOnly=TRUE)
m=as.numeric(args[1])

filepaths = c("PesticidesPCBBDEs_train","PAHs_train")

ifelse(dir.exists("../Results/"),"",dir.create("../Results/"))
ifelse(dir.exists("../Results/R2GUESS/"),"",dir.create("../Results/R2GUESS/"))

ifelse(dir.exists("../Figures/"),"",dir.create("../Figures/"))
ifelse(dir.exists("../Figures/R2GUESS/"),"",dir.create("../Figures/R2GUESS/"))

## Load data
Y = readRDS(paste0("../Data/",filepaths[m],"_response.rds"))
Z = readRDS(paste0("../Processed/",filepaths[m],"_Z.rds"))
X = readRDS(paste0("../Processed/",filepaths[m],"_expo_ndimp_naimp_log.rds"))

Z = Z %>% select(Age, Gender)
Z = model.matrix(~ ., Z)[,-1]

colnames(Y)
Y = Y %>% select(BMI, WC, TG, TC, LDLC, HDLC, SBP, DBP, FPG)

f0='X[,k] ~ Z'

X_denoised=NULL
for (k in seq(1:ncol(X))){
  print(colnames(X)[k])
  model=lm(as.formula(f0))
  X_denoised = cbind(X_denoised, model$residuals)
}
colnames(X_denoised)=colnames(X)
rownames(X_denoised)=rownames(X)
  
# sPLS: sparsity on both X and Y
stab = BiSelection(xdata = X_denoised, ydata = Y, family = "gaussian", ncomp = 3,
                   LambdaX = 1:(ncol(X) - 1),
                   LambdaY = 1:(ncol(Y) - 1),
                   implementation = SparsePLS)

saveRDS(stab, paste0("../Results/R2GUESS/sPLS_output_",filepaths[m],".rds"))

pdf(paste0("../Figures/R2GUESS/sPLS_output_",filepaths[m],".pdf"))
par(mar=c(5,5,1,1))
CalibrationPlot(stab)
dev.off()

pdf(paste0("../Figures/R2GUESS/sPLS_network_",filepaths[m],".pdf"))
plot(stab, cex = 5)
dev.off()

print(summary(stab))

print(stab$selectedY)




