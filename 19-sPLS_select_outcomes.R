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
data = readRDS(paste0("../Processed/",filepaths[m],"_XY.rds"))
Y = data[,c(17:25)]
X = data[,27:ncol(data)]

# sPLS: sparsity on both X and Y
stab = BiSelection(xdata = X, ydata = Y, family = "gaussian", ncomp = 3,
                   LambdaX = 1:(ncol(X) - 1),
                   LambdaY = 1:(ncol(Y) - 1),
                   implementation = SparsePLS, n_cat = 2)
saveRDS(stab, paste0("../Results/R2GUESS/sPLS_XY_output_",filepaths[m],".rds"))

pdf(paste0("../Figures/R2GUESS/sPLS_XY_output_",filepaths[m],".pdf"))
par(mar=c(5,5,1,1))
CalibrationPlot(stab)
dev.off()

pdf(paste0("../Figures/R2GUESS/sPLS_XY_network_",filepaths[m],".pdf"))
plot(stab)
dev.off()

print(summary(stab))

print(stab$selectedY)
