# R2GUESS

rm(list=ls())
project_path="/rds/general/user/rw1317/home/Hair_pollutant_CVD/Scripts"
setwd(project_path)


library(R2GUESS)
library(tidyverse)

## Parameters
filepaths = c("PesticidesPCBBDEs_train","PAHs_train")

m = 2

ifelse(dir.exists("../Results/"),"",dir.create("../Results/"))
ifelse(dir.exists("../Results/R2GUESS/"),"",dir.create("../Results/R2GUESS/"))
ifelse(dir.exists(paste0("../Results/R2GUESS/",filepaths[m])),"",dir.create(paste0("../Results/R2GUESS/",filepaths[m])))

ifelse(dir.exists("../Figures/"),"",dir.create("../Figures/"))
ifelse(dir.exists("../Figures/R2GUESS/"),"",dir.create("../Figures/R2GUESS/"))


# Load data
Z = readRDS(paste0("../Processed/",filepaths[m],"_Z.rds"))
X = readRDS(paste0("../Processed/",filepaths[m],"_expo_ndimp_naimp_log.rds"))
X = as.data.frame(X)

stab = readRDS(paste0("../Results/R2GUESS/sPLS_output_",filepaths[m],".rds"))

# Select outcomes
# Y = readRDS(paste0("../Data/",filepaths[m],"_response.rds"))
# Y = Y %>% select(BMI, WC, TG, TC, LDLC, HDLC, SBP, DBP, FPG)
# Y = Y[,rowSums(stab$selectedY)>0]
# Y = Y[,stab$selectedY[,1]>0]

# egam = sum(rowSums(stab$selectedX)>0)
# sgam = sum(rowSums(stab$selectedX)>0)
# egam = sum(stab$selectedX[,1]>0)
# sgam = sum(stab$selectedX[,1]>0)

# Prior
egam = 2
sgam = 2

# Confounders
Z = Z %>% select(Age, Gender, Education, Smoking)
Z = model.matrix(~ ., Z)[,-1]
Z = as.data.frame(Z)

# parameters
path.par = paste(system.file("extdata", package = "R2GUESS"), "/", sep = "")
nsweep = 30000
burn.in = 10000

path.output = gsub("Scripts", paste0("Results/R2GUESS/",filepaths[m]), getwd())

## Obesity ----
Y = readRDS(paste0("../Data/",filepaths[m],"_response.rds"))
Y = Y %>% select(BMI, WC, FPG)

ifelse(dir.exists(sprintf("%s/model2.1",path.output)),"",dir.create(sprintf("%s/model2.1",path.output)))
path.input = sprintf("%s/model2.1",path.output)

## Bayesian Variable Selection
model = R2GUESS(Y, X, path.input, path.output,
                nsweep = nsweep, burn.in = burn.in,
                root.file.output = "model2.1", 
                path.par = path.par,
                file.par = "Par_file_pahs.xml",
                time = TRUE, top = 100, history = TRUE,
                Egam = egam, Sgam = sgam, label.Y = colnames(Y), 
                nb.chain = 3,
                conf = Z, cuda = FALSE,
                MAP.file = NULL, seed = 2)

saveRDS(model, paste0("../Results/R2GUESS/Model2.1_",filepaths[m],".rds"))

## Diagnostic plots to assess the convergence of the algorithm
jpeg(paste0("../Figures/R2GUESS/Model2.1_monitor_App_",filepaths[m],".jpg"), width = 20, height = 17, units = "cm", res = 300)
plot(model)
dev.off()

## Diagnostic plots to assess the convergence of the algorithm
pdf(paste0("../Figures/R2GUESS/Model2.1_conv1_",filepaths[m],".pdf"))
check.convergence(model, 1)
dev.off()

pdf(paste0("../Figures/R2GUESS/Model2.1_conv2_",filepaths[m],".pdf"))
check.convergence(model, 2)
dev.off()

## Dyslipidemia ----
Y = readRDS(paste0("../Data/",filepaths[m],"_response.rds"))
Y = Y %>% select(TG, HDLC)

ifelse(dir.exists(sprintf("%s/model2.2",path.output)),"",dir.create(sprintf("%s/model2.2",path.output)))
path.input = sprintf("%s/model2.2",path.output)

## Bayesian Variable Selection
model = R2GUESS(Y, X, path.input, path.output,
                nsweep = nsweep, burn.in = burn.in,
                root.file.output = "model2.2", 
                path.par = path.par,
                file.par = "Par_file_pahs.xml",
                time = TRUE, top = 100, history = TRUE,
                Egam = egam, Sgam = sgam, label.Y = colnames(Y), 
                nb.chain = 3,
                conf = Z, cuda = FALSE,
                MAP.file = NULL, seed = 2)

saveRDS(model, paste0("../Results/R2GUESS/Model2.2_",filepaths[m],".rds"))

## Diagnostic plots to assess the convergence of the algorithm
jpeg(paste0("../Figures/R2GUESS/Model2.2_monitor_App_",filepaths[m],".jpg"), width = 20, height = 17, units = "cm", res = 300)
plot(model)
dev.off()

## Diagnostic plots to assess the convergence of the algorithm
pdf(paste0("../Figures/R2GUESS/Model2.2_conv1_",filepaths[m],".pdf"))
check.convergence(model, 1)
dev.off()

pdf(paste0("../Figures/R2GUESS/Model2.2_conv2_",filepaths[m],".pdf"))
check.convergence(model, 2)
dev.off()
