# R2GUESS

rm(list=ls())
project_path="/rds/general/user/rw1317/home/Hair_pollutant_CVD/Scripts"
setwd(project_path)


library(R2GUESS)
library(tidyverse)

## Parameters
filepaths = c("PesticidesPCBBDEs_train","PAHs_train")

m = 1

ifelse(dir.exists("../Results/"),"",dir.create("../Results/"))
ifelse(dir.exists("../Results/R2GUESS/"),"",dir.create("../Results/R2GUESS/"))
ifelse(dir.exists(paste0("../Results/R2GUESS/",filepaths[m])),"",dir.create(paste0("../Results/R2GUESS/",filepaths[m])))

ifelse(dir.exists("../Figures/"),"",dir.create("../Figures/"))
ifelse(dir.exists("../Figures/R2GUESS/"),"",dir.create("../Figures/R2GUESS/"))


# Load data
Y = readRDS(paste0("../Data/",filepaths[m],"_response.rds"))
Z = readRDS(paste0("../Processed/",filepaths[m],"_Z.rds"))
X = readRDS(paste0("../Processed/",filepaths[m],"_expo_ndimp_naimp_log.rds"))
X = as.data.frame(X)

stab = readRDS(paste0("../Results/R2GUESS/sPLS_output_",filepaths[m],".rds"))

# Select outcomes
colnames(Y)
Y = Y %>% select(BMI, WC, TG, TC, LDLC, HDLC, SBP, DBP, FPG)
Y = Y[,rowSums(stab$selectedY)>0]

# Prior
egam = sum(rowSums(stab$selectedX)>0)
sgam = sum(rowSums(stab$selectedX)>0)

# Confounders
Z = Z %>% select(Age, Gender, Education, Smoking, starts_with("Diet"))
Z = model.matrix(~ ., Z)[,-1]
Z = as.data.frame(Z)

# parameters
path.par = paste(system.file("extdata", package = "R2GUESS"), "/", sep = "")
nsweep = 30000
burn.in = 10000

path.output = gsub("Scripts", paste0("Results/R2GUESS/",filepaths[m]), getwd())
ifelse(dir.exists(sprintf("%s/model3",path.output)),"",dir.create(sprintf("%s/model3",path.output)))
path.input = sprintf("%s/model3",path.output)

## Bayesian Variable Selection
model = R2GUESS(Y, X, path.input, path.output,
                nsweep = nsweep, burn.in = burn.in,
                root.file.output = "model3", 
                path.par = path.par,
                file.par = "Par_file_pest.xml",
                time = TRUE, top = 100, history = TRUE,
                Egam = egam, Sgam = sgam, label.Y = colnames(Y), 
                nb.chain = 3,
                conf = Z, cuda = FALSE,
                MAP.file = NULL, seed = 2)

saveRDS(model, paste0("../Results/R2GUESS/Model3_",filepaths[m],".rds"))

## Diagnostic plots to assess the convergence of the algorithm
jpeg(paste0("../Figures/R2GUESS/Model3_monitor_App_",filepaths[m],".jpg"), width = 20, height = 17, units = "cm", res = 300)
plot(model)
dev.off()

## Diagnostic plots to assess the convergence of the algorithm
pdf(paste0("../Figures/R2GUESS/Model3_conv1_",filepaths[m],".pdf"))
check.convergence(model, 1)
dev.off()

pdf(paste0("../Figures/R2GUESS/Model3_conv2_",filepaths[m],".pdf"))
check.convergence(model, 2)
dev.off()

# set.seed(3)
# n.sweep = get.sweep.best.model(model, models = 1)
# res.g = get.g.sweep(model, n.sweep$result, 1) # error
# beta = sample.beta(model, res.g, Nmonte = 4, Nmonte.sigma = 4)
# 
# saveRDS(res.g, paste0("../Results/R2GUESS/",model_type,"_model_res_",filepaths[m],".rds"))
# saveRDS(beta, paste0("../Results/R2GUESS/",model_type,"_model_beta_",filepaths[m],".rds"))

