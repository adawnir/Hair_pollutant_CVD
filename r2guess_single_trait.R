# R2GUESS

rm(list=ls())
project_path="/rds/general/user/rw1317/home/Hair_pollutant_CVD/Scripts"
setwd(project_path)


library(R2GUESS)
library(tidyverse)

## Parameters
filepath = "PesticidesPCBBDEs"
args=commandArgs(trailingOnly=TRUE)
m=as.numeric(args[1])

# Load data
trait = readRDS(paste0("../Data/",filepath,"_trait.rds"))
Z = readRDS(paste0("../Processed/",filepath,"_Z.rds"))
X = readRDS(paste0("../Processed/",filepath,"_expo_ndimp_naimp_log.rds"))
X = as.data.frame(X)

# Select outcomes
Y = trait %>% dplyr::select(BMI, WC, TG, TC, LDLC, HDLC, SBP, DBP, FPG)
trait_group = colnames(trait)

mat = matrix(rep(0:3,length(trait_group)), ncol=length(trait_group))
f = mat[m]
i = arrayInd(m, dim(mat))[2]

if (f == 0){
  Z = Z %>% dplyr::select(Age, Gender)
}
if (f == 1){
  Z = Z %>% dplyr::select(Age, Gender, Education)
}
if (f == 2){
  Z = Z %>% dplyr::select(Age, Gender, Education, Smoking)
}
if (f == 3){
  Z = Z %>% dplyr::select(Age, Gender, Education, Smoking, starts_with("Diet"))
}
# Confounders
Z = model.matrix(~ ., Z)[,-1]
Z = as.data.frame(Z)

# parameters
# Prior
egam = 5
sgam = 4
path.par = paste(system.file("extdata", package = "R2GUESS"), "/", sep = "")
nsweep = 30000
burn.in = 10000

ifelse(dir.exists("../Results/"),"",dir.create("../Results/"))
path.output = gsub("Scripts", paste0("Results/GUESS-",trait_group[i]), getwd())
ifelse(dir.exists(path.output),"",dir.create(path.output))
path.input = sprintf(paste0("%s/model",f,"_input"), path.output)
ifelse(dir.exists(path.input),"",dir.create(path.input))

## Bayesian Variable Selection
model = R2GUESS(Y[,i, drop = FALSE], X, path.input, path.output,
                nsweep = nsweep, burn.in = burn.in,
                root.file.output = paste0("model",f), 
                path.par = path.par,
                file.par = "Par_file_pest.xml",
                time = TRUE, top = 100, history = TRUE,
                Egam = egam, Sgam = sgam, label.Y = colnames(Y)[,i], 
                nb.chain = 3,
                conf = Z, cuda = FALSE,
                MAP.file = NULL, seed = 2)

saveRDS(model, paste0(path.output,"/model",f,"_ESS.rds"))

