# R2GUESS

rm(list=ls())
project_path="/rds/general/user/rw1317/home/Hair_pollutant_CVD/Scripts"
setwd(project_path)


library(R2GUESS)
library(tidyverse)

## Parameters
trait_group = c("Core","Core+FPG","All_traits")
filepath = "PesticidesPCBBDEs"

args=commandArgs(trailingOnly=TRUE)
m=as.numeric(args[1])

mat = matrix(rep(0:4,length(trait_group)), ncol=length(trait_group))
f = mat[m]
i = arrayInd(m, dim(mat))[2]

ifelse(dir.exists("../Results/"),"",dir.create("../Results/"))
ifelse(dir.exists("../Results/R2GUESS/"),"",dir.create("../Results/R2GUESS/"))
ifelse(dir.exists(paste0("../Results/R2GUESS/",trait_group[i])),"",dir.create(paste0("../Results/R2GUESS/",trait_group[i])))

ifelse(dir.exists("../Figures/"),"",dir.create("../Figures/"))
ifelse(dir.exists("../Figures/R2GUESS/"),"",dir.create("../Figures/R2GUESS/"))
ifelse(dir.exists(paste0("../Figures/R2GUESS/",trait_group[i])),"",dir.create(paste0("../Figures/R2GUESS/",trait_group[i])))


# Load data
trait = readRDS(paste0("../Data/",filepath,"_trait.rds"))
Z = readRDS(paste0("../Processed/",filepath,"_Z.rds"))
X = readRDS(paste0("../Processed/",filepath,"_expo_ndimp_naimp_log.rds"))
X = as.data.frame(X)

# Select outcomes
colnames(response)
if (i==1){
  Y = response %>% select(BMI, WC, TG, HDLC, SBP, DBP)
}
if (i==2){
  Y = response %>% select(BMI, WC, TG, HDLC, SBP, DBP, FPG)
}
if (i==3){
  Y = response %>% select(BMI, WC, TG, TC, LDLC, HDLC, SBP, DBP, FPG)
}

if (f == 0){
  Z = Z %>% select(Age, Gender)
}
if (f == 1){
  Z = Z %>% select(Age, Gender, Centre)
}
if (f == 2){
  Z = Z %>% select(Age, Gender, Education)
}
if (f == 3){
  Z = Z %>% select(Age, Gender, Education, Smoking)
}
if (f == 4){
  Z = Z %>% select(Age, Gender, Education, Smoking, starts_with("Diet"))
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

path.output = gsub("Scripts", paste0("Results/R2GUESS/",trait_group[i]), getwd())
ifelse(dir.exists(sprintf(paste0("%s/model",f),path.output)),"",dir.create(sprintf(paste0("%s/model",f), path.output)))
path.input = sprintf(paste0("%s/model",f), path.output)

## Bayesian Variable Selection
model = R2GUESS(Y, X, path.input, path.output,
                nsweep = nsweep, burn.in = burn.in,
                root.file.output = paste0("model",f), 
                path.par = path.par,
                file.par = "Par_file_pest.xml",
                time = TRUE, top = 100, history = TRUE,
                Egam = egam, Sgam = sgam, label.Y = colnames(Y), 
                nb.chain = 3,
                conf = Z, cuda = FALSE,
                MAP.file = NULL, seed = 2)

saveRDS(model, paste0("../Results/R2GUESS/",trait_group[i],"/Model",f,".rds"))

## Diagnostic plots to assess the convergence of the algorithm
jpeg(paste0("../Figures/R2GUESS/",trait_group[i],"/Model",f,"_monitor_App.jpg"), width = 20, height = 17, units = "cm", res = 300)
plot(model)
dev.off()

## Diagnostic plots to assess the convergence of the algorithm
pdf(paste0("../Figures/R2GUESS/",trait_group[i],"/Model",f,"_conv1.pdf"))
check.convergence(model, 1)
dev.off()

pdf(paste0("../Figures/R2GUESS/",trait_group[i],"/Model",f,"_conv2.pdf"))
check.convergence(model, 2)
dev.off()

set.seed(3)
n.sweep = get.sweep.best.model(model, models = 1)
res.g = get.g.sweep(model, n.sweep$result, 1)
beta = sample.beta(model, res.g, Nmonte = 2, Nmonte.sigma = 2)

saveRDS(res.g, paste0("../Results/R2GUESS/",trait_group[i],"/Model",f,"_res.rds"))
saveRDS(beta, paste0("../Results/R2GUESS/",trait_group[i],"/Model",f,"_beta.rds"))
