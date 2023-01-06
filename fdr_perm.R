# R2GUESS
rm(list=ls())
project_path="/rds/general/user/rw1317/home/Hair_pollutant_CVD/Scripts"
setwd(project_path)

args=commandArgs(trailingOnly=TRUE)
m=as.numeric(args[1])
nbcpu = 2
  
library(R2GUESS)
library(tidyverse)

filepath = "PesticidesPCBBDEs"
trait = readRDS(paste0("../Data/",filepath,"_trait.rds"))
Y = trait %>% dplyr::select(BMI, WC, TG, TC, LDLC, HDLC, SBP, DBP, FPG)
trait_group = c(colnames(Y),"Core","Plus_FPG","All_traits")

f = 3
i = m

model = readRDS(paste0("../Results/GUESS-",trait_group[i],"/model",f,"_ESS.rds"))

perm.input = sprintf("%s/perm_input/", model$path.output)
perm.output = sprintf("%s/perm_output/", model$path.output)

npermut = 100

FDRthresh = FDR.permutation(x = model, threshold = 0.05,
                            Npermut = npermut, number.cutoff = model$p,
                            path.input = perm.input, path.output = perm.output,
                            nbcpu = nbcpu)

saveRDS(FDRthresh,paste0(model$path.output,"/model",f,"_fdr_perm100.rds"))

