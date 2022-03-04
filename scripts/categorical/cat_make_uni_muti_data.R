
### Sumemr Project -- Make data
## Created by Vivian on 21 July

rm(list=ls())
project_path="~/hair/scripts"
setwd(project_path)

library(tidyverse)
library(parallel)

##### Load data--
pest <- readRDS("../processed/uni_multi_pest.rds")
pahs <- readRDS("../processed/uni_multi_pahs.rds")

pest_cat_l50 <- readRDS("../processed/cat_imputation/pest_imp_l50.rds")
pest_cat_5080 <- readRDS("../processed/cat_imputation/pest_imp_5080.rds")
pest_cat_h80 <- readRDS("../processed/cat_imputation/pest_imp_h80.rds")
pest_5080 <- readRDS("../processed/cat_imputation/pest_imp_5080_cont.rds")
pest_h80 <- readRDS("../processed/cat_imputation/pest_imp_h80_cont.rds")

pahs_cat_l50 <- readRDS("../processed/cat_imputation/pahs_imp_l50.rds")
pahs_cat_5080 <- readRDS("../processed/cat_imputation/pahs_imp_5080.rds")
pahs_cat_h80 <- readRDS("../processed/cat_imputation/pahs_imp_h80.rds")
pahs_5080 <- readRDS("../processed/cat_imputation/pahs_imp_5080_cont.rds")
pahs_h80 <- readRDS("../processed/cat_imputation/pahs_imp_h80_cont.rds")


##### Combine data --
colnames(pest)
pest_cat <- cbind(pest[ ,c("identifiant", "Gender", "Age", "Country",
                           "Obesity", "MS", "hypertension", "Lipid_disorder", "diabetes")], 
                  pest_cat_l50, pest_cat_5080, pest_cat_h80)
str(pest_cat)
levels(pest_cat$PCB_138)

pest_cont <- cbind(pest[ ,c("identifiant", "Gender", "Age", "Country",
                            "Obesity", "MS", "hypertension", "Lipid_disorder", "diabetes")], 
                   pest_5080, pest_h80)
str(pest_cont)

pahs_cat <- cbind(pahs[ ,c("identifiant", "Gender", "Age", "Country",
                           "Obesity", "MS", "hypertension", "Lipid_disorder", "diabetes")], 
                  pahs_cat_l50, pahs_cat_5080, pahs_cat_h80)
str(pahs_cat)
levels(pahs_cat$Two_OH_Fluorene)

pahs_cont <- cbind(pahs[ ,c("identifiant", "Gender", "Age", "Country",
                            "Obesity", "MS", "hypertension", "Lipid_disorder", "diabetes")], 
                   pahs_5080, pahs_h80)
str(pahs_cont)

saveRDS(pest_cat, "../cat_results/pest_cat.rds")
saveRDS(pahs_cat, "../cat_results/pahs_cat.rds")
saveRDS(pest_cont, "../cat_results/pest_cont.rds")
saveRDS(pahs_cont, "../cat_results/pahs_cont.rds")

