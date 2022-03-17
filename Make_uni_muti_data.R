
### Sumemr Project -- Make data
## Created by Vivian on 30 June reviewed on 01 July added on 28 July

rm(list=ls())
project_path="~/hair/scripts"
setwd(project_path)

library(tidyverse)


##### Load data --
pest <- readRDS("../processed/imputation/imp_pest.rds")
pahs <- readRDS("../processed/imputation/imp_pahs.rds")
pest_log10 <- readRDS("../processed/imputation/log10_pest.rds")
pahs_log10 <- readRDS("../processed/imputation/log10_pahs.rds")

pest_cov <- readRDS("../processed/PesticidesPCBBDEs_complete.rds")
pahs_cov <- readRDS("../processed/PAHs_complete.rds")


##### Recode CVRFs --
pest_tmp <- pest_cov 
pahs_tmp <- pahs_cov 


pest_tmp$Obesity = as.factor(ifelse(pest_tmp$BMI < 30, 0, 1))
pest_tmp$WC <- as.factor(ifelse(pest_tmp$WC == "Normal", 0, 1))
pest_tmp$hypertension <- as.factor(ifelse(pest_tmp$hypertension == "Normal", 0, 1))
pest_tmp$diabetes <- as.factor(ifelse(pest_tmp$diabetes == "Normal", 0, 1))
pest_tmp$Lipid_disorder <- as.factor(ifelse(pest_tmp$Lipid_disorder == "Normal", 0, 1))
pest_tmp$MS <- as.factor(ifelse(pest_tmp$MS < 3, 0, 1))

pahs_tmp$Obesity = as.factor(ifelse(pahs_tmp$BMI < 30, 0, 1))
pahs_tmp$WC <- as.factor(ifelse(pahs_tmp$WC == "Normal", 0, 1))
pahs_tmp$hypertension <- as.factor(ifelse(pahs_tmp$hypertension == "Normal", 0, 1))
pahs_tmp$diabetes <- as.factor(ifelse(pahs_tmp$diabetes == "Normal", 0, 1))
pahs_tmp$Lipid_disorder <- as.factor(ifelse(pahs_tmp$Lipid_disorder == "Normal", 0, 1))
pahs_tmp$MS <- as.factor(ifelse(pahs_tmp$MS < 3, 0, 1))


##### Combine data --
pahs_final <- cbind(pahs_tmp[, c(1:7,29, 8:15)], pahs, pahs_tmp[, c(30, 40, 31:33, 39, 34:38)])
pest_final <- cbind(pest_tmp[, c(1:7,49, 8:15)], pest, pest_tmp[, c(50, 60, 51:53, 59, 54:58)])


##### Revode names --
colnames(pahs_final)[c(3:4,6,9:10,16:29)] <- c("Gender", "Fasting", "Waist_measurement",
                                            "SBP", "DBP", "Plasma_sugar",
                                            "Cotinine", "Nicotine",
                                            "One_OH_Naphthalene", "Two_OH_Naphthalene", "Nine_OH_Fluorene",
                                            "Three_OH_Fluorene", "Two_OH_Fluorene", "Four_OH_Phenanthrene",
                                            "Three_OH_Phenanthrene","One_OH_Phenanthrene",
                                            "Two_OH_Phenanthrene", "One_OH_Pyrene",
                                            "Six_OH_Chrysene11_OH_Benz_a_anthrac")

colnames(pest_final)[c(3:4,6,9:10,16:26, 38, 42, 44:46)] <- c("Gender", "Fasting", "Waist_measurement",
                                                              "SBP", "DBP", "Plasma_sugar",
                                                              "PCB_138", "PCB_153", "PCB_180", "BDE_47",
                                                              "p_p_DDT", "p_p_DDE",
                                                              "alpha_endosulfan", "beta_endosulfan",
                                                              "beta_HCH", "gamma_HCH",
                                                              "Three_Me4NP", "Three_PBA", 
                                                              "Fipronil_sulfone","Two_IPP",
                                                              "Carbofuran_phenol")


##### Relevel --
levels(pahs_final$Gender)
pahs_final$Country <- relevel(pahs_final$Country, ref = "Luxembourg")

pest_final$Country <- relevel(pest_final$Country, ref = "Luxembourg")

saveRDS(pahs_final, "../processed/uni_multi_pahs.rds")
saveRDS(pest_final, "../processed/uni_multi_pest.rds")


##### Overlapping --
sum(pahs_final$identifiant %in% pest_final$identifiant)    # only 213
ids <- pahs_final$identifiant[which(pahs_final$identifiant %in% pest_final$identifiant)]
pahs_over <- pahs_final[which(pahs_final$identifiant %in% ids),]
pest_over <- pest_final[which(pest_final$identifiant %in% ids),] %>%  dplyr::select(1,17:49)
overlap <- inner_join(pahs_over, pest_over, by = "identifiant")

colnames(overlap)
overlap <- overlap[, c(1:29, 41:73, 30:40)]

saveRDS(overlap, "../processed/uni_multi_overlap.rds")


id1 <- which(pahs_final$identifiant %in% pest_final$identifiant)
tmp1 <- pahs[id1,]
id2 <- which(pest_final$identifiant %in% pahs_final$identifiant)
tmp2 <- pest[id2,]
overlap_imp <- cbind(tmp1, tmp2)

saveRDS(overlap_imp, "../processed/imputation/imp_overlap.rds")

tmp3 <- pahs_log10[id1,]
tmp4 <- pest_log10[id2,]
overlap_log10 <- cbind(tmp3, tmp4)
saveRDS(overlap_log10, "../processed/imputation/log10_overlap.rds")
