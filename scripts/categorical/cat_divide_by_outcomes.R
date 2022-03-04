
### Sumemr Project -- Divide data according to different outcomes
## Created by Vivian on 21 July


rm(list=ls())
project_path="~/hair/scripts"
setwd(project_path)


##### Load data --
pest_comb_de <- readRDS("../cat_results/denoised/pest_comb_de.rds")
pahs_comb_de <- readRDS("../cat_results/denoised/pahs_comb_de.rds")

pest_de <- readRDS("../cat_results/denoised/pest_de.rds")
pahs_de <- readRDS("../cat_results/denoised/pahs_de.rds")


##### Divide data
outcomes <- c("Obesity", "hypertension", "diabetes", "Lipid_disorder", "MS")

for (i in 1:5) {
  oc <- outcomes[i]
  print(oc)
  tmp <- pahs_de[, oc]
  tmp1 <- pahs_de[, c(1:3, 9:ncol(pahs_de))]
  df <- cbind(tmp1, tmp)
  colnames(df)[ncol(pahs_de)-4] <- oc 
  assign(paste0("pahs_", oc),df)
}

saveRDS(pahs_Obesity, "../cat_results/denoised/pahs_de_obesity.rds")
saveRDS(pahs_hypertension, "../cat_results/denoised/pahs_de_hypertension.rds")
saveRDS(pahs_diabetes, "../cat_results/denoised/pahs_de_diabetes.rds")
saveRDS(pahs_Lipid_disorder, "../cat_results/denoised/pahs_de_LD.rds")
saveRDS(pahs_MS, "../cat_results/denoised/pahs_de_MS.rds")

for (i in 1:5) {
  oc <- outcomes[i]
  print(oc)
  tmp <- pest_de[, oc]
  tmp1 <- pest_de[, c(1:3, 9:ncol(pest_de))]
  df <- cbind(tmp1, tmp)
  colnames(df)[ncol(df)] <- oc 
  assign(paste0("pest_", oc),df)
}

saveRDS(pest_Obesity, "../cat_results/denoised/pest_de_obesity.rds")
saveRDS(pest_hypertension, "../cat_results/denoised/pest_de_hypertension.rds")
saveRDS(pest_diabetes, "../cat_results/denoised/pest_de_diabetes.rds")
saveRDS(pest_Lipid_disorder, "../cat_results/denoised/pest_de_LD.rds")
saveRDS(pest_MS, "../cat_results/denoised/pest_de_MS.rds")


for (i in 1:5) {
  oc <- outcomes[i]
  print(oc)
  tmp <- over_de[, oc]
  tmp1 <- over_de[, 1:(ncol(over_de)-5)]
  df <- cbind(tmp1, tmp)
  colnames(df)[ncol(over_de)-4] <- oc 
  assign(paste0("over_", oc),df)
}

saveRDS(over_Obesity, "../results/Denoised/over_de_obesity.rds")
saveRDS(over_hypertension, "../results/Denoised/over_de_hypertension.rds")
saveRDS(over_diabetes, "../results/Denoised/over_de_diabetes.rds")
saveRDS(over_Lipid_disorder, "../results/Denoised/over_de_Lipid_disorder.rds")
saveRDS(over_MS, "../results/Denoised/over_de_MS.rds")



for (i in 1:5) {
  oc <- outcomes[i]
  print(oc)
  tmp <- pahs_comb_de[, oc]
  tmp1 <- pahs_comb_de[, c(1:3, 9:ncol(pahs_comb_de))]
  df <- cbind(tmp1, tmp)
  colnames(df)[ncol(df)] <- oc 
  assign(paste0("pahs_comb_", oc),df)
}

saveRDS(pahs_comb_Obesity, "../cat_results/denoised/pahs_comb_de_obesity.rds")
saveRDS(pahs_comb_hypertension, "../cat_results/denoised/pahs_comb_de_hypertension.rds")
saveRDS(pahs_comb_diabetes, "../cat_results/denoised/pahs_comb_de_diabetes.rds")
saveRDS(pahs_comb_Lipid_disorder, "../cat_results/denoised/pahs_comb_de_LD.rds")
saveRDS(pahs_comb_MS, "../cat_results/denoised/pahs_comb_de_MS.rds")

for (i in 1:5) {
  oc <- outcomes[i]
  print(oc)
  tmp <- pest_comb_de[, oc]
  tmp1 <- pest_comb_de[, c(1:3, 9:ncol(pest_comb_de))]
  df <- cbind(tmp1, tmp)
  colnames(df)[ncol(df)] <- oc 
  assign(paste0("pest_comb_", oc),df)
}

saveRDS(pest_comb_Obesity, "../cat_results/denoised/pest_comb_de_obesity.rds")
saveRDS(pest_comb_hypertension, "../cat_results/denoised/pest_comb_de_hypertension.rds")
saveRDS(pest_comb_diabetes, "../cat_results/denoised/pest_comb_de_diabetes.rds")
saveRDS(pest_comb_Lipid_disorder, "../cat_results/denoised/pest_comb_de_LD.rds")
saveRDS(pest_comb_MS, "../cat_results/denoised/pest_comb_de_MS.rds")


for (i in 1:5) {
  oc <- outcomes[i]
  print(oc)
  tmp <- over1[, oc]
  tmp1 <- over1[, 1:(ncol(over1)-5)]
  df <- cbind(tmp1, tmp)
  colnames(df)[ncol(over1)-4] <- oc 
  assign(paste0("over1_", oc),df)
}

saveRDS(over1_Obesity, "../results/Denoised/over1_obesity.rds")
saveRDS(over1_hypertension, "../results/Denoised/over1_hypertension.rds")
saveRDS(over1_diabetes, "../results/Denoised/over1_diabetes.rds")
saveRDS(over1_Lipid_disorder, "../results/Denoised/over1_Lipid_disorder.rds")
saveRDS(over1_MS, "../results/Denoised/over1_MS.rds")

