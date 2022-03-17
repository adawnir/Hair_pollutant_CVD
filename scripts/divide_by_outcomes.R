
### Sumemr Project -- Divide data according to different outcomes
## Created by Vivian on 01 July


rm(list=ls())
project_path="~/hair/scripts"
setwd(project_path)


##### Load data --
pest1 <- readRDS("../results/Denoised/pest_one.rds")
pahs1 <- readRDS("../results/Denoised/pahs_one.rds")
over1 <- readRDS("../results/Denoised/over_one.rds")

pest_de <- readRDS("../results/Denoised/pest_de.rds")
pahs_de <- readRDS("../results/Denoised/pahs_de.rds")
over_de <- readRDS("../results/Denoised/over_de.rds")


##### Divide data
outcomes <- c("Obesity", "hypertension", "diabetes", "Lipid_disorder", "MS")

for (i in 1:5) {
  oc <- outcomes[i]
  print(oc)
  tmp <- pahs_de[, oc]
  tmp1 <- pahs_de[, 1:(ncol(pahs_de)-5)]
  df <- cbind(tmp1, tmp)
  colnames(df)[ncol(pahs_de)-4] <- oc 
  assign(paste0("pahs_", oc),df)
}

saveRDS(pahs_Obesity, "../results/Denoised/pahs_de_obesity.rds")
saveRDS(pahs_hypertension, "../results/Denoised/pahs_de_hypertension.rds")
saveRDS(pahs_diabetes, "../results/Denoised/pahs_de_diabetes.rds")
saveRDS(pahs_Lipid_disorder, "../results/Denoised/pahs_de_Lipid_disorder.rds")
saveRDS(pahs_MS, "../results/Denoised/pahs_de_MS.rds")

for (i in 1:5) {
  oc <- outcomes[i]
  print(oc)
  tmp <- pest_de[, oc]
  tmp1 <- pest_de[, 1:(ncol(pest_de)-5)]
  df <- cbind(tmp1, tmp)
  colnames(df)[ncol(pest_de)-4] <- oc 
  assign(paste0("pest_", oc),df)
}

saveRDS(pest_Obesity, "../results/Denoised/pest_de_obesity.rds")
saveRDS(pest_hypertension, "../results/Denoised/pest_de_hypertension.rds")
saveRDS(pest_diabetes, "../results/Denoised/pest_de_diabetes.rds")
saveRDS(pest_Lipid_disorder, "../results/Denoised/pest_de_Lipid_disorder.rds")
saveRDS(pest_MS, "../results/Denoised/pest_de_MS.rds")


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
  tmp <- pahs1[, oc]
  tmp1 <- pahs1[, 1:(ncol(pahs1)-5)]
  df <- cbind(tmp1, tmp)
  colnames(df)[ncol(pahs1)-4] <- oc 
  assign(paste0("pahs1_", oc),df)
}

saveRDS(pahs1_Obesity, "../results/Denoised/pahs1_obesity.rds")
saveRDS(pahs1_hypertension, "../results/Denoised/pahs1_hypertension.rds")
saveRDS(pahs1_diabetes, "../results/Denoised/pahs1_diabetes.rds")
saveRDS(pahs1_Lipid_disorder, "../results/Denoised/pahs1_Lipid_disorder.rds")
saveRDS(pahs1_MS, "../results/Denoised/pahs1_MS.rds")

for (i in 1:5) {
  oc <- outcomes[i]
  print(oc)
  tmp <- pest1[, oc]
  tmp1 <- pest1[, 1:(ncol(pest1)-5)]
  df <- cbind(tmp1, tmp)
  colnames(df)[ncol(pest1)-4] <- oc 
  assign(paste0("pest1_", oc),df)
}

saveRDS(pest1_Obesity, "../results/Denoised/pest1_obesity.rds")
saveRDS(pest1_hypertension, "../results/Denoised/pest1_hypertension.rds")
saveRDS(pest1_diabetes, "../results/Denoised/pest1_diabetes.rds")
saveRDS(pest1_Lipid_disorder, "../results/Denoised/pest1_Lipid_disorder.rds")
saveRDS(pest1_MS, "../results/Denoised/pest1_MS.rds")


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

