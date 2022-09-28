## Stratify data by gender

rm(list=ls())
project_path=dirname(rstudioapi::getActiveDocumentContext()$path)
setwd(project_path)
setwd("../")

filepaths = c("PesticidesPCBBDEs_train","PAHs_train","Overlap_test")

for (m in 1:3){
  # Load data
  data = readRDS(paste0("../Processed/",filepaths[m],"_XY.rds"))
  
  # Stratify by gender
  male=data[data$Gender=="Male",]
  female=data[data$Gender=="Female",]
  
  saveRDS(female, paste0("../Processed/",filepaths[m],"_XY_Female.rds"))
  saveRDS(male, paste0("../Processed/",filepaths[m],"_XY_Male.rds"))
}
