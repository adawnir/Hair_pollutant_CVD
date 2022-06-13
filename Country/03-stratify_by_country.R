## Stratify data by country

rm(list=ls())
project_path=dirname(rstudioapi::getActiveDocumentContext()$path)
setwd(project_path)
setwd("../")

filepaths = c("PesticidesPCBBDEs","PAHs","Combined")

for (m in 1:2){
  # Load data
  data = readRDS(paste0("../Processed/",filepaths[m],"_XY.rds"))
  
  # Stratify by gender
  lux=data[data$Country=="Luxembourg",]
  bel=data[data$Country=="Belgium",]
  if (m == 1){
    fra=data[data$Country=="France",]
    saveRDS(fra, paste0("../Processed/",filepaths[m],"_XY_France.rds"))
  }
  saveRDS(lux, paste0("../Processed/",filepaths[m],"_XY_Luxembourg.rds"))
  saveRDS(bel, paste0("../Processed/",filepaths[m],"_XY_Belgium.rds"))
}
