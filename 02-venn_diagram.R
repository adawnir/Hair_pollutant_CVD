## Co-occurence of CVDRFs

rm(list=ls())
project_path=dirname(rstudioapi::getActiveDocumentContext()$path)
setwd(project_path)

# install.packages("VennDiagram")
library(VennDiagram)
library(tidyverse)
library(colorspace)

filepaths = c("PesticidesPCBBDEs","PAHs","Combined")
for (i in 1:3){
  data = readRDS(paste0("../Processed/",filepaths[i],"_XY.rds"))
  
  obesity = data$ID[which(data$Obesity == "Yes")]
  hypertension = data$ID[which(data$Hypertension == "Yes")]
  diabetes = data$ID[which(data$Diabetes == "Yes")]
  ld = data$ID[which(data$`Lipid disorder` == "Yes")]
  ms = data$ID[which(data$`Metabolic syndrome` == "Yes")]
  
  mylist = list(obesity, hypertension, diabetes, ld, ms)
  mycolours = c("orange","lightcoral", "skyblue", "limegreen", "purple")
  mynames = paste0(c("Obesity", "Hypertension", "Diabetes", "Lipid disorder", "Metabolic syndrome"), 
                 " (N=", sapply(mylist, length), ")")
  venn.diagram(mylist, filename = paste0("../Figures/",filepaths[i],"_venn_diagram_outcome.tiff"),
               fill = mycolours, fontfamily ="sans", cat.fontfamily ="sans",
               print.mode=c("raw"), cex = 0.8, cat.cex = 0.8,
               category.names = mynames, imagetype = "tiff", 
               cat.just = list(c(0.5, 1), c(0, -6), c(0, 0), c(0.5, 0.5), c(1, -6)))
}

