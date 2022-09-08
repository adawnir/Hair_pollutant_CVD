## Co-occurence of CVDRFs

rm(list=ls())
project_path=dirname(rstudioapi::getActiveDocumentContext()$path)
setwd(project_path)

# install.packages("VennDiagram")
library(VennDiagram)
library(tidyverse)
library(colorspace)

{
  data = readRDS(paste0("../Data/","All_participants_info",".rds"))
  
  obesity = data$ID[which(data$Obesity == "Yes")]
  hypertension = data$ID[which(data$Hypertension == "Yes")]
  diabetes = data$ID[which(data$Diabetes == "Yes")]
  ld = data$ID[which(data$Lipid_disorder == "Yes")]
  ms = data$ID[which(data$Metabolic_syndrome == "Yes")]
  
  mylist = list(obesity, hypertension, diabetes, ld, ms)
  mycolours = c("orange","lightcoral", "skyblue", "limegreen", "purple")
  mynames = paste0(c("Obesity", "Hypertension", "Diabetes", "Lipid disorder", "Metabolic syndrome"), 
                 " (N=", sapply(mylist, length), ")")
  venn.diagram(mylist, filename = paste0("../Figures/Venn_diagram_outcome_","All_participants",".tiff"),
               fill = mycolours, fontfamily ="sans", cat.fontfamily ="sans",
               print.mode=c("raw"), cex = 0.8, cat.cex = 0.8,
               category.names = mynames, imagetype = "tiff", 
               cat.just = list(c(0.5, 1), c(0, -6), c(0, 0), c(0.5, 0.5), c(1, -6)))
}

