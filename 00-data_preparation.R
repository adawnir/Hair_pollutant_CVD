## Data cleaning

rm(list=ls())
project_path=dirname(rstudioapi::getActiveDocumentContext()$path)
setwd(project_path)

library(tidyverse)
library(colorspace)

data = read.csv("../Data/Dataset_PesticidesPCBBDEs.csv")
str(data)
colnames(data) = data[1,]
colnames(data)[16:48] = readLines("Dictionaries/PesticidesPCBBDEs_names.txt")
pest = data[-c(1:2),]
rownames(pest) = pest$identifiant
dim(pest)    # 1096   48

data = read.csv("../Data/Dataset_PAHs.csv")
colnames(data) = data[1,]
colnames(data)[16:28] = readLines("Dictionaries/PAHs_names.txt")
pahs <- data[-c(1:2),]
rownames(pahs) = pahs$identifiant
dim(pahs)    # 682   28

### Exclude participants with missing outcomes (clinical measurements)
colnames(pest)[6:15]
colnames(pahs)[6:15]
pest = pest[complete.cases(pest[,6:15]),]
pahs = pahs[complete.cases(pahs[,6:15]),]
dim(pest)    # 1065   48
dim(pahs)    # 670   28

ifelse(dir.exists("../Exports/"), "", dir.create("../Exports/"))

saveRDS(pest, "../Data/PesticidesPCBBDEs.rds")
write.csv(pest, paste0("../Exports/PesticidesPCBBDEs.csv"))
saveRDS(pahs, "../Data/PAHs.csv")
write.csv(pahs, paste0("../Exports/PAHs.csv"))




