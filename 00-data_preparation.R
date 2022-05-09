## Data cleaning

rm(list=ls())
project_path=dirname(rstudioapi::getActiveDocumentContext()$path)
setwd(project_path)

library(tidyverse)
library(colorspace)

## Extract exposure matrices
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

## Covariate information
str(pest)
str(pahs) 

# Remove redundant covariate
pest = pest[,-which(colnames(pest) %in% c("ldl_hdl","Is the subject fasting?"))]

str(pest)
colnames(pest)[1:13] = c("ID","Country","Gender","Age",
                         "WC","BMI","SysBP","DiaBP",
                         "TC","LDL-C", "HDL-C", "TG", "FPG")

# Set classes for variables
class(pest$ID)

table(pest$Country)
pest$Country = factor(pest$Country)
class(pest$Country)

table(pest$Gender)
pest$Gender = factor(ifelse(pest$Gender=="F", "Female", "Male"))
class(pest$Gender)
pest$Gender = relevel(pest$Gender, "Male")

pest[,4:13] = apply(pest[,4:13], 2, as.numeric)

str(pest)

## Covariate transformation
summary(pest$BMI)
pest$BMI_cat = ifelse(pest$BMI <18.5,"Underweight",
                      ifelse(pest$BMI >=18.5 & pest$BMI <25, "Normal",
                             ifelse(pest$BMI >=25 & pest$BMI <30, "Pre-obesity",
                                    ifelse(pest$BMI >=30 & pest$BMI <35, "Obesity class I",
                                           ifelse(pest$BMI >=35 & pest$BMI <40, "Obesity class II",
                                                  "Obesity class III")))))
table(pest$BMI_cat)
pest$BMI_cat = factor(pest$BMI_cat)
class(pest$BMI_cat)

### WC
summary(pest$WC)
pest$WC_cat = ifelse(pest$WC <102 & pest$Gender == "Male", "Normal",
                     ifelse(pest$WC >=102 & pest$Gender == "Male", "High",
                        ifelse(pest$WC <88 & pest$Gender == "Female", "Normal",
                               "High")))
table(pest$WC_cat)
pest$WC_cat = factor(pest$WC_cat)
class(pest$WC_cat)

## Outcome ascertainment

# Hypertension
summary(pest$SysBP)
summary(pest$DiaBP)
pest$Hypertension = ifelse(pest$SysBP < 140 & pest$DiaBP < 90, "No","Yes")
table(pest$Hypertension)
pest$Hypertension = factor(pest$Hypertension)
class(pest$Hypertension)

# Diabetes
summary(pest$FPG)
pest$Diabetes = ifelse(pest$FPG < 126, "No","Yes")
table(pest$Diabetes)
pest$Diabetes = factor(pest$Diabetes)
class(pest$Diabetes)

# Lipid disorder
# TC (total cholesterol) >= 190 mg/dl, TG (triglycerides) >= 150 mg/dl, 
# LDL-C ≥ 115 mg/dl, and HDLC < 40 mg/dl for men and < 46 mg/dl for women
pest$`Lipid disorder`=ifelse(pest$TC <190 & pest$TG <150 & pest$`LDL-C` <115 & pest$`HDL-C` >=40 & pest$Gender == "Male", "No",
                             ifelse(pest$TC <190 & pest$TG <150 & pest$`LDL-C` <115 & pest$`HDL-C`>=46 & pest$Gender == "Female", "No",
                                    "Yes"))
table(pest$`Lipid disorder`)
pest$`Lipid disorder` = factor(pest$`Lipid disorder`)
class(pest$`Lipid disorder`)

# Metabolic syndrome
# 1) WC ≥ 102 cm for men and ≥ 88 cm for women;
# 2) raised concentration of TG ≥ 150 mg/dl 
# 3) reduced concentration of HDL-C < 40 mg/dl for men and < 50 mg/dl for women 
# 4) SBP was ≥ 130 mmHg, or DBP ≥ 85 mmHg;
# 5) FPG level ≥100 mg/dl

score = ifelse(pest$WC_cat=="High", 1, 0)
score = score + ifelse(pest$TG >=150, 1, 0)
score = score + ifelse(pest$`HDL-C` <40 & pest$Gender == "Male", 1,
                       ifelse(pest$`HDL-C` <50 & pest$Gender == "Female", 1,
                              0))
score = score + ifelse(pest$SysBP >=130 | pest$DiaBP >=85, 1, 0)
score = score + ifelse(pest$FPG >=100, 1, 0)
table(score)

pest$`Metabolic syndrome` = ifelse(score>=3, "Yes", "No")
table(pest$`Metabolic syndrome`)

## Reorder columns
colnames(pest)
pest = pest[,c(1:13,47:52,14:46)]

# Remove redundant covariate
pahs = pahs[,-which(colnames(pahs) %in% c("ldl_hdl","Is the subject fasting?"))]

str(pahs)
colnames(pahs)[1:13] = c("ID","Country","Gender","Age",
                         "WC","BMI","SysBP","DiaBP",
                         "TC","LDL-C", "HDL-C", "TG", "FPG")

# Set classes for variables
class(pahs$ID)

table(pahs$Country)
pahs$Country = factor(pahs$Country)
class(pahs$Country)

table(pahs$Gender)
pahs$Gender = factor(ifelse(pahs$Gender=="F", "Female", "Male"))
class(pahs$Gender)
pahs$Gender = relevel(pahs$Gender, "Male")

pahs[,4:13] = apply(pahs[,4:13], 2, as.numeric)

str(pahs)

## Covariate transformation
# BMI
summary(pahs$BMI)
pahs$BMI_cat = ifelse(pahs$BMI <18.5,"Underweight",
                      ifelse(pahs$BMI >=18.5 & pahs$BMI <25, "Normal",
                             ifelse(pahs$BMI >=25 & pahs$BMI <30, "Pre-obesity",
                                    ifelse(pahs$BMI >=30 & pahs$BMI <35, "Obesity class I",
                                           ifelse(pahs$BMI >=35 & pahs$BMI <40, "Obesity class II",
                                                  "Obesity class III")))))
table(pahs$BMI_cat)
pahs$BMI_cat = factor(pahs$BMI_cat)
class(pahs$BMI_cat)

# WC
summary(pahs$WC)
pahs$WC_cat = ifelse(pahs$WC <102 & pahs$Gender == "Male", "Normal",
                     ifelse(pahs$WC >=102 & pahs$Gender == "Male", "High",
                            ifelse(pahs$WC <88 & pahs$Gender == "Female", "Normal",
                                   "High")))
table(pahs$WC_cat)
pahs$WC_cat = factor(pahs$WC_cat)
class(pahs$WC_cat)

## Outcome ascertainment

# Hypertension
summary(pahs$SysBP)
summary(pahs$DiaBP)
pahs$Hypertension = ifelse(pahs$SysBP < 140 & pahs$DiaBP < 90, "No","Yes")
table(pahs$Hypertension)
pahs$Hypertension = factor(pahs$Hypertension)
class(pahs$Hypertension)

# Diabetes
summary(pahs$FPG)
pahs$Diabetes = ifelse(pahs$FPG < 126, "No","Yes")
table(pahs$Diabetes)
pahs$Diabetes = factor(pahs$Diabetes)
class(pahs$Diabetes)

# Lipid disorder
# TC (total cholesterol) >= 190 mg/dl, TG (triglycerides) >= 150 mg/dl, 
# LDL-C ≥ 115 mg/dl, and HDLC < 40 mg/dl for men and < 46 mg/dl for women
pahs$`Lipid disorder`=ifelse(pahs$TC <190 & pahs$TG <150 & pahs$`LDL-C` <115 & pahs$`HDL-C` >=40 & pahs$Gender == "Male", "No",
                             ifelse(pahs$TC <190 & pahs$TG <150 & pahs$`LDL-C` <115 & pahs$`HDL-C`>=46 & pahs$Gender == "Female", "No",
                                    "Yes"))
table(pahs$`Lipid disorder`)
pahs$`Lipid disorder` = factor(pahs$`Lipid disorder`)
class(pahs$`Lipid disorder`)

# Metabolic syndrome
# 1) WC ≥ 102 cm for men and ≥ 88 cm for women;
# 2) raised concentration of TG ≥ 150 mg/dl 
# 3) reduced concentration of HDL-C < 40 mg/dl for men and < 50 mg/dl for women 
# 4) SBP was ≥ 130 mmHg, or DBP ≥ 85 mmHg;
# 5) FPG level ≥100 mg/dl

score = ifelse(pahs$WC_cat=="High", 1, 0)
score = score + ifelse(pahs$TG >=150, 1, 0)
score = score + ifelse(pahs$`HDL-C` <40 & pahs$Gender == "Male", 1,
                       ifelse(pahs$`HDL-C` <50 & pahs$Gender == "Female", 1,
                              0))
score = score + ifelse(pahs$SysBP >=130 | pahs$DiaBP >=85, 1, 0)
score = score + ifelse(pahs$FPG >=100, 1, 0)
table(score)

pahs$`Metabolic syndrome` = ifelse(score>=3, "Yes", "No")
table(pahs$`Metabolic syndrome`)

## Reorder columns
colnames(pahs)
pahs = pahs[,c(1:13,27:32,14:26)]

## Overlapping participants
comb = cbind(pest[(intersect(pest$ID, pahs$ID)),],
             pahs[(intersect(pest$ID, pahs$ID)),20:ncol(pahs)])
colnames(comb)

ifelse(dir.exists("../Exports/"), "", dir.create("../Exports/"))
saveRDS(pest, "../Data/PesticidesPCBBDEs.rds")
write.csv(pest, paste0("../Exports/PesticidesPCBBDEs.csv"))
saveRDS(pahs, "../Data/PAHs.csv")
write.csv(pahs, paste0("../Exports/PAHs.csv"))
saveRDS(pahs, "../Data/Combined.csv")
write.csv(pahs, paste0("../Exports/Combined.csv"))



