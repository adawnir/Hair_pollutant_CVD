## Data cleaning

rm(list=ls())
project_path=dirname(rstudioapi::getActiveDocumentContext()$path)
setwd(project_path)

library(tidyverse)
library(readxl)
library(colorspace)

### Pesticides and POPs ----
## Extract exposure matrices
data = read_excel("../Data/Dataset_Pesticides_PAHs_05242022.xlsx", sheet = 1)
str(data)
colnames(data)[38:70] = readLines("../Dictionaries/PesticidesPCBBDEs_names.txt")
pest = as.data.frame(data)
rownames(pest) = pest$identifiant
dim(pest)    # 989   70

# Subset relevant columns
pest = pest[,-which(colnames(pest) %in% c("Fasting","LDLC_HDLC","MetS_WC","MetS_TG","MetS_HDLC",
                                          "MetS_BP","MetS_FPG"))]

str(pest)
colnames(pest)[1] = "ID"
colnames(pest)[23:25] = c("Diabetes_medication","Hypertension_medication","Lipid_disorder_medication")
colnames(pest)[29:30] = c("Lipid_disorder","Metabolic_syndrome")

# Set classes for variables
class(pest$ID)

table(pest$Country)
pest$Country = factor(pest$Country)
class(pest$Country)

table(pest$Gender)
pest$Gender = factor(ifelse(pest$Gender=="F", "Female", "Male"))
class(pest$Gender)
pest$Gender = relevel(pest$Gender, "Male")

table(pest$Smoking)
pest$Smoking = factor(pest$Smoking, 1:3, labels = c("Never smoker","Former smoker","Current smoker"))

table(pest$Physical_activity)
pest$Physical_activity = factor(pest$Physical_activity, 1:3, labels = c("Inactive","Moderately active","Active"))

table(pest$Alcohol)
pest$Alcohol = factor(pest$Alcohol, 1:3, labels = c("Low","Moderate","High"))

table(pest$Education)
pest$Education = factor(pest$Education, 1:3, labels = c("Low","Intermediate","High"))

str(pest)

## Exclude participants with >50% nutrient information missing
hist(rowSums(is.na(pest[10:14]))/5)
pest = pest[rowSums(is.na(pest[10:14]))/5 < 0.5,]
hist(rowSums(is.na(pest[6:14]))/9)

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
pest$WC_cat = ifelse(pest$WC >=102 & pest$Gender == "Male", "High",
                     ifelse(pest$WC >=88 & pest$Gender == "Female", "High","Normal"))
table(pest$WC_cat)
pest$WC_cat = factor(pest$WC_cat)
class(pest$WC_cat)

### Medication
pest$Diabetes_medication = factor(pest$Diabetes_medication)
pest$Hypertension_medication = factor(pest$Hypertension_medication)
pest$Lipid_disorder_medication = factor(pest$Lipid_disorder_medication)

pest = pest %>% mutate_at(vars(ends_with("_medication")),
                          function(x) factor(x, 0:1, labels = c("No","Yes")))

## Outcome ascertainment
# Obesity
# pest$Obesity = ifelse(pest$BMI <30, "No","Yes")
table(pest$Obesity)
pest$Obesity = factor(pest$Obesity, 0:1, labels = c("No","Yes"))
class(pest$Obesity)

# Hypertension
summary(pest$SBP)
summary(pest$DBP)
# pest$Hypertension = ifelse(pest$SysBP < 140 & pest$DiaBP < 90 & pest$Hypertension_medication == 01", "No","Yes")
table(pest$Hypertension)
pest$Hypertension = factor(pest$Hypertension, 0:1, labels = c("No","Yes"))
class(pest$Hypertension)

# Diabetes
summary(pest$FPG)
# pest$Diabetes = ifelse(pest$FPG < 126 & pest$Diabetes_medication == "0", "No","Yes")
table(pest$Diabetes)
pest$Diabetes = factor(pest$Diabetes, 0:1, labels = c("No","Yes"))
class(pest$Diabetes)

# Lipid disorder
# TC (total cholesterol) >= 190 mg/dl, TG (triglycerides) >= 150 mg/dl, 
# LDL-C ≥ 115 mg/dl, and HDLC < 40 mg/dl for men and < 46 mg/dl for women
# pest$Lipid_disorder = ifelse(pest$TC <190 & pest$TG <150 & pest$LDLC <115 & pest$HDLC >=40 & pest$Lipid_disorder_medication == "0" & pest$Gender == "Male", "No",
#                              ifelse(pest$TC <190 & pest$TG <150 & pest$LDLC <115 & pest$HDLC>=46 & pest$Lipid_disorder_medication == "0" & pest$Gender == "Female", "No",
#                                     "Yes"))
table(pest$Lipid_disorder)
pest$Lipid_disorder = factor(pest$Lipid_disorder, 0:1, labels = c("No","Yes"))
class(pest$Lipid_disorder)

# Metabolic syndrome
# 1) WC ≥ 102 cm for men and ≥ 88 cm for women;
# 2) raised concentration of TG ≥ 150 mg/dl 
# 3) reduced concentration of HDL-C < 40 mg/dl for men and < 50 mg/dl for women 
# 4) SBP was ≥ 130 mmHg, or DBP ≥ 85 mmHg;
# 5) FPG level ≥100 mg/dl

score = ifelse(pest$WC_cat=="High", 1, 0)
score = score + ifelse(pest$TG >=150 | pest$Lipid_disorder_medication == "1", 1, 0)
score = score + ifelse(pest$Lipid_disorder_medication == "1", 1,
                       ifelse(pest$HDLC <40 & pest$Gender == "Male", 1,
                              ifelse(pest$HDLC <50 & pest$Gender == "Female", 1, 0)))
score = score + ifelse(pest$SBP >=130 | pest$DBP >=85  | pest$Hypertension_medication == "1", 1, 0)
score = score + ifelse(pest$FPG >=100 | pest$Diabetes_medication == "1", 1, 0)
table(score)

pest$Metabolic_syndrome = ifelse(score>=3, "Yes", "No")
table(pest$Metabolic_syndrome)
pest$Metabolic_syndrome = factor(pest$Metabolic_syndrome)
class(pest$Metabolic_syndrome)

# Reorder columns
colnames(pest)

pest = cbind(pest[,c(1:30,ncol(pest)-1,ncol(pest))], pest[,c(33:ncol(pest)-2)])

ifelse(dir.exists("../Exports/"), "", dir.create("../Exports/"))
ifelse(dir.exists("../Exports/Data/"), "", dir.create("../Exports/Data/"))
saveRDS(pest, "../Data/PesticidesPCBBDEs.rds")
write.csv(pest, "../Exports/Data/PesticidesPCBBDEs.csv")

### PAHs ----
## Extract exposure matrices
data = read_excel("../Data/Dataset_Pesticides_PAHs_05242022.xlsx", sheet = 2)
str(data)
colnames(data)[38:50] = readLines("../Dictionaries/PAHs_names.txt")
pahs = as.data.frame(data)
rownames(pahs) = pahs$identifiant
dim(pahs)    # 670   50

# Subset relevant columns
pahs = pahs[,-which(colnames(pahs) %in% c("Fasting","LDLC_HDLC",
                                          "MetS_WC","MetS_TG","MetS_HDLC",
                                          "MetS_BP","MetS_FPG"))]

str(pahs)
colnames(pahs)[1] = "ID"
colnames(pahs)[23:25] = c("Diabetes_medication","Hypertension_medication","Lipid_disorder_medication")
colnames(pahs)[29:30] = c("Lipid_disorder","Metabolic_syndrome")

# Set classes for variables
class(pahs$ID)

table(pahs$Country)
pahs$Country = factor(pahs$Country)
class(pahs$Country)

table(pahs$Gender)
pahs$Gender = factor(ifelse(pahs$Gender=="F", "Female", "Male"))
class(pahs$Gender)
pahs$Gender = relevel(pahs$Gender, "Male")

table(pahs$Smoking)
pahs$Smoking = factor(pahs$Smoking, 1:3, labels = c("Never smoker","Former smoker","Current smoker"))

table(pahs$Physical_activity)
pahs$Physical_activity = factor(pahs$Physical_activity, 1:3, labels = c("Inactive","Moderately active","Active"))

table(pahs$Alcohol)
pahs$Alcohol = factor(pahs$Alcohol, 1:3, labels = c("Low","Moderate","High"))

table(pahs$Education)
pahs$Education = factor(pahs$Education, 1:3, labels = c("Low","Intermediate","High"))

str(pahs)

## Exclude participants with >50% nutrient information missing
hist(rowSums(is.na(pest[10:14]))/5)
pahs = pahs[rowSums(is.na(pahs[10:14]))/5 < 0.5,]
hist(rowSums(is.na(pest[6:14]))/9)

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

# Medication
pahs$Diabetes_medication = factor(pahs$Diabetes_medication)
pahs$Hypertension_medication = factor(pahs$Hypertension_medication)
pahs$Lipid_disorder_medication = factor(pahs$Lipid_disorder_medication)

pahs = pahs %>% mutate_at(vars(ends_with("_medication")),
                          function(x) factor(x, 0:1, labels = c("No","Yes")))

## Outcome ascertainment
# Obesity
# pahs$Obesity = ifelse(pahs$BMI <30, "No","Yes")
table(pahs$Obesity)
pahs$Obesity = factor(pahs$Obesity, 0:1, labels = c("No","Yes"))
class(pahs$Obesity)

# Hypertension
summary(pahs$SBP)
summary(pahs$DBP)
# pahs$Hypertension = ifelse(pahs$SysBP < 140 & pahs$DiaBP < 90 & pahs$Hypertension_medication == "0", "No","Yes")
table(pahs$Hypertension)
pahs$Hypertension = factor(pahs$Hypertension, 0:1, labels = c("No","Yes"))
class(pahs$Hypertension)

# Diabetes
summary(pahs$FPG)
# pahs$Diabetes = ifelse(pahs$FPG < 126 & pahs$Diabetes_medication == "0", "No","Yes")
table(pahs$Diabetes)
pahs$Diabetes = factor(pahs$Diabetes, 0:1, labels = c("No","Yes"))
class(pahs$Diabetes)

# Lipid disorder
# TC (total cholesterol) >= 190 mg/dl, TG (triglycerides) >= 150 mg/dl, 
# LDL-C ≥ 115 mg/dl, and HDLC < 40 mg/dl for men and < 46 mg/dl for women
# pahs$Lipid_disorder = ifelse(pahs$TC <190 & pahs$TG <150 & pahs$LDLC <115 & pahs$HDLC >=40 & pahs$Lipid_disorder_medication == "0" & pahs$Gender == "Male", "No",
#                              ifelse(pahs$TC <190 & pahs$TG <150 & pahs$LDLC <115 & pahs$HDLC>=46 & pahs$Lipid_disorder_medication == "0" & pahs$Gender == "Female", "No",
#                                     "Yes"))
table(pahs$Lipid_disorder)
pahs$Lipid_disorder = factor(pahs$Lipid_disorder, 0:1, labels = c("No","Yes"))
class(pahs$Lipid_disorder)

# Metabolic syndrome
# 1) WC ≥ 102 cm for men and ≥ 88 cm for women;
# 2) raised concentration of TG ≥ 150 mg/dl 
# 3) reduced concentration of HDL-C < 40 mg/dl for men and < 50 mg/dl for women 
# 4) SBP was ≥ 130 mmHg, or DBP ≥ 85 mmHg;
# 5) FPG level ≥100 mg/dl

score = ifelse(pahs$WC_cat=="High", 1, 0)
score = score + ifelse(pahs$TG >=150 | pahs$Lipid_disorder_medication == "1", 1, 0)
score = score + ifelse(pahs$Lipid_disorder_medication == "1", 1,
                       ifelse(pahs$HDLC <40 & pahs$Gender == "Male", 1,
                              ifelse(pahs$HDLC <50 & pahs$Gender == "Female", 1, 0)))
score = score + ifelse(pahs$SBP >=130 | pahs$DBP >=85  | pahs$Hypertension_medication == "1", 1, 0)
score = score + ifelse(pahs$FPG >=100 | pahs$Diabetes_medication == "1", 1, 0)
table(score)

pahs$Metabolic_syndrome = ifelse(score>=3, "Yes", "No")
table(pahs$Metabolic_syndrome)
pahs$Metabolic_syndrome = factor(pahs$Metabolic_syndrome)
class(pahs$Metabolic_syndrome)

# Reorder columns
colnames(pahs)

pahs = cbind(pahs[,c(1:30,ncol(pahs)-1,ncol(pahs))], pahs[,c(33:ncol(pahs)-2)])

saveRDS(pahs, "../Data/PAHs.rds")
write.csv(pahs, "../Exports/Data/PAHs.csv")

### Train/Test split----

# Unique participants for training
train_pest = pest[setdiff(pest$ID, pahs$ID),]
train_pahs = pahs[setdiff(pahs$ID, pest$ID),]

# Check if covariate information is consistent
all.equal(pest[(intersect(pest$ID, pahs$ID)),1:32],pahs[(intersect(pest$ID, pahs$ID)),1:32])

# Overlapping participants for validation
test = cbind(pest[(intersect(pest$ID, pahs$ID)),],
             pahs[(intersect(pest$ID, pahs$ID)),33:ncol(pahs)])
colnames(test)
str(test)

# drop levels
train_pest = train_pest %>% mutate_if(is.factor,droplevels)
train_pahs = train_pahs %>% mutate_if(is.factor,droplevels)
test = test %>% mutate_if(is.factor,droplevels)

### Combine all participants (covariate and outcome information)----
outer = rbind(train_pest[,1:32],
              train_pahs[,1:32],
              test[,1:32])
colnames(outer)
str(outer)

outer$data_type = rep(c("train_pest","train_pahs","test"),
                      c(nrow(train_pest),nrow(train_pahs), nrow(test)))
outer$data_type = factor(outer$data_type, levels = c("train_pest","train_pahs","test"))

table(outer$data_type)
table(outer$Country,outer$data_type)
summary(outer)

saveRDS(outer, "../Data/All_participants_info.rds")
write.csv(outer, "../Exports/Data/All_participants_info.csv")



