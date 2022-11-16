## Data cleaning

rm(list=ls())
project_path=dirname(rstudioapi::getActiveDocumentContext()$path)
setwd(project_path)

source("functions.R")
library(tidyverse)
library(readxl)

ifelse(dir.exists("../Processed/"), "", dir.create("../Processed/"))
ifelse(dir.exists("../Figures/"), "", dir.create("../Figures/"))
ifelse(dir.exists("../Figures/Supplementary/"), "", dir.create("../Figures/Supplementary/"))

## Data cleaning ----
filepaths = c("PesticidesPCBBDEs","PAHs")
m = 1
data = read_excel("../Data/Dataset_Pesticides_PAHs_05242022.xlsx", sheet = m)
str(data)
dim(data)
colnames(data)[38:ncol(data)] = readLines(paste0("../Dictionaries/",filepaths[m],"_names.txt"))
newdata = as.data.frame(data)
rownames(newdata) = newdata$identifiant
dim(newdata)
colnames(newdata)

# Check if all participants are Fasting
newdata %>% filter(Fasting==0) %>% nrow
# Check for participants missing >50% diet information
remove = newdata %>% select(Energy, Proteins, Fats, Carbohydrates, Fibers) %>%
  .[rowSums(is.na(.))/ncol(.) > 0.5,] %>% rownames
print(paste0("Removing ",length(remove)," participants missing >50% diet information"))
newdata = newdata[-which(rownames(newdata) %in% remove),]

# Check for participants missing >50% covariate information
newdata %>% select(Age, Gender, Smoking,
                   Energy, Proteins, Fats, Carbohydrates, Fibers,
                   Education, Physical_activity, Alcohol) %>%
  .[rowSums(is.na(.))/ncol(.) > 0.5,] %>% nrow

# Rename columns
newdata = newdata %>% rename(ID = "identifiant",
                             Centre = "Country",
                             Diabetes_medication = "Diabetic_MD",
                             Hypertension_medication = "Antihypertensive_MD",
                             Lipid_medication = "Lipid_MD")

newdata = newdata %>% mutate_at(vars(ends_with("_medication")),
                                function(x) factor(x, 0:1, labels = c("No","Yes")))

# Covariate transformation
table(newdata$Centre)
newdata$Centre = factor(newdata$Centre)

table(newdata$Gender)
newdata$Gender = factor(ifelse(newdata$Gender=="F", "Female", "Male"))
newdata$Gender = relevel(newdata$Gender, "Male") # Male as reference

table(newdata$Smoking)
newdata$Smoking = factor(newdata$Smoking, 1:3, labels = c("Never smoker","Former smoker","Current smoker"))

table(newdata$Physical_activity)
newdata$Physical_activity = factor(newdata$Physical_activity, 1:3, labels = c("Inactive","Moderately active","Active"))

table(newdata$Alcohol)
newdata$Alcohol = factor(newdata$Alcohol, 1:3, labels = c("Low","Moderate","High"))

table(newdata$Education)
newdata$Education = factor(newdata$Education, 1:3, labels = c("Low","Intermediate","High"))

# Outcome ascertainment
# Categorise BMI
newdata$BMI_cat = ifelse(newdata$BMI <18.5,"Underweight",
                         ifelse(newdata$BMI >=18.5 & newdata$BMI <25, "Normal",
                                ifelse(newdata$BMI >=25 & newdata$BMI <30, "Pre-obesity",
                                       ifelse(newdata$BMI >=30 & newdata$BMI <35, "Obesity class I",
                                              ifelse(newdata$BMI >=35 & newdata$BMI <40, "Obesity class II",
                                                     "Obesity class III")))))
newdata$BMI_cat = factor(newdata$BMI_cat,
                         levels = c("Underweight","Normal", "Pre-obesity",
                                    "Obesity class I","Obesity class II",
                                    "Obesity class III"))

# Dichotomous WC
newdata = newdata %>% mutate(WC_cat =  factor(ifelse(newdata$WC >=102 & newdata$Gender == "Male", 1,
                                                     ifelse(newdata$WC >=88 & newdata$Gender == "Female", 1, 0)),
                                              labels = c("Normal","High")))

# Dichotomous TG
newdata = newdata %>% mutate(TG_cat = factor(ifelse(newdata$TG >= 150, 1, 0), labels = c("Normal","High")))
newdata$TG_cat = factor(newdata$TG_cat)

# Subset relevant columns
expo = colnames(data)[38:ncol(data)]
expo = newdata %>% select(all_of(expo))

covar = newdata %>% select(Centre, Age, Education, Gender, Smoking,
                           Energy, Proteins, Fats, Carbohydrates, Fibers,
                           Physical_activity, Alcohol)
trait = newdata %>% select(BMI, WC, TG, TC, LDLC, HDLC, SBP, DBP, FPG,
                              BMI_cat, WC_cat, TG_cat, ends_with("_medication"),
                              Obesity, Dyslipidemia, Hypertension, Diabetes)
all(rownames(expo) == rownames(covar))
all(rownames(covar) == rownames(trait))

ifelse(dir.exists("../Exports/"), "", dir.create("../Exports/"))
ifelse(dir.exists("../Exports/Data/"), "", dir.create("../Exports/Data/"))
saveRDS(expo, paste0("../Data/",filepaths[m],"_expo.rds"))
write.csv(expo, paste0("../Exports/Data/",filepaths[m],"_expo.csv"))
saveRDS(covar, paste0("../Data/",filepaths[m],"_covar.rds"))
write.csv(covar, paste0("../Exports/Data/",filepaths[m],"_covar.csv"))
saveRDS(trait, paste0("../Data/",filepaths[m],"_trait.rds"))
write.csv(trait, paste0("../Exports/Data/",filepaths[m],"_trait.csv"))

### Proportion of NAs per chemical compounds ----
naprop = apply(expo, 2, function(x) sum(is.na(x))/nrow(expo))
prop = sort(naprop, decreasing = TRUE)

pdf(paste0("../Figures/Supplementary/Missing_prop_",filepaths[m],".pdf"), width = 9, height = 4.75)
par(mar=c(9,5,2,1))
plot(prop, type = "h", ylab="Proportion missing", xlab="", xaxt = "n", lwd=1, ylim = c(0,1),
     col=ifelse(prop>=0.5, yes="tomato", no="black"), cex.lab=1.5)
abline(h=0.5, lty=2, col="darkred", lwd = 1)
for (i in 1:length(prop)){
  axis(side=1, at=i, labels=names(prop)[i], las=2,
       col=ifelse(prop[i]>=0.5, yes="tomato", no="black"),
       col.axis=ifelse(prop[i]>=0.5, yes="tomato", no="black"))
}
dev.off()

### Proportion of NAs per participant ----
summary(apply(expo, 1, function(x) sum(is.na(x))/ncol(expo)))

### Proportion of detected per chemical compounds ----
ndprop = apply(expo, 2, function(x) sum(x=="nd", na.rm = TRUE)/nrow(expo))
prop =sort(1-(ndprop+naprop), decreasing = TRUE)

pdf(paste0("../Figures/Supplementary/Detection_prop_",filepaths[m],".pdf"), width = 9, height = 4.75)
par(mar=c(9,5,2,1))
plot(prop, type = "h", ylab="Proportion detected", xlab="", xaxt = "n", lwd=1, ylim = c(0,1),
     col=ifelse(prop<=0.1, yes="tomato", no="black"), cex.lab=1.5)
abline(h=0.1, lty=2, col="darkred", lwd = 1)
for (i in 1:length(prop)){
  axis(side=1, at=i, labels=names(prop)[i], las=2,
       col=ifelse(prop[i]<=0.1, yes="tomato", no="black"),
       col.axis=ifelse(prop[i]<=0.1, yes="tomato", no="black"))
}
dev.off()

### Proportion detected per participant ----
summary(1-(apply(expo, 1, function(x) sum(x=="nd", na.rm = TRUE)/ncol(expo))+
             apply(expo, 1, function(x) sum(is.na(x))/ncol(expo))))

### Recoding nd ----
# Extract LOD
lod = apply(expo, 2,function(x) suppressWarnings(min(as.numeric(unlist(x)), na.rm = T)))

# Save exposure information
info = data.frame(lod = lod, ndprop = ndprop, naprop = naprop,row.names = colnames(expo))

saveRDS(info, paste0("../Processed/",filepaths[m],"_info.rds"))

### Replace nd with random values from 0 to minimum detection (Gaussian)
for(k in 1:ncol(expo)){
  expo[which(expo[,k]=="nd"),k] = rtruncnorm(sum(expo[,k]=="nd", na.rm = T),min=0,max=lod[k]) 
}
expo_ndimp = apply(expo, 2, as.numeric)
rownames(expo_ndimp) = rownames(expo)
summary(expo_ndimp)

saveRDS(expo_ndimp, paste0("../Processed/",filepaths[m],"_expo_ndimp.rds"))
write.csv(expo_ndimp, paste0("../Exports/Data/",filepaths[m],"_expo_ndimp.csv"))

