
### Sumemr Project -- Imputation missing value and recode non detected 
## Created by Vivian on 19 July edited on 20 July

rm(list=ls())
project_path="~/hair/scripts"
setwd(project_path)

library(tidyverse)
library(colorspace)
library(imputeLCMD)


##### Load data
pest <- read.csv("../data/Dataset_PesticidesPCBBDEs.csv")
names(pest) <- pest %>% slice(1) %>% unlist()
pest <- pest %>% slice(-c(1:2))
dim(pest)    # 1096   48
str(pest)
 
pahs <- read.csv("../data/Dataset_PAHs.csv")
names(pahs) <- pahs %>% slice(1) %>% unlist()
pahs <- pahs %>% slice(-c(1:2))
dim(pahs)    # 682   28
str(pahs)

pest_name <- readRDS("../processed/uni_multi_pest.rds")
colnames(pest_name)
pahs_name <- readRDS("../processed/uni_multi_pahs.rds")
colnames(pahs_name)


##### Define function --
### funciton
rtruncnorm = function(n, mean = 0, sd = 1, min = -Inf, max = Inf){
  if (min > max) stop('Error: Truncation range is empty')
  x = runif(n, pnorm(min, mean, sd), pnorm(max, mean, sd))
  qnorm(x, mean, sd)
}


##### Proportion of NAs per chemical compounds --
pest <- pest[complete.cases(pest[ , 6:15]),]
pest <- pest[, 16:48]
colnames(pest) <- colnames(pest_name)[17:49]
pest_NA_prop = apply(pest, 2, function(x) sum(is.na(x))/nrow(pest))

pahs <- pahs[complete.cases(pahs[ , 6:15]),]
pahs <- pahs[, 16:28]
colnames(pahs) <- colnames(pahs_name)[17:29]
pahs_NA_prop = apply(pahs, 2, function(x) sum(is.na(x))/nrow(pahs)) %>% sort()


##### Proportion of detected per chemical compounds --
### pest
pest_nd_prop <- apply(pest, 2, function(x) sum(x=="nd", na.rm = TRUE)/nrow(pest)) %>% sort()
#pest_nd_prop_1 <- data.frame(unlist(pest_nd_prop))


##### Recode variables --
### chemicals detected less than 50 %
pest_id_l50 <- names(which(pest_nd_prop >= 0.5))
pest_id_h80 <- names(which(pest_nd_prop < 0.2))
pest_id_l80 <- names(which(pest_nd_prop >= 0.2))
pest_l50 <- pest[,which(colnames(pest) %in% pest_id_l50)]    # 15
pest_h80 <- pest[,which(colnames(pest) %in% pest_id_h80)]    # 13
pest_l80 <- pest[,which(colnames(pest) %in% pest_id_l80)]    # 20

### replace nd temporarily
pest_tmp1 = NULL
for (i in 1:33) {
  tmp <- pest[,i]
  tmp1 <- as.numeric(as.character(tmp))
  pest_tmp1 <- cbind(pest_tmp1, tmp1)
}
pest_tmp1 <- data.frame(pest_tmp1)
colnames(pest_tmp1) <- colnames(pest)

LOD <- apply(pest_tmp1, 2, function(x) min(x, na.rm = T))  ### set up LOD from data

pest_tmp2  <- data.frame(lapply(pest, as.character), stringsAsFactors=FALSE)

for(k in 1:ncol(pest_tmp2)){
  set.seed(150621)
  pest_tmp2[pest_tmp2[,k]=="nd"&!is.na(pest_tmp2[,k]),k] = rtruncnorm(sum(pest_tmp2[,k]=="nd"&!is.na(pest_tmp2[,k])), min=0, max=LOD[k]) 
}

pest_tmp2  = apply(pest_tmp2 , 2, as.numeric) %>% data.frame()
colnames(pest_tmp2) <- colnames(pest)

### run imputation 
pest_tmp2_log10 = log10(pest_tmp2)
set.seed(7)
pest_tmp2_imp=impute.QRILC(t(pest_tmp2_log10))
pest_tmp2_imp <- data.frame(t(pest_tmp2_imp[[1]]))
colnames(pest_tmp2_imp) = colnames(pest)
str(pest_tmp2_imp)

### recode as categorical
# replace nd back 
pest_imp <- as.vector(unlist(pest_tmp2_imp))
pest_imp[which(pest == "nd")] <- "nd"
pest_imp <- as.data.frame(matrix(pest_imp, nrow = 1065))
colnames(pest_imp) <- colnames(pest)

# less than 50% / 50% -80% of detection and recode as binary
pest_imp_l80 <- pest_imp[, which(colnames(pest_imp) %in% pest_id_l80)]
pest_imp_l80 <- data.frame(ifelse(pest_imp_l80 == "nd", 0, 1))
pest_imp_l80 <- data.frame(lapply(pest_imp_l80, factor))
colnames(pest_imp_l80) <- colnames(pest_l80)
pest_imp_l50 <- pest_imp_l80[, which(colnames(pest_imp_l80) %in% pest_id_l50)]
pest_imp_5080 <- pest_imp_l80[, which(!(colnames(pest_imp_l80) %in% pest_id_l50))]

# continuous imputed over 80% / 50%-80%
pest_imp_h80_con <- pest_tmp2_imp[,which(colnames(pest_tmp2_imp) %in% pest_id_h80)]
pest_imp_5080_con <- pest_tmp2_imp[,which(!(colnames(pest_tmp2_imp) %in% pest_id_h80) & !(colnames(pest_tmp2_imp) %in% pest_id_l50))]

# replace nd more than 80% of detection and recode as categorical
# ND-25th: 1; 25-50th: 2; 50-75th: 3; and > 75th: 4
pest_imp_h80 <- pest_imp[, which(colnames(pest_imp) %in% pest_id_h80)]
str(pest_imp_h80)

pest_tmp_h80 = NULL
for (i in 1:13) {
  tmp <- pest_imp_h80[,i]
  tmp1 <- as.numeric(as.character(tmp))
  pest_tmp_h80 <- cbind(pest_tmp_h80, tmp1)
}
pest_tmp_h80 <- data.frame(pest_tmp_h80)
colnames(pest_tmp_h80) <- colnames(pest_h80)

quan <- apply(pest_tmp_h80 , 2, function(x) quantile(x, na.rm = T))
tmp_h80 = NULL
for (i in 1:13) {
  tmp <- pest_tmp_h80[,i]
  tmp2 <- ifelse(is.na(tmp) , "1", 
                 ifelse(tmp<quan[2,i], "1",
                        ifelse(tmp<quan[3,i] & tmp>=quan[2,i], "2",
                               ifelse(tmp<quan[4,i] & tmp>=quan[3,i], "3", "4"))))
  tmp_h80 <- cbind(tmp_h80, tmp2)
}
tmp_h80 <- data.frame(tmp_h80)
colnames(tmp_h80) <- colnames(pest_h80)
str(tmp_h80)

### inspect
prop.table(table(pest_imp_5080$Diflufenican))
prop.table(table(tmp_h80$Chlorpyrifos))

### save
saveRDS(pest_imp_l50, "../processed/cat_imputation/pest_imp_l50.rds")
saveRDS(pest_imp_5080, "../processed/cat_imputation/pest_imp_5080.rds")
saveRDS(tmp_h80, "../processed/cat_imputation/pest_imp_h80.rds")
saveRDS(pest_imp_h80_con, "../processed/cat_imputation/pest_imp_h80_cont.rds")
saveRDS(pest_imp_5080_con, "../processed/cat_imputation/pest_imp_5080_cont.rds")



pahs_nd_prop = apply(pahs, 2, function(x) sum(x=="nd", na.rm = TRUE)/nrow(pahs)) %>% sort()
#pahs_nd_prop_1 <- data.frame(unlist(pahs_nd_prop))

##### Recode variables --
### chemicals detected less than 50 %
pahs_id_l50 <- names(which(pahs_nd_prop >= 0.5))
pahs_id_h80 <- names(which(pahs_nd_prop < 0.2))
pahs_id_l80 <- names(which(pahs_nd_prop >= 0.2))
pahs_l50 <- pahs[,which(colnames(pahs) %in% pahs_id_l50)]    # 8
pahs_h80 <- pahs[,which(colnames(pahs) %in% pahs_id_h80)]    # 3
pahs_l80 <- pahs[,which(colnames(pahs) %in% pahs_id_l80)]    # 10

### replace nd temporarily
pahs_tmp1 = NULL
for (i in 1:13) {
  tmp <- pahs[,i]
  tmp1 <- as.numeric(as.character(tmp))
  pahs_tmp1 <- cbind(pahs_tmp1, tmp1)
}
pahs_tmp1 <- data.frame(pahs_tmp1)
colnames(pahs_tmp1) <- colnames(pahs)

LOD_pahs <- apply(pahs_tmp1, 2, function(x) min(x, na.rm = T))

pahs_tmp2  <- data.frame(lapply(pahs, as.character), stringsAsFactors=FALSE)

for(k in 1:ncol(pahs_tmp2)){
  set.seed(150621)
  pahs_tmp2[pahs_tmp2[,k]=="nd"&!is.na(pahs_tmp2[,k]),k] = rtruncnorm(sum(pahs_tmp2[,k]=="nd"&!is.na(pahs_tmp2[,k])), min=0, max=LOD[k]) 
}

pahs_tmp2  = apply(pahs_tmp2 , 2, as.numeric) %>% data.frame()
colnames(pahs_tmp2) <- colnames(pahs)

### run imputation 
pahs_tmp2_log10 = log10(pahs_tmp2)
set.seed(7)
pahs_tmp2_imp=impute.QRILC(t(pahs_tmp2_log10))
pahs_tmp2_imp <- data.frame(t(pahs_tmp2_imp[[1]]))
colnames(pahs_tmp2_imp) = colnames(pahs)
str(pahs_tmp2_imp)

### recode as categorical
# replace nd back 
pahs_imp <- as.vector(unlist(pahs_tmp2_imp))
pahs_imp[which(pahs == "nd")] <- "nd"
pahs_imp <- as.data.frame(matrix(pahs_imp, nrow = 670))
colnames(pahs_imp) <- colnames(pahs)

# less than 50% / 50% -80% of detection and recode as binary
pahs_imp_l80 <- pahs_imp[, which(colnames(pahs_imp) %in% pahs_id_l80)]
pahs_imp_l80 <- data.frame(ifelse(pahs_imp_l80 == "nd", 0, 1))
pahs_imp_l80 <- data.frame(lapply(pahs_imp_l80, factor))
colnames(pahs_imp_l80) <- colnames(pahs_l80)
pahs_imp_l50 <- pahs_imp_l80[, which(colnames(pahs_imp_l80) %in% pahs_id_l50)]
pahs_imp_5080 <- pahs_imp_l80[, which(!(colnames(pahs_imp_l80) %in% pahs_id_l50))]

# continuous imputed over 80% / 50%-80%
pahs_imp_h80_con <- pahs_tmp2_imp[,which(colnames(pahs_tmp2_imp) %in% pahs_id_h80)]
pahs_imp_5080_con <- pahs_tmp2_imp[,which(!(colnames(pahs_tmp2_imp) %in% pahs_id_h80) & !(colnames(pahs_tmp2_imp) %in% pahs_id_l50))]

# replace nd more than 80% of detection and recode as categorical
# ND-25th: 1; 25-50th: 2; 50-75th: 3; and > 75th: 4
pahs_imp_h80 <- pahs_imp[, which(colnames(pahs_imp) %in% pahs_id_h80)]
str(pahs_imp_h80)

pahs_tmp_h80 = NULL
for (i in 1:3) {
  tmp <- pahs_imp_h80[,i]
  tmp1 <- as.numeric(as.character(tmp))
  pahs_tmp_h80 <- cbind(pahs_tmp_h80, tmp1)
}
pahs_tmp_h80 <- data.frame(pahs_tmp_h80)
colnames(pahs_tmp_h80) <- colnames(pahs_h80)

quan <- apply(pahs_tmp_h80 , 2, function(x) quantile(x, na.rm = T))
tmp_h80_1 = NULL
for (i in 1:3) {
  tmp <- pahs_tmp_h80[,i]
  tmp2 <- ifelse(is.na(tmp) , "1", 
                 ifelse(tmp<quan[2,i], "1",
                        ifelse(tmp<quan[3,i] & tmp>=quan[2,i], "2",
                               ifelse(tmp<quan[4,i] & tmp>=quan[3,i], "3", "4"))))
  tmp_h80_1 <- cbind(tmp_h80_1, tmp2)
}
tmp_h80_1 <- data.frame(tmp_h80_1)
colnames(tmp_h80_1) <- colnames(pahs_h80)
str(tmp_h80_1)


### inspect
prop.table(table(pahs_imp_5080$Cotinine))
prop.table(table(pahs_imp_l50$`1-OH-Phenanthrene`))
prop.table(table(tmp_h80_1$Nicotine))

### save
saveRDS(pahs_imp_l50, "../processed/cat_imputation/pahs_imp_l50.rds")
saveRDS(pahs_imp_5080, "../processed/cat_imputation/pahs_imp_5080.rds")
saveRDS(tmp_h80_1, "../processed/cat_imputation/pahs_imp_h80.rds")
saveRDS(pahs_imp_h80_con, "../processed/cat_imputation/pahs_imp_h80_cont.rds")
saveRDS(pahs_imp_5080_con, "../processed/cat_imputation/pahs_imp_5080_cont.rds")



