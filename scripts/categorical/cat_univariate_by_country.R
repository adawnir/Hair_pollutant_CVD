
### Sumemr Project -- univariate analysis by country considering categorical variables
## Created by Vivian on 22 July 

rm(list=ls())
project_path="~/hair/scripts"
setwd(project_path)

library(tidyverse)
library(parallel)

##### Load data--
pest <- readRDS("../processed/uni_multi_pest.rds")
pahs <- readRDS("../processed/uni_multi_pahs.rds")

pest_cat_l50 <- readRDS("../processed/cat_imputation/pest_imp_l50.rds")
pest_cat_5080 <- readRDS("../processed/cat_imputation/pest_imp_5080.rds")
pest_cat_h80 <- readRDS("../processed/cat_imputation/pest_imp_h80.rds")
pest_5080 <- readRDS("../processed/cat_imputation/pest_imp_5080_cont.rds")
pest_h80 <- readRDS("../processed/cat_imputation/pest_imp_h80_cont.rds")

pahs_cat_l50 <- readRDS("../processed/cat_imputation/pahs_imp_l50.rds")
pahs_cat_5080 <- readRDS("../processed/cat_imputation/pahs_imp_5080.rds")
pahs_cat_h80 <- readRDS("../processed/cat_imputation/pahs_imp_h80.rds")
pahs_5080 <- readRDS("../processed/cat_imputation/pahs_imp_5080_cont.rds")
pahs_h80 <- readRDS("../processed/cat_imputation/pahs_imp_h80_cont.rds")



##### Scaling --
### Scale continuous variables
pest_5080 <- scale(pest_5080)
summary(pest_5080) # check mean = 0  

pest_h80 <- scale(pest_h80)
summary(pest_h80) # check mean = 0  

pahs_5080 <- scale(pahs_5080)
summary(pahs_5080) # check mean = 0  

pahs_h80 <- scale(pahs_h80)
summary(pahs_h80) # check mean = 0  

##### Combine data --
colnames(pest)
pest_cat <- cbind(pest[ ,c("Gender", "Age", "Country", "Obesity", "MS", "hypertension", "Lipid_disorder", "diabetes")], 
                  pest_cat_l50, pest_cat_5080, pest_cat_h80)
str(pest_cat)
levels(pest_cat$PCB_138)

pest_cont <- cbind(pest[ ,c("Gender", "Age", "Country", "Obesity", "MS", "hypertension", "Lipid_disorder", "diabetes")], 
                   pest_5080, pest_h80)
str(pest_cont)

pahs_cat <- cbind(pahs[ ,c("Gender", "Age", "Country", "Obesity", "MS", "hypertension", "Lipid_disorder", "diabetes")], 
                  pahs_cat_l50, pahs_cat_5080, pahs_cat_h80)
str(pahs_cat)
levels(pahs_cat$Two_OH_Fluorene)

pahs_cont <- cbind(pahs[ ,c("Gender", "Age", "Country", "Obesity", "MS", "hypertension", "Lipid_disorder", "diabetes")], 
                   pahs_5080, pahs_h80)
str(pahs_cont)

saveRDS(pest_cat, "../cat_results/pest_cat.rds")
saveRDS(pahs_cat, "../cat_results/pahs_cat.rds")
saveRDS(pest_cont, "../cat_results/pest_cont.rds")
saveRDS(pahs_cat, "../cat_results/pahs_cont.rds")


##### Divide by country: cat --
### pest
pest_cat_l <- pest_cat[which(pest_cat$Country == "Luxembourg"),]   #487
pest_cat_b <- pest_cat[which(pest_cat$Country == "Belgium"),]    #503
pest_cat_f <- pest_cat[which(pest_cat$Country == "France"),]     #75

saveRDS(pest_cat_l, "../cat_results/pest_cat_l.rds")
saveRDS(pest_cat_b, "../cat_results/pest_cat_b.rds")
saveRDS(pest_cat_f, "../cat_results/pest_cat_f.rds")

pest_cont_l <- pest_cont[which(pest_cont$Country == "Luxembourg"),]   #487
pest_cont_b <- pest_cont[which(pest_cont$Country == "Belgium"),]    #503
pest_cont_f <- pest_cont[which(pest_cont$Country == "France"),]     #75


saveRDS(pest_cont_l, "../cat_results/pest_cont_l.rds")
saveRDS(pest_cont_b, "../cat_results/pest_cont_b.rds")
saveRDS(pest_cont_f, "../cat_results/pest_cont_f.rds")

### pahs
pahs_cat_l <- pahs_cat[which(pahs_cat$Country == "Luxembourg"),]   #612
pahs_cat_b <- pahs_cat[which(pahs_cat$Country == "Belgium"),]    #58

saveRDS(pahs_cat_l, "../cat_results/pahs_cat_l.rds")
saveRDS(pahs_cat_b, "../cat_results/pahs_cat_b.rds")

pahs_cont_l <- pahs_cont[which(pahs_cont$Country == "Luxembourg"),]   #612
pahs_cont_b <- pahs_cont[which(pahs_cont$Country == "Belgium"),]    #58

saveRDS(pahs_cont_l, "../cat_results/pahs_cont_l.rds")
saveRDS(pahs_cont_b, "../cat_results/pahs_cont_b.rds")





##### Logistic regression----
### Get beta coefficients, standard errors, glm p-values and anova p-values
# adjusted for country
foo = function(i, dat, outcome = NULL){
  var_name= colnames(dat)[i]
  if (outcome %in% c("Obesity", "MS", "hypertension", "Lipid_disorder", "diabetes")){
    print(var_name)
    print(outcome)
    confound=c("Age", "Gender", "Country")  # Change confounders accordingly
    model = glm(as.formula(paste(outcome, paste(c(var_name,confound),collapse="+"),sep="~")),
                data = dat, family="binomial")
    model0 = glm(as.formula(paste(outcome, paste(confound,collapse="+"),sep="~")),
                 data = dat[complete.cases(dat[,i]),], family="binomial")
    summary_table=summary(model)$coefficients
    res = list(summary_table[grepl(paste0("^",var_name),rownames(summary_table)),-3],
               anova(model0, model, test = 'Chisq')$`Pr(>Chi)`[2])
    names(res) = c('glm_output', 'anova_output')
    return(res)
  }
}

# separete model adjusted for age and sex only 
foo1 = function(i, dat, outcome = NULL){
  var_name= colnames(dat)[i]
  if (outcome %in% c("Obesity", "MS", "hypertension", "Lipid_disorder", "diabetes")){
    print(var_name)
    print(outcome)
    confound=c("Age", "Gender")  # Change confounders accordingly
    model = glm(as.formula(paste(outcome, paste(c(var_name,confound),collapse="+"),sep="~")),
                data = dat, family="binomial")
    model0 = glm(as.formula(paste(outcome, paste(confound,collapse="+"),sep="~")),
                 data = dat[complete.cases(dat[,i]),], family="binomial")
    summary_table=summary(model)$coefficients
    res = list(summary_table[grepl(paste0("^",var_name),rownames(summary_table)),-3],
               anova(model0, model, test = 'Chisq')$`Pr(>Chi)`[2])
    names(res) = c('glm_output', 'anova_output')
    return(res)
  }
}

### Run models
pest_cat_l_ob <- lapply(9:41, foo1, dat=pest_cat_l, outcome = "Obesity")
pest_cat_l_ld <- lapply(9:41, foo1, dat=pest_cat_l, outcome = "Lipid_disorder")
pest_cat_l_ms <- lapply(9:41, foo1, dat=pest_cat_l, outcome = "MS")
pest_cat_l_hyper <- lapply(9:41, foo1, dat=pest_cat_l, outcome = "hypertension")
pest_cat_l_dia <- lapply(9:41, foo1, dat=pest_cat_l, outcome = "diabetes")

pest_cat_dia[[1]]
pahs_ld[[1]]

saveRDS(pest_cat_l_ob, "../cat_results/univ/univ_pest_cat_l_ob.rds")
saveRDS(pest_cat_l_ld, "../cat_results/univ/univ_pest_cat_l_ld.rds")
saveRDS(pest_cat_l_ms, "../cat_results/univ/univ_pest_cat_l_ms.rds")
saveRDS(pest_cat_l_hyper, "../cat_results/univ/univ_pest_cat_l_hyper.rds")
saveRDS(pest_cat_l_dia, "../cat_results/univ/univ_pest_cat_l_dia.rds")


pest_cat_b_ob <- lapply(9:41, foo1, dat=pest_cat_b, outcome = "Obesity")
pest_cat_b_ld <- lapply(9:41, foo1, dat=pest_cat_b, outcome = "Lipid_disorder")
pest_cat_b_ms <- lapply(9:41, foo1, dat=pest_cat_b, outcome = "MS")
pest_cat_b_hyper <- lapply(9:41, foo1, dat=pest_cat_b, outcome = "hypertension")
pest_cat_b_dia <- lapply(9:41, foo1, dat=pest_cat_b, outcome = "diabetes")

pest_cat_dia[[1]]

saveRDS(pest_cat_b_ob, "../cat_results/univ/univ_pest_cat_b_ob.rds")
saveRDS(pest_cat_b_ld, "../cat_results/univ/univ_pest_cat_b_ld.rds")
saveRDS(pest_cat_b_ms, "../cat_results/univ/univ_pest_cat_b_ms.rds")
saveRDS(pest_cat_b_hyper, "../cat_results/univ/univ_pest_cat_b_hyper.rds")
saveRDS(pest_cat_b_dia, "../cat_results/univ/univ_pest_cat_b_dia.rds")


pest_cat_f_ob <- lapply(c(9:19, 21:41), foo1, dat=pest_cat_f, outcome = "Obesity")
pest_cat_f_ld <- lapply(c(9:19, 21:41), foo1, dat=pest_cat_f, outcome = "Lipid_disorder")
pest_cat_f_ms <- lapply(c(9:19, 21:41), foo1, dat=pest_cat_f, outcome = "MS")
pest_cat_f_hyper <- lapply(c(9:19, 21:41), foo1, dat=pest_cat_f, outcome = "hypertension")
pest_cat_f_dia <- lapply(c(9:19, 21:41), foo1, dat=pest_cat_f, outcome = "diabetes")

pest_cat_dia[[1]]

saveRDS(pest_cat_f_ob, "../cat_results/univ/univ_pest_cat_f_ob.rds")
saveRDS(pest_cat_f_ld, "../cat_results/univ/univ_pest_cat_f_ld.rds")
saveRDS(pest_cat_f_ms, "../cat_results/univ/univ_pest_cat_f_ms.rds")
saveRDS(pest_cat_f_hyper, "../cat_results/univ/univ_pest_cat_f_hyper.rds")
saveRDS(pest_cat_f_dia, "../cat_results/univ/univ_pest_cat_f_dia.rds")


###PAHs
pahs_cat_l_ob <- lapply(9:21, foo1, dat=pahs_cat_l, outcome = "Obesity")
pahs_cat_l_ld <- lapply(9:21, foo1, dat=pahs_cat_l, outcome = "Lipid_disorder")
pahs_cat_l_ms <- lapply(9:21, foo1, dat=pahs_cat_l, outcome = "MS")
pahs_cat_l_hyper <- lapply(9:21, foo1, dat=pahs_cat_l, outcome = "hypertension")
pahs_cat_l_dia <- lapply(9:21, foo1, dat=pahs_cat_l, outcome = "diabetes")

pahs_cat_ld[[10]]

saveRDS(pahs_cat_l_ob, "../cat_results/univ/univ_pahs_cat_l_ob.rds")
saveRDS(pahs_cat_l_ld, "../cat_results/univ/univ_pahs_cat_l_ld.rds")
saveRDS(pahs_cat_l_ms, "../cat_results/univ/univ_pahs_cat_l_ms.rds")
saveRDS(pahs_cat_l_hyper, "../cat_results/univ/univ_pahs_cat_l_hyper.rds")
saveRDS(pahs_cat_l_dia, "../cat_results/univ/univ_pahs_cat_l_dia.rds")


pahs_cat_b_ob <- lapply(c(10:11, 13, 15:16, 18:21), foo1, dat=pahs_cat_b, outcome = "Obesity")
pahs_cat_b_ld <- lapply(c(10:11, 13, 15:16, 18:21), foo1, dat=pahs_cat_b, outcome = "Lipid_disorder")
pahs_cat_b_ms <- lapply(c(10:11, 13, 15:16, 18:21), foo1, dat=pahs_cat_b, outcome = "MS")
pahs_cat_b_hyper <- lapply(c(10:11, 13, 15:16, 18:21), foo1, dat=pahs_cat_b, outcome = "hypertension")
pahs_cat_b_dia <- lapply(c(10:11, 13, 15:16, 18:21), foo1, dat=pahs_cat_b, outcome = "diabetes")

pahs_cat_ld[[10]]

saveRDS(pahs_cat_b_ob, "../cat_results/univ/univ_pahs_cat_b_ob.rds")
saveRDS(pahs_cat_b_ld, "../cat_results/univ/univ_pahs_cat_b_ld.rds")
saveRDS(pahs_cat_b_ms, "../cat_results/univ/univ_pahs_cat_b_ms.rds")
saveRDS(pahs_cat_b_hyper, "../cat_results/univ/univ_pahs_cat_b_hyper.rds")
saveRDS(pahs_cat_b_dia, "../cat_results/univ/univ_pahs_cat_b_dia.rds")


pahs_cont_l[9:13] <- scale(pahs_cont_l[9:13])
pahs_l_ob <- lapply(9:13, foo1, dat=pahs_cont_l, outcome = "Obesity")
pahs_l_ld <- lapply(9:13, foo1, dat=pahs_cont_l, outcome = "Lipid_disorder")
pahs_l_ms <- lapply(9:13, foo1, dat=pahs_cont_l, outcome = "MS")
pahs_l_hyper <- lapply(9:13, foo1, dat=pahs_cont_l, outcome = "hypertension")
pahs_l_dia <- lapply(9:13, foo1, dat=pahs_cont_l, outcome = "diabetes")

pahs_ld[[3]]

saveRDS(pahs_l_ob, "../cat_results/univ/univ_pahs_l_ob.rds")
saveRDS(pahs_l_ld, "../cat_results/univ/univ_pahs_l_ld.rds")
saveRDS(pahs_l_ms, "../cat_results/univ/univ_pahs_l_ms.rds")
saveRDS(pahs_l_hyper, "../cat_results/univ/univ_pahs_l_hyper.rds")
saveRDS(pahs_l_dia, "../cat_results/univ/univ_pahs_l_dia.rds")

pahs_cont_b[9:13] <- scale(pahs_cont_b[9:13])
pahs_b_ob <- lapply(9:13, foo1, dat=pahs_cont_b, outcome = "Obesity")
pahs_b_ld <- lapply(9:13, foo1, dat=pahs_cont_b, outcome = "Lipid_disorder")
pahs_b_ms <- lapply(9:13, foo1, dat=pahs_cont_b, outcome = "MS")
pahs_b_hyper <- lapply(9:13, foo1, dat=pahs_cont_b, outcome = "hypertension")
pahs_b_dia <- lapply(9:13, foo1, dat=pahs_cont_b, outcome = "diabetes")

pahs_ld[[3]]

saveRDS(pahs_b_ob, "../cat_results/univ/univ_pahs_b_ob.rds")
saveRDS(pahs_b_ld, "../cat_results/univ/univ_pahs_b_ld.rds")
saveRDS(pahs_b_ms, "../cat_results/univ/univ_pahs_b_ms.rds")
saveRDS(pahs_b_hyper, "../cat_results/univ/univ_pahs_b_hyper.rds")
saveRDS(pahs_b_dia, "../cat_results/univ/univ_pahs_b_dia.rds")


pest_cont_l[9:26] <- scale(pest_cont_l[9:26])
pest_l_ob <- lapply(9:26, foo1, dat=pest_cont_l, outcome = "Obesity")
pest_l_ld <- lapply(9:26, foo1, dat=pest_cont_l, outcome = "Lipid_disorder")
pest_l_ms <- lapply(9:26, foo1, dat=pest_cont_l, outcome = "MS")
pest_l_hyper <- lapply(9:26, foo1, dat=pest_cont_l, outcome = "hypertension")
pest_l_dia <- lapply(9:26, foo1, dat=pest_cont_l, outcome = "diabetes")

pest_ld[[3]]

saveRDS(pest_l_ob, "../cat_results/univ/univ_pest_l_ob.rds")
saveRDS(pest_l_ld, "../cat_results/univ/univ_pest_l_ld.rds")
saveRDS(pest_l_ms, "../cat_results/univ/univ_pest_l_ms.rds")
saveRDS(pest_l_hyper, "../cat_results/univ/univ_pest_l_hyper.rds")
saveRDS(pest_l_dia, "../cat_results/univ/univ_pest_l_dia.rds")

pest_cont_b[9:26] <- scale(pest_cont_b[9:26])
pest_b_ob <- lapply(9:26, foo1, dat=pest_cont_b, outcome = "Obesity")
pest_b_ld <- lapply(9:26, foo1, dat=pest_cont_b, outcome = "Lipid_disorder")
pest_b_ms <- lapply(9:26, foo1, dat=pest_cont_b, outcome = "MS")
pest_b_hyper <- lapply(9:26, foo1, dat=pest_cont_b, outcome = "hypertension")
pest_b_dia <- lapply(9:26, foo1, dat=pest_cont_b, outcome = "diabetes")

pest_ld[[3]]

saveRDS(pest_b_ob, "../cat_results/univ/univ_pest_b_ob.rds")
saveRDS(pest_b_ld, "../cat_results/univ/univ_pest_b_ld.rds")
saveRDS(pest_b_ms, "../cat_results/univ/univ_pest_b_ms.rds")
saveRDS(pest_b_hyper, "../cat_results/univ/univ_pest_b_hyper.rds")
saveRDS(pest_b_dia, "../cat_results/univ/univ_pest_b_dia.rds")

pest_cont_f[9:26] <- scale(pest_cont_f[9:26])
pest_f_ob <- lapply(9:26, foo1, dat=pest_cont_f, outcome = "Obesity")
pest_f_ld <- lapply(9:26, foo1, dat=pest_cont_f, outcome = "Lipid_disorder")
pest_f_ms <- lapply(9:26, foo1, dat=pest_cont_f, outcome = "MS")
pest_f_hyper <- lapply(9:26, foo1, dat=pest_cont_f, outcome = "hypertension")
pest_f_dia <- lapply(9:26, foo1, dat=pest_cont_f, outcome = "diabetes")

pest_ld[[3]]

saveRDS(pest_f_ob, "../cat_results/univ/univ_pest_f_ob.rds")
saveRDS(pest_f_ld, "../cat_results/univ/univ_pest_f_ld.rds")
saveRDS(pest_f_ms, "../cat_results/univ/univ_pest_f_ms.rds")
saveRDS(pest_f_hyper, "../cat_results/univ/univ_pest_f_hyper.rds")
saveRDS(pest_f_dia, "../cat_results/univ/univ_pest_f_dia.rds")



##### Make data set for Manhattan plot --
manhattan_pest_l <- cbind(do.call("rbind", lapply(pest_l_ob, `[[`, 2)),
                        do.call("rbind", lapply(pest_l_ld, `[[`, 2)),
                        do.call("rbind", lapply(pest_l_ms, `[[`, 2)),
                        do.call("rbind", lapply(pest_l_hyper, `[[`, 2)),
                        do.call("rbind", lapply(pest_l_dia, `[[`, 2)))
# Change to log scale
manhattan_pest_l <- -log10(manhattan_pest_l)
# Change column name
colnames(manhattan_pest_l) <- c("obesity","lipid_disorder", "MS", "hypertension", "diabetes")
saveRDS(manhattan_pest_l, "../cat_results/manhattan_pest_l.rds")

manhattan_pest_b <- cbind(do.call("rbind", lapply(pest_b_ob, `[[`, 2)),
                          do.call("rbind", lapply(pest_b_ld, `[[`, 2)),
                          do.call("rbind", lapply(pest_b_ms, `[[`, 2)),
                          do.call("rbind", lapply(pest_b_hyper, `[[`, 2)),
                          do.call("rbind", lapply(pest_b_dia, `[[`, 2)))
# Change to log scale
manhattan_pest_b <- -log10(manhattan_pest_b)
# Change column name
colnames(manhattan_pest_b) <- c("obesity","lipid_disorder", "MS", "hypertension", "diabetes")
saveRDS(manhattan_pest_b, "../cat_results/manhattan_pest_b.rds")

manhattan_pest_f <- cbind(do.call("rbind", lapply(pest_f_ob, `[[`, 2)),
                          do.call("rbind", lapply(pest_f_ld, `[[`, 2)),
                          do.call("rbind", lapply(pest_f_ms, `[[`, 2)),
                          do.call("rbind", lapply(pest_f_hyper, `[[`, 2)),
                          do.call("rbind", lapply(pest_f_dia, `[[`, 2)))
# Change to log scale
manhattan_pest_f <- -log10(manhattan_pest_f)
# Change column name
colnames(manhattan_pest_f) <- c("obesity","lipid_disorder", "MS", "hypertension", "diabetes")
saveRDS(manhattan_pest_f, "../cat_results/manhattan_pest_f.rds")


manhattan_pahs_l <- cbind(do.call("rbind", lapply(pahs_l_ob, `[[`, 2)),
                        do.call("rbind", lapply(pahs_l_ld, `[[`, 2)),
                        do.call("rbind", lapply(pahs_l_ms, `[[`, 2)),
                        do.call("rbind", lapply(pahs_l_hyper, `[[`, 2)),
                        do.call("rbind", lapply(pahs_l_dia, `[[`, 2)))
# Change to log scale
manhattan_pahs_l <- -log10(manhattan_pahs_l)
# Change column name
colnames(manhattan_pahs_l) <- c("obesity","lipid_disorder", "MS", "hypertension", "diabetes")
saveRDS(manhattan_pahs_l, "../cat_results/manhattan_pahs_l.rds")

manhattan_pahs_b <- cbind(do.call("rbind", lapply(pahs_b_ob, `[[`, 2)),
                          do.call("rbind", lapply(pahs_b_ld, `[[`, 2)),
                          do.call("rbind", lapply(pahs_b_ms, `[[`, 2)),
                          do.call("rbind", lapply(pahs_b_hyper, `[[`, 2)),
                          do.call("rbind", lapply(pahs_b_dia, `[[`, 2)))
# Change to log scale
manhattan_pahs_b <- -log10(manhattan_pahs_b)
# Change column name
colnames(manhattan_pahs_b) <- c("obesity","lipid_disorder", "MS", "hypertension", "diabetes")
saveRDS(manhattan_pahs_b, "../cat_results/manhattan_pahs_b.rds")



manhattan_pest_cat_l <- cbind(do.call("rbind", lapply(pest_cat_l_ob, `[[`, 2)),
                            do.call("rbind", lapply(pest_cat_l_ld, `[[`, 2)),
                            do.call("rbind", lapply(pest_cat_l_ms, `[[`, 2)),
                            do.call("rbind", lapply(pest_cat_l_hyper, `[[`, 2)),
                            do.call("rbind", lapply(pest_cat_l_dia, `[[`, 2)))
# Change to log scale
manhattan_pest_cat_l <- -log10(manhattan_pest_cat_l)
# Change column name
colnames(manhattan_pest_cat_l) <- c("obesity","lipid_disorder", "MS", "hypertension", "diabetes")
saveRDS(manhattan_pest_cat_l, "../cat_results/manhattan_pest_cat_l.rds")


manhattan_pest_cat_b <- cbind(do.call("rbind", lapply(pest_cat_b_ob, `[[`, 2)),
                              do.call("rbind", lapply(pest_cat_b_ld, `[[`, 2)),
                              do.call("rbind", lapply(pest_cat_b_ms, `[[`, 2)),
                              do.call("rbind", lapply(pest_cat_b_hyper, `[[`, 2)),
                              do.call("rbind", lapply(pest_cat_b_dia, `[[`, 2)))
# Change to log scale
manhattan_pest_cat_b <- -log10(manhattan_pest_cat_b)
# Change column name
colnames(manhattan_pest_cat_b) <- c("obesity","lipid_disorder", "MS", "hypertension", "diabetes")
saveRDS(manhattan_pest_cat_b, "../cat_results/manhattan_pest_cat_b.rds")


manhattan_pest_cat_f <- cbind(do.call("rbind", lapply(pest_cat_f_ob, `[[`, 2)),
                              do.call("rbind", lapply(pest_cat_f_ld, `[[`, 2)),
                              do.call("rbind", lapply(pest_cat_f_ms, `[[`, 2)),
                              do.call("rbind", lapply(pest_cat_f_hyper, `[[`, 2)),
                              do.call("rbind", lapply(pest_cat_f_dia, `[[`, 2)))
# Change to log scale
manhattan_pest_cat_f <- -log10(manhattan_pest_cat_f)
# Change column name
colnames(manhattan_pest_cat_f) <- c("obesity","lipid_disorder", "MS", "hypertension", "diabetes")
saveRDS(manhattan_pest_cat_f, "../cat_results/manhattan_pest_cat_f.rds")




manhattan_pahs_cat_l <- cbind(do.call("rbind", lapply(pahs_cat_l_ob, `[[`, 2)),
                            do.call("rbind", lapply(pahs_cat_l_ld, `[[`, 2)),
                            do.call("rbind", lapply(pahs_cat_l_ms, `[[`, 2)),
                            do.call("rbind", lapply(pahs_cat_l_hyper, `[[`, 2)),
                            do.call("rbind", lapply(pahs_cat_l_dia, `[[`, 2)))
# Change to log scale
manhattan_pahs_cat_l <- -log10(manhattan_pahs_cat_l)
# Change column name
colnames(manhattan_pahs_cat_l) <- c("obesity","lipid_disorder", "MS", "hypertension", "diabetes")
saveRDS(manhattan_pahs_cat_l, "../cat_results/manhattan_pahs_cat_l.rds")


manhattan_pahs_cat_b <- cbind(do.call("rbind", lapply(pahs_cat_b_ob, `[[`, 2)),
                              do.call("rbind", lapply(pahs_cat_b_ld, `[[`, 2)),
                              do.call("rbind", lapply(pahs_cat_b_ms, `[[`, 2)),
                              do.call("rbind", lapply(pahs_cat_b_hyper, `[[`, 2)),
                              do.call("rbind", lapply(pahs_cat_b_dia, `[[`, 2)))
# Change to log scale
manhattan_pahs_cat_b <- -log10(manhattan_pahs_cat_b)
# Change column name
colnames(manhattan_pahs_cat_b) <- c("obesity","lipid_disorder", "MS", "hypertension", "diabetes")
saveRDS(manhattan_pahs_cat_b, "../cat_results/manhattan_pahs_cat_b.rds")




### Make data for forest plot ----

foo_1=function(x){
  name <- substitute(x)
  # name=gsub("_ob|_ld|_ms|_hyper|_dia","", x)
  # Extract first element of nest list and rbind for each model results
  table=do.call("rbind", lapply(x, `[[`, 1))
  tmp=apply(table, 1, function(y) exp(y[1]+c(0,-1,1)*qnorm(0.975)*y[2]))
  res=t(tmp)
  res=cbind(res,-log10(table[,3]))
  # Rename columns
  colnames(res)=paste0(c("or_","l95_","u95_","logp_"), name)
  return(res)
}

# cbind into one table
forest_pahs_l <- cbind(foo_1(pahs_l_ob),foo_1(pahs_l_ld),foo_1(pahs_l_ms),
                     foo_1(pahs_l_hyper),foo_1(pahs_l_dia))
rownames(forest_pahs_l) <- colnames(pahs_cont)[9:13]

forest_pahs_b <- cbind(foo_1(pahs_b_ob),foo_1(pahs_b_ld),foo_1(pahs_b_ms),
                       foo_1(pahs_b_hyper),foo_1(pahs_b_dia))
rownames(forest_pahs_b) <- colnames(pahs_cont)[9:13]

forest_pest_l <- cbind(foo_1(pest_l_ob),foo_1(pest_l_ld),foo_1(pest_l_ms),
                     foo_1(pest_l_hyper),foo_1(pest_l_dia))
rownames(forest_pest_l) <- colnames(pest_cont)[9:26]

forest_pest_b <- cbind(foo_1(pest_b_ob),foo_1(pest_b_ld),foo_1(pest_b_ms),
                       foo_1(pest_b_hyper),foo_1(pest_b_dia))
rownames(forest_pest_b) <- colnames(pest_cont)[9:26]

forest_pest_f <- cbind(foo_1(pest_f_ob),foo_1(pest_f_ld),foo_1(pest_f_ms),
                       foo_1(pest_f_hyper),foo_1(pest_f_dia))
rownames(forest_pest_f) <- colnames(pest_cont)[9:26]


forest_pest_cat_l <- cbind(foo_1(pest_cat_l_ob),foo_1(pest_cat_l_ld),foo_1(pest_cat_l_ms),
                         foo_1(pest_cat_l_hyper),foo_1(pest_cat_l_dia))
tmp_pest <- as.data.frame(model.matrix(~.,pest_cat[,9:41])[,-1])
colnames(tmp_pest)
rownames(forest_pest_cat_l) <- colnames(tmp_pest)

forest_pest_cat_b <- cbind(foo_1(pest_cat_b_ob),foo_1(pest_cat_b_ld),foo_1(pest_cat_b_ms),
                           foo_1(pest_cat_b_hyper),foo_1(pest_cat_b_dia))
rownames(forest_pest_cat_b) <- colnames(tmp_pest)

forest_pest_cat_f <- cbind(foo_1(pest_cat_f_ob),foo_1(pest_cat_f_ld),foo_1(pest_cat_f_ms),
                           foo_1(pest_cat_f_hyper),foo_1(pest_cat_f_dia))
tmp_pest1 <- as.data.frame(model.matrix(~.,pest_cat[,c(9:19, 21:41)])[,-1])
colnames(tmp_pest1)
rownames(forest_pest_cat_f) <- colnames(tmp_pest1[c(1:34, 37:38, 41:58)])



forest_pahs_cat_l <- cbind(foo_1(pahs_cat_l_ob),foo_1(pahs_cat_l_ld),foo_1(pahs_cat_l_ms),
                         foo_1(pahs_cat_l_hyper),foo_1(pahs_cat_l_dia))
tmp_pahs <- as.data.frame(model.matrix(~.,pahs_cat[,9:21])[,-1])
colnames(tmp_pahs)
rownames(forest_pahs_cat_l) <- colnames(tmp_pahs)

forest_pahs_cat_b <- cbind(foo_1(pahs_cat_b_ob),foo_1(pahs_cat_b_ld),foo_1(pahs_cat_b_ms),
                           foo_1(pahs_cat_b_hyper),foo_1(pahs_cat_b_dia))
tmp_pahs1 <- as.data.frame(model.matrix(~.,pahs_cat[,c(10:11, 13, 15:16, 18:21)])[,-1])
colnames(tmp_pahs1)
rownames(forest_pahs_cat_b) <- colnames(tmp_pahs1)



saveRDS(forest_pest_l, "../cat_results/forest_pest_l.rds")
saveRDS(forest_pest_b, "../cat_results/forest_pest_b.rds")
saveRDS(forest_pest_f, "../cat_results/forest_pest_f.rds")
saveRDS(forest_pahs_l, "../cat_results/forest_pahs_l.rds")
saveRDS(forest_pahs_b, "../cat_results/forest_pahs_b.rds")

saveRDS(forest_pest_cat_l, "../cat_results/forest_pest_cat_l.rds")
saveRDS(forest_pest_cat_b, "../cat_results/forest_pest_cat_b.rds")
saveRDS(forest_pest_cat_f, "../cat_results/forest_pest_cat_f.rds")
saveRDS(forest_pahs_cat_l, "../cat_results/forest_pahs_cat_l.rds")
saveRDS(forest_pahs_cat_b, "../cat_results/forest_pahs_cat_b.rds")

