
### Sumemr Project -- univariate analysis considering categorical variables
## Created by Vivian on 20 July 

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
saveRDS(pest_cont, "../cat_results/pest_cont_scale.rds")
saveRDS(pahs_cat, "../cat_results/pahs_cont_scale.rds")


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
pest_cat_ob <- lapply(9:41, foo, dat=pest_cat, outcome = "Obesity")
pest_cat_ld <- lapply(9:41, foo, dat=pest_cat, outcome = "Lipid_disorder")
pest_cat_ms <- lapply(9:41, foo, dat=pest_cat, outcome = "MS")
pest_cat_hyper <- lapply(9:41, foo, dat=pest_cat, outcome = "hypertension")
pest_cat_dia <- lapply(9:41, foo, dat=pest_cat, outcome = "diabetes")

pest_cat_dia[[1]]
pahs_ld[[1]]

ifelse(dir.exists("../cat_results/univ"),"",dir.create("../cat_results/univ"))
saveRDS(pest_cat_ob, "../cat_results/univ/univ_pest_cat_ob.rds")
saveRDS(pest_cat_ld, "../cat_results/univ/univ_pest_cat_ld.rds")
saveRDS(pest_cat_ms, "../cat_results/univ/univ_pest_cat_ms.rds")
saveRDS(pest_cat_hyper, "../cat_results/univ/univ_pest_cat_hyper.rds")
saveRDS(pest_cat_dia, "../cat_results/univ/univ_pest_cat_dia.rds")


pahs_cat_ob <- lapply(9:21, foo, dat=pahs_cat, outcome = "Obesity")
pahs_cat_ld <- lapply(9:21, foo, dat=pahs_cat, outcome = "Lipid_disorder")
pahs_cat_ms <- lapply(9:21, foo, dat=pahs_cat, outcome = "MS")
pahs_cat_hyper <- lapply(9:21, foo, dat=pahs_cat, outcome = "hypertension")
pahs_cat_dia <- lapply(9:21, foo, dat=pahs_cat, outcome = "diabetes")

pahs_cat_ld[[10]]

saveRDS(pahs_cat_ob, "../cat_results/univ/univ_pahs_cat_ob.rds")
saveRDS(pahs_cat_ld, "../cat_results/univ/univ_pahs_cat_ld.rds")
saveRDS(pahs_cat_ms, "../cat_results/univ/univ_pahs_cat_ms.rds")
saveRDS(pahs_cat_hyper, "../cat_results/univ/univ_pahs_cat_hyper.rds")
saveRDS(pahs_cat_dia, "../cat_results/univ/univ_pahs_cat_dia.rds")


pahs_ob <- lapply(9:13, foo, dat=pahs_cont, outcome = "Obesity")
pahs_ld <- lapply(9:13, foo, dat=pahs_cont, outcome = "Lipid_disorder")
pahs_ms <- lapply(9:13, foo, dat=pahs_cont, outcome = "MS")
pahs_hyper <- lapply(9:13, foo, dat=pahs_cont, outcome = "hypertension")
pahs_dia <- lapply(9:13, foo, dat=pahs_cont, outcome = "diabetes")

pahs_ld[[3]]

saveRDS(pahs_ob, "../cat_results/univ/univ_pahs_ob.rds")
saveRDS(pahs_ld, "../cat_results/univ/univ_pahs_ld.rds")
saveRDS(pahs_ms, "../cat_results/univ/univ_pahs_ms.rds")
saveRDS(pahs_hyper, "../cat_results/univ/univ_pahs_hyper.rds")
saveRDS(pahs_dia, "../cat_results/univ/univ_pahs_dia.rds")


pest_ob <- lapply(9:26, foo, dat=pest_cont, outcome = "Obesity")
pest_ld <- lapply(9:26, foo, dat=pest_cont, outcome = "Lipid_disorder")
pest_ms <- lapply(9:26, foo, dat=pest_cont, outcome = "MS")
pest_hyper <- lapply(9:26, foo, dat=pest_cont, outcome = "hypertension")
pest_dia <- lapply(9:26, foo, dat=pest_cont, outcome = "diabetes")

pest_ld[[3]]

saveRDS(pest_ob, "../cat_results/univ/univ_pest_ob.rds")
saveRDS(pest_ld, "../cat_results/univ/univ_pest_ld.rds")
saveRDS(pest_ms, "../cat_results/univ/univ_pest_ms.rds")
saveRDS(pest_hyper, "../cat_results/univ/univ_pest_hyper.rds")
saveRDS(pest_dia, "../cat_results/univ/univ_pest_dia.rds")


##### Make data set for Manhattan plot --
manhattan_pest <- cbind(do.call("rbind", lapply(pest_ob, `[[`, 2)),
                        do.call("rbind", lapply(pest_ld, `[[`, 2)),
                        do.call("rbind", lapply(pest_ms, `[[`, 2)),
                        do.call("rbind", lapply(pest_hyper, `[[`, 2)),
                        do.call("rbind", lapply(pest_dia, `[[`, 2)))
# Change to log scale
manhattan_pest <- -log10(manhattan_pest)
# Change column name
colnames(manhattan_pest) <- c("obesity","lipid_disorder", "MS", "hypertension", "diabetes")
saveRDS(manhattan_pest, "../cat_results/manhattan_pest.rds")


manhattan_pahs <- cbind(do.call("rbind", lapply(pahs_ob, `[[`, 2)),
                        do.call("rbind", lapply(pahs_ld, `[[`, 2)),
                        do.call("rbind", lapply(pahs_ms, `[[`, 2)),
                        do.call("rbind", lapply(pahs_hyper, `[[`, 2)),
                        do.call("rbind", lapply(pahs_dia, `[[`, 2)))
# Change to log scale
manhattan_pahs <- -log10(manhattan_pahs)
# Change column name
colnames(manhattan_pahs) <- c("obesity","lipid_disorder", "MS", "hypertension", "diabetes")
saveRDS(manhattan_pahs, "../cat_results/manhattan_pahs.rds")


manhattan_pest_cat <- cbind(do.call("rbind", lapply(pest_cat_ob, `[[`, 2)),
                            do.call("rbind", lapply(pest_cat_ld, `[[`, 2)),
                            do.call("rbind", lapply(pest_cat_ms, `[[`, 2)),
                            do.call("rbind", lapply(pest_cat_hyper, `[[`, 2)),
                            do.call("rbind", lapply(pest_cat_dia, `[[`, 2)))
# Change to log scale
manhattan_pest_cat <- -log10(manhattan_pest_cat)
# Change column name
colnames(manhattan_pest_cat) <- c("obesity","lipid_disorder", "MS", "hypertension", "diabetes")
saveRDS(manhattan_pest_cat, "../cat_results/manhattan_pest_cat.rds")


manhattan_pahs_cat <- cbind(do.call("rbind", lapply(pahs_cat_ob, `[[`, 2)),
                            do.call("rbind", lapply(pahs_cat_ld, `[[`, 2)),
                            do.call("rbind", lapply(pahs_cat_ms, `[[`, 2)),
                            do.call("rbind", lapply(pahs_cat_hyper, `[[`, 2)),
                            do.call("rbind", lapply(pahs_cat_dia, `[[`, 2)))
# Change to log scale
manhattan_pahs_cat <- -log10(manhattan_pahs_cat)
# Change column name
colnames(manhattan_pahs_cat) <- c("obesity","lipid_disorder", "MS", "hypertension", "diabetes")
saveRDS(manhattan_pahs_cat, "../cat_results/manhattan_pahs_cat.rds")




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
forest_pahs <- cbind(foo_1(pahs_ob),foo_1(pahs_ld),foo_1(pahs_ms),
                     foo_1(pahs_hyper),foo_1(pahs_dia))
rownames(forest_pahs) <- colnames(pahs_cont)[9:13]

forest_pest <- cbind(foo_1(pest_ob),foo_1(pest_ld),foo_1(pest_ms),
                     foo_1(pest_hyper),foo_1(pest_dia))
colnames(pest_cont)
rownames(forest_pest) <- colnames(pest_cont)[9:26]

forest_pest_cat <- cbind(foo_1(pest_cat_ob),foo_1(pest_cat_ld),foo_1(pest_cat_ms),
                         foo_1(pest_cat_hyper),foo_1(pest_cat_dia))
tmp_pest <- as.data.frame(model.matrix(~.,pest_cat[,9:41])[,-1])
colnames(tmp_pest)
rownames(forest_pest_cat) <- colnames(tmp_pest)

forest_pahs_cat <- cbind(foo_1(pahs_cat_ob),foo_1(pahs_cat_ld),foo_1(pahs_cat_ms),
                         foo_1(pahs_cat_hyper),foo_1(pahs_cat_dia))
tmp_pahs <- as.data.frame(model.matrix(~.,pahs_cat[,9:21])[,-1])
colnames(tmp_pahs)
rownames(forest_pahs_cat) <- colnames(tmp_pahs)

saveRDS(forest_pest, "../cat_results/forest_pest.rds")
saveRDS(forest_pahs, "../cat_results/forest_pahs.rds")
saveRDS(forest_pest_cat, "../cat_results/forest_pest_cat.rds")
saveRDS(forest_pahs_cat, "../cat_results/forest_pahs_cat.rds")
