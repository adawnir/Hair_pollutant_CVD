## Univariate analyses: association between exposures and CVDRF outcomes

rm(list=ls())
project_path=dirname(rstudioapi::getActiveDocumentContext()$path)
setwd(project_path)

library(tidyverse)
library(parallel)

## Load data ----
filepaths = c("PesticidesPCBBDEs","PAHs","Combined")
for (i in 1:3){
  # Load data
  data = readRDS(paste0("../Processed/",filepaths[i],"_XY.rds"))
  
  # Extract data
  expo = data[c(21:ncol(data))]
  Y = data[,c(16:20)]
  
  # Scale exposures
  expo = scale(expo)
}
##### Logistic regression----
### Get beta coefficients, standard errors, glm p-values and anova p-values

foo = function(i, dat, outcome = NULL){
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


foo_con = function(i, dat, outcome = NULL){
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


### Run models
# PAHs
pahs_ob = lapply(c(2,17:29), foo, dat=pahs_tmp, outcome = "Obesity")
pahs_ld = lapply(c(2,17:29), foo, dat=pahs_tmp, outcome = "Lipid_disorder")
pahs_ms = lapply(c(2,17:29), foo, dat=pahs_tmp, outcome = "MS")
pahs_hyper = lapply(c(2,17:29), foo, dat=pahs_tmp, outcome = "hypertension")
pahs_dia = lapply(c(2,17:29), foo, dat=pahs_tmp, outcome = "diabetes")

pahs_ob[[2]]
pahs_ld[[1]]

ifelse(dir.exists("../results/univ"),"",dir.create("../results/univ"))
saveRDS(pahs_ob, "../results/univ/univ_pahs_ob.rds")
saveRDS(pahs_ld, "../results/univ/univ_pahs_ld.rds")
saveRDS(pahs_ms, "../results/univ/univ_pahs_ms.rds")
saveRDS(pahs_hyper, "../results/univ/univ_pahs_hyper.rds")
saveRDS(pahs_dia, "../results/univ/univ_pahs_dia.rds")

pahs_ob_con = lapply(17:29, foo_con, dat=pahs_tmp, outcome = "Obesity")
pahs_ld_con = lapply(17:29, foo_con, dat=pahs_tmp, outcome = "Lipid_disorder")
pahs_ms_con = lapply(17:29, foo_con, dat=pahs_tmp, outcome = "MS")
pahs_hyper_con = lapply(17:29, foo_con, dat=pahs_tmp, outcome = "hypertension")
pahs_dia_con = lapply(17:29, foo_con, dat=pahs_tmp, outcome = "diabetes")

saveRDS(pahs_ob_con, "../results/univ/univ_pahs_ob_con.rds")
saveRDS(pahs_ld_con, "../results/univ/univ_pahs_ld_con.rds")
saveRDS(pahs_ms_con, "../results/univ/univ_pahs_ms_con.rds")
saveRDS(pahs_hyper_con, "../results/univ/univ_pahs_hyper_con.rds")
saveRDS(pahs_dia_con, "../results/univ/univ_pahs_dia_con.rds")

# Pest
pest_ob = lapply(c(2, 17:49), foo, dat=pest_tmp, outcome = "Obesity")
pest_ld = lapply(c(2, 17:49), foo, dat=pest_tmp, outcome = "Lipid_disorder")
pest_ms = lapply(c(2, 17:49), foo, dat=pest_tmp, outcome = "MS")
pest_hyper = lapply(c(2, 17:49), foo, dat=pest_tmp, outcome = "hypertension")
pest_dia = lapply(c(2, 17:49), foo, dat=pest_tmp, outcome = "diabetes")

saveRDS(pest_ob, "../results/univ/univ_pest_ob.rds")
saveRDS(pest_ld, "../results/univ/univ_pest_ld.rds")
saveRDS(pest_ms, "../results/univ/univ_pest_ms.rds")
saveRDS(pest_hyper, "../results/univ/univ_pest_hyper.rds")
saveRDS(pest_dia, "../results/univ/univ_pest_dia.rds")

pest_ob_con = lapply(17:49, foo_con, dat=pest_tmp, outcome = "Obesity")
pest_ld_con = lapply(17:49, foo_con, dat=pest_tmp, outcome = "Lipid_disorder")
pest_ms_con = lapply(17:49, foo_con, dat=pest_tmp, outcome = "MS")
pest_hyper_con = lapply(17:49, foo_con, dat=pest_tmp, outcome = "hypertension")
pest_dia_con = lapply(17:49, foo_con, dat=pest_tmp, outcome = "diabetes")

saveRDS(pest_ob_con, "../results/univ/univ_pest_ob_con.rds")
saveRDS(pest_ld_con, "../results/univ/univ_pest_ld_con.rds")
saveRDS(pest_ms_con, "../results/univ/univ_pest_ms_con.rds")
saveRDS(pest_hyper_con, "../results/univ/univ_pest_hyper_con.rds")
saveRDS(pest_dia_con, "../results/univ/univ_pest_dia_con.rds")

# Overlapping
over_ob = lapply(c(2,17:62), foo, dat=over_tmp, outcome = "Obesity")
over_ld = lapply(c(2,17:62), foo, dat=over_tmp, outcome = "Lipid_disorder")
over_ms = lapply(c(2,17:62), foo, dat=over_tmp, outcome = "MS")
over_hyper = lapply(c(2,17:62), foo, dat=over_tmp, outcome = "hypertension")
over_dia = lapply(c(2,17:62), foo, dat=over_tmp, outcome = "diabetes")

saveRDS(over_ob, "../results/univ/univ_over_ob.rds")
saveRDS(over_ld, "../results/univ/univ_over_ld.rds")
saveRDS(over_ms, "../results/univ/univ_over_ms.rds")
saveRDS(over_hyper, "../results/univ/univ_over_hyper.rds")
saveRDS(over_dia, "../results/univ/univ_over_dia.rds")



##### Make data set for Manhattan plot --
### Pest
manhattan=cbind(do.call("rbind", lapply(pest_ob, `[[`, 2)),
                do.call("rbind", lapply(pest_ld, `[[`, 2)),
                do.call("rbind", lapply(pest_ms, `[[`, 2)),
                do.call("rbind", lapply(pest_hyper, `[[`, 2)),
                do.call("rbind", lapply(pest_dia, `[[`, 2)))

manhattan=-log10(manhattan)
colnames(manhattan) = c("obesity","lipid_disorder", "MS", "hypertension", "diabetes")
saveRDS(manhattan, "../results/manhattan_pest.rds")


manhattan_con = cbind(do.call("rbind", lapply(pest_ob_con, `[[`, 2)),
                       do.call("rbind", lapply(pest_ld_con, `[[`, 2)),
                       do.call("rbind", lapply(pest_ms_con, `[[`, 2)),
                       do.call("rbind", lapply(pest_hyper_con, `[[`, 2)),
                       do.call("rbind", lapply(pest_dia_con, `[[`, 2)))

manhattan_con = -log10(manhattan_con)
colnames(manhattan_con) = c("obesity","lipid_disorder", "MS", "hypertension", "diabetes")
saveRDS(manhattan_con, "../results/manhattan_pest_con.rds")


### PAHs
manhattan_pahs = cbind(do.call("rbind", lapply(pahs_ob, `[[`, 2)),
                        do.call("rbind", lapply(pahs_ld, `[[`, 2)),
                        do.call("rbind", lapply(pahs_ms, `[[`, 2)),
                        do.call("rbind", lapply(pahs_hyper, `[[`, 2)),
                        do.call("rbind", lapply(pahs_dia, `[[`, 2)))

manhattan_pahs = -log10(manhattan_pahs)
colnames(manhattan_pahs) = c("obesity","lipid_disorder", "MS", "hypertension", "diabetes")
saveRDS(manhattan_pahs, "../results/manhattan_pahs.rds")


manhattan_pahs_con = cbind(do.call("rbind", lapply(pahs_ob_con, `[[`, 2)),
                            do.call("rbind", lapply(pahs_ld_con, `[[`, 2)),
                            do.call("rbind", lapply(pahs_ms_con, `[[`, 2)),
                            do.call("rbind", lapply(pahs_hyper_con, `[[`, 2)),
                            do.call("rbind", lapply(pahs_dia_con, `[[`, 2)))

manhattan_pahs_con = -log10(manhattan_pahs_con)
colnames(manhattan_pahs_con) = c("obesity","lipid_disorder", "MS", "hypertension", "diabetes")
saveRDS(manhattan_pahs_con, "../results/manhattan_pahs_con.rds")


### Overlapping
manhattan_over = cbind(do.call("rbind", lapply(over_ob, `[[`, 2)),
                        do.call("rbind", lapply(over_ld, `[[`, 2)),
                        do.call("rbind", lapply(over_ms, `[[`, 2)),
                        do.call("rbind", lapply(over_hyper, `[[`, 2)),
                        do.call("rbind", lapply(over_dia, `[[`, 2)))

manhattan_over = -log10(manhattan_over)
colnames(manhattan_over) = c("obesity","lipid_disorder", "MS", "hypertension", "diabetes")
saveRDS(manhattan_over, "../results/manhattan_over.rds")



### Make data for forest plot ----

foo_1=function(x){
  name = substitute(x)
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
forest_pahs = cbind(foo_1(pahs_ob),foo_1(pahs_ld),foo_1(pahs_ms),
             foo_1(pahs_hyper),foo_1(pahs_dia))
tmp_pahs = as.data.frame(model.matrix(~.,pahs_tmp[,c(2, 17:29)])[,-1])
colnames(tmp_pahs)
rownames(forest_pahs) = colnames(tmp_pahs)[1:14]


forest_pest = cbind(foo_1(pest_ob),foo_1(pest_ld),foo_1(pest_ms),
                     foo_1(pest_hyper),foo_1(pest_dia))
tmp_pest = as.data.frame(model.matrix(~.,pest_tmp[,c(2, 17:49)])[,-1])
colnames(tmp_pest)
rownames(forest_pest) = colnames(tmp_pest )[1:35]

forest_over = cbind(foo_1(over_ob),foo_1(over_ld),foo_1(over_ms),
                     foo_1(over_hyper),foo_1(over_dia))
tmp_over = as.data.frame(model.matrix(~.,over_tmp[,c(2,17:62)])[,-1])
colnames(tmp_over)
rownames(forest_over) = colnames(tmp_over)


saveRDS(forest_pest, "../results/forest_pest.rds")
saveRDS(forest_pahs, "../results/forest_pahs.rds")
saveRDS(forest_over, "../results/forest_over.rds")


forest_pahs_con = cbind(foo_1(pahs_ob_con),foo_1(pahs_ld_con),foo_1(pahs_ms_con),
                         foo_1(pahs_hyper_con),foo_1(pahs_dia_con))
tmp_pahs = as.data.frame(model.matrix(~.,pahs_tmp[,c(2, 17:29)])[,-1])
colnames(tmp_pahs)
rownames(forest_pahs_con) = colnames(tmp_pahs)[2:14]


forest_pest_con = cbind(foo_1(pest_ob_con),foo_1(pest_ld_con),foo_1(pest_ms_con),
                         foo_1(pest_hyper_con),foo_1(pest_dia_con))
tmp_pest = as.data.frame(model.matrix(~.,pest_tmp[,c(2, 17:49)])[,-1])
colnames(tmp_pest)
rownames(forest_pest_con) = colnames(tmp_pest)[3:35]

saveRDS(forest_pest_con, "../results/forest_pest_con.rds")
saveRDS(forest_pahs_con, "../results/forest_pahs_con.rds")
