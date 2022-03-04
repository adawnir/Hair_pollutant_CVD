
### Sumemr Project -- Descriptive analyses on categorical variables
## Created by Vivian on 20 July

rm(list=ls())
project_path="~/hair/scripts"
setwd(project_path)


##### Load data
pest_cat_l50 <- readRDS("../processed/cat_imputation/pest_imp_l50.rds")
pest_cat_5080 <- readRDS("../processed/cat_imputation/pest_imp_5080.rds")
pest_cat_h80 <- readRDS("../processed/cat_imputation/pest_imp_h80.rds")

pahs_cat_l50 <- readRDS("../processed/cat_imputation/pahs_imp_l50.rds")
pahs_cat_5080 <- readRDS("../processed/cat_imputation/pahs_imp_5080.rds")
pahs_cat_h80 <- readRDS("../processed/cat_imputation/pahs_imp_h80.rds")

pest <- readRDS("../data/PesticidesPCBBDEs.rds")
pahs <- readRDS("../data/PAHs.rds")


##### Detection frequencies --

pest_nd_prop <- apply(pest[,16:48], 2, function(x) sum(x=="nd", na.rm = TRUE)/nrow(pest)) %>% 
  sort(decreasing = T)
pest_df_prop <- 1- pest_nd_prop

pahs_nd_prop <- apply(pahs[,16:28], 2, function(x) sum(x=="nd", na.rm = TRUE)/nrow(pahs)) %>% 
  sort(decreasing = T)
pahs_df_prop <- 1- pahs_nd_prop


##### Check categorical from chemicals over 80% of detection
prop.table(table(pahs_cat_h80$`Nicotine (ng/mg)`))
prop.table(table(pahs_cat_h80$`1-OH-Naphthalene`))
prop.table(table(pahs_cat_h80$`2-OH-Naphthalene`))

colnames(pest_cat_h80)
prop.table(table(pest_cat_h80$HCB))
prop.table(table(pest_cat_h80$PCP))
prop.table(table(pest_cat_h80$Chlorpyrifos))
prop.table(table(pest_cat_h80$TCPy))
prop.table(table(pest_cat_h80$DEP))
prop.table(table(pest_cat_h80$PNP))
prop.table(table(pest_cat_h80[,9]))
prop.table(table(pest_cat_h80$Cl2CA))
prop.table(table(pest_cat_h80$`γ-HCH`))
prop.table(table(pest_cat_h80$`α-endosulfan`))
prop.table(table(pest_cat_h80$Trifluralin))
prop.table(table(pest_cat_h80$`Fipronil sulfone`))
prop.table(table(pest_cat_h80[,11]))



