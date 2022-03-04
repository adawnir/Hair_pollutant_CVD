
### Sumemr Project -- LASSO inspect (correlation)
## Created by Vivian on 13 July

rm(list=ls())
project_path="~/hair/scripts"
setwd(project_path)

source("penalisation_functions.R")

##### Load data --
pahs <- readRDS("../processed/imputation/imp_pahs.rds")
pest <- readRDS("../processed/imputation/imp_pest.rds")

for (m in 1:10){
  arr=paste0(rep(c("pest_de", "pahs_de"), each = 5), "_", 
             c("obesity", "hypertension", "diabetes", "Lipid_disorder", "MS"))[m]
  
  ### Lasso
  lasso_out=readRDS(paste0("../results/lasso/out_",arr,".rds")) # Load output
  assign(paste0("lasso_out_",arr),lasso_out) # Assign name
  lasso_beta=names(which(CalibratedStableRegression(lasso_out) == 1)) # Shrink non-selected beta to zero
  assign(paste0("lasso_beta_",arr),lasso_beta)
}



##### Correlation --
corr_pest <- cor(pest, use="pairwise.complete.obs") %>% data.frame()
pheatmap(corr_pest, na_col = "grey80", cluster_rows = T, cluster_cols = T, border=NA, 
         breaks = seq(-1, 1, length.out = 100),
         display_numbers = T, number_color = "black", fontsize_number = 5)

corr_pahs <- cor(pahs, use="pairwise.complete.obs") %>% data.frame()
pheatmap(corr_pahs, na_col = "grey80", cluster_rows = T, cluster_cols = T, border=NA, 
         breaks = seq(-1, 1, length.out = 100),
         display_numbers = T, number_color = "black", fontsize_number = 5)


##### Pesticides
### Obesity
lasso_beta_pest_de_obesity
corr_pest$α.endosulfan

### Hypertension
lasso_beta_pest_de_hypertension
corr_pest$β.HCH
View(corr_pest[,c("γ.HCH", "HCB", "Diflufenican", "Oxadiazon")])

### MS
lasso_beta_pest_de_MS
View(corr_pest[,c("β.HCH", "HCB")])


##### PAHS
### Diabetes
lasso_beta_pahs_de_diabetes



