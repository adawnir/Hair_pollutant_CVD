### Imputing missing values: Diagnostics

rm(list=ls())
project_path=dirname(rstudioapi::getActiveDocumentContext()$path)
setwd(project_path)

library(sm)

filepaths = c("PesticidesPCBBDEs","PAHs")
m = 1

# Exposures ----
# Pre-imputation
expo = readRDS(paste0("../Processed/",filepaths[m],"_expo_ndimp.rds"))
# Post
expo_imp = readRDS(paste0("../Processed/",filepaths[m],"_expo_ndimp_naimp.rds"))

# Distribution of imputed values and observed values
isna = colnames(expo)[apply(expo, 2, function(x) sum(is.na(x))>2)]
if(length(isna)>0){
  {pdf(paste0("../Figures/Supplementary/Expo_dist_obsv_imp_RF_",filepaths[m],".pdf"), height = ceiling(length(isna)/2)*5, width = 9)
    par(mfrow = c(ceiling(length(isna)/2),2), pty = "s", mar = c(5,5,5,1))
    for (k in 1:length(isna)){
      var = isna[k]
      print(var)
      tmp = factor(ifelse(is.na(expo[,var]), 1, 0))
      sm.density.compare(c(expo_imp[,k]),tmp,
                         xlab="pg/mg", ylab ="Density",
                         lwd = 1, lty = c(1,1), col = c("#0019a8","#dc251f"))
      title(main=var)
      legend("topright", lwd = 1, lty = c(1,1), legend = c("observed","imputed"),col = c("#0019a8","#dc251f"))
    }
    dev.off()
  }
}

# Diet information ----
# Pre-imputation
covar = readRDS(paste0("../Data/",filepaths[m],"_covar.rds"))
covar = covar %>% select(Energy, Proteins, Fats, Carbohydrates, Fibers)
# Post
covar_imp = readRDS(paste0("../Processed/",filepaths[m],"_covar_naimp.rds"))
covar_imp = covar_imp %>% select(Energy, Proteins, Fats, Carbohydrates, Fibers)

# Distribution of imputed values and observed values
isna = colnames(covar)[apply(covar, 2, function(x) sum(is.na(x))>2)]
if(length(isna)>0){
  {pdf(paste0("../Figures/Supplementary/Covar_dist_obsv_imp_RF_",filepaths[m],".pdf"), height = ceiling(length(isna)/2)*5, width = 9)
    par(mfrow = c(ceiling(length(isna)/2),2), pty = "s", mar = c(5,5,5,1))
    for (k in 1:length(isna)){
      var = isna[k]
      print(var)
      tmp = factor(ifelse(is.na(covar[,var]), 1, 0))
      sm.density.compare(c(covar_imp[,k]), tmp,
                         xlab="", ylab ="Density",
                         lwd = 1, lty = c(1,1), col = c("#0019a8","#dc251f"))
      title(main=var)
      legend("topright", lwd = 1, lty = c(1,1), legend = c("observed","imputed"),col = c("#0019a8","#dc251f"))
    }
    dev.off()
  }
}