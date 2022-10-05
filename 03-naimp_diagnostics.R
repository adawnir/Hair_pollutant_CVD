### Imputing missing values: Diagnostics

rm(list=ls())
project_path=dirname(rstudioapi::getActiveDocumentContext()$path)
setwd(project_path)

library(sm)

# Load data
# Pre-imputation
train_pest = readRDS("../Processed/PesticidesPCBBDEs_train_info_expo_ndimp.rds")
train_pahs = readRDS("../Processed/PAHs_train_info_expo_ndimp.rds")
test = readRDS("../Processed/Overlap_test_info_expo_ndimp.rds")

# Post
train_pest_imp = readRDS("../Processed/PesticidesPCBBDEs_train_info_expo_ndimp_naimp.rds")
train_pahs_imp = readRDS("../Processed/PAHs_train_info_expo_ndimp_naimp.rds")
test_imp = readRDS("../Processed/Overlap_test_info_expo_ndimp_naimp.rds")

## Diet information ----
tmp = train_pest[,10:14]
tmp_imp = train_pest_imp[,10:14]
# Distribution of imputed values and observed values
isna = colnames(tmp)[apply(tmp, 2, function(x) sum(is.na(x))>1)]
length(isna)
length(isna)
{pdf(paste0("../Figures/Descriptive_statistics/Diet_info_dist_obsv_imp_RF_PesticidesPCBBDEs_train.pdf"), height = ceiling(length(isna)/2)*5, width = 9)
  par(mfrow = c(ceiling(length(isna)/2),2), pty = "s", mar = c(5,5,5,1))
  for (k in 1:length(isna)){
    var = isna[k]
    print(var)
    tmp_imp[which(is.na(tmp[,var])),"data_type"] = "observed"
    tmp_imp[-which(is.na(tmp[,var])),"data_type"] = "imputed"
    tmp_imp$data_type = factor(tmp_imp$data_type, levels = c("observed","imputed"))
    sm.density.compare(tmp_imp[,k],tmp_imp$data_type,
                       xlab="pg/mg", ylab ="Density",
                       lwd = 1, lty = c(1,1), col = c("#0019a8","#dc251f"))
    title(main=var)
    legend("topright", lwd = 1, lty = c(1,1), legend = c("observed","imputed"),col = c("#0019a8","#dc251f"))
    }
  dev.off()
}

tmp = train_pahs[,10:14]
tmp_imp = train_pahs_imp[,10:14]
# Distribution of imputed values and observed values
isna = colnames(tmp)[apply(tmp, 2, function(x) sum(is.na(x))>1)]
length(isna)
{pdf(paste0("../Figures/Descriptive_statistics/Diet_info_dist_obsv_imp_RF_PAHs_train.pdf"), height = ceiling(length(isna)/2)*5, width = 9)
  par(mfrow = c(ceiling(length(isna)/2),2), pty = "s", mar = c(5,5,5,1))
  for (k in 1:length(isna)){
    var = isna[k]
    print(var)
    tmp_imp[which(is.na(tmp[,var])),"data_type"] = "observed"
    tmp_imp[-which(is.na(tmp[,var])),"data_type"] = "imputed"
    tmp_imp$data_type = factor(tmp_imp$data_type, levels = c("observed","imputed"))
    sm.density.compare(tmp_imp[,k],tmp_imp$data_type,
                       xlab="pg/mg", ylab ="Density",
                       lwd = 1, lty = c(1,1), col = c("#0019a8","#dc251f"))
    title(main=var)
    legend("topright", lwd = 1, lty = c(1,1), legend = c("observed","imputed"),col = c("#0019a8","#dc251f"))
  }
  dev.off()
}

tmp = test[,10:14]
tmp_imp = test_imp[,10:14]
# Distribution of imputed values and observed values
isna = colnames(tmp)[apply(tmp, 2, function(x) sum(is.na(x))>1)]
length(isna)
{pdf(paste0("../Figures/Descriptive_statistics/Diet_info_dist_obsv_imp_RF_Overlap_test.pdf"), height = ceiling(length(isna)/2)*5, width = 9)
  par(mfrow = c(ceiling(length(isna)/2),2), pty = "s", mar = c(5,5,5,1))
  for (k in 1:length(isna)){
    var = isna[k]
    print(var)
    tmp_imp[which(is.na(tmp[,var])),"data_type"] = "observed"
    tmp_imp[-which(is.na(tmp[,var])),"data_type"] = "imputed"
    tmp_imp$data_type = factor(tmp_imp$data_type, levels = c("observed","imputed"))
    sm.density.compare(tmp_imp[,k],tmp_imp$data_type,
                       xlab="pg/mg", ylab ="Density",
                       lwd = 1, lty = c(1,1), col = c("#0019a8","#dc251f"))
    title(main=var)
    legend("topright", lwd = 1, lty = c(1,1), legend = c("observed","imputed"),col = c("#0019a8","#dc251f"))
  }
  dev.off()
}

## Exposure ----
tmp = log10(train_pest[,34:ncol(train_pest)])
tmp_imp = log10(train_pest_imp[,34:ncol(train_pest)])
# Distribution of imputed values and observed values
isna = colnames(tmp)[apply(tmp, 2, function(x) sum(is.na(x))>1)]
length(isna)
{pdf(paste0("../Figures/Descriptive_statistics/Expo_dist_obsv_imp_RF_PesticidesPCBBDEs_train.pdf"), height = ceiling(length(isna)/2)*5, width = 9)
  par(mfrow = c(ceiling(length(isna)/2),2), pty = "s", mar = c(5,5,5,1))
  for (k in 1:length(isna)){
    var = isna[k]
    print(var)
    tmp_imp[which(is.na(tmp[,var])),"data_type"] = "observed"
    tmp_imp[-which(is.na(tmp[,var])),"data_type"] = "imputed"
    tmp_imp$data_type = factor(tmp_imp$data_type, levels = c("observed","imputed"))
    sm.density.compare(tmp_imp[,k],tmp_imp$data_type,
                       xlab="pg/mg", ylab ="Density",
                       lwd = 1, lty = c(1,1), col = c("#0019a8","#dc251f"))
    title(main=var)
    legend("topright", lwd = 1, lty = c(1,1), legend = c("observed","imputed"),col = c("#0019a8","#dc251f"))
  }
  dev.off()
}

tmp = log10(train_pahs[,34:ncol(train_pahs)])
tmp_imp = log10(train_pahs_imp[,34:ncol(train_pahs_imp)])
# Distribution of imputed values and observed values
isna = colnames(tmp)[apply(tmp, 2, function(x) sum(is.na(x))>1)]
length(isna)
{pdf(paste0("../Figures/Descriptive_statistics/Expo_dist_obsv_imp_RF_PAHs_train.pdf"), height = ceiling(length(isna)/2)*5, width = 9)
  par(mfrow = c(ceiling(length(isna)/2),2), pty = "s", mar = c(5,5,5,1))
  for (k in 1:length(isna)){
    var = isna[k]
    print(var)
    tmp_imp[which(is.na(tmp[,var])),"data_type"] = "observed"
    tmp_imp[-which(is.na(tmp[,var])),"data_type"] = "imputed"
    tmp_imp$data_type = factor(tmp_imp$data_type, levels = c("observed","imputed"))
    sm.density.compare(tmp_imp[,k],tmp_imp$data_type,
                       xlab="pg/mg", ylab ="Density",
                       lwd = 1, lty = c(1,1), col = c("#0019a8","#dc251f"))
    title(main=var)
    legend("topright", lwd = 1, lty = c(1,1), legend = c("observed","imputed"),col = c("#0019a8","#dc251f"))
  }
  dev.off()
}

tmp = log10(test[,34:ncol(test)])
tmp_imp = log10(test_imp[,34:ncol(test_imp)])
# Distribution of imputed values and observed values
isna = colnames(tmp)[apply(tmp, 2, function(x) sum(is.na(x))>1)]
length(isna)
{pdf(paste0("../Figures/Descriptive_statistics/Expo_dist_obsv_imp_RF_Overlap_test.pdf"), height = ceiling(length(isna)/2)*5, width = 9)
  par(mfrow = c(ceiling(length(isna)/2),2), pty = "s", mar = c(5,5,5,1))
  for (k in 1:length(isna)){
    var = isna[k]
    print(var)
    tmp_imp[which(is.na(tmp[,var])),"data_type"] = "observed"
    tmp_imp[-which(is.na(tmp[,var])),"data_type"] = "imputed"
    tmp_imp$data_type = factor(tmp_imp$data_type, levels = c("observed","imputed"))
    sm.density.compare(tmp_imp[,k],tmp_imp$data_type,
                       xlab="pg/mg", ylab ="Density",
                       lwd = 1, lty = c(1,1), col = c("#0019a8","#dc251f"))
    title(main=var)
    legend("topright", lwd = 1, lty = c(1,1), legend = c("observed","imputed"),col = c("#0019a8","#dc251f"))
  }
  dev.off()
}

