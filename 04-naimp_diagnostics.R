### Imputing missing values: Diagnostics

rm(list=ls())
project_path=dirname(rstudioapi::getActiveDocumentContext()$path)
setwd(project_path)

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
tmp = train_pest[,7:11]
tmp_imp = train_pest_imp[,7:11]
# Distribution of imputed values and observed values
isna = colnames(tmp)[apply(tmp, 2, function(x) sum(is.na(x))>1)]
length(isna)
{pdf(paste0("../Figures/PesticidesPCBBDEs_train_diet_dist_obsv_imp_RF.pdf"))
  par(mfrow = c(3, 3), oma = c(0.5,0.5,0,0.5), mar = c(2,2,2,1), mgp=c(1,0.05,0))
  for (k in 1:length(isna)){
    var = isna[k]
    print(var)
    observed = tmp_imp[which(is.na(tmp[,var])),]
    imputed = tmp_imp[-which(is.na(tmp[,var])),]
    ylim = range(density(observed[,var], na.rm = T)$y,density(imputed[,var], na.rm = T)$y)
    xlim = range(density(observed[,var], na.rm = T)$x,density(imputed[,var], na.rm = T)$x)
    plot(NULL, main=var, xlab="pg/mg", ylab ="Density",
         xlim=c(min(xlim, na.rm = T),max(xlim, na.rm = T)),
         ylim=c(min(ylim, na.rm = T),max(ylim, na.rm = T)),
         cex.main = 1,  cex.lab=0.8, cex.axis=0.8, tck=-0.01, lwd = 0.5)
    x=density(observed[,var], na.rm = T)
    lines(x, col="royalblue", lwd = 0.5)
    x=density(imputed[,var], na.rm = T)
    lines(x, col="tomato", lwd = 0.5)
  }
  dev.off()
}

tmp = train_pahs[,7:11]
tmp_imp = train_pahs_imp[,7:11]
# Distribution of imputed values and observed values
isna = colnames(tmp)[apply(tmp, 2, function(x) sum(is.na(x))>1)]
length(isna)
{pdf(paste0("../Figures/PAHs_train_diet_dist_obsv_imp_RF.pdf"))
  par(mfrow = c(3, 3), oma = c(0.5,0.5,0,0.5), mar = c(2,2,2,1), mgp=c(1,0.05,0))
  for (k in 1:length(isna)){
    var = isna[k]
    print(var)
    observed = tmp_imp[which(is.na(tmp[,var])),]
    imputed = tmp_imp[-which(is.na(tmp[,var])),]
    ylim = range(density(observed[,var], na.rm = T)$y,density(imputed[,var], na.rm = T)$y)
    xlim = range(density(observed[,var], na.rm = T)$x,density(imputed[,var], na.rm = T)$x)
    plot(NULL, main=var, xlab="pg/mg", ylab ="Density",
         xlim=c(min(xlim, na.rm = T),max(xlim, na.rm = T)),
         ylim=c(min(ylim, na.rm = T),max(ylim, na.rm = T)),
         cex.main = 1,  cex.lab=0.8, cex.axis=0.8, tck=-0.01, lwd = 0.5)
    x=density(observed[,var], na.rm = T)
    lines(x, col="royalblue", lwd = 0.5)
    x=density(imputed[,var], na.rm = T)
    lines(x, col="tomato", lwd = 0.5)
  }
  dev.off()
}

tmp = test[,7:11]
tmp_imp = test_imp[,7:11]
# Distribution of imputed values and observed values
isna = colnames(tmp)[apply(tmp, 2, function(x) sum(is.na(x))>1)]
length(isna)
{pdf(paste0("../Figures/Overlap_test_diet_dist_obsv_imp_RF.pdf"))
  par(mfrow = c(3, 3), oma = c(0.5,0.5,0,0.5), mar = c(2,2,2,1), mgp=c(1,0.05,0))
  for (k in 1:length(isna)){
    var = isna[k]
    print(var)
    observed = tmp_imp[which(is.na(tmp[,var])),]
    imputed = tmp_imp[-which(is.na(tmp[,var])),]
    ylim = range(density(observed[,var], na.rm = T)$y,density(imputed[,var], na.rm = T)$y)
    xlim = range(density(observed[,var], na.rm = T)$x,density(imputed[,var], na.rm = T)$x)
    plot(NULL, main=var, xlab="pg/mg", ylab ="Density",
         xlim=c(min(xlim, na.rm = T),max(xlim, na.rm = T)),
         ylim=c(min(ylim, na.rm = T),max(ylim, na.rm = T)),
         cex.main = 1,  cex.lab=0.8, cex.axis=0.8, tck=-0.01, lwd = 0.5)
    x=density(observed[,var], na.rm = T)
    lines(x, col="royalblue", lwd = 0.5)
    x=density(imputed[,var], na.rm = T)
    lines(x, col="tomato", lwd = 0.5)
  }
  dev.off()
}

## Exposure ----
tmp = log10(train_pest[,31:ncol(train_pest)])
tmp_imp = log10(train_pest_imp[,31:ncol(train_pest)])
# Distribution of imputed values and observed values
isna = colnames(tmp)[apply(tmp, 2, function(x) sum(is.na(x))>1)]
length(isna)
{pdf(paste0("../Figures/PesticidesPCBBDEs_train_expo_dist_obsv_imp_RF.pdf"))
  par(mfrow = c(3, 3), oma = c(0.5,0.5,0,0.5), mar = c(2,2,2,1), mgp=c(1,0.05,0))
  for (k in 1:length(isna)){
    var = isna[k]
    print(var)
    observed = tmp_imp[which(is.na(tmp[,var])),]
    imputed = tmp_imp[-which(is.na(tmp[,var])),]
    ylim = range(density(observed[,var], na.rm = T)$y,density(imputed[,var], na.rm = T)$y)
    xlim = range(density(observed[,var], na.rm = T)$x,density(imputed[,var], na.rm = T)$x)
    plot(NULL, main=var, xlab="pg/mg", ylab ="Density",
         xlim=c(min(xlim, na.rm = T),max(xlim, na.rm = T)),
         ylim=c(min(ylim, na.rm = T),max(ylim, na.rm = T)),
         cex.main = 1,  cex.lab=0.8, cex.axis=0.8, tck=-0.01, lwd = 0.5)
    x=density(observed[,var], na.rm = T)
    lines(x, col="royalblue", lwd = 0.5)
    x=density(imputed[,var], na.rm = T)
    lines(x, col="tomato", lwd = 0.5)
  }
  dev.off()
}

tmp = log10(train_pahs[,31:ncol(train_pahs)])
tmp_imp = log10(train_pahs_imp[,31:ncol(train_pahs_imp)])
# Distribution of imputed values and observed values
isna = colnames(tmp)[apply(tmp, 2, function(x) sum(is.na(x))>1)]
length(isna)
{pdf(paste0("../Figures/PAHs_train_expo_dist_obsv_imp_RF.pdf"))
  par(mfrow = c(3, 3), oma = c(0.5,0.5,0,0.5), mar = c(2,2,2,1), mgp=c(1,0.05,0))
  for (k in 1:length(isna)){
    var = isna[k]
    print(var)
    observed = tmp_imp[which(is.na(tmp[,var])),]
    imputed = tmp_imp[-which(is.na(tmp[,var])),]
    ylim = range(density(observed[,var], na.rm = T)$y,density(imputed[,var], na.rm = T)$y)
    xlim = range(density(observed[,var], na.rm = T)$x,density(imputed[,var], na.rm = T)$x)
    plot(NULL, main=var, xlab="pg/mg", ylab ="Density",
         xlim=c(min(xlim, na.rm = T),max(xlim, na.rm = T)),
         ylim=c(min(ylim, na.rm = T),max(ylim, na.rm = T)),
         cex.main = 1,  cex.lab=0.8, cex.axis=0.8, tck=-0.01, lwd = 0.5)
    x=density(observed[,var], na.rm = T)
    lines(x, col="royalblue", lwd = 0.5)
    x=density(imputed[,var], na.rm = T)
    lines(x, col="tomato", lwd = 0.5)
  }
  dev.off()
}

tmp = log10(test[,31:ncol(test)])
tmp_imp = log10(test_imp[,31:ncol(test_imp)])
# Distribution of imputed values and observed values
isna = colnames(tmp)[apply(tmp, 2, function(x) sum(is.na(x))>1)]
length(isna)
{pdf(paste0("../Figures/Overlap_test_expo_dist_obsv_imp_RF.pdf"))
  par(mfrow = c(3, 3), oma = c(0.5,0.5,0,0.5), mar = c(2,2,2,1), mgp=c(1,0.05,0))
  for (k in 1:length(isna)){
    var = isna[k]
    print(var)
    observed = tmp_imp[which(is.na(tmp[,var])),]
    imputed = tmp_imp[-which(is.na(tmp[,var])),]
    ylim = range(density(observed[,var], na.rm = T)$y,density(imputed[,var], na.rm = T)$y)
    xlim = range(density(observed[,var], na.rm = T)$x,density(imputed[,var], na.rm = T)$x)
    plot(NULL, main=var, xlab="pg/mg", ylab ="Density",
         xlim=c(min(xlim, na.rm = T),max(xlim, na.rm = T)),
         ylim=c(min(ylim, na.rm = T),max(ylim, na.rm = T)),
         cex.main = 1,  cex.lab=0.8, cex.axis=0.8, tck=-0.01, lwd = 0.5)
    x=density(observed[,var], na.rm = T)
    lines(x, col="royalblue", lwd = 0.5)
    x=density(imputed[,var], na.rm = T)
    lines(x, col="tomato", lwd = 0.5)
  }
  dev.off()
}

