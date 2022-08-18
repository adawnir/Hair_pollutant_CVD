### Imputing missing values: Diagnostics

## Nutrient information ----
rm(list=ls())
project_path=dirname(rstudioapi::getActiveDocumentContext()$path)
setwd(project_path)

# Load data
pest = readRDS("../Data/PesticidesPCBBDEs.rds")
pahs = readRDS("../Data/PAHs.rds")
comb = readRDS("../Data/Combined.rds")

# Combine all participants
outer = rbind(pest[setdiff(pest$ID, pahs$ID),7:11],
              pahs[setdiff(pahs$ID, pest$ID),7:11],
              comb[,7:11])
colnames(outer)

outer_imp = readRDS("../Processed/All_participants_nutri_naimp.rds")

# Distribution of imputed values and observed values
isna = colnames(outer)[apply(outer, 2, function(x) sum(is.na(x))>1)]
length(isna)

{pdf(paste0("../Figures/Nutri_dist_obsv_imp_RF.pdf"))
  par(mfrow = c(3, 3), oma = c(0.5,0.5,0,0.5), mar = c(2,2,2,1), mgp=c(1,0.05,0))
  for (k in 1:length(isna)){
    var = isna[k]
    print(var)
    observed = outer_imp[which(is.na(outer[,var])),]
    imputed = outer_imp[-which(is.na(outer[,var])),]
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
rm(list=ls())
project_path=dirname(rstudioapi::getActiveDocumentContext()$path)
setwd(project_path)

# Load data
pest_expo = log10(readRDS("../Processed/PesticidesPCBBDEs_ndimp.rds"))
pahs_expo = log10(readRDS("../Processed/PAHs_ndimp.rds"))

pest_expo_imp = log10(readRDS("../Processed/PesticidesPCBBDEs_ndimp_naimp.rds"))
pahs_expo_imp = log10(readRDS("../Processed/PAHs_ndimp_naimp.rds"))

# Distribution of imputed values and observed values - Pesticides PCBs BDEs
isna = colnames(pest_expo)[apply(pest_expo, 2, function(x) sum(is.na(x))>1)]
length(isna)

{pdf(paste0("../Figures/Comp_dist_obsv_imp_PesticidesPCBBDEs_RF.pdf"))
  par(mfrow = c(3, 3), oma = c(0.5,0.5,0,0.5), mar = c(2,2,2,1), mgp=c(1,0.05,0))
  for (k in 1:length(isna)){
    var = isna[k]
    print(var)
    observed = pest_expo_imp[which(is.na(pest_expo[,var])),]
    imputed = pest_expo_imp[-which(is.na(pest_expo[,var])),]
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

# Distribution of imputed values and observed values - PAHs
isna = colnames(pahs_expo)[apply(pahs_expo, 2, function(x) sum(is.na(x))>1)]
length(isna)

{pdf(paste0("../Figures/Comp_dist_obsv_imp_PAHs_RF.pdf"))
  par(mfrow = c(1, 1), oma = c(0.5,0.5,0,0.5), mar = c(2,2,2,1), mgp=c(1,0.05,0))
  for (k in 1:length(isna)){
    var = isna[k]
    print(var)
    observed = pahs_expo_imp[which(is.na(pahs_expo[,var])),]
    imputed = pahs_expo_imp[-which(is.na(pahs_expo[,var])),]
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

