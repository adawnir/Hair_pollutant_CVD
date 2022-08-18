### Distribution of exposures

rm(list=ls())
project_path=dirname(rstudioapi::getActiveDocumentContext()$path)
setwd(project_path)

# Load data
pest = readRDS("../Data/PesticidesPCBBDEs.rds")
pahs = readRDS("../Data/PAHs.rds")

pest_expo = readRDS("../Processed/PesticidesPCBBDEs_ndimp_naimp.rds")
pahs_expo = readRDS("../Processed/PAHs_ndimp_naimp.rds")

pest_expo_log = readRDS("../Processed/PesticidesPCBBDEs_ndimp_naimp_log.rds")
pahs_expo_log = readRDS("../Processed/PAHs_ndimp_naimp_log.rds")

# Before log transformation: Distribution of exposures stratified by country
{pdf(paste0("../Figures/Comp_dist_ndimp_naimp_PesticidesPCBBDEs.pdf"))
  par(mfrow = c(4, 3), oma = c(0.5,0.5,0,0.5), mar = c(2,2,2,1), mgp=c(1,0.05,0))
  for (k in 1:ncol(pest_expo)){
    var = colnames(pest_expo)[k]
    print(var)
    x0 = density(pest_expo[pest$Country == "Belgium", k], na.rm = T)
    x1 = density(pest_expo[pest$Country == "Luxembourg", k], na.rm = T)
    plot(NULL, xlab = "pg/mg", main = var, ylab="Density",
         cex.main = 1,  cex.lab=0.8, cex.axis=0.8, tck=-0.01,
         ylim = range(c(x0$y, x1$y)), xlim = range(c(x0$x, x1$x)))
    lines(x0, col = "forestgreen")
    lines(x1, col = "tomato")
  }
  dev.off()
}

{pdf(paste0("../Figures/Comp_dist_ndimp_naimp_PAHs.pdf"))
  par(mfrow = c(4, 3), oma = c(0.5,0.5,0,0.5), mar = c(2,2,2,1), mgp=c(1,0.05,0))
  for (k in 1:ncol(pahs_expo)){
    var = colnames(pahs_expo)[k]
    print(var)
    x0 = density(pahs_expo[pahs$Country == "Belgium", k], na.rm = T)
    x1 = density(pahs_expo[pahs$Country == "Luxembourg", k], na.rm = T)
    plot(NULL, xlab = "pg/mg", main = var, ylab="Density",
         cex.main = 1,  cex.lab=0.8, cex.axis=0.8, tck=-0.01,
         ylim = range(c(x0$y, x1$y)), xlim = range(c(x0$x, x1$x)))
    lines(x0, col = "forestgreen")
    lines(x1, col = "tomato")
  }
  dev.off()
}


# After log transformation: Distribution of exposures stratified by country
{pdf(paste0("../Figures/Comp_dist_ndimp_naimp_log_PesticidesPCBBDEs.pdf"))
  par(mfrow = c(4, 3), oma = c(0.5,0.5,0,0.5), mar = c(2,2,2,1), mgp=c(1,0.05,0))
  for (k in 1:ncol(pest_expo_log)){
    var = colnames(pest_expo_log)[k]
    print(var)
    x0 = density(pest_expo_log[pest$Country == "Belgium", k], na.rm = T)
    x1 = density(pest_expo_log[pest$Country == "Luxembourg", k], na.rm = T)
    plot(NULL, xlab = expression(log[10](pg/mg)), main = var, ylab="Density",
         cex.main = 1,  cex.lab=0.8, cex.axis=0.8, tck=-0.01,
         ylim = range(c(x0$y, x1$y)), xlim = range(c(x0$x, x1$x)))
    lines(x0, col = "forestgreen")
    lines(x1, col = "tomato")
  }
  dev.off()
}

{pdf(paste0("../Figures/Comp_dist_ndimp_naimp_log_PAHs.pdf"))
  par(mfrow = c(4, 3), oma = c(0.5,0.5,0,0.5), mar = c(2,2,2,1), mgp=c(1,0.05,0))
  for (k in 1:ncol(pahs_expo_log)){
    var = colnames(pahs_expo_log)[k]
    print(var)
    x0 = density(pahs_expo_log[pahs$Country == "Belgium", k], na.rm = T)
    x1 = density(pahs_expo_log[pahs$Country == "Luxembourg", k], na.rm = T)
    plot(NULL, xlab = expression(log[10](pg/mg)), main = var, ylab="Density",
         cex.main = 1,  cex.lab=0.8, cex.axis=0.8, tck=-0.01,
         ylim = range(c(x0$y, x1$y)), xlim = range(c(x0$x, x1$x)))
    lines(x0, col = "forestgreen")
    lines(x1, col = "tomato")
  }
  dev.off()
}

