### Distribution of exposures

rm(list=ls())
project_path=dirname(rstudioapi::getActiveDocumentContext()$path)
setwd(project_path)

# Load data
pest = readRDS("../Data/PesticidesPCBBDEs.rds")
pahs = readRDS("../Data/PAHs.rds")

pest_expo_imp = readRDS("../Processed/PesticidesPCBBDEs_ndimp_naimp.rds")
pahs_expo_imp = readRDS("../Processed/PAHs_ndimp_naimp.rds")

# Distribution of exposures stratified by country

{pdf(paste0("../Figures/Comp_dist_ndimp_naimp_PesticidesPCBBDEs.pdf"))
  par(mfrow = c(4, 3), oma = c(0.5,0.5,0,0.5), mar = c(2,2,2,1), mgp=c(1,0.05,0))
  for (k in 1:ncol(pest_expo_imp)){
    var = colnames(pest_expo_imp)[k]
    print(var)
    x0 = density(pest_expo_imp[pest$Country == "Belgium", k], na.rm = T)
    x1 = density(pest_expo_imp[pest$Country == "Luxembourg", k], na.rm = T)
    x2 = density(pest_expo_imp[pest$Country == "France", k], na.rm = T)
    plot(NULL, xlab = "pg/mg", main = var, ylab="Density",
         cex.main = 1,  cex.lab=0.8, cex.axis=0.8, tck=-0.01,
         ylim = range(c(x0$y, x1$y, x2$y)), xlim = range(c(x0$x, x1$x,x2$x)))
    lines(x0, col = "forestgreen")
    lines(x1, col = "tomato")
    lines(x2, col = "royalblue")
  }
  dev.off()
}

{pdf(paste0("../Figures/Comp_dist_ndimp_naimp_PAHs.pdf"))
  par(mfrow = c(4, 3), oma = c(0.5,0.5,0,0.5), mar = c(2,2,2,1), mgp=c(1,0.05,0))
  for (k in 1:ncol(pahs_expo_imp)){
    var = colnames(pahs_expo_imp)[k]
    print(var)
    x0 = density(pahs_expo_imp[pahs$Country == "Belgium", k], na.rm = T)
    x1 = density(pahs_expo_imp[pahs$Country == "Luxembourg", k], na.rm = T)
    plot(NULL, xlab = "pg/mg", main = var, ylab="Density",
         cex.main = 1,  cex.lab=0.8, cex.axis=0.8, tck=-0.01,
         ylim = range(c(x0$y, x1$y, x2$y)), xlim = range(c(x0$x, x1$x,x2$x)))
    lines(x0, col = "forestgreen")
    lines(x1, col = "tomato")
  }
  dev.off()
}

