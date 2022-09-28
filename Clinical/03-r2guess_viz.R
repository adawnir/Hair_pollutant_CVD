# R2GUESS

rm(list=ls())
project_path=dirname(rstudioapi::getActiveDocumentContext()$path)
setwd(project_path)
setwd("../")


library(R2GUESS)

source("graph_colours.R")
library(colorspace)

## Load data
filepaths = c("PesticidesPCBBDEs_train","PAHs_train")

data = readRDS(paste0("../Processed/",filepaths[m],"_XY_Clinical.rds"))
X = data[,-c(1:17)]

model = readRDS("..Results/Clinical/",filepaths[m],"_R2GUESS_model.rds")
FDRthresh = readRDS("..Results/Clinical/",filepaths[m],"_R2GUESS_model_FDRthresh.rds")
res.g = readRDS("..Results/Clinical/",filepaths[m],"_R2GUESS_model_res.rds")
beta = readRDS("..Results/Clinical/",filepaths[m],"_R2GUESS_model_beta.rds")

annot = readRDS("../Data/Compound_family_annotation.rds")
annot_sub = annot[colnames(X)]

## MPPI plots
pdf("../Figures/Clinical/MPPI_",filepaths[m],".pdf", width = 9, height = 5)
par(mar=c(5,5,5,10))
plotMPPI(model, threshold.model = 100, cutoff = FALSE)
legend("topright",
       inset = c(-0.25,0),
       legend = paste0(as.numeric(res.g$predictors),". ",
                       colnames(X)[as.numeric(res.g$predictors)]),
       text.col = darken(annot.colours[annot_sub[as.numeric(res.g$predictors)]],0.5),
       xpd = NA, x.intersp = 0, bty = "n")
dev.off()

## Boxplots summarising the posterior distribution of the
## regression coefficients for selected variables
## Blue crosses represent the true value used in the simulation.
pdf(paste0("../Figures/Clinical/Estimated_beta_",filepaths[m],".pdf"), width = 9, height = 5)
par(mfrow = c(1, 4))
res.boxplot = boxplotbeta(model, beta, variable = "12")
res.boxplot = boxplotbeta(model, beta, variable = "19")
res.boxplot = boxplotbeta(model, beta, variable = "21")
res.boxplot = boxplotbeta(model, beta, variable = "29")
dev.off()

## Plot of the Marginal Posterior Probabilities of Inclusion (MPPIs)
pdf("../Figures/Clinical/MPPI_application_",filepaths[m],".pdf", width = 9, height = 5)
plotMPPI(model, threshold.variable = FDRthresh$cutoff_int)
dev.off()
## Figure 14-(a): variables included in the best models
pdf("../Figures/Clinical/Variables_included_top20_models_",filepaths[m],".pdf")
plotvariable(model, 20)
dev.off()
## Figure 14-(b): proximity between the best models
pdf("../Figures/Clinical/Proximity_between_top20_models_",filepaths[m],".pdf")
plotmodel(model, 20)
dev.off()