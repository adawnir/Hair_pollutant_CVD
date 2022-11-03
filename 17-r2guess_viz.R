# R2GUESS

rm(list=ls())
project_path=dirname(rstudioapi::getActiveDocumentContext()$path)
setwd(project_path)


library(R2GUESS)
library(sgPLS)

source("graph_colours.R")
library(colorspace)

## Load data
myfiles = list.files(path = "../Results/R2GUESS/", pattern = "Model")

for (m in 1:length(myfiles)){
  model = readRDS(paste0("../Results/R2GUESS/",myfiles[m]))
  data_type = gsub(".rds","",sub("^Model[^_]*_","",myfiles[m]))
  X = readRDS(paste0("../Processed/",data_type,"_expo_ndimp_naimp_log.rds"))
  
  ## Visualisation ----
  annot = readRDS("../Data/Compound_family_annotation.rds")
  annot_sub = annot[colnames(X)]
  
  # Extract MPPI
  NameMarg = file.path(model$path.output, paste(model$root.file.output,"output_marg_prob_incl.txt",sep="_"))
  Marg  = read.table(NameMarg,header=TRUE)
  Marg$Predictor = colnames(X)
  
  ## MPPI plots
  pdf(paste0("../Figures/R2GUESS/MPPI_",gsub(".rds",".pdf",myfiles[m])), width = 9, height = 6)
  par(mar=c(13,5,5,3))
  plot(Marg$Marg_Prob_Incl, type = "h", ylim = c(0,1),
       xaxt = "n", lwd=2,
       xlab = "", ylab = "Marginal Posterior Probability of Inclusion (MPPI)", cex.lab=1.2)
  abline(h = 0.1, col = "darkred", lty = 2)
  coord = par("usr")
  text(coord[2], 0.1, "MPPI = 0.1", col = "darkred", adj = c(1,1), cex = 0.8)
  for (i in 1:nrow(Marg)){
    axis(side=1, at=i, labels=Marg$Predictor[i],
         col.axis = ifelse(Marg$Marg_Prob_Incl[i]>0.1,darken(annot.colours[annot_sub[i]],0.5),"black"),
         las=2, cex.axis = 0.8)
  }
  dev.off() 
}


## Boxplots summarising the posterior distribution of the
## regression coefficients for selected variables
## Blue crosses represent the true value used in the simulation.
pdf(paste0("../Figures/R2GUESS/",model_type,"_Estimated_beta_",data_type,".pdf"), width = 9, height = 5)
for(i in 1:length(res.g$predictors)){
  res.boxplot = boxplotbeta(model, beta, variable = res.g$predictors[i])
}
dev.off()

write.table(colnames(X), paste0(model$path.input,"/X.annotation.txt"))

## Figure 14-(a): variables included in the best models
pdf(paste0("../Figures/R2GUESS/",model_type,"_Variables_included_top20_models_",data_type,".pdf"))
plotvariable(model, 20, file.annotation = paste0(model$path.input,"/X.annotation.txt"))
dev.off()
## Figure 14-(b): proximity between the best models
pdf(paste0("../Figures/R2GUESS/",model_type,"_Proximity_between_top20_models_",data_type,".pdf"))
plotmodel(model, 20)
dev.off()

## Recalibration ----
data = readRDS(paste0("../Processed/Overlap_test_XY.rds"))

Y_test = data[,c(17,19,20,22:25)] # remove WC and TC

tmp = model.matrix(~ Smoking + Education + Physical_activity + Alcohol, data)[,-1]
X_test = cbind(data$Age,
               data$Gender,
               tmp[,1:2],
               data[,c("DietScore1","DietScore2","DietScore3")],
               data[,as.numeric(res.g$predictors)+26])
X_test = X_test %>% mutate_if(is.factor, function(x) as.integer(x)-1)

train = readRDS(paste0("../Processed/",filepaths[m],"_XY.rds"))
Y_train = train[,c(17,19,20,22:25)] # remove WC and TC

tmp = model.matrix(~ Smoking + Education + Physical_activity + Alcohol, train)[,-1]
X_train = cbind(train$Age,
                train$Gender,
                tmp[,1:2],
                train[,c("DietScore1","DietScore2","DietScore3")],
                train[,c(27:ncol(train))])
X_train = X_train %>% mutate_if(is.factor, function(x) as.integer(x)-1)

colnames(X_train)[1:7] = colnames(X_test)[1:7]

# Recalibration
mypls = pls(X_train, Y_train, ncomp=1)
Y_pred=predict(mypls, newdata=X_test)
myloadings = mypls$loadings$X
residuals = Y_test-Y_pred
rmse = sqrt(crossprod(residuals)/length(residuals))

## f3 -----
model = readRDS("..Results/R2GUESS/Select_outcomes_f3_model_",filepaths[m],".rds")
res.g = readRDS("..Results/R2GUESS/Select_outcomes_f3_model_res_",filepaths[m],".rds")
beta = readRDS("..Results/R2GUESS/Select_outcomes_f3_model_beta_",filepaths[m],".rds")

annot = readRDS("../Data/Compound_family_annotation.rds")
annot_sub = annot[colnames(X)]

## MPPI plots
pdf(paste0("../Figures/R2GUESS/Selected_outcomes_f3_MPPI_",filepaths[m],".pdf"), width = 9, height = 5)
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
pdf(paste0("../Figures/R2GUESS/Selected_outcomes_f3_Estimated_beta_",filepaths[m],".pdf"), width = 9, height = 5)
for(i in 1:length(res.g$predictors)){
  res.boxplot = boxplotbeta(model, beta, variable = res.g$predictors)
}
dev.off()

## Figure 14-(a): variables included in the best models
pdf(paste0("../Figures/R2GUESS/Selected_outcomes_f3_Variables_included_top20_models_",filepaths[m],".pdf"))
plotvariable(model, 20)
dev.off()
## Figure 14-(b): proximity between the best models
pdf(paste0("../Figures/R2GUESS/Selected_outcomes_f3_Proximity_between_top20_models_",filepaths[m],".pdf"))
plotmodel(model, 20)
dev.off()


## f4 -----
model = readRDS("..Results/R2GUESS/Select_outcomes_f4_model_",filepaths[m],".rds")
res.g = readRDS("..Results/R2GUESS/Select_outcomes_f4_model_res_",filepaths[m],".rds")
beta = readRDS("..Results/R2GUESS/Select_outcomes_f4_model_beta_",filepaths[m],".rds")

annot = readRDS("../Data/Compound_family_annotation.rds")
annot_sub = annot[colnames(X)]

## MPPI plots
pdf(paste0("../Figures/R2GUESS/Selected_outcomes_f4_MPPI_",filepaths[m],".pdf"), width = 9, height = 5)
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
pdf(paste0("../Figures/R2GUESS/Selected_outcomes_f4_Estimated_beta_",filepaths[m],".pdf"), width = 9, height = 5)
for(i in 1:length(res.g$predictors)){
  res.boxplot = boxplotbeta(model, beta, variable = res.g$predictors)
}
dev.off()

## Figure 14-(a): variables included in the best models
pdf(paste0("../Figures/R2GUESS/Selected_outcomes_f4_Variables_included_top20_models_",filepaths[m],".pdf"))
plotvariable(model, 20)
dev.off()
## Figure 14-(b): proximity between the best models
pdf(paste0("../Figures/R2GUESS/Selected_outcomes_f4_Proximity_between_top20_models_",filepaths[m],".pdf"))
plotmodel(model, 20)
dev.off()