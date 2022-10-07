# sPLS for selecting outcomes and prior for R2GUESS

rm(list=ls())
project_path="/rds/general/user/rw1317/home/Hair_pollutant_CVD/Scripts"
setwd(project_path)


library(sharp)
library(tidyverse)

## Parameters
args=commandArgs(trailingOnly=TRUE)
m=as.numeric(args[1])

filepaths = c("PesticidesPCBBDEs_train","PAHs_train")

ifelse(dir.exists("../Results/"),"",dir.create("../Results/"))
ifelse(dir.exists("../Results/R2GUESS/"),"",dir.create("../Results/R2GUESS/"))

ifelse(dir.exists("../Figures/"),"",dir.create("../Figures/"))
ifelse(dir.exists("../Figures/R2GUESS/"),"",dir.create("../Figures/R2GUESS/"))

## Load data
data = readRDS(paste0("../Processed/",filepaths[m],"_XY.rds"))
Y = data[,c(17:25)]
X = data[,27:ncol(data)]

tmp = model.matrix(~ Smoking + Education + Physical_activity + Alcohol, data)[,-1]
Z = cbind(data$Age,
          data$Gender,
          tmp[,1:2],
          data[,c("DietScore1","DietScore2","DietScore3")],
          tmp[,3:ncol(tmp)])
Z = Z %>% mutate_if(is.factor, function(x) as.integer(x)-1)

f1='X[,k] ~ data$Age + data$Gender + `SmokingFormer smoker` + `SmokingCurrent smoker` + DietScore1 + DietScore2 + DietScore3'
f3='X[,k] ~ data$Age + data$Gender + `SmokingFormer smoker` + `SmokingCurrent smoker` + DietScore1 + DietScore2 + DietScore3 + EducationIntermediate + EducationHigh'
f4='X[,k] ~ data$Age + data$Gender + `SmokingFormer smoker` + `SmokingCurrent smoker` + DietScore1 + DietScore2 + DietScore3 + EducationIntermediate + EducationHigh + `Physical_activityModerately active` + Physical_activityActive + AlcoholModerate + AlcoholHigh'

ncomp = c(3,2)[m]

for (f in c(1,3,4)){
  X_denoised=NULL
  for (k in seq(1:ncol(X))){
    print(colnames(X)[k])
    formula = eval(parse(text = paste0("f",f)))
    model=lm(as.formula(formula), data=Z)
    X_denoised = cbind(X_denoised, model$residuals)
  }
  colnames(X_denoised)=colnames(X)
  rownames(X_denoised)=rownames(X)
  
  # sPLS: sparsity on both X and Y
  stab = BiSelection(xdata = X_denoised, ydata = Y, family = "gaussian", ncomp = ncomp,
                     LambdaX = 1:(ncol(X) - 1),
                     LambdaY = 1:(ncol(Y) - 1),
                     implementation = SparsePLS, n_cat = 2)
  saveRDS(stab, paste0("../Results/R2GUESS/sPLS_XY_f",f,"_output_",filepaths[m],".rds"))
  
  pdf(paste0("../Figures/R2GUESS/sPLS_XY_f",f,"_output_",filepaths[m],".pdf"))
  par(mar=c(5,5,1,1))
  CalibrationPlot(stab)
  dev.off()
  
  pdf(paste0("../Figures/R2GUESS/sPLS_XY_f",f,"_network_",filepaths[m],".pdf"))
  plot(stab)
  dev.off()
  
  print(summary(stab))
  
  print(stab$selectedY)
}

