# R2GUESS

rm(list=ls())
project_path="/rds/general/user/rw1317/home/Hair_pollutant_CVD/Scripts"
setwd(project_path)


library(R2GUESS)
library(tidyverse)

## Parameters
args=commandArgs(trailingOnly=TRUE)
m=as.numeric(args[1])

filepaths = c("PesticidesPCBBDEs_train","PAHs_train")

if(m==1){
  outcome.id = c("BMI","WC","SBP","DBP","TG","LDLC","HDLC","FPG")
  egam = 5
  sgam = 5
}
if(m==2){
  outcome.id = c("BMI","WC","SBP","DBP","TG","LDLC","FPG")
  egam = 6
  sgam = 6
}

ifelse(dir.exists("../Results/"),"",dir.create("../Results/"))
ifelse(dir.exists("../Results/R2GUESS/"),"",dir.create("../Results/R2GUESS/"))
ifelse(dir.exists(paste0("../Results/R2GUESS/",filepaths[m])),"",dir.create(paste0("../Results/R2GUESS/",filepaths[m])))

ifelse(dir.exists("../Figures/"),"",dir.create("../Figures/"))
ifelse(dir.exists("../Figures/R2GUESS/"),"",dir.create("../Figures/R2GUESS/"))

## Load data
data = readRDS(paste0("../Processed/",filepaths[m],"_XY.rds"))
tmp = model.matrix(~ Smoking + Education + Physical_activity + Alcohol, data)[,-1]

Y = data[,outcome.id] # remove WC and TC
X = data[,27:ncol(data)]

# parameters
path.par = paste(system.file("extdata", package = "R2GUESS"), "/", sep = "")
nsweep = 30000
burn.in = 1000

path.output = gsub("Scripts", paste0("Results/R2GUESS/",filepaths[m]), getwd())

## f1 -----
ifelse(dir.exists(sprintf("%s/f1",path.output)),"",dir.create(sprintf("%s/f1",path.output)))
path.input = sprintf("%s/f1",path.output)
Z = cbind(data$Age,
          data$Gender,
          tmp[,1:2],
          data[,c("DietScore1","DietScore2","DietScore3")])
Z = Z %>% mutate_if(is.factor, function(x) as.integer(x)-1)

## Bayesian Variable Selection
model = R2GUESS(Y, X, path.input, path.output,
                nsweep = nsweep, burn.in = burn.in,
                root.file.output = "f1", 
                path.par = path.par,
                file.par = "Par_file_example_Hopx.xml",
                time = TRUE, top = 100, history = TRUE,
                Egam = egam, Sgam = sgam, label.Y = colnames(Y), 
                nb.chain = 3,
                conf = Z, cuda = FALSE,
                MAP.file = NULL, seed = 2)

# summary(model, 100)

set.seed(3)
n.sweep = get.sweep.best.model(model, models = 1)
res.g = get.g.sweep(model, n.sweep$result, 1)
beta = sample.beta(model, res.g, Nmonte = 4, Nmonte.sigma = 4)

saveRDS(model, paste0("../Results/R2GUESS/f1_model_",filepaths[m],".rds"))
saveRDS(res.g, paste0("../Results/R2GUESS/f1_model_res_",filepaths[m],".rds"))
saveRDS(beta, paste0("../Results/R2GUESS/f1_model_beta_",filepaths[m],".rds"))

## Diagnostic plots to assess the convergence of the algorithm
jpeg(paste0("../Figures/R2GUESS/f1_monitor_App_",filepaths[m],".jpg"), width = 20, height = 17, units = "cm", res = 300)
plot(model)
dev.off()

## Diagnostic plots to assess the convergence of the algorithm
pdf(paste0("../Figures/R2GUESS/f1_conv1_",filepaths[m],".pdf"))
check.convergence(model, 1)
dev.off()

pdf(paste0("../Figures/R2GUESS/f1_conv2_",filepaths[m],".pdf"))
check.convergence(model, 2)
dev.off()

## f3 -----
ifelse(dir.exists(sprintf("%s/f3",path.output)),"",dir.create(sprintf("%s/f3",path.output)))
path.input = sprintf("%s/f3",path.output)
Z = cbind(data$Age,
          data$Gender,
          tmp[,1:2],
          data[,c("DietScore1","DietScore2","DietScore3")],
          tmp[,3:4])
Z = Z %>% mutate_if(is.factor, function(x) as.integer(x)-1)

## Bayesian Variable Selection
model = R2GUESS(Y, X, path.input, path.output,
                nsweep = nsweep, burn.in = burn.in,
                root.file.output = "f3", 
                path.par = path.par,
                file.par = "Par_file_example_Hopx.xml",
                time = TRUE, top = 100, history = TRUE,
                Egam = egam, Sgam = sgam, label.Y = colnames(Y), 
                nb.chain = 3,
                conf = Z, cuda = FALSE,
                MAP.file = NULL, seed = 2)

# summary(model, 100)

set.seed(3)
n.sweep = get.sweep.best.model(model, models = 1)
res.g = get.g.sweep(model, n.sweep$result, 1)
beta = sample.beta(model, res.g, Nmonte = 4, Nmonte.sigma = 4)

saveRDS(model, paste0("../Results/R2GUESS/f3_model_",filepaths[m],".rds"))
saveRDS(res.g, paste0("../Results/R2GUESS/f3_model_res_",filepaths[m],".rds"))
saveRDS(beta, paste0("../Results/R2GUESS/f3_model_beta_",filepaths[m],".rds"))

## Diagnostic plots to assess the convergence of the algorithm
jpeg(paste0("../Figures/R2GUESS/f3_monitor_App_",filepaths[m],".jpg"), width = 20, height = 17, units = "cm", res = 300)
plot(model)
dev.off()

## Diagnostic plots to assess the convergence of the algorithm
pdf(paste0("../Figures/R2GUESS/f3_conv1_",filepaths[m],".pdf"))
check.convergence(model, 1)
dev.off()

pdf(paste0("../Figures/R2GUESS/f3_conv2_",filepaths[m],".pdf"))
check.convergence(model, 2)
dev.off()

## f4 -----
ifelse(dir.exists(sprintf("%s/f4",path.output)),"",dir.create(sprintf("%s/f4",path.output)))
path.input = sprintf("%s/f4",path.output)
Z = cbind(data$Age,
          data$Gender,
          tmp[,1:2],
          data[,c("DietScore1","DietScore2","DietScore3")],
          tmp[,3:ncol(tmp)])
Z = Z %>% mutate_if(is.factor, function(x) as.integer(x)-1)

## Bayesian Variable Selection
model = R2GUESS(Y, X, path.input, path.output,
                nsweep = nsweep, burn.in = burn.in,
                root.file.output = "f4", 
                path.par = path.par,
                file.par = "Par_file_example_Hopx.xml",
                time = TRUE, top = 100, history = TRUE,
                Egam = egam, Sgam = sgam, label.Y = colnames(Y), 
                nb.chain = 3,
                conf = Z, cuda = FALSE,
                MAP.file = NULL, seed = 2)

# summary(model, 100)

set.seed(3)
n.sweep = get.sweep.best.model(model, models = 1)
res.g = get.g.sweep(model, n.sweep$result, 1)
beta = sample.beta(model, res.g, Nmonte = 4, Nmonte.sigma = 4)

saveRDS(model, paste0("../Results/R2GUESS/f4_model_",filepaths[m],".rds"))
saveRDS(res.g, paste0("../Results/R2GUESS/f4_model_res_",filepaths[m],".rds"))
saveRDS(beta, paste0("../Results/R2GUESS/f4_model_beta_",filepaths[m],".rds"))

## Diagnostic plots to assess the convergence of the algorithm
jpeg(paste0("../Figures/R2GUESS/f4_monitor_App_",filepaths[m],".jpg"), width = 20, height = 17, units = "cm", res = 300)
plot(model)
dev.off()

## Diagnostic plots to assess the convergence of the algorithm
pdf(paste0("../Figures/R2GUESS/f4_conv1_",filepaths[m],".pdf"))
check.convergence(model, 1)
dev.off()

pdf(paste0("../Figures/R2GUESS/f4_conv2_",filepaths[m],".pdf"))
check.convergence(model, 2)
dev.off()
