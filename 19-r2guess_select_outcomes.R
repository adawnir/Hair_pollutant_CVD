# R2GUESS

rm(list=ls())
project_path="/rds/general/user/rw1317/home/Hair_pollutant_CVD/Scripts"
setwd(project_path)


library(R2GUESS)
library(tidyverse)

## Parameters
args=commandArgs(trailingOnly=TRUE)
m=as.numeric(args[1])

ifelse(dir.exists("../Processed/R2GUESS/"),"",dir.create("../Processed/R2GUESS/"))
ifelse(dir.exists("../Results/"),"",dir.create("../Results/"))
ifelse(dir.exists("../Results/R2GUESS/"),"",dir.create("../Results/R2GUESS/"))

## Load data
filepaths = c("PesticidesPCBBDEs_train","PAHs_train")
data = readRDS(paste0("../Processed/",filepaths[m],"_XY.rds"))
tmp = model.matrix(~ Smoking + Education + Physical_activity + Alcohol, data)[,-1]

Y = data[,c(17,19,20,22:25)] # remove WC and TC
X = data[,27:ncol(data)]

# parameters
path.par = paste(system.file("extdata", package = "R2GUESS"), "/", sep = "")
nsweep = 30000
burn.in = 1000

## All outcomes f1 5, 5 -----
ifelse(dir.exists("../Processed/R2GUESS/Select-outcomes-f1-5-5/"),"",dir.create("../Processed/R2GUESS/Select-outcomes-f1-5-5/"))
ifelse(dir.exists("../Results/R2GUESS/Select-outcomes-f1-5-5/"),"",dir.create("../Results/R2GUESS/Select-outcomes-f1-5-5/"))
path.output = gsub("Scripts", "Results/R2GUESS/Select-outcomes-f1-5-5", getwd())
path.input = gsub("Scripts", "Processed/R2GUESS/Select-outcomes-f1-5-5", getwd())

Z = cbind(data$Age,
          data$Gender,
          tmp[,1:2],
          data[,c("DietScore1","DietScore2","DietScore3")])
Z = Z %>% mutate_if(is.factor, function(x) as.integer(x)-1)

## Bayesian Variable Selection
model = R2GUESS(Y, X, path.input, path.output,
                nsweep = nsweep, burn.in = burn.in,
                root.file.output = "Select-outcomes-f1-5-5", 
                path.par = path.par,
                file.par = "Par_file_example_Hopx.xml",
                time = TRUE, top = 100, history = TRUE,
                Egam = 5, Sgam = 5, label.Y = colnames(Y), 
                nb.chain = 3,
                conf = Z, cuda = FALSE,
                MAP.file = NULL, seed = 2)

# summary(model, 100)

set.seed(3)
n.sweep = get.sweep.best.model(model, models = 1)
res.g = get.g.sweep(model, n.sweep$result, 1)
beta = sample.beta(model, res.g, Nmonte = 4, Nmonte.sigma = 4)

## Code for running permutation to get MPPI cut-off value which controls the
## empirical FDR
perm.input = gsub("Scripts", "Results/R2GUESS/Select-outcomes-f1-5-5/perm_input", getwd())
perm.output = gsub("Scripts", "Results/R2GUESS/Select-outcomes-f1-5-5/perm_output", getwd())

FDRthresh = FDR.permutation(x = model,
                            path.input = perm.input,
                            path.output = perm.output,
                            Npermut = 50, nbcpu = 5)

saveRDS(model, paste0("..Results/R2GUESS/Select_outcomes_f1_5_5_model_",filepaths[m],".rds"))
saveRDS(FDRthresh, paste0("..Results/R2GUESS/Select_outcomes_f1_5_5_model_FDRthresh_",filepaths[m],".rds"))
saveRDS(res.g, paste0("..Results/R2GUESS/Select_outcomes_f1_5_5_model_res_",filepaths[m],".rds"))
saveRDS(beta, paste0("..Results/R2GUESS/Select_outcomes_f1_5_5_model_beta_",filepaths[m],".rds"))

ifelse(dir.exists("../Figures/R2GUESS/"),"",dir.create("../Figures/R2GUESS/"))

## Diagnostic plots to assess the convergence of the algorithm
jpeg(paste0("../Figures/R2GUESS/Select_outcomes_f1_5_5_monitor_App_",filepaths[m],".jpg"), width = 20, height = 17, units = "cm", res = 300)
plot(model)
dev.off()

## Diagnostic plots to assess the convergence of the algorithm
pdf(paste0("../Figures/R2GUESS/Select_outcomes_f1_5_5_conv1_",filepaths[m],".pdf"))
check.convergence(model, 1)
dev.off()

pdf(paste0("../Figures/R2GUESS/Select_outcomes_f1_5_5_conv2_",filepaths[m],".pdf"))
check.convergence(model, 2)
dev.off()

## All outcomes f3 5, 5 -----
ifelse(dir.exists("../Processed/R2GUESS/Select-outcomes-f3-5-5/"),"",dir.create("../Processed/R2GUESS/Select-outcomes-f3-5-5/"))
ifelse(dir.exists("../Results/R2GUESS/Select-outcomes-f3-5-5/"),"",dir.create("../Results/R2GUESS/Select-outcomes-f3-5-5/"))
path.output = gsub("Scripts", "Results/R2GUESS/Select-outcomes-f3-5-5", getwd())
path.input = gsub("Scripts", "Processed/R2GUESS/Select-outcomes-f3-5-5", getwd())

Z = cbind(data$Age,
             data$Gender,
             tmp[,1:2],
             data[,c("DietScore1","DietScore2","DietScore3")],
             tmp[,3:4])
Z = Z %>% mutate_if(is.factor, function(x) as.integer(x)-1) %>% as.matrix()

## Bayesian Variable Selection
model = R2GUESS(Y, X, path.input, path.output,
                nsweep = nsweep, burn.in = burn.in,
                root.file.output = "Select-outcomes-f3-5-5", 
                path.par = path.par,
                file.par = "Par_file_example_Hopx.xml",
                time = TRUE, top = 100, history = TRUE,
                Egam = 5, Sgam = 5, label.Y = colnames(Y), 
                nb.chain = 3,
                conf = Z, cuda = FALSE,
                MAP.file = NULL, seed = 2)

# summary(model, 100)

set.seed(3)
n.sweep = get.sweep.best.model(model, models = 1)
res.g = get.g.sweep(model, n.sweep$result, 1)
beta = sample.beta(model, res.g, Nmonte = 2, Nmonte.sigma = 2)

## Code for running permutation to get MPPI cut-off value which controls the
## empirical FDR
perm.input = gsub("Scripts", "Results/R2GUESS/Select-outcomes-f3-5-5/perm_input", getwd())
perm.output = gsub("Scripts", "Results/R2GUESS/Select-outcomes-f3-5-5/perm_output", getwd())

FDRthresh = FDR.permutation(x = model,
                            path.input = perm.input,
                            path.output = perm.output,
                            Npermut = 50, nbcpu = 5)

saveRDS(model, paste0("..Results/R2GUESS/Select_outcomes_f3_5_5_model_",filepaths[m],".rds"))
saveRDS(FDRthresh, paste0("..Results/R2GUESS/Select_outcomes_f3_5_5_model_FDRthresh_",filepaths[m],".rds"))
saveRDS(res.g, paste0("..Results/R2GUESS/Select_outcomes_f3_5_5_model_res_",filepaths[m],".rds"))
saveRDS(beta, paste0("..Results/R2GUESS/Select_outcomes_f3_5_5_model_beta_",filepaths[m],".rds"))

ifelse(dir.exists("../Figures/"),"",dir.create("../Figures/"))
ifelse(dir.exists("../Figures/R2GUESS/"),"",dir.create("../Figures/R2GUESS/"))

## Diagnostic plots to assess the convergence of the algorithm
jpeg(paste0("../Figures/R2GUESS/Select_outcomes_f3_5_5_monitor_App_",filepaths[m],".jpg"), width = 20, height = 17, units = "cm", res = 300)
plot(model)
dev.off()

## Diagnostic plots to assess the convergence of the algorithm
pdf(paste0("../Figures/R2GUESS/Select_outcomes_f3_5_5_conv1_",filepaths[m],".pdf"))
check.convergence(model, 1)
dev.off()

pdf(paste0("../Figures/R2GUESS/Select_outcomes_f3_5_5_conv2_",filepaths[m],".pdf"))
check.convergence(model, 2)
dev.off()

## All outcomes f4 5, 5 -----
ifelse(dir.exists("../Processed/R2GUESS/Select-outcomes-f4-5-5/"),"",dir.create("../Processed/R2GUESS/Select-outcomes-f4-5-5/"))
ifelse(dir.exists("../Results/R2GUESS/Select-outcomes-f4-5-5/"),"",dir.create("../Results/R2GUESS/Select-outcomes-f4-5-5/"))
path.output = gsub("Scripts", "Results/R2GUESS/Select-outcomes-f4-5-5", getwd())
path.input = gsub("Scripts", "Processed/R2GUESS/Select-outcomes-f4-5-5", getwd())

Z = cbind(data$Age,
             data$Gender,
             tmp[,1:2],
             data[,c("DietScore1","DietScore2","DietScore3")],
             tmp[,3:ncol(tmp)])
Z = Z %>% mutate_if(is.factor, function(x) as.integer(x)-1) %>% as.matrix()

## Bayesian Variable Selection
model = R2GUESS(Y, X, path.input, path.output,
                nsweep = nsweep, burn.in = burn.in,
                root.file.output = "Select-outcomes-f4-5-5", 
                path.par = path.par,
                file.par = "Par_file_example_Hopx.xml",
                time = TRUE, top = 100, history = TRUE,
                Egam = 5, Sgam = 5, label.Y = colnames(Y), 
                nb.chain = 3,
                conf = Z, cuda = FALSE,
                MAP.file = NULL, seed = 2)

# summary(model, 100)

set.seed(3)
n.sweep = get.sweep.best.model(model, models = 1)
res.g = get.g.sweep(model, n.sweep$result, 1)
beta = sample.beta(model, res.g, Nmonte = 2, Nmonte.sigma = 2)

## Code for running permutation to get MPPI cut-off value which controls the
## empirical FDR
perm.input = gsub("Scripts", "Results/R2GUESS/Select-outcomes-f4-5-5/perm_input", getwd())
perm.output = gsub("Scripts", "Results/R2GUESS/Select-outcomes-f4-5-5/perm_output", getwd())

FDRthresh = FDR.permutation(x = model,
                            path.input = perm.input,
                            path.output = perm.output,
                            Npermut = 50, nbcpu = 5)

saveRDS(model, paste0("..Results/R2GUESS/Select_outcomes_f4_5_5_model_",filepaths[m],".rds"))
saveRDS(FDRthresh, paste0("..Results/R2GUESS/Select_outcomes_f4_5_5_model_FDRthresh_",filepaths[m],".rds"))
saveRDS(res.g, paste0("..Results/R2GUESS/Select_outcomes_f4_5_5_model_res_",filepaths[m],".rds"))
saveRDS(beta, paste0("..Results/R2GUESS/Select_outcomes_f4_5_5_model_beta_",filepaths[m],".rds"))

ifelse(dir.exists("../Figures/R2GUESS/"),"",dir.create("../Figures/R2GUESS/"))

## Diagnostic plots to assess the convergence of the algorithm
jpeg(paste0("../Figures/R2GUESS/Select_outcomes_f4_5_5_monitor_App_",filepaths[m],".jpg"), width = 20, height = 17, units = "cm", res = 300)
plot(model)
dev.off()

## Diagnostic plots to assess the convergence of the algorithm
pdf(paste0("../Figures/R2GUESS/Select_outcomes_f4_5_5_conv1_",filepaths[m],".pdf"))
check.convergence(model, 1)
dev.off()

pdf(paste0("../Figures/R2GUESS/Select_outcomes_f4_5_5_conv2_",filepaths[m],".pdf"))
check.convergence(model, 2)
dev.off()



