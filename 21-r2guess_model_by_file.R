# R2GUESS

rm(list=ls())
project_path="/rds/general/user/rw1317/home/Hair_pollutant_CVD/Scripts"
setwd(project_path)


library(R2GUESS)
library(tidyverse)

## Parameters
args=commandArgs(trailingOnly=TRUE)
m=as.numeric(args[1])

myfiles = list.files(path = "../Results/R2GUESS/", pattern = "sPLS_XY_f[0-9]_output_")

split = gsub("sPLS_XY_f[0-9]_output_","",myfiles[m])
data_type = gsub(".rds","",split)
model_type = regmatches(myfiles[m], regexpr("f[1-4]", myfiles[m]))

# Confounders
if(model_type == "f1"){
  confounder.id = 1:7
} else if (model_type == "f3"){
  confounder.id = 1:9
} else {
  confounder.id = 1:13
}

# Outcomes and priors
if(data_type == "PesticidesPCBBDEs_train"){
  outcome.id = c("WC","SBP","DBP","TG","BMI","TC","LDLC")
  egam = 6
  sgam = 6
} else {
  outcome.id = c("BMI","WC")
  egam = 6
  sgam = 6
}

ifelse(dir.exists("../Results/"),"",dir.create("../Results/"))
ifelse(dir.exists("../Results/R2GUESS/"),"",dir.create("../Results/R2GUESS/"))
ifelse(dir.exists(paste0("../Results/R2GUESS/",data_type)),"",dir.create(paste0("../Results/R2GUESS/",data_type)))

ifelse(dir.exists("../Figures/"),"",dir.create("../Figures/"))
ifelse(dir.exists("../Figures/R2GUESS/"),"",dir.create("../Figures/R2GUESS/"))

## Load data
data = readRDS(paste0("../Processed/",data_type,"_XY.rds"))
tmp = model.matrix(~ Smoking + Education + Physical_activity + Alcohol, data)[,-1]

Y = data[,outcome.id]
X = data[,27:ncol(data)]
Z = cbind(data$Age,
          data$Gender,
          tmp[,1:2],
          data[,c("DietScore1","DietScore2","DietScore3")],
          tmp[,3:ncol(tmp)])
Z = Z %>% mutate_if(is.factor, function(x) as.integer(x)-1)
Z = Z[,confounder.id]


# parameters
path.par = paste(system.file("extdata", package = "R2GUESS"), "/", sep = "")
nsweep = 30000
burn.in = 1000

path.output = gsub("Scripts", paste0("Results/R2GUESS/",data_type), getwd())
ifelse(dir.exists(sprintf(paste0("%s/",model_type),path.output)),"",dir.create(sprintf(paste0("%s/",model_type),path.output)))
path.input = sprintf(paste0("%s/",model_type),path.output)

## Bayesian Variable Selection
model = R2GUESS(Y, X, path.input, path.output,
                nsweep = nsweep, burn.in = burn.in,
                root.file.output = model_type, 
                path.par = path.par,
                file.par = "Par_file_example_Hopx.xml",
                time = TRUE, top = 100, history = TRUE,
                Egam = egam, Sgam = sgam, label.Y = colnames(Y), 
                nb.chain = 3,
                conf = Z, cuda = FALSE,
                MAP.file = NULL, seed = 2)

# summary(model, 100)

saveRDS(model, paste0("../Results/R2GUESS/",model_type,"_model_",data_type,".rds"))

set.seed(3)
n.sweep = get.sweep.best.model(model, models = 1)
res.g = get.g.sweep(model, n.sweep$result, 1) # error
beta = sample.beta(model, res.g, Nmonte = 4, Nmonte.sigma = 4)

saveRDS(res.g, paste0("../Results/R2GUESS/",model_type,"_model_res_",data_type,".rds"))
saveRDS(beta, paste0("../Results/R2GUESS/",model_type,"_model_beta_",data_type,".rds"))

## Diagnostic plots to assess the convergence of the algorithm
jpeg(paste0("../Figures/R2GUESS/",model_type,"_monitor_App_",data_type,".jpg"), width = 20, height = 17, units = "cm", res = 300)
plot(model)
dev.off()

## Diagnostic plots to assess the convergence of the algorithm
pdf(paste0("../Figures/R2GUESS/",model_type,"_conv1_",data_type,".pdf"))
check.convergence(model, 1)
dev.off()

pdf(paste0("../Figures/R2GUESS/",model_type,"_conv2_",data_type,".pdf"))
check.convergence(model, 2)
dev.off()
