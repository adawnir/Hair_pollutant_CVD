# R2GUESS

rm(list=ls())
project_path=dirname(rstudioapi::getActiveDocumentContext()$path)
setwd(project_path)
setwd("../")


library(R2GUESS)

## Parameters
args=commandArgs(trailingOnly=TRUE)
m=as.numeric(args[1])

## Load data
filepaths = c("PesticidesPCBBDEs_train","PAHs_train")


data = readRDS(paste0("../Processed/",filepaths[m],"_XY_Clinical.rds"))

X = data[,-c(1:17)]
genderfemale = ifelse(conf$Gender == "Female", 1, 0)
formersmoker = ifelse(conf$Smoking == "Former smoker", 1, 0)
currentsmoker = ifelse(conf$Smoking == "Current smoker", 1, 0)
conf = cbind(data$Age,
             genderfemale,
             formersmoker,
             currentsmoker,
             data[,c("DietScore1","DietScore2","DietScore3")])
Y = data[,9:17]

## Run R2GUESS on this data
ifelse(dir.exists("../Processed/Clinical"),"",dir.create("../Processed/Clinical"))
ifelse(dir.exists("../Processed/Clinical"),"",dir.create("../Processed/Clinical"))
path.output = gsub("Scripts", "Results/Clinical", getwd())
path.input = gsub("Scripts", "Processed/Clinical", getwd())
path.par = paste(system.file("extdata", package = "R2GUESS"), "/", sep = "")
nsweep = 30000
burn.in = 1000

## Bayesian Variable Selection
model = R2GUESS(Y, X, path.input, path.output,
                nsweep = nsweep, burn.in = burn.in,
                root.file.output = "R2GUESS", 
                path.par = path.par,
                file.par = "Par_file_example_Hopx.xml",
                time = TRUE, top = 100, history = TRUE,
                Egam = 5, Sgam = 5, label.Y = colnames(Y), 
                nb.chain = 3,
                conf = conf, cuda = FALSE,
                MAP.file = NULL, seed = 2)

# summary(model, 100)

set.seed(3)
n.sweep = get.sweep.best.model(model, models = 1)
res.g = get.g.sweep(model, n.sweep$result, 1)
beta = sample.beta(model, res.g, Nmonte = 4, Nmonte.sigma = 4)

## Code for running permutation to get MPPI cut-off value which controls the
## empirical FDR
perm.input = gsub("Scripts", "Results/Clinical/perm_input", getwd())
perm.output = gsub("Scripts", "Results/Clinical/perm_output", getwd())

FDRthresh = FDR.permutation(x = model,
                            path.input = perm.input,
                            path.output = perm.output,
                            Npermut = 50, nbcpu = 5)

saveRDS(model, paste0("..Results/Clinical/",filepaths[m],"_R2GUESS_model.rds"))
saveRDS(FDRthresh, paste0("..Results/Clinical/",filepaths[m],"_R2GUESS_model_FDRthresh.rds"))
saveRDS(res.g, paste0("..Results/Clinical/",filepaths[m],"_R2GUESS_model_res.rds"))
saveRDS(beta, paste0("..Results/Clinical/",filepaths[m],"_R2GUESS_model_beta.rds"))

ifelse(dir.exists("../Figures/Clinical"),"",dir.create("../Figures/Clinical"))

## Diagnostic plots to assess the convergence of the algorithm
jpeg(paste0("../Figures/Clinical//",filepaths[m],"_monitor_App.jpg"), width = 20, height = 17, units = "cm", res = 300)
plot(model)
dev.off()

## Diagnostic plots to assess the convergence of the algorithm
pdf(paste0("../Figures/Clinical/",filepaths[m],"_conv1.pdf"))
check.convergence(model, 1)
dev.off()

pdf(paste0("../Figures/Clinical/",filepaths[m],"_conv2.pdf"))
check.convergence(model, 2)
dev.off()


