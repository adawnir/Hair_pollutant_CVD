# R2GUESS
rm(list=ls())
project_path=dirname(rstudioapi::getActiveDocumentContext()$path)
setwd(project_path)


library(R2GUESS)
library(sharp)
library(sgPLS)

source("graph_colours.R")
library(colorspace)

filepaths = c("PesticidesPCBBDEs_train","PAHs_train","Overlap_test")
paths = c("Option1","Option2","Option3")

# R2GUESS ----
i = 1
j = 1
f = 4
X = readRDS(paste0("../Processed/",filepaths[i],"_expo_ndimp_naimp_log.rds"))
model = readRDS(paste0("../Results/R2GUESS/",paths[j],"/Model",f,".rds"))
write.table(colnames(X), paste0(model$path.input,"/X.annotation.txt"))

## Variables included in the best models
pdf(paste0("../Figures/R2GUESS/Model",f-1,"_Variables_included_top20_models_",paths[j],".pdf"))
plotvariable(model, 20, file.annotation = paste0(model$path.input,"/X.annotation.txt"))
dev.off()
## Proximity between the best models
pdf(paste0("../Figures/R2GUESS/Model",f-1,"_Proximity_between_top20_models_",paths[j],".pdf"))
plotmodel(model, 20)
dev.off()

# Validation ----
j = 1
f = 4

# Load data
response = readRDS(paste0("../Data/",filepaths[3],"_response.rds"))
Z = readRDS(paste0("../Processed/",filepaths[3],"_Z.rds"))
X = readRDS(paste0("../Processed/",filepaths[3],"_expo_ndimp_naimp_log.rds"))
X = as.data.frame(X)

# Select outcomes
colnames(response)
if (j==1){
  Y = response %>% select(BMI, WC, TG, HDLC, SBP, DBP)
}
if (j==2){
  Y = response %>% select(BMI, WC, TG, TC, LDLC, HDLC, SBP, DBP)
}
if (j==3){
  Y = response %>% select(BMI, WC, TG, TC, LDLC, HDLC, SBP, DBP, FPG)
}
if (f == 0){
  Z = Z %>% select(Age, Gender)
}
if (f == 1){
  Z = Z %>% select(Age, Gender, Centre)
}
if (f == 2){
  Z = Z %>% select(Age, Gender, Education)
}
if (f == 3){
  Z = Z %>% select(Age, Gender, Education, Smoking)
}
if (f == 4){
  Z = Z %>% select(Age, Gender, Education, Smoking, starts_with("Diet"))
}
if (f == 5){
  Z = Z %>% select(Age, Gender, Education, Smoking, starts_with("Diet"),
                   Physical_activity)
}
if (f == 6){
  Z = Z %>% select(Age, Gender, Education, Smoking, starts_with("Diet"),
                   Physical_activity, Alcohol)
}
if (f == 7){
  Z = Z %>% select(Age, Gender, Education, Smoking, starts_with("Diet"),
                   Physical_activity, Alcohol) %>%
    bind_cols(response %>% select(Diabetes_medication, Lipid_medication))
}
Z = model.matrix(~ ., Z)[,-1]
Z = as.data.frame(Z)



model = readRDS(paste0("../Results/R2GUESS/",paths[j],"/Model",f,".rds"))
bestmodel = colnames(X)[1:33][as.numeric(strsplit(model$BestModels$modelName[1], " ")[[1]])]

selected = 0
for (k in 1:ncol(Y)){
  stab = readRDS(paste0("../Results/Multivariate/",colnames(Y)[k],"_X_f",f,"_output_",filepaths[i],".rds"))
  selected = selected + SelectedVariables(stab)
}
combmodel = names(selected)[selected>0]

# Recalibration
Xtest = X %>% select(all_of(bestmodel))
mypls = PLS(cbind(Z,Xtest), Y, family = "gaussian")
pred = PredictPLS(cbind(Z,Xtest),mypls)[,,1]
q2 = NULL
for(k in 1:ncol(pred)){
  q2 = c(q2, cor(pred[,k], Y[,k])^2)
}
plot(q2, type = "b", ylim = c(0,1), xlab = "", ylab = expression("Q"^2))

Xtest = X %>% select(all_of(bestmodel), Cotinine, Nicotine)
mypls = PLS(cbind(Z,Xtest), Y, family = "gaussian")
pred = PredictPLS(cbind(Z,Xtest),mypls)[,,1]
q2 = NULL
for(k in 1:ncol(pred)){
  q2 = c(q2, cor(pred[,k], Y[,k])^2)
}
lines(q2, type = "b")

Xtest = X %>% selectqs(all_of(combmodel))
mypls = PLS(cbind(Z,Xtest), Y, family = "gaussian")
pred = PredictPLS(cbind(Z,Xtest),mypls)[,,1]
q2 = NULL
for(k in 1:ncol(pred)){
  q2 = c(q2, cor(pred[,k], Y[,k])^2)
}
lines(q2, type = "b", col = "#994517")

