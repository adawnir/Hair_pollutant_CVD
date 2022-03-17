
### Sumemr Project -- Stratify denoised data by gender with same obs.
## Created by Vivian on 19 July

rm(list=ls())
project_path="~/hair/scripts"
setwd(project_path)


##### Load data --
for (m in 1:10){
  arr <- paste0(rep(c("pest_de", "pahs_de"), each = 5), "_", 
                c("obesity", "hypertension", "diabetes", "Lipid_disorder", "MS"))[m]
  dat=readRDS(paste0("../results/Denoised/",arr,".rds"))
  assign(arr,dat)
} 

pest <- readRDS("../processed/uni_multi_pest.rds")
pahs <- readRDS("../processed/uni_multi_pahs.rds")

##### 
for (m in 1:10){
  arr <- paste0(rep(c("pest_de", "pahs_de"), each = 5), "_", 
                c("obesity", "hypertension", "diabetes", "Lipid_disorder", "MS"))[m] 
  dat_name=rep(c("pest","pahs"),each=5)[m]
  original = eval(parse(text = dat_name))
  denoised = eval(parse(text = arr))
  outcome = rep(c("Obesity", "hypertension", "diabetes", "Lipid_disorder", "MS"), 2)[m]
  # Subsample data sets: adjust case size to smaller data set (female and male)
  original.m=original[original$Gender=="M",]
  original.f=original[original$Gender=="F",]
  
  no_1 <- min(sum((original.f[,outcome]=="1")), sum((original.m[,outcome]=="1")))
  no_0 <- min(sum((original.f[,outcome]=="0")), sum((original.m[,outcome]=="0")))
  
  set.seed(100)
  sf1=sample(which(original.f[,outcome]=="1"),
             size=no_1, replace=FALSE)
  set.seed(100)
  sf0=sample(which(original.f[,outcome]=="0"),
             size=no_0, replace=FALSE)
  sub=c(sf1, sf0)
  f <- original.f$identifiant[sub]
  
  set.seed(100)
  sm1=sample(which(original.m[,outcome]=="1"),
             size=no_1, replace=FALSE)
  set.seed(100)
  sm0=sample(which(original.m[,outcome]=="0"),
             size=no_0, replace=FALSE)
  sub=c(sm1, sm0)
  m <- original.m$identifiant[sub]
  
  # Subset denoised data by stratification
  
  denoised.f = denoised[which(denoised$identifiant %in% f),]
  assign(paste0(arr, ".f"),denoised.f)
  denoised.m = denoised[which(denoised$identifiant %in% m),]
  assign(paste0(arr, ".m"),denoised.m)
}


table(pest_de_diabetes.f$diabetes)
table(pest_de_diabetes.m$diabetes)
table(pest_de_obesity.f$Obesity)
table(pest_de_obesity.m$Obesity)
table(pest_de_MS.f$MS)
table(pest_de_MS.m$MS)
table(pest_de_hypertension.f$hypertension)
table(pest_de_hypertension.m$hypertension)
table(pest_de_Lipid_disorder.f$Lipid_disorder)
table(pest_de_Lipid_disorder.m$Lipid_disorder)

table(pahs_de_diabetes.f$diabetes)
table(pahs_de_diabetes.m$diabetes)
table(pahs_de_obesity.f$Obesity)
table(pahs_de_obesity.m$Obesity)
table(pahs_de_MS.f$MS)
table(pahs_de_MS.m$MS)
table(pahs_de_hypertension.f$hypertension)
table(pahs_de_hypertension.m$hypertension)
table(pahs_de_Lipid_disorder.f$Lipid_disorder)
table(pahs_de_Lipid_disorder.m$Lipid_disorder)


ifelse(dir.exists("../results/strat_sex_denoised_same"),"",dir.create("../results/strat_sex_denoised_same")) ### Change path for other stratification
for (m in 1:20){
  arr <- paste0(rep(c("pest_de", "pahs_de"), each = 5), "_", 
                c("obesity", "hypertension", "diabetes", "Lipid_disorder", "MS"), rep(c(".f",".m"),each=10))[m]
  saveRDS(eval(parse(text=arr)),paste0("../results/strat_sex_denoised_same/",arr,"_denoised.rds")) ### Change path for other stratification
}

