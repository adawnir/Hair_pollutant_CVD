
### Sumemr Project -- Stratify denoised data by gender
## Created by Vivian on 13 July

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
  
  # set.seed(100)
  # s1=sample(which(original.f[,outcome]=="1"),
  #          size=sum(original.m[,outcome]=="1"), replace=FALSE)
  # sub=c(s1,which(original.f[,outcome]=="0"))
  # male=rownames(original.f)[sub]
  # set.seed(100)
  # s0=sample(which(original.m[,outcome]=="0"),
  #          size=sum(original.f[,outcome]=="0"), replace=FALSE)
  # sub=c(which(original.m[,outcome]=="1"),s0)
  # female=rownames(original.m)[sub]
  
  # Subset denoised data by stratification
  male <- original.m$identifiant
  female <- original.f$identifiant
  denoised.f = denoised[which(denoised$identifiant %in% female),]
  assign(paste0(arr, ".f"),denoised.f)
  denoised.m = denoised[which(denoised$identifiant %in% male),]
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


ifelse(dir.exists("../results/strat_sex_denoised"),"",dir.create("../results/strat_sex_denoised")) ### Change path for other stratification
for (m in 1:20){
  arr <- paste0(rep(c("pest_de", "pahs_de"), each = 5), "_", 
                c("obesity", "hypertension", "diabetes", "Lipid_disorder", "MS"), rep(c(".f",".m"),each=10))[m]
  saveRDS(eval(parse(text=arr)),paste0("../results/strat_sex_denoised/",arr,"_denoised.rds")) ### Change path for other stratification
}

