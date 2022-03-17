
### Sumemr Project -- Stratify denoised data by country
## Created by Vivian on 18 July

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
  dat_name = rep(c("pest","pahs"),each=5)[m]
  original = eval(parse(text = dat_name))
  denoised = eval(parse(text = arr))
  outcome = rep(c("Obesity", "hypertension", "diabetes", "Lipid_disorder", "MS"), 2)[m]
  # Subsample data sets: adjust case size to smaller data set (female and male)
  original.b=original[original$Country =="Belgium",]
  original.f=original[original$Country =="France",]
  original.l=original[original$Country =="Luxembourg",]
  
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
  belgium <- original.b$identifiant
  france <- original.f$identifiant
  lux <- original.l$identifiant
  denoised.f = denoised[which(denoised$identifiant %in% france),]
  assign(paste0(arr, ".f"),denoised.f)
  denoised.b = denoised[which(denoised$identifiant %in% belgium),]
  assign(paste0(arr, ".b"),denoised.b)
  denoised.l = denoised[which(denoised$identifiant %in% lux),]
  assign(paste0(arr, ".l"),denoised.l)
}


table(pest_de_diabetes.f$diabetes)
table(pest_de_diabetes.b$diabetes)
table(pest_de_diabetes.l$diabetes)
table(pest_de_obesity.f$Obesity)
table(pest_de_obesity.b$Obesity)
table(pest_de_obesity.l$Obesity)
table(pest_de_MS.f$MS)
table(pest_de_MS.b$MS)
table(pest_de_MS.l$MS)
table(pest_de_hypertension.f$hypertension)
table(pest_de_hypertension.b$hypertension)
table(pest_de_hypertension.l$hypertension)
table(pest_de_Lipid_disorder.f$Lipid_disorder)
table(pest_de_Lipid_disorder.b$Lipid_disorder)
table(pest_de_Lipid_disorder.l$Lipid_disorder)

table(pahs_de_diabetes.b$diabetes)
table(pahs_de_diabetes.l$diabetes)
table(pahs_de_obesity.b$Obesity)
table(pahs_de_obesity.l$Obesity)
table(pahs_de_MS.b$MS)
table(pahs_de_MS.l$MS)
table(pahs_de_hypertension.b$hypertension)
table(pahs_de_hypertension.l$hypertension)
table(pahs_de_Lipid_disorder.b$Lipid_disorder)
table(pahs_de_Lipid_disorder.l$Lipid_disorder)


##### Remove the education variables before analysis --
pest_de_diabetes.f <- subset(pest_de_diabetes.f, select = -c(CountryBelgium, CountryFrance))
pest_de_diabetes.b <- subset(pest_de_diabetes.b, select = -c(CountryBelgium, CountryFrance))
pest_de_diabetes.l <- subset(pest_de_diabetes.l, select = -c(CountryBelgium, CountryFrance))
pest_de_hypertension.f <- subset(pest_de_hypertension.f, select = -c(CountryBelgium, CountryFrance))
pest_de_hypertension.b <- subset(pest_de_hypertension.b, select = -c(CountryBelgium, CountryFrance))
pest_de_hypertension.l <- subset(pest_de_hypertension.l, select = -c(CountryBelgium, CountryFrance))
pest_de_obesity.f <- subset(pest_de_obesity.f, select = -c(CountryBelgium, CountryFrance))
pest_de_obesity.b <- subset(pest_de_obesity.b, select = -c(CountryBelgium, CountryFrance))
pest_de_obesity.l <- subset(pest_de_obesity.l, select = -c(CountryBelgium, CountryFrance))
pest_de_MS.f <- subset(pest_de_MS.f, select = -c(CountryBelgium, CountryFrance))
pest_de_MS.b <- subset(pest_de_MS.b, select = -c(CountryBelgium, CountryFrance))
pest_de_MS.l <- subset(pest_de_MS.l, select = -c(CountryBelgium, CountryFrance))
pest_de_Lipid_disorder.f <- subset(pest_de_Lipid_disorder.f, select = -c(CountryBelgium, CountryFrance))
pest_de_Lipid_disorder.b <- subset(pest_de_Lipid_disorder.b, select = -c(CountryBelgium, CountryFrance))
pest_de_Lipid_disorder.l <- subset(pest_de_Lipid_disorder.l, select = -c(CountryBelgium, CountryFrance))

pahs_de_diabetes.b <- subset(pahs_de_diabetes.b, select = -c(CountryBelgium))
pahs_de_diabetes.l <- subset(pahs_de_diabetes.l, select = -c(CountryBelgium))
pahs_de_hypertension.b <- subset(pahs_de_hypertension.b, select = -c(CountryBelgium))
pahs_de_hypertension.l <- subset(pahs_de_hypertension.l, select = -c(CountryBelgium))
pahs_de_obesity.b <- subset(pahs_de_obesity.b, select = -c(CountryBelgium))
pahs_de_obesity.l <- subset(pahs_de_obesity.l, select = -c(CountryBelgium))
pahs_de_MS.b <- subset(pahs_de_MS.b, select = -c(CountryBelgium))
pahs_de_MS.l <- subset(pahs_de_MS.l, select = -c(CountryBelgium))
pahs_de_Lipid_disorder.b <- subset(pahs_de_Lipid_disorder.b, select = -c(CountryBelgium))
pahs_de_Lipid_disorder.l <- subset(pahs_de_Lipid_disorder.l, select = -c(CountryBelgium))




ifelse(dir.exists("../results/strat_country_denoised"),"",dir.create("../results/strat_country_denoised")) ### Change path for other stratification
for (m in 1:15){
  arr <- paste0(rep(c("pest_de"), each = 5), "_", 
                c("obesity", "hypertension", "diabetes", "Lipid_disorder", "MS"), rep(c(".f",".b",".l"),each=5))[m]
  saveRDS(eval(parse(text=arr)),paste0("../results/strat_country_denoised/",arr,"_denoised.rds")) ### Change path for other stratification
}

for (m in 1:10){
  arr <- paste0(rep(c("pahs_de"), each = 5), "_", 
                c("obesity", "hypertension", "diabetes", "Lipid_disorder", "MS"), rep(c(".b",".l"),each=5))[m]
  saveRDS(eval(parse(text=arr)),paste0("../results/strat_country_denoised/",arr,"_denoised.rds")) ### Change path for other stratification
}

