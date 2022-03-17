
### Sumemr Project -- Stratify denoised data by country with same obs.
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
for (m in 1:5){
  arr <- paste0(rep(c("pest_de"), each = 5), "_", 
                c("obesity", "hypertension", "diabetes", "Lipid_disorder", "MS"))[m] 
  dat_name = rep(c("pest"),each=5)[m]
  original = eval(parse(text = dat_name))
  denoised = eval(parse(text = arr))
  outcome = rep(c("Obesity", "hypertension", "diabetes", "Lipid_disorder", "MS"), 2)[m]
  # Subsample data sets: adjust case size to smaller data set (female and male)
  original.b=original[original$Country =="Belgium",]
  original.f=original[original$Country =="France",]
  original.l=original[original$Country =="Luxembourg",]
  
  no_1 <- min(sum((original.f[,outcome]=="1")), sum((original.l[,outcome]=="1")), sum((original.b[,outcome]=="1")))
  no_0 <- min(sum((original.f[,outcome]=="0")), sum((original.l[,outcome]=="0")), sum((original.b[,outcome]=="0")))
  
  set.seed(100)
  sf1=sample(which(original.f[,outcome]=="1"),
            size=no_1, replace=FALSE)
  set.seed(100)
  sf0=sample(which(original.f[,outcome]=="0"),
             size=no_0, replace=FALSE)
  sub=c(sf1, sf0)
  f <- original.f$identifiant[sub]
  
  set.seed(100)
  sb1=sample(which(original.b[,outcome]=="1"),
             size=no_1, replace=FALSE)
  set.seed(100)
  sb0=sample(which(original.b[,outcome]=="0"),
             size=no_0, replace=FALSE)
  sub=c(sb1, sb0)
  b <- original.b$identifiant[sub]
 
  set.seed(100)
  sl1=sample(which(original.l[,outcome]=="1"),
             size=no_1, replace=FALSE)
  set.seed(100)
  sl0=sample(which(original.l[,outcome]=="0"),
             size=no_0, replace=FALSE)
  sub=c(sl1, sl0)
  l <- original.l$identifiant[sub]
  
  # Subset denoised data by stratification
  denoised.f = denoised[which(denoised$identifiant %in% f),]
  assign(paste0(arr, ".f"),denoised.f)
  denoised.b = denoised[which(denoised$identifiant %in% b),]
  assign(paste0(arr, ".b"),denoised.b)
  denoised.l = denoised[which(denoised$identifiant %in% l),]
  assign(paste0(arr, ".l"),denoised.l)
}

for (m in 1:5){
  arr <- paste0(rep(c("pahs_de"), each = 5), "_", 
                c("obesity", "hypertension", "diabetes", "Lipid_disorder", "MS"))[m] 
  dat_name = rep(c("pahs"),each=5)[m]
  original = eval(parse(text = dat_name))
  denoised = eval(parse(text = arr))
  outcome = rep(c("Obesity", "hypertension", "diabetes", "Lipid_disorder", "MS"), 2)[m]
  # Subsample data sets: adjust case size to smaller data set (female and male)
  original.b=original[original$Country =="Belgium",]
  original.l=original[original$Country =="Luxembourg",]
  
  no_1 <- min(sum((original.l[,outcome]=="1")), sum((original.b[,outcome]=="1")))
  no_0 <- min(sum((original.l[,outcome]=="0")), sum((original.b[,outcome]=="0")))
  
  set.seed(100)
  sb1=sample(which(original.b[,outcome]=="1"),
             size=no_1, replace=FALSE)
  set.seed(100)
  sb0=sample(which(original.b[,outcome]=="0"),
             size=no_0, replace=FALSE)
  sub=c(sb1, sb0)
  b <- original.b$identifiant[sub]
  
  set.seed(100)
  sl1=sample(which(original.l[,outcome]=="1"),
             size=no_1, replace=FALSE)
  set.seed(100)
  sl0=sample(which(original.l[,outcome]=="0"),
             size=no_0, replace=FALSE)
  sub=c(sl1, sl0)
  l <- original.l$identifiant[sub]
  
  # Subset denoised data by stratification
  denoised.b = denoised[which(denoised$identifiant %in% b),]
  assign(paste0(arr, ".b"),denoised.b)
  denoised.l = denoised[which(denoised$identifiant %in% l),]
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




ifelse(dir.exists("../results/strat_country_denoised_same"),"",dir.create("../results/strat_country_denoised_same")) ### Change path for other stratification
for (m in 1:15){
  arr <- paste0(rep(c("pest_de"), each = 5), "_", 
                c("obesity", "hypertension", "diabetes", "Lipid_disorder", "MS"), rep(c(".f",".b",".l"),each=5))[m]
  saveRDS(eval(parse(text=arr)),paste0("../results/strat_country_denoised_same/",arr,"_denoised.rds")) ### Change path for other stratification
}

for (m in 1:10){
  arr <- paste0(rep(c("pahs_de"), each = 5), "_", 
                c("obesity", "hypertension", "diabetes", "Lipid_disorder", "MS"), rep(c(".b",".l"),each=5))[m]
  saveRDS(eval(parse(text=arr)),paste0("../results/strat_country_denoised_same/",arr,"_denoised.rds")) ### Change path for other stratification
}

