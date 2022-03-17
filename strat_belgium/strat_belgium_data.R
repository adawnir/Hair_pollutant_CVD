
### Sumemr Project -- Stratify denoised data by 2 belgium population
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

id_l <- read.csv("../processed/PCA/ids_low.csv")
id_u <- read.csv("../processed/PCA/ids_up.csv")

##### 
for (m in 1:5){
  arr <- paste0(rep(c("pest_de"), each = 5), "_", 
                c("obesity", "hypertension", "diabetes", "Lipid_disorder", "MS"))[m] 
  dat_1 = rep(c("id_l"),each=5)[m]
  dat_2 = rep(c("id_u"),each=5)[m]
  original_l = eval(parse(text = dat_1))
  original_u = eval(parse(text = dat_2))
  denoised = eval(parse(text = arr))
  outcome = c("Obesity", "hypertension", "diabetes", "Lipid_disorder", "MS")[m]
  
  # Subset denoised data by stratification
  denoised.l = denoised[which(denoised$identifiant %in% original_l$x),]
  assign(paste0(arr, ".low"),denoised.l)
  denoised.u = denoised[which(denoised$identifiant %in% original_u$x),]
  assign(paste0(arr, ".up"),denoised.u)
}


table(pest_de_diabetes.low$diabetes)
table(pest_de_diabetes.up$diabetes)

table(pest_de_obesity.low$Obesity)
table(pest_de_obesity.up$Obesity)

table(pest_de_MS.low$MS)
table(pest_de_MS.up$MS)

table(pest_de_hypertension.low$hypertension)
table(pest_de_hypertension.up$hypertension)

table(pest_de_Lipid_disorder.low$Lipid_disorder)
table(pest_de_Lipid_disorder.up$Lipid_disorder)



##### Remove the education variables before analysis --
pest_de_diabetes.low <- subset(pest_de_diabetes.low, select = -c(CountryBelgium, CountryFrance))
pest_de_diabetes.up <- subset(pest_de_diabetes.up, select = -c(CountryBelgium, CountryFrance))

pest_de_hypertension.low <- subset(pest_de_hypertension.low, select = -c(CountryBelgium, CountryFrance))
pest_de_hypertension.up <- subset(pest_de_hypertension.up, select = -c(CountryBelgium, CountryFrance))

pest_de_obesity.low <- subset(pest_de_obesity.low, select = -c(CountryBelgium, CountryFrance))
pest_de_obesity.up <- subset(pest_de_obesity.up, select = -c(CountryBelgium, CountryFrance))

pest_de_MS.low <- subset(pest_de_MS.low, select = -c(CountryBelgium, CountryFrance))
pest_de_MS.up <- subset(pest_de_MS.up, select = -c(CountryBelgium, CountryFrance))

pest_de_Lipid_disorder.low <- subset(pest_de_Lipid_disorder.low, select = -c(CountryBelgium, CountryFrance))
pest_de_Lipid_disorder.up <- subset(pest_de_Lipid_disorder.up, select = -c(CountryBelgium, CountryFrance))



ifelse(dir.exists("../results/strat_belgium_denoised"),"",dir.create("../results/strat_belgium_denoised")) ### Change path for other stratification

for (m in 1:10){
  arr <- paste0(rep(c("pest_de"), each = 5), "_", 
                c("obesity", "hypertension", "diabetes", "Lipid_disorder", "MS"), rep(c(".low",".up"),each=5))[m]
  saveRDS(eval(parse(text=arr)),paste0("../results/strat_belgium_denoised/",arr,"_denoised.rds")) ### Change path for other stratification
}

