
### Sumemr Project --  Compare concnetration among countries
## Programme created by Vivian on 05 July edited on 18 July

rm(list=ls())
project_path="~/hair/scripts"
setwd(project_path)

library(tidyverse)

##### Load data--
##### Load data--
#pest <- readRDS("../processed/PesticidesPCBBDEs_complete.rds")
#pahs <- readRDS("../processed/PAHs_complete.rds")
pest <- readRDS("../processed/imputation/log10_pest.rds")
pahs <- readRDS("../processed/imputation/log10_pahs.rds")

pest1 <- readRDS("../processed/uni_multi_pest.rds")
pahs1 <- readRDS("../processed/uni_multi_pahs.rds")


##### Function --

foo = function(i, dat){
  outcome = colnames(dat)[i]
  print(outcome)
  var_name <- "Country"
  aov_test <- aov(as.formula(paste(outcome, var_name, sep="~")), data = dat)
  # summary(aov_test)[[1]][["Pr(>F)"]][1]
  res = summary(aov_test)[[1]][["Pr(>F)"]][1]
  names(res) = outcome
  return(res)
}

foo_t = function(i, dat){
  outcome = colnames(dat)[i]
  print(outcome)
  var_name <- "Country"
  tmp <- dat[-which(dat$Country == "France"),]
  t_test <- t.test(as.formula(paste(outcome, var_name, sep="~")), data = tmp)
  res = t_test$p.value
  names(res) = outcome
  return(res)
}

foo_tt = function(i, dat){
  outcome = colnames(dat)[i]
  print(outcome)
  var_name <- "Country"
  t_test <- t.test(as.formula(paste(outcome, var_name, sep="~")), data = dat)
  res = t_test$p.value
  names(res) = outcome
  return(res)
}


##### Pre-processing --
pest <- 10^pest
pest <- cbind(pest1$Country, pest)
colnames(pest)
colnames(pest1)
colnames(pest) <- colnames(pest1)[c(2, 17:49)]
pest_to <- pest 
#pest_to <- pest %>% dplyr::select(2,16:48)
#pest_to[,2:34] <- log10(pest_to[,2:34])

pahs <- 10^pahs
pahs <- cbind(pahs1$Country, pahs)
colnames(pahs)
colnames(pahs1)
colnames(pahs) <- colnames(pahs1)[c(2, 17:29)]
pahs_to <- pahs 
#pahs_to <- pahs %>% dplyr::select(2,16:28)
#pahs_to[,2:14] <- log10(pahs_to[,2:14])


##### Run function --
pest_log <- cbind(pest_to["Country"], log10(pest_to[2:34]))
pest_ano <- lapply(2:34, foo, dat=pest_log)
unlist(pest_ano)

pest_t <- lapply(c(18,20:21), foo_t, dat=pest_log)
unlist(pest_t)

pahs_log <- cbind(pahs_to["Country"], log10(pahs_to[2:14]))
pahs_t <- lapply(c(2:5, 7:14), foo_tt, dat=pahs_log)
unlist(pahs_t)


##### Descriptive by country
for (i in 2:14) {
  outcome = colnames(pahs_to)[i]
  print(outcome)
  var_name <- "Country"
  print(aggregate(as.formula(paste(outcome, var_name, sep="~")), data = pahs_to, 
                  function(x) c(mean = mean(x), sd = sd(x))))
}


for (i in 2:34) {
  outcome = colnames(pest_to)[i]
  print(outcome)
  var_name <- "Country"
  print(aggregate(as.formula(paste(outcome, var_name, sep="~")), data = pest_to, 
                  function(x) c(mean = mean(x), sd = sd(x))))
}



##### Inspection --
### p,p'-DDE: 21
colnames(pest)
sum(is.na(pest$`p,p'-DDE`[which(pest$Country == "Belgium")]))
summary(pest$`p,p'-DDE`[which(pest$Country == "Belgium")])
summary(pest$`p,p'-DDE`[which(pest$Country == "France")])
summary(pest$`p,p'-DDE`[which(pest$Country == "Luxembourg")])

### parathion: 31
colnames(pest)
summary(pest$Parathion[which(pest$Country == "Belgium")])
summary(pest$Parathion[which(pest$Country == "France")])
summary(pest$Parathion[which(pest$Country == "Luxembourg")])

### 3PBA
summary(pest$`3-PBA`[which(pest$Country == "Belgium")])
summary(pest$`3-PBA`[which(pest$Country == "France")])
summary(pest$`3-PBA`[which(pest$Country == "Luxembourg")])

### HCB
summary(pest$HCB[which(pest$Country == "Belgium")])
summary(pest$HCB[which(pest$Country == "France")])
summary(pest$HCB[which(pest$Country == "Luxembourg")])

### Cotinine
summary(pahs$`Cotinine (pg/mg)`[which(pahs$Country == "Belgium")])
summary(pahs$`Cotinine (pg/mg)`[which(pahs$Country == "Luxembourg")])

### 2-OH-Phen
summary(pahs$`2-OH-Phenanthrene`[which(pahs$Country == "Belgium")])
summary(pahs$`2-OH-Phenanthrene`[which(pahs$Country == "Luxembourg")])

### 6-OH-Chrys
summary(pahs$`6-OH-Chrysene+11-OH-Benz(a)anthrac`[which(pahs$Country == "Belgium")])
summary(pahs$`6-OH-Chrysene+11-OH-Benz(a)anthrac`[which(pahs$Country == "Luxembourg")])


