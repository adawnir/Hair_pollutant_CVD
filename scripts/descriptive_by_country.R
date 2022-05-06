
### Sumemr Project -- Descriptive analyses stratified by country
## Created by Vivian on 18 July added on 28 July

rm(list=ls())
project_path="~/hair/scripts"
setwd(project_path)


##### Load data --
pest <- readRDS("../processed/imputation/log10_pest.rds")
pahs <- readRDS("../processed/imputation/log10_pahs.rds")
over <- readRDS("../processed/imputation/log10_overlap.rds")
pest_data <- readRDS("../processed/uni_multi_pest.rds")
pahs_data <- readRDS("../processed/uni_multi_pahs.rds")
over_data <- readRDS("../processed/uni_multi_overlap.rds")
colnames(pahs_data)[1:20]


### transform back
pest <- 10^pest
pahs <- 10^pahs
over <- 10^over

##### Descriptive
### Age
aggregate(Age ~ Country, function(x) c(mean = mean(x), sd = sd(x)), data = pest_data)
plot(density(pest_data$Age[which(pest_data$Country == "Luxembourg")]))
age_aov <- aov(Age ~ Country,  data = pest_data)
summary(age_aov)

aggregate(Age ~ Country, function(x) c(mean = mean(x), sd = sd(x)), data = pahs_data)
plot(density(pahs_data$Age[which(pahs_data$Country == "Belgium")]))
t.test(Age ~ Country,  data = pahs_data)

aggregate(Age ~ Country, function(x) c(mean = mean(x), sd = sd(x)), data = over_data)
plot(density(over_data$Age[which(over_data$Country == "Belgium")]))
t.test(Age ~ Country,  data = over_data)

### Gender
table(pest_data$Gender, pest_data$Country)
prop.table(table(pest_data$Gender, pest_data$Country), margin = 2)
chisq.test(table(pest_data$Gender, pest_data$Country))

table(pahs_data$Gender, pahs_data$Country)
prop.table(table(pahs_data$Gender, pahs_data$Country), margin = 2)
chisq.test(table(pahs_data$Gender, pahs_data$Country))

table(over_data$Gender, over_data$Country)
prop.table(table(over_data$Gender, over_data$Country), margin = 2)
chisq.test(table(over_data$Gender, over_data$Country))


##### CVRFs
### obesity
prop.table(table(pest_data$Obesity, pest_data$Country), margin = 2)
chisq.test(table(pest_data$Obesity, pest_data$Country))
table(pest_data$Gender[which(pest_data$Obesity == 1)], pest_data$Country[which(pest_data$Obesity == 1)])

prop.table(table(pahs_data$Obesity, pahs_data$Country), margin = 2)
chisq.test(table(pahs_data$Obesity, pahs_data$Country))
table(pahs_data$Gender[which(pahs_data$Obesity == 1)], pahs_data$Country[which(pahs_data$Obesity == 1)])

prop.table(table(over_data$Obesity, over_data$Country), margin = 2)
chisq.test(table(over_data$Obesity, over_data$Country))
table(over_data$Gender[which(over_data$Obesity == 1)], over_data$Country[which(over_data$Obesity == 1)])


### huypertension
prop.table(table(pest_data$hypertension, pest_data$Country), margin = 2)
chisq.test(table(pest_data$hypertension, pest_data$Country))
table(pest_data$Gender[which(pest_data$hypertension == 1)], pest_data$Country[which(pest_data$hypertension == 1)])

prop.table(table(pahs_data$hypertension, pahs_data$Country), margin = 2)
chisq.test(table(pahs_data$hypertension, pahs_data$Country))
table(pahs_data$Gender[which(pahs_data$hypertension == 1)], pahs_data$Country[which(pahs_data$hypertension == 1)])

prop.table(table(over_data$hypertension, over_data$Country), margin = 2)
chisq.test(table(over_data$hypertension, over_data$Country))
table(over_data$Gender[which(over_data$hypertension == 1)], over_data$Country[which(over_data$hypertension == 1)])


### diabetes
prop.table(table(pest_data$diabetes, pest_data$Country), margin = 2)
chisq.test(table(pest_data$diabetes, pest_data$Country))
table(pest_data$Gender[which(pest_data$diabetes == 1)], pest_data$Country[which(pest_data$diabetes == 1)])

prop.table(table(pahs_data$diabetes, pahs_data$Country), margin = 2)
chisq.test(table(pahs_data$diabetes, pahs_data$Country))
table(pahs_data$Gender[which(pahs_data$diabetes == 1)], pahs_data$Country[which(pahs_data$diabetes == 1)])

prop.table(table(over_data$diabetes, over_data$Country), margin = 2)
chisq.test(table(over_data$diabetes, over_data$Country))
table(over_data$Gender[which(over_data$diabetes == 1)], over_data$Country[which(over_data$diabetes == 1)])


### lipid
prop.table(table(pest_data$Lipid_disorder, pest_data$Country), margin = 2)
chisq.test(table(pest_data$Lipid_disorder, pest_data$Country))
table(pest_data$Gender[which(pest_data$Lipid_disorder == 1)], pest_data$Country[which(pest_data$Lipid_disorder == 1)])

prop.table(table(pahs_data$Lipid_disorder, pahs_data$Country), margin = 2)
chisq.test(table(pahs_data$Lipid_disorder, pahs_data$Country))
table(pahs_data$Gender[which(pahs_data$Lipid_disorder == 1)], pahs_data$Country[which(pahs_data$Lipid_disorder == 1)])

prop.table(table(over_data$Lipid_disorder, over_data$Country), margin = 2)
chisq.test(table(over_data$Lipid_disorder, over_data$Country))
table(over_data$Gender[which(over_data$Lipid_disorder == 1)], over_data$Country[which(over_data$Lipid_disorder == 1)])


### MS
prop.table(table(pest_data$MS, pest_data$Country), margin = 2)
chisq.test(table(pest_data$MS, pest_data$Country))
table(pest_data$Gender[which(pest_data$MS == 1)], pest_data$Country[which(pest_data$MS == 1)])

prop.table(table(pahs_data$MS, pahs_data$Country), margin = 2)
chisq.test(table(pahs_data$MS, pahs_data$Country))
table(pahs_data$Gender[which(pahs_data$MS == 1)], pahs_data$Country[which(pahs_data$MS == 1)])

prop.table(table(over_data$MS, over_data$Country), margin = 2)
chisq.test(table(over_data$MS, over_data$Country))
table(over_data$Gender[which(over_data$MS == 1)], over_data$Country[which(over_data$MS == 1)])

