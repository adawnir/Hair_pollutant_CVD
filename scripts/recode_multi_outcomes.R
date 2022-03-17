
### Sumemr Project -- Recode different outcomes
## Created by Vivian on 26 July


rm(list=ls())
project_path="~/hair/scripts"
setwd(project_path)


##### Load data --
pest1 <- readRDS("../results/Denoised/pest_one.rds")
pahs1 <- readRDS("../results/Denoised/pahs_one.rds")
over1 <- readRDS("../results/Denoised/over_one.rds")

pest_de <- readRDS("../results/Denoised/pest_de.rds")
pahs_de <- readRDS("../results/Denoised/pahs_de.rds")
over_de <- readRDS("../results/Denoised/over_de.rds")


##### Recode outcomes --
pest_outcome <- pest_de[, 37:41]

pest_multi <- ifelse(pest_outcome$Obesity == 1 & pest_outcome$Lipid_disorder == 0 & pest_outcome$hypertension == 0 & pest_outcome$diabetes == 0 & pest_outcome$MS == 0, 1,
                     ifelse(pest_outcome$Obesity == 0 & pest_outcome$Lipid_disorder == 1 & pest_outcome$hypertension == 0 & pest_outcome$diabetes == 0 & pest_outcome$MS == 0, 2,
                            ifelse(pest_outcome$Obesity == 0 & pest_outcome$Lipid_disorder == 0 & pest_outcome$hypertension == 1 & pest_outcome$diabetes == 0 & pest_outcome$MS == 0, 3,
                                   ifelse(pest_outcome$Obesity == 0 & pest_outcome$Lipid_disorder == 0 & pest_outcome$hypertension == 0 & pest_outcome$diabetes == 1 & pest_outcome$MS == 0, 4,
                                          ifelse(pest_outcome$Obesity == 0 & pest_outcome$Lipid_disorder == 0 & pest_outcome$hypertension == 0 & pest_outcome$diabetes == 0 & pest_outcome$MS == 1, 5,
                                                 ifelse(pest_outcome$Obesity == 1 & pest_outcome$Lipid_disorder == 1 & pest_outcome$hypertension == 0 & pest_outcome$diabetes == 0 & pest_outcome$MS == 0, 6,
                                                        ifelse(pest_outcome$Obesity == 1 & pest_outcome$Lipid_disorder == 0 & pest_outcome$hypertension == 1 & pest_outcome$diabetes == 0 & pest_outcome$MS == 0, 7,
                                                               ifelse(pest_outcome$Obesity == 1 & pest_outcome$Lipid_disorder == 0 & pest_outcome$hypertension == 0 & pest_outcome$diabetes == 1 & pest_outcome$MS == 0, 8,
                                                                      ifelse(pest_outcome$Obesity == 1 & pest_outcome$Lipid_disorder == 0 & pest_outcome$hypertension == 0 & pest_outcome$diabetes == 0 & pest_outcome$MS == 1, 9,
                                                                             ifelse(pest_outcome$Obesity == 0 & pest_outcome$Lipid_disorder == 1 & pest_outcome$hypertension == 1 & pest_outcome$diabetes == 0 & pest_outcome$MS == 0, 10,
                                                                                    ifelse(pest_outcome$Obesity == 0 & pest_outcome$Lipid_disorder == 1 & pest_outcome$hypertension == 0 & pest_outcome$diabetes == 1 & pest_outcome$MS == 0, 11,
                                                                                           ifelse(pest_outcome$Obesity == 0 & pest_outcome$Lipid_disorder == 1 & pest_outcome$hypertension == 0 & pest_outcome$diabetes == 0 & pest_outcome$MS == 1, 12,
                                                                                                  ifelse(pest_outcome$Obesity == 0 & pest_outcome$Lipid_disorder == 0 & pest_outcome$hypertension == 1 & pest_outcome$diabetes == 1 & pest_outcome$MS == 0, 13,
                                                                                                         ifelse(pest_outcome$Obesity == 0 & pest_outcome$Lipid_disorder == 0 & pest_outcome$hypertension == 1 & pest_outcome$diabetes == 0 & pest_outcome$MS == 1, 14,
                                                                                                                ifelse(pest_outcome$Obesity == 0 & pest_outcome$Lipid_disorder == 0 & pest_outcome$hypertension == 0 & pest_outcome$diabetes == 1 & pest_outcome$MS == 1, 15,
                                                                                                                       ifelse(pest_outcome$Obesity == 1 & pest_outcome$Lipid_disorder == 1 & pest_outcome$hypertension == 1 & pest_outcome$diabetes == 0 & pest_outcome$MS == 0, 16,
                                                                                                                              ifelse(pest_outcome$Obesity == 1 & pest_outcome$Lipid_disorder == 1 & pest_outcome$hypertension == 0 & pest_outcome$diabetes == 1 & pest_outcome$MS == 0, 17,
                                                                                                                                     ifelse(pest_outcome$Obesity == 1 & pest_outcome$Lipid_disorder == 1 & pest_outcome$hypertension == 0 & pest_outcome$diabetes == 0 & pest_outcome$MS == 1, 18,
                                                                                                                                            ifelse(pest_outcome$Obesity == 1 & pest_outcome$Lipid_disorder == 0 & pest_outcome$hypertension == 1 & pest_outcome$diabetes == 1 & pest_outcome$MS == 0, 19,
                                                                                                                                                   ifelse(pest_outcome$Obesity == 1 & pest_outcome$Lipid_disorder == 0 & pest_outcome$hypertension == 1 & pest_outcome$diabetes == 0 & pest_outcome$MS == 1, 20,
                                                                                                                                                          ifelse(pest_outcome$Obesity == 1 & pest_outcome$Lipid_disorder == 0 & pest_outcome$hypertension == 0 & pest_outcome$diabetes == 1 & pest_outcome$MS == 1, 21,
                                                                                                                                                                 ifelse(pest_outcome$Obesity == 0 & pest_outcome$Lipid_disorder == 1 & pest_outcome$hypertension == 1 & pest_outcome$diabetes == 1 & pest_outcome$MS == 0, 22,
                                                                                                                                                                        ifelse(pest_outcome$Obesity == 0 & pest_outcome$Lipid_disorder == 1 & pest_outcome$hypertension == 1 & pest_outcome$diabetes == 0 & pest_outcome$MS == 1, 23,
                                                                                                                                                                               ifelse(pest_outcome$Obesity == 0 & pest_outcome$Lipid_disorder == 1 & pest_outcome$hypertension == 0 & pest_outcome$diabetes == 1 & pest_outcome$MS == 1, 24,
                                                                                                                                                                                      ifelse(pest_outcome$Obesity == 0 & pest_outcome$Lipid_disorder == 0 & pest_outcome$hypertension == 1 & pest_outcome$diabetes == 1 & pest_outcome$MS == 1, 25,
                                                                                                                                                                                             ifelse(pest_outcome$Obesity == 1 & pest_outcome$Lipid_disorder == 1 & pest_outcome$hypertension == 1 & pest_outcome$diabetes == 1 & pest_outcome$MS == 0, 26,
                                                                                                                                                                                                    ifelse(pest_outcome$Obesity == 1 & pest_outcome$Lipid_disorder == 1 & pest_outcome$hypertension == 1 & pest_outcome$diabetes == 0 & pest_outcome$MS == 1, 27,
                                                                                                                                                                                                           ifelse(pest_outcome$Obesity == 1 & pest_outcome$Lipid_disorder == 0 & pest_outcome$hypertension == 1 & pest_outcome$diabetes == 1 & pest_outcome$MS == 1, 28,
                                                                                                                                                                                                                  ifelse(pest_outcome$Obesity == 1 & pest_outcome$Lipid_disorder == 1 & pest_outcome$hypertension == 0 & pest_outcome$diabetes == 1 & pest_outcome$MS == 1, 29,
                                                                                                                                                                                                                         ifelse(pest_outcome$Obesity == 0 & pest_outcome$Lipid_disorder == 1 & pest_outcome$hypertension == 1 & pest_outcome$diabetes == 1 & pest_outcome$MS == 1, 30,
                                                                                                                                                                                                                                ifelse(pest_outcome$Obesity == 1 & pest_outcome$Lipid_disorder == 1 & pest_outcome$hypertension == 1 & pest_outcome$diabetes == 1 & pest_outcome$MS == 1, 31,0)))))))))))))))))))))))))))))))
table(pest_multi)  
length(pest_multi)  
pest_final <- cbind(pest_de[, 1:36], pest_multi)
colnames(pest_final)[37] <- "outcomes"

saveRDS(pest_final, "../results/denoised_multi_y/pest_multi.rds")

table(pest_final$outcomes)
pest_small <- c(4,5,7,8,9,11,14,21,24,25,28,29)
pest_red <- pest_final[-which(pest_final$outcomes %in% pest_small),]   # 1031

saveRDS(pest_red, "../results/denoised_multi_y/pest_multi_redu.rds")


### PAHs
pahs_outcome <- pahs_de[, 16:20]

pahs_multi <- ifelse(pahs_outcome$Obesity == 1 & pahs_outcome$Lipid_disorder == 0 & pahs_outcome$hypertension == 0 & pahs_outcome$diabetes == 0 & pahs_outcome$MS == 0, 1,
                     ifelse(pahs_outcome$Obesity == 0 & pahs_outcome$Lipid_disorder == 1 & pahs_outcome$hypertension == 0 & pahs_outcome$diabetes == 0 & pahs_outcome$MS == 0, 2,
                            ifelse(pahs_outcome$Obesity == 0 & pahs_outcome$Lipid_disorder == 0 & pahs_outcome$hypertension == 1 & pahs_outcome$diabetes == 0 & pahs_outcome$MS == 0, 3,
                                   ifelse(pahs_outcome$Obesity == 0 & pahs_outcome$Lipid_disorder == 0 & pahs_outcome$hypertension == 0 & pahs_outcome$diabetes == 1 & pahs_outcome$MS == 0, 4,
                                          ifelse(pahs_outcome$Obesity == 0 & pahs_outcome$Lipid_disorder == 0 & pahs_outcome$hypertension == 0 & pahs_outcome$diabetes == 0 & pahs_outcome$MS == 1, 5,
                                                 ifelse(pahs_outcome$Obesity == 1 & pahs_outcome$Lipid_disorder == 1 & pahs_outcome$hypertension == 0 & pahs_outcome$diabetes == 0 & pahs_outcome$MS == 0, 6,
                                                        ifelse(pahs_outcome$Obesity == 1 & pahs_outcome$Lipid_disorder == 0 & pahs_outcome$hypertension == 1 & pahs_outcome$diabetes == 0 & pahs_outcome$MS == 0, 7,
                                                               ifelse(pahs_outcome$Obesity == 1 & pahs_outcome$Lipid_disorder == 0 & pahs_outcome$hypertension == 0 & pahs_outcome$diabetes == 1 & pahs_outcome$MS == 0, 8,
                                                                      ifelse(pahs_outcome$Obesity == 1 & pahs_outcome$Lipid_disorder == 0 & pahs_outcome$hypertension == 0 & pahs_outcome$diabetes == 0 & pahs_outcome$MS == 1, 9,
                                                                             ifelse(pahs_outcome$Obesity == 0 & pahs_outcome$Lipid_disorder == 1 & pahs_outcome$hypertension == 1 & pahs_outcome$diabetes == 0 & pahs_outcome$MS == 0, 10,
                                                                                    ifelse(pahs_outcome$Obesity == 0 & pahs_outcome$Lipid_disorder == 1 & pahs_outcome$hypertension == 0 & pahs_outcome$diabetes == 1 & pahs_outcome$MS == 0, 11,
                                                                                           ifelse(pahs_outcome$Obesity == 0 & pahs_outcome$Lipid_disorder == 1 & pahs_outcome$hypertension == 0 & pahs_outcome$diabetes == 0 & pahs_outcome$MS == 1, 12,
                                                                                                  ifelse(pahs_outcome$Obesity == 0 & pahs_outcome$Lipid_disorder == 0 & pahs_outcome$hypertension == 1 & pahs_outcome$diabetes == 1 & pahs_outcome$MS == 0, 13,
                                                                                                         ifelse(pahs_outcome$Obesity == 0 & pahs_outcome$Lipid_disorder == 0 & pahs_outcome$hypertension == 1 & pahs_outcome$diabetes == 0 & pahs_outcome$MS == 1, 14,
                                                                                                                ifelse(pahs_outcome$Obesity == 0 & pahs_outcome$Lipid_disorder == 0 & pahs_outcome$hypertension == 0 & pahs_outcome$diabetes == 1 & pahs_outcome$MS == 1, 15,
                                                                                                                       ifelse(pahs_outcome$Obesity == 1 & pahs_outcome$Lipid_disorder == 1 & pahs_outcome$hypertension == 1 & pahs_outcome$diabetes == 0 & pahs_outcome$MS == 0, 16,
                                                                                                                              ifelse(pahs_outcome$Obesity == 1 & pahs_outcome$Lipid_disorder == 1 & pahs_outcome$hypertension == 0 & pahs_outcome$diabetes == 1 & pahs_outcome$MS == 0, 17,
                                                                                                                                     ifelse(pahs_outcome$Obesity == 1 & pahs_outcome$Lipid_disorder == 1 & pahs_outcome$hypertension == 0 & pahs_outcome$diabetes == 0 & pahs_outcome$MS == 1, 18,
                                                                                                                                            ifelse(pahs_outcome$Obesity == 1 & pahs_outcome$Lipid_disorder == 0 & pahs_outcome$hypertension == 1 & pahs_outcome$diabetes == 1 & pahs_outcome$MS == 0, 19,
                                                                                                                                                   ifelse(pahs_outcome$Obesity == 1 & pahs_outcome$Lipid_disorder == 0 & pahs_outcome$hypertension == 1 & pahs_outcome$diabetes == 0 & pahs_outcome$MS == 1, 20,
                                                                                                                                                          ifelse(pahs_outcome$Obesity == 1 & pahs_outcome$Lipid_disorder == 0 & pahs_outcome$hypertension == 0 & pahs_outcome$diabetes == 1 & pahs_outcome$MS == 1, 21,
                                                                                                                                                                 ifelse(pahs_outcome$Obesity == 0 & pahs_outcome$Lipid_disorder == 1 & pahs_outcome$hypertension == 1 & pahs_outcome$diabetes == 1 & pahs_outcome$MS == 0, 22,
                                                                                                                                                                        ifelse(pahs_outcome$Obesity == 0 & pahs_outcome$Lipid_disorder == 1 & pahs_outcome$hypertension == 1 & pahs_outcome$diabetes == 0 & pahs_outcome$MS == 1, 23,
                                                                                                                                                                               ifelse(pahs_outcome$Obesity == 0 & pahs_outcome$Lipid_disorder == 1 & pahs_outcome$hypertension == 0 & pahs_outcome$diabetes == 1 & pahs_outcome$MS == 1, 24,
                                                                                                                                                                                      ifelse(pahs_outcome$Obesity == 0 & pahs_outcome$Lipid_disorder == 0 & pahs_outcome$hypertension == 1 & pahs_outcome$diabetes == 1 & pahs_outcome$MS == 1, 25,
                                                                                                                                                                                             ifelse(pahs_outcome$Obesity == 1 & pahs_outcome$Lipid_disorder == 1 & pahs_outcome$hypertension == 1 & pahs_outcome$diabetes == 1 & pahs_outcome$MS == 0, 26,
                                                                                                                                                                                                    ifelse(pahs_outcome$Obesity == 1 & pahs_outcome$Lipid_disorder == 1 & pahs_outcome$hypertension == 1 & pahs_outcome$diabetes == 0 & pahs_outcome$MS == 1, 27,
                                                                                                                                                                                                           ifelse(pahs_outcome$Obesity == 1 & pahs_outcome$Lipid_disorder == 0 & pahs_outcome$hypertension == 1 & pahs_outcome$diabetes == 1 & pahs_outcome$MS == 1, 28,
                                                                                                                                                                                                                  ifelse(pahs_outcome$Obesity == 1 & pahs_outcome$Lipid_disorder == 1 & pahs_outcome$hypertension == 0 & pahs_outcome$diabetes == 1 & pahs_outcome$MS == 1, 29,
                                                                                                                                                                                                                         ifelse(pahs_outcome$Obesity == 0 & pahs_outcome$Lipid_disorder == 1 & pahs_outcome$hypertension == 1 & pahs_outcome$diabetes == 1 & pahs_outcome$MS == 1, 30, 
                                                                                                                                                                                                                                ifelse(pahs_outcome$Obesity == 1 & pahs_outcome$Lipid_disorder == 1 & pahs_outcome$hypertension == 1 & pahs_outcome$diabetes == 1 & pahs_outcome$MS == 1, 31, 0)))))))))))))))))))))))))))))))
table(pahs_multi)  
length(pahs_multi)  
pahs_final <- cbind(pahs_de[, 1:15], pahs_multi)
colnames(pahs_final)[16] <- "outcomes"

saveRDS(pahs_final, "../results/denoised_multi_y/pahs_multi.rds")

table(pahs_final$outcomes)
pahs_small <- c(5,9,11,14,15,20,25,29,30)
pahs_red <- pahs_final[-which(pahs_final$outcomes %in% pahs_small),]   # 645

saveRDS(pahs_red, "../results/denoised_multi_y/pahs_multi_redu.rds")

