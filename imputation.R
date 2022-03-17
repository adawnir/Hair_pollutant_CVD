
### Sumemr Project -- imputation
## Created by Vivian on 24 June 

rm(list=ls())
project_path="~/hair/scripts"
setwd(project_path)

library(imputeLCMD)

##### Load data --
pest <- readRDS("../data/PesticidesPCBBDEs_complete.rds")
colnames(pest)[16:48]

pahs <- readRDS("../data/PAHs_complete.rds")
colnames(pahs)[16:28]

over <- readRDS("../data/Over_complete.rds")
colnames(over)[16:48]

##### Processing --
pest_50per <- pest[,-which((colSums(is.na(pest))/1065) > 0.5)]
colnames(pest_50per)
pest1 <- log2(pest_50per[16:33])
summary(pest1[1:3]) 

pahs_50per <- pahs[,-which((colSums(is.na(pahs))/670) > 0.5)]
colnames(pahs_50per)
pahs1 <- log2(pahs_50per[16:20])
summary(pahs1[1:3]) 

over_50per <- over[,-which((colSums(is.na(over))/213) > 0.5)]
colnames(over_50per)
over1 <- log2(over_50per[16:39])
summary(over1[1:3]) 


##### Imputation --
### impute.QRILC
set.seed(420)
pahs1_tmp <- impute.QRILC(t(pahs1))
pahs1_imp <- data.frame(t(pahs1_tmp[[1]]))
summary(pahs1_imp)
colnames(pahs1_imp) <- colnames(pahs1)
str(pahs1_imp)  

### check
plot(density(pahs1_imp[,3]))
plot(density(pahs1[,3], na.rm = T))

pahs_final <- cbind(pahs_50per[,1:15], pahs1_imp, pahs_50per[,21:31])

saveRDS(pahs_final, "../data/PAHs_imp.rds")


set.seed(420)
pest1_tmp <- impute.QRILC(t(pest1))
pest1_imp <- data.frame(t(pest1_tmp[[1]]))
summary(pest1_imp)
colnames(pest1_imp) <- colnames(pest1)
str(pest1_imp)  

### check
plot(density(pest_50per_imp[,3]))
plot(density(pest_50per[,3], na.rm = T))

pest_final <- cbind(pest_50per[,1:15], pest1_imp, pest_50per[,34:44])

saveRDS(pest_final, "../data/Pest_imp.rds")


set.seed(420)
over1_tmp <- impute.QRILC(t(over1))
over1_imp <- data.frame(t(over1_tmp[[1]]))
summary(over1_imp)
colnames(over1_imp) <- colnames(over1)
str(over1_imp)  

### check
plot(density(over_50per_imp[,3]))
plot(density(over_50per[,3], na.rm = T))

over_final <- cbind(over_50per[,1:15], over1_imp, over_50per[,40:50])

saveRDS(over_final, "../data/Over_imp.rds")
