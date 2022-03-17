
### Sumemr Project -- sample data exploration (dichotomy)
## Created by Vivian on 20 May 

rm(list=ls())
project_path="~/hair/scripts"
setwd(project_path)

library(openxlsx)
library(data.table)
install.packages("readxl")
library(readxl)
library(dplyr)


##### Load data
mydata <- readRDS("../data/hair_data.rds")
str(mydata)
myloq <- readRDS("../data/loqs.rds")


##### Clean data --
tmp <- lapply(mydata[,8:161], as.numeric) %>% data.frame()


##### Compare with loq --

newdata <- NULL
for (i in 1:154) {
  tmp1 <- tmp[,i]
  tmp2 <- tmp1*(tmp1>myloq[i])
  newdata <- cbind(newdata, tmp2)
}

newdata <- data.frame(newdata)
colnames(newdata) <- colnames(mydata)[8:161]

dich <- apply(newdata , 2, function(x) (sum(x != 0, na.rm = T)/39))
sum(dich >= 0.48)







