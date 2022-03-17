
### Sumemr Project -- Clean data
## Created by Vivian on 20 July

rm(list=ls())
project_path="~/hair/scripts"
setwd(project_path)

library(tidyverse)
library(colorspace)


##### Load data and clean
pest <- read.csv("../data/Dataset_PesticidesPCBBDEs.csv")
names(pest) <- pest %>% slice(1) %>% unlist()
pest <- pest %>% slice(-c(1:2))
dim(pest)    # 1096   48

pahs <- read.csv("../data/Dataset_PAHs.csv")
names(pahs) <- pahs %>% slice(1) %>% unlist()
pahs <- pahs %>% slice(-c(1:2))
dim(pahs)    # 682   28

##### Make complete cases
pest <- pest[complete.cases(pest[ , 6:15]),]
pahs <- pahs[complete.cases(pahs[ , 6:15]),]

saveRDS(pest, "../data/PesticidesPCBBDEs.rds")
saveRDS(pahs, "../data/PAHs.rds")

