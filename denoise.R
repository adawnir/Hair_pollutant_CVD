
### Sumemr Project -- Denoise data
## Created by Vivian on 30 June 

rm(list=ls())
project_path="~/hair/scripts"
setwd(project_path)

library(tidyverse)


##### Load data --
pest <- readRDS("../processed/uni_multi_pest.rds")
pahs <- readRDS("../processed/uni_multi_pahs.rds")
over <- readRDS("../processed/uni_multi_overlap.rds")


##### Extract data--
colnames(pest)
pest <- pest %>% dplyr::select(c(1:3,5,17:49, 51:55))
colnames(pahs)
pahs <- pahs %>% dplyr::select(c(1:3,5,17:29, 31:35))
colnames(over)
over <- over %>% dplyr::select(c(1:3,5,17:62, 64:68))


##### Denoise data --
### function
foo = function(i, dat){
  var_name=colnames(dat)[i]
  print(var_name)
  confound=c("Age","Gender")  # Change confounders accordingly
  if(is.numeric(dat[,i])){
    model = lm(as.formula(paste(var_name,paste(confound,collapse="+"),sep="~")),data=dat)
    res=model$residuals
  } else {
    X=as.data.frame(model.matrix(~., dat[,-1])[,-1])
    # Find all columns with var_name
    mygrep=grep(paste0("^",var_name),colnames(X))
    res=NULL
    for (j in mygrep){
      model = glm(as.formula(paste("X[,j]",paste(confound,collapse="+"),sep="~")),data=dat,
                  family="binomial")
      res=cbind(res,model$residuals)
    }
  }
  return(res)
}


### run the function
pest_de <- sapply(c(2, 5:37), foo, dat=pest)
pest_de <- data.frame(pest_de)

#pest_de <- do.call("cbind", pest_de)
pahs_de <- sapply(c(2, 5:17), foo, dat=pahs)

over_de <- sapply(c(2, 5:50), foo, dat=over)


##### One hot encoding for non-denoised data --
pest_tmp <- pest[, -c(1, 38:42)]
pest_one <- as.data.frame(model.matrix(~.,pest_tmp)[,-1])
pest_one <- cbind(pest[, 1], pest_one[c(3:4,1:2,5:ncol(pest_one))], pest[, 38:42])
colnames(pest_one)[1]="identifiant"

pahs_tmp <- pahs[, -c(1, 18:22)]
pahs_one <- as.data.frame(model.matrix(~.,pahs_tmp)[,-1])
pahs_one <- cbind(pahs[, 1], pahs_one[c(2:3,1,4:ncol(pahs_one))], pahs[, 18:ncol(pahs)])
colnames(pahs_one)[1]="identifiant"

over_tmp <- over[, -c(1, 51:55)]
over_one <- as.data.frame(model.matrix(~.,over_tmp)[,-1])
over_one <- cbind(over[, 1], over_one[c(2:3,1,4:ncol(over_one))], over[, 51:ncol(over)])
colnames(over_one)[1]="identifiant"


saveRDS(pest_one, "../results/Denoised/pest_one.rds")
saveRDS(pahs_one, "../results/Denoised/pahs_one.rds")
saveRDS(over_one, "../results/Denoised/over_one.rds")


##### Arrange data
pest_de_1 <- cbind(pest[, 1], pest_de, pest[,38:ncol(pest)])
colnames(pest_de_1) <- colnames(pest_one)[c(1:3, 6:ncol(pest_one))]

pahs_de_1 <- cbind(pahs[, 1], pahs_de, pahs[,18:ncol(pahs)])
colnames(pahs_de_1) <- colnames(pahs_one)[c(1:2, 5:ncol(pahs_one))]

over_de_1 <- cbind(over[,1], over_de, over[,51:ncol(over)])
colnames(over_de_1) <- colnames(over_one)[c(1:2, 5:ncol(over_one))]


saveRDS(pest_de_1, "../results/Denoised/pest_de.rds")
saveRDS(pahs_de_1, "../results/Denoised/pahs_de.rds")
saveRDS(over_de_1, "../results/Denoised/over_de.rds")



##### Test --
pest_test <- pest[, 1:5]
pest_test <- as.data.frame(model.matrix(~.,pest_test[,c(2,5)])[,-1])
pest_test <- cbind(pest_test, pest[,3:4])

model = glm(CountryFrance ~ Age + Gender, data=pest_test, family="binomial")
res <- model$residuals
view(res)


