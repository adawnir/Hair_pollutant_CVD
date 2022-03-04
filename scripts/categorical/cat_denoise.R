
### Sumemr Project -- Denoise data
## Created by Vivian on 20 July

rm(list=ls())
project_path="~/hair/scripts"
setwd(project_path)

library(tidyverse)


##### Load data --
pest_cat <- readRDS("../cat_results/pest_cat.rds")
pahs_cat <- readRDS("../cat_results/pahs_cat.rds")
pest_cont <- readRDS("../cat_results/pest_cont.rds")
pahs_cont <- readRDS("../cat_results/pahs_cont.rds")


##### Extract data and combine --
### for all: categorical and continuous
colnames(pest_cat)
pest_cat_tmp <- pest_cat %>% dplyr::select(c(1:24))
colnames(pest_cont)
pest_cont_tmp <- pest_cont %>% dplyr::select(c(10:27))
pest_comb <- cbind(pest_cat_tmp, pest_cont_tmp)
str(pest_comb)

colnames(pahs_cat)
pahs_cat_tmp <- pahs_cat %>% dplyr::select(c(1:17))
colnames(pahs_cont)
pahs_cont_tmp <- pahs_cont %>% dplyr::select(c(10:14))
pahs_comb <- cbind(pahs_cat_tmp, pahs_cont_tmp)
str(pahs_comb)


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
colnames(pest_cont)
pest_de <- sapply(c(4,10:27), foo, dat=pest_cont)
pest_de <- data.frame(pest_de)

colnames(pahs_cont)
pahs_de <- sapply(c(4, 10:14), foo, dat=pahs_cont)

colnames(pest_comb)
pest_comb_de <- sapply(c(4, 10:42), foo, dat=pest_comb)

colnames(pahs_comb)
pahs_comb_de <- sapply(c(4, 10:22), foo, dat=pahs_comb)


##### One hot encoding for non-denoised data --
pest_tmp <- pest_comb[, -c(1:3, 5:9)]
pest_one <- as.data.frame(model.matrix(~.,pest_tmp)[,-1])
pest_one <- cbind(pest_comb[, c(1:3, 5:9)], pest_one)
colnames(pest_one)[1]="identifiant"

pahs_tmp <- pahs_comb[, -c(1:3, 5:9)]
pahs_one <- as.data.frame(model.matrix(~.,pahs_tmp)[,-1])
pahs_one <- cbind(pahs_comb[, c(1:3, 5:9)], pahs_one)
colnames(pahs_one)[1]="identifiant"

over_tmp <- over[, -c(1, 51:55)]
over_one <- as.data.frame(model.matrix(~.,over_tmp)[,-1])
over_one <- cbind(over[, 1], over_one, over[, 51:ncol(over)])
colnames(over_one)[1]="identifiant"


saveRDS(pest_one, "../cat_results/denoised/pest_one.rds")
saveRDS(pahs_one, "../cat_results/denoised/pahs_one.rds")
#saveRDS(over_one, "../cat_results/denoised/over_one.rds")


##### Arrange data
pest_de_1 <- cbind(pest_cont[, c(1:3, 5:9)], pest_de)
colnames(pest_de_1) <- c(colnames(pest_one)[1:10], colnames(pest_cont)[10:27])

pahs_de_1 <- cbind(pahs_cont[, c(1:3, 5:9)], pahs_de)
colnames(pahs_de_1) <- c(colnames(pahs_one)[1:9], colnames(pahs_cont)[10:14])

pest_comb_de_1 <- cbind(pest_comb[, c(1:3, 5:9)], pest_comb_de)
colnames(pest_comb_de_1) <- c(colnames(pest_one)[1:10], colnames(pest_comb)[10:42])

pahs_comb_de_1 <- cbind(pahs_comb[, c(1:3, 5:9)], pahs_comb_de)
colnames(pahs_comb_de_1) <- c(colnames(pahs_one)[1:9], colnames(pahs_comb)[10:22])

saveRDS(pest_de_1, "../cat_results/denoised/pest_de.rds")
saveRDS(pahs_de_1, "../cat_results/denoised/pahs_de.rds")
saveRDS(pest_comb_de_1, "../cat_results/denoised/pest_comb_de.rds")
saveRDS(pahs_comb_de_1, "../cat_results/denoised/pahs_comb_de.rds")



##### Test --
pest_test <- pest[, 1:5]
pest_test <- as.data.frame(model.matrix(~.,pest_test[,c(2,5)])[,-1])
pest_test <- cbind(pest_test, pest[,3:4])

model = glm(CountryFrance ~ Age + Gender, data=pest_test, family="binomial")
res <- model$residuals
view(res)


