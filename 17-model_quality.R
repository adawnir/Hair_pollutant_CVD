# R2GUESS
rm(list=ls())
project_path=dirname(rstudioapi::getActiveDocumentContext()$path)
setwd(project_path)


library(R2GUESS)
library(tidyverse)
library(data.table)
library(formattable)


# source("graph_colours.R")
# library(colorspace)

# Ratio of Bayes Factor ----
ifelse(dir.exists("../Exports/"),"",dir.create("../Exports/"))

filepath = "PesticidesPCBBDEs"
X = readRDS(paste0("../Processed/",filepath,"_expo_ndimp_naimp_log.rds"))
trait = readRDS(paste0("../Data/",filepath,"_trait.rds"))
Y = trait %>% dplyr::select(BMI, WC, TG, TC, LDLC, HDLC, SBP, DBP, FPG)
trait_group = c(colnames(Y),"Core","Plus_FPG","All_traits")

f = 3

### Single-trait
rbf.table = mpp = sum.mpp = NULL
bvm.p.table = matrix(FALSE,nrow = 9, ncol = ncol(X))
for (i in 1:9){
  model = readRDS(paste0("../Results/GUESS-",trait_group[i],"/model",f,"_ESS.rds"))
  fdr.perm = readRDS(paste0(model$path.output,"/model",f,"_fdr_perm450.rds"))
  # Extract MPPI
  NameMarg = file.path(model$path.output, paste(model$root.file.output,"output_marg_prob_incl.txt",sep="_"))
  Marg  = read.table(NameMarg,header=TRUE)
  Marg$Predictor = colnames(X)
  
  print(fdr.perm$cutoff_int)
  if(!is.na(fdr.perm$cutoff_int)){
    mppi = Marg[Marg$Marg_Prob_Incl >= fdr.perm$cutoff_int,]
    prior_odds = 5/model$p/(1-5/model$p)
    post.odds = mppi$Marg_Prob_Incl/(1-mppi$Marg_Prob_Incl)
    post.odds[is.infinite(post.odds)] = (1-.Machine$double.base^.Machine$double.neg.ulp.digits)/.Machine$double.base^.Machine$double.neg.ulp.digits
    bf = post.odds/prior_odds
    bf.fdr = fdr.perm$cutoff_int/(1-fdr.perm$cutoff_int)/prior_odds
    rbf = bf/bf.fdr
    names(rbf) = mppi$Predictor
    rbf.table = as.data.frame(bind_rows(rbf.table, rbf))
    
    bestmodel = strsplit(model$BestModels$modelName[1], " ")[[1]]
    bvm.p.table[i, as.numeric(bestmodel)] = TRUE
  } else {
    rbf = NA
    rbf.table = rbind(rbf.table, rbf)
  }
  mpp = c(mpp, round(model$BestModels$postProb[1],2))
  sum.mpp = c(sum.mpp, round(sum(model$BestModels$postProb[1:5]),2))
}
rownames(rbf.table) = rownames(bvm.p.table) = trait_group[1:9]
colnames(bvm.p.table) = colnames(X)
bvm.p.table = bvm.p.table[,colnames(rbf.table)]

mytable = cbind(mpp, sum.mpp, log10(rbf.table))
mytable[is.na(mytable)] = ""
arr.ind = which(bvm.p.table,arr.ind = T)

custom_color_bar =  function(fun = "normalize", ...) {
  fun <- match.fun(fun)
  formatter("span", x ~ digits(x, digits = 2), style = function(y) style(
    display = "inline-block",
    direction = "rtl",
    "unicode-bidi" = "plaintext",
    "border-radius" = "4px",
    "padding-right" = "2px",
    "background-color" = ifelse(bvm.p.table,
                                "#dc251f","#a1a5a7"),
    width = percent(fun(as.numeric(y), ...))
  ))
}

formattable(mytable,
            align =rep("l",ncol(mytable)),
            list(mpp = color_bar("#93ceba"),
                 sum.mpp= color_bar("#00afac"),
                 area(1:nrow(mytable),3:ncol(mytable)) ~ custom_color_bar("normalize", na.rm = T))) %>%
  as.htmlwidget() %>% 
  htmlwidgets::saveWidget(file="../Exports/RBF_table_single_trait.html")

### Multi-trait ----
rbf.table = mpp = sum.mpp = NULL
bvm.p.table = matrix(FALSE,nrow = 3, ncol = ncol(X))
for (i in 10:12){
  model = readRDS(paste0("../Results/GUESS-",trait_group[i],"/model",f,"_ESS.rds"))
  fdr.perm = readRDS(paste0(model$path.output,"/model",f,"_fdr_perm50.rds"))
  # Extract MPPI
  NameMarg = file.path(model$path.output, paste(model$root.file.output,"output_marg_prob_incl.txt",sep="_"))
  Marg  = read.table(NameMarg,header=TRUE)
  Marg$Predictor = colnames(X)
  
  print(fdr.perm$cutoff_int)
  if(!is.na(fdr.perm$cutoff_int)){
    mppi = Marg[Marg$Marg_Prob_Incl >= fdr.perm$cutoff_int,]
    prior_odds = 5/model$p/(1-5/model$p)
    post.odds = mppi$Marg_Prob_Incl/(1-mppi$Marg_Prob_Incl)
    post.odds[is.infinite(post.odds)] = (1-.Machine$double.base^.Machine$double.neg.ulp.digits)/.Machine$double.base^.Machine$double.neg.ulp.digits
    bf = post.odds/prior_odds
    bf.fdr = fdr.perm$cutoff_int/(1-fdr.perm$cutoff_int)/prior_odds
    rbf = bf/bf.fdr
    names(rbf) = mppi$Predictor
    rbf.table = as.data.frame(bind_rows(rbf.table, rbf))
    
    bestmodel = strsplit(model$BestModels$modelName[1], " ")[[1]]
    bvm.p.table[which(i == 10:12), as.numeric(bestmodel)] = TRUE
  } else {
    rbf = NA
    rbf.table = rbind(rbf.table, rbf)
  }
  mpp = c(mpp, round(model$BestModels$postProb[1],2))
  sum.mpp = c(sum.mpp, round(sum(model$BestModels$postProb[1:5]),2))
}
rownames(rbf.table) = rownames(bvm.p.table) = trait_group[10:12]
colnames(bvm.p.table) = colnames(X)
bvm.p.table = bvm.p.table[,colnames(rbf.table)]

mytable = cbind(mpp, sum.mpp, log10(rbf.table))
mytable[is.na(mytable)] = ""
arr.ind = which(bvm.p.table,arr.ind = T)

custom_color_bar =  function(fun = "normalize", ...) {
  fun <- match.fun(fun)
  formatter("span", x ~ digits(x, digits = 2), style = function(y) style(
    display = "inline-block",
    direction = "rtl",
    "unicode-bidi" = "plaintext",
    "border-radius" = "4px",
    "padding-right" = "2px",
    "background-color" = ifelse(bvm.p.table,
                                "#dc251f","#a1a5a7"),
    width = percent(fun(as.numeric(y), ...))
  ))
}

formattable(mytable,
            align =rep("l",ncol(mytable)),
            list(mpp = color_bar("#93ceba"),
                 sum.mpp= color_bar("#00afac"),
                 area(1:nrow(mytable),3:ncol(mytable)) ~ custom_color_bar("normalize", na.rm = T))) %>%
  as.htmlwidget() %>% 
  htmlwidgets::saveWidget(file="../Exports/RBF_table_multi_trait.html")


# Predicting outcome ----
filepath = "PesticidesPCBBDEs"
f = 4

Y = readRDS(paste0("../Data/",filepath,"_trait.rds"))
trait_group = colnames(Y)[1:9]

r2mu_list = r2ui_list = r2li_list = NULL
for (i in c(1:2,4:9)){
  model = readRDS(paste0("../Results/R2GUESS/",trait_group[i],"/Model",f,".rds"))
  res.g = readRDS(paste0("../Results/R2GUESS/",trait_group[i],"/Model",f,"_res.rds"))
  beta = readRDS(paste0("../Results/R2GUESS/",trait_group[i],"/Model",f,"_beta.rds"))
  
  filepath = "PesticidesPCBBDEs"
  dataX = read.table(file.path(model$path.input,model$dataX),skip=2)
  dataY = read.table(file.path(model$path.input,model$dataY),skip=2)
  
  B = beta$res.beta
  X = as.matrix(dataX[,as.numeric(res.g$predictors)])
  centerX <- scale(X,scale=FALSE)
  
  yhat = apply(B, 1,
               function(x){
                 tmpB = matrix(x,nrow = length(res.g$predictors),ncol = model$q,byrow = TRUE)
                 return(centerX %*% tmpB)
               })
  r2mu = r2se = NULL
  for(l in 1:ncol(dataY)){
    indix1 = (nrow(dataY))*(l-1)+1
    matyhat = yhat[indix1:(indix1+nrow(dataY)-1),]
    r2 = cor(matyhat, dataY[,l])^2
    r2mu = c(r2mu, mean(r2))
    r2se = c(r2se, sd(r2)/sqrt(length(r2)))
  }
  r2mu_list = c(r2mu_list, r2mu)
  r2ui = r2mu + qnorm(0.975) * r2se
  r2li = r2mu - qnorm(0.975) * r2se
  r2ui_list = c(r2ui_list, r2ui)
  r2li_list = c(r2li_list, r2li)
}
names(r2mu_list) = trait_group[-3]

trait_group = c("Core","Core+FPG","All_traits")
r2summary = r2mu_list
for (i in 1:3){
  print(trait_group[i])
  model = readRDS(paste0("../Results/R2GUESS/",trait_group[i],"/Model",f,".rds"))
  res.g = readRDS(paste0("../Results/R2GUESS/",trait_group[i],"/Model",f,"_res.rds"))
  beta = readRDS(paste0("../Results/R2GUESS/",trait_group[i],"/Model",f,"_beta.rds"))
  
  filepath = "PesticidesPCBBDEs"
  dataX = read.table(file.path(model$path.input,model$dataX),skip=2)
  dataY = read.table(file.path(model$path.input,model$dataY),skip=2)
  
  B = beta$res.beta
  X = as.matrix(dataX[,as.numeric(res.g$predictors)])
  centerX <- scale(X,scale=FALSE)
  
  yhat = apply(B, 1,
               function(x){
                 tmpB = matrix(x,nrow = length(res.g$predictors),ncol = model$q,byrow = TRUE)
                 return(centerX %*% tmpB)
               })
  r2mu = r2se = NULL
  for(l in 1:ncol(dataY)){
    indix1 = (nrow(dataY))*(l-1)+1
    matyhat = yhat[indix1:(indix1+nrow(dataY)-1),]
    r2 = cor(matyhat, dataY[,l])^2
    r2mu = c(r2mu, mean(r2))
    r2se = c(r2se, sd(r2)/sqrt(length(r2)))
  }
  max(r2)
  min(r2)
  r2ui = r2mu + qnorm(0.975) * r2se
  r2li = r2mu - qnorm(0.975) * r2se
  names(r2mu) = model$label.Y
  r2summary = bind_rows(r2summary, r2mu)
}

order = c("BMI","WC","TG","HDLC","SBP","DBP","FPG","TC","LDLC")
r2summary = r2summary[,order]

pdf(paste0("../Figures/R2GUESS/Single_vs_multi_trait_error.pdf"), height = 5, width = 8)
par(mar=c(5,5,1,1))
plot(x = 1:ncol(r2summary), type = "n",
     ylim = c(0,0.2), ylab = expression(R^2), xlab = "", xaxt = "n")
for (i in 1:nrow(r2summary)){
  lines(x = 1:ncol(r2summary), y = r2summary[i,], type = "b",
        pch = c(1,3,4,8)[i], lty = i,
        col = c("#a1a5a7","#dc251f", "#009ee2", "#ef7c10")[i])
}
axis(side = 1, at = 1:ncol(r2summary), labels = colnames(r2summary))
legend("topright",
       legend = c("Single-trait", "Multiple-trait (6)", "Multiple-trait (7)", "Multiple-trait (9)"),
       pch = c(1,3,4,8), lty = 1:4, col = c("#a1a5a7","#dc251f", "#009ee2", "#ef7c10"), cex = 0.8)
dev.off()


# Summary of BVM ----
write.table(colnames(X), paste0(model$path.input,"/X.annotation.txt"))
## Variables included in the best models
pdf(paste0("../Figures/R2GUESS/Model",f-1,"_Variables_included_top20_models_",paths[j],".pdf"))
plotvariable(model, 20, file.annotation = paste0(model$path.input,"/X.annotation.txt"))
dev.off()
## Proximity between the best models
pdf(paste0("../Figures/R2GUESS/Model",f-1,"_Proximity_between_top20_models_",paths[j],".pdf"))
plotmodel(model, 20)
dev.off()
