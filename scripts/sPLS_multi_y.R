
### Sumemr Project -- Stability selection sPLS with multivariate outcomes
## Created by Vivian on 27 July

rm(list=ls())
project_path="~/hair/scripts/"
setwd(project_path)

#install.packages("fastDummies")
#library(fastDummies)

library(devtools)
#devtools::install("focus-master")
library(focus)

##### Parameters
args=commandArgs(trailingOnly=TRUE)
m=as.numeric(args[1])


##### Load data set --
m = 2
arr <- c("pest_multi_redu", "pahs_multi_redu")[m]
dat=readRDS(paste0("../results/denoised_multi_y/",arr,".rds"))


##### Split data --
s <- NULL
for (mycat in levels(factor(dat$outcomes))) {
  set.seed(1000)
  scat <- sample(which(dat$outcomes == mycat), size = 0.7 * sum(dat$outcomes == mycat), replace = F)
  s <- c(s, scat)
}

y <- dat$outcomes
x <- dat[, -c(1, ncol(dat))]
x_train <- x[s, ]
y_train <- y[s]
x_test <- x[-s, ]
y_test <- y[-s]


##### Create directories --
ifelse(dir.exists("../figures/spls_multi"),"",dir.create("../figures/spls_multi"))
ifelse(dir.exists("../results/spls_multi"),"",dir.create("../results/spls_multi"))


##### Run sPLS stability models--
stab <- BiSelection(xdata = x_train, ydata = cbind(y_train),
                     family = "binomial", ncomp = 3, 
                     LambdaX = 1:(ncol(x_train) - 1),
                     implementation = SparsePLS)

saveRDS(stab, paste0("../results/spls_multi/out_",arr, ".rds"))


### Variance explained
ind <- rownames(stab$summary_full[which.max(stab$summary_full[,4]),])
idx <- names(which(stab$selectedX_full[as.numeric(ind),] == 1))

XX <- x_train[,which(colnames(x_train) %in% idx)]
YY <-  as.factor(y_train)
XY.pls <- plsda(XX, YY, ncomp = 3)

ev_X_X <- XY.pls$Xvar/XY.pls$Xtotvar


saveRDS(ev_X_X, paste0("../results/spls_multi/ev_X_X_",arr, ".rds"))


### Extracting average loadings (with recalibration)
loading_x <- XY.pls$loadings
loading_y <- XY.pls$Yloadings

saveRDS(loading_x, paste0("../results/spls_multi/loading_x_",arr, ".rds"))
saveRDS(loading_y, paste0("../results/spls_multi/loading_y_",arr, ".rds"))


predictions_re <- predict(XY.pls, newdata=x_test[,which(colnames(x_test) %in% idx)])
orin_pred_matrix <- cbind(y_test, predictions_re)

saveRDS(orin_pred_matrix, paste0("../results/spls_multi/pred_mat_",arr, ".rds"))

