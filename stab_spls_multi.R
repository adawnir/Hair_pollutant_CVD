
### Sumemr Project -- Stability selection sPLS with multivariate outcomes
## Created by Vivian on 05 August

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
pest_de <- readRDS("../results/denoised/pest_de.rds")
pahs_de <- readRDS("../results/denoised/pahs_de.rds")

pest_out <- readRDS("../processed/uni_multi_pest.rds")
pahs_out <- readRDS("../processed/uni_multi_pahs.rds")

x_pest <- pest_de[2:36]
y_pest <- pest_out[c(6:7,9:12, 14:16)]
x_pahs <- pahs_de[2:15]
y_pahs <- pahs_out[c(6:7,9:12, 14:16)]




##### Create directories --
ifelse(dir.exists("../figures/spls_multi"),"",dir.create("../figures/spls_multi"))
ifelse(dir.exists("../results/spls_multi"),"",dir.create("../results/spls_multi"))



##### Function --
ExplainedVariance=function(model,X,Y){
  if(dim(as.matrix(Y))[2] == 1){
    ev_X_X=model$explained_variance$X
    ev_Y_Y=model$explained_variance$Y
    p1 <- predict(model,X) 
    matmean <- matrix(mean(Y),ncol=1,nrow=length(Y),byrow=TRUE)
    sumYT <- colSums((Y-matmean)**2)
    cumP <- rep(0,model$ncomp)
    cumPY <- matrix(0,nrow=model$ncomp,ncol=1)
    for (k in 1:model$ncomp){
      selectedY <- which(model$loadings$Y[,k]!=0)
      sumPLS <- sum((p1$predict[,selectedY,k]-matmean[,selectedY])**2)
      cumPY[k,selectedY] <- (sumPLS/sumYT)[selectedY]
      cumP[k] <- mean((sumPLS/sumYT)[selectedY]) 
    }
    IndP <- diff(c(0,cumP))
    IndPY <- apply(cumPY, 2, function(x)(diff(c(0,x))))
    return(list(ev_X_by_X=ev_X_X, ev_Y_by_Y=ev_Y_Y, ev_Y_by_X=IndP, ev_each_Y_by_X = IndPY))
  }else{
    ev_X_X=model$explained_variance$X
    ev_Y_Y=model$explained_variance$Y
    p1 <- predict(model,X) 
    matmean <- matrix(colMeans(Y),ncol=dim(Y)[2],nrow=dim(Y)[1],byrow=TRUE)
    sumYT <- colSums((Y-matmean)**2)
    cumP <- rep(0,model$ncomp)
    cumPY <- matrix(0,nrow=model$ncomp,ncol=dim(Y)[2])
    for (k in 1:model$ncomp){
      selectedY <- which(model$loadings$Y[,k]!=0)
      sumPLS <- colSums((p1$predict[,selectedY,k]-matmean[,selectedY])**2)
      cumPY[k,selectedY] <- (sumPLS/sumYT)[selectedY]
      cumP[k] <- mean((sumPLS/sumYT)[selectedY]) 
    }
    IndP <- diff(c(0,cumP))
    IndPY <- apply(cumPY, 2, function(x)(diff(c(0,x))))
    return(list(ev_X_by_X=ev_X_X, ev_Y_by_Y=ev_Y_Y, ev_Y_by_X=IndP, ev_each_Y_by_X = IndPY))
    
  }
}

##### Unpenalised PLS --

pls_pest <- pls(x_pest, y_pest, ncomp = 3)
ev_pest <- ExplainedVariance(pls_pest, X=x_pest, Y=y_pest)

saveRDS(pls_pest$loadings$X, "../results/spls_multi/unpen_load_x_pest.rds")
saveRDS(pls_pest$loadings$Y, "../results/spls_multi/unpen_load_y_pest.rds")
saveRDS(ev_pest, "../results/spls_multi/unpen_ev_pest.rds")

pls_pahs <- pls(x_pahs, y_pahs, ncomp = 3)
ev_pahs <- ExplainedVariance(pls_pahs, X=x_pahs, Y=y_pahs)

saveRDS(pls_pahs$loadings$X, "../results/spls_multi/unpen_load_x_pahs.rds")
saveRDS(pls_pahs$loadings$Y, "../results/spls_multi/unpen_load_y_pahs.rds")
saveRDS(ev_pahs, "../results/spls_multi/unpen_ev_pahs.rds")


##### Split dataset--


##### sPLS: sparsity on x -- 
# change tau to 0.6 for better recalibration...
### Pest
out_x_pest <- BiSelection(xdata = x_pest, ydata = y_pest,
                          family = "gaussian", ncomp = 1, tau = 0.6,
                          LambdaX = 1:(ncol(x_pest) - 1),
                          implementation = SparsePLS)

saveRDS(out_x_pest, "../results/spls_multi/out_x_pest.rds")

# Variance explained
ind <- rownames(out_x_pest$summary_full[which.max(out_x_pest$summary_full[,4]),])
idx <- names(which(out_x_pest$selectedX_full[as.numeric(ind),] == 1))
idy <- names(which(out_x_pest$selectedY_full[as.numeric(ind),] == 1))

XX <- x_pest[,which(colnames(x_pest) %in% idx)]
YY <-  y_pest[,which(colnames(y_pest) %in% idy)]
XY.pls <- pls(XX, YY, ncomp = 3)
ev_x_pest <- ExplainedVariance(XY.pls, X=XX, Y=YY)

saveRDS(ev_x_pest, "../results/spls_multi/ev_x_pest.rds")

# Extracting average loadings (with recalibration)
loading_x_x_pest <- XY.pls$loadings$X
loading_x_y_pest <- XY.pls$loadings$Y

saveRDS(loading_x_x_pest, paste0("../results/spls_multi/loading_x_x_pest.rds"))
saveRDS(loading_x_y_pest, paste0("../results/spls_multi/loading_x_y_pest.rds"))


### PAHs
out_x_pahs <- BiSelection(xdata = x_pahs, ydata = y_pahs,
                          family = "gaussian", ncomp = 1, tau = 0.6,
                          LambdaX = 1:(ncol(x_pahs) - 1),
                          implementation = SparsePLS)

saveRDS(out_x_pahs, "../results/spls_multi/out_x_pahs.rds")

# Variance explained
ind <- rownames(out_x_pahs$summary_full[which.max(out_x_pahs$summary_full[,4]),])
idx <- names(which(out_x_pahs$selectedX_full[as.numeric(ind),] == 1))
idy <- names(which(out_x_pahs$selectedY_full[as.numeric(ind),] == 1))

XX <- x_pahs[,which(colnames(x_pahs) %in% idx)]
YY <-  y_pahs[,which(colnames(y_pahs) %in% idy)]
XY.pls <- pls(XX, YY, ncomp = 3)
ev_x_pahs <- ExplainedVariance(XY.pls, X=XX, Y=YY)

saveRDS(ev_x_pahs, "../results/spls_multi/ev_x_pahs.rds")

# Extracting average loadings (with recalibration)
loading_x_x_pahs <- XY.pls$loadings$X
loading_x_y_pahs <- XY.pls$loadings$Y

saveRDS(loading_x_x_pahs, paste0("../results/spls_multi/loading_x_x_pahs.rds"))
saveRDS(loading_x_y_pahs, paste0("../results/spls_multi/loading_x_y_pahs.rds"))



##### sPLS: sparsity on x and y -- 
### Pest
out_xy_pest <- BiSelection(xdata = x_pest, ydata = y_pest,
                          family = "gaussian", ncomp = 3, tau = 0.6,
                          LambdaX = 1:(ncol(x_pest) - 1),
                          LambdaY = 1:(ncol(y_pest) - 1),
                          implementation = SparsePLS)

saveRDS(out_xy_pest, "../results/spls_multi/out_xy_pest.rds")

# Variance explained
ind <- rownames(out_xy_pest$summary_full[which.max(out_xy_pest$summary_full[,6]),])
idx <- names(which(out_xy_pest$selectedX_full[as.numeric(ind),] == 1))
idy <- names(which(out_xy_pest$selectedY_full[as.numeric(ind),] == 1))

XX <- x_pest[,which(colnames(x_pest) %in% idx)]
YY <-  y_pest[,which(colnames(y_pest) %in% idy)]
XY.pls <- pls(XX, YY, ncomp = 3)
ev_xy_pest <- ExplainedVariance(XY.pls, X=XX, Y=YY)

saveRDS(ev_xy_pest, "../results/spls_multi/ev_xy_pest.rds")

# Extracting average loadings (with recalibration)
loading_xy_x_pest <- XY.pls$loadings$X
loading_xy_y_pest <- XY.pls$loadings$Y

saveRDS(loading_xy_x_pest, paste0("../results/spls_multi/loading_xy_x_pest.rds"))
saveRDS(loading_xy_y_pest, paste0("../results/spls_multi/loading_xy_y_pest.rds"))


### PAHs
out_xy_pahs <- BiSelection(xdata = x_pahs, ydata = y_pahs,
                           family = "gaussian", ncomp = 3, tau = 0.6,
                           LambdaX = 1:(ncol(x_pahs) - 1),
                           LambdaY = 1:(ncol(y_pahs) - 1),
                           implementation = SparsePLS)

saveRDS(out_xy_pahs, "../results/spls_multi/out_xy_pahs.rds")

# Variance explained
ind <- rownames(out_xy_pahs$summary_full[which.max(out_xy_pahs$summary_full[,6]),])
idx <- names(which(out_xy_pahs$selectedX_full[as.numeric(ind),] == 1))
idy <- names(which(out_xy_pahs$selectedY_full[as.numeric(ind),] == 1))

XX <- x_pahs[,which(colnames(x_pahs) %in% idx)]
YY <-  y_pahs[,which(colnames(y_pahs) %in% idy)]
XY.pls <- pls(XX, YY, ncomp = 3)
ev_xy_pahs <- ExplainedVariance(XY.pls, X=XX, Y=YY)

saveRDS(ev_xy_pahs, "../results/spls_multi/ev_xy_pahs.rds")

# Extracting average loadings (with recalibration)
loading_xy_x_pahs <- XY.pls$loadings$X
loading_xy_y_pahs <- XY.pls$loadings$Y

saveRDS(loading_xy_x_pahs, paste0("../results/spls_multi/loading_xy_x_pahs.rds"))
saveRDS(loading_xy_y_pahs, paste0("../results/spls_multi/loading_xy_y_pest.rds"))




