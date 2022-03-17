
### Sumemr Project -- Stability selection sPLS with multivariate outcomes strat by country
## Created by Vivian on 13 August

rm(list=ls())
project_path="~/hair/scripts/"
setwd(project_path)

#install.packages("fastDummies")
#library(fastDummies)

library(devtools)
#devtools::install("focus-master")
library(focus)


##### Load data set --
pest_de <- readRDS("../results/denoised/pest_de.rds")

pest_out <- readRDS("../processed/uni_multi_pest.rds")

x_pest <- pest_de[4:36]
y_pest <- pest_out[c(6:7,9:12, 14:16)]

id_b <- which(pest_out$Country == "Belgium")
id_l <- which(pest_out$Country == "Luxembourg")
id_f <- which(pest_out$Country == "France")

x_pest_b <- x_pest[id_b,]
y_pest_b <- y_pest[id_b,]
x_pest_l <- x_pest[id_l,]
y_pest_l <- y_pest[id_l,]
x_pest_f <- x_pest[id_f,]
y_pest_f <- y_pest[id_f,]


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


##### Create directories --
ifelse(dir.exists("../figures/strat_country_spls_multi"),"",dir.create("../figures/strat_country_spls_multi"))
ifelse(dir.exists("../results/strat_country_spls_multi"),"",dir.create("../results/strat_country_spls_multi"))


##### sPLS: sparsity on x and y -- 
### Pest L
out_xy_pest_l <- BiSelection(xdata = x_pest_l, ydata = y_pest_l,
                           family = "gaussian", ncomp = 3, tau = 0.6,
                           LambdaX = 1:(ncol(x_pest_l) - 1),
                           LambdaY = 1:(ncol(y_pest_l) - 1),
                           implementation = SparsePLS)

saveRDS(out_xy_pest_l, "../results/strat_country_spls_multi/out_xy_pest_l.rds")

# Variance explained
ind <- rownames(out_xy_pest_l$summary_full[which.max(out_xy_pest_l$summary_full[,6]),])
idx <- names(which(out_xy_pest_l$selectedX_full[as.numeric(ind),] == 1))
idy <- names(which(out_xy_pest_l$selectedY_full[as.numeric(ind),] == 1))

XX <- x_pest_l[,which(colnames(x_pest_l) %in% idx)]
YY <-  y_pest_l[,which(colnames(y_pest_l) %in% idy)]
XY.pls <- pls(XX, YY, ncomp = 3)
ev_xy_pest_l <- ExplainedVariance(XY.pls, X=XX, Y=YY)

saveRDS(ev_xy_pest_l, "../results/strat_country_spls_multi/ev_xy_pest_l.rds")

# Extracting average loadings (with recalibration)
loading_xy_x_pest_l <- XY.pls$loadings$X
loading_xy_y_pest_l <- XY.pls$loadings$Y

saveRDS(loading_xy_x_pest_l, paste0("../results/strat_country_spls_multi/loading_xy_x_pest_l.rds"))
saveRDS(loading_xy_y_pest_l, paste0("../results/strat_country_spls_multi/loading_xy_y_pest_l.rds"))


### Pest B
out_xy_pest_b <- BiSelection(xdata = x_pest_b, ydata = y_pest_b,
                             family = "gaussian", ncomp = 3, tau = 0.6,
                             LambdaX = 1:(ncol(x_pest_b) - 1),
                             LambdaY = 1:(ncol(y_pest_b) - 1),
                             implementation = SparsePLS)

saveRDS(out_xy_pest_b, "../results/strat_country_spls_multi/out_xy_pest_b.rds")

# Variance explained
ind <- rownames(out_xy_pest_b$summary_full[which.max(out_xy_pest_b$summary_full[,6]),])
idx <- names(which(out_xy_pest_b$selectedX_full[as.numeric(ind),] == 1))
idy <- names(which(out_xy_pest_b$selectedY_full[as.numeric(ind),] == 1))

XX <- x_pest_b[,which(colnames(x_pest_b) %in% idx)]
YY <-  y_pest_b[,which(colnames(y_pest_b) %in% idy)]
XY.pls <- pls(XX, YY, ncomp = 3)
ev_xy_pest_b <- ExplainedVariance(XY.pls, X=XX, Y=YY)

saveRDS(ev_xy_pest_b, "../results/strat_country_spls_multi/ev_xy_pest_b.rds")

# Extracting average loadings (with recalibration)
loading_xy_x_pest_b <- XY.pls$loadings$X
loading_xy_y_pest_b <- XY.pls$loadings$Y

saveRDS(loading_xy_x_pest_b, paste0("../results/strat_country_spls_multi/loading_xy_x_pest_b.rds"))
saveRDS(loading_xy_y_pest_b, paste0("../results/strat_country_spls_multi/loading_xy_y_pest_b.rds"))


### Pest F
out_xy_pest_f <- BiSelection(xdata = x_pest_f, ydata = y_pest_f,
                             family = "gaussian", ncomp = 3, tau = 0.6,
                             LambdaX = 1:(ncol(x_pest_f) - 1),
                             LambdaY = 1:(ncol(y_pest_f) - 1),
                             implementation = SparsePLS)

saveRDS(out_xy_pest_f, "../results/strat_country_spls_multi/out_xy_pest_f.rds")

# Variance explained
ind <- rownames(out_xy_pest_f$summary_full[which.max(out_xy_pest_f$summary_full[,6]),])
idx <- names(which(out_xy_pest_f$selectedX_full[as.numeric(ind),] == 1))
idy <- names(which(out_xy_pest_f$selectedY_full[as.numeric(ind),] == 1))

XX <- x_pest_f[,which(colnames(x_pest_f) %in% idx)]
YY <-  y_pest_f[,which(colnames(y_pest_f) %in% idy)]
XY.pls <- pls(XX, YY, ncomp = 3)
ev_xy_pest_f <- ExplainedVariance(XY.pls, X=XX, Y=YY)

saveRDS(ev_xy_pest_f, "../results/strat_country_spls_multi/ev_xy_pest_f.rds")

# Extracting average loadings (with recalibration)
loading_xy_x_pest_f <- XY.pls$loadings$X
loading_xy_y_pest_f <- XY.pls$loadings$Y

saveRDS(loading_xy_x_pest_f, paste0("../results/strat_country_spls_multi/loading_xy_x_pest_f.rds"))
saveRDS(loading_xy_y_pest_f, paste0("../results/strat_country_spls_multi/loading_xy_y_pest_f.rds"))
