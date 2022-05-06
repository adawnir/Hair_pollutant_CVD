rm(list=ls())

library(mixOmics)

ExplainedVariance=function(model,X,Y){
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
  return(list(ev_X_by_X=ev_X_X, ev_Y_by_Y=ev_Y_Y, ev_Y_by_X=IndP, ev_each_Y_by_X = cumPY))
}

# Example
data(liver.toxicity)
X <- liver.toxicity$gene
Y <- liver.toxicity$clinic
toxicity.pls <- pls(X, Y, ncomp = 3)
ExplainedVariance(toxicity.pls, X=X, Y=Y)
