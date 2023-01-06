
set.seed(3)
n.sweep = get.sweep.best.model(model, models = 1)
res.g = get.g.sweep(model, n.sweep$result, 1)
beta = sample.beta(model, res.g, Nmonte = 2, Nmonte.sigma = 2)

### Regression coefficient ----
## Boxplots summarising the posterior distribution of the
## regression coefficients for selected variables
filepath = "PesticidesPCBBDEs"
trait_group = c("Core","Core+FPG","All_traits")
i = 1
f = 4

res.g = readRDS(paste0("../Results/R2GUESS/",trait_group[i],"/Model",f,"_res.rds"))
beta = readRDS(paste0("../Results/R2GUESS/",trait_group[i],"/Model",f,"_beta.rds"))

model = readRDS(paste0("../Results/R2GUESS/",trait_group[i],"/Model",f,".rds"))

X = readRDS(paste0("../Processed/",filepath,"_expo_ndimp_naimp_log.rds"))

for(i in 1:length(res.g$predictors)){
  pdf(paste0("../Figures/R2GUESS/Model",f,"_",colnames(X)[as.numeric(res.g$predictors[i])],"_estimated_beta_",trait_group[i],".pdf"), height = 4)
  par(mar=c(5,5,3,1))
  variable <- as.character(res.g$predictors[i])
  indice <- match(variable,beta$predictors)
  indix1 <- (model$q)*(indice-1)+1
  matbeta <- as.matrix(beta$res.beta[,indix1:(indix1+model$q-1)])
  colnames(matbeta) <- model$label.Y 
  
  boxplot(matbeta,
          main = colnames(X)[as.numeric(res.g$predictors[i])],
          xlab = expression(beta),
          col = alpha(c("#ef7c10","#ef7c10","#00722a","#00722a","#00722a",
                        "#00722a","#dc251f","#dc251f","#009ee2"),0.5),
          horizontal = TRUE, outline = FALSE, las = 1)
  abline(v=0, lty = 2)
  abline(h=c(1:(ncol(matbeta)-1))+0.5, col = "grey", lty = 3)
  dev.off()
}