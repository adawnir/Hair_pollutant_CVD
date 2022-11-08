
# R2GUESS ----
write.table(colnames(X), paste0(model$path.input,"/X.annotation.txt"))

## Variables included in the best models
pdf(paste0("../Figures/R2GUESS/",model_type,"_Variables_included_top20_models_",data_type,".pdf"))
plotvariable(model, 20, file.annotation = paste0(model$path.input,"/X.annotation.txt"))
dev.off()
## Proximity between the best models
pdf(paste0("../Figures/R2GUESS/",model_type,"_Proximity_between_top20_models_",data_type,".pdf"))
plotmodel(model, 20)
dev.off()

# Validation ----
data = readRDS(paste0("../Processed/Overlap_test_XY.rds"))

Y_test = data[,c(17,19,20,22:25)] # remove WC and TC

tmp = model.matrix(~ Smoking + Education + Physical_activity + Alcohol, data)[,-1]
X_test = cbind(data$Age,
               data$Gender,
               tmp[,1:2],
               data[,c("DietScore1","DietScore2","DietScore3")],
               data[,as.numeric(res.g$predictors)+26])
X_test = X_test %>% mutate_if(is.factor, function(x) as.integer(x)-1)

train = readRDS(paste0("../Processed/",filepaths[m],"_XY.rds"))
Y_train = train[,c(17,19,20,22:25)] # remove WC and TC

tmp = model.matrix(~ Smoking + Education + Physical_activity + Alcohol, train)[,-1]
X_train = cbind(train$Age,
                train$Gender,
                tmp[,1:2],
                train[,c("DietScore1","DietScore2","DietScore3")],
                train[,c(27:ncol(train))])
X_train = X_train %>% mutate_if(is.factor, function(x) as.integer(x)-1)

colnames(X_train)[1:7] = colnames(X_test)[1:7]

# Recalibration
mypls = pls(X_train, Y_train, ncomp=1)
Y_pred=predict(mypls, newdata=X_test)
myloadings = mypls$loadings$X
residuals = Y_test-Y_pred
rmse = sqrt(crossprod(residuals)/length(residuals))