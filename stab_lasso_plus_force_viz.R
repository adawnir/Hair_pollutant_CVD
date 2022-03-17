
### Sumemr Project -- Stability selection LASSO Logistic Regression with forced variables visualisation
## Created by Vivian on 11 August

rm(list=ls())
project_path="~/hair/scripts"
setwd(project_path)

source("penalisation_functions.R")

library(RColorBrewer)
library(tidyverse)
library(plotrix)
library(colorspace)
library(cowplot)
library(ggrepel)


pest <- readRDS("../processed/imputation/imp_pest.rds")
pahs <- readRDS("../processed/imputation/imp_pahs.rds")


##### Load selection proportion --

for (m in 1:10){
  arr=paste0(rep(c("pest1", "pahs1"), each = 5), "_", 
             c("obesity", "hypertension", "diabetes", "Lipid_disorder", "MS"))[m]
  ### Lasso
  lasso_forced_out=readRDS(paste0("../results/lasso_forced/out_",arr,".rds")) # Load output
  
  lasso_forced_hat_params=GetArgmax(lasso_forced_out) # Extract calibrated pi
  assign(paste0("lasso_hat_params_",arr), lasso_forced_hat_params) # Assign name
  
  lasso_forced_calib=sum(CalibratedStableRegression(lasso_forced_out)) # Number of selected variables
  assign(paste0("lasso_calib_",arr),lasso_forced_calib) # Assign name
  
  lasso_selprop=readRDS(paste0("../results/lasso_forced/selprop_",arr,".rds")) # Load selection proportion
  assign(paste0("lasso_selprop_",arr),lasso_selprop) # Assign name
  
  lasso_forced_beta=readRDS(paste0("../results/lasso_forced/average_beta_",arr,".rds"))
  lasso_forced_beta=ifelse(CalibratedStableRegression(lasso_forced_out) == 1, lasso_forced_beta, 0) # Shrink non-selected beta to zero
  lasso_forced_beta=exp(lasso_forced_beta) # Exponentiate to OR
  assign(paste0("lasso_beta_",arr),lasso_forced_beta) 
}


for (m in 1:10){
  arr=paste0(rep(c("pest_de", "pahs_de"), each = 5), "_", 
             c("obesity", "hypertension", "diabetes", "Lipid_disorder", "MS"))[m]
  
  ### Lasso
  lasso_out=readRDS(paste0("../results/lasso_1000/out_",arr,".rds")) # Load output
  assign(paste0("lasso_out_",arr),lasso_out) # Assign name
  
  lasso_hat_params=GetArgmax(lasso_out) # Extract calibrated pi
  assign(paste0("lasso_hat_params_",arr), lasso_hat_params) # Assign name
  
  lasso_selprop=readRDS(paste0("../results/lasso_1000/selprop_",arr,".rds")) # Load selection proportion
  assign(paste0("lasso_selprop_",arr),lasso_selprop) # Assign name
  
  lasso_beta=readRDS(paste0("../results/lasso_1000/average_beta_",arr,".rds")) # Load beta
  lasso_beta=ifelse(CalibratedStableRegression(lasso_out) == 1, lasso_beta, 0)
  lasso_beta <- exp(lasso_beta)
  assign(paste0("lasso_beta_",arr),lasso_beta) # Assign name
  
}


##### Reorder selprop --
lasso_beta_pahs1_diabetes<- lasso_beta_pahs1_diabetes[c(3:16)]
lasso_beta_pahs1_obesity <- lasso_beta_pahs1_obesity[c(3:16)]
lasso_beta_pahs1_Lipid_disorder <- lasso_beta_pahs1_Lipid_disorder[c(3:16)]
lasso_beta_pahs1_hypertension <- lasso_beta_pahs1_hypertension[c(3:16)]
lasso_beta_pahs1_MS <- lasso_beta_pahs1_MS[c(3:16)]

lasso_beta_pest1_diabetes<- lasso_beta_pest1_diabetes[c(3:37)]
lasso_beta_pest1_obesity <- lasso_beta_pest1_obesity[c(3:37)]
lasso_beta_pest1_Lipid_disorder <- lasso_beta_pest1_Lipid_disorder[c(3:37)]
lasso_beta_pest1_hypertension <- lasso_beta_pest1_hypertension[c(3:37)]
lasso_beta_pest1_MS <- lasso_beta_pest1_MS[c(3:37)]


##### Functions --
mycolours = brewer.pal(10, "Paired")
outcomes <- c("Obesity", "Lipid disorder", "Hypertension", "Diabetes","Metabolic syndrome")

foo3 = function(l1, l2, l3, l4, l5, s1, s2, s3, s4, s5, order){
  x=data.frame(s1)
  x[,2] <- s2[match(rownames(x),names(s2))]
  x[,3] <- s3[match(rownames(x),names(s3))]
  x[,4] <- s4[match(rownames(x),names(s4))]
  x[,5] <- s5[match(rownames(x),names(s5))]
  x[,6] <- l1[match(rownames(x),names(l1))]
  x[,7] <- l2[match(rownames(x),names(l2))]
  x[,8] <- l3[match(rownames(x),names(l3))]
  x[,9] <- l4[match(rownames(x),names(l4))]
  x[,10] <- l5[match(rownames(x),names(l5))]
  x = slice(x, match(order,rownames(x)))
  df=data.frame(or=unlist(x[,6:10]),or_forced=unlist(x[,1:5]),
                var=rep(rownames(x),5),
                outcomes = rep(outcomes, each=14)) # change accordingly
}


foo2 = function(l1, l2, l3, l4, l5, s1, s2, s3, s4, s5, order){
  x=data.frame(s1)
  x[,2] <- s2[match(rownames(x),names(s2))]
  x[,3] <- s3[match(rownames(x),names(s3))]
  x[,4] <- s4[match(rownames(x),names(s4))]
  x[,5] <- s5[match(rownames(x),names(s5))]
  x[,6] <- l1[match(rownames(x),names(l1))]
  x[,7] <- l2[match(rownames(x),names(l2))]
  x[,8] <- l3[match(rownames(x),names(l3))]
  x[,9] <- l4[match(rownames(x),names(l4))]
  x[,10] <- l5[match(rownames(x),names(l5))]
  x = slice(x, match(order,rownames(x)))
  df=data.frame(sel_lasso=unlist(x[,6:10]),sel_forced=unlist(x[,1:5]),
                var=rep(rownames(x),5),
                outcomes = rep(outcomes, each=14)) # change accordingly
}


myorder <- names(lasso_beta_pest_de_obesity)
coef_pest<- foo3(lasso_beta_pest_de_obesity, lasso_beta_pest_de_Lipid_disorder,
                 lasso_beta_pest_de_hypertension, lasso_beta_pest_de_diabetes,
                 lasso_beta_pest_de_MS, 
                 lasso_beta_pest1_obesity, lasso_beta_pest1_Lipid_disorder,
                 lasso_beta_pest1_hypertension, lasso_beta_pest1_diabetes,
                 lasso_beta_pest1_MS, 
                 myorder)

myorder <- names(lasso_selprop_pest_de_obesity)
selprop_pest<- foo2(lasso_selprop_pest_de_obesity, lasso_selprop_pest_de_Lipid_disorder,
                    lasso_selprop_pest_de_hypertension, lasso_selprop_pest_de_diabetes,
                    lasso_selprop_pest_de_MS, 
                    lasso_selprop_pest1_obesity, lasso_selprop_pest1_Lipid_disorder,
                    lasso_selprop_pest1_hypertension, lasso_selprop_pest1_diabetes,
                    lasso_selprop_pest1_MS, 
                    myorder)


myorder <- names(lasso_beta_pahs_de_obesity)
coef_pahs<- foo3(lasso_beta_pahs_de_obesity, lasso_beta_pahs_de_Lipid_disorder,
                 lasso_beta_pahs_de_hypertension, lasso_beta_pahs_de_diabetes,
                 lasso_beta_pahs_de_MS, 
                 lasso_beta_pahs1_obesity, lasso_beta_pahs1_Lipid_disorder,
                 lasso_beta_pahs1_hypertension, lasso_beta_pahs1_diabetes,
                 lasso_beta_pahs1_MS, 
                 myorder)

myorder <- names(lasso_selprop_pahs_de_obesity)
selprop_pahs<- foo2(lasso_selprop_pahs_de_obesity, lasso_selprop_pahs_de_Lipid_disorder,
                    lasso_selprop_pahs_de_hypertension, lasso_selprop_pahs_de_diabetes,
                    lasso_selprop_pahs_de_MS, 
                    lasso_selprop_pahs1_obesity, lasso_selprop_pahs1_Lipid_disorder,
                    lasso_selprop_pahs1_hypertension, lasso_selprop_pahs1_diabetes,
                    lasso_selprop_pahs1_MS, 
                    myorder)


##### Beta plot --
# Pesticides
xlim=c(0.6, 1.4)
ylim=c(0.8,1.4)
#coef_lung$var_cat=rep(variable_cat)
coef_pest$label=rep(1:(nrow(coef_pest)/5), 5)
coef_pest$mycolour_point=rep(mycolours[c(2,4,6,8,10)],each = 35)

coef_pest$mycolour_lab=darken(coef_pest$mycolour_point, amount=0.2)
coef_pest$myshape <- rep(c(15, 16, 17, 18, 25),  each = 35)

p1=ggplot(coef_pest, aes(or, or_forced,
                         label=ifelse(((abs(or-1)<0.05&or_forced==1)|(or==1&abs(or_forced-1)<0.05)),
                                      "",label))) +
  geom_vline(aes(xintercept = 1),linetype = "dashed",colour = "black") +
  geom_hline(aes(yintercept = 1),linetype = "dashed",colour = "black") +
  geom_point(shape=coef_pest$myshape, colour=coef_pest$mycolour_point, size = 3) +
  geom_text_repel(color=coef_pest$mycolour_lab,
                  nudge_y = 0.02,
                  nudge_x = 0.02,
                  size=5, segment.color="grey",
                  segment.size=0.2) + 
  xlab("Mean Odds Ratio (LASSO)")+
  ylab("Mean Odds Ratio (LASSO forced model)") +
  ggtitle("POPs and non-persistent pesticides") +
  xlim(xlim) +
  ylim(ylim) +
  theme_bw() +
  theme(legend.position = "none")

pdf("../figures/lasso_forced/ors_pest.pdf", width=7, height=7)
p1
dev.off()


# PAHs
xlim=c(0.9, 1.3)
ylim=c(0.9,1.2)
#coef_lung$var_cat=rep(variable_cat)
coef_pahs$label=rep(1:(nrow(coef_pahs)/5), 5)
coef_pahs$mycolour_point=rep(mycolours[c(2,4,6,8,10)],each = 14)

coef_pahs$mycolour_lab=darken(coef_pahs$mycolour_point, amount=0.2)
coef_pahs$myshape <- rep(c(15, 16, 17, 18, 25),  each = 14)

p2=ggplot(coef_pahs, aes(or, or_forced,
                         label=ifelse(((abs(or-1)<0.05&or_forced==1)|(or==1&abs(or_forced-1)<0.05)),
                                      "",label))) +
  geom_vline(aes(xintercept = 1),linetype = "dashed",colour = "black") +
  geom_hline(aes(yintercept = 1),linetype = "dashed",colour = "black") +
  geom_point(shape=coef_pahs$myshape, colour=coef_pahs$mycolour_point, size = 3) +
  geom_text_repel(color=coef_pahs$mycolour_lab,
                  nudge_y = 0.02,
                  nudge_x = 0.02,
                  size=5, segment.color="grey",
                  segment.size=0.2) + 
  xlab("Mean Odds Ratio (LASSO)")+
  ylab("Mean Odds Ratio (LASSO forced model)") +
  ggtitle("Metabolites of PAHs and tobacco-related chemicals") +
  xlim(xlim) +
  ylim(ylim) +
  theme_bw() +
  theme(legend.position = "none")

pdf("../figures/lasso_forced/ors_pahs.pdf", width=7, height=7)
p2
dev.off()



##### Selection proportion --
# Pesticides
xlim=c(0, 1)
ylim=c(0, 1)
#coef_lung$var_cat=rep(variable_cat)
selprop_pest$label=rep(1:(nrow(selprop_pest)/5), 5)
selprop_pest$mycolour_point=rep(mycolours[c(2,4,6,8,10)],each = 35)

selprop_pest$mycolour_lab=darken(selprop_pest$mycolour_point, amount=0.2)
selprop_pest$myshape <- rep(c(15, 16, 17, 18, 25),  each = 35)

id1 <- ifelse(selprop_pest$sel_lasso[1:35]<0.9 & selprop_pest$sel_forced[1:35]<0.9, "N", "Y") 
id2 <- ifelse(selprop_pest$sel_lasso[36:70]<0.83 & selprop_pest$sel_forced[36:70]<0.9, "N", "Y") 
id3 <- ifelse(selprop_pest$sel_lasso[71:105]<0.83 & selprop_pest$sel_forced[71:105]<0.9, "N", "Y") 
id4 <- ifelse(selprop_pest$sel_lasso[106:140]<0.88 & selprop_pest$sel_forced[106:140]<0.9, "N", "Y") 
id5 <- ifelse(selprop_pest$sel_lasso[141:175]<0.88 & selprop_pest$sel_forced[141:175]<0.88, "N", "Y") 

selprop_pest$label_ny <- c(id1, id2, id3, id4, id5)

p3=ggplot(selprop_pest, aes(sel_lasso, sel_forced,
                            label=ifelse(label_ny == "N","",label))) +
  geom_abline(slope = 1,linetype = "dotted",colour = "grey") +
  geom_vline(aes(xintercept = 0.9),linetype = "dashed",colour = lighten(mycolours[2], amount = 0.2)) +
  geom_vline(aes(xintercept = 0.83),linetype = "dashed",colour = lighten(mycolours[4], amount = 0.2)) +
  geom_vline(aes(xintercept = 0.83),linetype = "dashed",colour = lighten(mycolours[6], amount = 0.2)) +
  geom_vline(aes(xintercept = 0.88),linetype = "dashed",colour = lighten(mycolours[8], amount = 0.2)) +
  geom_vline(aes(xintercept = 0.88),linetype = "dashed",colour = lighten(mycolours[10], amount = 0.2)) +
  geom_hline(aes(yintercept = 0.9),linetype = "dashed",colour = lighten(mycolours[2], amount = 0.2)) +
  geom_hline(aes(yintercept = 0.9),linetype = "dashed",colour = lighten(mycolours[4], amount = 0.2)) +
  geom_hline(aes(yintercept = 0.9),linetype = "dashed",colour = lighten(mycolours[6], amount = 0.2)) +
  geom_hline(aes(yintercept = 0.9),linetype = "dashed",colour = lighten(mycolours[8], amount = 0.2)) +
  geom_hline(aes(yintercept = 0.88),linetype = "dashed",colour = lighten(mycolours[10], amount = 0.2)) +
  geom_point(shape=selprop_pest$myshape, colour=selprop_pest$mycolour_point, size = 2.5) +
  geom_text_repel(color=selprop_pest$mycolour_lab,
                  nudge_y = 0.02,
                  nudge_x = 0.02,
                  size=3, segment.color="grey",
                  segment.size=0.2) + 
  xlab("Selection Proportion (LASSO)")+
  ylab("Selection Proportion (LASSO forced model)") +
  labs(title = "POPs and non-persistent pesticides") +
  xlim(xlim) +
  ylim(ylim) +
  theme_bw() +
  theme(legend.position = "none")

pdf("../figures/lasso_forced/selprop_pest.pdf", width=7, height=7)
p3
dev.off()



# PAHs
xlim=c(0, 1)
ylim=c(0, 1)
#coef_lung$var_cat=rep(variable_cat)
selprop_pahs$label=rep(1:(nrow(selprop_pahs)/5), 5)
selprop_pahs$mycolour_point=rep(mycolours[c(2,4,6,8,10)],each = 14)

selprop_pahs$mycolour_lab=darken(selprop_pahs$mycolour_point, amount=0.2)
selprop_pahs$myshape <- rep(c(15, 16, 17, 18, 25),  each = 14)

id1 <- ifelse(selprop_pahs$sel_lasso[1:14]<0.75 & selprop_pahs$sel_forced[1:14]<0.9, "N", "Y") 
id2 <- ifelse(selprop_pahs$sel_lasso[15:28]<0.72 & selprop_pahs$sel_forced[15:28]<0.9, "N", "Y") 
id3 <- ifelse(selprop_pahs$sel_lasso[29:42]<0.74 & selprop_pahs$sel_forced[29:42]<0.9, "N", "Y") 
id4 <- ifelse(selprop_pahs$sel_lasso[43:56]<0.78 & selprop_pahs$sel_forced[43:56]<0.9, "N", "Y") 
id5 <- ifelse(selprop_pahs$sel_lasso[57:70]<0.87 & selprop_pahs$sel_forced[57:70]<0.89, "N", "Y") 

selprop_pahs$label_ny <- c(id1, id2, id3, id4, id5)

p4=ggplot(selprop_pahs, aes(sel_lasso, sel_forced,
                            label=ifelse(label_ny == "N","",label))) +
  geom_abline(slope = 1,linetype = "dotted",colour = "grey") +
  geom_vline(aes(xintercept = 0.75),linetype = "dashed",colour = lighten(mycolours[2], amount = 0.2)) +
  geom_vline(aes(xintercept = 0.72),linetype = "dashed",colour = lighten(mycolours[4], amount = 0.2)) +
  geom_vline(aes(xintercept = 0.74),linetype = "dashed",colour = lighten(mycolours[6], amount = 0.2)) +
  geom_vline(aes(xintercept = 0.78),linetype = "dashed",colour = lighten(mycolours[8], amount = 0.2)) +
  geom_vline(aes(xintercept = 0.87),linetype = "dashed",colour = lighten(mycolours[10], amount = 0.2)) +
  geom_hline(aes(yintercept = 0.9),linetype = "dashed",colour = lighten(mycolours[2], amount = 0.2)) +
  geom_hline(aes(yintercept = 0.9),linetype = "dashed",colour = lighten(mycolours[4], amount = 0.2)) +
  geom_hline(aes(yintercept = 0.9),linetype = "dashed",colour = lighten(mycolours[6], amount = 0.2)) +
  geom_hline(aes(yintercept = 0.9),linetype = "dashed",colour = lighten(mycolours[8], amount = 0.2)) +
  geom_hline(aes(yintercept = 0.89),linetype = "dashed",colour = lighten(mycolours[10], amount = 0.2)) +
  geom_point(shape=selprop_pahs$myshape, colour=selprop_pahs$mycolour_point, size = 2.5) +
  geom_text_repel(color=selprop_pahs$mycolour_lab,
                  nudge_y = 0.02,
                  nudge_x = 0.02,
                  size=3, segment.color="grey",
                  segment.size=0.2) + 
  xlab("Selection Proportion (LASSO)")+
  ylab("Selection Proportion (LASSO forced model)") +
  labs(title = "Metabolites of PAHs and tobacco-related chemicals") +
  xlim(xlim) +
  ylim(ylim) +
  theme_bw() +
  theme(legend.position = "none")



pdf("../figures/lasso_forced/selprop_pahs.pdf", width=7, height=7)
p4
dev.off()



##### Prediction performance --

rm(list=ls())
project_path="~/hair/scripts"
setwd(project_path)

source("penalisation_functions.R")

library(RColorBrewer)
library(tidyverse)
library(plotrix)
library(colorspace)
library(cowplot)
library(ggrepel)


pahs <- readRDS("../processed/imputation/imp_pahs.rds")
pest <- readRDS("../processed/imputation/imp_pest.rds")

for (m in 1:10){
  arr=paste0(rep(c("pahs_de", "pest_de"), each = 5), "_", 
             c("obesity", "hypertension", "diabetes","MS", "Lipid_disorder"))[m]
  ### Lasso
  lasso_out=readRDS(paste0("../results/lasso/out_",arr,".rds")) # Load output
  assign(paste0("lasso_out_",arr),lasso_out) # Assign name
  lasso_calib=sum(CalibratedStableRegression(lasso_out)) # Number of selected variables
  assign(paste0("lasso_calib_",arr),lasso_calib) # Assign name
  
  lasso_sensitivities <- readRDS(paste0("../results/lasso_1000/sensitivities_",arr,".rds")) # Load roc (recalibrated)
  lasso_ave_sen <- rowMeans(lasso_sensitivities)
  lasso_sd_sen <- apply(lasso_sensitivities, 1, function(x) sd(x))
  lasso_u95_sen <- lasso_ave_sen + qnorm(0.975)*lasso_sd_sen
  lasso_u95_sen <- ifelse(lasso_u95_sen > 1, 1, lasso_u95_sen)
  lasso_l95_sen <- lasso_ave_sen - qnorm(0.975)*lasso_sd_sen
  lasso_l95_sen <- ifelse(lasso_l95_sen < 0, 0, lasso_l95_sen)
  lasso_sen <- cbind(lasso_ave_sen, lasso_sd_sen, lasso_u95_sen, lasso_l95_sen)
  assign(paste0("lasso_sen_",arr),lasso_sen) # Assign name
  
  lasso_specificities <- readRDS(paste0("../results/lasso_1000/specificities_",arr,".rds")) # Load roc (recalibrated)
  lasso_ave_fpr <- 1- rowMeans(lasso_specificities)
  lasso_sd_fpr <- apply(lasso_specificities, 1, function(x) sd(x))
  lasso_u95_fpr <- lasso_ave_fpr + qnorm(0.975)*lasso_sd_fpr
  lasso_u95_fpr <- ifelse(lasso_u95_fpr > 1, 1, lasso_u95_fpr)
  lasso_l95_fpr <- lasso_ave_fpr - qnorm(0.975)*lasso_sd_fpr
  lasso_l95_fpr <- ifelse(lasso_l95_fpr < 0, 0, lasso_l95_fpr)
  lasso_fpr <- cbind(lasso_ave_fpr, lasso_sd_fpr, lasso_u95_fpr, lasso_l95_fpr)
  assign(paste0("lasso_fpr_",arr),lasso_fpr) # Assign name
  
  lasso_myaucs <- readRDS(paste0("../results/lasso_1000/auc_",arr,".rds")) # Load roc (recalibrated)
  assign(paste0("lasso_auc_",arr),lasso_myaucs)
  
}

for (m in 1:10){
  arr <- paste0(rep(c("pahs_de", "pest_de"), each = 5), "_", 
                c("obesity", "hypertension", "diabetes","MS", "Lipid_disorder"))[m]
  ### sPLS
  spls_out <- readRDS(paste0("../results/spls_1000_uni/out_",arr,".rds")) # Load output
  assign(paste0("spls_out_",arr),spls_out) # Assign name
  
  spls_calib <- sum(CalibratedStableRegression(spls_out)) # Number of selected variables
  assign(paste0("spls_calib_",arr),spls_calib) # Assign name
  
  spls_sensitivities <- readRDS(paste0("../results/spls_1000_uni/sensitivities_",arr,".rds")) # Load roc (recalibrated)
  spls_ave_sen <- rowMeans(spls_sensitivities)
  spls_sd_sen <- apply(spls_sensitivities, 1, function(x) sd(x))
  spls_u95_sen <- spls_ave_sen + qnorm(0.975)*spls_sd_sen
  spls_u95_sen <- ifelse(spls_u95_sen > 1, 1, spls_u95_sen)
  spls_l95_sen <- spls_ave_sen - qnorm(0.975)*spls_sd_sen
  spls_l95_sen <- ifelse(spls_l95_sen < 0, 0, spls_l95_sen)
  spls_sen <- cbind(spls_ave_sen, spls_sd_sen, spls_u95_sen, spls_l95_sen)
  assign(paste0("spls_sen_",arr),spls_sen) # Assign name
  
  spls_specificities <- readRDS(paste0("../results/spls_1000_uni/specificities_",arr,".rds")) # Load roc (recalibrated)
  spls_ave_fpr <- 1- rowMeans(spls_specificities)
  spls_sd_fpr <- apply(spls_specificities, 1, function(x) sd(x))
  spls_u95_fpr <- spls_ave_fpr + qnorm(0.975)*spls_sd_fpr
  spls_u95_fpr <- ifelse(spls_u95_fpr > 1, 1, spls_u95_fpr)
  spls_l95_fpr <- spls_ave_fpr - qnorm(0.975)*spls_sd_fpr
  spls_l95_fpr <- ifelse(spls_l95_fpr < 0, 0, spls_l95_fpr)
  spls_fpr <- cbind(spls_ave_fpr, spls_sd_fpr, spls_u95_fpr, spls_l95_fpr)
  assign(paste0("spls_fpr_",arr),spls_fpr) # Assign name
  
  spls_myaucs <- readRDS(paste0("../results/spls_1000_uni/auc_",arr,".rds")) # Load roc (recalibrated)
  assign(paste0("spls_auc_",arr),spls_myaucs)
}


for (m in 1:10){
  arr <- paste0(rep(c("pahs1", "pest1"), each = 5), "_", 
                c("obesity", "hypertension", "diabetes","MS", "Lipid_disorder"))[m]
  ### sPLS
  forced_out <- readRDS(paste0("../results/lasso_forced/out_",arr,".rds")) # Load output
  assign(paste0("forced_out_",arr),forced_out) # Assign name
  
  forced_calib <- sum(CalibratedStableRegression(forced_out)) # Number of selected variables
  assign(paste0("forced_calib_",arr),forced_calib) # Assign name
  
  forced_sensitivities <- readRDS(paste0("../results/lasso_forced/sensitivities_",arr,".rds")) # Load roc (recalibrated)
  forced_ave_sen <- rowMeans(forced_sensitivities)
  forced_sd_sen <- apply(forced_sensitivities, 1, function(x) sd(x))
  forced_u95_sen <- forced_ave_sen + qnorm(0.975)*forced_sd_sen
  forced_u95_sen <- ifelse(forced_u95_sen > 1, 1, forced_u95_sen)
  forced_l95_sen <- forced_ave_sen - qnorm(0.975)*forced_sd_sen
  forced_l95_sen <- ifelse(forced_l95_sen < 0, 0, forced_l95_sen)
  forced_sen <- cbind(forced_ave_sen, forced_sd_sen, forced_u95_sen, forced_l95_sen)
  assign(paste0("forced_sen_",arr),forced_sen) # Assign name
  
  forced_specificities <- readRDS(paste0("../results/lasso_forced/specificities_",arr,".rds")) # Load roc (recalibrated)
  forced_ave_fpr <- 1- rowMeans(forced_specificities)
  forced_sd_fpr <- apply(forced_specificities, 1, function(x) sd(x))
  forced_u95_fpr <- forced_ave_fpr + qnorm(0.975)*forced_sd_fpr
  forced_u95_fpr <- ifelse(forced_u95_fpr > 1, 1, forced_u95_fpr)
  forced_l95_fpr <- forced_ave_fpr - qnorm(0.975)*forced_sd_fpr
  forced_l95_fpr <- ifelse(forced_l95_fpr < 0, 0, forced_l95_fpr)
  forced_fpr <- cbind(forced_ave_fpr, forced_sd_fpr, forced_u95_fpr, forced_l95_fpr)
  assign(paste0("forced_fpr_",arr),forced_fpr) # Assign name
  
  forced_myaucs <- readRDS(paste0("../results/lasso_forced/auc_",arr,".rds")) # Load roc (recalibrated)
  assign(paste0("forced_auc_",arr),forced_myaucs)
}


### function
foo = function(sen1, sen2, sen3, spe1, spe2, spe3){
  a1.coord = data.frame(fpr=spe1[,1], tpr=sen1[,1], fpr_l95=spe1[,4], tpr_l95=sen1[,4], fpr_u95=spe1[,3], tpr_u95=sen1[,3])
  a2.coord = data.frame(fpr=spe2[,1], tpr=sen2[,1], fpr_l95=spe2[,4], tpr_l95=sen2[,4], fpr_u95=spe2[,3], tpr_u95=sen2[,3])
  a3.coord = data.frame(fpr=spe3[,1], tpr=sen3[,1], fpr_l95=spe3[,4], tpr_l95=sen3[,4], fpr_u95=spe3[,3], tpr_u95=sen3[,3])
  x=list(a1.coord, a2.coord, a3.coord)
  names(x) = c("a1","a2","a3")
  return(x)
}


foo3 = function(l1, l2, l3, l4, l5){
  l1.coord = data.frame(auc=round(mean(as.numeric(l1[,2])),2), 
                        auc_l95=round(mean(as.numeric(l1[,2]))-qnorm(0.975)*sd(as.numeric(l1[,2])),2), 
                        auc_u95=round(mean(as.numeric(l1[,2]))+qnorm(0.975)*sd(as.numeric(l1[,2])),2))
  l2.coord = data.frame(auc=round(mean(as.numeric(l2[,2])),2), 
                        auc_l95=round(mean(as.numeric(l2[,2]))-qnorm(0.975)*sd(as.numeric(l2[,2])),2), 
                        auc_u95=round(mean(as.numeric(l2[,2]))+qnorm(0.975)*sd(as.numeric(l2[,2])),2))
  l3.coord = data.frame(auc=round(mean(as.numeric(l3[,2])),2), 
                        auc_l95=round(mean(as.numeric(l3[,2]))-qnorm(0.975)*sd(as.numeric(l3[,2])),2), 
                        auc_u95=round(mean(as.numeric(l3[,2]))+qnorm(0.975)*sd(as.numeric(l3[,2])),2))
  x = c(paste0(l1.coord$auc," [",l1.coord$auc_l95,"-",l1.coord$auc_u95,"]"),
        paste0(l2.coord$auc," [",l2.coord$auc_l95,"-",l2.coord$auc_u95,"]"),
        paste0(l3.coord$auc," [",l3.coord$auc_l95,"-",l3.coord$auc_u95,"]"))
  return(x)
}


### pest_obesity
roc_pest_obesity <- foo(forced_sen_pest1_obesity, lasso_sen_pest_de_obesity,
                        spls_sen_pest_de_obesity,
                        forced_fpr_pest1_obesity, lasso_fpr_pest_de_obesity,
                        spls_fpr_pest_de_obesity)

auc_pest_obesity <- foo3(forced_auc_pest1_obesity, lasso_auc_pest_de_obesity,
                         spls_auc_pest_de_obesity)

mycolours = brewer.pal(10, "Paired")

pdf("../figures/lasso_forced/roc_pest_obesity.pdf", width = 7, height = 7)
par(mar=c(5,5,2,1))
plot(roc_pest_obesity[[1]][,1],roc_pest_obesity [[1]][,2], ylim = c(0,1),
     col = mycolours[1], type="n", lwd = 2,
     xlab = "False Positive Rate", ylab = "True Positive Rate", cex.lab = 0.8, cex.axis = 0.8)
mtext("POPs and non-persistent pesticides, Obesity", side=3, adj=0, line=0.5, cex=0.8, font=2)
abline(0,1, lty = 3, col = "grey")
lines(roc_pest_obesity [[1]][,1],roc_pest_obesity [[1]][,2], col = "darkgray", type="l", lwd = 2)
#lines(roc_pest_obesity [[1]][,3],roc_pest_obesity [[1]][,4], col = "darkgray", type="l", lwd = 1, lty = 4)
#lines(roc_pest_obesity [[1]][,5],roc_pest_obesity [[1]][,6], col = "darkgray", type="l", lwd = 1, lty = 4)
lines(roc_pest_obesity [[2]][,1],roc_pest_obesity [[2]][,2], col = mycolours[2], type="l", lwd = 2)
#lines(roc_pest_obesity [[2]][,3],roc_pest_obesity [[2]][,4], col = mycolours[2], type="l", lwd = 1, lty = 4)
#lines(roc_pest_obesity [[2]][,5],roc_pest_obesity [[2]][,6], col = mycolours[2], type="l", lwd = 1, lty = 4)
lines(roc_pest_obesity [[3]][,1],roc_pest_obesity [[3]][,2], col = mycolours[1], type="l", lwd = 2)
#lines(roc_pest_obesity [[3]][,3],roc_pest_obesity [[3]][,4], col = mycolours[1], type="l", lwd = 1, lty = 4)
#lines(roc_pest_obesity [[3]][,5],roc_pest_obesity [[3]][,6], col = mycolours[1], type="l", lwd = 1, lty = 4)

legend("bottomright",
       legend = c(paste0("LASSO Model adjusted on age and sex: ",
                         auc_pest_obesity[1]),
                  paste0("LASSO Model removed effect of age and sex: ",
                         auc_pest_obesity[2]),
                  paste0("sPLS-DA Model removed effect of age and sex: ",
                         auc_pest_obesity[3])),
       lty = 1, lwd = 2, col = c("darkgray", mycolours[c(2,1)]), cex = 0.8, bty = "n")
dev.off()


### pest_LD
roc_pest_lipid_disorder <- foo(forced_sen_pest1_Lipid_disorder, lasso_sen_pest_de_Lipid_disorder,
                               spls_sen_pest_de_Lipid_disorder,
                               forced_fpr_pest1_Lipid_disorder, lasso_fpr_pest_de_Lipid_disorder,
                               spls_fpr_pest_de_Lipid_disorder)

auc_pest_lipid_disorder <- foo3(forced_auc_pest1_Lipid_disorder, lasso_auc_pest_de_Lipid_disorder,
                                spls_auc_pest_de_Lipid_disorder)

mycolours = brewer.pal(10, "Paired")

pdf("../figures/lasso_forced/roc_pest_lipid_disorder.pdf", width = 7, height = 7)
par(mar=c(5,5,2,1))
plot(roc_pest_lipid_disorder[[1]][,1],roc_pest_lipid_disorder [[1]][,2], ylim = c(0,1),
     col = mycolours[1], type="n", lwd = 2,
     xlab = "False Positive Rate", ylab = "True Positive Rate", cex.lab = 0.8, cex.axis = 0.8)
mtext("POPs and non-persistent pesticides, Lipid Disorder", side=3, adj=0, line=0.5, cex=0.8, font=2)
abline(0,1, lty = 3, col = "grey")
lines(roc_pest_lipid_disorder [[1]][,1],roc_pest_lipid_disorder [[1]][,2], col = "darkgray", type="l", lwd = 2)
#lines(roc_pest_lipid_disorder [[1]][,3],roc_pest_lipid_disorder [[1]][,4], col = "darkgray", type="l", lwd = 1, lty = 4)
#lines(roc_pest_lipid_disorder [[1]][,5],roc_pest_lipid_disorder [[1]][,6], col = "darkgray", type="l", lwd = 1, lty = 4)
lines(roc_pest_lipid_disorder [[2]][,1],roc_pest_lipid_disorder [[2]][,2], col = mycolours[4], type="l", lwd = 2)
#lines(roc_pest_lipid_disorder [[2]][,3],roc_pest_lipid_disorder [[2]][,4], col = mycolours[2], type="l", lwd = 1, lty = 4)
#lines(roc_pest_lipid_disorder [[2]][,5],roc_pest_lipid_disorder [[2]][,6], col = mycolours[2], type="l", lwd = 1, lty = 4)
lines(roc_pest_lipid_disorder [[3]][,1],roc_pest_lipid_disorder [[3]][,2], col = mycolours[3], type="l", lwd = 2)
#lines(roc_pest_lipid_disorder [[3]][,3],roc_pest_lipid_disorder [[3]][,4], col = mycolours[1], type="l", lwd = 1, lty = 4)
#lines(roc_pest_lipid_disorder [[3]][,5],roc_pest_lipid_disorder [[3]][,6], col = mycolours[1], type="l", lwd = 1, lty = 4)

legend("bottomright",
       legend = c(paste0("LASSO Model adjusted on age and sex: ",
                         auc_pest_lipid_disorder[1]),
                  paste0("LASSO Model removed effect of age and sex: ",
                         auc_pest_lipid_disorder[2]),
                  paste0("sPLS-DA Model removed effect of age and sex: ",
                         auc_pest_lipid_disorder[3])),
       lty = 1, lwd = 2, col = c("darkgray", mycolours[c(4,3)]), cex = 0.8, bty = "n")
dev.off()


### pest_hyper
roc_pest_hypertension <- foo(forced_sen_pest1_hypertension, lasso_sen_pest_de_hypertension,
                               spls_sen_pest_de_hypertension,
                               forced_fpr_pest1_hypertension, lasso_fpr_pest_de_hypertension,
                               spls_fpr_pest_de_hypertension)

auc_pest_hypertension <- foo3(forced_auc_pest1_hypertension, lasso_auc_pest_de_hypertension,
                                spls_auc_pest_de_hypertension)

pdf("../figures/lasso_forced/roc_pest_hypertension.pdf", width = 7, height = 7)
par(mar=c(5,5,2,1))
plot(roc_pest_hypertension[[1]][,1],roc_pest_hypertension [[1]][,2], ylim = c(0,1),
     col = mycolours[1], type="n", lwd = 2,
     xlab = "False Positive Rate", ylab = "True Positive Rate", cex.lab = 0.8, cex.axis = 0.8)
mtext("POPs and non-persistent pesticides, Hypertension", side=3, adj=0, line=0.5, cex=0.8, font=2)
abline(0,1, lty = 3, col = "grey")
lines(roc_pest_hypertension [[1]][,1],roc_pest_hypertension [[1]][,2], col = "darkgray", type="l", lwd = 2)
#lines(roc_pest_hypertension [[1]][,3],roc_pest_hypertension [[1]][,4], col = "darkgray", type="l", lwd = 1, lty = 4)
#lines(roc_pest_hypertension [[1]][,5],roc_pest_hypertension [[1]][,6], col = "darkgray", type="l", lwd = 1, lty = 4)
lines(roc_pest_hypertension [[2]][,1],roc_pest_hypertension [[2]][,2], col = mycolours[6], type="l", lwd = 2)
#lines(roc_pest_hypertension [[2]][,3],roc_pest_hypertension [[2]][,4], col = mycolours[2], type="l", lwd = 1, lty = 4)
#lines(roc_pest_hypertension [[2]][,5],roc_pest_hypertension [[2]][,6], col = mycolours[2], type="l", lwd = 1, lty = 4)
lines(roc_pest_hypertension [[3]][,1],roc_pest_hypertension [[3]][,2], col = mycolours[5], type="l", lwd = 2)
#lines(roc_pest_hypertension [[3]][,3],roc_pest_hypertension [[3]][,4], col = mycolours[1], type="l", lwd = 1, lty = 4)
#lines(roc_pest_hypertension [[3]][,5],roc_pest_hypertension [[3]][,6], col = mycolours[1], type="l", lwd = 1, lty = 4)

legend("bottomright",
       legend = c(paste0("LASSO Model adjusted on age and sex: ",
                         auc_pest_hypertension[1]),
                  paste0("LASSO Model removed effect of age and sex: ",
                         auc_pest_hypertension[2]),
                  paste0("sPLS-DA Model removed effect of age and sex: ",
                         auc_pest_hypertension[3])),
       lty = 1, lwd = 2, col = c("darkgray", mycolours[c(6,5)]), cex = 0.8, bty = "n")
dev.off()



### pest_DM
forced_sen_pest1_diabetes <- forced_sen_pest1_diabetes[-c(318:320), ]
forced_sen_pest1_diabetes <- rbind(forced_sen_pest1_diabetes, test)
forced_fpr_pest1_diabetes <- forced_fpr_pest1_diabetes[-c(318:320), ]
forced_fpr_pest1_diabetes <- rbind(forced_fpr_pest1_diabetes, test)

roc_pest_diabetes <- foo(forced_sen_pest1_diabetes, lasso_sen_pest_de_diabetes,
                             spls_sen_pest_de_diabetes,
                             forced_fpr_pest1_diabetes, lasso_fpr_pest_de_diabetes,
                             spls_fpr_pest_de_diabetes)

auc_pest_diabetes <- foo3(forced_auc_pest1_diabetes, lasso_auc_pest_de_diabetes,
                              spls_auc_pest_de_diabetes)

pdf("../figures/lasso_forced/roc_pest_diabetes.pdf", width = 7, height = 7)
par(mar=c(5,5,2,1))
plot(roc_pest_diabetes[[1]][,1],roc_pest_diabetes [[1]][,2], ylim = c(0,1),
     col = mycolours[1], type="n", lwd = 2,
     xlab = "False Positive Rate", ylab = "True Positive Rate", cex.lab = 0.8, cex.axis = 0.8)
mtext("POPs and non-persistent pesticides, Diabetes Mellitus", side=3, adj=0, line=0.5, cex=0.8, font=2)
abline(0,1, lty = 3, col = "grey")
lines(roc_pest_diabetes[[1]][,1],roc_pest_diabetes[[1]][,2], col = "darkgray", type="l", lwd = 2)
#lines(roc_pest_diabetes [[1]][,3],roc_pest_diabetes [[1]][,4], col = "darkgray", type="l", lwd = 1, lty = 4)
#lines(roc_pest_diabetes [[1]][,5],roc_pest_diabetes [[1]][,6], col = "darkgray", type="l", lwd = 1, lty = 4)
lines(roc_pest_diabetes[[2]][,1],roc_pest_diabetes[[2]][,2], col = mycolours[8], type="l", lwd = 2)
#lines(roc_pest_diabetes [[2]][,3],roc_pest_diabetes [[2]][,4], col = mycolours[2], type="l", lwd = 1, lty = 4)
#lines(roc_pest_diabetes [[2]][,5],roc_pest_diabetes [[2]][,6], col = mycolours[2], type="l", lwd = 1, lty = 4)
lines(roc_pest_diabetes[[3]][,1],roc_pest_diabetes[[3]][,2], col = mycolours[7], type="l", lwd = 2)
#lines(roc_pest_diabetes [[3]][,3],roc_pest_diabetes [[3]][,4], col = mycolours[1], type="l", lwd = 1, lty = 4)
#lines(roc_pest_diabetes [[3]][,5],roc_pest_diabetes [[3]][,6], col = mycolours[1], type="l", lwd = 1, lty = 4)

legend("bottomright",
       legend = c(paste0("LASSO Model adjusted on age and sex: ",
                         auc_pest_diabetes[1]),
                  paste0("LASSO Model removed effect of age and sex: ",
                         auc_pest_diabetes[2]),
                  paste0("sPLS-DA Model removed effect of age and sex: ",
                         auc_pest_diabetes[3])),
       lty = 1, lwd = 2, col = c("darkgray", mycolours[c(8,7)]), cex = 0.8, bty = "n")
dev.off()


### pest_MS
forced_sen_pest1_diabetes <- forced_sen_pest1_diabetes[-c(318:320), ]
forced_sen_pest1_diabetes <- rbind(forced_sen_pest1_diabetes, test)
forced_fpr_pest1_diabetes <- forced_fpr_pest1_diabetes[-c(318:320), ]
forced_fpr_pest1_diabetes <- rbind(forced_fpr_pest1_diabetes, test)

roc_pest_MS <- foo(forced_sen_pest1_MS, lasso_sen_pest_de_MS,
                         spls_sen_pest_de_MS,
                         forced_fpr_pest1_MS, lasso_fpr_pest_de_MS,
                         spls_fpr_pest_de_MS)

auc_pest_MS <- foo3(forced_auc_pest1_MS, lasso_auc_pest_de_MS,
                          spls_auc_pest_de_MS)

pdf("../figures/lasso_forced/roc_pest_MS.pdf", width = 7, height = 7)
par(mar=c(5,5,2,1))
plot(roc_pest_MS[[1]][,1],roc_pest_MS [[1]][,2], ylim = c(0,1),
     col = mycolours[1], type="n", lwd = 2,
     xlab = "False Positive Rate", ylab = "True Positive Rate", cex.lab = 0.8, cex.axis = 0.8)
mtext("POPs and non-persistent pesticides, Metabolic Syndrome", side=3, adj=0, line=0.5, cex=0.8, font=2)
abline(0,1, lty = 3, col = "grey")
lines(roc_pest_MS[[1]][,1],roc_pest_MS[[1]][,2], col = "darkgray", type="l", lwd = 2)
#lines(roc_pest_MS [[1]][,3],roc_pest_MS [[1]][,4], col = "darkgray", type="l", lwd = 1, lty = 4)
#lines(roc_pest_MS [[1]][,5],roc_pest_MS [[1]][,6], col = "darkgray", type="l", lwd = 1, lty = 4)
lines(roc_pest_MS[[2]][,1],roc_pest_MS[[2]][,2], col = mycolours[10], type="l", lwd = 2)
#lines(roc_pest_MS [[2]][,3],roc_pest_MS [[2]][,4], col = mycolours[2], type="l", lwd = 1, lty = 4)
#lines(roc_pest_MS [[2]][,5],roc_pest_MS [[2]][,6], col = mycolours[2], type="l", lwd = 1, lty = 4)
lines(roc_pest_MS[[3]][,1],roc_pest_MS[[3]][,2], col = mycolours[9], type="l", lwd = 2)
#lines(roc_pest_MS [[3]][,3],roc_pest_MS [[3]][,4], col = mycolours[1], type="l", lwd = 1, lty = 4)
#lines(roc_pest_MS [[3]][,5],roc_pest_MS [[3]][,6], col = mycolours[1], type="l", lwd = 1, lty = 4)

legend("bottomright",
       legend = c(paste0("LASSO Model adjusted on age and sex: ",
                         auc_pest_MS[1]),
                  paste0("LASSO Model removed effect of age and sex: ",
                         auc_pest_MS[2]),
                  paste0("sPLS-DA Model removed effect of age and sex: ",
                         auc_pest_MS[3])),
       lty = 1, lwd = 2, col = c("darkgray", mycolours[c(10,9)]), cex = 0.8, bty = "n")
dev.off()



### pahs_obesity
forced_sen_pahs1_obesity <- forced_sen_pahs1_obesity[-200,]
forced_sen_pahs1_obesity <- rbind(forced_sen_pahs1_obesity, test)
forced_fpr_pahs1_obesity <- forced_fpr_pahs1_obesity[-200,]
forced_fpr_pahs1_obesity <- rbind(forced_fpr_pahs1_obesity, test)

roc_pahs_obesity <- foo(forced_sen_pahs1_obesity, lasso_sen_pahs_de_obesity,
                        spls_sen_pahs_de_obesity,
                        forced_fpr_pahs1_obesity, lasso_fpr_pahs_de_obesity,
                        spls_fpr_pahs_de_obesity)

auc_pahs_obesity <- foo3(forced_auc_pahs1_obesity, lasso_auc_pahs_de_obesity,
                         spls_auc_pahs_de_obesity)

pdf("../figures/lasso_forced/roc_pahs_obesity.pdf", width = 7, height = 7)
par(mar=c(5,5,2,1))
plot(roc_pahs_obesity[[1]][,1],roc_pahs_obesity [[1]][,2], ylim = c(0,1),
     col = mycolours[1], type="n", lwd = 2,
     xlab = "False Positive Rate", ylab = "True Positive Rate", cex.lab = 0.8, cex.axis = 0.8)
mtext("Metabolites of PAHs and tobacco-related chemicals, Obesity", side=3, adj=0, line=0.5, cex=0.8, font=2)
abline(0,1, lty = 3, col = "grey")
lines(roc_pahs_obesity [[1]][,1],roc_pahs_obesity [[1]][,2], col = "darkgray", type="l", lwd = 2)
#lines(roc_pahs_obesity [[1]][,3],roc_pahs_obesity [[1]][,4], col = "darkgray", type="l", lwd = 1, lty = 4)
#lines(roc_pahs_obesity [[1]][,5],roc_pahs_obesity [[1]][,6], col = "darkgray", type="l", lwd = 1, lty = 4)
lines(roc_pahs_obesity [[2]][,1],roc_pahs_obesity [[2]][,2], col = mycolours[2], type="l", lwd = 2)
#lines(roc_pahs_obesity [[2]][,3],roc_pahs_obesity [[2]][,4], col = mycolours[2], type="l", lwd = 1, lty = 4)
#lines(roc_pahs_obesity [[2]][,5],roc_pahs_obesity [[2]][,6], col = mycolours[2], type="l", lwd = 1, lty = 4)
lines(roc_pahs_obesity [[3]][,1],roc_pahs_obesity [[3]][,2], col = mycolours[1], type="l", lwd = 2)
#lines(roc_pahs_obesity [[3]][,3],roc_pahs_obesity [[3]][,4], col = mycolours[1], type="l", lwd = 1, lty = 4)
#lines(roc_pahs_obesity [[3]][,5],roc_pahs_obesity [[3]][,6], col = mycolours[1], type="l", lwd = 1, lty = 4)

legend("bottomright",
       legend = c(paste0("LASSO Model adjusted on age and sex: ",
                         auc_pahs_obesity[1]),
                  paste0("LASSO Model removed effect of age and sex: ",
                         auc_pahs_obesity[2]),
                  paste0("sPLS-DA Model removed effect of age and sex: ",
                         auc_pahs_obesity[3])),
       lty = 1, lwd = 2, col = c("darkgray", mycolours[c(2,1)]), cex = 0.8, bty = "n")
dev.off()


### pahs_LD
forced_sen_pahs1_Lipid_disorder <- forced_sen_pahs1_Lipid_disorder[-c(199:201),]
forced_sen_pahs1_Lipid_disorder <- rbind(forced_sen_pahs1_Lipid_disorder, test)
forced_fpr_pahs1_Lipid_disorder <- forced_fpr_pahs1_Lipid_disorder[-c(199:201),]
forced_fpr_pahs1_Lipid_disorder <- rbind(forced_fpr_pahs1_Lipid_disorder, test)

roc_pahs_lipid_disorder <- foo(forced_sen_pahs1_Lipid_disorder, lasso_sen_pahs_de_Lipid_disorder,
                               spls_sen_pahs_de_Lipid_disorder,
                               forced_fpr_pahs1_Lipid_disorder, lasso_fpr_pahs_de_Lipid_disorder,
                               spls_fpr_pahs_de_Lipid_disorder)

auc_pahs_lipid_disorder <- foo3(forced_auc_pahs1_Lipid_disorder, lasso_auc_pahs_de_Lipid_disorder,
                                spls_auc_pahs_de_Lipid_disorder)


pdf("../figures/lasso_forced/roc_pahs_lipid_disorder.pdf", width = 7, height = 7)
par(mar=c(5,5,2,1))
plot(roc_pahs_lipid_disorder[[1]][,1],roc_pahs_lipid_disorder [[1]][,2], ylim = c(0,1),
     col = mycolours[1], type="n", lwd = 2,
     xlab = "False Positive Rate", ylab = "True Positive Rate", cex.lab = 0.8, cex.axis = 0.8)
mtext("Metabolites of PAHs and tobacco-related chemicals, Lipid Disorder", side=3, adj=0, line=0.5, cex=0.8, font=2)
abline(0,1, lty = 3, col = "grey")
lines(roc_pahs_lipid_disorder [[1]][,1],roc_pahs_lipid_disorder [[1]][,2], col = "darkgray", type="l", lwd = 2)
#lines(roc_pahs_lipid_disorder [[1]][,3],roc_pahs_lipid_disorder [[1]][,4], col = "darkgray", type="l", lwd = 1, lty = 4)
#lines(roc_pahs_lipid_disorder [[1]][,5],roc_pahs_lipid_disorder [[1]][,6], col = "darkgray", type="l", lwd = 1, lty = 4)
lines(roc_pahs_lipid_disorder [[2]][,1],roc_pahs_lipid_disorder [[2]][,2], col = mycolours[4], type="l", lwd = 2)
#lines(roc_pahs_lipid_disorder [[2]][,3],roc_pahs_lipid_disorder [[2]][,4], col = mycolours[2], type="l", lwd = 1, lty = 4)
#lines(roc_pahs_lipid_disorder [[2]][,5],roc_pahs_lipid_disorder [[2]][,6], col = mycolours[2], type="l", lwd = 1, lty = 4)
lines(roc_pahs_lipid_disorder [[3]][,1],roc_pahs_lipid_disorder [[3]][,2], col = mycolours[3], type="l", lwd = 2)
#lines(roc_pahs_lipid_disorder [[3]][,3],roc_pahs_lipid_disorder [[3]][,4], col = mycolours[1], type="l", lwd = 1, lty = 4)
#lines(roc_pahs_lipid_disorder [[3]][,5],roc_pahs_lipid_disorder [[3]][,6], col = mycolours[1], type="l", lwd = 1, lty = 4)

legend("bottomright",
       legend = c(paste0("LASSO Model adjusted on age and sex: ",
                         auc_pahs_lipid_disorder[1]),
                  paste0("LASSO Model removed effect of age and sex: ",
                         auc_pahs_lipid_disorder[2]),
                  paste0("sPLS-DA Model removed effect of age and sex: ",
                         auc_pahs_lipid_disorder[3])),
       lty = 1, lwd = 2, col = c("darkgray", mycolours[c(4,3)]), cex = 0.8, bty = "n")
dev.off()


### pahs_hyper
forced_sen_pahs1_hypertension <- forced_sen_pahs1_hypertension[-c(200),]
forced_sen_pahs1_hypertension <- rbind(forced_sen_pahs1_hypertension, test)
forced_fpr_pahs1_hypertension <- forced_fpr_pahs1_hypertension[-c(200),]
forced_fpr_pahs1_hypertension <- rbind(forced_fpr_pahs1_hypertension, test)

roc_pahs_hypertension <- foo(forced_sen_pahs1_hypertension, lasso_sen_pahs_de_hypertension,
                             spls_sen_pahs_de_hypertension,
                             forced_fpr_pahs1_hypertension, lasso_fpr_pahs_de_hypertension,
                             spls_fpr_pahs_de_hypertension)

auc_pahs_hypertension <- foo3(forced_auc_pahs1_hypertension, lasso_auc_pahs_de_hypertension,
                              spls_auc_pahs_de_hypertension)

pdf("../figures/lasso_forced/roc_pahs_hypertension.pdf", width = 7, height = 7)
par(mar=c(5,5,2,1))
plot(roc_pahs_hypertension[[1]][,1],roc_pahs_hypertension [[1]][,2], ylim = c(0,1),
     col = mycolours[1], type="n", lwd = 2,
     xlab = "False Positive Rate", ylab = "True Positive Rate", cex.lab = 0.8, cex.axis = 0.8)
mtext("Metabolites of PAHs and tobacco-related chemicals, Hypertension", side=3, adj=0, line=0.5, cex=0.8, font=2)
abline(0,1, lty = 3, col = "grey")
lines(roc_pahs_hypertension [[1]][,1],roc_pahs_hypertension [[1]][,2], col = "darkgray", type="l", lwd = 2)
#lines(roc_pahs_hypertension [[1]][,3],roc_pahs_hypertension [[1]][,4], col = "darkgray", type="l", lwd = 1, lty = 4)
#lines(roc_pahs_hypertension [[1]][,5],roc_pahs_hypertension [[1]][,6], col = "darkgray", type="l", lwd = 1, lty = 4)
lines(roc_pahs_hypertension [[2]][,1],roc_pahs_hypertension [[2]][,2], col = mycolours[6], type="l", lwd = 2)
#lines(roc_pahs_hypertension [[2]][,3],roc_pahs_hypertension [[2]][,4], col = mycolours[2], type="l", lwd = 1, lty = 4)
#lines(roc_pahs_hypertension [[2]][,5],roc_pahs_hypertension [[2]][,6], col = mycolours[2], type="l", lwd = 1, lty = 4)
lines(roc_pahs_hypertension [[3]][,1],roc_pahs_hypertension [[3]][,2], col = mycolours[5], type="l", lwd = 2)
#lines(roc_pahs_hypertension [[3]][,3],roc_pahs_hypertension [[3]][,4], col = mycolours[1], type="l", lwd = 1, lty = 4)
#lines(roc_pahs_hypertension [[3]][,5],roc_pahs_hypertension [[3]][,6], col = mycolours[1], type="l", lwd = 1, lty = 4)

legend("bottomright",
       legend = c(paste0("LASSO Model adjusted on age and sex: ",
                         auc_pahs_hypertension[1]),
                  paste0("LASSO Model removed effect of age and sex: ",
                         auc_pahs_hypertension[2]),
                  paste0("sPLS-DA Model removed effect of age and sex: ",
                         auc_pahs_hypertension[3])),
       lty = 1, lwd = 2, col = c("darkgray", mycolours[c(6,5)]), cex = 0.8, bty = "n")
dev.off()



### pahs_DM
forced_sen_pahs1_diabetes <- forced_sen_pahs1_diabetes[-c(200:201), ]
forced_sen_pahs1_diabetes <- rbind(forced_sen_pahs1_diabetes, test)
forced_fpr_pahs1_diabetes <- forced_fpr_pahs1_diabetes[-c(200:201), ]
forced_fpr_pahs1_diabetes <- rbind(forced_fpr_pahs1_diabetes, test)

roc_pahs_diabetes <- foo(forced_sen_pahs1_diabetes, lasso_sen_pahs_de_diabetes,
                         spls_sen_pahs_de_diabetes,
                         forced_fpr_pahs1_diabetes, lasso_fpr_pahs_de_diabetes,
                         spls_fpr_pahs_de_diabetes)

auc_pahs_diabetes <- foo3(forced_auc_pahs1_diabetes, lasso_auc_pahs_de_diabetes,
                          spls_auc_pahs_de_diabetes)

pdf("../figures/lasso_forced/roc_pahs_diabetes.pdf", width = 7, height = 7)
par(mar=c(5,5,2,1))
plot(roc_pahs_diabetes[[1]][,1],roc_pahs_diabetes [[1]][,2], ylim = c(0,1),
     col = mycolours[1], type="n", lwd = 2,
     xlab = "False Positive Rate", ylab = "True Positive Rate", cex.lab = 0.8, cex.axis = 0.8)
mtext("Metabolites of PAHs and tobacco-related chemicals, Diabetes Mellitus", side=3, adj=0, line=0.5, cex=0.8, font=2)
abline(0,1, lty = 3, col = "grey")
lines(roc_pahs_diabetes[[1]][,1],roc_pahs_diabetes[[1]][,2], col = "darkgray", type="l", lwd = 2)
#lines(roc_pahs_diabetes [[1]][,3],roc_pahs_diabetes [[1]][,4], col = "darkgray", type="l", lwd = 1, lty = 4)
#lines(roc_pahs_diabetes [[1]][,5],roc_pahs_diabetes [[1]][,6], col = "darkgray", type="l", lwd = 1, lty = 4)
lines(roc_pahs_diabetes[[2]][,1],roc_pahs_diabetes[[2]][,2], col = mycolours[8], type="l", lwd = 2)
#lines(roc_pahs_diabetes [[2]][,3],roc_pahs_diabetes [[2]][,4], col = mycolours[2], type="l", lwd = 1, lty = 4)
#lines(roc_pahs_diabetes [[2]][,5],roc_pahs_diabetes [[2]][,6], col = mycolours[2], type="l", lwd = 1, lty = 4)
lines(roc_pahs_diabetes[[3]][,1],roc_pahs_diabetes[[3]][,2], col = mycolours[7], type="l", lwd = 2)
#lines(roc_pahs_diabetes [[3]][,3],roc_pahs_diabetes [[3]][,4], col = mycolours[1], type="l", lwd = 1, lty = 4)
#lines(roc_pahs_diabetes [[3]][,5],roc_pahs_diabetes [[3]][,6], col = mycolours[1], type="l", lwd = 1, lty = 4)

legend("bottomright",
       legend = c(paste0("LASSO Model adjusted on age and sex: ",
                         auc_pahs_diabetes[1]),
                  paste0("LASSO Model removed effect of age and sex: ",
                         auc_pahs_diabetes[2]),
                  paste0("sPLS-DA Model removed effect of age and sex: ",
                         auc_pahs_diabetes[3])),
       lty = 1, lwd = 2, col = c("darkgray", mycolours[c(8,7)]), cex = 0.8, bty = "n")
dev.off()


### pahs_MS

roc_pahs_MS <- foo(forced_sen_pahs1_MS, lasso_sen_pahs_de_MS,
                   spls_sen_pahs_de_MS,
                   forced_fpr_pahs1_MS, lasso_fpr_pahs_de_MS,
                   spls_fpr_pahs_de_MS)

auc_pahs_MS <- foo3(forced_auc_pahs1_MS, lasso_auc_pahs_de_MS,
                    spls_auc_pahs_de_MS)

pdf("../figures/lasso_forced/roc_pahs_MS.pdf", width = 7, height = 7)
par(mar=c(5,5,2,1))
plot(roc_pahs_MS[[1]][,1],roc_pahs_MS [[1]][,2], ylim = c(0,1),
     col = mycolours[1], type="n", lwd = 2,
     xlab = "False Positive Rate", ylab = "True Positive Rate", cex.lab = 0.8, cex.axis = 0.8)
mtext("Metabolites of PAHs and tobacco-related chemicals, Metabolic Syndrome", side=3, adj=0, line=0.5, cex=0.8, font=2)
abline(0,1, lty = 3, col = "grey")
lines(roc_pahs_MS[[1]][,1],roc_pahs_MS[[1]][,2], col = "darkgray", type="l", lwd = 2)
#lines(roc_pahs_MS [[1]][,3],roc_pahs_MS [[1]][,4], col = "darkgray", type="l", lwd = 1, lty = 4)
#lines(roc_pahs_MS [[1]][,5],roc_pahs_MS [[1]][,6], col = "darkgray", type="l", lwd = 1, lty = 4)
lines(roc_pahs_MS[[2]][,1],roc_pahs_MS[[2]][,2], col = mycolours[10], type="l", lwd = 2)
#lines(roc_pahs_MS [[2]][,3],roc_pahs_MS [[2]][,4], col = mycolours[2], type="l", lwd = 1, lty = 4)
#lines(roc_pahs_MS [[2]][,5],roc_pahs_MS [[2]][,6], col = mycolours[2], type="l", lwd = 1, lty = 4)
lines(roc_pahs_MS[[3]][,1],roc_pahs_MS[[3]][,2], col = mycolours[9], type="l", lwd = 2)
#lines(roc_pahs_MS [[3]][,3],roc_pahs_MS [[3]][,4], col = mycolours[1], type="l", lwd = 1, lty = 4)
#lines(roc_pahs_MS [[3]][,5],roc_pahs_MS [[3]][,6], col = mycolours[1], type="l", lwd = 1, lty = 4)

legend("bottomright",
       legend = c(paste0("LASSO Model adjusted on age and sex: ",
                         auc_pahs_MS[1]),
                  paste0("LASSO Model removed effect of age and sex: ",
                         auc_pahs_MS[2]),
                  paste0("sPLS-DA Model removed effect of age and sex: ",
                         auc_pahs_MS[3])),
       lty = 1, lwd = 2, col = c("darkgray", mycolours[c(10,9)]), cex = 0.8, bty = "n")
dev.off()

