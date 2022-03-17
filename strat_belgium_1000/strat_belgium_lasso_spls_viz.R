
### Sumemr Project --  Stratified analysis by Belgium: spls lasso
## Created by Vivian on 12 August

rm(list=ls())
project_path="~/hair/scripts"
setwd(project_path)

# Loading packages
library(RColorBrewer)
library(tidyverse)
library(plotrix)
library(colorspace)

source("penalisation_functions.R")

pahs <- readRDS("../processed/imputation/imp_pahs.rds")
pest <- readRDS("../processed/imputation/imp_pest.rds")
pest_imp <- readRDS("../processed/imputation/imp_pest.rds")


##### Load data --
for (m in 1:9){
  arr <- c(paste0(rep(c("pest_de"), times = 9), "_", 
                  c("obesity", "hypertension",  "Lipid_disorder","MS", "diabetes"), 
                  rep(c(".up", ".low"), times=c(5,4))))[m]
  
  ### Lasso
  lasso_out=readRDS(paste0("../results/strat_belgium_lasso_1000/out_",arr,".rds")) # Load output
  assign(paste0("lasso_out_",arr),lasso_out) # Assign name
  
  lasso_hat_params=GetArgmax(lasso_out) # Extract calibrated pi
  assign(paste0("lasso_hat_params_",arr),lasso_hat_params) # Assign name
  
  lasso_selprop=readRDS(paste0("../results/strat_belgium_lasso_1000/selprop_",arr,".rds")) # Load selection proportion
  assign(paste0("lasso_selprop_",arr),lasso_selprop) # Assign name
  
  lasso_beta=readRDS(paste0("../results/strat_belgium_lasso_1000/average_beta_",arr,".rds")) # Load beta
  lasso_beta=ifelse(CalibratedStableRegression(lasso_out) == 1, lasso_beta, 0) # Shrink non-selected beta to zero
  lasso_beta=exp(lasso_beta) # Exponentiate to OR
  assign(paste0("lasso_beta_",arr),lasso_beta) # Assign name
}


for (m in 1:9){
  arr <- c(paste0(rep(c("pest_de"), times = 9), "_", 
                  c("obesity", "hypertension",  "Lipid_disorder","MS", "diabetes"), 
                  rep(c(".up", ".low"), times=c(5,4))))[m]
  
  spls_out=readRDS(paste0("../results/strat_belgium_spls_1000/out_",arr,".rds")) # Load output
  assign(paste0("spls_out_",arr),spls_out) # Assign name
  
  hat_params=GetArgmax(spls_out) # Extract calibrated pi
  assign(paste0("hat_params_",arr), hat_params) # Assign name
  
  spls_selprop=readRDS(paste0("../results/strat_belgium_spls_1000/selprop_",arr,".rds")) # Load selection proportion
  assign(paste0("spls_selprop_",arr),spls_selprop) # Assign name
  
  spls_beta=readRDS(paste0("../results/strat_belgium_spls_1000/beta_",arr,".rds")) # Load beta
  spls_beta=ifelse(CalibratedStableRegression(spls_out) == 1, spls_beta, 0) # Shrink non-selected beta to zero
  assign(paste0("spls_beta_",arr),spls_beta) # Assign name
  
}


##### functions --
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
  df=data.frame(or.f=unlist(x[,6:10]),or.m=unlist(x[,1:5]),
                var=rep(rownames(x),5),
                outcomes = rep(outcomes, each=33)) # change accordingly
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
  df=data.frame(sel.f=unlist(x[,6:10]),sel.m=unlist(x[,1:5]),
                var=rep(rownames(x),5),
                outcomes = rep(outcomes, each=33)) # change accordingly
}


lasso_beta_pest_de_diabetes.low <- lasso_beta_pest_de_diabetes.up
lasso_beta_pest_de_diabetes.low <- ifelse(lasso_beta_pest_de_diabetes.low !=1, 1, lasso_beta_pest_de_diabetes.low)
myorder <- names(lasso_beta_pest_de_obesity.low)
coef_pest_b <- foo3(lasso_beta_pest_de_obesity.low, lasso_beta_pest_de_Lipid_disorder.low,
                   lasso_beta_pest_de_hypertension.low, lasso_beta_pest_de_diabetes.low,
                   lasso_beta_pest_de_MS.low, 
                   lasso_beta_pest_de_obesity.up, lasso_beta_pest_de_Lipid_disorder.up,
                   lasso_beta_pest_de_hypertension.up, lasso_beta_pest_de_diabetes.up,
                   lasso_beta_pest_de_MS.up, 
                   myorder)

myorder <- names(lasso_selprop_pest_de_obesity.low)
lasso_selprop_pest_de_diabetes.low <- lasso_selprop_pest_de_diabetes.up
lasso_selprop_pest_de_diabetes.low <- ifelse(lasso_selprop_pest_de_diabetes.low != 0, 0, lasso_selprop_pest_de_diabetes.low)
selprop_pest_b <- foo2(lasso_selprop_pest_de_obesity.low, lasso_selprop_pest_de_Lipid_disorder.low,
                       lasso_selprop_pest_de_hypertension.low, lasso_selprop_pest_de_diabetes.low,
                       lasso_selprop_pest_de_MS.low, 
                       lasso_selprop_pest_de_obesity.up, lasso_selprop_pest_de_Lipid_disorder.up,
                       lasso_selprop_pest_de_hypertension.up, lasso_selprop_pest_de_diabetes.up,
                       lasso_selprop_pest_de_MS.up, 
                       myorder)


##### Beta plot --
# LASSO 
xlim=c(0.5, 1.6)
ylim=c(0.5,1.6)
#coef_lung$var_cat=rep(variable_cat)
coef_pest_b$label=rep(1:(nrow(coef_pest_b)/5), 5)
coef_pest_b$mycolour_point=rep(mycolours[c(2,4,6,8,10)],each = 33)

coef_pest_b$mycolour_lab=darken(coef_pest_b$mycolour_point, amount=0.2)
coef_pest_b$myshape <- rep(c(15, 16, 17, 18, 25),  each = 33)

p1=ggplot(coef_pest_b, aes(or.f, or.m,
                           label=ifelse(((abs(or.f-1)<0.05&or.m==1)|(or.f==1&abs(or.m-1)<0.05)),
                                        "",label))) +
  geom_vline(aes(xintercept = 1),linetype = "dashed",colour = "black") +
  geom_hline(aes(yintercept = 1),linetype = "dashed",colour = "black") +
  geom_point(shape=coef_pest_b$myshape, colour=coef_pest_b$mycolour_point, size = 2.5) +
  geom_text_repel(color=coef_pest_b$mycolour_lab,
                  nudge_y = 0.03,
                  nudge_x = 0.04,
                  size=4, segment.color="grey",
                  segment.size=0.2) + 
  xlab("Mean Odds Ratio (Belgium Group1)")+
  ylab("Mean Odds Ratio (Belgium Group2)") +
  ggtitle("POPs and non-persistent pesticides, LASSO Models") +
  xlim(xlim) +
  ylim(ylim) +
  theme_bw() +
  theme(legend.position = "none")

pdf("../figures/strat_belgium_lasso_1000/lasso_belgium_or_pest_b.pdf", width=7, height=7)
p1
dev.off()


mylabel_pest <- c(colnames(pest_imp)[1:4],
                  "p,p-DDT", "p,p-DDE",
                  colnames(pest_imp)[7:33])
p1_1=ggplot(coef_pest_b, aes(or.f, or.m,
                           label=ifelse(((abs(or.f-1)<0.05&or.m==1)|(or.f==1&abs(or.m-1)<0.05)),
                                        "", mylabel_pest))) +
  geom_vline(aes(xintercept = 1),linetype = "dashed",colour = "black") +
  geom_hline(aes(yintercept = 1),linetype = "dashed",colour = "black") +
  geom_point(shape=coef_pest_b$myshape, colour=coef_pest_b$mycolour_point, size = 2) +
  geom_text_repel(color=coef_pest_b$mycolour_lab,
                  nudge_y = 0.055,
                  nudge_x = 0.06,
                  size=2.6, segment.color="grey",
                  segment.size=0.2) + 
  xlab("Mean Odds Ratio (Belgium Group1)")+
  ylab("Mean Odds Ratio (Belgium Group2)") +
  ggtitle("POPs and non-persistent pesticides, LASSO Models") +
  xlim(xlim) +
  ylim(ylim) +
  theme_bw() +
  theme(legend.position = "none")

cairo_pdf("../figures/strat_belgium_lasso_1000/ppt_lasso_belgium_or_pest_b.pdf", width=7, height=7)
p1_1
dev.off()



##### Selection proportion --
# LASSO 
xlim=c(0, 1)
ylim=c(0, 1)
#coef_lung$var_cat=rep(variable_cat)
selprop_pest_b$label=rep(1:(nrow(selprop_pest_b)/5), 5)
selprop_pest_b$mycolour_point=rep(mycolours[c(2,4,6,8,10)],each = 33)

selprop_pest_b$mycolour_lab=darken(selprop_pest_b$mycolour_point, amount=0.2)
selprop_pest_b$myshape <- rep(c(15, 16, 17, 18, 25),  each = 33)

id1 <- ifelse(selprop_pest_b$sel.f[1:33]<0.84 & selprop_pest_b$sel.m[1:33]<0.88, "N", "Y") 
id2 <- ifelse(selprop_pest_b$sel.f[34:66]<0.89 & selprop_pest_b$sel.m[34:66]<0.87, "N", "Y") 
id3 <- ifelse(selprop_pest_b$sel.f[67:99]<0.9 & selprop_pest_b$sel.m[67:99]<0.75, "N", "Y") 
id4 <- ifelse(selprop_pest_b$sel.f[100:132]<0.5 & selprop_pest_b$sel.m[100:132]<0.9, "N", "Y") 
id5 <- ifelse(selprop_pest_b$sel.f[133:165]<0.83 & selprop_pest_b$sel.m[133:165]<0.83, "N", "Y") 

selprop_pest_b$label_ny <- c(id1, id2, id3, id4, id5)

p4=ggplot(selprop_pest_b, aes(sel.f, sel.m,
                              label=ifelse(label_ny == "N","",label))) +
  geom_abline(slope = 1,linetype = "dotted",colour = "grey") +
  geom_vline(aes(xintercept = 0.84),linetype = "dashed",colour = lighten(mycolours[2], amount = 0.2)) +
  geom_vline(aes(xintercept = 0.89),linetype = "dashed",colour = lighten(mycolours[4], amount = 0.2)) +
  geom_vline(aes(xintercept = 0.9),linetype = "dashed",colour = lighten(mycolours[6], amount = 0.2)) +
  #geom_vline(aes(xintercept = 0.5),linetype = "dashed",colour = lighten(mycolours[8], amount = 0.2)) +
  geom_vline(aes(xintercept = 0.83),linetype = "dashed",colour = lighten(mycolours[10], amount = 0.2)) +
  geom_hline(aes(yintercept = 0.88),linetype = "dashed",colour = lighten(mycolours[2], amount = 0.2)) +
  geom_hline(aes(yintercept = 0.87),linetype = "dashed",colour = lighten(mycolours[4], amount = 0.2)) +
  geom_hline(aes(yintercept = 0.75),linetype = "dashed",colour = lighten(mycolours[6], amount = 0.2)) +
  geom_hline(aes(yintercept = 0.9),linetype = "dashed",colour = lighten(mycolours[8], amount = 0.2)) +
  geom_hline(aes(yintercept = 0.83),linetype = "dashed",colour = lighten(mycolours[10], amount = 0.2)) +
  geom_point(shape=selprop_pest_b$myshape, colour=selprop_pest_b$mycolour_point, size = 2.5) +
  geom_text_repel(color=selprop_pest_b$mycolour_lab,
                  nudge_y = 0.02,
                  nudge_x = 0.02,
                  size=3, segment.color="grey",
                  segment.size=0.2) + 
  xlab("Selection Proportion (Belgium Group1)")+
  ylab("Selection Proportion (Belgium Group2)") +
  labs(title = "POPs and non-persistent pesticides, LASSO Models") +
  xlim(xlim) +
  ylim(ylim) +
  theme_bw() +
  theme(legend.position = "none")

pdf("../figures/strat_belgium_lasso_1000/lasso_belgium_selprop_pest_b.pdf", width=7, height=7)
p4
dev.off()



##### functions --
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
  df=data.frame(or.f=unlist(x[,6:10]),or.m=unlist(x[,1:5]),
                var=rep(rownames(x),5),
                outcomes = rep(outcomes, each=33)) # change accordingly
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
  df=data.frame(sel.f=unlist(x[,6:10]),sel.m=unlist(x[,1:5]),
                var=rep(rownames(x),5),
                outcomes = rep(outcomes, each=33)) # change accordingly
}


##### Inverse --
spls_beta_pest_de_obesity.low <- spls_beta_pest_de_obesity.low*(-1)
spls_beta_pest_de_hypertension.low <- spls_beta_pest_de_hypertension.low*(-1)
spls_beta_pest_de_diabetes.up <- spls_beta_pest_de_diabetes.up*(-1)
spls_beta_pest_de_MS.low <- spls_beta_pest_de_MS.low*(-1)


myorder <- names(spls_beta_pest_de_obesity.low)
spls_beta_pest_de_diabetes.low <- spls_beta_pest_de_diabetes.up
spls_beta_pest_de_diabetes.low <- ifelse(spls_beta_pest_de_diabetes.low != 0, 0, spls_beta_pest_de_diabetes.low)
coef_pest_b<- foo3(spls_beta_pest_de_obesity.low, spls_beta_pest_de_Lipid_disorder.low,
                   spls_beta_pest_de_hypertension.low, spls_beta_pest_de_diabetes.low,
                   spls_beta_pest_de_MS.low, 
                   spls_beta_pest_de_obesity.up, spls_beta_pest_de_Lipid_disorder.up,
                   spls_beta_pest_de_hypertension.up, spls_beta_pest_de_diabetes.up,
                   spls_beta_pest_de_MS.up, 
                   myorder)

myorder <- names(spls_selprop_pest_de_obesity.low)
spls_selprop_pest_de_diabetes.low <- spls_selprop_pest_de_diabetes.up
spls_selprop_pest_de_diabetes.low <- ifelse(spls_selprop_pest_de_diabetes.low !=0, 0, spls_selprop_pest_de_diabetes.low)
selprop_pest_b <- foo2(spls_selprop_pest_de_obesity.low, spls_selprop_pest_de_Lipid_disorder.low,
                       spls_selprop_pest_de_hypertension.low, spls_selprop_pest_de_diabetes.low,
                       spls_selprop_pest_de_MS.low, 
                       spls_selprop_pest_de_obesity.up, spls_selprop_pest_de_Lipid_disorder.up,
                       spls_selprop_pest_de_hypertension.up, spls_selprop_pest_de_diabetes.up,
                       spls_selprop_pest_de_MS.up, 
                       myorder)

##### Beta plot --
# spls Pesticides L+B
xlim=c(-0.3, 0.4)
ylim=c(-0.3,0.75)
#coef_lung$var_cat=rep(variable_cat)
coef_pest_b$label=rep(1:(nrow(coef_pest_b)/5), 5)
coef_pest_b$mycolour_point=rep(mycolours[c(2,4,6,8,10)],each = 33)

coef_pest_b$mycolour_lab=darken(coef_pest_b$mycolour_point, amount=0.2)
coef_pest_b$myshape <- rep(c(15, 16, 17, 18, 25),  each = 33)

p7=ggplot(coef_pest_b, aes(or.f, or.m,
                           label=ifelse(((abs(or.f)<0.05&or.m==0)|(or.f==0&abs(or.m)<0.05)),
                                        "",label))) +
  geom_vline(aes(xintercept = 0),linetype = "dashed",colour = "black") +
  geom_hline(aes(yintercept = 0),linetype = "dashed",colour = "black") +
  geom_point(shape=coef_pest_b$myshape, colour=coef_pest_b$mycolour_point, size = 2.5) +
  geom_text_repel(color=coef_pest_b$mycolour_lab,
                  nudge_y = 0.03,
                  nudge_x = 0.04,
                  size=3.5, segment.color="grey",
                  segment.size=0.2) + 
  xlab("Mean Loading Coefficient (Belgium Group1)")+
  ylab("Mean Loading Coefficient (Belgium Group2)") +
  ggtitle("POPs and non-persistent pesticides, sPLS-DA Models") +
  xlim(xlim) +
  ylim(ylim) +
  theme_bw() +
  theme(legend.position = "none")

pdf("../figures/strat_belgium_spls_1000/spls_belgium_or_pest_b.pdf", width=7, height=7)
p7
dev.off()



##### Selection proportion --
# spls Pesticides L+B
xlim=c(0, 1)
ylim=c(0, 1)
#coef_lung$var_cat=rep(variable_cat)
selprop_pest_b$label=rep(1:(nrow(selprop_pest_b)/5), 5)
selprop_pest_b$mycolour_point=rep(mycolours[c(2,4,6,8,10)],each = 33)

selprop_pest_b$mycolour_lab=darken(selprop_pest_b$mycolour_point, amount=0.2)
selprop_pest_b$myshape <- rep(c(15, 16, 17, 18, 25),  each = 33)

id1 <- ifelse(selprop_pest_b$sel.f[1:33]<0.9 & selprop_pest_b$sel.m[1:33]<0.86, "N", "Y") 
id2 <- ifelse(selprop_pest_b$sel.f[34:66]<0.7 & selprop_pest_b$sel.m[34:66]<0.67, "N", "Y") 
id3 <- ifelse(selprop_pest_b$sel.f[67:99]<0.81 & selprop_pest_b$sel.m[67:99]<0.68, "N", "Y") 
id4 <- ifelse(selprop_pest_b$sel.f[100:132]<0.5 & selprop_pest_b$sel.m[100:132]<0.82, "N", "Y") 
id5 <- ifelse(selprop_pest_b$sel.f[133:165]<0.74 & selprop_pest_b$sel.m[133:165]<0.71, "N", "Y") 

selprop_pest_b$label_ny <- c(id1, id2, id3, id4, id5)

p10=ggplot(selprop_pest_b, aes(sel.f, sel.m,
                               label=ifelse(label_ny == "N","",label))) +
  geom_abline(slope = 1,linetype = "dotted",colour = "grey") +
  geom_vline(aes(xintercept = 0.9),linetype = "dashed",colour = lighten(mycolours[2], amount = 0.2)) +
  geom_vline(aes(xintercept = 0.7),linetype = "dashed",colour = lighten(mycolours[4], amount = 0.2)) +
  geom_vline(aes(xintercept = 0.81),linetype = "dashed",colour = lighten(mycolours[6], amount = 0.2)) +
  #geom_vline(aes(xintercept = 0.87),linetype = "dashed",colour = lighten(mycolours[8], amount = 0.2)) +
  geom_vline(aes(xintercept = 0.74),linetype = "dashed",colour = lighten(mycolours[10], amount = 0.2)) +
  geom_hline(aes(yintercept = 0.86),linetype = "dashed",colour = lighten(mycolours[2], amount = 0.2)) +
  geom_hline(aes(yintercept = 0.67),linetype = "dashed",colour = lighten(mycolours[4], amount = 0.2)) +
  geom_hline(aes(yintercept = 0.68),linetype = "dashed",colour = lighten(mycolours[6], amount = 0.2)) +
  geom_hline(aes(yintercept = 0.82),linetype = "dashed",colour = lighten(mycolours[8], amount = 0.2)) +
  geom_hline(aes(yintercept = 0.71),linetype = "dashed",colour = lighten(mycolours[10], amount = 0.2)) +
  geom_point(shape=selprop_pest_b$myshape, colour=selprop_pest_b$mycolour_point, size = 2.5) +
  geom_text_repel(color=selprop_pest_b$mycolour_lab,
                  nudge_y = 0.02,
                  nudge_x = 0.02,
                  size=3, segment.color="grey",
                  segment.size=0.2) + 
  xlab("Selection Proportion (Belgium Group1)")+
  ylab("Selection Proportion (Belgium Group2)") +
  labs(title = "POPs and non-persistent pesticides, sPLS-DA Models") +
  xlim(xlim) +
  ylim(ylim) +
  theme_bw() +
  theme(legend.position = "none")

pdf("../figures/strat_belgium_spls_1000/spls_belgium_selprop_pest_b.pdf", width=7, height=7)
p10
dev.off()





