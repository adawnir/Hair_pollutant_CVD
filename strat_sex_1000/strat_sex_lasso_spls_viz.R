

### Sumemr Project --  Stratified analysis by sex: spls lasso
## Created by Vivian on 11 August

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
for (m in 1:20){
  arr <- paste0(rep(c("pest_de", "pahs_de"), each = 5), "_", 
                c("obesity", "hypertension", "diabetes", "Lipid_disorder", "MS"), 
                rep(c(".f",".m"),each=10))[m]
  
  ### Lasso
  lasso_out=readRDS(paste0("../results/strat_sex_lasso_1000/out_",arr,".rds")) # Load output
  assign(paste0("lasso_out_",arr),lasso_out) # Assign name
  
  lasso_hat_params=GetArgmax(lasso_out) # Extract calibrated pi
  assign(paste0("lasso_hat_params_",arr),lasso_hat_params) # Assign name
  
  lasso_selprop=readRDS(paste0("../results/strat_sex_lasso_1000/selprop_",arr,".rds")) # Load selection proportion
  assign(paste0("lasso_selprop_",arr),lasso_selprop) # Assign name
  
  lasso_beta=readRDS(paste0("../results/strat_sex_lasso_1000/average_beta_",arr,".rds")) # Load beta
  lasso_beta=ifelse(CalibratedStableRegression(lasso_out) == 1, lasso_beta, 0) # Shrink non-selected beta to zero
  lasso_beta=exp(lasso_beta) # Exponentiate to OR
  assign(paste0("lasso_beta_",arr),lasso_beta) # Assign name
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
  df=data.frame(sel.f=unlist(x[,6:10]),sel.m=unlist(x[,1:5]),
                var=rep(rownames(x),5),
                outcomes = rep(outcomes, each=14)) # change accordingly
}


myorder <- names(lasso_beta_pest_de_obesity.f)
coef_pest<- foo3(lasso_beta_pest_de_obesity.f, lasso_beta_pest_de_Lipid_disorder.f,
                 lasso_beta_pest_de_hypertension.f, lasso_beta_pest_de_diabetes.f,
                 lasso_beta_pest_de_MS.f, 
                 lasso_beta_pest_de_obesity.m, lasso_beta_pest_de_Lipid_disorder.m,
                 lasso_beta_pest_de_hypertension.m, lasso_beta_pest_de_diabetes.m,
                 lasso_beta_pest_de_MS.m, 
                 myorder)

myorder <- names(lasso_selprop_pest_de_obesity.f)
selprop_pest<- foo2(lasso_selprop_pest_de_obesity.f, lasso_selprop_pest_de_Lipid_disorder.f,
                    lasso_selprop_pest_de_hypertension.f, lasso_selprop_pest_de_diabetes.f,
                    lasso_selprop_pest_de_MS.f, 
                    lasso_selprop_pest_de_obesity.m, lasso_selprop_pest_de_Lipid_disorder.m,
                    lasso_selprop_pest_de_hypertension.m, lasso_selprop_pest_de_diabetes.m,
                    lasso_selprop_pest_de_MS.m, 
                    myorder)


myorder <- names(lasso_beta_pahs_de_obesity.f)
coef_pahs<- foo3(lasso_beta_pahs_de_obesity.f, lasso_beta_pahs_de_Lipid_disorder.f,
                 lasso_beta_pahs_de_hypertension.f, lasso_beta_pahs_de_diabetes.f,
                 lasso_beta_pahs_de_MS.f, 
                 lasso_beta_pahs_de_obesity.m, lasso_beta_pahs_de_Lipid_disorder.m,
                 lasso_beta_pahs_de_hypertension.m, lasso_beta_pahs_de_diabetes.m,
                 lasso_beta_pahs_de_MS.m, 
                 myorder)

myorder <- names(lasso_selprop_pahs_de_obesity.f)
selprop_pahs<- foo2(lasso_selprop_pahs_de_obesity.f, lasso_selprop_pahs_de_Lipid_disorder.f,
                    lasso_selprop_pahs_de_hypertension.f, lasso_selprop_pahs_de_diabetes.f,
                    lasso_selprop_pahs_de_MS.f, 
                    lasso_selprop_pahs_de_obesity.m, lasso_selprop_pahs_de_Lipid_disorder.m,
                    lasso_selprop_pahs_de_hypertension.m, lasso_selprop_pahs_de_diabetes.m,
                    lasso_selprop_pahs_de_MS.m, 
                    myorder)


##### Beta plot --
# LASSO Pesticides
xlim=c(0.6, 1.5)
ylim=c(0.8,1.5)
#coef_lung$var_cat=rep(variable_cat)
coef_pest$label=rep(1:(nrow(coef_pest)/5), 5)
coef_pest$mycolour_point=rep(mycolours[c(2,4,6,8,10)],each = 35)

coef_pest$mycolour_lab=darken(coef_pest$mycolour_point, amount=0.2)
coef_pest$myshape <- rep(c(15, 16, 17, 18, 25),  each = 35)

p1=ggplot(coef_pest, aes(or.f, or.m,
                         label=ifelse(((abs(or.f-1)<0.05&or.m==1)|(or.f==1&abs(or.m-1)<0.05)),
                                      "",label))) +
  geom_vline(aes(xintercept = 1),linetype = "dashed",colour = "black") +
  geom_hline(aes(yintercept = 1),linetype = "dashed",colour = "black") +
  geom_point(shape=coef_pest$myshape, colour=coef_pest$mycolour_point, size = 3) +
  geom_text_repel(color=coef_pest$mycolour_lab,
                  nudge_y = 0.02,
                  nudge_x = 0.02,
                  size=5, segment.color="grey",
                  segment.size=0.2) + 
  xlab("Mean Odds Ratio (Females)")+
  ylab("Mean Odds Ratio (Males)") +
  ggtitle("POPs and non-persistent pesticides, LASSO Models") +
  xlim(xlim) +
  ylim(ylim) +
  theme_bw() +
  theme(legend.position = "none")

pdf("../figures/strat_sex_lasso_1000/lasso_sex_or_pest.pdf", width=7, height=7)
p1
dev.off()


mylabel_pest <- c("CountryBelgium","CountryFrance", colnames(pest_imp)[1:4],
                  "p,p-DDT", "p,p-DDE",
                  colnames(pest_imp)[7:33])
p1_1=ggplot(coef_pest, aes(or.f, or.m,
                         label=ifelse(((abs(or.f-1)<0.05&or.m==1)|(or.f==1&abs(or.m-1)<0.05)),
                                      "", mylabel_pest))) +
  geom_vline(aes(xintercept = 1),linetype = "dashed",colour = "black") +
  geom_hline(aes(yintercept = 1),linetype = "dashed",colour = "black") +
  geom_point(shape=coef_pest$myshape, colour=coef_pest$mycolour_point, size = 2) +
  geom_text_repel(color=coef_pest$mycolour_lab,
                  nudge_y = 0.02,
                  nudge_x = 0.02,
                  size=3, segment.color="grey",
                  segment.size=0.2) + 
  xlab("Mean Odds Ratio (Females)")+
  ylab("Mean Odds Ratio (Males)") +
  ggtitle("POPs and non-persistent pesticides, LASSO Models") +
  xlim(xlim) +
  ylim(ylim) +
  theme_bw() +
  theme(legend.position = "none")

cairo_pdf("../figures/strat_sex_lasso_1000/ppt_lasso_sex_or_pest.pdf", width=7, height=7)
p1_1
dev.off()


# LASSO PAHs
xlim=c(0.6, 1.5)
ylim=c(0.8,1.7)
#coef_lung$var_cat=rep(variable_cat)
coef_pahs$label=rep(1:(nrow(coef_pahs)/5), 5)
coef_pahs$mycolour_point=rep(mycolours[c(2,4,6,8,10)],each = 14)

coef_pahs$mycolour_lab=darken(coef_pahs$mycolour_point, amount=0.2)
coef_pahs$myshape <- rep(c(15, 16, 17, 18, 25),  each = 14)

p2=ggplot(coef_pahs, aes(or.f, or.m,
                         label=ifelse(((abs(or.f-1)<0.05&or.m==1)|(or.f==1&abs(or.m-1)<0.05)),
                                      "",label))) +
  geom_vline(aes(xintercept = 1),linetype = "dashed",colour = "black") +
  geom_hline(aes(yintercept = 1),linetype = "dashed",colour = "black") +
  geom_point(shape=coef_pahs$myshape, colour=coef_pahs$mycolour_point, size = 3) +
  geom_text_repel(color=coef_pahs$mycolour_lab,
                  nudge_y = 0.02,
                  nudge_x = 0.02,
                  size=5, segment.color="grey",
                  segment.size=0.2) + 
  xlab("Mean Odds Ratio (Females)")+
  ylab("Mean Odds Ratio (Males)") +
  ggtitle("Metabolites of PAHs and tobacco-related chemicals, LASSO Models") +
  xlim(xlim) +
  ylim(ylim) +
  theme_bw() +
  theme(legend.position = "none")

pdf("../figures/strat_sex_lasso_1000/lasso_sex_or_pahs.pdf", width=7, height=7)
p2
dev.off()


mylabel_pahs <- c("CountryBelgium","Cotinine", "Nicotine", colnames(pahs)[3:13])
p2_1=ggplot(coef_pahs, aes(or.f, or.m,
                         label=ifelse(((abs(or.f-1)<0.05&or.m==1)|(or.f==1&abs(or.m-1)<0.05)),
                                      "", mylabel_pahs))) +
  geom_vline(aes(xintercept = 1),linetype = "dashed",colour = "black") +
  geom_hline(aes(yintercept = 1),linetype = "dashed",colour = "black") +
  geom_point(shape=coef_pahs$myshape, colour=coef_pahs$mycolour_point, size = 2) +
  geom_text_repel(color=coef_pahs$mycolour_lab,
                  nudge_y = 0.04,
                  nudge_x = 0.04,
                  size=3, segment.color="grey",
                  segment.size=0.2) + 
  xlab("Mean Odds Ratio (Females)")+
  ylab("Mean Odds Ratio (Males)") +
  ggtitle("Metabolites of PAHs and tobacco-related chemicals, LASSO Models") +
  xlim(xlim) +
  ylim(ylim) +
  theme_bw() +
  theme(legend.position = "none")

cairo_pdf("../figures/strat_sex_lasso_1000/ppt_lasso_sex_or_pahs.pdf", width=7, height=7)
p2_1
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

id1 <- ifelse(selprop_pest$sel.f[1:35]<0.88 & selprop_pest$sel.m[1:35]<0.89, "N", "Y") 
id2 <- ifelse(selprop_pest$sel.f[36:70]<0.75 & selprop_pest$sel.m[36:70]<0.87, "N", "Y") 
id3 <- ifelse(selprop_pest$sel.f[71:105]<0.86 & selprop_pest$sel.m[71:105]<0.9, "N", "Y") 
id4 <- ifelse(selprop_pest$sel.f[106:140]<0.86 & selprop_pest$sel.m[106:140]<0.9, "N", "Y") 
id5 <- ifelse(selprop_pest$sel.f[141:175]<0.85 & selprop_pest$sel.m[141:175]<0.77, "N", "Y") 

selprop_pest$label_ny <- c(id1, id2, id3, id4, id5)

p3=ggplot(selprop_pest, aes(sel.f, sel.m,
                            label=ifelse(label_ny == "N","",label))) +
  geom_abline(slope = 1,linetype = "dotted",colour = "grey") +
  geom_vline(aes(xintercept = 0.88),linetype = "dashed",colour = lighten(mycolours[2], amount = 0.2)) +
  geom_vline(aes(xintercept = 0.75),linetype = "dashed",colour = lighten(mycolours[4], amount = 0.2)) +
  geom_vline(aes(xintercept = 0.86),linetype = "dashed",colour = lighten(mycolours[6], amount = 0.2)) +
  geom_vline(aes(xintercept = 0.86),linetype = "dashed",colour = lighten(mycolours[8], amount = 0.2)) +
  geom_vline(aes(xintercept = 0.85),linetype = "dashed",colour = lighten(mycolours[10], amount = 0.2)) +
  geom_hline(aes(yintercept = 0.89),linetype = "dashed",colour = lighten(mycolours[2], amount = 0.2)) +
  geom_hline(aes(yintercept = 0.87),linetype = "dashed",colour = lighten(mycolours[4], amount = 0.2)) +
  geom_hline(aes(yintercept = 0.9),linetype = "dashed",colour = lighten(mycolours[6], amount = 0.2)) +
  geom_hline(aes(yintercept = 0.9),linetype = "dashed",colour = lighten(mycolours[8], amount = 0.2)) +
  geom_hline(aes(yintercept = 0.77),linetype = "dashed",colour = lighten(mycolours[10], amount = 0.2)) +
  geom_point(shape=selprop_pest$myshape, colour=selprop_pest$mycolour_point, size = 2.5) +
  geom_text_repel(color=selprop_pest$mycolour_lab,
                  nudge_y = 0.02,
                  nudge_x = 0.02,
                  size=3, segment.color="grey",
                  segment.size=0.2) + 
  xlab("Selection Proportion (Females)")+
  ylab("Selection Proportion (Males)") +
  labs(title = "POPs and non-persistent pesticides, LASSO Models") +
  xlim(xlim) +
  ylim(ylim) +
  theme_bw() +
  theme(legend.position = "none")

pdf("../figures/strat_sex_lasso_1000/lasso_sex_selprop_pest.pdf", width=7, height=7)
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

id1 <- ifelse(selprop_pahs$sel.f[1:14]<0.73 & selprop_pahs$sel.m[1:14]<0.9, "N", "Y") 
id2 <- ifelse(selprop_pahs$sel.f[15:28]<0.72 & selprop_pahs$sel.m[15:28]<0.81, "N", "Y") 
id3 <- ifelse(selprop_pahs$sel.f[29:42]<0.89 & selprop_pahs$sel.m[29:42]<0.7, "N", "Y") 
id4 <- ifelse(selprop_pahs$sel.f[43:56]<0.72 & selprop_pahs$sel.m[43:56]<0.64, "N", "Y") 
id5 <- ifelse(selprop_pahs$sel.f[57:70]<0.84 & selprop_pahs$sel.m[57:70]<0.82, "N", "Y") 

selprop_pahs$label_ny <- c(id1, id2, id3, id4, id5)

p4=ggplot(selprop_pahs, aes(sel.f, sel.m,
                            label=ifelse(label_ny == "N","",label))) +
  geom_abline(slope = 1,linetype = "dotted",colour = "grey") +
  geom_vline(aes(xintercept = 0.73),linetype = "dashed",colour = lighten(mycolours[2], amount = 0.2)) +
  geom_vline(aes(xintercept = 0.72),linetype = "dashed",colour = lighten(mycolours[4], amount = 0.2)) +
  geom_vline(aes(xintercept = 0.89),linetype = "dashed",colour = lighten(mycolours[6], amount = 0.2)) +
  geom_vline(aes(xintercept = 0.72),linetype = "dashed",colour = lighten(mycolours[8], amount = 0.2)) +
  geom_vline(aes(xintercept = 0.84),linetype = "dashed",colour = lighten(mycolours[10], amount = 0.2)) +
  geom_hline(aes(yintercept = 0.9),linetype = "dashed",colour = lighten(mycolours[2], amount = 0.2)) +
  geom_hline(aes(yintercept = 0.81),linetype = "dashed",colour = lighten(mycolours[4], amount = 0.2)) +
  geom_hline(aes(yintercept = 0.7),linetype = "dashed",colour = lighten(mycolours[6], amount = 0.2)) +
  geom_hline(aes(yintercept = 0.64),linetype = "dashed",colour = lighten(mycolours[8], amount = 0.2)) +
  geom_hline(aes(yintercept = 0.82),linetype = "dashed",colour = lighten(mycolours[10], amount = 0.2)) +
  geom_point(shape=selprop_pahs$myshape, colour=selprop_pahs$mycolour_point, size = 2.5) +
  geom_text_repel(color=selprop_pahs$mycolour_lab,
                  nudge_y = 0.02,
                  nudge_x = 0.02,
                  size=3, segment.color="grey",
                  segment.size=0.2) + 
  xlab("Selection Proportion (Females)")+
  ylab("Selection Proportion (Males)") +
  labs(title = "Metabolites of PAHs and tobacco-related chemicals, LASSO Models") +
  xlim(xlim) +
  ylim(ylim) +
  theme_bw() +
  theme(legend.position = "none")



pdf("../figures/strat_sex_lasso_1000/lasso_sex_selprop_pahs.pdf", width=7, height=7)
p4
dev.off()



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


##### Load data --
for (m in 1:20){
  arr <- paste0(rep(c("pest_de", "pahs_de"), each = 5), "_", 
                c("obesity", "hypertension", "diabetes", "Lipid_disorder", "MS"),
                rep(c(".f",".m"),each=10))[m]
  
  spls_out=readRDS(paste0("../results/strat_sex_spls_1000/out_",arr,".rds")) # Load output
  assign(paste0("spls_out_",arr),spls_out) # Assign name
  
  hat_params=GetArgmax(spls_out) # Extract calibrated pi
  assign(paste0("hat_params_",arr), hat_params) # Assign name
  
  spls_selprop=readRDS(paste0("../results/strat_sex_spls_1000/selprop_",arr,".rds")) # Load selection proportion
  assign(paste0("spls_selprop_",arr),spls_selprop) # Assign name
  
  spls_beta=readRDS(paste0("../results/strat_sex_spls_1000/beta_",arr,".rds")) # Load beta
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
  df=data.frame(sel.f=unlist(x[,6:10]),sel.m=unlist(x[,1:5]),
                var=rep(rownames(x),5),
                outcomes = rep(outcomes, each=14)) # change accordingly
}


myorder <- names(spls_beta_pest_de_obesity.f)
coef_pest<- foo3(spls_beta_pest_de_obesity.f, spls_beta_pest_de_Lipid_disorder.f,
                 spls_beta_pest_de_hypertension.f, spls_beta_pest_de_diabetes.f,
                 spls_beta_pest_de_MS.f, 
                 spls_beta_pest_de_obesity.m, spls_beta_pest_de_Lipid_disorder.m,
                 spls_beta_pest_de_hypertension.m, spls_beta_pest_de_diabetes.m,
                 spls_beta_pest_de_MS.m, 
                 myorder)

myorder <- names(spls_selprop_pest_de_obesity.f)
selprop_pest<- foo2(spls_selprop_pest_de_obesity.f, spls_selprop_pest_de_Lipid_disorder.f,
                    spls_selprop_pest_de_hypertension.f, spls_selprop_pest_de_diabetes.f,
                    spls_selprop_pest_de_MS.f, 
                    spls_selprop_pest_de_obesity.m, spls_selprop_pest_de_Lipid_disorder.m,
                    spls_selprop_pest_de_hypertension.m, spls_selprop_pest_de_diabetes.m,
                    spls_selprop_pest_de_MS.m, 
                    myorder)


myorder <- names(spls_beta_pahs_de_obesity.f)
spls_beta_pahs_de_obesity.f <- spls_beta_pahs_de_obesity.f*(-1)
spls_beta_pahs_de_hypertension.m <- spls_beta_pahs_de_hypertension.m*(-1)
coef_pahs<- foo3(spls_beta_pahs_de_obesity.f, spls_beta_pahs_de_Lipid_disorder.f,
                 spls_beta_pahs_de_hypertension.f, spls_beta_pahs_de_diabetes.f,
                 spls_beta_pahs_de_MS.f, 
                 spls_beta_pahs_de_obesity.m, spls_beta_pahs_de_Lipid_disorder.m,
                 spls_beta_pahs_de_hypertension.m, spls_beta_pahs_de_diabetes.m,
                 spls_beta_pahs_de_MS.m, 
                 myorder)

myorder <- names(spls_selprop_pahs_de_obesity.f)

selprop_pahs<- foo2(spls_selprop_pahs_de_obesity.f, spls_selprop_pahs_de_Lipid_disorder.f,
                    spls_selprop_pahs_de_hypertension.f, spls_selprop_pahs_de_diabetes.f,
                    spls_selprop_pahs_de_MS.f, 
                    spls_selprop_pahs_de_obesity.m, spls_selprop_pahs_de_Lipid_disorder.m,
                    spls_selprop_pahs_de_hypertension.m, spls_selprop_pahs_de_diabetes.m,
                    spls_selprop_pahs_de_MS.m, 
                    myorder)


##### Beta plot --
# sPLS Pesticides
xlim=c(-0.4, 1)
ylim=c(-0.5,0.5)
#coef_lung$var_cat=rep(variable_cat)
coef_pest$label=rep(1:(nrow(coef_pest)/5), 5)
coef_pest$mycolour_point=rep(mycolours[c(2,4,6,8,10)],each = 35)

coef_pest$mycolour_lab=darken(coef_pest$mycolour_point, amount=0.2)
coef_pest$myshape <- rep(c(15, 16, 17, 18, 25),  each = 35)

p5=ggplot(coef_pest, aes(or.f, or.m,
                         label=ifelse(((abs(or.f)<0.05&or.m==0)|(or.f==0&abs(or.m)<0.05)),
                                      "",label))) +
  geom_vline(aes(xintercept = 0),linetype = "dashed",colour = "black") +
  geom_hline(aes(yintercept = 0),linetype = "dashed",colour = "black") +
  geom_point(shape=coef_pest$myshape, colour=coef_pest$mycolour_point, size = 2.5) +
  geom_text_repel(color=coef_pest$mycolour_lab,
                  nudge_y = 0.03,
                  nudge_x = 0.04,
                  size=4, segment.color="grey",
                  segment.size=0.2) + 
  xlab("Mean Loading Coefficients (Females)")+
  ylab("Mean Loading Coefficients (Males)") +
  ggtitle("POPs and non-persistent pesticides, sPLS Models") +
  xlim(xlim) +
  ylim(ylim) +
  theme_bw() +
  theme(legend.position = "none")

pdf("../figures/strat_sex_spls_1000/spls_sex_or_pest.pdf", width=7, height=7)
p5
dev.off()


# sPLS PAHs
xlim=c(-0.5, 0.5)
ylim=c(-0.5,0.75)
#coef_lung$var_cat=rep(variable_cat)
coef_pahs$label=rep(1:(nrow(coef_pahs)/5), 5)
coef_pahs$mycolour_point=rep(mycolours[c(2,4,6,8,10)],each = 14)

coef_pahs$mycolour_lab=darken(coef_pahs$mycolour_point, amount=0.2)
coef_pahs$myshape <- rep(c(15, 16, 17, 18, 25),  each = 14)

p6=ggplot(coef_pahs, aes(or.f, or.m,
                         label=ifelse(((abs(or.f)<0.05&or.m==0)|(or.f==0&abs(or.m)<0.05)),
                                      "",label))) +
  geom_vline(aes(xintercept = 0),linetype = "dashed",colour = "black") +
  geom_hline(aes(yintercept = 0),linetype = "dashed",colour = "black") +
  geom_point(shape=coef_pahs$myshape, colour=coef_pahs$mycolour_point, size = 3) +
  geom_text_repel(color=coef_pahs$mycolour_lab,
                  nudge_y = 0.03,
                  nudge_x = 0.04,
                  size=5, segment.color="grey",
                  segment.size=0.2) + 
  xlab("Mean Loading Coefficients (Females)")+
  ylab("Mean Loading Coefficients (Males)") +
  ggtitle("Metabolites of PAHs and tobacco-related chemicals, sPLS Models") +
  xlim(xlim) +
  ylim(ylim) +
  theme_bw() +
  theme(legend.position = "none")

pdf("../figures/strat_sex_spls_1000/spls_sex_or_pahs.pdf", width=7, height=7)
p6
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

id1 <- ifelse(selprop_pest$sel.f[1:35]<0.9 & selprop_pest$sel.m[1:35]<0.82, "N", "Y") 
id2 <- ifelse(selprop_pest$sel.f[36:70]<0.81 & selprop_pest$sel.m[36:70]<0.86, "N", "Y") 
id3 <- ifelse(selprop_pest$sel.f[71:105]<0.88 & selprop_pest$sel.m[71:105]<0.89, "N", "Y") 
id4 <- ifelse(selprop_pest$sel.f[106:140]<0.85 & selprop_pest$sel.m[106:140]<0.86, "N", "Y") 
id5 <- ifelse(selprop_pest$sel.f[141:175]<0.87 & selprop_pest$sel.m[141:175]<0.84, "N", "Y") 

selprop_pest$label_ny <- c(id1, id2, id3, id4, id5)

p7=ggplot(selprop_pest, aes(sel.f, sel.m,
                            label=ifelse(label_ny == "N","",label))) +
  geom_abline(slope = 1,linetype = "dotted",colour = "grey") +
  geom_vline(aes(xintercept = 0.9),linetype = "dashed",colour = lighten(mycolours[2], amount = 0.2)) +
  geom_vline(aes(xintercept = 0.81),linetype = "dashed",colour = lighten(mycolours[4], amount = 0.2)) +
  geom_vline(aes(xintercept = 0.88),linetype = "dashed",colour = lighten(mycolours[6], amount = 0.2)) +
  geom_vline(aes(xintercept = 0.85),linetype = "dashed",colour = lighten(mycolours[8], amount = 0.2)) +
  geom_vline(aes(xintercept = 0.87),linetype = "dashed",colour = lighten(mycolours[10], amount = 0.2)) +
  geom_hline(aes(yintercept = 0.82),linetype = "dashed",colour = lighten(mycolours[2], amount = 0.2)) +
  geom_hline(aes(yintercept = 0.86),linetype = "dashed",colour = lighten(mycolours[4], amount = 0.2)) +
  geom_hline(aes(yintercept = 0.89),linetype = "dashed",colour = lighten(mycolours[6], amount = 0.2)) +
  geom_hline(aes(yintercept = 0.86),linetype = "dashed",colour = lighten(mycolours[8], amount = 0.2)) +
  geom_hline(aes(yintercept = 0.84),linetype = "dashed",colour = lighten(mycolours[10], amount = 0.2)) +
  geom_point(shape=selprop_pest$myshape, colour=selprop_pest$mycolour_point, size = 3) +
  geom_text_repel(color=selprop_pest$mycolour_lab,
                  nudge_y = 0.02,
                  nudge_x = 0.02,
                  size=3, segment.color="grey",
                  segment.size=0.2) + 
  xlab("Selection Proportion (Females)")+
  ylab("Selection Proportion (Males)") +
  labs(title = "POPs and non-persistent pesticides, sPLS Models") +
  xlim(xlim) +
  ylim(ylim) +
  theme_bw() +
  theme(legend.position = "none")

pdf("../figures/strat_sex_spls_1000/spls_sex_selprop_pest.pdf", width=7, height=7)
p7
dev.off()



# PAHs
xlim=c(0, 1)
ylim=c(0, 1)
#coef_lung$var_cat=rep(variable_cat)
selprop_pahs$label=rep(1:(nrow(selprop_pahs)/5), 5)
selprop_pahs$mycolour_point=rep(mycolours[c(2,4,6,8,10)],each = 14)

selprop_pahs$mycolour_lab=darken(selprop_pahs$mycolour_point, amount=0.2)
selprop_pahs$myshape <- rep(c(15, 16, 17, 18, 25),  each = 14)

id1 <- ifelse(selprop_pahs$sel.f[1:14]<0.76 & selprop_pahs$sel.m[1:14]<0.84, "N", "Y") 
id2 <- ifelse(selprop_pahs$sel.f[15:28]<0.63 & selprop_pahs$sel.m[15:28]<0.74, "N", "Y") 
id3 <- ifelse(selprop_pahs$sel.f[29:42]<0.71 & selprop_pahs$sel.m[29:42]<0.78, "N", "Y") 
id4 <- ifelse(selprop_pahs$sel.f[43:56]<0.79 & selprop_pahs$sel.m[43:56]<0.84, "N", "Y") 
id5 <- ifelse(selprop_pahs$sel.f[57:70]<0.85 & selprop_pahs$sel.m[57:70]<0.89, "N", "Y") 

selprop_pahs$label_ny <- c(id1, id2, id3, id4, id5)

p8=ggplot(selprop_pahs, aes(sel.f, sel.m,
                            label=ifelse(label_ny == "N","",label))) +
  geom_abline(slope = 1,linetype = "dotted",colour = "grey") +
  geom_vline(aes(xintercept = 0.76),linetype = "dashed",colour = lighten(mycolours[2], amount = 0.2)) +
  geom_vline(aes(xintercept = 0.63),linetype = "dashed",colour = lighten(mycolours[4], amount = 0.2)) +
  geom_vline(aes(xintercept = 0.71),linetype = "dashed",colour = lighten(mycolours[6], amount = 0.2)) +
  geom_vline(aes(xintercept = 0.79),linetype = "dashed",colour = lighten(mycolours[8], amount = 0.2)) +
  geom_vline(aes(xintercept = 0.85),linetype = "dashed",colour = lighten(mycolours[10], amount = 0.2)) +
  geom_hline(aes(yintercept = 0.84),linetype = "dashed",colour = lighten(mycolours[2], amount = 0.2)) +
  geom_hline(aes(yintercept = 0.74),linetype = "dashed",colour = lighten(mycolours[4], amount = 0.2)) +
  geom_hline(aes(yintercept = 0.78),linetype = "dashed",colour = lighten(mycolours[6], amount = 0.2)) +
  geom_hline(aes(yintercept = 0.84),linetype = "dashed",colour = lighten(mycolours[8], amount = 0.2)) +
  geom_hline(aes(yintercept = 0.89),linetype = "dashed",colour = lighten(mycolours[10], amount = 0.2)) +
  geom_point(shape=selprop_pahs$myshape, colour=selprop_pahs$mycolour_point, size = 2.5) +
  geom_text_repel(color=selprop_pahs$mycolour_lab,
                  nudge_y = 0.02,
                  nudge_x = 0.02,
                  size=3, segment.color="grey",
                  segment.size=0.2) + 
  xlab("Selection Proportion (Females)")+
  ylab("Selection Proportion (Males)") +
  labs(title = "Metabolites of PAHs and tobacco-related chemicals, sPLS Models") +
  xlim(xlim) +
  ylim(ylim) +
  theme_bw() +
  theme(legend.position = "none")



pdf("../figures/strat_sex_spls_1000/spls_sex_selprop_pahs.pdf", width=7, height=7)
p8
dev.off()








