
### Sumemr Project --  Stratified analysis by country: spls lasso
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
for (m in 1:24){
  arr <- c(paste0(rep(c("pest_de"), each = 15), "_", 
                  c("obesity", "hypertension", "diabetes", "Lipid_disorder", "MS"), 
                  rep(c(".f",".b",".l"),each=5)), 
           paste0(rep(c("pahs_de"), each = 9), "_", 
                  c("obesity", "hypertension", "Lipid_disorder", "MS", "diabetes"), 
                  rep(c(".b",".l"), times = c(4,5))))[m]
  
  ### spls
  lasso_out=readRDS(paste0("../results/strat_country_lasso_1000/out_",arr,".rds")) # Load output
  assign(paste0("lasso_out_",arr),lasso_out) # Assign name
  
  lasso_hat_params=GetArgmax(lasso_out) # Extract calibrated pi
  assign(paste0("lasso_hat_params_",arr),lasso_hat_params) # Assign name
  
  lasso_selprop=readRDS(paste0("../results/strat_country_lasso_1000/selprop_",arr,".rds")) # Load selection proportion
  assign(paste0("lasso_selprop_",arr),lasso_selprop) # Assign name
  
  lasso_beta=readRDS(paste0("../results/strat_country_lasso_1000/average_beta_",arr,".rds")) # Load beta
  lasso_beta=ifelse(CalibratedStableRegression(lasso_out) == 1, lasso_beta, 0) # Shrink non-selected beta to zero
  lasso_beta=exp(lasso_beta) # Exponentiate to OR
  assign(paste0("lasso_beta_",arr),lasso_beta) # Assign name
}


for (m in 1:24){
  arr <- c(paste0(rep(c("pest_de"), each = 15), "_", 
                  c("obesity", "hypertension", "diabetes", "Lipid_disorder", "MS"), 
                  rep(c(".f",".b",".l"),each=5)), 
           paste0(rep(c("pahs_de"), each = 9), "_", 
                  c("obesity", "hypertension", "Lipid_disorder", "MS", "diabetes"), 
                  rep(c(".b",".l"), times = c(4,5))))[m]
  
  spls_out=readRDS(paste0("../results/strat_country_spls_1000/out_",arr,".rds")) # Load output
  assign(paste0("spls_out_",arr),spls_out) # Assign name
  
  hat_params=GetArgmax(spls_out) # Extract calibrated pi
  assign(paste0("hat_params_",arr), hat_params) # Assign name
  
  spls_selprop=readRDS(paste0("../results/strat_country_spls_1000/selprop_",arr,".rds")) # Load selection proportion
  assign(paste0("spls_selprop_",arr),spls_selprop) # Assign name
  
  spls_beta=readRDS(paste0("../results/strat_country_spls_1000/beta_",arr,".rds")) # Load beta
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
                outcomes = rep(outcomes, each=13)) # change accordingly
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
                outcomes = rep(outcomes, each=13)) # change accordingly
}


myorder <- names(lasso_beta_pest_de_obesity.l)
coef_pest_b<- foo3(lasso_beta_pest_de_obesity.l, lasso_beta_pest_de_Lipid_disorder.l,
                   lasso_beta_pest_de_hypertension.l, lasso_beta_pest_de_diabetes.l,
                   lasso_beta_pest_de_MS.l, 
                   lasso_beta_pest_de_obesity.b, lasso_beta_pest_de_Lipid_disorder.b,
                   lasso_beta_pest_de_hypertension.b, lasso_beta_pest_de_diabetes.b,
                   lasso_beta_pest_de_MS.b, 
                   myorder)
coef_pest_f<- foo3(lasso_beta_pest_de_obesity.l, lasso_beta_pest_de_Lipid_disorder.l,
                   lasso_beta_pest_de_hypertension.l, lasso_beta_pest_de_diabetes.l,
                   lasso_beta_pest_de_MS.l, 
                   lasso_beta_pest_de_obesity.f, lasso_beta_pest_de_Lipid_disorder.f,
                   lasso_beta_pest_de_hypertension.f, lasso_beta_pest_de_diabetes.f,
                   lasso_beta_pest_de_MS.f, 
                   myorder)

myorder <- names(lasso_selprop_pest_de_obesity.l)
selprop_pest_b <- foo2(lasso_selprop_pest_de_obesity.l, lasso_selprop_pest_de_Lipid_disorder.l,
                       lasso_selprop_pest_de_hypertension.l, lasso_selprop_pest_de_diabetes.l,
                       lasso_selprop_pest_de_MS.l, 
                       lasso_selprop_pest_de_obesity.b, lasso_selprop_pest_de_Lipid_disorder.b,
                       lasso_selprop_pest_de_hypertension.b, lasso_selprop_pest_de_diabetes.b,
                       lasso_selprop_pest_de_MS.b, 
                       myorder)
selprop_pest_f <- foo2(lasso_selprop_pest_de_obesity.l, lasso_selprop_pest_de_Lipid_disorder.l,
                       lasso_selprop_pest_de_hypertension.l, lasso_selprop_pest_de_diabetes.l,
                       lasso_selprop_pest_de_MS.l, 
                       lasso_selprop_pest_de_obesity.f, lasso_selprop_pest_de_Lipid_disorder.f,
                       lasso_selprop_pest_de_hypertension.f, lasso_selprop_pest_de_diabetes.f,
                       lasso_selprop_pest_de_MS.f, 
                       myorder)


myorder <- names(lasso_beta_pahs_de_obesity.l)
lasso_beta_pahs_de_diabetes.b <- lasso_beta_pahs_de_diabetes.l
lasso_beta_pahs_de_diabetes.b <- ifelse(lasso_beta_pahs_de_diabetes.b != 1, 1,lasso_beta_pahs_de_diabetes.b)
coef_pahs<- foo3(lasso_beta_pahs_de_obesity.l, lasso_beta_pahs_de_Lipid_disorder.l,
                 lasso_beta_pahs_de_hypertension.l, lasso_beta_pahs_de_diabetes.l,
                 lasso_beta_pahs_de_MS.l, 
                 lasso_beta_pahs_de_obesity.b, lasso_beta_pahs_de_Lipid_disorder.b,
                 lasso_beta_pahs_de_hypertension.b, lasso_beta_pahs_de_diabetes.b,
                 lasso_beta_pahs_de_MS.b, 
                 myorder)

myorder <- names(lasso_selprop_pahs_de_obesity.l)
lasso_selprop_pahs_de_diabetes.b <- lasso_selprop_pahs_de_diabetes.l
lasso_selprop_pahs_de_diabetes.b <-  ifelse(lasso_selprop_pahs_de_diabetes.b !=0, 0, lasso_selprop_pahs_de_diabetes.b)
selprop_pahs<- foo2(lasso_selprop_pahs_de_obesity.l, lasso_selprop_pahs_de_Lipid_disorder.l,
                    lasso_selprop_pahs_de_hypertension.l, lasso_selprop_pahs_de_diabetes.l,
                    lasso_selprop_pahs_de_MS.l, 
                    lasso_selprop_pahs_de_obesity.b, lasso_selprop_pahs_de_Lipid_disorder.b,
                    lasso_selprop_pahs_de_hypertension.b, lasso_selprop_pahs_de_diabetes.b,
                    lasso_selprop_pahs_de_MS.b, 
                    myorder)


##### Beta plot --
mylabel_pest <- c(colnames(pest_imp)[1:4],
                  "p,p-DDT", "p,p-DDE",
                  colnames(pest_imp)[7:33])
# LASSO Pesticides L+B
xlim=c(0.8, 1.6)
ylim=c(0.8,1.6)
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
  xlab("Mean Odds Ratio (Luxembourg)")+
  ylab("Mean Odds Ratio (Belgium)") +
  ggtitle("POPs and non-persistent pesticides, LASSO Models") +
  xlim(xlim) +
  ylim(ylim) +
  theme_bw() +
  theme(legend.position = "none")

pdf("../figures/strat_country_lasso_1000/lasso_country_or_pest_b.pdf", width=7, height=7)
p1
dev.off()

p1_1=ggplot(coef_pest_b, aes(or.f, or.m,
                           label=ifelse(((abs(or.f-1)<0.05&or.m==1)|(or.f==1&abs(or.m-1)<0.05)),
                                        "", mylabel_pest))) +
  geom_vline(aes(xintercept = 1),linetype = "dashed",colour = "black") +
  geom_hline(aes(yintercept = 1),linetype = "dashed",colour = "black") +
  geom_point(shape=coef_pest_b$myshape, colour=coef_pest_b$mycolour_point, size = 2) +
  geom_text_repel(color=coef_pest_b$mycolour_lab,
                  nudge_y = 0.03,
                  nudge_x = 0.04,
                  size=3, segment.color="grey",
                  segment.size=0.2) + 
  xlab("Mean Odds Ratio (Luxembourg)")+
  ylab("Mean Odds Ratio (Belgium)") +
  ggtitle("POPs and non-persistent pesticides, LASSO Models") +
  xlim(xlim) +
  ylim(ylim) +
  theme_bw() +
  theme(legend.position = "none")

cairo_pdf("../figures/strat_country_lasso_1000/ppt_lasso_country_or_pest_b.pdf", width=7, height=7)
p1_1
dev.off()


# LASSO Pesticides L+F
xlim=c(0.8, 1.6)
ylim=c(0.5,2.6)
#coef_lung$var_cat=rep(variable_cat)
coef_pest_f$label=rep(1:(nrow(coef_pest_f)/5), 5)
coef_pest_f$mycolour_point=rep(mycolours[c(2,4,6,8,10)],each = 33)

coef_pest_f$mycolour_lab=darken(coef_pest_f$mycolour_point, amount=0.2)
coef_pest_f$myshape <- rep(c(15, 16, 17, 18, 25),  each = 33)

p2=ggplot(coef_pest_f, aes(or.f, or.m,
                           label=ifelse(((abs(or.f-1)<0.05&or.m==1)|(or.f==1&abs(or.m-1)<0.05)),
                                        "",label))) +
  geom_vline(aes(xintercept = 1),linetype = "dashed",colour = "black") +
  geom_hline(aes(yintercept = 1),linetype = "dashed",colour = "black") +
  geom_point(shape=coef_pest_f$myshape, colour=coef_pest_f$mycolour_point, size = 2.5) +
  geom_text_repel(color=coef_pest_f$mycolour_lab,
                  nudge_y = 0.03,
                  nudge_x = 0.04,
                  size=4, segment.color="grey",
                  segment.size=0.2) + 
  xlab("Mean Odds Ratio (Luxembourg)")+
  ylab("Mean Odds Ratio (France)") +
  ggtitle("POPs and non-persistent pesticides, LASSO Models") +
  xlim(xlim) +
  ylim(ylim) +
  theme_bw() +
  theme(legend.position = "none")

pdf("../figures/strat_country_lasso_1000/lasso_country_or_pest_f.pdf", width=7, height=7)
p2
dev.off()


p2_1=ggplot(coef_pest_f, aes(or.f, or.m,
                           label=ifelse(((abs(or.f-1)<0.05&or.m==1)|(or.f==1&abs(or.m-1)<0.05)),
                                        "", mylabel_pest))) +
  geom_vline(aes(xintercept = 1),linetype = "dashed",colour = "black") +
  geom_hline(aes(yintercept = 1),linetype = "dashed",colour = "black") +
  geom_point(shape=coef_pest_f$myshape, colour=coef_pest_f$mycolour_point, size = 2) +
  geom_text_repel(color=coef_pest_f$mycolour_lab,
                  nudge_y = 0.04,
                  nudge_x = 0.05,
                  size=3, segment.color="grey",
                  segment.size=0.2) + 
  xlab("Mean Odds Ratio (Luxembourg)")+
  ylab("Mean Odds Ratio (France)") +
  ggtitle("POPs and non-persistent pesticides, LASSO Models") +
  xlim(xlim) +
  ylim(ylim) +
  theme_bw() +
  theme(legend.position = "none")

cairo_pdf("../figures/strat_country_lasso_1000/ppt_lasso_country_or_pest_f.pdf", width=7, height=7)
p2_1
dev.off()


# LASSO PAHs
xlim=c(0.8, 1.4)
ylim=c(0.3,1.7)
#coef_lung$var_cat=rep(variable_cat)
coef_pahs$label=rep(1:(nrow(coef_pahs)/5), 5)
coef_pahs$mycolour_point=rep(mycolours[c(2,4,6,8,10)],each = 13)

coef_pahs$mycolour_lab=darken(coef_pahs$mycolour_point, amount=0.2)
coef_pahs$myshape <- rep(c(15, 16, 17, 18, 25),  each = 13)

p3=ggplot(coef_pahs, aes(or.f, or.m,
                         label=ifelse(((abs(or.f-1)<0.05&or.m==1)|(or.f==1&abs(or.m-1)<0.05)),
                                      "",label))) +
  geom_vline(aes(xintercept = 1),linetype = "dashed",colour = "black") +
  geom_hline(aes(yintercept = 1),linetype = "dashed",colour = "black") +
  geom_point(shape=coef_pahs$myshape, colour=coef_pahs$mycolour_point, size = 3) +
  geom_text_repel(color=coef_pahs$mycolour_lab,
                  nudge_y = 0.02,
                  nudge_x = 0.03,
                  size=5, segment.color="grey",
                  segment.size=0.2) + 
  xlab("Mean Odds Ratio (Luxembourg)")+
  ylab("Mean Odds Ratio (Belgium)") +
  ggtitle("Metabolites of PAHs and tobacco-related chemicals, LASSO Models") +
  xlim(xlim) +
  ylim(ylim) +
  theme_bw() +
  theme(legend.position = "none")

pdf("../figures/strat_country_lasso_1000/lasso_country_or_pahs.pdf", width=7, height=7)
p3
dev.off()


mylabel_pahs <- c("Cotinine", "Nicotine", colnames(pahs)[3:13])
p3_1=ggplot(coef_pahs, aes(or.f, or.m,
                         label=ifelse(((abs(or.f-1)<0.05&or.m==1)|(or.f==1&abs(or.m-1)<0.05)),
                                      "", mylabel_pahs))) +
  geom_vline(aes(xintercept = 1),linetype = "dashed",colour = "black") +
  geom_hline(aes(yintercept = 1),linetype = "dashed",colour = "black") +
  geom_point(shape=coef_pahs$myshape, colour=coef_pahs$mycolour_point, size = 2) +
  geom_text_repel(color=coef_pahs$mycolour_lab,
                  nudge_y = 0.02,
                  nudge_x = 0.03,
                  size=3, segment.color="grey",
                  segment.size=0.2) + 
  xlab("Mean Odds Ratio (Luxembourg)")+
  ylab("Mean Odds Ratio (Belgium)") +
  ggtitle("Metabolites of PAHs and tobacco-related chemicals, LASSO Models") +
  xlim(xlim) +
  ylim(ylim) +
  theme_bw() +
  theme(legend.position = "none")

pdf("../figures/strat_country_lasso_1000/ppt_lasso_country_or_pahs.pdf", width=7, height=7)
p3_1
dev.off()



##### Selection proportion --
# LASSO Pesticides L+B
xlim=c(0, 1)
ylim=c(0, 1)
#coef_lung$var_cat=rep(variable_cat)
selprop_pest_b$label=rep(1:(nrow(selprop_pest_b)/5), 5)
selprop_pest_b$mycolour_point=rep(mycolours[c(2,4,6,8,10)],each = 33)

selprop_pest_b$mycolour_lab=darken(selprop_pest_b$mycolour_point, amount=0.2)
selprop_pest_b$myshape <- rep(c(15, 16, 17, 18, 25),  each = 33)

id1 <- ifelse(selprop_pest_b$sel.f[1:33]<0.9 & selprop_pest_b$sel.m[1:33]<0.88, "N", "Y") 
id2 <- ifelse(selprop_pest_b$sel.f[34:66]<0.76 & selprop_pest_b$sel.m[34:66]<0.78, "N", "Y") 
id3 <- ifelse(selprop_pest_b$sel.f[67:99]<0.73 & selprop_pest_b$sel.m[67:99]<0.86, "N", "Y") 
id4 <- ifelse(selprop_pest_b$sel.f[100:132]<0.9 & selprop_pest_b$sel.m[100:132]<0.9, "N", "Y") 
id5 <- ifelse(selprop_pest_b$sel.f[133:165]<0.9 & selprop_pest_b$sel.m[133:165]<0.9, "N", "Y") 

selprop_pest_b$label_ny <- c(id1, id2, id3, id4, id5)

p4=ggplot(selprop_pest_b, aes(sel.f, sel.m,
                            label=ifelse(label_ny == "N","",label))) +
  geom_abline(slope = 1,linetype = "dotted",colour = "grey") +
  geom_vline(aes(xintercept = 0.9),linetype = "dashed",colour = lighten(mycolours[2], amount = 0.2)) +
  geom_vline(aes(xintercept = 0.76),linetype = "dashed",colour = lighten(mycolours[4], amount = 0.2)) +
  geom_vline(aes(xintercept = 0.73),linetype = "dashed",colour = lighten(mycolours[6], amount = 0.2)) +
  geom_vline(aes(xintercept = 0.9),linetype = "dashed",colour = lighten(mycolours[8], amount = 0.2)) +
  geom_vline(aes(xintercept = 0.9),linetype = "dashed",colour = lighten(mycolours[10], amount = 0.2)) +
  geom_hline(aes(yintercept = 0.88),linetype = "dashed",colour = lighten(mycolours[2], amount = 0.2)) +
  geom_hline(aes(yintercept = 0.78),linetype = "dashed",colour = lighten(mycolours[4], amount = 0.2)) +
  geom_hline(aes(yintercept = 0.86),linetype = "dashed",colour = lighten(mycolours[6], amount = 0.2)) +
  geom_hline(aes(yintercept = 0.9),linetype = "dashed",colour = lighten(mycolours[8], amount = 0.2)) +
  geom_hline(aes(yintercept = 0.9),linetype = "dashed",colour = lighten(mycolours[10], amount = 0.2)) +
  geom_point(shape=selprop_pest_b$myshape, colour=selprop_pest_b$mycolour_point, size = 2.5) +
  geom_text_repel(color=selprop_pest_b$mycolour_lab,
                  nudge_y = 0.02,
                  nudge_x = 0.02,
                  size=3, segment.color="grey",
                  segment.size=0.2) + 
  xlab("Selection Proportion (Luxembourg)")+
  ylab("Selection Proportion (Belgium)") +
  labs(title = "POPs and non-persistent pesticides, LASSO Models") +
  xlim(xlim) +
  ylim(ylim) +
  theme_bw() +
  theme(legend.position = "none")

pdf("../figures/strat_country_lasso_1000/lasso_country_selprop_pest_b.pdf", width=7, height=7)
p4
dev.off()


# LASSO Pesticides L+F
xlim=c(0, 1)
ylim=c(0, 1)
#coef_lung$var_cat=rep(variable_cat)
selprop_pest_f$label=rep(1:(nrow(selprop_pest_f)/5), 5)
selprop_pest_f$mycolour_point=rep(mycolours[c(2,4,6,8,10)],each = 33)

selprop_pest_f$mycolour_lab=darken(selprop_pest_f$mycolour_point, amount=0.2)
selprop_pest_f$myshape <- rep(c(15, 16, 17, 18, 25),  each = 33)

id1 <- ifelse(selprop_pest_f$sel.f[1:33]<0.9 & selprop_pest_f$sel.m[1:33]<0.83, "N", "Y") 
id2 <- ifelse(selprop_pest_f$sel.f[34:66]<0.76 & selprop_pest_f$sel.m[34:66]<0.85, "N", "Y") 
id3 <- ifelse(selprop_pest_f$sel.f[67:99]<0.73 & selprop_pest_f$sel.m[67:99]<0.83, "N", "Y") 
id4 <- ifelse(selprop_pest_f$sel.f[100:132]<0.9 & selprop_pest_f$sel.m[100:132]<0.9, "N", "Y") 
id5 <- ifelse(selprop_pest_f$sel.f[133:165]<0.9 & selprop_pest_f$sel.m[133:165]<0.6, "N", "Y") 

selprop_pest_f$label_ny <- c(id1, id2, id3, id4, id5)

p5=ggplot(selprop_pest_f, aes(sel.f, sel.m,
                              label=ifelse(label_ny == "N","",label))) +
  geom_abline(slope = 1,linetype = "dotted",colour = "grey") +
  geom_vline(aes(xintercept = 0.9),linetype = "dashed",colour = lighten(mycolours[2], amount = 0.2)) +
  geom_vline(aes(xintercept = 0.76),linetype = "dashed",colour = lighten(mycolours[4], amount = 0.2)) +
  geom_vline(aes(xintercept = 0.73),linetype = "dashed",colour = lighten(mycolours[6], amount = 0.2)) +
  geom_vline(aes(xintercept = 0.9),linetype = "dashed",colour = lighten(mycolours[8], amount = 0.2)) +
  geom_vline(aes(xintercept = 0.9),linetype = "dashed",colour = lighten(mycolours[10], amount = 0.2)) +
  geom_hline(aes(yintercept = 0.83),linetype = "dashed",colour = lighten(mycolours[2], amount = 0.2)) +
  geom_hline(aes(yintercept = 0.85),linetype = "dashed",colour = lighten(mycolours[4], amount = 0.2)) +
  geom_hline(aes(yintercept = 0.83),linetype = "dashed",colour = lighten(mycolours[6], amount = 0.2)) +
  geom_hline(aes(yintercept = 0.9),linetype = "dashed",colour = lighten(mycolours[8], amount = 0.2)) +
  geom_hline(aes(yintercept = 0.6),linetype = "dashed",colour = lighten(mycolours[10], amount = 0.2)) +
  geom_point(shape=selprop_pest_f$myshape, colour=selprop_pest_f$mycolour_point, size = 2.5) +
  geom_text_repel(color=selprop_pest_f$mycolour_lab,
                  nudge_y = 0.02,
                  nudge_x = 0.02,
                  size=3, segment.color="grey",
                  segment.size=0.2) + 
  xlab("Selection Proportion (Luxembourg)")+
  ylab("Selection Proportion (Belgium)") +
  labs(title = "POPs and non-persistent pesticides, LASSO Models") +
  xlim(xlim) +
  ylim(ylim) +
  theme_bw() +
  theme(legend.position = "none")

pdf("../figures/strat_country_lasso_1000/lasso_country_selprop_pest_f.pdf", width=7, height=7)
p5
dev.off()


# PAHs
xlim=c(0, 1)
ylim=c(0, 1)
#coef_lung$var_cat=rep(variable_cat)
selprop_pahs$label=rep(1:(nrow(selprop_pahs)/5), 5)
selprop_pahs$mycolour_point=rep(mycolours[c(2,4,6,8,10)],each = 13)

selprop_pahs$mycolour_lab=darken(selprop_pahs$mycolour_point, amount=0.2)
selprop_pahs$myshape <- rep(c(15, 16, 17, 18, 25),  each = 13)

id1 <- ifelse(selprop_pahs$sel.f[1:13]<0.8 & selprop_pahs$sel.m[1:13]<0.89, "N", "Y") 
id2 <- ifelse(selprop_pahs$sel.f[14:26]<0.83 & selprop_pahs$sel.m[14:26]<0.86, "N", "Y") 
id3 <- ifelse(selprop_pahs$sel.f[27:39]<0.82 & selprop_pahs$sel.m[27:39]<0.76, "N", "Y") 
id4 <- ifelse(selprop_pahs$sel.f[40:52]<0.9 & selprop_pahs$sel.m[40:52]<0.5, "N", "Y") 
id5 <- ifelse(selprop_pahs$sel.f[53:65]<0.9 & selprop_pahs$sel.m[53:65]<0.72, "N", "Y") 

selprop_pahs$label_ny <- c(id1, id2, id3, id4, id5)

p6=ggplot(selprop_pahs, aes(sel.f, sel.m,
                            label=ifelse(label_ny == "N","",label))) +
  geom_abline(slope = 1,linetype = "dotted",colour = "grey") +
  geom_vline(aes(xintercept = 0.8),linetype = "dashed",colour = lighten(mycolours[2], amount = 0.2)) +
  geom_vline(aes(xintercept = 0.83),linetype = "dashed",colour = lighten(mycolours[4], amount = 0.2)) +
  geom_vline(aes(xintercept = 0.82),linetype = "dashed",colour = lighten(mycolours[6], amount = 0.2)) +
  geom_vline(aes(xintercept = 0.9),linetype = "dashed",colour = lighten(mycolours[8], amount = 0.2)) +
  geom_vline(aes(xintercept = 0.9),linetype = "dashed",colour = lighten(mycolours[10], amount = 0.2)) +
  geom_hline(aes(yintercept = 0.89),linetype = "dashed",colour = lighten(mycolours[2], amount = 0.2)) +
  geom_hline(aes(yintercept = 0.86),linetype = "dashed",colour = lighten(mycolours[4], amount = 0.2)) +
  geom_hline(aes(yintercept = 0.76),linetype = "dashed",colour = lighten(mycolours[6], amount = 0.2)) +
  #geom_hline(aes(yintercept = 0.64),linetype = "dashed",colour = lighten(mycolours[8], amount = 0.2)) +
  geom_hline(aes(yintercept = 0.72),linetype = "dashed",colour = lighten(mycolours[10], amount = 0.2)) +
  geom_point(shape=selprop_pahs$myshape, colour=selprop_pahs$mycolour_point, size = 2.5) +
  geom_text_repel(color=selprop_pahs$mycolour_lab,
                  nudge_y = 0.02,
                  nudge_x = 0.02,
                  size=3, segment.color="grey",
                  segment.size=0.2) + 
  xlab("Selection Proportion (Luxembourg)")+
  ylab("Selection Proportion (Belgium)") +
  labs(title = "Metabolites of PAHs and tobacco-related chemicals, LASSO Models") +
  xlim(xlim) +
  ylim(ylim) +
  theme_bw() +
  theme(legend.position = "none")



pdf("../figures/strat_country_lasso_1000/lasso_country_selprop_pahs.pdf", width=7, height=7)
p6
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
                outcomes = rep(outcomes, each=13)) # change accordingly
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
                outcomes = rep(outcomes, each=13)) # change accordingly
}


##### Inverse --
spls_beta_pest_de_obesity.f <- spls_beta_pest_de_obesity.f*(-1)
spls_beta_pest_de_obesity.l <- spls_beta_pest_de_obesity.l*(-1)
spls_beta_pest_de_Lipid_disorder.f <- spls_beta_pest_de_Lipid_disorder.f*(-1)
spls_beta_pest_de_hypertension.f <- spls_beta_pest_de_hypertension.f*(-1)
spls_beta_pest_de_diabetes.f <- spls_beta_pest_de_diabetes.f*(-1)
spls_beta_pest_de_diabetes.l <- spls_beta_pest_de_diabetes.l*(-1)
spls_beta_pest_de_diabetes.b <- spls_beta_pest_de_diabetes.b*(-1)
spls_beta_pest_de_MS.f <- spls_beta_pest_de_MS.f*(-1)
spls_beta_pest_de_MS.l <- spls_beta_pest_de_MS.l*(-1)

spls_beta_pahs_de_obesity.l <- spls_beta_pahs_de_obesity.l*(-1)
spls_beta_pahs_de_Lipid_disorder.l <- spls_beta_pahs_de_Lipid_disorder.l*(-1)
spls_beta_pahs_de_Lipid_disorder.b <- spls_beta_pahs_de_Lipid_disorder.b*(-1)
spls_beta_pahs_de_hypertension.b <- spls_beta_pahs_de_hypertension.b*(-1)
spls_beta_pahs_de_diabetes.l <- spls_beta_pahs_de_diabetes.l*(-1)
spls_beta_pahs_de_MS.l <- spls_beta_pahs_de_MS.l*(-1)


myorder <- names(spls_beta_pest_de_obesity.l)
coef_pest_b<- foo3(spls_beta_pest_de_obesity.l, spls_beta_pest_de_Lipid_disorder.l,
                   spls_beta_pest_de_hypertension.l, spls_beta_pest_de_diabetes.l,
                   spls_beta_pest_de_MS.l, 
                   spls_beta_pest_de_obesity.b, spls_beta_pest_de_Lipid_disorder.b,
                   spls_beta_pest_de_hypertension.b, spls_beta_pest_de_diabetes.b,
                   spls_beta_pest_de_MS.b, 
                   myorder)
coef_pest_f<- foo3(spls_beta_pest_de_obesity.l, spls_beta_pest_de_Lipid_disorder.l,
                   spls_beta_pest_de_hypertension.l, spls_beta_pest_de_diabetes.l,
                   spls_beta_pest_de_MS.l, 
                   spls_beta_pest_de_obesity.f, spls_beta_pest_de_Lipid_disorder.f,
                   spls_beta_pest_de_hypertension.f, spls_beta_pest_de_diabetes.f,
                   spls_beta_pest_de_MS.f, 
                   myorder)

myorder <- names(spls_selprop_pest_de_obesity.l)
selprop_pest_b <- foo2(spls_selprop_pest_de_obesity.l, spls_selprop_pest_de_Lipid_disorder.l,
                       spls_selprop_pest_de_hypertension.l, spls_selprop_pest_de_diabetes.l,
                       spls_selprop_pest_de_MS.l, 
                       spls_selprop_pest_de_obesity.b, spls_selprop_pest_de_Lipid_disorder.b,
                       spls_selprop_pest_de_hypertension.b, spls_selprop_pest_de_diabetes.b,
                       spls_selprop_pest_de_MS.b, 
                       myorder)
selprop_pest_f <- foo2(spls_selprop_pest_de_obesity.l, spls_selprop_pest_de_Lipid_disorder.l,
                       spls_selprop_pest_de_hypertension.l, spls_selprop_pest_de_diabetes.l,
                       spls_selprop_pest_de_MS.l, 
                       spls_selprop_pest_de_obesity.f, spls_selprop_pest_de_Lipid_disorder.f,
                       spls_selprop_pest_de_hypertension.f, spls_selprop_pest_de_diabetes.f,
                       spls_selprop_pest_de_MS.f, 
                       myorder)


myorder <- names(spls_beta_pahs_de_obesity.l)
spls_beta_pahs_de_diabetes.b <- spls_beta_pahs_de_diabetes.l
spls_beta_pahs_de_diabetes.b <- ifelse(spls_beta_pahs_de_diabetes.b != 0, 0,spls_beta_pahs_de_diabetes.b)
coef_pahs<- foo3(spls_beta_pahs_de_obesity.l, spls_beta_pahs_de_Lipid_disorder.l,
                 spls_beta_pahs_de_hypertension.l, spls_beta_pahs_de_diabetes.l,
                 spls_beta_pahs_de_MS.l, 
                 spls_beta_pahs_de_obesity.b, spls_beta_pahs_de_Lipid_disorder.b,
                 spls_beta_pahs_de_hypertension.b, spls_beta_pahs_de_diabetes.b,
                 spls_beta_pahs_de_MS.b, 
                 myorder)

myorder <- names(spls_selprop_pahs_de_obesity.l)
spls_selprop_pahs_de_diabetes.b <- spls_selprop_pahs_de_diabetes.l
spls_selprop_pahs_de_diabetes.b <-  ifelse(spls_selprop_pahs_de_diabetes.b !=0, 0, spls_selprop_pahs_de_diabetes.b)
selprop_pahs<- foo2(spls_selprop_pahs_de_obesity.l, spls_selprop_pahs_de_Lipid_disorder.l,
                    spls_selprop_pahs_de_hypertension.l, spls_selprop_pahs_de_diabetes.l,
                    spls_selprop_pahs_de_MS.l, 
                    spls_selprop_pahs_de_obesity.b, spls_selprop_pahs_de_Lipid_disorder.b,
                    spls_selprop_pahs_de_hypertension.b, spls_selprop_pahs_de_diabetes.b,
                    spls_selprop_pahs_de_MS.b, 
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
                  size=4, segment.color="grey",
                  segment.size=0.2) + 
  xlab("Mean Loading Coefficient (Luxembourg)")+
  ylab("Mean Loading Coefficient (Belgium)") +
  ggtitle("POPs and non-persistent pesticides, sPLS-DA Models") +
  xlim(xlim) +
  ylim(ylim) +
  theme_bw() +
  theme(legend.position = "none")

pdf("../figures/strat_country_spls_1000/spls_country_or_pest_b.pdf", width=7, height=7)
p7
dev.off()


# spls Pesticides L+F
xlim=c(-0.3, 0.4)
ylim=c(-0.4,0.4)
#coef_lung$var_cat=rep(variable_cat)
coef_pest_f$label=rep(1:(nrow(coef_pest_f)/5), 5)
coef_pest_f$mycolour_point=rep(mycolours[c(2,4,6,8,10)],each = 33)

coef_pest_f$mycolour_lab=darken(coef_pest_f$mycolour_point, amount=0.2)
coef_pest_f$myshape <- rep(c(15, 16, 17, 18, 25),  each = 33)

p8=ggplot(coef_pest_f, aes(or.f, or.m,
                           label=ifelse(((abs(or.f)<0.05&or.m==0)|(or.f==0&abs(or.m)<0.05)),
                                        "",label))) +
  geom_vline(aes(xintercept = 0),linetype = "dashed",colour = "black") +
  geom_hline(aes(yintercept = 0),linetype = "dashed",colour = "black") +
  geom_point(shape=coef_pest_f$myshape, colour=coef_pest_f$mycolour_point, size = 2.5) +
  geom_text_repel(color=coef_pest_f$mycolour_lab,
                  nudge_y = 0.03,
                  nudge_x = 0.04,
                  size=4, segment.color="grey",
                  segment.size=0.2) + 
  xlab("Mean Loading Coefficient (Luxembourg)")+
  ylab("Mean Loading Coefficient (France)") +
  ggtitle("POPs and non-persistent pesticides, sPLS-DA Models") +
  xlim(xlim) +
  ylim(ylim) +
  theme_bw() +
  theme(legend.position = "none")

pdf("../figures/strat_country_spls_1000/spls_country_or_pest_f.pdf", width=7, height=7)
p8
dev.off()


# spls PAHs
xlim=c(0, 0.8)
ylim=c(0,0.4)
#coef_lung$var_cat=rep(variable_cat)
coef_pahs$label=rep(1:(nrow(coef_pahs)/5), 5)
coef_pahs$mycolour_point=rep(mycolours[c(2,4,6,8,10)],each = 13)

coef_pahs$mycolour_lab=darken(coef_pahs$mycolour_point, amount=0.2)
coef_pahs$myshape <- rep(c(15, 16, 17, 18, 25),  each = 13)

p9=ggplot(coef_pahs, aes(or.f, or.m,
                         label=ifelse(((abs(or.f)<0.05&or.m==0)|(or.f==0&abs(or.m)<0.05)),
                                      "",label))) +
  geom_vline(aes(xintercept = 0),linetype = "dashed",colour = "black") +
  geom_hline(aes(yintercept = 0),linetype = "dashed",colour = "black") +
  geom_point(shape=coef_pahs$myshape, colour=coef_pahs$mycolour_point, size = 3) +
  geom_text_repel(color=coef_pahs$mycolour_lab,
                  nudge_y = 0.02,
                  nudge_x = 0.03,
                  size=5, segment.color="grey",
                  segment.size=0.2) + 
  xlab("Mean Loading Coefficient (Luxembourg)")+
  ylab("Mean Loading Coefficient (Belgium)") +
  ggtitle("Metabolites of PAHs and tobacco-related chemicals, sPLS-DA Models") +
  xlim(xlim) +
  ylim(ylim) +
  theme_bw() +
  theme(legend.position = "none")

pdf("../figures/strat_country_spls_1000/spls_country_or_pahs.pdf", width=7, height=7)
p9
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

id1 <- ifelse(selprop_pest_b$sel.f[1:33]<0.85 & selprop_pest_b$sel.m[1:33]<0.87, "N", "Y") 
id2 <- ifelse(selprop_pest_b$sel.f[34:66]<0.72 & selprop_pest_b$sel.m[34:66]<0.81, "N", "Y") 
id3 <- ifelse(selprop_pest_b$sel.f[67:99]<0.66 & selprop_pest_b$sel.m[67:99]<0.9, "N", "Y") 
id4 <- ifelse(selprop_pest_b$sel.f[100:132]<0.87 & selprop_pest_b$sel.m[100:132]<0.87, "N", "Y") 
id5 <- ifelse(selprop_pest_b$sel.f[133:165]<0.8 & selprop_pest_b$sel.m[133:165]<0.75, "N", "Y") 

selprop_pest_b$label_ny <- c(id1, id2, id3, id4, id5)

p10=ggplot(selprop_pest_b, aes(sel.f, sel.m,
                              label=ifelse(label_ny == "N","",label))) +
  geom_abline(slope = 1,linetype = "dotted",colour = "grey") +
  geom_vline(aes(xintercept = 0.85),linetype = "dashed",colour = lighten(mycolours[2], amount = 0.2)) +
  geom_vline(aes(xintercept = 0.72),linetype = "dashed",colour = lighten(mycolours[4], amount = 0.2)) +
  geom_vline(aes(xintercept = 0.66),linetype = "dashed",colour = lighten(mycolours[6], amount = 0.2)) +
  geom_vline(aes(xintercept = 0.87),linetype = "dashed",colour = lighten(mycolours[8], amount = 0.2)) +
  geom_vline(aes(xintercept = 0.8),linetype = "dashed",colour = lighten(mycolours[10], amount = 0.2)) +
  geom_hline(aes(yintercept = 0.87),linetype = "dashed",colour = lighten(mycolours[2], amount = 0.2)) +
  geom_hline(aes(yintercept = 0.81),linetype = "dashed",colour = lighten(mycolours[4], amount = 0.2)) +
  geom_hline(aes(yintercept = 0.9),linetype = "dashed",colour = lighten(mycolours[6], amount = 0.2)) +
  geom_hline(aes(yintercept = 0.87),linetype = "dashed",colour = lighten(mycolours[8], amount = 0.2)) +
  geom_hline(aes(yintercept = 0.75),linetype = "dashed",colour = lighten(mycolours[10], amount = 0.2)) +
  geom_point(shape=selprop_pest_b$myshape, colour=selprop_pest_b$mycolour_point, size = 2.5) +
  geom_text_repel(color=selprop_pest_b$mycolour_lab,
                  nudge_y = 0.02,
                  nudge_x = 0.02,
                  size=3, segment.color="grey",
                  segment.size=0.2) + 
  xlab("Selection Proportion (Luxembourg)")+
  ylab("Selection Proportion (Belgium)") +
  labs(title = "POPs and non-persistent pesticides, sPLS-DA Models") +
  xlim(xlim) +
  ylim(ylim) +
  theme_bw() +
  theme(legend.position = "none")

pdf("../figures/strat_country_spls_1000/spls_country_selprop_pest_b.pdf", width=7, height=7)
p10
dev.off()


# spls Pesticides L+F
xlim=c(0, 1)
ylim=c(0, 1)
#coef_lung$var_cat=rep(variable_cat)
selprop_pest_f$label=rep(1:(nrow(selprop_pest_f)/5), 5)
selprop_pest_f$mycolour_point=rep(mycolours[c(2,4,6,8,10)],each = 33)

selprop_pest_f$mycolour_lab=darken(selprop_pest_f$mycolour_point, amount=0.2)
selprop_pest_f$myshape <- rep(c(15, 16, 17, 18, 25),  each = 33)

id1 <- ifelse(selprop_pest_f$sel.f[1:33]<0.9 & selprop_pest_f$sel.m[1:33]<0.68, "N", "Y") 
id2 <- ifelse(selprop_pest_f$sel.f[34:66]<0.76 & selprop_pest_f$sel.m[34:66]<0.75, "N", "Y") 
id3 <- ifelse(selprop_pest_f$sel.f[67:99]<0.73 & selprop_pest_f$sel.m[67:99]<0.81, "N", "Y") 
id4 <- ifelse(selprop_pest_f$sel.f[100:132]<0.9 & selprop_pest_f$sel.m[100:132]<0.81, "N", "Y") 
id5 <- ifelse(selprop_pest_f$sel.f[133:165]<0.9 & selprop_pest_f$sel.m[133:165]<0.63, "N", "Y") 

selprop_pest_f$label_ny <- c(id1, id2, id3, id4, id5)

p11=ggplot(selprop_pest_f, aes(sel.f, sel.m,
                              label=ifelse(label_ny == "N","",label))) +
  geom_abline(slope = 1,linetype = "dotted",colour = "grey") +
  geom_vline(aes(xintercept = 0.9),linetype = "dashed",colour = lighten(mycolours[2], amount = 0.2)) +
  geom_vline(aes(xintercept = 0.76),linetype = "dashed",colour = lighten(mycolours[4], amount = 0.2)) +
  geom_vline(aes(xintercept = 0.73),linetype = "dashed",colour = lighten(mycolours[6], amount = 0.2)) +
  geom_vline(aes(xintercept = 0.9),linetype = "dashed",colour = lighten(mycolours[8], amount = 0.2)) +
  geom_vline(aes(xintercept = 0.9),linetype = "dashed",colour = lighten(mycolours[10], amount = 0.2)) +
  geom_hline(aes(yintercept = 0.68),linetype = "dashed",colour = lighten(mycolours[2], amount = 0.2)) +
  geom_hline(aes(yintercept = 0.75),linetype = "dashed",colour = lighten(mycolours[4], amount = 0.2)) +
  geom_hline(aes(yintercept = 0.81),linetype = "dashed",colour = lighten(mycolours[6], amount = 0.2)) +
  geom_hline(aes(yintercept = 0.81),linetype = "dashed",colour = lighten(mycolours[8], amount = 0.2)) +
  geom_hline(aes(yintercept = 0.63),linetype = "dashed",colour = lighten(mycolours[10], amount = 0.2)) +
  geom_point(shape=selprop_pest_f$myshape, colour=selprop_pest_f$mycolour_point, size = 2.5) +
  geom_text_repel(color=selprop_pest_f$mycolour_lab,
                  nudge_y = 0.02,
                  nudge_x = 0.02,
                  size=3, segment.color="grey",
                  segment.size=0.2) + 
  xlab("Selection Proportion (Luxembourg)")+
  ylab("Selection Proportion (Belgium)") +
  labs(title = "POPs and non-persistent pesticides, sPLS-DA Models") +
  xlim(xlim) +
  ylim(ylim) +
  theme_bw() +
  theme(legend.position = "none")

pdf("../figures/strat_country_spls_1000/spls_country_selprop_pest_f.pdf", width=7, height=7)
p11
dev.off()


# PAHs
xlim=c(0, 1)
ylim=c(0, 1)
#coef_lung$var_cat=rep(variable_cat)
selprop_pahs$label=rep(1:(nrow(selprop_pahs)/5), 5)
selprop_pahs$mycolour_point=rep(mycolours[c(2,4,6,8,10)],each = 13)

selprop_pahs$mycolour_lab=darken(selprop_pahs$mycolour_point, amount=0.2)
selprop_pahs$myshape <- rep(c(15, 16, 17, 18, 25),  each = 13)

id1 <- ifelse(selprop_pahs$sel.f[1:13]<0.8 & selprop_pahs$sel.m[1:13]<0.67, "N", "Y") 
id2 <- ifelse(selprop_pahs$sel.f[14:26]<0.88 & selprop_pahs$sel.m[14:26]<0.68, "N", "Y") 
id3 <- ifelse(selprop_pahs$sel.f[27:39]<0.6 & selprop_pahs$sel.m[27:39]<0.68, "N", "Y") 
id4 <- ifelse(selprop_pahs$sel.f[40:52]<0.9 & selprop_pahs$sel.m[40:52]<0.5, "N", "Y") 
id5 <- ifelse(selprop_pahs$sel.f[53:65]<0.86 & selprop_pahs$sel.m[53:65]<0.67, "N", "Y") 

selprop_pahs$label_ny <- c(id1, id2, id3, id4, id5)

p12=ggplot(selprop_pahs, aes(sel.f, sel.m,
                            label=ifelse(label_ny == "N","",label))) +
  geom_abline(slope = 1,linetype = "dotted",colour = "grey") +
  geom_vline(aes(xintercept = 0.8),linetype = "dashed",colour = lighten(mycolours[2], amount = 0.2)) +
  geom_vline(aes(xintercept = 0.88),linetype = "dashed",colour = lighten(mycolours[4], amount = 0.2)) +
  geom_vline(aes(xintercept = 0.6),linetype = "dashed",colour = lighten(mycolours[6], amount = 0.2)) +
  geom_vline(aes(xintercept = 0.9),linetype = "dashed",colour = lighten(mycolours[8], amount = 0.2)) +
  geom_vline(aes(xintercept = 0.86),linetype = "dashed",colour = lighten(mycolours[10], amount = 0.2)) +
  geom_hline(aes(yintercept = 0.67),linetype = "dashed",colour = lighten(mycolours[2], amount = 0.2)) +
  geom_hline(aes(yintercept = 0.68),linetype = "dashed",colour = lighten(mycolours[4], amount = 0.2)) +
  geom_hline(aes(yintercept = 0.68),linetype = "dashed",colour = lighten(mycolours[6], amount = 0.2)) +
  #geom_hline(aes(yintercept = 0.64),linetype = "dashed",colour = lighten(mycolours[8], amount = 0.2)) +
  geom_hline(aes(yintercept = 0.67),linetype = "dashed",colour = lighten(mycolours[10], amount = 0.2)) +
  geom_point(shape=selprop_pahs$myshape, colour=selprop_pahs$mycolour_point, size = 2.5) +
  geom_text_repel(color=selprop_pahs$mycolour_lab,
                  nudge_y = 0.02,
                  nudge_x = 0.03,
                  size=3, segment.color="grey",
                  segment.size=0.2) + 
  xlab("Selection Proportion (Luxembourg)")+
  ylab("Selection Proportion (Belgium)") +
  labs(title = "Metabolites of PAHs and tobacco-related chemicals, sPLS-DA Models") +
  xlim(xlim) +
  ylim(ylim) +
  theme_bw() +
  theme(legend.position = "none")



pdf("../figures/strat_country_spls_1000/spls_country_selprop_pahs.pdf", width=7, height=7)
p12
dev.off()




