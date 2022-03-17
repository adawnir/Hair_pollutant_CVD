
### Sumemr Project -- Stability selection LASSO & sPLS Visualisation
## Created by Vivian on 10 August 

rm(list=ls())
project_path="~/hair/scripts"
setwd(project_path)

##### Loading packages --
library(RColorBrewer)
library(tidyverse)
library(plotrix)
library(colorspace)
library(cowplot)
library(ggrepel)

source("penalisation_functions.R")


##### Load data --
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


for (m in 1:10){
  arr=paste0(rep(c("pest_de", "pahs_de"), each = 5), "_", 
             c("obesity", "hypertension", "diabetes", "Lipid_disorder", "MS"))[m]
  
  ### sPLS
  out=readRDS(paste0("../results/spls_1000_uni/out_",arr,".rds")) # Load output
  assign(paste0("out_",arr), out) # Assign name
  
  hat_params=GetArgmax(out) # Extract calibrated pi
  assign(paste0("hat_params_",arr), hat_params) # Assign name
  
  selprop=readRDS(paste0("../results/spls_1000_uni/selprop_",arr,".rds")) # Load selection proportion
  assign(paste0("selprop_",arr), selprop) # Assign name
  
  loading=readRDS(paste0("../results/spls_1000_uni/average_load_",arr,".rds")) # Load beta
  loading=ifelse(CalibratedStableRegression(out) == 1, loading, 0) 
  assign(paste0("load_",arr), loading) # Assign name
}

pest_imp <- readRDS("../processed/imputation/imp_pest.rds")
pest <- readRDS("../processed/uni_multi_pest.rds")
pahs <- readRDS("../processed/imputation/imp_pahs.rds")


mycolours = brewer.pal(10, "Paired")
outcomes <- c("Obesity", "Lipid disorder", "Hypertension", "Diabetes","Metabolic syndrome")

##### functions --
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
  df=data.frame(or=unlist(x[,6:10]),load=unlist(x[,1:5]),
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
  df=data.frame(sel_lasso=unlist(x[,6:10]),sel_pls=unlist(x[,1:5]),
                var=rep(rownames(x),5),
                outcomes = rep(outcomes, each=14)) # change accordingly
}



myorder <- names(lasso_beta_pest_de_obesity)
coef_pest<- foo3(lasso_beta_pest_de_obesity, lasso_beta_pest_de_Lipid_disorder,
                 lasso_beta_pest_de_hypertension, lasso_beta_pest_de_diabetes,
                 lasso_beta_pest_de_MS, 
                 load_pest_de_obesity, load_pest_de_Lipid_disorder,
                 load_pest_de_hypertension, load_pest_de_diabetes,
                 load_pest_de_MS, 
                 myorder)

myorder <- names(selprop_pest_de_obesity)
selprop_pest<- foo2(lasso_selprop_pest_de_obesity, lasso_selprop_pest_de_Lipid_disorder,
                    lasso_selprop_pest_de_hypertension, lasso_selprop_pest_de_diabetes,
                    lasso_selprop_pest_de_MS, 
                    selprop_pest_de_obesity, selprop_pest_de_Lipid_disorder,
                    selprop_pest_de_hypertension, selprop_pest_de_diabetes,
                    selprop_pest_de_MS, 
                    myorder)


myorder <- names(lasso_beta_pahs_de_obesity)
coef_pahs<- foo3(lasso_beta_pahs_de_obesity, lasso_beta_pahs_de_Lipid_disorder,
                 lasso_beta_pahs_de_hypertension, lasso_beta_pahs_de_diabetes,
                 lasso_beta_pahs_de_MS, 
                 load_pahs_de_obesity, load_pahs_de_Lipid_disorder,
                 load_pahs_de_hypertension, load_pahs_de_diabetes,
                 load_pahs_de_MS, 
                 myorder)

myorder <- names(selprop_pahs_de_obesity)
selprop_pahs<- foo2(lasso_selprop_pahs_de_obesity, lasso_selprop_pahs_de_Lipid_disorder,
                    lasso_selprop_pahs_de_hypertension, lasso_selprop_pahs_de_diabetes,
                    lasso_selprop_pahs_de_MS, 
                    selprop_pahs_de_obesity, selprop_pahs_de_Lipid_disorder,
                    selprop_pahs_de_hypertension, selprop_pahs_de_diabetes,
                    selprop_pahs_de_MS, 
                    myorder)





##### Beta plot --
# Pesticides
xlim=c(0.6, 1.4)
ylim=c(-0.5,0.6)
#coef_lung$var_cat=rep(variable_cat)
coef_pest$label=rep(1:(nrow(coef_pest)/5), 5)
coef_pest$mycolour_point=rep(mycolours[c(2,4,6,8,10)],each = 35)

coef_pest$mycolour_lab=darken(coef_pest$mycolour_point, amount=0.2)
coef_pest$myshape <- rep(c(15, 16, 17, 18, 25),  each = 35)

p1=ggplot(coef_pest, aes(or, load,
                         label=ifelse(((abs(or-1)<0.1&load==0)|(or==1&abs(load)<0.1)),
                                      "",label))) +
  geom_vline(aes(xintercept = 1),linetype = "dashed",colour = "black") +
  geom_hline(aes(yintercept = 0),linetype = "dashed",colour = "black") +
  geom_point(shape=coef_pest$myshape, colour=coef_pest$mycolour_point, size = 3) +
  geom_text_repel(color=coef_pest$mycolour_lab,
                  nudge_y = 0.02,
                  nudge_x = 0.02,
                  size=5, segment.color="grey",
                  segment.size=0.2) + 
  xlab("Mean Odds Ratio (LASSO)")+
  ylab("Mean Loading Coefficient (sPLS)") +
  ggtitle("POPs and non-persistent pesticides") +
  xlim(xlim) +
  ylim(ylim) +
  theme_bw() +
  theme(legend.position = "none")

pdf("../figures/lasso_spls/or_load_pest.pdf", width=7, height=7)
p1
dev.off()

mylabel_pest <- c("CountryBelgium","CountryFrance", colnames(pest_imp)[1:4],
                  "p,p-DDT", "p,p-DDE",
                  colnames(pest_imp)[7:33])
p1_1=ggplot(coef_pest, aes(or, load,
                         label=ifelse(((abs(or-1)<0.1&load==0)|(or==1&abs(load)<0.1)),
                                      "",mylabel_pest))) +
  geom_vline(aes(xintercept = 1),linetype = "dashed",colour = "black") +
  geom_hline(aes(yintercept = 0),linetype = "dashed",colour = "black") +
  geom_point(shape=coef_pest$myshape, colour=coef_pest$mycolour_point, size = 3) +
  geom_text_repel(color=coef_pest$mycolour_lab,
                  nudge_y = 0.02,
                  nudge_x = 0.02,
                  size=3.5, segment.color="grey",
                  segment.size=0.2) + 
  xlab("Mean Odds Ratio (LASSO)")+
  ylab("Mean Loading Coefficient (sPLS)") +
  ggtitle("POPs and non-persistent pesticides") +
  xlim(xlim) +
  ylim(ylim) +
  theme_bw() +
  theme(legend.position = "none")

cairo_pdf("../figures/lasso_spls/ppt_or_load_pest.pdf", width=7, height=7)
p1_1
dev.off()


# PAHs
xlim=c(0.9, 1.25)
ylim=c(-0.4,0.75)
#coef_lung$var_cat=rep(variable_cat)
coef_pahs$label=rep(1:(nrow(coef_pahs)/5), 5)
coef_pahs$mycolour_point=rep(mycolours[c(2,4,6,8,10)],each = 14)

coef_pahs$mycolour_lab=darken(coef_pahs$mycolour_point, amount=0.2)
coef_pahs$myshape <- rep(c(15, 16, 17, 18, 25),  each = 14)

p2=ggplot(coef_pahs, aes(or, load,
                         label=ifelse(((abs(or-1)<0.1&load==0)|(or==1&abs(load)<0.1)),
                                      "",label))) +
  geom_vline(aes(xintercept = 1),linetype = "dashed",colour = "black") +
  geom_hline(aes(yintercept = 0),linetype = "dashed",colour = "black") +
  geom_point(shape=coef_pahs$myshape, colour=coef_pahs$mycolour_point, size = 3) +
  geom_text_repel(color=coef_pahs$mycolour_lab,
                  nudge_y = 0.02,
                  nudge_x = 0.02,
                  size=5, segment.color="grey",
                  segment.size=0.2) + 
  xlab("Mean Odds Ratio (LASSO)")+
  ylab("Mean Loading Coefficient (sPLS)") +
  ggtitle("Metabolites of PAHs and tobacco-related chemicals") +
  xlim(xlim) +
  ylim(ylim) +
  theme_bw() +
  theme(legend.position = "none")

pdf("../figures/lasso_spls/or_load_pahs.pdf", width=7, height=7)
p2
dev.off()


mylabel_pahs <- c("CountryBelgium","Cotinine", "Nicotine", colnames(pahs)[3:13])
p2_1=ggplot(coef_pahs, aes(or, load,
                         label=ifelse(((abs(or-1)<0.05&load==0)|(or==1&abs(load)<0.05)),
                                      "", mylabel_pahs))) +
  geom_vline(aes(xintercept = 1),linetype = "dashed",colour = "black") +
  geom_hline(aes(yintercept = 0),linetype = "dashed",colour = "black") +
  geom_point(shape=coef_pahs$myshape, colour=coef_pahs$mycolour_point, size = 3) +
  geom_text_repel(color=coef_pahs$mycolour_lab,
                  nudge_y = 0.03,
                  nudge_x = 0.03,
                  size=3.5, segment.color="grey",
                  segment.size=0.2) + 
  xlab("Mean Odds Ratio (LASSO)")+
  ylab("Mean Loading Coefficient (sPLS)") +
  ggtitle("Metabolites of PAHs and tobacco-related chemicals") +
  xlim(xlim) +
  ylim(ylim) +
  theme_bw() +
  theme(legend.position = "none")

cairo_pdf("../figures/lasso_spls/ppt_or_load_pahs.pdf", width=7, height=7)
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

id1 <- ifelse(selprop_pest$sel_lasso[1:35]<0.9 & selprop_pest$sel_pls[1:35]<0.9, "N", "Y") 
id2 <- ifelse(selprop_pest$sel_lasso[36:70]<0.83 & selprop_pest$sel_pls[36:70]<0.79, "N", "Y") 
id3 <- ifelse(selprop_pest$sel_lasso[71:105]<0.83 & selprop_pest$sel_pls[71:105]<0.9, "N", "Y") 
id4 <- ifelse(selprop_pest$sel_lasso[106:140]<0.88 & selprop_pest$sel_pls[106:140]<0.72, "N", "Y") 
id5 <- ifelse(selprop_pest$sel_lasso[141:175]<0.88 & selprop_pest$sel_pls[141:175]<0.9, "N", "Y") 

selprop_pest$label_ny <- c(id1, id2, id3, id4, id5)

p3=ggplot(selprop_pest, aes(sel_lasso, sel_pls,
                         label=ifelse(label_ny == "N","",label))) +
  geom_abline(slope = 1,linetype = "dotted",colour = "grey") +
  geom_vline(aes(xintercept = 0.9),linetype = "dashed",colour = lighten(mycolours[2], amount = 0.2)) +
  geom_vline(aes(xintercept = 0.83),linetype = "dashed",colour = lighten(mycolours[4], amount = 0.2)) +
  geom_vline(aes(xintercept = 0.83),linetype = "dashed",colour = lighten(mycolours[6], amount = 0.2)) +
  geom_vline(aes(xintercept = 0.88),linetype = "dashed",colour = lighten(mycolours[8], amount = 0.2)) +
  geom_vline(aes(xintercept = 0.88),linetype = "dashed",colour = lighten(mycolours[10], amount = 0.2)) +
  geom_hline(aes(yintercept = 0.9),linetype = "dashed",colour = lighten(mycolours[2], amount = 0.2)) +
  geom_hline(aes(yintercept = 0.79),linetype = "dashed",colour = lighten(mycolours[4], amount = 0.2)) +
  geom_hline(aes(yintercept = 0.9),linetype = "dashed",colour = lighten(mycolours[6], amount = 0.2)) +
  geom_hline(aes(yintercept = 0.72),linetype = "dashed",colour = lighten(mycolours[8], amount = 0.2)) +
  geom_hline(aes(yintercept = 0.9),linetype = "dashed",colour = lighten(mycolours[10], amount = 0.2)) +
  geom_point(shape=selprop_pest$myshape, colour=selprop_pest$mycolour_point, size = 2.5) +
  geom_text_repel(color=selprop_pest$mycolour_lab,
                  nudge_y = 0.02,
                  nudge_x = 0.02,
                  size=3, segment.color="grey",
                  segment.size=0.2) + 
  xlab("Selection Proportion (LASSO)")+
  ylab("Selection Proportion (sPLS)") +
  labs(title = "POPs and non-persistent pesticides") +
  xlim(xlim) +
  ylim(ylim) +
  theme_bw() +
  theme(legend.position = "none")

pdf("../figures/lasso_spls/selprop_pest.pdf", width=7, height=7)
p3
dev.off()


p3_1=ggplot(selprop_pest, aes(sel_lasso, sel_pls,
                            label=ifelse(label_ny == "N","",mylabel_pest))) +
  geom_abline(slope = 1,linetype = "dotted",colour = "grey") +
  geom_vline(aes(xintercept = 0.9),linetype = "dashed",colour = lighten(mycolours[2], amount = 0.2)) +
  geom_vline(aes(xintercept = 0.83),linetype = "dashed",colour = lighten(mycolours[4], amount = 0.2)) +
  geom_vline(aes(xintercept = 0.83),linetype = "dashed",colour = lighten(mycolours[6], amount = 0.2)) +
  geom_vline(aes(xintercept = 0.88),linetype = "dashed",colour = lighten(mycolours[8], amount = 0.2)) +
  geom_vline(aes(xintercept = 0.88),linetype = "dashed",colour = lighten(mycolours[10], amount = 0.2)) +
  geom_hline(aes(yintercept = 0.9),linetype = "dashed",colour = lighten(mycolours[2], amount = 0.2)) +
  geom_hline(aes(yintercept = 0.79),linetype = "dashed",colour = lighten(mycolours[4], amount = 0.2)) +
  geom_hline(aes(yintercept = 0.9),linetype = "dashed",colour = lighten(mycolours[6], amount = 0.2)) +
  geom_hline(aes(yintercept = 0.72),linetype = "dashed",colour = lighten(mycolours[8], amount = 0.2)) +
  geom_hline(aes(yintercept = 0.9),linetype = "dashed",colour = lighten(mycolours[10], amount = 0.2)) +
  geom_point(shape=selprop_pest$myshape, colour=selprop_pest$mycolour_point, size = 2) +
  geom_text_repel(color=selprop_pest$mycolour_lab,
                  nudge_y = 0.02,
                  nudge_x = 0.02,
                  size=3, segment.color="grey",
                  segment.size=0.2) + 
  xlab("Selection Proportion (LASSO)")+
  ylab("Selection Proportion (sPLS)") +
  labs(title = "POPs and non-persistent pesticides") +
  xlim(xlim) +
  ylim(ylim) +
  theme_bw() +
  theme(legend.position = "none")

cairo_pdf("../figures/lasso_spls/ppt_selprop_pest.pdf", width=7, height=7)
p3_1
dev.off()


# PAHs
xlim=c(0, 1)
ylim=c(0, 1)
#coef_lung$var_cat=rep(variable_cat)
selprop_pahs$label=rep(1:(nrow(selprop_pahs)/5), 5)
selprop_pahs$mycolour_point=rep(mycolours[c(2,4,6,8,10)],each = 14)

selprop_pahs$mycolour_lab=darken(selprop_pahs$mycolour_point, amount=0.2)
selprop_pahs$myshape <- rep(c(15, 16, 17, 18, 25),  each = 14)

id1 <- ifelse(selprop_pahs$sel_lasso[1:14]<0.88 & selprop_pahs$sel_pls[1:14]<0.69, "N", "Y") 
id2 <- ifelse(selprop_pahs$sel_lasso[15:28]<0.72 & selprop_pahs$sel_pls[15:28]<0.78, "N", "Y") 
id3 <- ifelse(selprop_pahs$sel_lasso[29:42]<0.74 & selprop_pahs$sel_pls[29:42]<0.74, "N", "Y") 
id4 <- ifelse(selprop_pahs$sel_lasso[43:56]<0.78 & selprop_pahs$sel_pls[43:56]<0.83, "N", "Y") 
id5 <- ifelse(selprop_pahs$sel_lasso[57:70]<0.87 & selprop_pahs$sel_pls[57:70]<0.86, "N", "Y") 

selprop_pahs$label_ny <- c(id1, id2, id3, id4, id5)

p4=ggplot(selprop_pahs, aes(sel_lasso, sel_pls,
                            label=ifelse(label_ny == "N","",label))) +
  geom_abline(slope = 1,linetype = "dotted",colour = "grey") +
  geom_vline(aes(xintercept = 0.75),linetype = "dashed",colour = lighten(mycolours[2], amount = 0.2)) +
  geom_vline(aes(xintercept = 0.72),linetype = "dashed",colour = lighten(mycolours[4], amount = 0.2)) +
  geom_vline(aes(xintercept = 0.74),linetype = "dashed",colour = lighten(mycolours[6], amount = 0.2)) +
  geom_vline(aes(xintercept = 0.78),linetype = "dashed",colour = lighten(mycolours[8], amount = 0.2)) +
  geom_vline(aes(xintercept = 0.87),linetype = "dashed",colour = lighten(mycolours[10], amount = 0.2)) +
  geom_hline(aes(yintercept = 0.69),linetype = "dashed",colour = lighten(mycolours[2], amount = 0.2)) +
  geom_hline(aes(yintercept = 0.78),linetype = "dashed",colour = lighten(mycolours[4], amount = 0.2)) +
  geom_hline(aes(yintercept = 0.74),linetype = "dashed",colour = lighten(mycolours[6], amount = 0.2)) +
  geom_hline(aes(yintercept = 0.83),linetype = "dashed",colour = lighten(mycolours[8], amount = 0.2)) +
  geom_hline(aes(yintercept = 0.86),linetype = "dashed",colour = lighten(mycolours[10], amount = 0.2)) +
  geom_point(shape=selprop_pahs$myshape, colour=selprop_pahs$mycolour_point, size = 2.5) +
  geom_text_repel(color=selprop_pahs$mycolour_lab,
                  nudge_y = 0.02,
                  nudge_x = 0.02,
                  size=3, segment.color="grey",
                  segment.size=0.2) + 
  xlab("Selection Proportion (LASSO)")+
  ylab("Selection Proportion (sPLS)") +
  labs(title = "Metabolites of PAHs and tobacco-related chemicals") +
  xlim(xlim) +
  ylim(ylim) +
  theme_bw() +
  theme(legend.position = "none")

  
pdf("../figures/lasso_spls/selprop_pahs.pdf", width=7, height=7)
p4
dev.off()


p4_1=ggplot(selprop_pahs, aes(sel_lasso, sel_pls,
                            label=ifelse(label_ny == "N","",mylabel_pahs))) +
  geom_abline(slope = 1,linetype = "dotted",colour = "grey") +
  geom_vline(aes(xintercept = 0.75),linetype = "dashed",colour = lighten(mycolours[2], amount = 0.2)) +
  geom_vline(aes(xintercept = 0.72),linetype = "dashed",colour = lighten(mycolours[4], amount = 0.2)) +
  geom_vline(aes(xintercept = 0.74),linetype = "dashed",colour = lighten(mycolours[6], amount = 0.2)) +
  geom_vline(aes(xintercept = 0.78),linetype = "dashed",colour = lighten(mycolours[8], amount = 0.2)) +
  geom_vline(aes(xintercept = 0.87),linetype = "dashed",colour = lighten(mycolours[10], amount = 0.2)) +
  geom_hline(aes(yintercept = 0.69),linetype = "dashed",colour = lighten(mycolours[2], amount = 0.2)) +
  geom_hline(aes(yintercept = 0.78),linetype = "dashed",colour = lighten(mycolours[4], amount = 0.2)) +
  geom_hline(aes(yintercept = 0.74),linetype = "dashed",colour = lighten(mycolours[6], amount = 0.2)) +
  geom_hline(aes(yintercept = 0.83),linetype = "dashed",colour = lighten(mycolours[8], amount = 0.2)) +
  geom_hline(aes(yintercept = 0.86),linetype = "dashed",colour = lighten(mycolours[10], amount = 0.2)) +
  geom_point(shape=selprop_pahs$myshape, colour=selprop_pahs$mycolour_point, size = 2) +
  geom_text_repel(color=selprop_pahs$mycolour_lab,
                  nudge_y = 0.02,
                  nudge_x = 0.02,
                  size=3, segment.color="grey",
                  segment.size=0.2) + 
  xlab("Selection Proportion (LASSO)")+
  ylab("Selection Proportion (sPLS)") +
  labs(title = "Metabolites of PAHs and tobacco-related chemicals") +
  xlim(xlim) +
  ylim(ylim) +
  theme_bw() +
  theme(legend.position = "none")


pdf("../figures/lasso_spls/ppt_selprop_pahs.pdf", width=7, height=7)
p4_1
dev.off()

