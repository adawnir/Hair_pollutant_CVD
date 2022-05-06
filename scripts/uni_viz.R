
### Summer Project --  Univariate analysis Visualisation (Forest plot and Manhattan plot)
## Programme created by Vivian on 02 August added on 01 Sept.


rm(list=ls())
project_path="~/hair/scripts"
setwd(project_path)

##### Loading packages
library(RColorBrewer)
library(tidyverse)
library(plotrix)
library(colorspace)
library(ggrepel)
#install.packages("latex2exp")
#install.packages("greekLetters")
#install.packages("Cairo")
library(latex2exp)
library(greekLetters)
library(Cairo)

##### Settings --
#mycolours <- RColorBrewer::brewer.pal(12, "Set3")
#mycolours  <- darken(mycolours, amount=0.3)
#mycolours  <- mycolours %>% adjust_luminance(-0.5)
mycolours = brewer.pal(10, "Paired")
options(digits=3)

##### Load data --
pest_imp <- readRDS("../processed/imputation/imp_pest.rds")
pest <- readRDS("../processed/uni_multi_pest.rds")
pahs <- readRDS("../processed/imputation/imp_pahs.rds")


manhat_pahs <- as.data.frame(readRDS("../results/manhattan_pahs.rds"))
manhat_pest <- as.data.frame(readRDS("../results/manhattan_pest.rds"))
manhat_pahs_con <- as.data.frame(readRDS("../results/manhattan_pahs_con.rds"))
manhat_pest_con <- as.data.frame(readRDS("../results/manhattan_pest_con.rds"))

forest_pahs <- as.data.frame(readRDS("../results/forest_pahs.rds"))
forest_pest <- as.data.frame(readRDS("../results/forest_pest.rds"))
forest_pahs_con <- as.data.frame(readRDS("../results/forest_pahs_con.rds"))
forest_pest_con <- as.data.frame(readRDS("../results/forest_pest_con.rds"))


##### SCATTER PLOTS --
### PAHs ALL
mylabel_pahs <- c("CountryBelgium","Cotinine", "Nicotine", colnames(pahs)[3:13])


pval_pahs <- unlist(forest_pahs[,which(grepl("logp_",colnames(forest_pahs)))]) 
pval_pahs_tmp <- forest_pahs[,which(grepl("logp_",colnames(forest_pahs)))]
pval_pahs_con <- forest_pahs_con[,which(grepl("logp_",colnames(forest_pahs_con)))]
colnames(pval_pahs_tmp) <- colnames(pval_pahs_con)
pval_pahs_con <- rbind(pval_pahs_tmp[1,], pval_pahs_con)
pval_pahs_con <- unlist(pval_pahs_con) 


or_pahs <- unlist(forest_pahs[,which(grepl("or_",colnames(forest_pahs)))])
or_pahs_tmp <- forest_pahs[,which(grepl("or_",colnames(forest_pahs)))]
or_pahs_con <- forest_pahs_con[,which(grepl("or_",colnames(forest_pahs_con)))]
colnames(or_pahs_tmp) <- colnames(or_pahs_con)
or_pahs_con <- rbind(or_pahs_tmp[1,], or_pahs_con)
or_pahs_con <- unlist(or_pahs_con)


l95_pahs <- unlist(forest_pahs[,which(grepl("l95_",colnames(forest_pahs)))])
l95_pahs_tmp <- forest_pahs[,which(grepl("l95_",colnames(forest_pahs)))]
l95_pahs_con <- forest_pahs_con[,which(grepl("l95_",colnames(forest_pahs_con)))]
colnames(l95_pahs_tmp) <- colnames(l95_pahs_con)
l95_pahs_con <- rbind(l95_pahs_tmp[1,], l95_pahs_con)
l95_pahs_con <- unlist(l95_pahs_con)

u95_pahs <- unlist(forest_pahs[,which(grepl("u95_",colnames(forest_pahs)))])
u95_pahs_tmp <- forest_pahs[,which(grepl("u95_",colnames(forest_pahs)))]
u95_pahs_con <- forest_pahs_con[,which(grepl("u95_",colnames(forest_pahs_con)))]
colnames(u95_pahs_tmp) <- colnames(u95_pahs_con)
u95_pahs_con <- rbind(u95_pahs_tmp[1,], u95_pahs_con)
u95_pahs_con <- unlist(u95_pahs_con)

pval_pahs_full <- cbind(pval_pahs, pval_pahs_con) %>% data.frame()
rownames(pval_pahs_full) <- paste0(rep(c("ob_", "ld_", "ms_", "hyper_", "dia_"), each = 14),
                                  mylabel_pahs)

pval_pahs_full$mycolour <- rep(mycolours[c(2,4,10,6,8)], each = 14)

or_pahs_full <- cbind(or_pahs, or_pahs_con, l95_pahs, l95_pahs_con, u95_pahs, u95_pahs_con) %>% data.frame()
rownames(or_pahs_full) <- paste0(rep(c("ob_", "ld_", "ms_", "hyper_", "dia_"), each = 14),
                                   mylabel_pahs)

or_pahs_full$label <- rep(c("CountryBelgium","Cotinine", "Nicotine", colnames(pahs)[3:13]), times = 5)
or_pahs_full$label <- rep(c(1:14), times = 5)
or_pahs_full$mycolour <- rep(mycolours[c(2,4,10,6,8)], each = 14)
or_pahs_full$myshape <- rep(c(15, 16, 25, 17, 18),  each = 14)
myshape <- rep(c(15, 16, 25, 17, 18),  each = 14)


xlim=c(0.2,3)
ylim=c(0.2,3)


p1 <- ggplot(or_pahs_full, aes(x=or_pahs, y=or_pahs_con, group = myshape,
                               label=ifelse((pval_pahs_full$pval_pahs>= -log10(0.05) | pval_pahs_full$pval_pahs_con>=-log10(0.05)),
                                             label,""))) + 
  geom_abline(linetype = 'dotted', colour = 'grey') +
  geom_errorbar(aes(ymin=l95_pahs_con, ymax=u95_pahs_con),colour=ifelse((pval_pahs_full$pval_pahs>= -log10(0.05) | pval_pahs_full$pval_pahs_con>=-log10(0.05)),or_pahs_full$mycolour,alpha("grey90",0.1))) +
  geom_errorbarh(aes(xmin=l95_pahs, xmax=u95_pahs),colour=ifelse((pval_pahs_full$pval_pahs>= -log10(0.05) | pval_pahs_full$pval_pahs_con>=-log10(0.05)),or_pahs_full$mycolour,alpha("grey90",0.1))) +
  geom_hline(yintercept = 1, linetype = 'dashed', colour = 'black') +
  geom_vline(xintercept = 1, linetype = 'dashed', colour = 'black') +
  geom_label_repel(size=4, segment.colour = "grey", colour = or_pahs_full$mycolour,
                   segment.size = 0.5, max.overlaps = Inf,
                   nudge_y = 0.05, nudge_x = 0.05,
                   box.padding = .5,
                   label.size = NA, label.padding=.1, na.rm=TRUE, fill = alpha(c("white"),0.7)) +
  geom_point(shape=myshape, size=2,
             colour=ifelse((pval_pahs_full$pval_pahs>= -log10(0.05) | pval_pahs_full$pval_pahs_con>=-log10(0.05)),
                           or_pahs_full$mycolour, alpha("grey90",0.1))) +
  ylab('OR [95% CI] (Model further adjusted for country)') +
  xlab('OR [95% CI] (Model adjusted for age and sex)') +
  labs(title = "Metabolites of PAHs and tobacco-related chemicals") +
  theme_bw() +
  xlim(xlim) +
  ylim(ylim) +
  theme(legend.position = "none")

pdf("../figures/Univ/scatter_or_pahs.pdf", width = 7, height = 7)
p1
dev.off()


p1_1 <- ggplot(or_pahs_full, aes(x=or_pahs, y=or_pahs_con, group = myshape,
                               label=ifelse((pval_pahs_full$pval_pahs>= -log10(0.05) | pval_pahs_full$pval_pahs_con>=-log10(0.05)),
                                            mylabel_pahs,""))) + 
  geom_abline(linetype = 'dotted', colour = 'grey') +
  geom_errorbar(aes(ymin=l95_pahs_con, ymax=u95_pahs_con),colour=ifelse((pval_pahs_full$pval_pahs>= -log10(0.05) | pval_pahs_full$pval_pahs_con>=-log10(0.05)),or_pahs_full$mycolour,alpha("grey90",0.1))) +
  geom_errorbarh(aes(xmin=l95_pahs, xmax=u95_pahs),colour=ifelse((pval_pahs_full$pval_pahs>= -log10(0.05) | pval_pahs_full$pval_pahs_con>=-log10(0.05)),or_pahs_full$mycolour,alpha("grey90",0.1))) +
  geom_hline(yintercept = 1, linetype = 'dashed', colour = 'black') +
  geom_vline(xintercept = 1, linetype = 'dashed', colour = 'black') +
  geom_label_repel(size=3, segment.colour = "grey", colour = or_pahs_full$mycolour,
                   segment.size = 0.5, max.overlaps = Inf,
                   nudge_y = 0.05, nudge_x = 0.05,
                   box.padding = .5,
                   label.size = NA, label.padding=.1, na.rm=TRUE, fill = alpha(c("white"),0.7)) +
  geom_point(shape=myshape, size=2,
             colour=ifelse((pval_pahs_full$pval_pahs>= -log10(0.05) | pval_pahs_full$pval_pahs_con>=-log10(0.05)),
                           or_pahs_full$mycolour, alpha("grey90",0.1))) +
  ylab('OR [95% CI] (Model further adjusted for country)') +
  xlab('OR [95% CI] (Model adjusted for age and sex)') +
  labs(title = "Metabolites of PAHs and tobacco-related chemicals") +
  theme_bw() +
  xlim(xlim) +
  ylim(ylim) +
  theme(legend.position = "none")

pdf("../figures/Univ/ppt_scatter_or_pahs.pdf", width = 7, height = 7)
p1_1
dev.off()


xlim=c(0,max(pval_pahs_full[,1],na.rm=T))
ylim=c(0,max(pval_pahs_full[,2],na.rm=T))

p2 = ggplot(pval_pahs_full, aes(pval_pahs, pval_pahs_con,
                                label = ifelse((pval_pahs>= -log10(0.05) | pval_pahs_con>= -log10(0.05)),
                                               or_pahs_full$label,""))) +
  geom_abline(slope = 1, linetype="dotted",colour = "grey") +
  geom_vline(aes(xintercept = -log10(0.05/14)),linetype = "dashed",colour = "darkred") +
  geom_hline(aes(yintercept = -log10(0.05/14)),linetype = "dashed",colour = "darkred") +
  geom_vline(aes(xintercept = -log10(0.05)),linetype = "dashed",colour = "grey50") +
  geom_hline(aes(yintercept = -log10(0.05)),linetype = "dashed",colour = "grey50") +
  geom_point(shape=myshape, size=3,
             color = ifelse((pval_pahs>= -log10(0.05) | pval_pahs_con>= -log10(0.05)),
                            pval_pahs_full$mycolour, "grey90")) +
  geom_label_repel(size=4, segment.colour = "grey", colour = pval_pahs_full$mycolour,
                   segment.size = 0.5, max.overlaps = Inf,
                   nudge_y = 0.05, nudge_x = 0.05,
                   box.padding = .5, point.padding = 0.1,
                   label.size = NA, label.padding=.1, na.rm=TRUE, fill = alpha(c("white"),0.99)) +
  xlab(expression(-log[10](italic(p))~"(Model adjusted for age and sex)"))+
  ylab(expression(-log[10](italic(p))~"(Model further adjusted for country)")) +
  labs(title = "Metabolites of PAHs and tobacco-related chemicals") +
  xlim(xlim) +
  ylim(ylim) +
  theme_bw()

pdf("../figures/Univ/scatter_p_pahs.pdf", width = 7, height = 7)
p2
dev.off()

p2_1 = ggplot(pval_pahs_full, aes(pval_pahs, pval_pahs_con,
                                label = ifelse((pval_pahs>= -log10(0.05) | pval_pahs_con>= -log10(0.05)),
                                               mylabel_pahs,""))) +
  geom_abline(slope = 1, linetype="dotted",colour = "grey") +
  geom_vline(aes(xintercept = -log10(0.05/14)),linetype = "dashed",colour = "darkred") +
  geom_hline(aes(yintercept = -log10(0.05/14)),linetype = "dashed",colour = "darkred") +
  geom_vline(aes(xintercept = -log10(0.05)),linetype = "dashed",colour = "grey50") +
  geom_hline(aes(yintercept = -log10(0.05)),linetype = "dashed",colour = "grey50") +
  geom_point(shape=myshape, size=2,
             color = ifelse((pval_pahs>= -log10(0.05) | pval_pahs_con>= -log10(0.05)),
                            pval_pahs_full$mycolour, "grey90")) +
  geom_label_repel(size=3, segment.colour = "grey", colour = pval_pahs_full$mycolour,
                   segment.size = 0.5, max.overlaps = Inf,
                   nudge_y = 0.05, nudge_x = 0.05,
                   box.padding = .5, point.padding = 0.1,
                   label.size = NA, label.padding=.1, na.rm=TRUE, fill = alpha(c("white"),0.99)) +
  xlab(expression(-log[10](italic(p))~"(Model adjusted for age and sex)"))+
  ylab(expression(-log[10](italic(p))~"(Model further adjusted for country)")) +
  labs(title = "Metabolites of PAHs and tobacco-related chemicals") +
  xlim(xlim) +
  ylim(ylim) +
  theme_bw()

pdf("../figures/Univ/ppt_scatter_p_pahs.pdf", width = 7, height = 7)
p2_1
dev.off()


### Pest ALL
mylabel_pest <- c("CountryBelgium","CountryFrance", colnames(pest_imp)[1:4],
                  "p,p-DDT", "p,p-DDE",
                  colnames(pest_imp)[7:33])

pval_pest <- unlist(forest_pest[,which(grepl("logp_",colnames(forest_pest)))]) 
pval_pest_tmp <- forest_pest[,which(grepl("logp_",colnames(forest_pest)))]
pval_pest_con <- forest_pest_con[,which(grepl("logp_",colnames(forest_pest_con)))]
colnames(pval_pest_tmp) <- colnames(pval_pest_con)
pval_pest_con <- rbind(pval_pest_tmp[1:2,], pval_pest_con)
pval_pest_con <- unlist(pval_pest_con) 

or_pest <- unlist(forest_pest[,which(grepl("or_",colnames(forest_pest)))])
or_pest_tmp <- forest_pest[,which(grepl("or_",colnames(forest_pest)))]
or_pest_con <- forest_pest_con[,which(grepl("or_",colnames(forest_pest_con)))]
colnames(or_pest_tmp) <- colnames(or_pest_con)
or_pest_con <- rbind(or_pest_tmp[1:2,], or_pest_con)
or_pest_con <- unlist(or_pest_con)


l95_pest <- unlist(forest_pest[,which(grepl("l95_",colnames(forest_pest)))])
l95_pest_tmp <- forest_pest[,which(grepl("l95_",colnames(forest_pest)))]
l95_pest_con <- forest_pest_con[,which(grepl("l95_",colnames(forest_pest_con)))]
colnames(l95_pest_tmp) <- colnames(l95_pest_con)
l95_pest_con <- rbind(l95_pest_tmp[1:2,], l95_pest_con)
l95_pest_con <- unlist(l95_pest_con)

u95_pest <- unlist(forest_pest[,which(grepl("u95_",colnames(forest_pest)))])
u95_pest_tmp <- forest_pest[,which(grepl("u95_",colnames(forest_pest)))]
u95_pest_con <- forest_pest_con[,which(grepl("u95_",colnames(forest_pest_con)))]
colnames(u95_pest_tmp) <- colnames(u95_pest_con)
u95_pest_con <- rbind(u95_pest_tmp[1:2,], u95_pest_con)
u95_pest_con <- unlist(u95_pest_con)

pval_pest_full <- cbind(pval_pest, pval_pest_con) %>% data.frame()
rownames(pval_pest_full) <- paste0(rep(c("ob_", "ld_", "ms_", "hyper_", "dia_"), each = 35),
                                   mylabel_pest)

pval_pest_full$mycolour <- rep(mycolours[c(2,4,10,6,8)], each = 35)

or_pest_full <- cbind(or_pest, or_pest_con, l95_pest, l95_pest_con, u95_pest, u95_pest_con) %>% data.frame()
rownames(or_pest_full) <- paste0(rep(c("ob_", "ld_", "ms_", "hyper_", "dia_"), each = 35),
                                 mylabel_pest)

or_pest_full$label <- rep(mylabel_pest, times = 5)
or_pest_full$label <- rep(1:35, times = 5)
or_pest_full$mycolour <- rep(mycolours[c(2,4,10,6,8)], each = 35)

myshape <- rep(c(15, 16, 25, 17, 18),  each = 35)

xlim=c(0.2,2.5)
ylim=c(0.2,3)


p3 <- ggplot(or_pest_full, aes(x=or_pest, y=or_pest_con, group = myshape,
                               label=ifelse((pval_pest_full$pval_pest>= -log10(0.05) | pval_pest_full$pval_pest_con>=-log10(0.05)),
                                             label,""))) + 
  geom_point(shape=myshape, size=3,
             colour=ifelse((pval_pest_full$pval_pest>= -log10(0.05) | pval_pest_full$pval_pest_con>=-log10(0.05)),or_pest_full$mycolour,alpha("grey90",0.1))) +
  geom_abline(linetype = 'dotted', colour = 'grey') +
  geom_errorbar(aes(ymin=l95_pest_con, ymax=u95_pest_con),colour=ifelse((pval_pest_full$pval_pest>= -log10(0.05) | pval_pest_full$pval_pest_con>=-log10(0.05)),or_pest_full$mycolour,alpha("grey90",0.1))) +
  geom_errorbarh(aes(xmin=l95_pest, xmax=u95_pest),colour=ifelse((pval_pest_full$pval_pest>= -log10(0.05) | pval_pest_full$pval_pest_con>=-log10(0.05)),or_pest_full$mycolour,alpha("grey90",0.1))) +
  geom_hline(yintercept = 1, linetype = 'dashed', colour = 'black') +
  geom_vline(xintercept = 1, linetype = 'dashed', colour = 'black') +
  geom_label_repel(size=4, segment.colour = "grey",
                   segment.size = 0.5, max.overlaps = Inf, colour = or_pest_full$mycolour,
                   nudge_y = 0.05, nudge_x = 0.05,
                   box.padding = .5,
                   label.size = NA, label.padding=.1, na.rm=TRUE, fill = alpha(c("white"),0.7)) +
  ylab('OR [95% CI] (Model further adjusted for country)') +
  xlab('OR [95% CI] (Model adjusted for age and sex)') +
  labs(title = "POPs and non-persistent pesticides") +
  theme_bw() +
  xlim(xlim) +
  ylim(ylim) +
  theme(legend.position = "none")


pdf("../figures/Univ/scatter_or_pest.pdf", width = 7, height = 7)
p3
dev.off()

p3_1 <- ggplot(or_pest_full, aes(x=or_pest, y=or_pest_con, group = myshape,
                               label=ifelse((pval_pest_full$pval_pest>= -log10(0.05) | pval_pest_full$pval_pest_con>=-log10(0.05)),
                                            mylabel_pest,""))) + 
  geom_point(shape=myshape, size=2,
             colour=ifelse((pval_pest_full$pval_pest>= -log10(0.05) | pval_pest_full$pval_pest_con>=-log10(0.05)),or_pest_full$mycolour,alpha("grey90",0.1))) +
  geom_abline(linetype = 'dotted', colour = 'grey') +
  geom_errorbar(aes(ymin=l95_pest_con, ymax=u95_pest_con),colour=ifelse((pval_pest_full$pval_pest>= -log10(0.05) | pval_pest_full$pval_pest_con>=-log10(0.05)),or_pest_full$mycolour,alpha("grey90",0.1))) +
  geom_errorbarh(aes(xmin=l95_pest, xmax=u95_pest),colour=ifelse((pval_pest_full$pval_pest>= -log10(0.05) | pval_pest_full$pval_pest_con>=-log10(0.05)),or_pest_full$mycolour,alpha("grey90",0.1))) +
  geom_hline(yintercept = 1, linetype = 'dashed', colour = 'black') +
  geom_vline(xintercept = 1, linetype = 'dashed', colour = 'black') +
  geom_label_repel(size=3, segment.colour = "grey",
                   segment.size = 0.5, max.overlaps = Inf, colour = or_pest_full$mycolour,
                   nudge_y = 0.05, nudge_x = 0.05,
                   box.padding = .5,
                   label.size = NA, label.padding=.1, na.rm=TRUE, fill = alpha(c("white"),0.7)) +
  ylab('OR [95% CI] (Model further adjusted for country)') +
  xlab('OR [95% CI] (Model adjusted for age and sex)') +
  labs(title = "POPs and non-persistent pesticides") +
  theme_bw() +
  xlim(xlim) +
  ylim(ylim) +
  theme(legend.position = "none")


cairo_pdf("../figures/Univ/ppt_scatter_or_pest.pdf", width = 7, height = 7)
p3_1
dev.off()



xlim=c(0,max(pval_pest_full[,1],na.rm=T))
ylim=c(0,max(pval_pest_full[,2],na.rm=T))

p4 = ggplot(pval_pest_full, aes(pval_pest, pval_pest_con,
                                label = ifelse((pval_pest>= -log10(0.05) | pval_pest_con>= -log10(0.05)),
                                                or_pest_full$label,""))) +
  geom_abline(slope = 1, linetype="dotted",colour = "grey") +
  geom_vline(aes(xintercept = -log10(0.05/14)),linetype = "dashed",colour = "darkred") +
  geom_hline(aes(yintercept = -log10(0.05/14)),linetype = "dashed",colour = "darkred") +
  geom_vline(aes(xintercept = -log10(0.05)),linetype = "dashed",colour = "grey50") +
  geom_hline(aes(yintercept = -log10(0.05)),linetype = "dashed",colour = "grey50") +
  geom_point(shape=myshape, size=3,
             color = ifelse((pval_pest>= -log10(0.05) | pval_pest_con>= -log10(0.05)),
                            pval_pest_full$mycolour, "grey90")) +
  geom_label_repel(size=4, segment.colour = "grey", colour = pval_pest_full$mycolour,
                   segment.size = 0.5, max.overlaps = Inf, 
                   nudge_y = 0.05, nudge_x = 0.05,
                   box.padding = .5, point.padding = 0.1,
                   label.size = NA, label.padding=.1, na.rm=TRUE, fill = alpha(c("white"),0.7)) +
  xlab(expression(-log[10](italic(p))~"(Model adjusted for age and sex)"))+
  ylab(expression(-log[10](italic(p))~"(Model further adjusted for country)")) +
  labs(title = "POPs and non-persistent pesticides") +
  xlim(xlim) +
  ylim(ylim) +
  theme_bw()

pdf("../figures/Univ/scatter_p_pest.pdf", width = 7, height = 7)
p4
dev.off()

p4_1 = ggplot(pval_pest_full, aes(pval_pest, pval_pest_con,
                                label = ifelse((pval_pest>= -log10(0.05) | pval_pest_con>= -log10(0.05)),
                                               mylabel_pest,""))) +
  geom_abline(slope = 1, linetype="dotted",colour = "grey") +
  geom_vline(aes(xintercept = -log10(0.05/14)),linetype = "dashed",colour = "darkred") +
  geom_hline(aes(yintercept = -log10(0.05/14)),linetype = "dashed",colour = "darkred") +
  geom_vline(aes(xintercept = -log10(0.05)),linetype = "dashed",colour = "grey50") +
  geom_hline(aes(yintercept = -log10(0.05)),linetype = "dashed",colour = "grey50") +
  geom_point(shape=myshape, size=2,
             color = ifelse((pval_pest>= -log10(0.05) | pval_pest_con>= -log10(0.05)),
                            pval_pest_full$mycolour, "grey90")) +
  geom_label_repel(size=3, segment.colour = "grey", colour = pval_pest_full$mycolour,
                   segment.size = 0.5, max.overlaps = Inf, 
                   nudge_y = 0.05, nudge_x = 0.05,
                   box.padding = .5, point.padding = 0.1,
                   label.size = NA, label.padding=.1, na.rm=TRUE, fill = alpha(c("white"),0.7)) +
  xlab(expression(-log[10](italic(p))~"(Model adjusted for age and sex)"))+
  ylab(expression(-log[10](italic(p))~"(Model further adjusted for country)")) +
  labs(title = "POPs and non-persistent pesticides") +
  xlim(xlim) +
  ylim(ylim) +
  theme_bw()

cairo_pdf("../figures/Univ/ppt_scatter_p_pest.pdf", width = 7, height = 7)
p4_1
dev.off()



##### Make legends
foo=function(x,n){
  x=paste0(n," - ",x)
  if (grepl("m\\^", x)){
    x=gsub("m\\^","'~m^", x)
    x=sub(")","~')", x)
  }
  if (grepl("\\[", x)){
    x=gsub("\\[","'[", x)
    x=sub("\\(ug","~'(ug", x)
  }
  eval(parse(text=paste0("expression(","'", x,"'",")")))
}


mylabel_pest <- c("CountryBelgium","CountryFrance", colnames(pest_imp)[1:4],
                  "p,p-DDT", "p,p-DDE", 
                  colnames(pest_imp)[7:33])


pdf("../figures/Univ/legend.pdf", height = 5, width = 1.5)
par(mar=c(0,0,0,0), mfrow=c(1,1), xpd=TRUE)
plot(NULL ,xaxt='n',yaxt='n',bty='n',ylab='',xlab='', xlim=0:1, ylim=0:1)
legend("top", legend =c("Outcomes", 'Obesity', 'Lipid disorder', 'Hypertension',
                        'Diabetes', 'Metabolic syndrome', "",
                        "Variables", mapply(foo, mylabel_pest[1:35], 1:35)), 
       pch=c(NA ,15, 16, 25, 17, 18, rep(NA, 36)), 
       text.col = c("black", (mycolours[c(2,4,6,8,10)]), NA, rep("black", 35)),
       col = c("black", (mycolours[c(2,4,6,8,10)]), NA, rep("black", 35)),
       cex=0.55, bty="n")
dev.off()

cairo_pdf("../figures/Univ/legend_pest1.pdf", height = 3, width = 3)
par(mar=c(0,0,0,0), mfrow=c(1,1), xpd=TRUE)
plot(NULL ,xaxt='n',yaxt='n',bty='n',ylab='',xlab='', xlim=0:1, ylim=0:1)
legend("top", 
       legend=c("Variables (A, B)",mapply(foo, mylabel_pest[1:35], 1:35)),
       bty="n", cex=0.7, ncol = 2)
dev.off()

cairo_pdf("../figures/Univ/legend_pest1.pdf", height = 3, width = 3)
par(mar=c(0,0,0,0), mfrow=c(1,1), xpd=TRUE)
plot(NULL ,xaxt='n',yaxt='n',bty='n',ylab='',xlab='', xlim=0:1, ylim=0:1)
legend("top", 
       legend=c(mapply(foo, mylabel_pest[1:35], 1:35)),
       bty="n", cex=0.7)
dev.off()


pdf("../figures/Univ/legend_outcomes.1.pdf", height = 2, width = 2)
par(mar=c(0,0,0,0), mfrow=c(1,1), xpd=TRUE)
plot(NULL ,xaxt='n',yaxt='n',bty='n',ylab='',xlab='', xlim=0:1, ylim=0:1)
legend("topleft", legend =c("Outcomes", 'Obesity', 'Lipid disorder', 'Hypertension',
                            'Diabetes', 'Metabolic syndrome'), 
       pch=c(NA ,15, 16, 25, 17, 18),cex=0.7, bty='n',
       text.col = c("black", (mycolours[c(2,4,6,8,10)])),
       col = c("black", (mycolours[c(2,4,6,8,10)])))
mtext("Outcomes", at=0.2, cex=0.7)
dev.off()

cairo_pdf("../figures/Univ/legend_pahs.pdf", height = 3, width = 3)
par(mar=c(0,0,0,0), mfrow=c(1,1), xpd=TRUE)
plot(NULL ,xaxt='n',yaxt='n',bty='n',ylab='',xlab='', xlim=0:1, ylim=0:1)
legend("top", 
       legend=c("Variables (C, D)", mapply(foo, mylabel_pahs, 1:14)),
       bty="n", cex=0.7)
dev.off()
