
### Sumemr Project --  Univariate analysis Visualisation (Forest plot and Manhattan plot)
## Programme created by Vivian on 29 June

rm(list=ls())
project_path="~/hair/scripts"
setwd(project_path)

##### Loading packages
library(RColorBrewer)
library(tidyverse)
library(plotrix)
library(colorspace)
library(ggrepel)



##### Load data --
pest_imp <- readRDS("../processed/imputation/imp_pest.rds")
pest <- readRDS("../processed/uni_multi_pest.rds")
pahs <- readRDS("../processed/imputation/imp_pahs.rds")
over <- readRDS("../processed/imputation/imp_overlap.rds")

manhat_pahs <- as.data.frame(readRDS("../results/manhattan_pahs.rds"))
manhat_pest <- as.data.frame(readRDS("../results/manhattan_pest.rds"))
manhat_over <- as.data.frame(readRDS("../results/manhattan_over.rds"))
manhat_pahs_con <- as.data.frame(readRDS("../results/manhattan_pahs_con.rds"))
manhat_pest_con <- as.data.frame(readRDS("../results/manhattan_pest_con.rds"))

forest_pahs <- as.data.frame(readRDS("../results/forest_pahs.rds"))
forest_pest <- as.data.frame(readRDS("../results/forest_pest.rds"))
forest_over <- as.data.frame(readRDS("../results/forest_over.rds"))
forest_pahs_con <- as.data.frame(readRDS("../results/forest_pahs_con.rds"))
forest_pest_con <- as.data.frame(readRDS("../results/forest_pest_con.rds"))


##### Change data structure --
myorder <- c("obesity", "lipid_disorder", "hypertension", "diabetes","MS")
manhat_pest <- manhat_pest[, myorder]
manhat_pahs <- manhat_pahs[, myorder]
manhat_over <- manhat_over[, myorder]
manhat_pest_con <- manhat_pest_con[, myorder]
manhat_pahs_con <- manhat_pahs_con[, myorder]


manhat_pest_tmp <- manhat_pest %>%
  add_column(Empty1 = NA, .after="obesity") %>% 
  add_column(Empty2 = NA, .after="lipid_disorder") %>%
  add_column(Empty3 = NA, .after="hypertension") %>%
  add_column(Empty4 = NA, .after="diabetes") %>%
  add_column(Empty5 = NA, .after="MS")  %>%
  add_column(Empty0 = NA, .before = "obesity")

manhat_pest_tmp1 <- data.frame(t(manhat_pest_tmp))
manhat_pest1 <- unlist(manhat_pest_tmp1)

manhat_pahs_tmp <- manhat_pahs %>%
  add_column(Empty1 = NA, .after="obesity") %>% 
  add_column(Empty2 = NA, .after="lipid_disorder") %>%
  add_column(Empty3 = NA, .after="hypertension") %>%
  add_column(Empty4 = NA, .after="diabetes") %>%
  add_column(Empty5 = NA, .after="MS")  %>%
  add_column(Empty0 = NA, .before = "obesity")

manhat_pahs_tmp1 <- data.frame(t(manhat_pahs_tmp))
manhat_pahs1 <- unlist(manhat_pahs_tmp1)


manhat_over_tmp <- manhat_over %>%
  add_column(Empty1 = NA, .after="obesity") %>% 
  add_column(Empty2 = NA, .after="lipid_disorder") %>%
  add_column(Empty3 = NA, .after="hypertension") %>%
  add_column(Empty4 = NA, .after="diabetes") %>%
  add_column(Empty5 = NA, .after="MS")  %>%
  add_column(Empty0 = NA, .before = "obesity")

manhat_over_tmp1 <- data.frame(t(manhat_over_tmp))
manhat_over1 <- unlist(manhat_over_tmp1)


manhat_pest_con_tmp <- manhat_pest_con %>%
  add_column(Empty1 = NA, .after="obesity") %>% 
  add_column(Empty2 = NA, .after="lipid_disorder") %>%
  add_column(Empty3 = NA, .after="hypertension") %>%
  add_column(Empty4 = NA, .after="diabetes") %>%
  add_column(Empty5 = NA, .after="MS")  %>%
  add_column(Empty0 = NA, .before = "obesity")

manhat_pest_con_tmp1 <- data.frame(t(manhat_pest_con_tmp))
manhat_pest_con1 <- unlist(manhat_pest_con_tmp1)

manhat_pahs_con_tmp <- manhat_pahs_con %>%
  add_column(Empty1 = NA, .after="obesity") %>% 
  add_column(Empty2 = NA, .after="lipid_disorder") %>%
  add_column(Empty3 = NA, .after="hypertension") %>%
  add_column(Empty4 = NA, .after="diabetes") %>%
  add_column(Empty5 = NA, .after="MS")  %>%
  add_column(Empty0 = NA, .before = "obesity")

manhat_pahs_con_tmp1 <- data.frame(t(manhat_pahs_con_tmp))
manhat_pahs_con1 <- unlist(manhat_pahs_con_tmp1)


### OR
# PAHs
or_pahs_tmp <- forest_pahs %>% 
  dplyr::select(c("or_pahs_ob", "or_pahs_ld", "or_pahs_hyper", "or_pahs_dia", "or_pahs_ms"))
or_pahs_tmp <- or_pahs_tmp %>%
  add_column(Empty1 = NA, .after="or_pahs_ob") %>% 
  add_column(Empty2 = NA, .after="or_pahs_ld") %>%
  add_column(Empty3 = NA, .after="or_pahs_hyper") %>%
  add_column(Empty4 = NA, .after="or_pahs_dia") %>%
  add_column(Empty5 = NA, .after="or_pahs_ms")  %>%
  add_column(Empty0 = NA, .before = "or_pahs_ob")  
or_pahs_tmp1 <- data.frame(t(or_pahs_tmp))
or_pahs <- unlist(or_pahs_tmp1)

up_pahs_tmp <- forest_pahs %>% 
  dplyr::select(c("u95_pahs_ob", "u95_pahs_ld", "u95_pahs_hyper", "u95_pahs_dia", "u95_pahs_ms"))
up_pahs_tmp <- up_pahs_tmp %>%
  add_column(Empty1 = NA, .after="u95_pahs_ob") %>% 
  add_column(Empty2 = NA, .after="u95_pahs_ld") %>%
  add_column(Empty3 = NA, .after="u95_pahs_hyper") %>%
  add_column(Empty4 = NA, .after="u95_pahs_dia") %>%
  add_column(Empty5 = NA, .after="u95_pahs_ms")  %>%
  add_column(Empty0 = NA, .before = "u95_pahs_ob")  
up_pahs_tmp1 <- data.frame(t(up_pahs_tmp))
up_pahs <- unlist(up_pahs_tmp1)

lo_pahs_tmp <- forest_pahs %>% 
  dplyr::select(c("l95_pahs_ob", "l95_pahs_ld", "l95_pahs_hyper", "l95_pahs_dia", "l95_pahs_ms"))
lo_pahs_tmp <- lo_pahs_tmp %>%
  add_column(Empty1 = NA, .after="l95_pahs_ob") %>% 
  add_column(Empty2 = NA, .after="l95_pahs_ld") %>%
  add_column(Empty3 = NA, .after="l95_pahs_hyper") %>%
  add_column(Empty4 = NA, .after="l95_pahs_dia") %>%
  add_column(Empty5 = NA, .after="l95_pahs_ms")  %>%
  add_column(Empty0 = NA, .before = "l95_pahs_ob")  
lo_pahs_tmp1 <- data.frame(t(lo_pahs_tmp))
lo_pahs <- unlist(lo_pahs_tmp1)

# Pest
or_pest_tmp <- forest_pest %>% 
  dplyr::select(c("or_pest_ob", "or_pest_ld", "or_pest_hyper", "or_pest_dia", "or_pest_ms"))
or_pest_tmp <- or_pest_tmp %>%
  add_column(Empty1 = NA, .after="or_pest_ob") %>% 
  add_column(Empty2 = NA, .after="or_pest_ld") %>%
  add_column(Empty3 = NA, .after="or_pest_hyper") %>%
  add_column(Empty4 = NA, .after="or_pest_dia") %>%
  add_column(Empty5 = NA, .after="or_pest_ms")  %>%
  add_column(Empty0 = NA, .before = "or_pest_ob")  
or_pest_tmp1 <- data.frame(t(or_pest_tmp))
or_pest <- unlist(or_pest_tmp1)

up_pest_tmp <- forest_pest %>% 
  dplyr::select(c("u95_pest_ob", "u95_pest_ld", "u95_pest_hyper", "u95_pest_dia", "u95_pest_ms"))
up_pest_tmp <- up_pest_tmp %>%
  add_column(Empty1 = NA, .after="u95_pest_ob") %>% 
  add_column(Empty2 = NA, .after="u95_pest_ld") %>%
  add_column(Empty3 = NA, .after="u95_pest_hyper") %>%
  add_column(Empty4 = NA, .after="u95_pest_dia") %>%
  add_column(Empty5 = NA, .after="u95_pest_ms")  %>%
  add_column(Empty0 = NA, .before = "u95_pest_ob")  
up_pest_tmp1 <- data.frame(t(up_pest_tmp))
up_pest <- unlist(up_pest_tmp1)

lo_pest_tmp <- forest_pest %>% 
  dplyr::select(c("l95_pest_ob", "l95_pest_ld", "l95_pest_hyper", "l95_pest_dia", "l95_pest_ms"))
lo_pest_tmp <- lo_pest_tmp %>%
  add_column(Empty1 = NA, .after="l95_pest_ob") %>% 
  add_column(Empty2 = NA, .after="l95_pest_ld") %>%
  add_column(Empty3 = NA, .after="l95_pest_hyper") %>%
  add_column(Empty4 = NA, .after="l95_pest_dia") %>%
  add_column(Empty5 = NA, .after="l95_pest_ms")  %>%
  add_column(Empty0 = NA, .before = "l95_pest_ob")  
lo_pest_tmp1 <- data.frame(t(lo_pest_tmp))
lo_pest <- unlist(lo_pest_tmp1)


# Overlapping
or_over_tmp <- forest_over %>% 
  dplyr::select(c("or_over_ob", "or_over_ld", "or_over_hyper", "or_over_dia", "or_over_ms"))
or_over_tmp <- or_over_tmp %>%
  add_column(Empty1 = NA, .after="or_over_ob") %>% 
  add_column(Empty2 = NA, .after="or_over_ld") %>%
  add_column(Empty3 = NA, .after="or_over_hyper") %>%
  add_column(Empty4 = NA, .after="or_over_dia") %>%
  add_column(Empty5 = NA, .after="or_over_ms")  %>%
  add_column(Empty0 = NA, .before = "or_over_ob")  
or_over_tmp1 <- data.frame(t(or_over_tmp))
or_over <- unlist(or_over_tmp1)

up_over_tmp <- forest_over %>% 
  dplyr::select(c("u95_over_ob", "u95_over_ld", "u95_over_hyper", "u95_over_dia", "u95_over_ms"))
up_over_tmp <- up_over_tmp %>%
  add_column(Empty1 = NA, .after="u95_over_ob") %>% 
  add_column(Empty2 = NA, .after="u95_over_ld") %>%
  add_column(Empty3 = NA, .after="u95_over_hyper") %>%
  add_column(Empty4 = NA, .after="u95_over_dia") %>%
  add_column(Empty5 = NA, .after="u95_over_ms")  %>%
  add_column(Empty0 = NA, .before = "u95_over_ob")  
up_over_tmp1 <- data.frame(t(up_over_tmp))
up_over <- unlist(up_over_tmp1)

lo_over_tmp <- forest_over %>% 
  dplyr::select(c("l95_over_ob", "l95_over_ld", "l95_over_hyper", "l95_over_dia", "l95_over_ms"))
lo_over_tmp <- lo_over_tmp %>%
  add_column(Empty1 = NA, .after="l95_over_ob") %>% 
  add_column(Empty2 = NA, .after="l95_over_ld") %>%
  add_column(Empty3 = NA, .after="l95_over_hyper") %>%
  add_column(Empty4 = NA, .after="l95_over_dia") %>%
  add_column(Empty5 = NA, .after="l95_over_ms")  %>%
  add_column(Empty0 = NA, .before = "l95_over_ob")  
lo_over_tmp1 <- data.frame(t(lo_over_tmp))
lo_over <- unlist(lo_over_tmp1)


# PAHs adjusted for country as well
or_pahs_con_tmp <- forest_pahs_con %>% 
  dplyr::select(c("or_pahs_ob_con", "or_pahs_ld_con", "or_pahs_hyper_con", "or_pahs_dia_con", "or_pahs_ms_con"))
or_pahs_con_tmp <- or_pahs_con_tmp %>%
  add_column(Empty1 = NA, .after="or_pahs_ob_con") %>% 
  add_column(Empty2 = NA, .after="or_pahs_ld_con") %>%
  add_column(Empty3 = NA, .after="or_pahs_hyper_con") %>%
  add_column(Empty4 = NA, .after="or_pahs_dia_con") %>%
  add_column(Empty5 = NA, .after="or_pahs_ms_con")  %>%
  add_column(Empty0 = NA, .before = "or_pahs_ob_con")  
or_pahs_con_tmp1 <- data.frame(t(or_pahs_con_tmp))
or_pahs_con <- unlist(or_pahs_con_tmp1)

up_pahs_con_tmp <- forest_pahs_con %>% 
  dplyr::select(c("u95_pahs_ob_con", "u95_pahs_ld_con", "u95_pahs_hyper_con", "u95_pahs_dia_con", "u95_pahs_ms_con"))
up_pahs_con_tmp <- up_pahs_con_tmp %>%
  add_column(Empty1 = NA, .after="u95_pahs_ob_con") %>% 
  add_column(Empty2 = NA, .after="u95_pahs_ld_con") %>%
  add_column(Empty3 = NA, .after="u95_pahs_hyper_con") %>%
  add_column(Empty4 = NA, .after="u95_pahs_dia_con") %>%
  add_column(Empty5 = NA, .after="u95_pahs_ms_con")  %>%
  add_column(Empty0 = NA, .before = "u95_pahs_ob_con")  
up_pahs_con_tmp1 <- data.frame(t(up_pahs_con_tmp))
up_pahs_con <- unlist(up_pahs_con_tmp1)

lo_pahs_con_tmp <- forest_pahs_con %>% 
  dplyr::select(c("l95_pahs_ob_con", "l95_pahs_ld_con", "l95_pahs_hyper_con", "l95_pahs_dia_con", "l95_pahs_ms_con"))
lo_pahs_con_tmp <- lo_pahs_con_tmp %>%
  add_column(Empty1 = NA, .after="l95_pahs_ob_con") %>% 
  add_column(Empty2 = NA, .after="l95_pahs_ld_con") %>%
  add_column(Empty3 = NA, .after="l95_pahs_hyper_con") %>%
  add_column(Empty4 = NA, .after="l95_pahs_dia_con") %>%
  add_column(Empty5 = NA, .after="l95_pahs_ms_con")  %>%
  add_column(Empty0 = NA, .before = "l95_pahs_ob_con")  
lo_pahs_con_tmp1 <- data.frame(t(lo_pahs_con_tmp))
lo_pahs_con <- unlist(lo_pahs_con_tmp1)


# Pest adjusted for country as well
or_pest_con_tmp <- forest_pest_con %>% 
  dplyr::select(c("or_pest_ob_con", "or_pest_ld_con", "or_pest_hyper_con", "or_pest_dia_con", "or_pest_ms_con"))
or_pest_con_tmp <- or_pest_con_tmp %>%
  add_column(Empty1 = NA, .after="or_pest_ob_con") %>% 
  add_column(Empty2 = NA, .after="or_pest_ld_con") %>%
  add_column(Empty3 = NA, .after="or_pest_hyper_con") %>%
  add_column(Empty4 = NA, .after="or_pest_dia_con") %>%
  add_column(Empty5 = NA, .after="or_pest_ms_con")  %>%
  add_column(Empty0 = NA, .before = "or_pest_ob_con")  
or_pest_con_tmp1 <- data.frame(t(or_pest_con_tmp))
or_pest_con <- unlist(or_pest_con_tmp1)

up_pest_con_tmp <- forest_pest_con %>% 
  dplyr::select(c("u95_pest_ob_con", "u95_pest_ld_con", "u95_pest_hyper_con", "u95_pest_dia_con", "u95_pest_ms_con"))
up_pest_con_tmp <- up_pest_con_tmp %>%
  add_column(Empty1 = NA, .after="u95_pest_ob_con") %>% 
  add_column(Empty2 = NA, .after="u95_pest_ld_con") %>%
  add_column(Empty3 = NA, .after="u95_pest_hyper_con") %>%
  add_column(Empty4 = NA, .after="u95_pest_dia_con") %>%
  add_column(Empty5 = NA, .after="u95_pest_ms_con")  %>%
  add_column(Empty0 = NA, .before = "u95_pest_ob_con")  
up_pest_con_tmp1 <- data.frame(t(up_pest_con_tmp))
up_pest_con <- unlist(up_pest_con_tmp1)

lo_pest_con_tmp <- forest_pest_con %>% 
  dplyr::select(c("l95_pest_ob_con", "l95_pest_ld_con", "l95_pest_hyper_con", "l95_pest_dia_con", "l95_pest_ms_con"))
lo_pest_con_tmp <- lo_pest_con_tmp %>%
  add_column(Empty1 = NA, .after="l95_pest_ob_con") %>% 
  add_column(Empty2 = NA, .after="l95_pest_ld_con") %>%
  add_column(Empty3 = NA, .after="l95_pest_hyper_con") %>%
  add_column(Empty4 = NA, .after="l95_pest_dia_con") %>%
  add_column(Empty5 = NA, .after="l95_pest_ms_con")  %>%
  add_column(Empty0 = NA, .before = "l95_pest_ob_con")  
lo_pest_con_tmp1 <- data.frame(t(lo_pest_con_tmp))
lo_pest_con <- unlist(lo_pest_con_tmp1)


##### MANHATTAN --
### pest
labels_pest <- c("Country", colnames(pest_imp)[1:4], "p,p-DDT", "p,p-DDE",
                 "Alpha-endosulfan", "Beta-endosulfan", "Beta-HCH", "Gamma-HCH",
                 colnames(pest_imp)[11:33])

mycolours = brewer.pal(10, "Paired")
outcomes <- c("Obesity", "Lipid disorder", "Hypertension", "Diabetes","Metabolic syndrome")

n=5 # Number of lines per variable
myspacing=n*2+1
xseq=seq((n+1),34*myspacing, by=myspacing)
background=TRUE
myrange <- c(0,max(manhat_pest1, na.rm = T))

background_colour="darkgrey"

pdf("../figures/Univ/manhat_pest.pdf", height = 7, width = 13)
par(mar = c(10, 2, 2, 5))
plot(manhat_pest1, ylim=myrange, xlim=c(1,length(manhat_pest1)), type="n",
     xaxt="n", yaxt="n",
     col=c(mycolours[2], NA, mycolours[4],NA, mycolours[6], NA, mycolours[8],NA, mycolours[10],NA),
     lwd=1, ylab="",xlab="")
xseqgreysep=c(min(xseq)-myspacing/2,apply(rbind(xseq[-1],xseq[-length(xseq)]),2,mean),max(xseq)+myspacing/2)

if (background){
  for (k in seq(1,length(xseqgreysep),by=2)){
    polygon(x=c(xseqgreysep[k],xseqgreysep[k+1],xseqgreysep[k+1],xseqgreysep[k]),
            y=c(-5,-5,15,15), col=lighten(background_colour,0.90), border=NA)
  }
  for (k in seq(2,length(xseqgreysep),by=2)){
    polygon(x=c(xseqgreysep[k],xseqgreysep[k+1],xseqgreysep[k+1],xseqgreysep[k]),
            y=c(-5,-5,15,15), col=lighten(background_colour,0.95), border=NA)
  }
  box()
}
par(new=TRUE)
plot(manhat_pest1, ylim=myrange,
     xaxt="n", yaxt="n", cex = 0.8, 
     col=c(NA, mycolours[2], NA, mycolours[4],NA, mycolours[6], NA, mycolours[8],NA, mycolours[10],NA),
     pch = c(NA, 15, NA, 16,NA, 17, NA, 18,NA, 25,NA),
     lwd=1, ylab="", xlab="")

axis(side=4, at=axTicks(2))
mtext(side=4, text=expression(-log[10](italic(p))), line=2)
abline(h=-log10(0.05/33), lty = 2, col="darkred")
abline(h=-log10(0.05), lty = 2, col="darkgrey")
abline(v=xseqgreysep,lty=3,col="black",lwd=0.5)
for (k in 1:length(xseq)){
  mytext=labels_pest[k]
  if (grepl("m\\^", mytext)){
    mytext=gsub("m\\^","'~m^", mytext)
    mytext=sub(")","~')", mytext)
  }
  if (grepl("\\[", mytext)){
    mytext=gsub("\\[","'[", mytext)
    mytext=sub("\\(ug","~'(ug", mytext)
  }
  mytmp=eval(parse(text=paste0("expression(","'", mytext,"'",")")))
  axis(side=1, at=xseq[k], labels=mytmp, las=2, cex.axis=0.8)
}
legend("topright", pch=c(15, 16, 17, 18, 25, NA, NA), lty=c(NA, NA, NA, NA, NA, 2, 2), 
       col=c(mycolours[c(2,4,6,8,10)],"darkred","darkgrey"),
       legend=c(outcomes,"Bonferroni threshold","Nominal threshold"),
       cex=0.7, bg = "white", bty = "n")
dev.off()


### PAHs
labels_pahs <- c("Country", colnames(pahs)[1:13])

#mycolours <- c("lightcoral","lightgoldenrod", "palegreen2","steelblue1", "lightslateblue")
outcomes <- c("Obesity", "Lipid disorder", "Hypertension", "Diabetes","Metabolic syndrome")

n=5 # Number of lines per variable
myspacing=n*2+1
xseq=seq((n+1),14*myspacing, by=myspacing)
background=TRUE
myrange <- c(0,max(manhat_pahs1, na.rm = T))

background_colour="darkgrey"

pdf("../figures/Univ/manhat_pahs.pdf", height = 7, width = 13)
par(mar = c(12, 2, 2, 5))
plot(manhat_pahs1, ylim=myrange, xlim=c(1,length(manhat_pahs1)), type="n",
     xaxt="n", yaxt="n",
     col=c(mycolours[2], NA, mycolours[4],NA, mycolours[6], NA, mycolours[8],NA, mycolours[10],NA),
     lwd=1, ylab="",xlab="")
xseqgreysep=c(min(xseq)-myspacing/2,apply(rbind(xseq[-1],xseq[-length(xseq)]),2,mean),max(xseq)+myspacing/2)

if (background){
  for (k in seq(1,length(xseqgreysep),by=2)){
    polygon(x=c(xseqgreysep[k],xseqgreysep[k+1],xseqgreysep[k+1],xseqgreysep[k]),
            y=c(-10,-10,15,15), col=lighten(background_colour,0.90), border=NA)
  }
  for (k in seq(2,length(xseqgreysep),by=2)){
    polygon(x=c(xseqgreysep[k],xseqgreysep[k+1],xseqgreysep[k+1],xseqgreysep[k]),
            y=c(-10,-10,15,15), col=lighten(background_colour,0.95), border=NA)
  }
  box()
}
par(new=TRUE)
plot(manhat_pahs1, ylim=myrange,
     xaxt="n", yaxt="n",
     col=c(NA, mycolours[2], NA, mycolours[4],NA, mycolours[6], NA, mycolours[8],NA, mycolours[10],NA),
     pch = c(NA, 15, NA, 16,NA, 17, NA, 18,NA, 25,NA),
     lwd=1, ylab="", xlab="")

axis(side=4, at=axTicks(2))
mtext(side=4, text=expression(-log[10](italic(p))), line=2)
#xseqblack=c(xseq[!duplicated(variable_cat)]-myspacing/2, max(xseq)+myspacing/2)
#for (k in 1:(length(xseqblack)-1)){
#  axis(side=3, at=xseqblack[c(k,k+1)]+c(2,-2), line=0.5, labels=NA)
#}
#for (k in 1:(length(xseqblack)-1)){
#  axis(side=3, at=mean(xseqblack[c(k,k+1)]), line=0.2, tick=FALSE,
#       labels=unique(variable_cat)[k])
#}
#par(xpd=TRUE)
abline(h=-log10(0.05/5), lty = 2, col="darkred")
abline(h=-log10(0.05), lty = 2, col="gray48")
abline(v=xseqgreysep,lty=3,col="black",lwd=0.5)
for (k in 1:length(xseq)){
  mytext=labels_pahs[k]
  if (grepl("m\\^", mytext)){
    mytext=gsub("m\\^","'~m^", mytext)
    mytext=sub(")","~')", mytext)
  }
  if (grepl("\\[", mytext)){
    mytext=gsub("\\[","'[", mytext)
    mytext=sub("\\(ug","~'(ug", mytext)
  }
  mytmp=eval(parse(text=paste0("expression(","'", mytext,"'",")")))
  #labcol=ifelse((manhat_pahs_1[k,1]>=bonf & manhat[k,2]>=bonf), mycolours[3],
  #ifelse(manhat[k,1]>=bonf,mycolours[1],
  #ifelse(manhat[k,2]>=bonf, mycolours[2],"black")))
  #if(is.na(labcol)){
  #labcol=ifelse(manhat[k,1]>=bonf,mycolours[1],
  #ifelse(manhat[k,2]>=bonf, mycolours[2],"black"))
  #}
  #labfont=ifelse(labcol=="black",1,2)
  axis(side=1, at=xseq[k], labels=mytmp, las=2, cex.axis=0.7)
}
legend("topright", pch=c(15, 16, 17, 18, 25, NA, NA), lty=c(NA, NA, NA, NA, NA, 2, 2), 
       col=c(mycolours[c(2,4,6,8,10)],"darkred","gray48"),
       legend=c(outcomes,"Bonferroni threshold","Nominal threshold"),
       cex=0.7, bg = "white", bty = "n")
dev.off()


### Overlapping
labels_over <- c("Country", "Cotinine","Nicotine",colnames(over)[3:17], 
                 "p,p-DDT", "p,p-DDE", "alpha-endosulfan", "beta-endosulfan", "beta-HCH", "gamma-HCH",
                 colnames(over)[24:46])

#mycolours <- c("lightcoral","lightgoldenrod", "palegreen2","steelblue1", "lightslateblue")
outcomes <- c("Obesity", "Lipid disorder", "Hypertension", "Diabetes","Metabolic syndrome")

n=5 # Number of lines per variable
myspacing=n*2+1
xseq=seq((n+1),47*myspacing, by=myspacing)
background=TRUE
myrange <- c(0,max(manhat_over1, na.rm = T))

background_colour="darkgrey"

pdf("../figures/Univ/manhat_over.pdf", height = 7, width = 13)
par(mar = c(12, 2, 2, 5))
plot(manhat_over1, ylim=myrange, xlim=c(1,length(manhat_over1)), type="n",
     xaxt="n", yaxt="n",
     col=c(mycolours[2], NA, mycolours[4],NA, mycolours[6], NA, mycolours[8],NA, mycolours[10],NA),
     lwd=1, ylab="",xlab="")
xseqgreysep=c(min(xseq)-myspacing/2,apply(rbind(xseq[-1],xseq[-length(xseq)]),2,mean),max(xseq)+myspacing/2)

if (background){
  for (k in seq(1,length(xseqgreysep),by=2)){
    polygon(x=c(xseqgreysep[k],xseqgreysep[k+1],xseqgreysep[k+1],xseqgreysep[k]),
            y=c(-10,-10,15,15), col=lighten(background_colour,0.90), border=NA)
  }
  for (k in seq(2,length(xseqgreysep),by=2)){
    polygon(x=c(xseqgreysep[k],xseqgreysep[k+1],xseqgreysep[k+1],xseqgreysep[k]),
            y=c(-10,-10,15,15), col=lighten(background_colour,0.95), border=NA)
  }
  box()
}
par(new=TRUE)
plot(manhat_over1, ylim=myrange,
     xaxt="n", yaxt="n",
     col=c(NA, mycolours[2], NA, mycolours[4],NA, mycolours[6], NA, mycolours[8],NA, mycolours[10],NA),
     pch = c(NA, 15, NA, 16,NA, 17, NA, 18,NA, 25,NA),
     lwd=1, ylab="", xlab="")

axis(side=4, at=axTicks(2))
mtext(side=4, text=expression(-log[10](italic(p))), line=2)
abline(h=-log10(0.05/46), lty = 2, col="darkred")
abline(h=-log10(0.05), lty = 2, col="gray48")
abline(v=xseqgreysep,lty=3,col="black",lwd=0.5)
for (k in 1:length(xseq)){
  mytext=labels_over[k]
  if (grepl("m\\^", mytext)){
    mytext=gsub("m\\^","'~m^", mytext)
    mytext=sub(")","~')", mytext)
  }
  if (grepl("\\[", mytext)){
    mytext=gsub("\\[","'[", mytext)
    mytext=sub("\\(ug","~'(ug", mytext)
  }
  mytmp=eval(parse(text=paste0("expression(","'", mytext,"'",")")))
  axis(side=1, at=xseq[k], labels=mytmp, las=2, cex.axis=0.7)
}
legend("topleft", pch=c(15, 16, 17, 18, 25, NA, NA), lty=c(NA, NA, NA, NA, NA, 2, 2), 
       col=c(mycolours[c(2,4,6,8,10)],"darkred","gray48"), ncol = 2,
       legend=c(outcomes,"Bonferroni threshold","Nominal threshold"),
       cex=0.7, bg = "white", bty = "n")
dev.off()


### pest adjusted for country
labels_pest <- c(colnames(pest_imp)[1:4], colnames(pest)[21:37], colnames(pest_imp)[22:33])

mycolours = brewer.pal(10, "Paired")
outcomes <- c("Obesity", "Lipid disorder", "Hypertension", "Diabetes","Metabolic syndrome")

n=5 # Number of lines per variable
myspacing=n*2+1
xseq=seq((n+1),33*myspacing, by=myspacing)
background=TRUE
myrange <- c(0,max(manhat_pest_con1, na.rm = T))

background_colour="darkgrey"

pdf("../figures/Univ/manhat_pest_con.pdf", height = 7, width = 13)
par(mar = c(10, 2, 2, 5))
plot(manhat_pest_con1, ylim=myrange, xlim=c(1,length(manhat_pest_con1)), type="n",
     xaxt="n", yaxt="n",
     col=c(mycolours[2], NA, mycolours[4],NA, mycolours[6], NA, mycolours[8],NA, mycolours[10],NA),
     lwd=1, ylab="",xlab="")
xseqgreysep=c(min(xseq)-myspacing/2,apply(rbind(xseq[-1],xseq[-length(xseq)]),2,mean),max(xseq)+myspacing/2)

if (background){
  for (k in seq(1,length(xseqgreysep),by=2)){
    polygon(x=c(xseqgreysep[k],xseqgreysep[k+1],xseqgreysep[k+1],xseqgreysep[k]),
            y=c(-5,-5,15,15), col=lighten(background_colour,0.90), border=NA)
  }
  for (k in seq(2,length(xseqgreysep),by=2)){
    polygon(x=c(xseqgreysep[k],xseqgreysep[k+1],xseqgreysep[k+1],xseqgreysep[k]),
            y=c(-5,-5,15,15), col=lighten(background_colour,0.95), border=NA)
  }
  box()
}
par(new=TRUE)
plot(manhat_pest_con1, ylim=myrange,
     xaxt="n", yaxt="n", cex = 0.8, 
     col=c(NA, mycolours[2], NA, mycolours[4],NA, mycolours[6], NA, mycolours[8],NA, mycolours[10],NA),
     pch = c(NA, 15, NA, 16,NA, 17, NA, 18,NA, 25,NA),
     lwd=1, ylab="", xlab="")

axis(side=4, at=axTicks(2))
mtext(side=4, text=expression(-log[10](italic(p))), line=2)
abline(h=-log10(0.05/33), lty = 2, col="darkred")
abline(h=-log10(0.05), lty = 2, col="darkgrey")
abline(v=xseqgreysep,lty=3,col="black",lwd=0.5)
for (k in 1:length(xseq)){
  mytext=labels_pest[k]
  if (grepl("m\\^", mytext)){
    mytext=gsub("m\\^","'~m^", mytext)
    mytext=sub(")","~')", mytext)
  }
  if (grepl("\\[", mytext)){
    mytext=gsub("\\[","'[", mytext)
    mytext=sub("\\(ug","~'(ug", mytext)
  }
  mytmp=eval(parse(text=paste0("expression(","'", mytext,"'",")")))
  axis(side=1, at=xseq[k], labels=mytmp, las=2, cex.axis=0.8)
}
legend("topright", pch=c(15, 16, 17, 18, 25, NA, NA), lty=c(NA, NA, NA, NA, NA, 2, 2), 
       col=c(mycolours[c(2,4,6,8,10)],"darkred","darkgrey"),
       legend=c(outcomes,"Bonferroni threshold","Nominal threshold"),
       cex=0.7, bg = "white", bty = "n")
dev.off()


### PAHs adjusted for country
labels_pahs <- c("Cotinine", "Nicotine", colnames(pahs)[3:13])

outcomes <- c("Obesity", "Lipid disorder", "Hypertension", "Diabetes","Metabolic syndrome")

n=5 # Number of lines per variable
myspacing=n*2+1
xseq=seq((n+1),13*myspacing, by=myspacing)
background=TRUE
myrange <- c(0,max(manhat_pahs_con1, na.rm = T))

background_colour="darkgrey"

pdf("../figures/Univ/manhat_pahs_con.pdf", height = 7, width = 13)
par(mar = c(12, 2, 2, 5))
plot(manhat_pahs_con1, ylim=myrange, xlim=c(1,length(manhat_pahs_con1)), type="n",
     xaxt="n", yaxt="n",
     col=c(mycolours[2], NA, mycolours[4],NA, mycolours[6], NA, mycolours[8],NA, mycolours[10],NA),
     lwd=1, ylab="",xlab="")
xseqgreysep=c(min(xseq)-myspacing/2,apply(rbind(xseq[-1],xseq[-length(xseq)]),2,mean),max(xseq)+myspacing/2)

if (background){
  for (k in seq(1,length(xseqgreysep),by=2)){
    polygon(x=c(xseqgreysep[k],xseqgreysep[k+1],xseqgreysep[k+1],xseqgreysep[k]),
            y=c(-10,-10,15,15), col=lighten(background_colour,0.90), border=NA)
  }
  for (k in seq(2,length(xseqgreysep),by=2)){
    polygon(x=c(xseqgreysep[k],xseqgreysep[k+1],xseqgreysep[k+1],xseqgreysep[k]),
            y=c(-10,-10,15,15), col=lighten(background_colour,0.95), border=NA)
  }
  box()
}
par(new=TRUE)
plot(manhat_pahs_con1, ylim=myrange,
     xaxt="n", yaxt="n",
     col=c(NA, mycolours[2], NA, mycolours[4],NA, mycolours[6], NA, mycolours[8],NA, mycolours[10],NA),
     pch = c(NA, 15, NA, 16,NA, 17, NA, 18,NA, 25,NA),
     lwd=1, ylab="", xlab="")

axis(side=4, at=axTicks(2))
mtext(side=4, text=expression(-log[10](italic(p))), line=2)
abline(h=-log10(0.05/13), lty = 2, col="darkred")
abline(h=-log10(0.05), lty = 2, col="gray48")
abline(v=xseqgreysep,lty=3,col="black",lwd=0.5)
for (k in 1:length(xseq)){
  mytext=labels_pahs[k]
  if (grepl("m\\^", mytext)){
    mytext=gsub("m\\^","'~m^", mytext)
    mytext=sub(")","~')", mytext)
  }
  if (grepl("\\[", mytext)){
    mytext=gsub("\\[","'[", mytext)
    mytext=sub("\\(ug","~'(ug", mytext)
  }
  mytmp=eval(parse(text=paste0("expression(","'", mytext,"'",")")))

  axis(side=1, at=xseq[k], labels=mytmp, las=2, cex.axis=0.7)
}
legend("topright", pch=c(15, 16, 17, 18, 25, NA, NA), lty=c(NA, NA, NA, NA, NA, 2, 2), 
       col=c(mycolours[c(2,4,6,8,10)],"darkred","gray48"),
       legend=c(outcomes,"Bonferroni threshold","Nominal threshold"),
       cex=0.7, bg = "white", bty = "n")
dev.off()




##### OR plot --
### PAHs OR
labels_pahs <- c("CountryBelgium" ,colnames(pahs)[1:13])


n=5 # Number of lines per variable
myspacing=n*2+1
xseq=seq((n+1),14*myspacing, by=myspacing)
background=TRUE
myrange <- c(0,max(or_pahs, na.rm = T)+1)

background_colour="darkgrey"

pdf("../figures/Univ/OR_pahs.pdf", height = 7, width = 13)
par(mar = c(12, 2, 2, 5))
plot(or_pahs, ylim=myrange, xlim=c(1,length(or_pahs)), type="n",
     xaxt="n", yaxt="n",
     col=c(mycolours[2], NA, mycolours[4],NA, mycolours[6], NA, mycolours[8],NA, mycolours[10],NA),
     lwd=1, ylab="",xlab="")
xseqgreysep=c(min(xseq)-myspacing/2,apply(rbind(xseq[-1],xseq[-length(xseq)]),2,mean),max(xseq)+myspacing/2)

if (background){
  for (k in seq(1,length(xseqgreysep),by=2)){
    polygon(x=c(xseqgreysep[k],xseqgreysep[k+1],xseqgreysep[k+1],xseqgreysep[k]),
            y=c(-10,-10,15,15), col=lighten(background_colour,0.95), border=NA)
  }
  for (k in seq(2,length(xseqgreysep),by=2)){
    polygon(x=c(xseqgreysep[k],xseqgreysep[k+1],xseqgreysep[k+1],xseqgreysep[k]),
            y=c(-10,-10,15,15), col=lighten(background_colour,0.99), border=NA)
  }
  box()
}
par(new=TRUE)
plot(or_pahs, ylim=myrange,
     xaxt="n", yaxt="n",
     col=c(NA, mycolours[2], NA, mycolours[4],NA, mycolours[6], NA, mycolours[8],NA, mycolours[10],NA),
     pch = c(NA, 15, NA, 16,NA, 17, NA, 18,NA, 25,NA),
     lwd=1, ylab="", xlab="")
x_pahs <- 1:154
arrows(x0=x_pahs, y0=up_pahs, x1=x_pahs, y1=lo_pahs, code=3, angle=90, length=0.05, 
       col=c(NA, mycolours[2], NA, mycolours[4],NA, mycolours[6], NA, mycolours[8],NA, mycolours[10],NA))
axis(side=4, at=axTicks(2))
mtext(side=4, text="Odds Ratio", line=2)
#xseqblack=c(xseq[!duplicated(variable_cat)]-myspacing/2, max(xseq)+myspacing/2)
#for (k in 1:(length(xseqblack)-1)){
#  axis(side=3, at=xseqblack[c(k,k+1)]+c(2,-2), line=0.5, labels=NA)
#}
#for (k in 1:(length(xseqblack)-1)){
#  axis(side=3, at=mean(xseqblack[c(k,k+1)]), line=0.2, tick=FALSE,
#       labels=unique(variable_cat)[k])
#}
#par(xpd=TRUE)

abline(h=1, lty = 2, col="gray48")
abline(v=xseqgreysep,lty=3,col="black",lwd=0.5)
for (k in 1:length(xseq)){
  mytext=labels_pahs[k]
  if (grepl("m\\^", mytext)){
    mytext=gsub("m\\^","'~m^", mytext)
    mytext=sub(")","~')", mytext)
  }
  if (grepl("\\[", mytext)){
    mytext=gsub("\\[","'[", mytext)
    mytext=sub("\\(ug","~'(ug", mytext)
  }
  mytmp=eval(parse(text=paste0("expression(","'", mytext,"'",")")))
  #labcol=ifelse((manhat_pahs_1[k,1]>=bonf & manhat[k,2]>=bonf), mycolours[3],
  #ifelse(manhat[k,1]>=bonf,mycolours[1],
  #ifelse(manhat[k,2]>=bonf, mycolours[2],"black")))
  #if(is.na(labcol)){
  #labcol=ifelse(manhat[k,1]>=bonf,mycolours[1],
  #ifelse(manhat[k,2]>=bonf, mycolours[2],"black"))
  #}
  #labfont=ifelse(labcol=="black",1,2)
  axis(side=1, at=xseq[k], labels=mytmp, las=2, cex.axis=0.7)
}
legend("topright", pch=c(15, 16, 17, 18, 25), 
       col=c(mycolours[c(2,4,6,8,10)]),
       legend=c(outcomes),
       cex=0.7, bg = "white", bty = "n")
dev.off()


### pest OR
labels_pest <- c("CountryBelgium", "CountryFrance",colnames(pest)[17:49])
labels_pest_1 <- c("CountryBelgium", "CountryFrance", colnames(pest)[17:32])
labels_pest_2 <- colnames(pest)[33:49]



n=5 # Number of lines per variable
myspacing=n*2+1
xseq=seq((n+1),18*myspacing, by=myspacing)
background=TRUE
myrange <- c(0,max(or_pest, na.rm = T)+3)

background_colour="darkgrey"

pdf("../figures/Univ/OR_pest_1.pdf", height = 7, width = 13)
par(mar = c(10, 2, 2, 5))
plot(or_pest[1:198], ylim=myrange, xlim=c(1,length(or_pest)), type="n",
     xaxt="n", yaxt="n",
     col=c(mycolours[1], NA, mycolours[2],NA, mycolours[3], NA, mycolours[4],NA, mycolours[5],NA),
     lwd=1, ylab="",xlab="")
xseqgreysep=c(min(xseq)-myspacing/2,apply(rbind(xseq[-1],xseq[-length(xseq)]),2,mean),max(xseq)+myspacing/2)

if (background){
  for (k in seq(1,length(xseqgreysep),by=2)){
    polygon(x=c(xseqgreysep[k],xseqgreysep[k+1],xseqgreysep[k+1],xseqgreysep[k]),
            y=c(-10,-10,15,15), col=lighten(background_colour,0.95), border=NA)
  }
  for (k in seq(2,length(xseqgreysep),by=2)){
    polygon(x=c(xseqgreysep[k],xseqgreysep[k+1],xseqgreysep[k+1],xseqgreysep[k]),
            y=c(-10,-10,15,15), col=lighten(background_colour,0.99), border=NA)
  }
  box()
}
par(new=TRUE)
plot(or_pest[1:198], ylim=myrange,
     xaxt="n", yaxt="n",
     col=c(NA, mycolours[1], NA, mycolours[2],NA, mycolours[3], NA, mycolours[4],NA, mycolours[5],NA),
     pch = c(NA, 15, NA, 16,NA, 17, NA, 18,NA, 19,NA),
     lwd=1, ylab="", xlab="", cex=0.8)
x_pest <- 1:198
arrows(x0=x_pest, y0=up_pest[1:198], x1=x_pest, y1=lo_pest[1:198], code=3, angle=90, length=0.03, 
       col=c(NA, mycolours[1], NA, mycolours[2],NA, mycolours[3], NA, mycolours[4],NA, mycolours[5],NA))
axis(side=4, at=axTicks(2))
mtext(side=4, text="Odds Ratio", line=2)
#xseqblack=c(xseq[!duplicated(variable_cat)]-myspacing/2, max(xseq)+myspacing/2)
#for (k in 1:(length(xseqblack)-1)){
#  axis(side=3, at=xseqblack[c(k,k+1)]+c(2,-2), line=0.5, labels=NA)
#}
#for (k in 1:(length(xseqblack)-1)){
#  axis(side=3, at=mean(xseqblack[c(k,k+1)]), line=0.2, tick=FALSE,
#       labels=unique(variable_cat)[k])
#}
#par(xpd=TRUE)

abline(h=1, lty = 2, col="gray48")
abline(v=xseqgreysep,lty=3,col="black",lwd=0.5)
for (k in 1:length(xseq)){
  mytext=labels_pest_1[k]
  if (grepl("m\\^", mytext)){
    mytext=gsub("m\\^","'~m^", mytext)
    mytext=sub(")","~')", mytext)
  }
  if (grepl("\\[", mytext)){
    mytext=gsub("\\[","'[", mytext)
    mytext=sub("\\(ug","~'(ug", mytext)
  }
  mytmp=eval(parse(text=paste0("expression(","'", mytext,"'",")")))
  #labcol=ifelse((manhat_pahs_1[k,1]>=bonf & manhat[k,2]>=bonf), mycolours[3],
  #ifelse(manhat[k,1]>=bonf,mycolours[1],
  #ifelse(manhat[k,2]>=bonf, mycolours[2],"black")))
  #if(is.na(labcol)){
  #labcol=ifelse(manhat[k,1]>=bonf,mycolours[1],
  #ifelse(manhat[k,2]>=bonf, mycolours[2],"black"))
  #}
  #labfont=ifelse(labcol=="black",1,2)
  axis(side=1, at=xseq[k], labels=mytmp, las=2, cex.axis = 0.8)
}
legend("topright", pch=c(15, 16, 17, 18, 19), 
       col=c(mycolours[1:5]),
       legend=c(outcomes),
       cex=0.7, bg = "white", bty = "n")
dev.off()


n=5 # Number of lines per variable
myspacing=n*2+1
xseq=seq((n+1),17*myspacing, by=myspacing)
background=TRUE
myrange <- c(0,max(or_pest, na.rm = T)+1)

background_colour="darkgrey"

pdf("../figures/Univ/OR_pest_2.pdf", height = 7, width = 13)
par(mar = c(10, 2, 2, 5))
plot(or_pest[199:385], ylim=myrange, xlim=c(1,length(or_pest)), type="n",
     xaxt="n", yaxt="n",
     col=c(mycolours[1], NA, mycolours[2],NA, mycolours[3], NA, mycolours[4],NA, mycolours[5],NA),
     lwd=1, ylab="",xlab="")
xseqgreysep=c(min(xseq)-myspacing/2,apply(rbind(xseq[-1],xseq[-length(xseq)]),2,mean),max(xseq)+myspacing/2)

if (background){
  for (k in seq(1,length(xseqgreysep),by=2)){
    polygon(x=c(xseqgreysep[k],xseqgreysep[k+1],xseqgreysep[k+1],xseqgreysep[k]),
            y=c(-10,-10,15,15), col=lighten(background_colour,0.95), border=NA)
  }
  for (k in seq(2,length(xseqgreysep),by=2)){
    polygon(x=c(xseqgreysep[k],xseqgreysep[k+1],xseqgreysep[k+1],xseqgreysep[k]),
            y=c(-10,-10,15,15), col=lighten(background_colour,0.99), border=NA)
  }
  box()
}
par(new=TRUE)
plot(or_pest[199:385], ylim=myrange,
     xaxt="n", yaxt="n",
     col=c(NA, mycolours[1], NA, mycolours[2],NA, mycolours[3], NA, mycolours[4],NA, mycolours[5],NA),
     pch = c(NA, 15, NA, 16,NA, 17, NA, 18,NA, 19,NA),
     lwd=1, ylab="", xlab="", cex=0.8)
x_pest <- 1:187
arrows(x0=x_pest, y0=up_pest[199:385], x1=x_pest, y1=lo_pest[199:385], code=3, angle=90, length=0.03, 
       col=c(NA, mycolours[1], NA, mycolours[2],NA, mycolours[3], NA, mycolours[4],NA, mycolours[5],NA))
axis(side=4, at=axTicks(2))
mtext(side=4, text="Odds Ratio", line=2)
#xseqblack=c(xseq[!duplicated(variable_cat)]-myspacing/2, max(xseq)+myspacing/2)
#for (k in 1:(length(xseqblack)-1)){
#  axis(side=3, at=xseqblack[c(k,k+1)]+c(2,-2), line=0.5, labels=NA)
#}
#for (k in 1:(length(xseqblack)-1)){
#  axis(side=3, at=mean(xseqblack[c(k,k+1)]), line=0.2, tick=FALSE,
#       labels=unique(variable_cat)[k])
#}
#par(xpd=TRUE)

abline(h=1, lty = 2, col="gray48")
abline(v=xseqgreysep,lty=3,col="black",lwd=0.5)
for (k in 1:length(xseq)){
  mytext=labels_pest_2[k]
  if (grepl("m\\^", mytext)){
    mytext=gsub("m\\^","'~m^", mytext)
    mytext=sub(")","~')", mytext)
  }
  if (grepl("\\[", mytext)){
    mytext=gsub("\\[","'[", mytext)
    mytext=sub("\\(ug","~'(ug", mytext)
  }
  mytmp=eval(parse(text=paste0("expression(","'", mytext,"'",")")))
  #labcol=ifelse((manhat_pahs_1[k,1]>=bonf & manhat[k,2]>=bonf), mycolours[3],
  #ifelse(manhat[k,1]>=bonf,mycolours[1],
  #ifelse(manhat[k,2]>=bonf, mycolours[2],"black")))
  #if(is.na(labcol)){
  #labcol=ifelse(manhat[k,1]>=bonf,mycolours[1],
  #ifelse(manhat[k,2]>=bonf, mycolours[2],"black"))
  #}
  #labfont=ifelse(labcol=="black",1,2)
  axis(side=1, at=xseq[k], labels=mytmp, las=2, cex.axis = 0.8)
}
legend("topright", pch=c(15, 16, 17, 18, 19), 
       col=c(mycolours[1:5]),
       legend=c(outcomes),
       cex=0.7, bg = "white", bty = "n")
dev.off()


labels_pest <- c("CountryBelgium", "CountryFrance", colnames(pest_imp)[1:4], 
                 "p,p-DDT", "p,p-DDE",
                 "Alpha-endosulfan", "Beta-endosulfan", "Beta-HCH", "Gamma-HCH",
                 colnames(pest_imp)[11:33])

n=5 # Number of lines per variable
myspacing=n*2+1
xseq=seq((n+1),35*myspacing, by=myspacing)
background=TRUE
myrange <- c(0,max(or_pest, na.rm = T)+3)

background_colour="darkgrey"

pdf("../figures/Univ/OR_pest.pdf", height = 7, width = 18)
par(mar = c(10, 2, 2, 5))
plot(or_pest, ylim=myrange, xlim=c(1,length(or_pest)), type="n",
     xaxt="n", yaxt="n",
     col=c(mycolours[2], NA, mycolours[4],NA, mycolours[6], NA, mycolours[8],NA, mycolours[10],NA),
     lwd=1, ylab="",xlab="")
xseqgreysep=c(min(xseq)-myspacing/2,apply(rbind(xseq[-1],xseq[-length(xseq)]),2,mean),max(xseq)+myspacing/2)

if (background){
  for (k in seq(1,length(xseqgreysep),by=2)){
    polygon(x=c(xseqgreysep[k],xseqgreysep[k+1],xseqgreysep[k+1],xseqgreysep[k]),
            y=c(-10,-10,15,15), col=lighten(background_colour,0.95), border=NA)
  }
  for (k in seq(2,length(xseqgreysep),by=2)){
    polygon(x=c(xseqgreysep[k],xseqgreysep[k+1],xseqgreysep[k+1],xseqgreysep[k]),
            y=c(-10,-10,15,15), col=lighten(background_colour,0.99), border=NA)
  }
  box()
}
par(new=TRUE)
plot(or_pest, ylim=myrange,
     xaxt="n", yaxt="n",
     col=c(NA, mycolours[2], NA, mycolours[4],NA, mycolours[6], NA, mycolours[8],NA, mycolours[10],NA),
     pch = c(NA, 15, NA, 16,NA, 17, NA, 18,NA, 25,NA),
     lwd=1, ylab="", xlab="", cex=0.8)
x_pest <- 1:385
arrows(x0=x_pest, y0=up_pest, x1=x_pest, y1=lo_pest, code=3, angle=90, length=0.03, 
       col=c(NA, mycolours[2], NA, mycolours[4],NA, mycolours[6], NA, mycolours[8],NA, mycolours[10],NA))
axis(side=4, at=axTicks(2))
mtext(side=4, text="Odds Ratio", line=2)

abline(h=1, lty = 2, col="gray48")
abline(v=xseqgreysep,lty=3,col="black",lwd=0.5)
for (k in 1:length(xseq)){
  mytext=labels_pest[k]
  if (grepl("m\\^", mytext)){
    mytext=gsub("m\\^","'~m^", mytext)
    mytext=sub(")","~')", mytext)
  }
  if (grepl("\\[", mytext)){
    mytext=gsub("\\[","'[", mytext)
    mytext=sub("\\(ug","~'(ug", mytext)
  }
  mytmp=eval(parse(text=paste0("expression(","'", mytext,"'",")")))
  axis(side=1, at=xseq[k], labels=mytmp, las=2, cex.axis = 0.8)
}
legend("topright", pch=c(15, 16, 17, 18, 25), 
       col=c(mycolours[c(2,4,6,8,10)]),
       legend=c(outcomes),
       cex=0.7, bg = "white", bty = "n")
dev.off()


### Overlapping OR
labels_over <- c("CountryBelgium", "Cotinine","Nicotine",colnames(over)[3:17], 
                 "p,p-DDT", "p,p-DDE", "alpha-endosulfan", "beta-endosulfan", "beta-HCH", "gamma-HCH",
                 colnames(over)[24:46])
n=5 # Number of lines per variable
myspacing=n*2+1
xseq=seq((n+1),47*myspacing, by=myspacing)
background=TRUE
myrange <- c(0,max(or_over, na.rm = T)+6)

background_colour="darkgrey"

pdf("../figures/Univ/OR_over.pdf", height = 7, width = 18)
par(mar = c(12, 2, 2, 5))
plot(or_over, ylim=myrange, xlim=c(1,length(or_over)), type="n",
     xaxt="n", yaxt="n",
     col=c(NA,mycolours[2], NA, mycolours[4],NA, mycolours[6], NA, mycolours[8],NA, mycolours[10],NA),
     lwd=1, ylab="",xlab="")
xseqgreysep=c(min(xseq)-myspacing/2,apply(rbind(xseq[-1],xseq[-length(xseq)]),2,mean),max(xseq)+myspacing/2)

if (background){
  for (k in seq(1,length(xseqgreysep),by=2)){
    polygon(x=c(xseqgreysep[k],xseqgreysep[k+1],xseqgreysep[k+1],xseqgreysep[k]),
            y=c(-10,-10,15,15), col=lighten(background_colour,0.95), border=NA)
  }
  for (k in seq(2,length(xseqgreysep),by=2)){
    polygon(x=c(xseqgreysep[k],xseqgreysep[k+1],xseqgreysep[k+1],xseqgreysep[k]),
            y=c(-10,-10,15,15), col=lighten(background_colour,0.99), border=NA)
  }
  box()
}
par(new=TRUE)
plot(or_over, ylim=myrange,
     xaxt="n", yaxt="n",
     col=c(NA, mycolours[2], NA, mycolours[4],NA, mycolours[6], NA, mycolours[8],NA, mycolours[10],NA),
     pch = c(NA, 15, NA, 16,NA, 17, NA, 18,NA, 25,NA),
     lwd=1, ylab="", xlab="", cex=0.8)
x_pest <- 1:517
arrows(x0=x_pest, y0=up_over, x1=x_pest, y1=lo_over, code=3, angle=90, length=0.03, 
       col=c(NA, mycolours[2], NA, mycolours[4],NA, mycolours[6], NA, mycolours[8],NA, mycolours[10],NA))
axis(side=4, at=axTicks(2))
mtext(side=4, text="Odds Ratio", line=2)

abline(h=1, lty = 2, col="gray48")
abline(v=xseqgreysep,lty=3,col="black",lwd=0.5)
for (k in 1:length(xseq)){
  mytext=labels_over[k]
  if (grepl("m\\^", mytext)){
    mytext=gsub("m\\^","'~m^", mytext)
    mytext=sub(")","~')", mytext)
  }
  if (grepl("\\[", mytext)){
    mytext=gsub("\\[","'[", mytext)
    mytext=sub("\\(ug","~'(ug", mytext)
  }
  mytmp=eval(parse(text=paste0("expression(","'", mytext,"'",")")))
  axis(side=1, at=xseq[k], labels=mytmp, las=2, cex.axis = 0.7)
}
legend("topright", pch=c(15, 16, 17, 18, 25), 
       col=c(mycolours[c(2,4,6,8,10)]),
       legend=c(outcomes),
       cex=0.7, bg = "white", bty = "n")
dev.off()



### PAHs adjusted country OR
labels_pahs <- c("CountryBelgium" ,colnames(pahs)[1:13])

n=5 # Number of lines per variable
myspacing=n*2+1
xseq=seq((n+1),13*myspacing, by=myspacing)
background=TRUE
myrange <- c(0,max(or_pahs_con, na.rm = T)+1)

background_colour="darkgrey"

pdf("../figures/Univ/OR_pahs_con.pdf", height = 7, width = 13)
par(mar = c(12, 2, 2, 5))
plot(or_pahs_con, ylim=myrange, xlim=c(1,length(or_pahs_con)), type="n",
     xaxt="n", yaxt="n",
     col=c(mycolours[2], NA, mycolours[4],NA, mycolours[6], NA, mycolours[8],NA, mycolours[10],NA),
     lwd=1, ylab="",xlab="")
xseqgreysep=c(min(xseq)-myspacing/2,apply(rbind(xseq[-1],xseq[-length(xseq)]),2,mean),max(xseq)+myspacing/2)

if (background){
  for (k in seq(1,length(xseqgreysep),by=2)){
    polygon(x=c(xseqgreysep[k],xseqgreysep[k+1],xseqgreysep[k+1],xseqgreysep[k]),
            y=c(-10,-10,15,15), col=lighten(background_colour,0.95), border=NA)
  }
  for (k in seq(2,length(xseqgreysep),by=2)){
    polygon(x=c(xseqgreysep[k],xseqgreysep[k+1],xseqgreysep[k+1],xseqgreysep[k]),
            y=c(-10,-10,15,15), col=lighten(background_colour,0.99), border=NA)
  }
  box()
}
par(new=TRUE)
plot(or_pahs_con, ylim=myrange,
     xaxt="n", yaxt="n",
     col=c(NA, mycolours[2], NA, mycolours[4],NA, mycolours[6], NA, mycolours[8],NA, mycolours[10],NA),
     pch = c(NA, 15, NA, 16,NA, 17, NA, 18,NA, 25,NA),
     lwd=1, ylab="", xlab="")
x_pahs <- 1:143
arrows(x0=x_pahs, y0=up_pahs_con, x1=x_pahs, y1=lo_pahs_con, code=3, angle=90, length=0.05, 
       col=c(NA, mycolours[2], NA, mycolours[4],NA, mycolours[6], NA, mycolours[8],NA, mycolours[10],NA))
axis(side=4, at=axTicks(2))
mtext(side=4, text="Odds Ratio", line=2)
abline(h=1, lty = 2, col="gray48")
abline(v=xseqgreysep,lty=3,col="black",lwd=0.5)
for (k in 1:length(xseq)){
  mytext=labels_pahs[k]
  if (grepl("m\\^", mytext)){
    mytext=gsub("m\\^","'~m^", mytext)
    mytext=sub(")","~')", mytext)
  }
  if (grepl("\\[", mytext)){
    mytext=gsub("\\[","'[", mytext)
    mytext=sub("\\(ug","~'(ug", mytext)
  }
  mytmp=eval(parse(text=paste0("expression(","'", mytext,"'",")")))
  axis(side=1, at=xseq[k], labels=mytmp, las=2, cex.axis=0.7)
}
legend("topright", pch=c(15, 16, 17, 18, 25), 
       col=c(mycolours[c(2,4,6,8,10)]),
       legend=c(outcomes),
       cex=0.7, bg = "white", bty = "n")
dev.off()


### Pest adjusted country OR
n=5 # Number of lines per variable
myspacing=n*2+1
xseq=seq((n+1),33*myspacing, by=myspacing)
background=TRUE
myrange <- c(0,max(or_pest_con, na.rm = T)+3)

background_colour="darkgrey"

pdf("../figures/Univ/OR_pest_con.pdf", height = 7, width = 18)
par(mar = c(10, 2, 2, 5))
plot(or_pest_con, ylim=myrange, xlim=c(1,length(or_pest_con)), type="n",
     xaxt="n", yaxt="n",
     col=c(mycolours[2], NA, mycolours[4],NA, mycolours[6], NA, mycolours[8],NA, mycolours[10],NA),
     lwd=1, ylab="",xlab="")
xseqgreysep=c(min(xseq)-myspacing/2,apply(rbind(xseq[-1],xseq[-length(xseq)]),2,mean),max(xseq)+myspacing/2)

if (background){
  for (k in seq(1,length(xseqgreysep),by=2)){
    polygon(x=c(xseqgreysep[k],xseqgreysep[k+1],xseqgreysep[k+1],xseqgreysep[k]),
            y=c(-10,-10,15,15), col=lighten(background_colour,0.95), border=NA)
  }
  for (k in seq(2,length(xseqgreysep),by=2)){
    polygon(x=c(xseqgreysep[k],xseqgreysep[k+1],xseqgreysep[k+1],xseqgreysep[k]),
            y=c(-10,-10,15,15), col=lighten(background_colour,0.99), border=NA)
  }
  box()
}
par(new=TRUE)
plot(or_pest_con, ylim=myrange,
     xaxt="n", yaxt="n",
     col=c(NA, mycolours[2], NA, mycolours[4],NA, mycolours[6], NA, mycolours[8],NA, mycolours[10],NA),
     pch = c(NA, 15, NA, 16,NA, 17, NA, 18,NA, 25,NA),
     lwd=1, ylab="", xlab="", cex=0.8)
x_pest <- 1:363
arrows(x0=x_pest, y0=up_pest_con, x1=x_pest, y1=lo_pest_con, code=3, angle=90, length=0.03, 
       col=c(NA, mycolours[2], NA, mycolours[4],NA, mycolours[6], NA, mycolours[8],NA, mycolours[10],NA))
axis(side=4, at=axTicks(2))
mtext(side=4, text="Odds Ratio", line=2)
abline(h=1, lty = 2, col="gray48")
abline(v=xseqgreysep,lty=3,col="black",lwd=0.5)
for (k in 1:length(xseq)){
  mytext=labels_pest[k]
  if (grepl("m\\^", mytext)){
    mytext=gsub("m\\^","'~m^", mytext)
    mytext=sub(")","~')", mytext)
  }
  if (grepl("\\[", mytext)){
    mytext=gsub("\\[","'[", mytext)
    mytext=sub("\\(ug","~'(ug", mytext)
  }
  mytmp=eval(parse(text=paste0("expression(","'", mytext,"'",")")))
  axis(side=1, at=xseq[k], labels=mytmp, las=2, cex.axis = 0.8)
}
legend("topright", pch=c(15, 16, 17, 18, 25), 
       col=c(mycolours[c(2,4,6,8,10)]),
       legend=c(outcomes),
       cex=0.7, bg = "white", bty = "n")
dev.off()



