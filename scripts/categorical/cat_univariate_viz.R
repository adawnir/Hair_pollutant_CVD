
### Sumemr Project --  univariate analysis considering categorical variables visualisation (Forest plot and Manhattan plot)
## Programme created by Vivian on 20 July

rm(list=ls())
project_path="~/hair/scripts"
setwd(project_path)

##### Loading packages
library(RColorBrewer)
library(tidyverse)
library(plotrix)
library(colorspace)
library(ggrepel)
#install.packages("viridis") 
library("viridis") 

##### Load data --
pest <- readRDS("../cat_results/pest_cat.rds")
pahs <- readRDS("../cat_results/pahs_cat.rds")

manhat_pahs <- as.data.frame(readRDS("../cat_results/manhattan_pahs.rds"))
manhat_pest <- as.data.frame(readRDS("../cat_results/manhattan_pest.rds"))
manhat_pahs_cat <- as.data.frame(readRDS("../cat_results/manhattan_pahs_cat.rds"))
manhat_pest_cat <- as.data.frame(readRDS("../cat_results/manhattan_pest_cat.rds"))

forest_pahs <- as.data.frame(readRDS("../cat_results/forest_pahs.rds"))
forest_pest <- as.data.frame(readRDS("../cat_results/forest_pest.rds"))
forest_pahs_cat <- as.data.frame(readRDS("../cat_results/forest_pahs_cat.rds"))
forest_pest_cat <- as.data.frame(readRDS("../cat_results/forest_pest_cat.rds"))


##### Change data structure --
myorder <- c("obesity", "lipid_disorder", "hypertension", "diabetes","MS")
manhat_pest <- manhat_pest[, myorder]
manhat_pahs <- manhat_pahs[, myorder]
manhat_pest_cat <- manhat_pest_cat[, myorder]
manhat_pahs_cat <- manhat_pahs_cat[, myorder]

manhat_pest_cat_tmp <- manhat_pest_cat %>%
  add_column(Empty1 = NA, .after="obesity") %>% 
  add_column(Empty2 = NA, .after="lipid_disorder") %>%
  add_column(Empty3 = NA, .after="hypertension") %>%
  add_column(Empty4 = NA, .after="diabetes") %>%
  add_column(Empty5 = NA, .after="MS")  %>%
  add_column(Empty0 = NA, .before = "obesity")

manhat_pest_cat_tmp1 <- data.frame(t(manhat_pest_cat_tmp))
manhat_pest_cat1 <- unlist(manhat_pest_cat_tmp1)

manhat_pahs_cat_tmp <- manhat_pahs_cat %>%
  add_column(Empty1 = NA, .after="obesity") %>% 
  add_column(Empty2 = NA, .after="lipid_disorder") %>%
  add_column(Empty3 = NA, .after="hypertension") %>%
  add_column(Empty4 = NA, .after="diabetes") %>%
  add_column(Empty5 = NA, .after="MS")  %>%
  add_column(Empty0 = NA, .before = "obesity")

manhat_pahs_cat_tmp1 <- data.frame(t(manhat_pahs_cat_tmp))
manhat_pahs_cat1 <- unlist(manhat_pahs_cat_tmp1)

or_pahs_cat_tmp <- forest_pahs_cat %>% 
  dplyr::select(c("or_pahs_cat_ob", "or_pahs_cat_ld", "or_pahs_cat_hyper", "or_pahs_cat_dia", "or_pahs_cat_ms"))
or_pahs_cat_tmp <- or_pahs_cat_tmp %>%
  add_column(Empty1 = NA, .after="or_pahs_cat_ob") %>% 
  add_column(Empty2 = NA, .after="or_pahs_cat_ld") %>%
  add_column(Empty3 = NA, .after="or_pahs_cat_hyper") %>%
  add_column(Empty4 = NA, .after="or_pahs_cat_dia") %>%
  add_column(Empty5 = NA, .after="or_pahs_cat_ms")  %>%
  add_column(Empty0 = NA, .before = "or_pahs_cat_ob")  
or_pahs_cat_tmp1 <- data.frame(t(or_pahs_cat_tmp))
or_pahs_cat <- unlist(or_pahs_cat_tmp1)

up_pahs_cat_tmp <- forest_pahs_cat %>% 
  dplyr::select(c("u95_pahs_cat_ob", "u95_pahs_cat_ld", "u95_pahs_cat_hyper", "u95_pahs_cat_dia", "u95_pahs_cat_ms"))
up_pahs_cat_tmp <- up_pahs_cat_tmp %>%
  add_column(Empty1 = NA, .after="u95_pahs_cat_ob") %>% 
  add_column(Empty2 = NA, .after="u95_pahs_cat_ld") %>%
  add_column(Empty3 = NA, .after="u95_pahs_cat_hyper") %>%
  add_column(Empty4 = NA, .after="u95_pahs_cat_dia") %>%
  add_column(Empty5 = NA, .after="u95_pahs_cat_ms")  %>%
  add_column(Empty0 = NA, .before = "u95_pahs_cat_ob")  
up_pahs_cat_tmp1 <- data.frame(t(up_pahs_cat_tmp))
up_pahs_cat <- unlist(up_pahs_cat_tmp1)

lo_pahs_cat_tmp <- forest_pahs_cat %>% 
  dplyr::select(c("l95_pahs_cat_ob", "l95_pahs_cat_ld", "l95_pahs_cat_hyper", "l95_pahs_cat_dia", "l95_pahs_cat_ms"))
lo_pahs_cat_tmp <- lo_pahs_cat_tmp %>%
  add_column(Empty1 = NA, .after="l95_pahs_cat_ob") %>% 
  add_column(Empty2 = NA, .after="l95_pahs_cat_ld") %>%
  add_column(Empty3 = NA, .after="l95_pahs_cat_hyper") %>%
  add_column(Empty4 = NA, .after="l95_pahs_cat_dia") %>%
  add_column(Empty5 = NA, .after="l95_pahs_cat_ms")  %>%
  add_column(Empty0 = NA, .before = "l95_pahs_cat_ob")  
lo_pahs_cat_tmp1 <- data.frame(t(lo_pahs_cat_tmp))
lo_pahs_cat <- unlist(lo_pahs_cat_tmp1)


or_pest_cat_tmp <- forest_pest_cat %>% 
  dplyr::select(c("or_pest_cat_ob", "or_pest_cat_ld", "or_pest_cat_hyper", "or_pest_cat_dia", "or_pest_cat_ms"))
or_pest_cat_tmp <- or_pest_cat_tmp %>%
  add_column(Empty1 = NA, .after="or_pest_cat_ob") %>% 
  add_column(Empty2 = NA, .after="or_pest_cat_ld") %>%
  add_column(Empty3 = NA, .after="or_pest_cat_hyper") %>%
  add_column(Empty4 = NA, .after="or_pest_cat_dia") %>%
  add_column(Empty5 = NA, .after="or_pest_cat_ms")  %>%
  add_column(Empty0 = NA, .before = "or_pest_cat_ob")  
or_pest_cat_tmp1 <- data.frame(t(or_pest_cat_tmp))
or_pest_cat <- unlist(or_pest_cat_tmp1)

up_pest_cat_tmp <- forest_pest_cat %>% 
  dplyr::select(c("u95_pest_cat_ob", "u95_pest_cat_ld", "u95_pest_cat_hyper", "u95_pest_cat_dia", "u95_pest_cat_ms"))
up_pest_cat_tmp <- up_pest_cat_tmp %>%
  add_column(Empty1 = NA, .after="u95_pest_cat_ob") %>% 
  add_column(Empty2 = NA, .after="u95_pest_cat_ld") %>%
  add_column(Empty3 = NA, .after="u95_pest_cat_hyper") %>%
  add_column(Empty4 = NA, .after="u95_pest_cat_dia") %>%
  add_column(Empty5 = NA, .after="u95_pest_cat_ms")  %>%
  add_column(Empty0 = NA, .before = "u95_pest_cat_ob")  
up_pest_cat_tmp1 <- data.frame(t(up_pest_cat_tmp))
up_pest_cat <- unlist(up_pest_cat_tmp1)

lo_pest_cat_tmp <- forest_pest_cat %>% 
  dplyr::select(c("l95_pest_cat_ob", "l95_pest_cat_ld", "l95_pest_cat_hyper", "l95_pest_cat_dia", "l95_pest_cat_ms"))
lo_pest_cat_tmp <- lo_pest_cat_tmp %>%
  add_column(Empty1 = NA, .after="l95_pest_cat_ob") %>% 
  add_column(Empty2 = NA, .after="l95_pest_cat_ld") %>%
  add_column(Empty3 = NA, .after="l95_pest_cat_hyper") %>%
  add_column(Empty4 = NA, .after="l95_pest_cat_dia") %>%
  add_column(Empty5 = NA, .after="l95_pest_cat_ms")  %>%
  add_column(Empty0 = NA, .before = "l95_pest_cat_ob")  
lo_pest_cat_tmp1 <- data.frame(t(lo_pest_cat_tmp))
lo_pest_cat <- unlist(lo_pest_cat_tmp1)



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



##### MANHATTAN --
### pest
labels_pest <- c("PCB 138", "PCB 153", "PCB 180", "BDE 47", colnames(pest)[c(14:22)],
                 "Carbofuran phenol", colnames(pest)[24:27], "2 IPP", colnames(pest)[c(29:37)], 
                 "3Me4NP", colnames(pest)[39],
                 "3 PBA", "Fipronil sulfone", colnames(pest)[42])

mycolours = brewer.pal(10, "Paired")
#mycolours <- c("lightcoral","lightgoldenrod", "palegreen2","steelblue1", "lightslateblue")
outcomes <- c("Obesity", "Lipid disorder", "Hypertension", "Diabetes","Metabolic syndrome")

n=5 # Number of lines per variable
myspacing=n*2+1
xseq=seq((n+1),33*myspacing, by=myspacing)
background=TRUE
myrange <- c(0,max(manhat_pest_cat1, na.rm = T))

background_colour="darkgrey"

pdf("../cat_figures/univ/manhat_pest_cat1.pdf", height = 7, width = 13)
par(mar = c(10, 2, 2, 5))
plot(manhat_pest_cat1, ylim=myrange, xlim=c(1,length(manhat_pest_cat1)), type="n",
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
plot(manhat_pest_cat1, ylim=myrange,
     xaxt="n", yaxt="n", cex = 0.8, 
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
  #labcol=ifelse((manhat_pahs_1[k,1]>=bonf & manhat[k,2]>=bonf), mycolours[3],
  #ifelse(manhat[k,1]>=bonf,mycolours[1],
  #ifelse(manhat[k,2]>=bonf, mycolours[2],"black")))
  #if(is.na(labcol)){
  #labcol=ifelse(manhat[k,1]>=bonf,mycolours[1],
  #ifelse(manhat[k,2]>=bonf, mycolours[2],"black"))
  #}
  #labfont=ifelse(labcol=="black",1,2)
  axis(side=1, at=xseq[k], labels=mytmp, las=2, cex.axis=0.8)
}
legend("topright", pch=c(15, 16, 17, 18, 25, NA, NA), lty=c(NA, NA, NA, NA, NA, 2, 2), 
       col=c(mycolours[c(2,4,6,8,10)],"darkred","darkgrey"),
       legend=c(outcomes,"Bonferroni threshold","Nominal threshold"),
       cex=0.7, bg = "white", bty = "n")
dev.off()


### PAHs
labels_pahs <- c("9-OH-Fluorene", "3-OH-Fluorene", "4-OH-Phenanthrene", "3-OH-Phenanthrene",
                 "1-OH-Phenanthrene", "2-OH-Phenanthrene", "1-OH-Pyrene",       
                 "6-OH-Chrysene11-OH-Benz(a)anthrac", "Cotinine", "2-OH-Fluorene",                    
                 "Nicotine", "1-OH-Naphthalene", "2-OH-Naphthalene")


outcomes <- c("Obesity", "Lipid disorder", "Hypertension", "Diabetes","Metabolic syndrome")


n=5 # Number of lines per variable
myspacing=n*2+1
xseq=seq((n+1),13*myspacing, by=myspacing)
background=TRUE
myrange <- c(0,max(manhat_pahs_cat1, na.rm = T))

background_colour="darkgrey"

pdf("../cat_figures/univ/manhat_pahs_cat1.pdf", height = 7, width = 13)
par(mar = c(12, 2, 2, 5))
plot(manhat_pahs_cat1, ylim=myrange, xlim=c(1,length(manhat_pahs_cat1)), type="n",
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
plot(manhat_pahs_cat1, ylim=myrange,
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
  labcol=ifelse(k<=8, "black", ifelse(k>8 & k<=10, "goldenrod", "lightpink2"))
  #if(is.na(labcol)){
  #labcol=ifelse(manhat[k,1]>=bonf,mycolours[1],
  #ifelse(manhat[k,2]>=bonf, mycolours[2],"black"))
  #}
  axis(side=1, at=xseq[k], labels=mytmp, las=2, cex.axis=0.7, col = labcol, col.axis=labcol)
}
legend("topright", pch=c(15, 16, 17, 18, 25, NA, NA), lty=c(NA, NA, NA, NA, NA, 2, 2), 
       col=c(mycolours[c(2,4,6,8,10)],"darkred","gray48"),
       legend=c(outcomes,"Bonferroni threshold","Nominal threshold"),
       cex=0.7, bg = "white", bty = "n")
dev.off()


##### OR plot --
### PAHs OR
rownames(forest_pahs_cat)
labels_pahs <-c("9-OH-Fluorene", "3-OH-Fluorene", "4-OH-Phenanthrene", "3-OH-Phenanthrene",
                "1-OH-Phenanthrene", "2-OH-Phenanthrene", "1-OH-Pyrene", 
                "6-OH-Chrysene11-OH-Benz(a)anthrac", "Cotinine", "2-OH-Fluorene",                    
                "Nicotine2", "Nicotine3", "Nicotine4",
                "1-OH-Naphthalene2", "1-OH-Naphthalene3", "1-OH-Naphthalene4", 
                "2-OH-Naphthalene2", "2-OH-Naphthalene3", "2-OH-Naphthalene4")

n=5 # Number of lines per variable
myspacing=n*2+1
xseq=seq((n+1),19*myspacing, by=myspacing)
background=TRUE
myrange <- c(0,33)

background_colour="darkgrey"

pdf("../cat_figures/univ/OR_pahs_cat1.pdf", height = 7, width = 13)
par(mar = c(12, 2, 2, 5))
plot(or_pahs_cat, ylim=myrange, xlim=c(1,length(or_pahs_cat)), type="n",
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
plot(or_pahs_cat, ylim=myrange,
     xaxt="n", yaxt="n",
     col=c(NA, mycolours[2], NA, mycolours[4],NA, mycolours[6], NA, mycolours[8],NA, mycolours[10],NA),
     pch = c(NA, 15, NA, 16,NA, 17, NA, 18,NA, 25,NA),
     lwd=1, ylab="", xlab="")
x_pahs <- 1:209
arrows(x0=x_pahs, y0=up_pahs_cat, x1=x_pahs, y1=lo_pahs_cat, code=3, angle=90, length=0.05, 
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
  labcol=ifelse(k<=8, "black", ifelse(k>8 & k<=10, "goldenrod", "lightpink3"))
  axis(side=1, at=xseq[k], labels=mytmp, las=2, cex.axis=0.7, col = labcol, col.axis=labcol)
}
legend("topright", pch=c(15, 16, 17, 18, 25), 
       col=c(mycolours[c(2,4,6,8,10)]),
       legend=c(outcomes),
       cex=0.7, bg = "white", bty = "n")
dev.off()


rownames(forest_pahs)
labels_pahs <- c("Cotinine", "2-OH-Fluorene", "Nicotine", "1-OH-Naphthalene", "2-OH-Naphthalene")
n=5 # Number of lines per variable
myspacing=n*2+1
xseq=seq((n+1),5*myspacing, by=myspacing)
background=TRUE
myrange <- c(0,max(or_pahs, na.rm = T)+1)

background_colour="darkgrey"

pdf("../cat_figures/univ/OR_pahs1.pdf", height = 7, width = 13)
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
x_pahs <- 1:55
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
  labcol=ifelse(k<=2,  "goldenrod", "lightpink3")
  axis(side=1, at=xseq[k], labels=mytmp, las=2, cex.axis=0.7, col = labcol, col.axis=labcol)
}
legend("topright", pch=c(15, 16, 17, 18, 25), 
       col=c(mycolours[c(2,4,6,8,10)]),
       legend=c(outcomes),
       cex=0.7, bg = "white", bty = "n")
dev.off()





### pest OR
rownames(forest_pest_cat)
labels_pest_1 <- c("PCB 138", "PCB 153", "PCB 180", "BDE 47", colnames(pest)[c(14:22)],
                   "Carbofuran phenol", colnames(pest)[24:27], "2-IPP", colnames(pest)[29])
labels_pest_2 <- rownames(forest_pest_cat)[21:41]
labels_pest_3 <- c(rownames(forest_pest_cat)[42:44], "3Me4NP2", "3Me4NP3", "3Me4NP4",
                   rownames(forest_pest_cat)[48:50], "3PBA2", "3PBA3", "3PBA4",
                   "Fipronil sulfone2", "Fipronil_sulfone3", "Fipronil sulfone4",
                   rownames(forest_pest_cat)[57:59])


n=5 # Number of lines per variable
myspacing=n*2+1
xseq=seq((n+1),20*myspacing, by=myspacing)
background=TRUE
myrange <- c(0,max(or_pest_cat, na.rm = T)+3)

background_colour="darkgrey"

pdf("../cat_figures/univ/OR_pest_cat11.pdf", height = 7, width = 13)
par(mar = c(10, 2, 2, 5))
plot(or_pest_cat[1:220], ylim=myrange, xlim=c(1,length(or_pest_cat[1:220])), type="n",
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
plot(or_pest_cat[1:220], ylim=myrange,
     xaxt="n", yaxt="n",
     col=c(NA, mycolours[2], NA, mycolours[4],NA, mycolours[6], NA, mycolours[8],NA, mycolours[10],NA),
     pch = c(NA, 15, NA, 16,NA, 17, NA, 18,NA, 25,NA),
     lwd=1, ylab="", xlab="", cex=0.8)
x_pest <- 1:220
arrows(x0=x_pest, y0=up_pest_cat[1:220], x1=x_pest, y1=lo_pest_cat[1:220], code=3, angle=90, length=0.03, 
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
  labcol=ifelse(k<=15, "black", "goldenrod")
  axis(side=1, at=xseq[k], labels=mytmp, las=2, cex.axis=0.8, col = labcol, col.axis=labcol)
}
legend("topright", pch=c(15, 16, 17, 18, 25), 
       col=c(mycolours[c(2,4,6,8,10)]),
       legend=c(outcomes),
       cex=0.7, bg = "white", bty = "n")
dev.off()


n=5 # Number of lines per variable
myspacing=n*2+1
xseq=seq((n+1),21*myspacing, by=myspacing)
background=TRUE
myrange <- c(0,max(up_pest_cat, na.rm = T)+1)

background_colour="darkgrey"

pdf("../cat_figures/univ/OR_pest_cat12.pdf", height = 7, width = 13)
par(mar = c(10, 2, 2, 5))
plot(or_pest_cat[221:451], ylim=myrange, xlim=c(1,length(or_pest_cat[220:451])), type="n",
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
plot(or_pest_cat[221:451], ylim=myrange,
     xaxt="n", yaxt="n",
     col=c(NA, mycolours[2], NA, mycolours[4],NA, mycolours[6], NA, mycolours[8],NA, mycolours[10],NA),
     pch = c(NA, 15, NA, 16,NA, 17, NA, 18,NA, 25,NA),
     lwd=1, ylab="", xlab="", cex=0.8)
x_pest <- 1:231
arrows(x0=x_pest, y0=up_pest_cat[221:451], x1=x_pest, y1=lo_pest_cat[221:451], code=3, angle=90, length=0.03, 
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
  axis(side=1, at=xseq[k], labels=mytmp, las=2, cex.axis = 0.8, col = "lightpink3", col.axis="lightpink3")
}
legend("topright", pch=c(15, 16, 17, 18, 25), 
       col=c(mycolours[c(2,4,6,8,10)]),
       legend=c(outcomes),
       cex=0.7, bg = "white", bty = "n")
dev.off()


n=5 # Number of lines per variable
myspacing=n*2+1
xseq=seq((n+1),18*myspacing, by=myspacing)
background=TRUE
myrange <- c(0,max(or_pest_cat, na.rm = T)+3)

background_colour="darkgrey"

pdf("../cat_figures/univ/OR_pest_cat13.pdf", height = 7, width = 13)
par(mar = c(10, 2, 2, 5))
plot(or_pest_cat[452:649], ylim=myrange, xlim=c(1,length(or_pest_cat[452:649])), type="n",
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
plot(or_pest_cat[452:649], ylim=myrange,
     xaxt="n", yaxt="n",
     col=c(NA, mycolours[2], NA, mycolours[4],NA, mycolours[6], NA, mycolours[8],NA, mycolours[10],NA),
     pch = c(NA, 15, NA, 16,NA, 17, NA, 18,NA, 25,NA),
     lwd=1, ylab="", xlab="", cex=0.8)
x_pest <- 1:198
arrows(x0=x_pest, y0=up_pest_cat[452:649], x1=x_pest, y1=lo_pest_cat[452:649], code=3, angle=90, length=0.03, 
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
  mytext=labels_pest_3[k]
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
  axis(side=1, at=xseq[k], labels=mytmp, las=2, cex.axis = 0.8, col = "lightpink3", col.axis="lightpink3")
}
legend("topright", pch=c(15, 16, 17, 18, 25), 
       col=c(mycolours[c(2,4,6,8,10)]),
       legend=c(outcomes),
       cex=0.7, bg = "white", bty = "n")
dev.off()



rownames(forest_pest)
labels_pest <- c(rownames(forest_pest)[1:3], "2-IPP", rownames(forest_pest)[5:13],
                 "3Me4NP", "Cl2CA", "3 PBA", "Fipronil sulfone", "Trifluralin")
n=5 # Number of lines per variable
myspacing=n*2+1
xseq=seq((n+1),18*myspacing, by=myspacing)
background=TRUE
myrange <- c(0,max(or_pest, na.rm = T)+1)

background_colour="darkgrey"

pdf("../cat_figures/univ/OR_pest1.pdf", height = 7, width = 13)
par(mar = c(10, 2, 2, 5))
plot(or_pest, ylim=myrange, xlim=c(1,length(or_pest)), type="n",
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
plot(or_pest, ylim=myrange,
     xaxt="n", yaxt="n",
     col=c(NA, mycolours[2], NA, mycolours[4],NA, mycolours[6], NA, mycolours[8],NA, mycolours[10],NA),
     pch = c(NA, 15, NA, 16,NA, 17, NA, 18,NA, 25,NA),
     lwd=1, ylab="", xlab="", cex=0.8)
x_pest <- 1:198
arrows(x0=x_pest, y0=up_pest, x1=x_pest, y1=lo_pest, code=3, angle=90, length=0.03, 
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
  labcol=ifelse(k<=5, "goldenrod", "lightpink3")
  axis(side=1, at=xseq[k], labels=mytmp, las=2, cex.axis=0.8, col = labcol, col.axis=labcol)
}
legend("topright", pch=c(15, 16, 17, 18, 25), 
       col=c(mycolours[c(2,4,6,8,10)]),
       legend=c(outcomes),
       cex=0.7, bg = "white", bty = "n")
dev.off()
