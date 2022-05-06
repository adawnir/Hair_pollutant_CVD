
### Sumemr Project -- Make legend
## Created by Vivian on 14 August

rm(list=ls())
project_path="~/hair/scripts"
setwd(project_path)

##### Loading packages
library(RColorBrewer)
library(tidyverse)
library(plotrix)
library(colorspace)
library(ggrepel)
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


#### Make legend
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
cairo_pdf("../figures/legends/legend_sex_pest.pdf", height = 3, width = 3)
par(mar=c(0,0,0,0), mfrow=c(1,1), xpd=TRUE)
plot(NULL ,xaxt='n',yaxt='n',bty='n',ylab='',xlab='', xlim=0:1, ylim=0:1)
legend("top", 
       legend=c("Variables (A)",mapply(foo, mylabel_pest[1:35], 1:35)),
       bty="n", cex=0.7, ncol = 2)
dev.off()

mylabel_pahs <- c("CountryBelgium","Cotinine", "Nicotine", colnames(pahs)[3:13])
cairo_pdf("../figures/legends/legend_sex_pahs.pdf", height = 3, width = 3)
par(mar=c(0,0,0,0), mfrow=c(1,1), xpd=TRUE)
plot(NULL ,xaxt='n',yaxt='n',bty='n',ylab='',xlab='', xlim=0:1, ylim=0:1)
legend("top", 
       legend=c("Variables (B)", mapply(foo, mylabel_pahs, 1:14)),
       bty="n", cex=0.7)
dev.off()


mylabel_pest <- c(colnames(pest_imp)[1:4],
                  "p,p-DDT", "p,p-DDE", 
                  colnames(pest_imp)[7:33])
cairo_pdf("../figures/legends/legend_country_pest.pdf", height = 3, width = 3)
par(mar=c(0,0,0,0), mfrow=c(1,1), xpd=TRUE)
plot(NULL ,xaxt='n',yaxt='n',bty='n',ylab='',xlab='', xlim=0:1, ylim=0:1)
legend("top", 
       legend=c("Variables (A, B, D)",mapply(foo, mylabel_pest[1:33], 1:33)),
       bty="n", cex=0.7, ncol = 2)
dev.off()

mylabel_pahs <- c("Cotinine", "Nicotine", colnames(pahs)[3:13])
cairo_pdf("../figures/legends/legend_country_pahs.pdf", height = 3, width = 3)
par(mar=c(0,0,0,0), mfrow=c(1,1), xpd=TRUE)
plot(NULL ,xaxt='n',yaxt='n',bty='n',ylab='',xlab='', xlim=0:1, ylim=0:1)
legend("top", 
       legend=c("Variables (C)", mapply(foo, mylabel_pahs, 1:13)),
       bty="n", cex=0.7)
dev.off()
