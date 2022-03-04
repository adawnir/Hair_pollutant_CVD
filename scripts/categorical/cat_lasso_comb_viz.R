
### Sumemr Project -- Stability selection LASSO Visualisation 
## Created by Vivian on 21 July 

rm(list=ls())
project_path="~/hair/scripts"
setwd(project_path)

##### Loading packages--
library(RColorBrewer)
library(tidyverse)
library(plotrix)
library(colorspace)
library(cowplot)
library(ggrepel)

source("penalisation_functions.R")

pahs <- readRDS("../processed/imputation/imp_pahs.rds")
pest <- readRDS("../processed/imputation/imp_pest.rds")

##### Load data --
for (m in 1:10){
  arr=paste0(rep(c("pest_comb_de", "pahs_comb_de"), each = 5), "_", 
             c("obesity", "hypertension", "diabetes", "LD", "MS"))[m]
  
  ### Lasso
  lasso_out=readRDS(paste0("../cat_results/lasso/comb/out_",arr,".rds")) # Load output
  assign(paste0("lasso_out_",arr),lasso_out) # Assign name
  lasso_hat_params=GetArgmax(lasso_out) # Extract calibrated pi
  assign(paste0("lasso_hat_params_",arr),lasso_hat_params) # Assign name
  lasso_selprop=readRDS(paste0("../cat_results/lasso/comb/selprop_",arr,".rds")) # Load selection proportion
  assign(paste0("lasso_selprop_",arr),lasso_selprop) # Assign name
  lasso_beta=readRDS(paste0("../cat_results/lasso/comb/average_beta_",arr,".rds")) # Load beta
  lasso_beta=ifelse(CalibratedStableRegression(lasso_out) == 1, lasso_beta, 0) # Shrink non-selected beta to zero
  lasso_beta=exp(lasso_beta) # Exponentiate to OR
  assign(paste0("lasso_beta_",arr),lasso_beta) # Assign name
}



##########################   selection proportion    ##########################

##### Reorder rows and transform to vector with empty values in between--
### Function
foo = function(x1, x2, x3, x4, x5, order){
  x=data.frame(x1)
  x[,2]=x2[match(rownames(x),names(x2))]
  x[,3]=x3[match(rownames(x),names(x2))]
  x[,4]=x4[match(rownames(x),names(x2))]
  x[,5]=x5[match(rownames(x),names(x2))]
  x = slice(x, match(order,rownames(x)))
  x=cbind(rep(NA,nrow(x)),x[,1],
          rep(NA, nrow(x)),x[,2],
          rep(NA, nrow(x)),x[,3],
          rep(NA, nrow(x)),x[,4],
          rep(NA, nrow(x)),x[,5],
          rep(NA,nrow(x)))
  x=as.vector(t(x))
}

myorder <- names(lasso_selprop_pest_comb_de_obesity)

lasso_selprop_pest <- foo(lasso_selprop_pest_comb_de_obesity, lasso_selprop_pest_comb_de_LD,
                          lasso_selprop_pest_comb_de_hypertension,
                          lasso_selprop_pest_comb_de_diabetes, lasso_selprop_pest_comb_de_MS, 
                          myorder)

myorder.1 <- names(lasso_selprop_pahs_comb_de_obesity)
lasso_selprop_pahs <- foo(lasso_selprop_pahs_comb_de_obesity, lasso_selprop_pahs_comb_de_LD,
                          lasso_selprop_pahs_comb_de_hypertension,
                          lasso_selprop_pahs_comb_de_diabetes, lasso_selprop_pahs_comb_de_MS, 
                          myorder.1)

##### Plot --
### Pest
names(lasso_selprop_pest_comb_de_obesity)
labels_pest <- c("CountryBelgium", "CountryFrance",
                 "PCB 138", "PCB 153", "PCB 180", "BDE 47", "p,p-DDT", "p,p-DDE", 
                 names(lasso_selprop_pest_comb_de_obesity)[9:15], "Carbofuran phenol",
                 names(lasso_selprop_pest_comb_de_obesity)[17:20], "2-IPP", 
                 names(lasso_selprop_pest_comb_de_obesity)[22:30], 
                 "3Me4NP", "Cl2CA", "3-PBA", "Fipronil sulfone", "Trifluralin")


mycolours = brewer.pal(10, "Paired")
#mycolours <- c("lightcoral","lightgoldenrod", "palegreen2","steelblue1", "lightslateblue")

outcomes <- c("Obesity", "Lipid disorder", "Hypertension", "Diabetes","Metabolic syndrome")

n=5 # Number of lines per variable
myspacing=n*2+1
xseq=seq((n+1),35*myspacing, by=myspacing)
background=TRUE
myrange <- c(0,1)
background_colour="darkgrey"

pi_pest <- c(lasso_hat_params_pest_comb_de_obesity[2], lasso_hat_params_pest_comb_de_LD[2],
             lasso_hat_params_pest_comb_de_hypertension[2],lasso_hat_params_pest_comb_de_diabetes[2], 
             lasso_hat_params_pest_comb_de_MS[2]) 

pdf("../cat_figures/lasso/comb/lasso_selprop_pest.pdf", height = 7, width = 13)
par(mar = c(10, 2, 2, 6))
plot(lasso_selprop_pest , ylim=myrange, xlim=c(1,length(lasso_selprop_pest)), type="n",
     xaxt="n", yaxt="n",
     col=c(NA, mycolours[1], NA, mycolours[3],NA, mycolours[5], NA, mycolours[7],NA, mycolours[9],NA),
     lwd=1, ylab="",xlab="")
xseqgreysep=c(min(xseq)-myspacing/2,apply(rbind(xseq[-1],xseq[-length(xseq)]),2,mean),max(xseq)+myspacing/2)
if (background){
  for (k in seq(1,length(xseqgreysep),by=2)){
    polygon(x=c(xseqgreysep[k],xseqgreysep[k+1],xseqgreysep[k+1],xseqgreysep[k]),
            y=c(-5,-5,15,15), col=lighten(background_colour,0.95), border=NA)
  }
  for (k in seq(2,length(xseqgreysep),by=2)){
    polygon(x=c(xseqgreysep[k],xseqgreysep[k+1],xseqgreysep[k+1],xseqgreysep[k]),
            y=c(-5,-5,15,15), col=lighten(background_colour,0.99), border=NA)
  }
  box()
}
par(new=TRUE)
plot(lasso_selprop_pest, ylim=myrange, type="h",
     xaxt="n", yaxt="n",
     col=c(NA, mycolours[2], NA, mycolours[4],NA, mycolours[6], NA, mycolours[8],NA, mycolours[10],NA),
     lwd=1.5, ylab="", xlab="")
abline(h=pi_pest, lty=4, lwd=0.8, col=mycolours[c(2,4,6,8,10)])
axis(side=4, at=axTicks(2), cex.axis=0.7)
mtext(side=4, text="Selection Proportion (LASSO)", line=2, cex.lab=0.7)
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
legend("topright", lwd = 1,
       col=c(mycolours[c(2,4,6,8,10)]),
       legend=c(outcomes), ncol=2,
       cex=0.7, bg = "white", bty = "n")
dev.off()


### PAHs
colnames(pahs)
names(lasso_selprop_pahs_comb_de_obesity)
labels_pahs <- c("CountryBelgium", colnames(pahs)[c(5:6,8:13)],
                 "Cotinine", colnames(pahs)[7], "Nicotine", colnames(pahs)[3:4])

outcomes <- c("Obesity", "Lipid disorder", "Hypertension", "Diabetes","Metabolic syndrome")

n=5 # Number of lines per variable
myspacing=n*2+1
xseq=seq((n+1),14*myspacing, by=myspacing)
background=TRUE
myrange <- c(0,1)
background_colour="darkgrey"

pi_pahs <- c(lasso_hat_params_pahs_comb_de_obesity[2], lasso_hat_params_pahs_comb_de_LD[2],
             lasso_hat_params_pahs_comb_de_hypertension[2],lasso_hat_params_pahs_comb_de_diabetes[2], 
             lasso_hat_params_pahs_comb_de_MS[2]) 

pdf("../cat_figures/lasso/comb/lasso_selprop_pahs.pdf", height = 7, width = 13)
par(mar = c(12, 2, 2, 6))
plot(lasso_selprop_pahs , ylim=myrange, xlim=c(1,length(lasso_selprop_pahs)), type="n",
     xaxt="n", yaxt="n",
     col=c(NA, mycolours[2], NA, mycolours[4],NA, mycolours[6], NA, mycolours[8],NA, mycolours[10],NA),
     lwd=1, ylab="",xlab="")
xseqgreysep=c(min(xseq)-myspacing/2,apply(rbind(xseq[-1],xseq[-length(xseq)]),2,mean),max(xseq)+myspacing/2)
if (background){
  for (k in seq(1,length(xseqgreysep),by=2)){
    polygon(x=c(xseqgreysep[k],xseqgreysep[k+1],xseqgreysep[k+1],xseqgreysep[k]),
            y=c(-5,-5,15,15), col=lighten(background_colour,0.95), border=NA)
  }
  for (k in seq(2,length(xseqgreysep),by=2)){
    polygon(x=c(xseqgreysep[k],xseqgreysep[k+1],xseqgreysep[k+1],xseqgreysep[k]),
            y=c(-5,-5,15,15), col=lighten(background_colour,0.99), border=NA)
  }
  box()
}
par(new=TRUE)
plot(lasso_selprop_pahs, ylim=myrange, type="h",
     xaxt="n", yaxt="n",
     col=c(NA, mycolours[2], NA, mycolours[4],NA, mycolours[6], NA, mycolours[8],NA, mycolours[10],NA),
     lwd=1.5, ylab="", xlab="")
abline(h=pi_pahs, lty=4, lwd=0.8, col=mycolours[c(2,4,6,8,10)])
axis(side=4, at=axTicks(2), cex.axis=0.7)
mtext(side=4, text="Selection Proportion", line=2, cex.lab=0.7)
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
  #labfont=ifelse(k<=length(xseq),2,2)
  axis(side=1, at=xseq[k], labels=mytmp, las=2, cex.axis=0.7)
}
legend("topright", lwd = 1,
       col=c(mycolours[c(2,4,6,8,10)]),
       legend=c(outcomes), ncol=2,
       cex=0.7, bg = "white", bty = "n")
dev.off()




##########################   Odds Ratio   ##########################

##### Reorder --
myorder <- names(lasso_beta_pest_comb_de_diabetes)

lasso_beta_pest <- foo(lasso_beta_pest_comb_de_obesity, lasso_beta_pest_comb_de_LD,
                          lasso_beta_pest_comb_de_hypertension,
                          lasso_beta_pest_comb_de_diabetes, lasso_beta_pest_comb_de_MS, 
                          myorder)

myorder.1 <- names(lasso_beta_pahs_comb_de_obesity)
lasso_beta_pahs <- foo(lasso_beta_pahs_comb_de_obesity, lasso_beta_pahs_comb_de_LD,
                       lasso_beta_pahs_comb_de_hypertension,
                       lasso_beta_pahs_comb_de_diabetes, lasso_beta_pahs_comb_de_MS, 
                       myorder.1)


##### Plots --

### PAHs
outcomes <- c("Obesity", "Lipid disorder", "Hypertension", "Diabetes","Metabolic syndrome")

n=5 # Number of lines per variable
myspacing=n*2+1
xseq=seq((n+1),14*myspacing, by=myspacing)
background=TRUE
#myrange <- c(0,1)
background_colour="darkgrey"


pdf("../cat_figures/lasso/comb/lasso_OR_pahs.pdf", height = 7, width = 13)
par(mar = c(12, 2, 2, 6))
plot((lasso_beta_pahs-1) , xlim=c(1,length(lasso_beta_pahs)), type="n",
     xaxt="n", yaxt="n",
     col=c(NA, mycolours[2], NA, mycolours[4],NA, mycolours[6], NA, mycolours[8],NA, mycolours[10],NA),
     lwd=1, ylab="",xlab="")
xseqgreysep=c(min(xseq)-myspacing/2,apply(rbind(xseq[-1],xseq[-length(xseq)]),2,mean),max(xseq)+myspacing/2)
if (background){
  for (k in seq(1,length(xseqgreysep),by=2)){
    polygon(x=c(xseqgreysep[k],xseqgreysep[k+1],xseqgreysep[k+1],xseqgreysep[k]),
            y=c(-5,-5,15,15), col=lighten(background_colour,0.95), border=NA)
  }
  for (k in seq(2,length(xseqgreysep),by=2)){
    polygon(x=c(xseqgreysep[k],xseqgreysep[k+1],xseqgreysep[k+1],xseqgreysep[k]),
            y=c(-5,-5,15,15), col=lighten(background_colour,0.99), border=NA)
  }
  box()
}
par(new=TRUE)
plot((lasso_beta_pahs-1), type="h",
     xaxt="n", yaxt="n",
     col=c(NA, mycolours[2], NA, mycolours[4],NA, mycolours[6], NA, mycolours[8],NA, mycolours[10],NA),
     lwd=1.5, ylab="", xlab="")
abline(h=0, lty=2, lwd = 0.8)
axis(side=4, at=axTicks(2), labels=axTicks(2)+1, cex.axis=0.7)
#axis(side=4, at=axTicks(2), cex.axis=0.7)
mtext(side=4, text="Odds Ratio", line=2)
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
  labcol=ifelse(k<=9, "black", ifelse(k>9 & k<=11, "goldenrod", "lightpink3"))
  #labcol=ifelse((manhat_pahs_1[k,1]>=bonf & manhat[k,2]>=bonf), mycolours[3],
  #ifelse(manhat[k,1]>=bonf,mycolours[1],
  #ifelse(manhat[k,2]>=bonf, mycolours[2],"black")))
  #if(is.na(labcol)){
  #labcol=ifelse(manhat[k,1]>=bonf,mycolours[1],
  #ifelse(manhat[k,2]>=bonf, mycolours[2],"black"))
  #}
  #labfont=ifelse(labcol=="black",1,2)
  axis(side=1, at=xseq[k], labels=mytmp, las=2, cex.axis=0.7, col = labcol, col.axis=labcol)
}
legend("topright", lwd = 1,
       col=c(mycolours[c(2,4,6,8,10)]),
       legend=c(outcomes), ncol=2,
       cex=0.7, bg = "white", bty = "n")
dev.off()



### Pest

outcomes <- c("Obesity", "Lipid disorder", "Hypertension", "Diabetes","Metabolic syndrome")

n=5 # Number of lines per variable
myspacing=n*2+1
xseq=seq((n+1),35*myspacing, by=myspacing)
background=TRUE
#myrange <- c(0,1)
background_colour="darkgrey"


pdf("../cat_figures/lasso/comb/lasso_OR_pest.pdf", height = 7, width = 13)
par(mar = c(10, 2, 2, 6))
plot((lasso_beta_pest-1) , xlim=c(1,length(lasso_beta_pest)), type="n",
     xaxt="n", yaxt="n",
     col=c(NA, mycolours[2], NA, mycolours[4],NA, mycolours[6], NA, mycolours[8],NA, mycolours[10],NA),
     lwd=1, ylab="",xlab="")
xseqgreysep=c(min(xseq)-myspacing/2,apply(rbind(xseq[-1],xseq[-length(xseq)]),2,mean),max(xseq)+myspacing/2)
if (background){
  for (k in seq(1,length(xseqgreysep),by=2)){
    polygon(x=c(xseqgreysep[k],xseqgreysep[k+1],xseqgreysep[k+1],xseqgreysep[k]),
            y=c(-5,-5,15,15), col=lighten(background_colour,0.95), border=NA)
  }
  for (k in seq(2,length(xseqgreysep),by=2)){
    polygon(x=c(xseqgreysep[k],xseqgreysep[k+1],xseqgreysep[k+1],xseqgreysep[k]),
            y=c(-5,-5,15,15), col=lighten(background_colour,0.99), border=NA)
  }
  box()
}
par(new=TRUE)
plot((lasso_beta_pest-1), type="h",
     xaxt="n", yaxt="n",
     col=c(NA, mycolours[2], NA, mycolours[4],NA, mycolours[6], NA, mycolours[8],NA, mycolours[10],NA),
     lwd=1.5, ylab="", xlab="")
abline(h=0, lty=2, lwd = 0.8)
axis(side=4, at=axTicks(2), labels=axTicks(2)+1, cex.axis=0.7)
#axis(side=4, at=axTicks(2), cex.axis=0.7)
mtext(side=4, text="Odds Ratio", line=2)
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
  labcol=ifelse(k<=17, "black", ifelse(k>17 & k<=22, "goldenrod", "lightpink3"))
  #labcol=ifelse((manhat_pahs_1[k,1]>=bonf & manhat[k,2]>=bonf), mycolours[3],
  #ifelse(manhat[k,1]>=bonf,mycolours[1],
  #ifelse(manhat[k,2]>=bonf, mycolours[2],"black")))
  #if(is.na(labcol)){
  #labcol=ifelse(manhat[k,1]>=bonf,mycolours[1],
  #ifelse(manhat[k,2]>=bonf, mycolours[2],"black"))
  #}
  #labfont=ifelse(labcol=="black",1,2)
  axis(side=1, at=xseq[k], labels=mytmp, las=2, cex.axis=0.8, col = labcol, col.axis=labcol)
}
legend("topright", lwd = 1,
       col=c(mycolours[c(2,4,6,8,10)]),
       legend=c(outcomes), ncol=2,
       cex=0.7, bg = "white", bty = "n")
dev.off()

