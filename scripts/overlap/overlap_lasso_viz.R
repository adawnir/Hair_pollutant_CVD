
### Sumemr Project -- Stability selection LASSO Visualisation on overlapping 
## Created by Vivian on 28 July 

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

over <- readRDS("../processed/imputation/imp_overlap.rds")


##### Load data --
for (m in 1:5){
  arr=paste0(rep("over_de", each = 5), "_", 
             c("obesity", "hypertension", "diabetes", "Lipid_disorder", "MS"))[m]
  
  ### Lasso
  lasso_out=readRDS(paste0("../results/overlap_lasso/out_",arr,".rds")) # Load output
  assign(paste0("lasso_out_",arr),lasso_out) # Assign name
  lasso_hat_params=GetArgmax(lasso_out) # Extract calibrated pi
  assign(paste0("lasso_hat_params_",arr),lasso_hat_params) # Assign name
  lasso_selprop=readRDS(paste0("../results/overlap_lasso/selprop_",arr,".rds")) # Load selection proportion
  assign(paste0("lasso_selprop_",arr),lasso_selprop) # Assign name
  lasso_beta=readRDS(paste0("../results/overlap_lasso/average_beta_",arr,".rds")) # Load beta
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

myorder <- names(lasso_selprop_over_de_obesity)

lasso_selprop_over <- foo(lasso_selprop_over_de_obesity, lasso_selprop_over_de_Lipid_disorder,
                          lasso_selprop_over_de_hypertension,
                          lasso_selprop_over_de_diabetes, lasso_selprop_over_de_MS, 
                          myorder)

##### Plot --
### over

labels_over <- c("CountryBelgium", "Cotinin", "Nicotine",
                 colnames(over)[3:17], 
                 "p,p-DDT", "p,p-DDE", 
                 "Alpha-endosulfan", "Beta-endosulfan",
                 "Beta-HCH", "Gamma-HCH",
                 colnames(over)[24:46])


mycolours = brewer.pal(10, "Paired")
#mycolours <- c("lightcoral","lightgoldenrod", "palegreen2","steelblue1", "lightslateblue")

outcomes <- c("Obesity", "Lipid disorder", "Hypertension", "Diabetes","Metabolic syndrome")

n=5 # Number of lines per variable
myspacing=n*2+1
xseq=seq((n+1),47*myspacing, by=myspacing)
background=TRUE
myrange <- c(0,1)
background_colour="darkgrey"

pi_over <- c(lasso_hat_params_over_de_obesity[2], lasso_hat_params_over_de_Lipid_disorder[2],
             lasso_hat_params_over_de_hypertension[2],lasso_hat_params_over_de_diabetes[2], 
             lasso_hat_params_over_de_MS[2]) 

pdf("../figures/overlap_lasso/lasso_selprop_over.pdf", height = 7, width = 13)
par(mar = c(10, 2, 2, 6))
plot(lasso_selprop_over , ylim=myrange, xlim=c(1,length(lasso_selprop_over)), type="n",
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
plot(lasso_selprop_over, ylim=myrange, type="h",
     xaxt="n", yaxt="n",
     col=c(NA, mycolours[2], NA, mycolours[4],NA, mycolours[6], NA, mycolours[8],NA, mycolours[10],NA),
     lwd=1.5, ylab="", xlab="")
abline(h=pi_over, lty=4, lwd=0.8, col=mycolours[c(2,4,6,8,10)])
axis(side=4, at=axTicks(2), cex.axis=0.7)
mtext(side=4, text="Selection Proportion (LASSO)", line=2, cex.lab=0.7)
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
  axis(side=1, at=xseq[k], labels=mytmp, las=2, cex.axis=0.8)
}
legend("topright", lwd = 1,
       col=c(mycolours[c(2,4,6,8,10)]),
       legend=c(outcomes), ncol=2,
       cex=0.7, bg = "white", bty = "n")
dev.off()





##########################   Odds Ratio   ##########################

##### Reorder --
myorder <- names(lasso_beta_over_de_diabetes)

lasso_beta_over <- foo(lasso_beta_over_de_obesity, lasso_beta_over_de_Lipid_disorder,
                       lasso_beta_over_de_hypertension,
                       lasso_beta_over_de_diabetes, lasso_beta_over_de_MS, 
                       myorder)



##### Plots --

### over

outcomes <- c("Obesity", "Lipid disorder", "Hypertension", "Diabetes","Metabolic syndrome")

n=5 # Number of lines per variable
myspacing=n*2+1
xseq=seq((n+1),47*myspacing, by=myspacing)
background=TRUE
#myrange <- c(0,1)
background_colour="darkgrey"


pdf("../figures/overlap_lasso/lasso_OR_over.pdf", height = 7, width = 13)
par(mar = c(12, 2, 2, 6))
plot((lasso_beta_over-1) , xlim=c(1,length(lasso_beta_over)), type="n",
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
plot((lasso_beta_over-1), type="h",
     xaxt="n", yaxt="n",
     col=c(NA, mycolours[2], NA, mycolours[4],NA, mycolours[6], NA, mycolours[8],NA, mycolours[10],NA),
     lwd=1.5, ylab="", xlab="")
abline(h=0, lty=2, lwd = 0.8)
axis(side=4, at=axTicks(2), labels=axTicks(2)+1, cex.axis=0.7)
#axis(side=4, at=axTicks(2), cex.axis=0.7)
mtext(side=4, text="Odds Ratio", line=2)
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
legend("topright", lwd = 1,
       col=c(mycolours[c(2,4,6,8,10)]),
       legend=c(outcomes), ncol=2,
       cex=0.7, bg = "white", bty = "n")
dev.off()

