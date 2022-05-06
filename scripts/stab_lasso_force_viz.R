
### Sumemr Project -- Stability selection LASSO Logistic Regression with forced variables visualisation
## Created by Vivian on 29 July

rm(list=ls())
project_path="~/hair/scripts"
setwd(project_path)

source("penalisation_functions.R")

library(RColorBrewer)
library(tidyverse)
library(plotrix)
library(colorspace)
library(cowplot)
library(ggrepel)

##########################   selection proportion    ##########################

pahs <- readRDS("../processed/imputation/imp_pahs.rds")
pest <- readRDS("../processed/imputation/imp_pest.rds")


##### Load selection proportion --
lasso_forced_hat_params=NULL
for (m in 1:10){
  arr=paste0(rep(c("pest1", "pahs1"), each = 5), "_", 
             c("obesity", "hypertension", "diabetes", "Lipid_disorder", "MS"))[m]
  ### Lasso
  lasso_forced_out=readRDS(paste0("../results/lasso_forced/out_",arr,".rds")) # Load output
  assign(paste0("lasso_forced_out_",arr),lasso_forced_out) # Assign name
  lasso_forced_hat_params=rbind(lasso_forced_hat_params, GetArgmax(lasso_forced_out)) # Extract calibrated pi
  lasso_forced_calib=sum(CalibratedStableRegression(lasso_forced_out)) # Number of selected variables
  assign(paste0("lasso_forced_calib_",arr),lasso_forced_calib) # Assign name
  
  lasso_selprop=readRDS(paste0("../results/lasso_forced/selprop_",arr,".rds")) # Load selection proportion
  assign(paste0("lasso_selprop_",arr),lasso_selprop) # Assign name
  
  lasso_forced_beta=readRDS(paste0("../results/lasso_forced/average_beta_",arr,".rds"))
  lasso_forced_beta=ifelse(CalibratedStableRegression(lasso_forced_out) == 1, lasso_forced_beta, 0) # Shrink non-selected beta to zero
  lasso_forced_beta=exp(lasso_forced_beta) # Exponentiate to OR
  assign(paste0("lasso_beta_",arr),lasso_forced_beta) 

  lasso_forced_roc=readRDS(paste0("../results/lasso_forced/roc_recalib_beta_",arr,".rds")) # Load roc (recalibrated)
  assign(paste0("lasso_forced_roc_",arr),lasso_forced_roc) # Assign name
  
  lasso_forced_auc=readRDS(paste0("../results/lasso_forced/auc_recalib_beta_",arr,".rds")) # Load auc (recalibrated)
  assign(paste0("lasso_forced_auc_",arr),lasso_forced_auc) # Assign name
}



##### Reorder selprop --
lung = lung[c(12:97)]
bladder = bladder[c(12:97)]

lasso_beta_pahs1_diabetes<- lasso_beta_pahs1_diabetes[c(3:16)]
lasso_beta_pahs1_obesity <- lasso_beta_pahs1_obesity[c(3:16)]
lasso_beta_pahs1_Lipid_disorder <- lasso_beta_pahs1_Lipid_disorder[c(3:16)]
lasso_beta_pahs1_hypertension <- lasso_beta_pahs1_hypertension[c(3:16)]
lasso_beta_pahs1_MS <- lasso_beta_pahs1_MS[c(3:16)]

lasso_beta_pest1_diabetes<- lasso_beta_pest1_diabetes[c(3:37)]
lasso_beta_pest1_obesity <- lasso_beta_pest1_obesity[c(3:37)]
lasso_beta_pest1_Lipid_disorder <- lasso_beta_pest1_Lipid_disorder[c(3:37)]
lasso_beta_pest1_hypertension <- lasso_beta_pest1_hypertension[c(3:37)]
lasso_beta_pest1_MS <- lasso_beta_pest1_MS[c(3:37)]


##### Transform --

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

### reorder
myorder <- names(lasso_beta_pest1_diabetes)

lasso_beta_pest <- foo(lasso_beta_pest1_obesity, lasso_beta_pest1_Lipid_disorder,
                       lasso_beta_pest1_hypertension,
                       lasso_beta_pest1_diabetes, lasso_beta_pest1_MS, 
                       myorder)

myorder.1 <- names(lasso_beta_pahs1_obesity)
lasso_beta_pahs <- foo(lasso_beta_pahs1_obesity, lasso_beta_pahs1_Lipid_disorder,
                       lasso_beta_pahs1_hypertension,
                       lasso_beta_pahs1_diabetes, lasso_beta_pahs1_MS, 
                       myorder.1)



##### Plot --
### Pest
labels_pest <- c("CountryBelgium", "CountryFrance",
                 colnames(pest)[1:4], 
                 "p,p-DDT", "p,p-DDE", 
                 "Alpha-endosulfan", "Beta-endosulfan",
                 "Beta-HCH", "Gamma-HCH",
                 colnames(pest)[11:33])

mycolours = brewer.pal(10, "Paired")
outcomes <- c("Obesity", "Lipid disorder", "Hypertension", "Diabetes","Metabolic syndrome")

n=5 # Number of lines per variable
myspacing=n*2+1
xseq=seq((n+1),35*myspacing, by=myspacing)
background=TRUE
myrange <- c(0,1)
background_colour="darkgrey"


pdf("../figures/lasso_forced/lasso_OR_pest.pdf", width = 13, height = 7)
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

  axis(side=1, at=xseq[k], labels=mytmp, las=2, cex.axis=0.8)
}
legend("topright", lwd = 1,
       col=c(mycolours[c(2,4,6,8,10)]),
       legend=c(outcomes), ncol=2,
       cex=0.7, bg = "white", bty = "n")
dev.off()


### PAHs

labels_pahs <- c("CountryBelgium", "Cotinine", "Nicotine", colnames(pahs)[3:13])

outcomes <- c("Obesity", "Lipid disorder", "Hypertension", "Diabetes","Metabolic syndrome")

n=5 # Number of lines per variable
myspacing=n*2+1
xseq=seq((n+1),14*myspacing, by=myspacing)
background=TRUE
#myrange <- c(0,1)
background_colour="darkgrey"


pdf("../figures/lasso_forced/lasso_OR_pahs.pdf", height = 7, width = 13)
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
  axis(side=1, at=xseq[k], labels=mytmp, las=2, cex.axis=0.7)
}
legend("topright", lwd = 1,
       col=c(mycolours[c(2,4,6,8,10)]),
       legend=c(outcomes), ncol=2,
       cex=0.7, bg = "white", bty = "n")
dev.off()
