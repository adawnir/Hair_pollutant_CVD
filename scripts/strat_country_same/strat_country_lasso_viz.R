
### Sumemr Project --  Stratified analysis by country (same obs.): Stability selection LASSO Logistic Regression Visualisation
## Created by Vivian on 19 July

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
for (m in 1:23){
  arr <- c(paste0(rep(c("pest_de"), times = 15), "_", 
                  c("obesity", "hypertension", "diabetes", "Lipid_disorder", "MS"), 
                  rep(c(".f",".b",".l"), each=5)), 
           paste0(rep(c("pahs_de"), times = 8), "_", 
                  c("obesity", "hypertension", "Lipid_disorder", "MS"), 
                  rep(c(".b",".l"), each=4)))[m]
  
  ### Lasso
  lasso_out=readRDS(paste0("../results/strat_country_lasso_same/out_",arr,".rds")) # Load output
  assign(paste0("lasso_out_",arr),lasso_out) # Assign name
  lasso_hat_params=GetArgmax(lasso_out) # Extract calibrated pi
  assign(paste0("lasso_hat_params_",arr),lasso_hat_params) # Assign name
  lasso_selprop=readRDS(paste0("../results/strat_country_lasso_same/selprop_",arr,".rds")) # Load selection proportion
  assign(paste0("lasso_selprop_",arr),lasso_selprop) # Assign name
  lasso_beta=readRDS(paste0("../results/strat_country_lasso_same/average_beta_",arr,".rds")) # Load beta
  lasso_beta=ifelse(CalibratedStableRegression(lasso_out) == 1, lasso_beta, 0) # Shrink non-selected beta to zero
  lasso_beta=exp(lasso_beta) # Exponentiate to OR
  assign(paste0("lasso_beta_",arr),lasso_beta) # Assign name
}


##########################   selection proportion    ##########################

##### Reorder rows and transform to vector with empty values in between--
### Function
foo10 = function(x1, x2, x3, x4, x5, x6, x7, x8, x9, x10, order){
  x=data.frame(x1)
  x[,2]=x2[match(rownames(x),names(x2))]
  x[,3]=x3[match(rownames(x),names(x2))]
  x[,4]=x4[match(rownames(x),names(x2))]
  x[,5]=x5[match(rownames(x),names(x2))]
  x[,6]=x6[match(rownames(x),names(x2))]
  x[,7]=x7[match(rownames(x),names(x2))]
  x[,8]=x8[match(rownames(x),names(x2))]
  x[,9]=x9[match(rownames(x),names(x2))]
  x[,10]=x10[match(rownames(x),names(x2))]
  x = slice(x, match(order,rownames(x)))
  x=cbind(rep(NA,nrow(x)),x[,1],
          rep(NA, nrow(x)),x[,2],
          rep(NA, nrow(x)),x[,3],
          rep(NA, nrow(x)),x[,4],
          rep(NA, nrow(x)),x[,5],
          rep(NA,nrow(x)),x[,6],
          rep(NA, nrow(x)),x[,7],
          rep(NA, nrow(x)),x[,8],
          rep(NA, nrow(x)),x[,9],
          rep(NA, nrow(x)),x[,10],
          rep(NA,nrow(x)))
  x=as.vector(t(x))
}

foo15 = function(x1, x2, x3, x4, x5, x6, x7, x8, x9, x10, x11, x12, x13, x14, x15,order){
  x=data.frame(x1)
  x[,2]=x2[match(rownames(x),names(x2))]
  x[,3]=x3[match(rownames(x),names(x2))]
  x[,4]=x4[match(rownames(x),names(x2))]
  x[,5]=x5[match(rownames(x),names(x2))]
  x[,6]=x6[match(rownames(x),names(x2))]
  x[,7]=x7[match(rownames(x),names(x2))]
  x[,8]=x8[match(rownames(x),names(x2))]
  x[,9]=x9[match(rownames(x),names(x2))]
  x[,10]=x10[match(rownames(x),names(x2))]
  x[,11]=x11[match(rownames(x),names(x2))]
  x[,12]=x12[match(rownames(x),names(x2))]
  x[,13]=x13[match(rownames(x),names(x2))]
  x[,14]=x14[match(rownames(x),names(x2))]
  x[,15]=x15[match(rownames(x),names(x2))]
  x = slice(x, match(order,rownames(x)))
  x=cbind(rep(NA,nrow(x)),x[,1],
          rep(NA, nrow(x)),x[,2],
          rep(NA, nrow(x)),x[,3],
          rep(NA, nrow(x)),x[,4],
          rep(NA, nrow(x)),x[,5],
          rep(NA,nrow(x)),x[,6],
          rep(NA, nrow(x)),x[,7],
          rep(NA, nrow(x)),x[,8],
          rep(NA, nrow(x)),x[,9],
          rep(NA, nrow(x)),x[,10],
          rep(NA,nrow(x)),x[,11],
          rep(NA, nrow(x)),x[,12],
          rep(NA, nrow(x)),x[,13],
          rep(NA, nrow(x)),x[,14],
          rep(NA, nrow(x)),x[,15],
          rep(NA,nrow(x)))
  x=as.vector(t(x))
}

myorder <- names(lasso_selprop_pest_de_obesity.b)

lasso_selprop_pest <- foo15(lasso_selprop_pest_de_obesity.l, lasso_selprop_pest_de_obesity.b, lasso_selprop_pest_de_obesity.f,
                          lasso_selprop_pest_de_Lipid_disorder.l, lasso_selprop_pest_de_Lipid_disorder.b, lasso_selprop_pest_de_Lipid_disorder.f,
                          lasso_selprop_pest_de_hypertension.l, lasso_selprop_pest_de_hypertension.b, lasso_selprop_pest_de_hypertension.f,
                          lasso_selprop_pest_de_diabetes.l, lasso_selprop_pest_de_diabetes.b, lasso_selprop_pest_de_diabetes.f,
                          lasso_selprop_pest_de_MS.l, lasso_selprop_pest_de_MS.b, lasso_selprop_pest_de_MS.f,
                          myorder)

myorder.1 <- names(lasso_selprop_pahs_de_obesity.b)
lasso_selprop_pahs_de_diabetes.b <- lasso_selprop_pahs_de_diabetes.l <- lasso_selprop_pahs_de_Lipid_disorder.l

lasso_selprop_pahs <- foo10(lasso_selprop_pahs_de_obesity.l, lasso_selprop_pahs_de_obesity.b, 
                          lasso_selprop_pahs_de_Lipid_disorder.l, lasso_selprop_pahs_de_Lipid_disorder.b,
                          lasso_selprop_pahs_de_hypertension.l, lasso_selprop_pahs_de_hypertension.b,
                          lasso_selprop_pahs_de_diabetes.l, lasso_selprop_pahs_de_diabetes.b,
                          lasso_selprop_pahs_de_MS.l, lasso_selprop_pahs_de_MS.b,
                          myorder.1)


##### Plot --
### Pest
labels_pest <- c(colnames(pest)[1:4], "p,p-DDT", "p,p-DDE", 
                 "Alpha-endosulfan", "Beta-endosulfan",
                 "Beta-HCH", "Gamma-HCH",
                 colnames(pest)[11:33])

#mycolours = brewer.pal(10, "Paired")
colors1 <- RColorBrewer::brewer.pal(10, "RdBu")[c(7, 9:10)]
colors2 <- RColorBrewer::brewer.pal(10, "PRGn")[c(7, 9:10)]
colors3 <- colorspace::diverge_hsv(12)[c(9, 11:12)]
colors4 <-  RColorBrewer::brewer.pal(5, "Oranges")[c(2,4:5)]
colors5 <-  RColorBrewer::brewer.pal(10, "PuOr")[c(7, 9:10)]
mycolours <- c(colors1, colors2, colors3, colors4, colors5)

outcomes <- paste(rep(c("Luxembourg:", "Belgium:", "France"), 5),
                  rep(c("Obesity", "Lipid disorder", "Hypertension", "Diabetes","Metabolic syndrome"), each = 3))

n=15 # Number of lines per variable
myspacing=n*2+1
xseq=seq((n+1),33*myspacing, by=myspacing)
background=TRUE
myrange <- c(0,1)
background_colour="darkgrey"

pi_pest <- c(lasso_hat_params_pest_de_obesity.l[2], lasso_hat_params_pest_de_obesity.b[2], lasso_hat_params_pest_de_obesity.f[2],
             lasso_hat_params_pest_de_Lipid_disorder.l[2], lasso_hat_params_pest_de_Lipid_disorder.b[2], lasso_hat_params_pest_de_Lipid_disorder.f[2],
             lasso_hat_params_pest_de_hypertension.l[2], lasso_hat_params_pest_de_hypertension.b[2], lasso_hat_params_pest_de_hypertension.f[2],
             lasso_hat_params_pest_de_diabetes.l[2], lasso_hat_params_pest_de_diabetes.b[2], lasso_hat_params_pest_de_diabetes.f[2], 
             lasso_hat_params_pest_de_MS.l[2],  lasso_hat_params_pest_de_MS.b[2],lasso_hat_params_pest_de_MS.f[2]) 

pdf("../figures/strat_country_lasso_same/lasso_selprop_pest.pdf", height = 7, width = 13)
par(mar = c(10, 2, 2, 6))
plot(lasso_selprop_pest , ylim=myrange, xlim=c(1,length(lasso_selprop_pest)), type="n",
     xaxt="n", yaxt="n",
     col=c(NA, mycolours[1], NA, mycolours[2],NA, mycolours[3], NA, mycolours[4],NA, mycolours[5],NA,
           mycolours[6], NA, mycolours[7],NA, mycolours[8], NA, mycolours[9],NA, mycolours[10],NA,
           mycolours[11], NA, mycolours[12],NA, mycolours[13], NA, mycolours[14],NA, mycolours[15],NA),
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
     col=c(NA, mycolours[1], NA, mycolours[2],NA, mycolours[3], NA, mycolours[4],NA, mycolours[5],NA,
           mycolours[6], NA, mycolours[7],NA, mycolours[8], NA, mycolours[9],NA, mycolours[10],NA,
           mycolours[11], NA, mycolours[12],NA, mycolours[13], NA, mycolours[14],NA, mycolours[15],NA),
     lwd=1.5, ylab="", xlab="")
abline(h=pi_pest, lty=4, lwd=0.8, col=mycolours)
axis(side=4, at=axTicks(2), cex.axis=0.7)
mtext(side=4, text="Selection Proportion", line=2, cex.lab=0.7)
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
       col=c(mycolours),
       legend=c(outcomes), ncol=5,
       cex=0.7, bg = "white", bty = "n")
dev.off()


### PAHs
labels_pahs <- c("Cotinine", "Nicotine", colnames(pahs)[3:13])
#labels_pest <- c("CountryBelgium", "CountryFrance", colnames(pest))

mycolours = brewer.pal(10, "Paired")

outcomes <- paste(rep(c("Luxembourg:", "Belgium:"), 5),
                  rep(c("Obesity", "Lipid disorder", "Hypertension", "Diabetes","Metabolic syndrome"), each = 2))

n=10 # Number of lines per variable
myspacing=n*2+1
xseq=seq((n+1),13*myspacing, by=myspacing)
background=TRUE
myrange <- c(0,1)
background_colour="darkgrey"

pi_pahs <- c(lasso_hat_params_pahs_de_obesity.l[2], lasso_hat_params_pahs_de_obesity.b[2],
             lasso_hat_params_pahs_de_Lipid_disorder.l[2], lasso_hat_params_pahs_de_Lipid_disorder.b[2],
             lasso_hat_params_pahs_de_hypertension.l[2], lasso_hat_params_pahs_de_hypertension.b[2],
             lasso_hat_params_pahs_de_MS.l[2],  lasso_hat_params_pahs_de_MS.b[2]) 

pdf("../figures/strat_country_lasso_same/lasso_selprop_pahs.pdf", height = 7, width = 13)
par(mar = c(10, 2, 2, 6))
plot(lasso_selprop_pahs , ylim=myrange, xlim=c(1,length(lasso_selprop_pahs)), type="n",
     xaxt="n", yaxt="n",
     col=c(NA, mycolours[1], NA, mycolours[2],NA, mycolours[3], NA, mycolours[4],NA, mycolours[5],NA,
           mycolours[6], NA, NA,NA, NA, NA, mycolours[9],NA, mycolours[10],NA),
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
     col=c(NA, mycolours[1], NA, mycolours[2],NA, mycolours[3], NA, mycolours[4],NA, mycolours[5],NA,
           mycolours[6], NA, NA,NA, NA, NA, mycolours[9],NA, mycolours[10],NA),
     lwd=1.5, ylab="", xlab="")
abline(h=pi_pahs, lty=4, lwd=0.8, col=mycolours[c(1:6, 9:10)])
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
  #labfont=ifelse(labcol=="black",1,2)
  axis(side=1, at=xseq[k], labels=mytmp, las=2, cex.axis=0.8)
}
legend("topright", lwd = 1,
       col=c(mycolours[1:6], NA, NA, mycolours[9:10]),
       legend=c(outcomes[1:6], "", "", outcomes[9:10]), ncol=5,
       cex=0.7, bg = "white", bty = "n")
dev.off()



##########################   Odds Ratio   ##########################

##### Reorder --
myorder <- names(lasso_beta_pest_de_diabetes.f)

lasso_beta_pest <- foo15(lasso_beta_pest_de_obesity.l, lasso_beta_pest_de_obesity.b, lasso_beta_pest_de_obesity.f, 
                          lasso_beta_pest_de_Lipid_disorder.l, lasso_beta_pest_de_Lipid_disorder.b, lasso_beta_pest_de_Lipid_disorder.f,
                          lasso_beta_pest_de_hypertension.l, lasso_beta_pest_de_hypertension.b, lasso_beta_pest_de_hypertension.f,
                          lasso_beta_pest_de_diabetes.l, lasso_beta_pest_de_diabetes.b, lasso_beta_pest_de_diabetes.f,
                          lasso_beta_pest_de_MS.l, lasso_beta_pest_de_MS.b, lasso_beta_pest_de_MS.f,
                          myorder)

myorder.1 <- names(lasso_beta_pahs_de_obesity.b)
lasso_beta_pahs_de_diabetes.b <- lasso_beta_pahs_de_diabetes.l <-  lasso_beta_pahs_de_Lipid_disorder.l
lasso_beta_pahs <- foo10(lasso_beta_pahs_de_obesity.l, lasso_beta_pahs_de_obesity.b, 
                       lasso_beta_pahs_de_Lipid_disorder.l, lasso_beta_pahs_de_Lipid_disorder.b,
                       lasso_beta_pahs_de_hypertension.l, lasso_beta_pahs_de_hypertension.b,
                       lasso_beta_pahs_de_diabetes.l, lasso_beta_pahs_de_diabetes.b,
                       lasso_beta_pahs_de_MS.l, lasso_beta_pahs_de_MS.b,
                       myorder.1)


##### Plot --
### Pest
#labels_pest <- names(lasso_beta_pest_de_obesity.b)
labels_pest <- c(colnames(pest)[1:4], "p,p-DDT", "p,p-DDE", 
                 "Alpha-endosulfan", "Beta-endosulfan",
                 "Beta-HCH", "Gamma-HCH",
                 colnames(pest)[11:33])

colors1 <- RColorBrewer::brewer.pal(10, "RdBu")[c(7, 9:10)]
colors2 <- RColorBrewer::brewer.pal(10, "PRGn")[c(7, 9:10)]
colors3 <- colorspace::diverge_hsv(12)[c(9, 11:12)]
colors4 <-  RColorBrewer::brewer.pal(5, "Oranges")[c(2,4:5)]
colors5 <-  RColorBrewer::brewer.pal(10, "PuOr")[c(7, 9:10)]
mycolours <- c(colors1, colors2, colors3, colors4, colors5)

outcomes <- paste(rep(c("Luxembourg:", "Belgium:", "France"), 5),
                  rep(c("Obesity", "Lipid disorder", "Hypertension", "Diabetes","Metabolic syndrome"), each = 3))


n=15 # Number of lines per variable
myspacing=n*2+1
xseq=seq((n+1),33*myspacing, by=myspacing)
background=TRUE
#myrange <- c(0,1)
background_colour="darkgrey"


pdf("../figures/strat_country_lasso_same/lasso_OR_pest.pdf", height = 7, width = 13)
par(mar = c(10, 2, 2, 6))
plot((lasso_beta_pest-1) , xlim=c(1,length(lasso_beta_pest)), type="n",
     xaxt="n", yaxt="n",
     col=c(NA, mycolours[1], NA, mycolours[2],NA, mycolours[3], NA, mycolours[4],NA, mycolours[5],NA,
           mycolours[6], NA, mycolours[7],NA, mycolours[8], NA, mycolours[9],NA, mycolours[10],NA,
           mycolours[11], NA, mycolours[12],NA, mycolours[13], NA, mycolours[14],NA, mycolours[15],NA),
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
     col=c(NA, mycolours[1], NA, mycolours[2],NA, mycolours[3], NA, mycolours[4],NA, mycolours[5],NA,
           mycolours[6], NA, mycolours[7],NA, mycolours[8], NA, mycolours[9],NA, mycolours[10],NA,
           mycolours[11], NA, mycolours[12],NA, mycolours[13], NA, mycolours[14],NA, mycolours[15],NA),
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
       col=c(mycolours),
       legend=c(outcomes), ncol=5,
       cex=0.7, bg = "white", bty = "n")
dev.off()


### PAHs

n=10 # Number of lines per variable
myspacing=n*2+1
xseq=seq((n+1),13*myspacing, by=myspacing)
background=TRUE
#myrange <- c(0,1)
background_colour="darkgrey"

mycolours = brewer.pal(10, "Paired")

pdf("../figures/strat_country_lasso_same/lasso_OR_pahs.pdf", height = 7, width = 13)
par(mar = c(12, 2, 2, 6))
plot((lasso_beta_pahs-1) , xlim=c(1,length(lasso_beta_pahs)), type="n",
     xaxt="n", yaxt="n",
     col=c(NA, mycolours[1], NA, mycolours[2],NA, mycolours[3], NA, mycolours[4],NA, mycolours[5],NA,
           mycolours[6], NA, NA, NA, NA, NA, mycolours[9],NA, mycolours[10],NA),
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
     col=c(NA, mycolours[1], NA, mycolours[2],NA, mycolours[3], NA, mycolours[4],NA, mycolours[5],NA,
           mycolours[6], NA, NA, NA, NA, NA, mycolours[9],NA, mycolours[10],NA),
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
       col=c(mycolours[1:6], NA, NA, mycolours[9:10]),
       legend=c(outcomes[1:6], "", "", outcomes[9:10]), ncol=5,
       cex=0.7, bg = "white", bty = "n")
dev.off()




