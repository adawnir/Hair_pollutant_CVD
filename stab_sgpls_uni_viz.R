

### Sumemr Project -- Stability selection sgPLS with univariate outcome 1000 iteration visualisation
## Created by Vivian on 12 August

rm(list=ls())
project_path="~/hair/scripts"
setwd(project_path)

##### Load packages --
library(pheatmap)
library(parallel)
library(mixOmics)
library(tidyverse)
library(sgPLS)
library(pROC)


source("penalisation_functions.R")
source("pls_functions.R")


pahs <- readRDS("../processed/imputation/imp_pahs.rds")
pest <- readRDS("../processed/imputation/imp_pest.rds")


##### Load data --
for (m in 1:10){
  arr=paste0(rep(c("pest_de", "pahs_de"), each = 5), "_", 
             c("obesity", "hypertension", "diabetes", "Lipid_disorder", "MS"))[m]
  
  ### sPLS
  out=readRDS(paste0("../results/sgpls_uni/out_",arr,".rds")) # Load output
  assign(paste0("out_",arr), out) # Assign name
  
  hat_params=GetArgmax(out) # Extract calibrated pi
  assign(paste0("hat_params_",arr), hat_params) # Assign name
  
  selprop=readRDS(paste0("../results/sgpls_uni/selprop_",arr,".rds")) # Load selection proportion
  selprop <- selprop[!is.na(selprop)]
  assign(paste0("selprop_",arr), selprop) # Assign name
  
  loading=readRDS(paste0("../results/sgpls_uni/average_load_",arr,".rds")) # Load beta
  loading=ifelse(CalibratedStableRegression(out) == 1, loading, 0) 
  assign(paste0("load_",arr), loading) # Assign name
}


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

myorder <- names(load_pest_de_obesity)

selprop_pest <- foo(selprop_pest_de_obesity, selprop_pest_de_Lipid_disorder,
                    selprop_pest_de_hypertension,
                    selprop_pest_de_diabetes, selprop_pest_de_MS, 
                    myorder)

myorder.1 <- names(load_pahs_de_obesity)
selprop_pahs <- foo(selprop_pahs_de_obesity, selprop_pahs_de_Lipid_disorder,
                    selprop_pahs_de_hypertension,
                    selprop_pahs_de_diabetes, selprop_pahs_de_MS, 
                    myorder.1)

##### Selection Plot --
### Pest
#labels_pest <- names(load_pest_de_obesity_TT)
labels_pest <- c("CountryBelgium", "CountryFrance",
                 colnames(pest)[1:4], 
                 "p,p-DDT", "p,p-DDE", 
                 colnames(pest)[7:33])

mycolours = brewer.pal(10, "Paired")
outcomes <- c("Obesity", "Lipid disorder", "Hypertension", "Diabetes","Metabolic syndrome")

n=5 # Number of lines per variable
myspacing=n*2+1
xseq=seq((n+1),35*myspacing, by=myspacing)
background=TRUE
myrange <- c(0,1)
background_colour="darkgrey"
Xgroup = c(2,5,6,15,24,28,30,32,33,34)

pi_pest <- c(hat_params_pest_de_obesity[2], hat_params_pest_de_Lipid_disorder[2],
             hat_params_pest_de_hypertension[2],hat_params_pest_de_diabetes[2], 
             hat_params_pest_de_MS[2]) 

cairo_pdf("../figures/sgpls_uni/spls_selprop_pest.pdf", height = 7, width = 13)
par(mar = c(12, 2, 2, 6))
plot(selprop_pest , ylim=myrange, xlim=c(1,length(selprop_pest)), type="n",
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
plot(selprop_pest, ylim=myrange, type="h",
     xaxt="n", yaxt="n",
     col=c(NA, mycolours[2], NA, mycolours[4],NA, mycolours[6], NA, mycolours[8],NA, mycolours[10],NA),
     lwd=1.5, ylab="", xlab="")
abline(h=pi_pest, lty=4, lwd=0.8, col=mycolours[c(2,4,6,8,10)])
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
  axis(side=1, at=xseq[k], labels=mytmp, las=2, cex.axis=0.8)
}

axis(1, at = c(0, Xgroup, 35) * 11, line = 7, labels = NA) 
axis(1, at = c(1, 3.5, 5.5, 10.5, 19.5, 26, 29, 31, 32.5, 33.5, 34.5) * 11, 
     labels = c(1:11), line = 7, tick = FALSE, cex.axis = 0.8)
legend("topright", lwd = 1,
       col=c(mycolours[c(2,4,6,8,10)]),
       legend=c(outcomes), ncol=2,
       cex=0.7, bg = "white", bty = "n")
dev.off()


### PAHs
#labels_pahs <- names(lasso_selprop_pahs_de_obesity)
labels_pahs <- c("CountryBelgium", "Cotinine", "Nicotine", colnames(pahs)[3:13])

outcomes <- c("Obesity", "Lipid disorder", "Hypertension", "Diabetes","Metabolic syndrome")

n=5 # Number of lines per variable
myspacing=n*2+1
xseq=seq((n+1),14*myspacing, by=myspacing)
background=TRUE
myrange <- c(0,1)
background_colour="darkgrey"

pi_pahs <- c(hat_params_pahs_de_obesity[2], hat_params_pahs_de_Lipid_disorder[2],
             hat_params_pahs_de_hypertension[2],hat_params_pahs_de_diabetes[2], 
             hat_params_pahs_de_MS[2]) 

pdf("../figures/spls_1000_uni/spls_selprop_pahs.pdf", height = 7, width = 13)
par(mar = c(10, 2, 2, 6))
plot(selprop_pahs , ylim=myrange, xlim=c(1,length(selprop_pahs)), type="n",
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
plot(selprop_pahs, ylim=myrange, type="h",
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
  axis(side=1, at=xseq[k], labels=mytmp, las=2, cex.axis=0.8)
}
legend("topright", lwd = 1,
       col=c(mycolours[c(2,4,6,8,10)]),
       legend=c(outcomes), ncol=2,
       cex=0.7, bg = "white", bty = "n")
dev.off()



##### Reorder --
myorder <- names(load_pest_de_diabetes)
load_pest <- foo(load_pest_de_obesity, load_pest_de_Lipid_disorder,
                 load_pest_de_hypertension,
                 load_pest_de_diabetes, load_pest_de_MS, 
                 myorder)

myorder.1 <- names(load_pahs_de_diabetes)
load_pahs <- foo(load_pahs_de_obesity, load_pahs_de_Lipid_disorder,
                 load_pahs_de_hypertension,
                 load_pahs_de_diabetes, load_pahs_de_MS, 
                 myorder.1)


##### Plots --

### PAHs
#labels_pahs <- names(load_pahs_de_diabetes_TT)
labels_pahs <- c("CountryBelgium", "Cotinine", "Nicotine", colnames(pahs)[3:13])

outcomes <- c("Obesity", "Lipid disorder", "Hypertension", "Diabetes","Metabolic syndrome")

n=5 # Number of lines per variable
myspacing=n*2+1
xseq=seq((n+1),14*myspacing, by=myspacing)
background=TRUE
#myrange <- c(0,1)
background_colour="darkgrey"


pdf("../figures/spls_1000_uni/spls_loading_pahs.pdf", height = 7, width = 13)
par(mar = c(12, 2, 2, 6))
plot((load_pahs) , xlim=c(1,length(load_pahs)), type="n",
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
plot((load_pahs), type="h",
     xaxt="n", yaxt="n",
     col=c(NA, mycolours[2], NA, mycolours[4],NA, mycolours[6], NA, mycolours[8],NA, mycolours[10],NA),
     lwd=1.5, ylab="", xlab="")
abline(h=0, lty=2, lwd = 0.8)
axis(side=4, at=axTicks(2), labels=axTicks(2), cex.axis=0.7)
#axis(side=4, at=axTicks(2), cex.axis=0.7)
mtext(side=4, text="Loading coefficients", line=2)
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



### Pest

outcomes <- c("Obesity", "Lipid disorder", "Hypertension", "Diabetes","Metabolic syndrome")

n=5 # Number of lines per variable
myspacing=n*2+1
xseq=seq((n+1),35*myspacing, by=myspacing)
background=TRUE
#myrange <- c(0,1)
background_colour="darkgrey"


cairo_pdf("../figures/sgpls_uni/sgpls_loading_pest.pdf", height = 5, width = 12)
par(mar = c(10, 2, 2, 6))  
plot((load_pest) , xlim=c(1,length(load_pest)), type="n",
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
plot((load_pest), type="h",
     xaxt="n", yaxt="n",
     col=c(NA, mycolours[2], NA, mycolours[4],NA, mycolours[6], NA, mycolours[8],NA, mycolours[10],NA),
     lwd=1.5, ylab="", xlab="")
abline(h=0, lty=2, lwd = 0.8)
axis(side=4, at=axTicks(2), labels=axTicks(2), cex.axis=0.7)
#axis(side=4, at=axTicks(2), cex.axis=0.7)
mtext(side=4, text="Loading coefficients", line=2)
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
  axis(side=1, at=xseq[k], labels=mytmp, las=2, cex.axis=0.8, col.axis = "gray50", col= "gray50")
}
abline(v = c(0, Xgroup, 35) * 11+0.5, lty = 4, col = "black")
axis(1, at = c(0, Xgroup, 35) * 11, line = 7, labels = NA) 
axis(1, at = c(1, 3.5, 5.5, 10.5, 19.5, 26, 29, 31, 32.5, 33.5, 34.5) * 11, 
     labels = c(1:11), line = 7, tick = FALSE, cex.axis = 0.8)
par(xpd=TRUE)
legend(390,-0.65,c(mapply(foo, myfamily, 1:10)), 
       cex=0.7, bg = "white", bty = "n")

dev.off()



##### Make legend --
annot <- readRDS("../data/pest_annotation.rds")
myfamily <- unique(annot$Family)[c(1:4,6,8:12)]

foo=function(x,n){
  x=paste0(n,": ",x)
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

cairo_pdf("../figures/sgpls_uni/legend_pest.pdf", height = 1, width = 3)
par(mar=c(0,0,0,0), mfrow=c(1,1), xpd=TRUE)
plot(NULL ,xaxt='n',yaxt='n',bty='n',ylab='',xlab='', xlim=0:1, ylim=0:1)

legend("top", 
       legend=c(mapply(foo, myfamily, 1:10)),
       bty="n", cex=0.7, ncol = 2)
dev.off()


