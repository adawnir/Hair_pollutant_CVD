# Randomly sample from truncated Gaussian distribution
rtruncnorm = function(n, mean = 0, sd = 1, min = -Inf, max = Inf){
  set.seed(202204)
  if (min > max) stop('Error: Truncation range is empty')
  x = runif(n, pnorm(min, mean, sd), pnorm(max, mean, sd))
  qnorm(x, mean, sd)
}

# Flexible formatting
flex_format = function(f, digits = 2, thresh = 0.1){
  ifelse(sapply(f, function(x) round(x,digits)) <= thresh,
         formatC(f, format="e", digits=2),
         formatC(f, format="f", digits=2))
}

ReformatScientificNotation=function(x){
  xsub=x[-which(is.na(x)|!grepl("e",x))]
  mysplit=strsplit(xsub, split="e")
  xsub=sapply(mysplit, FUN=function(tmp){paste0(tmp[1],"x10_[",as.numeric(tmp[2]),"]")})
  x[-which(is.na(x)|!grepl("e",x))]=xsub
  return(x)
}

SaveExcelWithSuperscripts=function(dt, filename){
  wb <- openxlsx::createWorkbook() # create workbook
  openxlsx::addWorksheet(wb, sheetName = "data") # add sheet
  openxlsx::writeData(wb, sheet=1, x=dt, xy=c(1, 1)) # write data on workbook
  
  for(i in grep("\\_\\[([A-z0-9\\s\\-]*)\\]", wb$sharedStrings)){
    # if empty string in superscript notation, then just remove the superscript notation
    if(grepl("\\_\\[\\]", wb$sharedStrings[[i]])){
      wb$sharedStrings[[i]] <- gsub("\\_\\[\\]", "", wb$sharedStrings[[i]])
      next # skip to next iteration
    }
    
    # insert additioanl formating in shared string
    wb$sharedStrings[[i]] <- gsub("<si>", "<si><r>", gsub("</si>", "</r></si>", wb$sharedStrings[[i]]))
    
    # find the "_[...]" pattern, remove brackets and udnerline and enclose the text with superscript format
    wb$sharedStrings[[i]] <- gsub("\\_\\[([A-z0-9\\s\\-]*)\\]", "</t></r><r><rPr><vertAlign val=\"superscript\"/></rPr><t xml:space=\"preserve\">\\1</t></r><r><t xml:space=\"preserve\">", wb$sharedStrings[[i]])
  }
  openxlsx::saveWorkbook(wb, file=filename, overwrite = TRUE)
}

ContinuousTest3=function(x, y){
  # By status
  out=NULL
  for (k in levels(y)){
    mymean=formatC(mean(10^x[y==k], na.rm=TRUE), format="f", digits=2)
    mysd=formatC(sd(10^x[y==k], na.rm=TRUE), format="f", digits=2)
    out=c(out, paste0(mymean, " (", mysd, ")"))
  }
  
  # one-way ANOVA
  mytest=summary(aov(x ~ y))
  mypval=formatC(mytest[[1]]$`Pr(>F)`[1], format="e", digits=2)
  out=c(out, mypval)
  
  return(out)
}

# Create PCA score plot
CreateScorePlot=function(mypca, filename=NULL, type, mycolours, comp=NULL, pch=19,
                         ellipse = FALSE, legend = TRUE, wide = FALSE){
  if (is.null(comp)){
    comp=matrix(c(1,2,1,3,2,3), byrow=TRUE, ncol=2)
  }
  if (legend){
    extra = ceiling(length(mycolours)/25)
  } else {
    extra = 0
  }
  if (!is.null(filename)){
    pdf(paste0(filename), width=15+2.5*extra, height=5) 
  }
  if (legend){
    par(mar = c(5,5,1,1))
    layout(matrix(c(1,2,3,4), 1, 4, byrow = TRUE), widths=c(2,2,2,extra))
  } else {
    par(mar = c(5,5,1,1), mfrow = c(1,3))
  }
  for (k in 1:nrow(comp)){
    xcomp=comp[k,1]
    ycomp=comp[k,2]
    S = mypca$ind$coord[,c(xcomp,ycomp)]
    plot(S, pch=pch, cex=2, las=1, type = "n",
         col=mycolours[type],
         xlab=paste0("Comp ",xcomp," (", round(ev[xcomp], digits=2), "% e.v.)"),
         ylab=paste0("Comp ",ycomp," (", round(ev[ycomp], digits=2), "% e.v.)"),
         cex.lab = 1.5)
    points(S, pch=pch, cex=1.2, col=mycolours[type])
    families = unique(type)
    if (ellipse){
      for (f in 1:length(families)){
        tmpmat=S[type==families[f],]
        if (!is.null(nrow(tmpmat))){
          if (nrow(tmpmat)>2){
            lines(ellipse::ellipse(cov(tmpmat), centre = c(mean(tmpmat[,1]),mean(tmpmat[,2])), level = 0.95),
                  lty = 3, col = mycolours[unique(type)[f]])
          }
        }
        # else {
        #   plotrix::draw.ellipse(tmpmat[1], tmpmat[2], 0.5, 0.5,lty = 3, border = mycolours[f])
        # }
      } 
    }
    abline(v=axTicks(1), lty=3, col="grey")
    abline(h=axTicks(2), lty=3, col="grey")
  }
  if (legend){
    par(mar = c(0,1,1,0))
    plot(0, 0, type = 'l', bty = 'n', xaxt = 'n', yaxt = 'n', xlab = "", ylab = "")
    legend("left", col=mycolours, ncol = ceiling(length(mycolours)/24),
           pch=pch, pt.cex=2, legend=names(mycolours), bty = "n", cex = 1.5)
  }
  if (!is.null(filename)){
    print("Saved to filename")
    dev.off()
  }
}
