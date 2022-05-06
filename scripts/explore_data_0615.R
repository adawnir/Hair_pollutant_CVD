

### Sumemr Project -- data exploration 
## Created by Vivian on 15 June 

rm(list=ls())
project_path="~/hair/scripts"
setwd(project_path)


##### Load data
mydata_pahs <- read.csv("../data/Dataset_PAHs.csv")
names(mydata_pahs) <- mydata_pahs %>% slice(1) %>% unlist()
mydata_pahs <- mydata_pahs %>% slice(-c(1:2))
dim(mydata_pahs)   # 682  28
str(mydata_pahs)

mydata <- read.csv("../data/Dataset_PesticidesPCBBDEs.csv")
names(mydata) <- mydata %>% slice(1) %>% unlist()
mydata <- mydata %>% slice(-c(1:2))
dim(mydata)    # 1096   48
str(mydata)


##### Check two datasets
### change class
options(digits=20)

pahs_tmp1 = NULL
for (i in 5:28) {
  tmp <- mydata_pahs[,i]
  tmp1 <- as.numeric(as.character(tmp))
  pahs_tmp1 <- cbind(pahs_tmp1, tmp1)
}
pahs_tmp1 <- data.frame(pahs_tmp1)
colnames(pahs_tmp1) <- colnames(mydata_pahs)[5:28]
pahs_data <- cbind(mydata_pahs[,1:4], pahs_tmp1)

pahs_data$identifiant <- as.character(pahs_data$identifiant)
pahs_data$Country <- factor(pahs_data$Country)
pahs_data$`Gender of participant` <- factor(pahs_data$`Gender of participant`)
pahs_data$`Is the subject fasting?` <- factor(pahs_data$`Is the subject fasting?`)

pest_tmp1 = NULL
for (i in 5:48) {
  tmp <- mydata[,i]
  tmp1 <- as.numeric(as.character(tmp))
  pest_tmp1 <- cbind(pest_tmp1, tmp1)
}
pest_tmp1 <- data.frame(pest_tmp1)
colnames(pest_tmp1) <- colnames(mydata)[5:48]
pest_data <- cbind(mydata[,1:4], pest_tmp1)

pest_data$identifiant <- as.character(pest_data$identifiant)
pest_data$Country <- factor(pest_data$Country)
pest_data$`Gender of participant` <- factor(pest_data$`Gender of participant`)
pest_data$`Is the subject fasting?` <- factor(pest_data$`Is the subject fasting?`)


### overlapping
sum(pahs_data$identifiant %in% pest_data$identifiant)    # only 217 
ids <- pahs_data$identifiant[which(pahs_data$identifiant %in% pest_data$identifiant)]
pahs_over <- pahs_data[which(pahs_data$identifiant %in% ids),]
pest_over <- pest_data[which(pest_data$identifiant %in% ids),] %>%  dplyr::select(1,16:48)
overlap <- inner_join(pahs_over, pest_over, by = "identifiant")

saveRDS(overlap, "../data/overlap.rds")

## by country (belgium / Luxembourg)
pahs_B <- pahs_data[which(pahs_data$Country == "Belgium"),]
pest_B <- pest_data[which(pest_data$Country == "Belgium"),]
sum(pahs_B$identifiant %in% pest_B$identifiant)    # only 23

pahs_L <- pahs_data[which(pahs_data$Country == "Luxembourg"),]
pest_L <- pest_data[which(pest_data$Country == "Luxembourg"),]
sum(pahs_L$identifiant %in% pest_L$identifiant)    # only 194


##### Descriptive
### age
mean(pahs_data$Age)
sd(pahs_data$Age)
mean(pest_data$Age)
sd(pest_data$Age)
mean(overlap$Age)
sd(overlap$Age)

### categorical 
table(pahs_data$Country)
table(pest_data$Country)
table(overlap$Country)
table(pahs_data$`Gender of participant`)
table(pest_data$`Gender of participant`)
table(overlap$`Gender of participant`)
table(pahs_data$`Is the subject fasting?`)
table(pest_data$`Is the subject fasting?`)
table(overlap$`Is the subject fasting?`)

### CVDs related
summary(overlap[,c(6,8:15)])
sd(overlap[,15], na.rm = T)
sort(overlap$trig_conv, decreasing = T)[1:10]

### BMI 
summary(overlap$BMI)
overlap$bmi=ifelse(is.na(overlap$BMI),NA,
                 ifelse(overlap$BMI < 18.5,"Underweight",
                        ifelse(overlap$BMI >= 18.5 & overlap$BMI <25, "Normal",
                               ifelse(overlap$BMI >= 25 & overlap$BMI <30, "Pre-obesity",
                                      ifelse(overlap$BMI >= 30 & overlap$BMI <35, "Obesity class I",
                                             ifelse(overlap$BMI >= 35 & overlap$BMI <40, "Obesity class II",
                                                    "Obesity class III"))))))
table(overlap$bmi,useNA = "ifany")
prop.table(table(overlap$bmi,useNA = "ifany"))

summary(pahs_data$BMI)
pahs_data$bmi=ifelse(is.na(pahs_data$BMI),NA,
                   ifelse(pahs_data$BMI < 18.5,"Underweight",
                          ifelse(pahs_data$BMI >= 18.5 & pahs_data$BMI <25, "Normal",
                                 ifelse(pahs_data$BMI >= 25 & pahs_data$BMI <30, "Pre-obesity",
                                        ifelse(pahs_data$BMI >= 30 & pahs_data$BMI <35, "Obesity class I",
                                               ifelse(pahs_data$BMI >= 35 & pahs_data$BMI <40, "Obesity class II",
                                                      "Obesity class III"))))))
table(pahs_data$bmi,useNA = "ifany")
prop.table(table(pahs_data$bmi,useNA = "ifany"))

summary(pest_data$BMI)
pest_data$bmi=ifelse(is.na(pest_data$BMI),NA,
                     ifelse(pest_data$BMI < 18.5,"Underweight",
                            ifelse(pest_data$BMI >= 18.5 & pest_data$BMI <25, "Normal",
                                   ifelse(pest_data$BMI >= 25 & pest_data$BMI <30, "Pre-obesity",
                                          ifelse(pest_data$BMI >= 30 & pest_data$BMI <35, "Obesity class I",
                                                 ifelse(pest_data$BMI >= 35 & pest_data$BMI <40, "Obesity class II",
                                                        "Obesity class III"))))))
table(pest_data$bmi,useNA = "ifany")
prop.table(table(pest_data$bmi,useNA = "ifany"))



##### Missing rates --
pahs_na <- apply(pahs_data[16:28], 2, function(x) sum(is.na(x))) %>% sort()
pest_na <- apply(pest_data[16:48], 2, function(x) sum(is.na(x))) %>% sort()
over_na <- apply(overlap[16:61], 2, function(x) sum(is.na(x))) %>% sort()

pahs_na_1 <- data.frame(unlist(pahs_na))
pdf("../figures/missing_dist_pahs.pdf", width = 15, height = 12)
par(mar=c(18,6,2,1))
plot((pahs_na_1$unlist.pahs_na./682), type = "h", ylab="Missing rate", xlab="", xaxt="n", lwd=5, 
     col=ifelse(pahs_na_1$unlist.pahs_na.> 341, no="lightsalmon2", yes="gray70"),
     cex.lab=2, cex.axis = 2)
abline(h=0.5, lty=2, col="darkred", lwd = 6)
for (i in 1:13){
  axis(side=1, at=i, labels=rownames(pahs_na_1)[i], las=2, cex.axis = 1,
       col=ifelse(pahs_na_1$unlist.pahs_na.[i]>341, no="lightsalmon2", yes="gray70"),
       col.axis=ifelse(pahs_na_1$unlist.pahs_na.[i]>341, no="lightsalmon2", yes="gray70"))
}
dev.off()


pest_na_1 <- data.frame(unlist(pest_na))
pdf("../figures/missing_dist_pest.pdf", width = 15, height = 12)
par(mar=c(18,6,2,1))
plot((pest_na_1$unlist.pest_na./1096), type = "h", ylab="Missing rate", xlab="", xaxt="n", lwd=5, 
     col=ifelse(pest_na_1$unlist.pest_na.> 548, no="lightsalmon2", yes="gray70"),
     cex.lab=2, cex.axis = 2)
abline(h=0.5, lty=2, col="darkred", lwd = 6)
for (i in 1:33){
  axis(side=1, at=i, labels=rownames(pest_na_1)[i], las=2, cex.axis = 1,
       col=ifelse(pest_na_1$unlist.pest_na.[i]>548, no="lightsalmon2", yes="gray70"),
       col.axis=ifelse(pest_na_1$unlist.pest_na.[i]>548, no="lightsalmon2", yes="gray70"))
}
dev.off()


over_na_1 <- data.frame(unlist(over_na))
pdf("../figures/missing_dist_overlap.pdf", width = 12, height = 7.5)
par(mar=c(18,6,2,1))
plot((over_na_1$unlist.over_na./217), type = "h", ylab="Missing rate", xlab="", xaxt="n", lwd=5, 
     col=ifelse(over_na_1$unlist.over_na.> 108, no="lightsalmon2", yes="gray70"),
     cex.lab=1.5, cex.axis = 1.5)
abline(h=0.5, lty=2, col="darkred", lwd = 6)
for (i in 1:46){
  axis(side=1, at=i, labels=rownames(over_na_1)[i], las=2, cex.axis = 1, font = 2,
       col=ifelse(over_na_1$unlist.over_na.[i]>108, no="lightsalmon2", yes="gray70"),
       col.axis=ifelse(over_na_1$unlist.over_na.[i]>108, no="lightsalmon2", yes="gray70"))
}
dev.off()


no_of_na_ppl <- apply(overlap[16:61], 1, function(x) sum(!is.na(x)))
summary(unlist(no_of_na_ppl))   # 16-42

no_of_na_ppl_1 <- data.frame(unlist(no_of_na_ppl))
names(no_of_na_ppl_1) <- "no_of_detected"
scaleFUN <- function(x) sprintf("%.2f", x)

pdf("../figures/no_of_detected_overlap.pdf", width = 5, height = 6)
ggplot(no_of_na_ppl_1, aes(x=no_of_detected)) + 
  geom_histogram(aes(y=..density..), colour="black", fill="white", bins = 20)+
  geom_density(alpha=.5, fill="aquamarine4") +
  xlab("No. of chemicals detected in samples") +
  scale_y_continuous(labels=scaleFUN) +
  theme_bw() +
  theme(text = element_text(size = 15))
  
dev.off()


##### Distribution --
overlap_50per <- overlap[,-which((colSums(is.na(overlap))/217) > 0.5)]   # 24
pest_50per <- pest_data[,-which((colSums(is.na(pest_data))/1096) > 0.5)]   # 18
pahs_50per <- pahs_data[,-which((colSums(is.na(pahs_data))/682) > 0.5)]   # 18


pdf("../figures/distributions_pest_country.pdf")
par(mfrow = c(4, 3), oma = c(0.5,0.5,0,0.5), mar = c(1,1,1,1), mgp=c(0,0.2,0))
for (k in c(16:21,24:33)) {
  x0 = density(pest_50per[pest_50per$Country =="Belgium", k], na.rm = T)
  x1 = density(pest_50per[pest_50per$Country =="Luxembourg", k], na.rm = T)
  x2 = density(pest_50per[pest_50per$Country =="France", k], na.rm = T)
  plot(density(pest_50per[pest_50per$Country =="Belgium", k], na.rm = T), 
       col = "navy", xlab = "", xaxt="n",
       main = colnames(pest_50per)[k], ylab="", las = 1, cex.main = 0.5, cex.axis=0.5,
       ylim = range(c(x0$y, x1$y,  x2$y)), xlim = range(c(x0$x, x1$x,  x2$x)))
  lines(x1, col = "tomato")
  lines(x2, col = "aquamarine4")
  if (k == 16) {
    legend("topright", lwd = 1, col = c("navy", "tomato", "aquamarine4"), 
           legend = c("Belgium", "Luxembourg", "France"), cex = 0.6, bty = "n")
  }
}
for (k in 22:23) {
  x0 = density(pest_50per[pest_50per$Country =="Belgium", k], na.rm = T)
  x1 = density(pest_50per[pest_50per$Country =="Luxembourg", k], na.rm = T)
  plot(density(pest_50per[pest_50per$Country =="Belgium", k], na.rm = T), 
       col = "navy", xlab = "", xaxt="n",
       main = colnames(pest_50per)[k], ylab="", las = 1, cex.main = 0.5, cex.axis=0.5,
       ylim = range(c(x0$y, x1$y)), xlim = range(c(x0$x, x1$x)))
  lines(x1, col = "tomato")
}
dev.off()

pdf("../figures/distributions_pest_country_log2.pdf")
par(mfrow = c(4, 3), oma = c(0.5,0.5,0,0.5), mar = c(1,1,1,1), mgp=c(0,0.2,0))
for (k in c(16:21,24:33)) {
  x0 = density(log2(pest_50per[pest_50per$Country =="Belgium", k]), na.rm = T)
  x1 = density(log2(pest_50per[pest_50per$Country =="Luxembourg", k]), na.rm = T)
  x2 = density(log2(pest_50per[pest_50per$Country =="France", k]), na.rm = T)
  plot(density(log2(pest_50per[pest_50per$Country =="Belgium", k]), na.rm = T), 
       col = "navy", xlab = "", xaxt="n",
       main = colnames(pest_50per)[k], ylab="", las = 1, cex.main = 0.5, cex.axis=0.5,
       ylim = range(c(x0$y, x1$y,  x2$y)), xlim = range(c(x0$x, x1$x,  x2$x)))
  lines(x1, col = "tomato")
  lines(x2, col = "aquamarine4")
  if (k == 16) {
    legend("topright", lwd = 1, col = c("navy", "tomato", "aquamarine4"), 
           legend = c("Belgium", "Luxembourg", "France"), cex = 0.6, bty = "n")
  }
}
for (k in 22:23) {
  x0 = density(log2(pest_50per[pest_50per$Country =="Belgium", k]), na.rm = T)
  x1 = density(log2(pest_50per[pest_50per$Country =="Luxembourg", k]), na.rm = T)
  plot(density(log2(pest_50per[pest_50per$Country =="Belgium", k]), na.rm = T), 
       col = "navy", xlab = "", xaxt="n",
       main = colnames(pest_50per)[k], ylab="", las = 1, cex.main = 0.5, cex.axis=0.5,
       ylim = range(c(x0$y, x1$y)), xlim = range(c(x0$x, x1$x)))
  lines(x1, col = "tomato")
}
dev.off()


pdf("../figures/distributions_country.pdf")
par(mfrow = c(3, 4), oma = c(0.5,0.5,0,0.5), mar = c(1,1,1,1), mgp=c(0,0.2,0))
for (k in 16:39) {
  x0 = density(overlap_50per[overlap_50per$Country =="Belgium", k], na.rm = T)
  x1 = density(overlap_50per[overlap_50per$Country =="Luxembourg", k], na.rm = T)
  plot(density(overlap_50per[overlap_50per$Country =="Belgium", k], na.rm = T), 
       col = "navy", xlab = "", xaxt="n",
       main = colnames(overlap_50per)[k], ylab="", las = 1, cex.main = 0.5, cex.axis=0.5,
       ylim = range(c(x0$y, x1$y)), xlim = range(c(x0$x, x1$x)))
  lines(x1, col = "tomato")
  if (k == 18) {
    legend("topright", lwd = 1, col = c("navy", "tomato"), 
           legend = c("Belgium", "Luxembourg"), cex = 0.6, bty = "n")
  }
}
dev.off()

pdf("../figures/distributions_country_1.pdf")
par(mfrow = c(3, 4), oma = c(0.5,0.5,0,0.5), mar = c(1,1,1,1), mgp=c(0,0.2,0))
for (k in 16:27) {
  x0 = density(overlap_50per[overlap_50per$Country =="Belgium", k], na.rm = T)
  x1 = density(overlap_50per[overlap_50per$Country =="Luxembourg", k], na.rm = T)
  plot(density(overlap_50per[overlap_50per$Country =="Belgium", k], na.rm = T), 
       col = "navy", xlab = "", xaxt="n",
       main = colnames(overlap_50per)[k], ylab="", las = 1, cex.main = 0.5, cex.axis=0.5,
       ylim = range(c(x0$y, x1$y)), xlim = range(c(x0$x, x1$x)))
  lines(x1, col = "tomato")
  if (k == 16) {
    legend("topright", lwd = 1, col = c("navy", "tomato"), 
           legend = c("Belgium", "Luxembourg"), cex = 0.6, bty = "n")
  }
}
dev.off()

pdf("../figures/distributions_country_1_log2.pdf")
par(mfrow = c(3, 4), oma = c(0.5,0.5,0,0.5), mar = c(1,1,1,1), mgp=c(0,0.2,0))
for (k in 16:27) {
  x0 = density(log2(overlap_50per[overlap_50per$Country =="Belgium", k]), na.rm = T)
  x1 = density(log2(overlap_50per[overlap_50per$Country =="Luxembourg", k]), na.rm = T)
  plot(density(log2(overlap_50per[overlap_50per$Country =="Belgium", k]), na.rm = T), 
       col = "navy", xlab = "", xaxt="n",
       main = colnames(overlap_50per)[k], ylab="", las = 1, cex.main = 0.5, cex.axis=0.5,
       ylim = range(c(x0$y, x1$y)), xlim = range(c(x0$x, x1$x)))
  lines(x1, col = "tomato")
  if (k == 16) {
    legend("topright", lwd = 1, col = c("navy", "tomato"), 
           legend = c("Belgium", "Luxembourg"), cex = 0.6, bty = "n")
  }
}
dev.off()


pdf("../figures/distributions_country_2.pdf")
par(mfrow = c(3, 4), oma = c(0.5,0.5,0,0.5), mar = c(1,1,1,1), mgp=c(0,0.2,0))
for (k in 28:39) {
  x0 = density(overlap_50per[overlap_50per$Country =="Belgium", k], na.rm = T)
  x1 = density(overlap_50per[overlap_50per$Country =="Luxembourg", k], na.rm = T)
  plot(density(overlap_50per[overlap_50per$Country =="Belgium", k], na.rm = T), 
       col = "navy", xlab = "", xaxt="n",
       main = colnames(overlap_50per)[k], ylab="", las = 1, cex.main = 0.5, cex.axis=0.5,
       ylim = range(c(x0$y, x1$y)), xlim = range(c(x0$x, x1$x)))
  lines(x1, col = "tomato")
}
dev.off()

pdf("../figures/distributions_country_2_log2.pdf")
par(mfrow = c(3, 4), oma = c(0.5,0.5,0,0.5), mar = c(1,1,1,1), mgp=c(0,0.2,0))
for (k in 28:39) {
  x0 = density(log2(overlap_50per[overlap_50per$Country =="Belgium", k]), na.rm = T)
  x1 = density(log2(overlap_50per[overlap_50per$Country =="Luxembourg", k]), na.rm = T)
  plot(density(log2(overlap_50per[overlap_50per$Country =="Belgium", k]), na.rm = T), 
       col = "navy", xlab = "", xaxt="n",
       main = colnames(overlap_50per)[k], ylab="", las = 1, cex.main = 0.5, cex.axis=0.5,
       ylim = range(c(x0$y, x1$y)), xlim = range(c(x0$x, x1$x)))
  lines(x1, col = "tomato")
}
dev.off()


##### Correlation --
corr50per <- cor(overlap_50per[16:39], use="pairwise.complete.obs")
view(corr50per)

overlap_B <- overlap_50per[which(overlap_50per$Country == "Belgium"),]
corr50per_B <- cor(overlap_B[16:39], use="pairwise.complete.obs")
overlap_L <- overlap_50per[which(overlap_50per$Country == "Luxembourg"),]
corr50per_L <- cor(overlap_L[16:39], use="pairwise.complete.obs")

### add annotation
colnames(corr50per)
variable_cat_1=c(rep("Tabacco_related", 2),
                 rep("PAHs",3),
                 rep("PCBs", 1),
                 rep("Organochlorines", 4), 
                 rep("Others", 2),
                 rep("Metabolits_of_Organophosphorus", 4),
                 rep("Pyrethroid", 1),
                 rep("Metabolits_of_Pyrethroid", 2),
                 rep("Phenylpyrazole", 2),
                 rep("Others", 1),
                 rep("Carboxamide", 1),
                 rep("Dinitroaniline", 1))

sub_anno <- structure(list(classes = c(variable_cat_1)), .Names = "Family", row.names = c(colnames(corr50per)),
                      class = "data.frame")

### specify colours
ann_colors = list(Family = c(Tabacco_related = "palevioletred2", PAHs = "cadetblue1", 
                             PCBs= "mediumpurple1", Organochlorines = "yellow3",
                             Others = "tan", 
                             Metabolits_of_Organophosphorus = "turquoise3", 
                             Pyrethroid = "navyblue", Metabolits_of_Pyrethroid = "orange",
                             Phenylpyrazole = "coral2", Carboxamide = "violetred", 
                             Dinitroaniline = "springgreen2"))


### plot

pdf(("../figures/heatmap_50per_display.pdf"), width = 15, height = 12)
pheatmap(corr50per, na_col = "grey80", cluster_rows = T, cluster_cols = T, border=NA, 
         breaks = seq(-1, 1, length.out = 100),
        annotation_row = sub_anno, annotation_col = sub_anno,
        annotation_colors = ann_colors,
        display_numbers = T, number_color = "black", fontsize_number = 9)
dev.off()


pdf(("../figures/heatmap_50per_Belgium_display.pdf"), width = 15, height = 12)
pheatmap(corr50per_B, na_col = "grey80", cluster_rows = T, cluster_cols = T, border=NA, 
         breaks = seq(-1, 1, length.out = 100),
         annotation_row = sub_anno, annotation_col = sub_anno,
         annotation_colors = ann_colors,
         display_numbers = T, number_color = "black", fontsize_number = 9)
dev.off()


pdf(("../figures/heatmap_50per_Luxembourg_display.pdf"), width = 15, height = 12)
pheatmap(corr50per_L, na_col = "grey80", cluster_rows = T, cluster_cols = T, border=NA, 
         breaks = seq(-1, 1, length.out = 100),
         annotation_row = sub_anno, annotation_col = sub_anno,
         annotation_colors = ann_colors,
         display_numbers = T, number_color = "black", fontsize_number = 9)
dev.off()
