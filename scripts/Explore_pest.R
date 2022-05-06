
### Sumemr Project -- data exploration (on pesticide)
## Created by Vivian on 17 June 

rm(list=ls())
project_path="~/hair/scripts"
setwd(project_path)


library(RColorBrewer)

##### Load data -- 
mydata <- read.csv("../data/Dataset_PesticidesPCBBDEs.csv")
names(mydata) <- mydata %>% slice(1) %>% unlist()
mydata <- mydata %>% slice(-c(1:2))
dim(mydata)    # 1096   48
str(mydata)


##### Check datasets -- 
### change class
options(digits=20)

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

saveRDS(pest_data, "../data/PesticidesPCBBDEs.rds")


##### Load data --
mydata <- readRDS("../data/PesticidesPCBBDEs.rds")
str(mydata)
summary(mydata[1:15])

##### Descriptive --
mydata1 <- mydata[complete.cases(mydata[ , 6:15]),]  # 1065

### age
mean(mydata1$Age)
sd(mydata1$Age)

### CVD related
summary(mydata1[,c(6,8:15)])
sd(mydata1[,15], na.rm = T)


### categorical 
table(mydata1$Country)
prop.table(table(mydata1$Country))
table(mydata1$`Gender of participant`)
prop.table(table(mydata1$`Gender of participant`))
table(mydata1$`Is the subject fasting?`)


### BMI
summary(mydata1$BMI)
mydata1$bmi=ifelse(is.na(mydata1$BMI),NA,
                     ifelse(mydata1$BMI < 18.5,"Underweight",
                            ifelse(mydata1$BMI >= 18.5 & mydata1$BMI <25, "Normal",
                                   ifelse(mydata1$BMI >= 25 & mydata1$BMI <30, "Pre-obesity",
                                          ifelse(mydata1$BMI >= 30 & mydata1$BMI <35, "Obesity class I",
                                                 ifelse(mydata1$BMI >= 35 & mydata1$BMI <40, "Obesity class II",
                                                        "Obesity class III"))))))
table(mydata1$bmi)
prop.table(table(mydata1$bmi))

### WC
summary(mydata1$`Waist measurement`)
mydata1$WC=ifelse(is.na(mydata1$`Waist measurement`),NA,
                  ifelse(mydata1$`Waist measurement` < 102 & mydata1$`Gender of participant` == "M", "Normal",
                         ifelse(mydata1$`Waist measurement` >= 102 & mydata1$`Gender of participant` == "M", "High",
                                ifelse(mydata1$`Waist measurement` < 88 & mydata1$`Gender of participant` == "F", "Normal",
                                       "High"))))
table(mydata1$WC,useNA = "ifany")
prop.table(table(mydata1$WC,useNA = "ifany"))
table(mydata1$`Gender of participant`, mydata1$WC,useNA = "ifany")
prop.table(table(mydata1$`Gender of participant`, mydata1$WC,useNA = "ifany"), margin = 1)

### Hypertension
summary(mydata1$`Systolic blood pressure`)
summary(mydata1$`Diastolic blood pressure`)
mydata1$hypertension=ifelse(is.na(mydata1$`Systolic blood pressure`),NA,
                            ifelse(is.na(mydata1$`Diastolic blood pressure`),NA,
                                   ifelse(mydata1$`Systolic blood pressure` < 140 & mydata1$`Diastolic blood pressure` < 90, "Normal",
                                          "Hypertension")))
table(mydata1$hypertension)
prop.table(table(mydata1$hypertension))
table(mydata1$`Gender of participant`, mydata1$hypertension)
prop.table(table(mydata1$`Gender of participant`, mydata1$hypertension), margin = 1)

### Diabetes
summary(mydata1$`Plasma sugar`)
mydata1$diabetes=ifelse(is.na(mydata1$`Plasma sugar`),NA,
                        ifelse(mydata1$`Plasma sugar` < 126, "Normal",
                               "Diabetes"))
table(mydata1$diabetes)
prop.table(table(mydata1$diabetes))
table(mydata1$`Gender of participant`, mydata1$diabetes)
prop.table(table(mydata1$`Gender of participant`, mydata1$diabetes), margin = 1)

### Lipid disorder
#  TC(total cholesterol) ≥ 190 mg/dl, TG(triglycerides) ≥ 150 mg/dl, 
#  LDL-C ≥ 115 mg/dl, and HDLC < 40 mg/dl for men and < 46 mg/dl for women

mydata1$Lipid_disorder=ifelse(mydata1$Choltot_conv < 190 & mydata1$trig_conv < 150 & mydata1$ldl_conv < 115 & mydata1$hdl_conv >=40 & mydata1$`Gender of participant` == "M", "Normal",
                              ifelse(mydata1$Choltot_conv < 190 & mydata1$trig_conv < 150 & mydata1$ldl_conv < 115 & mydata1$hdl_conv >=46 & mydata1$`Gender of participant` == "F", "Normal",
                                     "Lipid_disorder"))

table(mydata1$Lipid_disorder)
prop.table(table(mydata1$Lipid_disorder))
table(mydata1$`Gender of participant`, mydata1$Lipid_disorder)
prop.table(table(mydata1$`Gender of participant`, mydata1$Lipid_disorder), margin = 1)

### Metabolic syndrome (MS) 
#  1) WC ≥ 102 cm for men and ≥ 88 cm for women; 2) raised concentration of TG ≥ 150 mg/dl 
#  3) reduced concentration of HDL-C < 40 mg/dl for men and < 50 mg/dl for women 
#  4) SBP was ≥ 130 mmHg, or DBP ≥ 85 mmHg; 5) FPG level ≥100 mg/dl

mydata1$MS_WC <- ifelse(mydata1$`Waist measurement` < 102 & mydata1$`Gender of participant` == "M", 0,
                        ifelse(mydata1$`Waist measurement` >= 102 & mydata1$`Gender of participant` == "M", 1,
                               ifelse(mydata1$`Waist measurement` < 88 & mydata1$`Gender of participant` == "F", 0,
                                      1)))
table(mydata1$MS_WC)

summary(mydata1$trig_conv)
mydata1$MS_TG <- ifelse(mydata1$trig_conv < 150 , 0, 1)
table(mydata1$MS_TG)

summary(mydata1$`Plasma sugar`)
mydata1$MS_FPG <- ifelse(mydata1$`Plasma sugar` < 100 , 0, 1)
table(mydata1$MS_FPG)

mydata1$MS_hypertension <- ifelse(mydata1$`Systolic blood pressure` < 130 & mydata1$`Diastolic blood pressure` < 85, 0, 1)
table(mydata1$MS_hypertension)

summary(mydata1$hdl_conv)
mydata1$MS_HDL <- ifelse(mydata1$hdl_conv >= 40 & mydata1$`Gender of participant` == "M", 0,
                         ifelse(mydata1$hdl_conv >= 50 & mydata1$`Gender of participant` == "F", 0,
                                1))
table(mydata1$MS_HDL)

mydata1$MS <- rowSums(mydata1[c("MS_hypertension", "MS_WC", "MS_HDL", "MS_FPG", "MS_TG")])
sum(mydata1$MS >=3)
table(mydata1$`Gender of participant`, (mydata1$MS >=3))
prop.table(table(mydata1$`Gender of participant`, (mydata1$MS >=3)), margin = 1)

saveRDS(mydata1, "../data/PesticidesPCBBDEs_complete.rds")


### Chemicals
summary(mydata[16:48])


##### Missing rates --
pest_na <- apply(mydata1[16:48], 2, function(x) sum(is.na(x))) %>% sort()

pest_na_1 <- data.frame(unlist(pest_na))
pdf("../figures/PesticidesPCBBDE/missing_dist_pest.pdf", width = 12, height = 7.5)
par(mar=c(15,6,2,1))
plot((pest_na_1$unlist.pest_na./1056), type = "h", ylab="Missing rate", xlab="", xaxt="n", lwd=5, 
     col=ifelse(pest_na_1$unlist.pest_na.> 528, no="lightsalmon2", yes="gray70"),
     cex.lab=2, cex.axis = 2)
abline(h=0.5, lty=2, col="darkred", lwd = 6)
for (i in 1:33){
  axis(side=1, at=i, labels=rownames(pest_na_1)[i], las=2, cex.axis = 1, font = 2,
       col=ifelse(pest_na_1$unlist.pest_na.[i]>528, no="lightsalmon2", yes="gray70"),
       col.axis=ifelse(pest_na_1$unlist.pest_na.[i]>528, no="lightsalmon2", yes="gray70"))
}
dev.off()



no_of_na_ppl <- apply(mydata1[16:48], 1, function(x) sum(!is.na(x)))
summary(unlist(no_of_na_ppl))   # 9-31 out of 33

no_of_na_ppl_1 <- data.frame(unlist(no_of_na_ppl))
names(no_of_na_ppl_1) <- "no_of_detected"
scaleFUN <- function(x) sprintf("%.2f", x)

pdf("../figures/PesticidesPCBBDE/no_of_detected_pest.pdf", width = 5, height = 6)
ggplot(no_of_na_ppl_1, aes(x=no_of_detected)) + 
  geom_histogram(aes(y=..density..), colour="black", fill="white", bins = 20)+
  geom_density(alpha=.5, fill="aquamarine4") +
  xlab("No. of chemicals detected in samples") +
  scale_y_continuous(labels=scaleFUN) +
  theme_bw() +
  theme(text = element_text(size = 15))

dev.off()


##### Distribution --
#pest_50per <- mydata1[,-which((colSums(is.na(mydata1))/1065) > 0.5)]   # 18

pest <- mydata1

pdf("../figures/PesticidesPCBBDE/distributions_pest_country.pdf")
par(mfrow = c(3, 4), oma = c(0.5,0.5,0,0.5), mar = c(1,1,1,1), mgp=c(0,0.2,0))
for (k in c(16:31, 33, 36:48)) {
  x0 = density(pest[pest$Country =="Belgium", k], na.rm = T)
  x1 = density(pest[pest$Country =="Luxembourg", k], na.rm = T)
  x2 = density(pest[pest$Country =="France", k], na.rm = T)
  plot(density(pest[pest$Country =="Belgium", k], na.rm = T), 
       col = "navy", xlab = "", xaxt="n",
       main = colnames(pest)[k], ylab="", las = 1, cex.main = 0.5, cex.axis=0.5,
       ylim = range(c(x0$y, x1$y,  x2$y)), xlim = range(c(x0$x, x1$x,  x2$x)))
  lines(x1, col = "tomato")
  lines(x2, col = "aquamarine4")
  if (k == 16) {
    legend("topright", lwd = 1, col = c("navy", "tomato", "aquamarine4"), 
           legend = c("Belgium", "Luxembourg", "France"), cex = 0.6, bty = "n")
  }
}
for (k in c(32, 34:35)) {
  x0 = density(pest[pest$Country =="Belgium", k], na.rm = T)
  x1 = density(pest[pest$Country =="Luxembourg", k], na.rm = T)
  plot(density(pest[pest$Country =="Belgium", k], na.rm = T), 
       col = "navy", xlab = "", xaxt="n",
       main = colnames(pest)[k], ylab="", las = 1, cex.main = 0.5, cex.axis=0.5,
       ylim = range(c(x0$y, x1$y)), xlim = range(c(x0$x, x1$x)))
  lines(x1, col = "tomato")
}
dev.off()


pdf("../figures/PesticidesPCBBDE/distributions_pest_country_log10_1.pdf")
par(mfrow = c(3, 4), oma = c(0.5,0.5,0,0.5), mar = c(1,1,1,1), mgp=c(0,0.2,0))
for (k in c(16:27)) {
  x0 = density(log10(pest[pest$Country =="Belgium", k]), na.rm = T)
  x1 = density(log10(pest[pest$Country =="Luxembourg", k]), na.rm = T)
  x2 = density(log10(pest[pest$Country =="France", k]), na.rm = T)
  plot(density(log10(pest[pest$Country =="Belgium", k]), na.rm = T), 
       col = "navy", xlab = "", xaxt="n",
       main = colnames(pest)[k], ylab="", las = 1, cex.main = 1, cex.axis=0.5,
       ylim = range(c(x0$y, x1$y,  x2$y)), xlim = range(c(x0$x, x1$x,  x2$x)))
  lines(x1, col = "tomato")
  lines(x2, col = "aquamarine4")
  if (k == 16) {
    legend("topright", lwd = 2, col = c("navy", "tomato", "aquamarine4"), 
           legend = c("Belgium", "Luxembourg", "France"), cex = 0.85, bty = "n")
  }
}
dev.off()

pdf("../figures/PesticidesPCBBDE/distributions_pest_country_log10_2.pdf")
par(mfrow = c(3, 4), oma = c(0.5,0.5,0,0.5), mar = c(1,1,1,1), mgp=c(0,0.2,0))
for (k in c(28:31,33, 36:42)) {
  x0 = density(log10(pest[pest$Country =="Belgium", k]), na.rm = T)
  x1 = density(log10(pest[pest$Country =="Luxembourg", k]), na.rm = T)
  x2 = density(log10(pest[pest$Country =="France", k]), na.rm = T)
  plot(density(log10(pest[pest$Country =="Belgium", k]), na.rm = T), 
       col = "navy", xlab = "", xaxt="n",
       main = colnames(pest)[k], ylab="", las = 1, cex.main = 1, cex.axis=0.5,
       ylim = range(c(x0$y, x1$y,  x2$y)), xlim = range(c(x0$x, x1$x,  x2$x)))
  lines(x1, col = "tomato")
  lines(x2, col = "aquamarine4")
  if (k == 29) {
    legend("topright", lwd = 2, col = c("navy", "tomato", "aquamarine4"), 
           legend = c("Belgium", "Luxembourg", "France"), cex = 0.8, bty = "n")
  }
}
dev.off()

pdf("../figures/PesticidesPCBBDE/distributions_pest_country_log10_3.pdf")
par(mfrow = c(3, 4), oma = c(0.5,0.5,0,0.5), mar = c(1,1,1,1), mgp=c(0,0.2,0))
for (k in 43:48) {
  x0 = density(log10(pest[pest$Country =="Belgium", k]), na.rm = T)
  x1 = density(log10(pest[pest$Country =="Luxembourg", k]), na.rm = T)
  x2 = density(log10(pest[pest$Country =="France", k]), na.rm = T)
  plot(density(log10(pest[pest$Country =="Belgium", k]), na.rm = T), 
       col = "navy", xlab = "", xaxt="n",
       main = colnames(pest)[k], ylab="", las = 1, cex.main = 1, cex.axis=0.5,
       ylim = range(c(x0$y, x1$y,  x2$y)), xlim = range(c(x0$x, x1$x,  x2$x)))
  lines(x1, col = "tomato")
  lines(x2, col = "aquamarine4")
  if (k == 43) {
    legend("topright", lwd = 2, col = c("navy", "tomato", "aquamarine4"), 
           legend = c("Belgium", "Luxembourg", "France"), cex = 0.8, bty = "n")
  }
}
for (k in c(32, 34:35)) {
  x0 = density(log10(pest[pest$Country =="Belgium", k]), na.rm = T)
  x1 = density(log10(pest[pest$Country =="Luxembourg", k]), na.rm = T)
  plot(density(log10(pest[pest$Country =="Belgium", k]), na.rm = T), 
       col = "navy", xlab = "", xaxt="n",
       main = colnames(pest)[k], ylab="", las = 1, cex.main = 1, cex.axis=0.5,
       ylim = range(c(x0$y, x1$y)), xlim = range(c(x0$x, x1$x)))
  lines(x1, col = "tomato")
}
dev.off()


##### Correlation --
#corr50per <- cor(pest_50per[16:33], use="pairwise.complete.obs")
#view(corr50per)

corr_pest <- cor(pest[16:48], use="pairwise.complete.obs")


pest_B <- pest_50per[which(pest_50per$Country == "Belgium"),]
corr_B <- cor(pest_50per_B[16:33], use="pairwise.complete.obs")
pest_L <- pest_50per[which(pest_50per$Country == "Luxembourg"),]
corr_L <- cor(pest_50per_L[16:33], use="pairwise.complete.obs")
pest_F <- pest_50per[which(pest_50per$Country == "France"),]
pest_F <- pest_50per_F[!names(pest_50per_F) %in% c("TCPy", "DEP")]
corr_F <- cor(pest_50per_F[16:31], use="pairwise.complete.obs")

pest_anno <- readRDS("../data/pest_annotation.rds")
table(pest_anno$Family)
variable_cat_1=c(rep("PCB", 3),
                 rep("BDE", 1),
                 rep("Organochlorines", 9),
                 rep("Organophosphorus", 3),
                 rep("Metabolits_of_Organophosphorus", 6),
                 rep("Pyrethroid", 2),
                 rep("Metabolits_of_Pyrethroid", 2),
                 rep("Phenylpyrazole", 2),
                 rep("Carbamates", 2),
                 rep("Carboxamide", 1),
                 rep("Dinitroaniline", 1),
                 rep("Oxadiazole"), 1)

sub_anno <- structure(list(classes = c(variable_cat_1)), .Names = "Family", row.names = c(colnames(corr_pest)),
                      class = "data.frame")

### specify colours
ann_colors = list(Family = c(PCB = "#A6CEE3", BDE = "#1F78B4", 
                             Organochlorines = "#B2DF8A",
                             Organophosphorus = "#33A02C", 
                             Metabolits_of_Organophosphorus = "#FB9A99", 
                             Pyrethroid = "#E31A1C", Metabolits_of_Pyrethroid = "#FDBF6F",
                             Phenylpyrazole = "#FF7F00", Carboxamide = "#CAB2D6", 
                             Dinitroaniline = "#6A3D9A", Carbamates = "#FFFF99",
                             Oxadiazole = "#B15928"))



### plot
pdf(("../figures/PesticidesPCBBDE/heatmap_pest_display.pdf"), width = 15, height = 12) 
pheatmap(corr_pest, na_col = "grey80", cluster_rows = T, cluster_cols = T, border=NA, 
         breaks = seq(-1, 1, length.out = 100),
         annotation_row = sub_anno, annotation_col = sub_anno,
         annotation_colors = ann_colors,
         display_numbers = T, number_color = "black", fontsize_number = 5)
dev.off()


pdf(("../figures/PesticidesPCBBDE/heatmap_pest_50per_display.pdf"), width = 15, height = 12) 
pheatmap(corr50per, na_col = "grey80", cluster_rows = T, cluster_cols = T, border=NA, 
         breaks = seq(-1, 1, length.out = 100),
         annotation_row = sub_anno, annotation_col = sub_anno,
         annotation_colors = ann_colors,
         display_numbers = T, number_color = "black", fontsize_number = 9)
dev.off()

pdf(("../figures/PesticidesPCBBDE/heatmap_pest_B_50per_display.pdf"), width = 15, height = 12) 
pheatmap(corr50per_B, na_col = "grey80", cluster_rows = T, cluster_cols = T, border=NA, 
         breaks = seq(-1, 1, length.out = 100),
         annotation_row = sub_anno, annotation_col = sub_anno,
         annotation_colors = ann_colors,
         display_numbers = T, number_color = "black", fontsize_number = 9)
dev.off()

pdf(("../figures/PesticidesPCBBDE/heatmap_pest_L_50per_display.pdf"), width = 15, height = 12) 
pheatmap(corr50per_L, na_col = "grey80", cluster_rows = T, cluster_cols = T, border=NA, 
         breaks = seq(-1, 1, length.out = 100),
         annotation_row = sub_anno, annotation_col = sub_anno,
         annotation_colors = ann_colors,
         display_numbers = T, number_color = "black", fontsize_number = 9)
dev.off()


pdf(("../figures/PesticidesPCBBDE/heatmap_pest_F_50per_display.pdf"), width = 15, height = 12) 
pheatmap(corr50per_F, na_col = "grey80", cluster_rows = T, cluster_cols = T, border=NA, 
         breaks = seq(-1, 1, length.out = 100),
         annotation_row = sub_anno, annotation_col = sub_anno,
         annotation_colors = ann_colors,
         display_numbers = T, number_color = "black", fontsize_number = 9)
dev.off()
