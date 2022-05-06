
### Sumemr Project -- data exploration (on PAHs)
## Created by Vivian on 17 June 

rm(list=ls())
project_path="~/hair/scripts"
setwd(project_path)


##### Load data
mydata_pahs <- read.csv("../data/Dataset_PAHs.csv")
names(mydata_pahs) <- mydata_pahs %>% slice(1) %>% unlist()
mydata_pahs <- mydata_pahs %>% slice(-c(1:2))
dim(mydata_pahs)   # 682  28
str(mydata_pahs)


##### Check datasets
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

saveRDS(pahs_data, "../data/PAHs.rds")


##### Load data --
mydata <- readRDS("../data/PAHs.rds")
str(mydata)


##### Descriptive --
mydata1 <- mydata[complete.cases(mydata[ , 6:15]),]  # 670

### age
mean(mydata1$Age)
sd(mydata1$Age)

### CVD related

summary(mydata1[,c(6,8:15)])
sd(mydata1[,15], na.rm = T)
sort(mydata$trig_conv, decreasing = T)[1:10]

### categorical 
table(mydata1$Country)
table(mydata1$`Gender of participant`)
table(mydata1$`Is the subject fasting?`)
prop.table(table(mydata1$Country, mydata$`Gender of participant`), margin = 1)

### BMI
summary(mydata1$BMI)
mydata1$bmi=ifelse(is.na(mydata1$BMI),NA,
                  ifelse(mydata1$BMI < 18.5,"Underweight",
                         ifelse(mydata1$BMI >= 18.5 & mydata1$BMI <25, "Normal",
                                ifelse(mydata1$BMI >= 25 & mydata1$BMI <30, "Pre-obesity",
                                       ifelse(mydata1$BMI >= 30 & mydata1$BMI <35, "Obesity class I",
                                              ifelse(mydata1$BMI >= 35 & mydata1$BMI <40, "Obesity class II",
                                                     "Obesity class III"))))))
table(mydata1$bmi,useNA = "ifany")
prop.table(table(mydata1$bmi,useNA = "ifany"))
table(mydata1$`Gender of participant`, mydata1$bmi,useNA = "ifany")

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

saveRDS(mydata1, "../data/PAHs_complete.rds")

### Chemicals



##### Missing rates --
pahs_na <- apply(mydata1[16:28], 2, function(x) sum(is.na(x))) %>% sort()

pahs_na_1 <- data.frame(unlist(pahs_na))
pdf("../figures/PAHs/missing_dist_pahs.pdf", width = 12, height = 7.5)
par(mar=c(15,6,2,1))
plot((pahs_na_1$unlist.pahs_na./670), type = "h", ylab="Missing rate", xlab="", xaxt="n", lwd=5, 
     col=ifelse(pahs_na_1$unlist.pahs_na.> 335, no="lightsalmon2", yes="gray70"),
     cex.lab=2, cex.axis = 2)
abline(h=0.5, lty=2, col="darkred", lwd = 6)
for (i in 1:13){
  axis(side=1, at=i, labels=rownames(pahs_na_1)[i], las=2, cex.axis = 1, font = 2,
       col=ifelse(pahs_na_1$unlist.pahs_na.[i]>335, no="lightsalmon2", yes="gray70"),
       col.axis=ifelse(pahs_na_1$unlist.pahs_na.[i]>335, no="lightsalmon2", yes="gray70"))
}
dev.off()



no_of_na_ppl <- apply(mydata1[16:28], 1, function(x) sum(!is.na(x)))
summary(unlist(no_of_na_ppl))   # 3-12 out of 13

no_of_na_ppl_1 <- data.frame(unlist(no_of_na_ppl))
names(no_of_na_ppl_1) <- "no_of_detected"
scaleFUN <- function(x) sprintf("%.2f", x)

pdf("../figures/PAHs/no_of_detected_pahs.pdf", width = 5, height = 6)
ggplot(no_of_na_ppl_1, aes(x=no_of_detected)) + 
  geom_histogram(aes(y=..density..), colour="black", fill="white", bins = 10)+
  geom_density(alpha=.5, fill="aquamarine4") +
  xlab("No. of chemicals detected in samples") +
  scale_y_continuous(labels=scaleFUN) +
  theme_bw() +
  theme(text = element_text(size = 15))

dev.off()


##### Distribution --
#pahs_50per <- mydata1[,-which((colSums(is.na(mydata1))/670) > 0.5)]   # 5

pahs <- mydata1

pdf("../figures/PAHs/distributions_pahs_country.pdf")
par(mfrow = c(3, 3), oma = c(0.5,0.5,0,0.5), mar = c(1,1,1,1), mgp=c(0,0.2,0))
for (k in c(16:19, 21:28)) {
  x0 = density(pahs[pahs$Country =="Belgium", k], na.rm = T)
  x1 = density(pahs[pahs$Country =="Luxembourg", k], na.rm = T)
  plot(density(pahs[pahs$Country =="Belgium", k], na.rm = T), 
       col = "navy", xlab = "", xaxt="n",
       main = colnames(pahs)[k], ylab="", las = 1, cex.main = 1, cex.axis=0.5,
       ylim = range(c(x0$y, x1$y)), xlim = range(c(x0$x, x1$x)))
  lines(x1, col = "tomato")
  if (k == 16) {
    legend("topright", lwd = 2, col = c("navy", "tomato"), 
           legend = c("Belgium", "Luxembourg"), cex = 0.85, bty = "n")
  }
}
for (k in 20) {
  plot(density(pahs[pahs$Country =="Luxembourg", k], na.rm = T), 
       col = "tomato", xlab = "", xaxt="n",
       main = colnames(pahs)[k], ylab="", las = 1, cex.main = 1, cex.axis=0.5)
}
dev.off()


pdf("../figures/PAHs/distributions_pahs_country_log10_1.pdf")
par(mfrow = c(3, 3), oma = c(0.5,0.5,0,0.5), mar = c(1,1,1,1), mgp=c(0,0.2,0))
for (k in c(16:19, 21:25)) {
  x0 = density(log10(pahs[pahs$Country =="Belgium", k]), na.rm = T)
  x1 = density(log10(pahs[pahs$Country =="Luxembourg", k]), na.rm = T)
  plot(x0, 
       col = "navy", xlab = "", xaxt="n",
       main = colnames(pahs)[k], ylab="", las = 1, cex.main = 1, cex.axis=0.5,
       ylim = range(c(x0$y, x1$y)), xlim = range(c(x0$x, x1$x)))
  lines(x1, col = "tomato")
  if (k == 16) {
    legend("topright", lwd = 2, col = c("navy", "tomato"), 
           legend = c("Belgium", "Luxembourg"), cex = 0.85, bty = "n")
  }
}
dev.off()

pdf("../figures/PAHs/distributions_pahs_country_log10_2.pdf")
par(mfrow = c(3, 3), oma = c(0.5,0.5,0,0.5), mar = c(1,1,1,1), mgp=c(0,0.2,0))
for (k in c(26:28)) {
  x0 = density(log10(pahs[pahs$Country =="Belgium", k]), na.rm = T)
  x1 = density(log10(pahs[pahs$Country =="Luxembourg", k]), na.rm = T)
  plot(x0, 
       col = "navy", xlab = "", xaxt="n",
       main = colnames(pahs)[k], ylab="", las = 1, cex.main = 1, cex.axis=0.5,
       ylim = range(c(x0$y, x1$y)), xlim = range(c(x0$x, x1$x)))
  lines(x1, col = "tomato")
  if (k == 16) {
    legend("topright", lwd = 2, col = c("navy", "tomato"), 
           legend = c("Belgium", "Luxembourg"), cex = 0.85, bty = "n")
  }
}
for (k in 20) {
  plot(density(log10(pahs[pahs$Country =="Luxembourg", k]), na.rm = T), 
       col = "tomato", xlab = "", xaxt="n",
       main = colnames(pahs)[k], ylab="", las = 1, cex.main = 1, cex.axis=0.5)
}
dev.off()



##### Correlation --
#corr50per <- cor(pahs[16:20], use="pairwise.complete.obs")
#view(corr50per)

corr_pahs <- cor(pahs[16:28], use="pairwise.complete.obs")


pahs_B <- pahs[which(pahs$Country == "Belgium"),]
corr50per_B <- cor(pahs_B[16:20], use="pairwise.complete.obs")
pahs_L <- pahs[which(pahs$Country == "Luxembourg"),]
corr50per_L <- cor(pahs_L[16:20], use="pairwise.complete.obs")



variable_cat_1=c(rep("Tabacco_related", 2),
                 rep("PAHs", 11))

sub_anno <- structure(list(classes = c(variable_cat_1)), .Names = "Family", row.names = c(colnames(corr_pahs)),
                      class = "data.frame")

### specify colours
ann_colors = list(Family = c(Tabacco_related = "mediumpurple1",
                             PAHs = "yellow3"))


### plot
pdf(("../figures/PAHs/heatmap_pahs_display.pdf"), width = 15, height = 12) 
pheatmap(corr_pahs, na_col = "grey80", cluster_rows = T, cluster_cols = T, border=NA, 
         breaks = seq(-1, 1, length.out = 100),
         annotation_row = sub_anno, annotation_col = sub_anno,
         annotation_colors = ann_colors,
         display_numbers = T, number_color = "black", fontsize_number = 10)
dev.off()


pdf(("../figures/PAHs/heatmap_pahs_50per_display.pdf"), width = 15, height = 12) 
pheatmap(corr50per, na_col = "grey80", cluster_rows = T, cluster_cols = T, border=NA, 
         breaks = seq(-1, 1, length.out = 100),
         annotation_row = sub_anno, annotation_col = sub_anno,
         annotation_colors = ann_colors,
         display_numbers = T, number_color = "black", fontsize_number = 12)
dev.off()

pdf(("../figures/PAHs/heatmap_pahs_B_50per_display.pdf"), width = 15, height = 12) 
pheatmap(corr50per_B, na_col = "grey80", cluster_rows = T, cluster_cols = T, border=NA, 
         breaks = seq(-1, 1, length.out = 100),
         annotation_row = sub_anno, annotation_col = sub_anno,
         annotation_colors = ann_colors,
         display_numbers = T, number_color = "black", fontsize_number = 12)
dev.off()

pdf(("../figures/PAHs/heatmap_pahs_L_50per_display.pdf"), width = 15, height = 12) 
pheatmap(corr50per_L, na_col = "grey80", cluster_rows = T, cluster_cols = T, border=NA, 
         breaks = seq(-1, 1, length.out = 100),
         annotation_row = sub_anno, annotation_col = sub_anno,
         annotation_colors = ann_colors,
         display_numbers = T, number_color = "black", fontsize_number = 12)
dev.off()





