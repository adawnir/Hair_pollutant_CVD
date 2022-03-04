
### Sumemr Project -- sample data exploration 
## Created by Vivian on 20 May 

rm(list=ls())
project_path="~/hair/scripts"
setwd(project_path)

library(openxlsx)
library(data.table)
install.packages("readxl")
library(readxl)
library(dplyr)

##### Load data
mydata <- read_excel("../data/Data_sample.xlsx")
dim(mydata)   # 91 161
view(mydata)
myannot <- read.csv("../data/annotation.csv")

##### Clean data --
### clear additional info.
mydata_1 <- mydata[c(1:39),]
saveRDS(mydata_1, "../data/hair_data.rds")
colnames(mydata_1)[113:117]

### check if figures below LOQ
loq <- mydata[40,8:161] %>% unlist()
loq_1 <- str_remove_all(loq, "[*]")
loq_1 <- as.numeric(loq_1)
saveRDS(loq_1, "../data/loqs.rds")

options(digits=15)
a <- lapply(mydata_1[,8:161], as.numeric) %>% data.frame()

newdata <- NULL
for (i in 1:154) {
  tmp <- a[,i]
  tmp1 <- replace(tmp, tmp < loq_1[i], NA)
  newdata <- cbind(newdata, tmp1)
}
newdata <- data.frame(newdata)

colnames(newdata) <- colnames(mydata_1)[8:161]

newdata_1 <- cbind(mydata_1[,1:7],newdata)

saveRDS(newdata_1, "../results/clear_data.rds")



##### Exploration --
function1 <- function(x){
  sum(x=="nd")
}

no_of_na <- apply(newdata, 2, function(x) sum(is.na(x))) %>% sort()
no_of_na_ppl <- apply(newdata, 1, function(x) sum(is.na(x)))

### distribution
no_of_com <- 154 - unlist(no_of_na_ppl)
summary(no_of_com)

pdf("../figures/no_of_com_dist.pdf")
png("../figures/no_of_com_dist.png", width=1200, height=1200, res = 250)
plot(density(no_of_com), col = "aquamarine4", xlab = "", ylab="", las = 1, 
     main = "No. of compounds detected in samples", lwd = 3)
dev.off()

no_of_com_1 <- data.frame(no_of_com)
pdf("../figures/no_of_com_dist_gg.pdf", width = 5, height = 5)
ggplot(no_of_com_1, aes(x=no_of_com)) + 
  geom_histogram(aes(y=..density..), colour="black", fill="white", bins = 13)+
  geom_density(alpha=.5, fill="aquamarine4") +
  xlab("No. of compounds detected in samples") +
  theme_bw()
dev.off()

no_of_na_1 <- data.frame(unlist(no_of_na))
pdf("../figures/missing_dist.pdf", width = 50, height = 25)
par(mar=c(20,6,1,1))
plot((no_of_na_1$unlist.no_of_na./39), type = "h", ylab="Missing rate", xlab="", xaxt="n", lwd=5, 
     col=ifelse(no_of_na_1$unlist.no_of_na.>20, no="lightsalmon2", yes="gray70"),
     cex.lab=3, cex.axis = 3)
abline(h=20/39, lty=2, col="darkred", lwd = 6)
for (i in 1:154){
  axis(side=1, at=i, labels=rownames(no_of_na_1)[i], las=2, cex.axis = 2,
       col=ifelse(no_of_na_1$unlist.no_of_na.[i]>20, no="lightsalmon2", yes="gray70"),
       col.axis=ifelse(no_of_na_1$unlist.no_of_na.[i]>20, no="lightsalmon2", yes="gray70"))
}
dev.off()

##### Correlation --
### remove na=39
not_all_na <- function(x) any(!is.na(x))    # 68
data_wo_na <- newdata %>% dplyr::select(where(not_all_na))

corr_all <- cor(data_wo_na, use="pairwise.complete.obs")
view(corr_all)


### remove na >= 38
data38 <- newdata[,-which(colSums(is.na(newdata))>=38)]   # 56

## distribution

pdf("../figures/distributions.pdf")
par(mfrow = c(5, 5), oma = c(0.5,0.5,0,0.5), mar = c(1,1,1,1), mgp=c(0,0.2,0))
for (k in 1:ncol(data38)) {
  plot(density(data38[, k], na.rm = T), col = "navy", xlab = "", xaxt="n",
       main = colnames(data38)[k], ylab="", las = 1, cex.main = 0.5, cex.axis=0.5)
}
dev.off()

pdf("../figures/distributions_log2.pdf")
par(mfrow = c(5, 5), oma = c(0.5,0.5,0,0.5), mar = c(1,1,1,1), mgp=c(0,0.2,0))
for (k in 1:ncol(data38)) {
  plot(density(log2(data38[, k]), na.rm = T), col = "navy", xlab = "", xaxt="n",
       main = colnames(data38)[k], ylab="", las = 1, cex.main = 0.5, cex.axis=0.5)
}
dev.off()

pdf("../figures/distributions_log10.pdf")
par(mfrow = c(6, 6), oma = c(0.5,0.5,0,0.5), mar = c(1,1,1,1), mgp=c(0,0.2,0))
for (k in 1:ncol(data38)) {
  plot(density(log10(data38[, k]), na.rm = T), col = "navy", xlab = "", xaxt="n",
       main = colnames(data38)[k], ylab="", las = 1, cex.main = 0.5, cex.axis=0.5)
}
dev.off()

corrMatrix <- cor(data38, use="pairwise.complete.obs")
view(corrMatrix)
upper_corr <- corrMatrix
upper_corr[lower.tri(upper_corr)] <- 0
view(upper_corr)
corr_out <- data.frame(corrMatrix)
write.csv(corr_out,"../results/correlation.csv", row.names = T)

### remove na >= 50% (20)
data20 <- newdata[,-which(colSums(is.na(newdata))>20)]   # 18
corr20 <- cor(data20, use="pairwise.complete.obs")
view(corr20)

## distribution

pdf("../figures/distributions_20.pdf")
par(mfrow = c(4, 5), oma = c(0.5,0.5,0,0.5), mar = c(1,1,1,1), mgp=c(0,0.2,0))
for (k in 1:ncol(data20)) {
  x0 = density(data20[, k], na.rm = T)
  x1 = density(log2(data20[, k]), na.rm = T)
  plot(density(data20[, k], na.rm = T), col = "navy", xlab = "", xaxt="n",
       main = colnames(data20)[k], ylab="", las = 1, cex.main = 0.5, cex.axis=0.5,
       ylim = range(c(x0$y, x1$y)), xlim = range(c(x0$x, x1$x)))
  lines(x1, col = "tomato")
  if (k == 18) {
    legend("topright", lwd = 1, col = c("navy", "tomato"), 
           legend = c("Original data", "Log2 transformation"), cex = 0.6, bty = "n")
  }
}
dev.off()


### add annotation
colnames(data38)
variable_cat=c(rep("ORGANOCHLORES", 5),
               rep("ORGANOPHOSPHORES_ET_METABOLITES",6),
               rep("PYRETHRINOIDES", 3),
               rep("METABOLITES_DE_PYRETHRINOIDES", 4),
               rep("PCBs", 2), 
               rep("PBDEs", 1), 
               rep("HERBICIDES_ACIDES", 3),
               rep("ANILINO_PYRIMIDINES", 1),
               rep("AZOLES", 6),
               rep("BENZAMIDES", 0),
               rep("CARBAMATES", 3),
               rep("CARBOXAMIDES", 2),
               rep("NEONICOTINOIDES", 1),
               rep("OXADIAZINES", 0),
               rep("PHENYLPYRAZOLES", 2),
               rep("PHENYLPYRAZOLES_LC", 2),
               rep("STROBILURINES", 3),
               rep("TRIAZINES_TRIAZINONES_DIAZINES", 2),
               rep("UREES_SUBSTITUEES", 4),
               rep("DINITROANILINES", 1),
               rep("DIVERS", 5))

colnames(data20)
variable_cat_1=c(rep("ORGANOCHLORES", 3),
               rep("ORGANOPHOSPHORES_ET_METABOLITES",1),
               rep("PYRETHRINOIDES", 2),
               rep("METABOLITES_DE_PYRETHRINOIDES", 2), 
               rep("HERBICIDES_ACIDES", 2),
               rep("AZOLES", 2),
               rep("CARBAMATES", 1),
               rep("STROBILURINES", 2),
               rep("TRIAZINES_TRIAZINONES_DIAZINES", 1),
               rep("DIVERS", 2))

sub_anno <- structure(list(classes = c(variable_cat)), .Names = "classes", row.names = c(colnames(data38)),
                      class = "data.frame")

sub_anno_1 <- structure(list(classes = c(variable_cat_1)), .Names = "classes", row.names = c(colnames(data20)),
                      class = "data.frame")


# Specify colors
ann_colors = list(classes = c(ORGANOCHLORES = "palevioletred2", ORGANOPHOSPHORES_ET_METABOLITES = "cadetblue1", 
            PYRETHRINOIDES = "mediumpurple1", METABOLITES_DE_PYRETHRINOIDES = "slateblue1",
            PCBs = "skyblue", 
            PBDEs = "turquoise3", HERBICIDES_ACIDES = "navyblue", ANILINO_PYRIMIDINES = "orange",
            AZOLES = "coral2", CARBAMATES = "violetred", CARBOXAMIDES = "darkred", 
            NEONICOTINOIDES = "springgreen2", PHENYLPYRAZOLES = "yellowgreen", 
            PHENYLPYRAZOLES_LC = "palegreen4",
            STROBILURINES = "yellow3", TRIAZINES_TRIAZINONES_DIAZINES = "wheat2",
            UREES_SUBSTITUEES = "tan", DINITROANILINES = "tan3", DIVERS = "grey50" ))

ann_colors = list(classes = c(ORGANOCHLORES = "palevioletred2", ORGANOPHOSPHORES_ET_METABOLITES = "cadetblue1", 
                              PYRETHRINOIDES = "mediumpurple1", METABOLITES_DE_PYRETHRINOIDES = "slateblue1",
                              PCBs = "skyblue", 
                              PBDEs = "turquoise3", HERBICIDES_ACIDES = "navyblue", ANILINO_PYRIMIDINES = "orange",
                              AZOLES = "coral2", CARBAMATES = "violetred", CARBOXAMIDES = "darkred", 
                              NEONICOTINOIDES = "springgreen2", PHENYLPYRAZOLES = "yellowgreen", 
                              PHENYLPYRAZOLES_LC = "palegreen4",
                              STROBILURINES = "yellow3", TRIAZINES_TRIAZINONES_DIAZINES = "wheat2",
                              UREES_SUBSTITUEES = "tan", DINITROANILINES = "tan3", DIVERS = "grey50" ))

### heatmap
## data38
pdf(("../figures/heatmap.pdf"), width = 15, height = 12)
pheatmap(corrMatrix, na_col = "grey80", cluster_rows = F, cluster_cols = F, 
         annotation_row = sub_anno, annotation_col = sub_anno, border=NA, 
         annotation_colors = ann_colors)
dev.off()

pdf(("../figures/heatmap_cluster.pdf"), width = 15, height = 12)
pheatmap(corrMatrix, na_col = "grey80", cluster_rows = T, cluster_cols = T, 
         treeheight_row = 0, treeheight_col = 0,
         annotation_row = sub_anno, annotation_col = sub_anno, border=NA, 
         annotation_colors = ann_colors)
dev.off()

pdf(("../figures/heatmap_display.pdf"), width = 15, height = 12)
pheatmap(corrMatrix, na_col = "grey80", cluster_rows = F, cluster_cols = F, 
         annotation_row = sub_anno, annotation_col = sub_anno, border=NA, 
         annotation_colors = ann_colors, 
         display_numbers = T, number_color = "black", fontsize_number = 3.5)
dev.off()


## data20
pdf(("../figures/heatmap_20.pdf"), width = 15, height = 12)
pheatmap(corr20, na_col = "grey80", cluster_rows = F, cluster_cols = F, 
         annotation_row = sub_anno_1, annotation_col = sub_anno_1, border=NA, 
         annotation_colors = ann_colors, breaks = seq(-1, 1, length.out = 100))
dev.off()

pdf(("../figures/heatmap_20_display.pdf"), width = 15, height = 12)
pheatmap(corr20, na_col = "grey80", cluster_rows = F, cluster_cols = F, 
         annotation_row = sub_anno_1, annotation_col = sub_anno_1, border=NA, 
         annotation_colors = ann_colors, breaks = seq(-1, 1, length.out = 100),
         display_numbers = T, number_color = "black", fontsize_number = 9)
dev.off()
