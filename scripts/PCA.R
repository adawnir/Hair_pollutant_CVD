
### Sumemr Project --  PCA (descriptive analyses?)
## Programme created by Vivian on 10 July added on 28 July

rm(list=ls())
project_path="~/hair/scripts"
setwd(project_path)

#install.packages("factoextra")
library(factoextra)


##### Load data --
pest1 <- readRDS("../processed/imputation/imp_pest.rds")
pahs1 <- readRDS("../processed/imputation/imp_pahs.rds")

pest <- readRDS("../processed/uni_multi_pest.rds")
pahs <- readRDS("../processed/uni_multi_pahs.rds")
over <- readRDS("../processed/uni_multi_overlap.rds")
colnames(pest)


##### Run PCA --
### pest all
colnames(pest)[17:49] <- c(colnames(pest1)[1:6], "Alpha-endosulfan", "Beta-endosulfan",
                           "Beta-HCH", "Gamma-HCH", colnames(pest1)[11:33])
pca_pest <- prcomp(pest[,c(17:49)], center = TRUE,scale. = TRUE)
a <- summary(pca_pest)
sum(summary(pca_pest)$importance[2,1:10])   # 0.58852999999999999758

var_pest <- get_pca_var(pca_pest)
view(var_pest$contrib)
ind_pest <- get_pca_ind(pca_pest)
view(ind_pest$contrib)

pdf("../figures/PCA/scree_pest.pdf", height = 6, width = 8)
fviz_eig(pca_pest, barcolor = "gray70", barfill = "gray70")
dev.off()

pdf("../figures/PCA/contri_pest.pdf", height = 6, width = 8)
fviz_pca_var(pca_pest, labelsize = 2,
             col.var = "contrib", # Color by contributions to the PC
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
             repel = TRUE     # Avoid text overlappingn
             )
dev.off()

pdf("../figures/PCA/ind_pest.pdf", height = 6, width = 8)
groups <- pest$Country
fviz_pca_ind(pca_pest,
             col.ind = groups, # color by groups
             palette = c("#FFA54F",  "#87CEFA", "#9ACD32"),
             legend.title = "Country",
             label="none")
dev.off()

pdf("../figures/PCA/bi_pest.pdf", height = 8, width = 10)
fviz_pca_biplot(pca_pest, 
                label = "var", labelsize = 3,
                col.ind = groups, palette = c("#FFA54F",  "#87CEFA", "#9ACD32"), 
                addEllipses = TRUE, 
                col.var = "black", repel = TRUE, 
                legend.title = "Country")
dev.off()


### pest Lux
pest_L <- pest[which(pest$Country == "Luxembourg"),]   # 487

pca_pest_L <- prcomp(pest_L[,c(17:49)], center = TRUE,scale. = TRUE)
a <- summary(pca_pest)
sum(summary(pca_pest_L)$importance[2,1:10])    # 0.55781999999999998252

pdf("../figures/PCA/scree_pest_L.pdf", height = 6, width = 8)
fviz_eig(pca_pest_L, barcolor = "gray70", barfill = "gray70")
dev.off()

pdf("../figures/PCA/contri_pest_L.pdf", height = 6, width = 8)
fviz_pca_var(pca_pest_L,  labelsize = 2,
             col.var = "contrib",  # Color by contributions to the PC
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
             repel = TRUE     # Avoid text overlappingn
)
dev.off()

pdf("../figures/PCA/ind_pest_L.pdf", height = 6, width = 8)

fviz_pca_ind(pca_pest_L,
             pointsize = 1.5, col.ind = "#FFA54F",
             legend.title = "Country",
             label="none")
dev.off()

pdf("../figures/PCA/bi_pest_L.pdf", height = 8, width = 10)
fviz_pca_biplot(pca_pest_L, 
                label = "var", labelsize = 3,
                col.ind = "#FFA54F",
                addEllipses = TRUE, 
                col.var = "black", repel = TRUE, 
                legend.title = "Country")
dev.off()


### pest Bel
pest_B <- pest[which(pest$Country == "Belgium"),]   # 503

pca_pest_B <- prcomp(pest_B[,c(17:49)], center = TRUE,scale. = TRUE)
sum(summary(pca_pest_B)$importance[2,1:10])    # 0.60735000000000005649

pdf("../figures/PCA/scree_pest_B.pdf", height = 6, width = 8)
fviz_eig(pca_pest_B, barcolor = "gray70", barfill = "gray70")
dev.off()

pdf("../figures/PCA/contri_pest_B.pdf", height = 6, width = 8)
fviz_pca_var(pca_pest_B,  labelsize = 2,
             col.var = "contrib",  # Color by contributions to the PC
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
             repel = TRUE)
dev.off()

pdf("../figures/PCA/ind_pest_B.pdf", height = 6, width = 8)
fviz_pca_ind(pca_pest_B,
             pointsize = 1.5, col.ind = "#87CEFA",
             legend.title = "Country",
             label="none")
dev.off()

pdf("../figures/PCA/bi_pest_B.pdf", height = 8, width = 10)
fviz_pca_biplot(pca_pest_B, 
                label = "var", labelsize = 3,
                col.ind = "#87CEFA",
                addEllipses = TRUE, 
                col.var = "black", repel = TRUE, 
                legend.title = "Country")
dev.off()


### pest France
pest_F <- pest[which(pest$Country == "France"),]   # 75

pca_pest_F <- prcomp(pest_F[,c(17:49)], center = TRUE,scale. = TRUE)
sum(summary(pca_pest_F)$importance[2,1:10])    # 0.69222000000000005748

pdf("../figures/PCA/scree_pest_F.pdf", height = 6, width = 8)
fviz_eig(pca_pest_F, barcolor = "gray70", barfill = "gray70")
dev.off()

pdf("../figures/PCA/contri_pest_F.pdf", height = 6, width = 8)
fviz_pca_var(pca_pest_F,  labelsize = 2,
             col.var = "contrib",  # Color by contributions to the PC
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
             repel = TRUE)
dev.off()

pdf("../figures/PCA/ind_pest_F.pdf", height = 6, width = 8)
fviz_pca_ind(pca_pest_F,
             pointsize = 1.5, col.ind = "#9ACD32",
             legend.title = "Country",
             label="none")
dev.off()

pdf("../figures/PCA/bi_pest_F.pdf", height = 8, width = 10)
fviz_pca_biplot(pca_pest_F, 
                label = "var", labelsize = 3,
                col.ind = "#9ACD32",
                addEllipses = TRUE, 
                col.var = "black", repel = TRUE, 
                legend.title = "Country")
dev.off()



### pahs
colnames(pahs)[17:29] <- c("Cotinine", "Nicotine", colnames(pahs1)[3:13])
pca_pahs <- prcomp(pahs[,c(17:29)], center = TRUE,scale. = TRUE)
summary(pca_pahs)
sum(summary(pca_pahs)$importance[2,1:5])   # 0.62861999999999995659

pdf("../figures/PCA/scree_pahs.pdf", height = 6, width = 8)
fviz_eig(pca_pahs, barcolor = "gray70", barfill = "gray70")
dev.off()

pdf("../figures/PCA/contri_pahs.pdf", height = 6, width = 8)
fviz_pca_var(pca_pahs,
             col.var = "contrib", # Color by contributions to the PC
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
             repel = TRUE     # Avoid text overlappingn
)
dev.off()

pdf("../figures/PCA/ind_pahs.pdf", height = 6, width = 8)
groups <- pahs$Country
fviz_pca_ind(pca_pahs,
             col.ind = groups, # color by groups
             palette = c("#FFA54F",  "#87CEFA"),
             legend.title = "Country",
             label="none")
dev.off()

pdf("../figures/PCA/bi_pahs.pdf", height = 8, width = 10)
fviz_pca_biplot(pca_pahs, 
                label = "var", labelsize = 3,
                col.ind = groups, palette = c("#FFA54F",  "#87CEFA"), 
                addEllipses = TRUE, 
                col.var = "black", repel = TRUE, 
                legend.title = "Country")
dev.off()


### PAHS Lux
pahs_L <- pahs[which(pahs$Country == "Luxembourg"),]
pca_pahs_L <- prcomp(pahs_L[,c(17:29)], center = TRUE,scale. = TRUE)
sum(summary(pca_pahs_L)$importance[2,1:5])    # 0.61365999999999998327

pdf("../figures/PCA/scree_pahs_L.pdf", height = 6, width = 8)
fviz_eig(pca_pahs_L, barcolor = "gray70", barfill = "gray70")
dev.off()

pdf("../figures/PCA/contri_pahs_L.pdf", height = 6, width = 8)
fviz_pca_var(pca_pahs_L,
             col.var = "contrib", # Color by contributions to the PC
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
             repel = TRUE     # Avoid text overlappingn
)
dev.off()

pdf("../figures/PCA/ind_pahs_L.pdf", height = 6, width = 8)
fviz_pca_ind(pca_pahs_L,
             col.ind = "#FFA54F",
             legend.title = "Country",
             label="none")
dev.off()

pdf("../figures/PCA/bi_pahs_L.pdf", height = 8, width = 10)
fviz_pca_biplot(pca_pahs_L, 
                label = "var", labelsize = 3,
                col.ind = "#FFA54F", 
                addEllipses = TRUE, 
                col.var = "black", repel = TRUE, 
                legend.title = "Country")
dev.off()


### PAHS Bel
pahs_B <- pahs[which(pahs$Country == "Belgium"),]
pca_pahs_B <- prcomp(pahs_B[,c(17:29)], center = TRUE,scale. = TRUE)
sum(summary(pca_pahs_B)$importance[2,1:5])    # 0.83617000000000007987

pdf("../figures/PCA/scree_pahs_B.pdf", height = 6, width = 8)
fviz_eig(pca_pahs_B, barcolor = "gray70", barfill = "gray70")
dev.off()

pdf("../figures/PCA/contri_pahs_B.pdf", height = 6, width = 8)
fviz_pca_var(pca_pahs_B,
             col.var = "contrib", # Color by contributions to the PC
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
             repel = TRUE     # Avoid text overlappingn
)
dev.off()

pdf("../figures/PCA/ind_pahs_B.pdf", height = 6, width = 8)
fviz_pca_ind(pca_pahs_B,
             col.ind = "#87CEFA",
             legend.title = "Country",
             label="none")
dev.off()

pdf("../figures/PCA/bi_pahs_B.pdf", height = 8, width = 10)
fviz_pca_biplot(pca_pahs_B, 
                label = "var", labelsize = 3,
                col.ind = "#87CEFA", 
                addEllipses = TRUE, 
                col.var = "black", repel = TRUE, 
                legend.title = "Country")
dev.off()


### Overlap
colnames(over)[17:62] <- c(colnames(pahs)[17:29], colnames(pest)[17:49])
pca_over <- prcomp(over[,c(17:62)], center = TRUE, scale. = TRUE)
summary(pca_over)
sum(summary(pca_over)$importance[2,1:10])   # 0.5196399999999999908

pdf("../figures/PCA/scree_over.pdf", height = 6, width = 8)
fviz_eig(pca_over, barcolor = "gray70", barfill = "gray70")
dev.off()

pdf("../figures/PCA/contri_over.pdf", height = 6, width = 8)
fviz_pca_var(pca_over,
             col.var = "contrib", # Color by contributions to the PC
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
             repel = TRUE     # Avoid text overlappingn
)
dev.off()

pdf("../figures/PCA/ind_over.pdf", height = 6, width = 8)
groups <- over$Country
fviz_pca_ind(pca_over,
             col.ind = groups, # color by groups
             palette = c("#FF7259",  "#3232FF"),
             addEllipses = TRUE, # Concentration ellipses
             ellipse.type = "confidence",
             legend.title = "Country",
             label="none")
dev.off()

pdf("../figures/PCA/bi_over.pdf", height = 8, width = 10)
fviz_pca_biplot(pca_over, 
                label = "var", labelsize = 3,
                col.ind = groups, palette = c("#FFA54F",  "#87CEFA"), 
                addEllipses = TRUE, 
                col.var = "black", repel = TRUE, 
                legend.title = "Country")
dev.off()


##### Inspection --
pest_raw <- readRDS("../processed/uni_multi_pest.rds")
pest_data <- fviz_pca_ind(pca_pest,
                          col.ind = groups, # color by groups
                          palette = c("#FF7259",  "#3232FF", "#198C19"),
                          addEllipses = TRUE, # Concentration ellipses
                          ellipse.type = "confidence",
                          legend.title = "Country",
                          label="none")
pest_data <- pest_data$data

fviz_pca_ind(pca_pest,
             col.ind = groups, # color by groups
             palette = c("#FF7259",  "#3232FF", "#198C19"),
             addEllipses = TRUE, # Concentration ellipses
             ellipse.type = "confidence",
             legend.title = "Country",
             label = "none") + geom_abline(intercept = -3, slope = -0.6, color="red", 
                                           linetype="dashed")

### run function (y = -3-0.6x)
pest_b <- pest_data[which(pest_data$Col. == "Belgium"), 1:3]

foo1 <- function(x, y){
  tmp = 0.6*x + y + 3
  return(tmp)
}

stored_vector <- foo1(pest_b$x, pest_b$y)
ids <- which(stored_vector < 0)
ids_low <- rownames(pest_b)[ids]
ids_low <- ids_low[-46]

### extract ids / rownames:"884"
belgium <- pest_raw[which(pest_raw$Country == "Belgium"),]
belgium_1 <- belgium[which(rownames(belgium) %in% ids_low),]
table(belgium_1$Country)
belgium_2 <- belgium[-which(rownames(belgium) %in% c(ids_low, "884")),]
table(belgium_2$Country)

belgium_ids_1 <- belgium_1$identifiant
belgium_ids_2 <- belgium_2$identifiant
belgium_ids_3 <- belgium$identifiant[which(rownames(belgium) == "884")]
sum(belgium_ids_1 %in% belgium_ids_2)

write.csv(belgium_ids_1, "../processed/PCA/ids_low.csv", row.names = F)
write.csv(belgium_ids_2, "../processed/PCA/ids_up.csv", row.names = F)
write.csv(belgium_ids_3, "../processed/PCA/ids_outlier.csv", row.names = F)


