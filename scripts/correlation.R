
### Sumemr Project -- Correlation and inspection
## Created by Vivian on 14 July added on 28 July

rm(list=ls())
project_path="~/hair/scripts"
setwd(project_path)


##### Load data --
pest_log10 <- readRDS("../processed/imputation/log10_pest.rds")
pahs_log10 <- readRDS("../processed/imputation/log10_pahs.rds")
over_log10 <- readRDS("../processed/imputation/log10_overlap.rds")
sum(is.na(pest_log10))   # 736
sum(is.na(pahs_log10))   # 58
sum(is.na(over_log10))   # 67

pest_imp_log10 <- readRDS("../processed/imputation/imp_pest.rds")
pahs_imp_log10 <- readRDS("../processed/imputation/imp_pahs.rds")
over_imp_log10 <- readRDS("../processed/imputation/imp_overlap.rds")
sum(is.na(pest_imp_log10))   
sum(is.na(pahs_imp_log10))
sum(is.na(over_imp_log10))


##### Plot settings --
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

sub_anno_1 <- structure(list(classes = c(variable_cat_1)), .Names = "Family", row.names = c(colnames(pest_log10)),
                      class = "data.frame")

ann_colors_1 = list(Family = c(PCB = "#A6CEE3", BDE = "#1F78B4", 
                             Organochlorines = "#B2DF8A",
                             Organophosphorus = "#33A02C", 
                             Metabolits_of_Organophosphorus = "#FB9A99", 
                             Pyrethroid = "#E31A1C", Metabolits_of_Pyrethroid = "#FDBF6F",
                             Phenylpyrazole = "#FF7F00", Carboxamide = "#CAB2D6", 
                             Dinitroaniline = "#6A3D9A", Carbamates = "#FFFF99",
                             Oxadiazole = "#B15928"))


variable_cat=c(rep("Tabacco_related", 2),
                 rep("PAHs", 11))

sub_anno <- structure(list(classes = c(variable_cat)), .Names = "Family", row.names = c(colnames(pahs_log10 )),
                      class = "data.frame")

ann_colors = list(Family = c(Tabacco_related = "mediumpurple1",
                             PAHs = "yellow3"))


variable_cat_all <- c(rep("Tabacco_related", 2),
                      rep("PAHs", 11),
                      rep("PCB", 3),
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

sub_anno_all <- structure(list(classes = c(variable_cat_all)), .Names = "Family", row.names = c(colnames(over_log10 )),
                          class = "data.frame")

ann_colors_all = list(Family = c(Tabacco_related = "mediumpurple1",
                                 PAHs = "yellow3",
                                 PCB = "#A6CEE3", BDE = "#1F78B4", 
                                 Organochlorines = "#B2DF8A",
                                 Organophosphorus = "#33A02C", 
                                 Metabolits_of_Organophosphorus = "#FB9A99", 
                                 Pyrethroid = "#E31A1C", Metabolits_of_Pyrethroid = "#FDBF6F",
                                 Phenylpyrazole = "#FF7F00", Carboxamide = "#CAB2D6", 
                                 Dinitroaniline = "#6A3D9A", Carbamates = "#FFFF99",
                                 Oxadiazole = "#B15928"))


##### Correlations before imputation but after replacement of nd --
### use log-transformation
corr_pest_log10 <- cor(pest_log10, use="pairwise.complete.obs")

pdf(("../figures/correlation/pest_log10_cluster.pdf"), width = 15, height = 12) 
pheatmap(corr_pest_log10, na_col = "grey80", cluster_rows = T, cluster_cols = T, border=NA, 
         breaks = seq(-1, 1, length.out = 100),
         annotation_row = sub_anno_1, annotation_col = sub_anno_1,
         annotation_colors = ann_colors_1,
         display_numbers = T, number_color = "black", fontsize_number = 5)
dev.off()

pdf(("../figures/correlation/pest_log10.pdf"), width = 15, height = 12) 
pheatmap(corr_pest_log10, na_col = "grey80", cluster_rows = F, cluster_cols = F, border=NA, 
         breaks = seq(-1, 1, length.out = 100),
         annotation_row = sub_anno_1, annotation_col = sub_anno_1,
         annotation_colors = ann_colors_1,
         display_numbers = T, number_color = "black", fontsize_number = 5)
dev.off()


corr_pahs_log10 <- cor(pahs_log10, use="pairwise.complete.obs")

pdf(("../figures/correlation/pahs_log10_cluster.pdf"), width = 15, height = 12) 
pheatmap(corr_pahs_log10, na_col = "grey80", cluster_rows = T, cluster_cols = T, border=NA, 
         breaks = seq(-1, 1, length.out = 100),
         annotation_row = sub_anno, annotation_col = sub_anno,
         annotation_colors = ann_colors,
         display_numbers = T, number_color = "black", fontsize_number = 7)
dev.off()

pdf(("../figures/correlation/pahs_log10.pdf"), width = 15, height = 12) 
pheatmap(corr_pahs_log10, na_col = "grey80", cluster_rows = F, cluster_cols = F, border=NA, 
         breaks = seq(-1, 1, length.out = 100),
         annotation_row = sub_anno, annotation_col = sub_anno,
         annotation_colors = ann_colors,
         display_numbers = T, number_color = "black", fontsize_number = 7)
dev.off()


corr_over_log10 <- cor(over_log10, use="pairwise.complete.obs")

pdf(("../figures/correlation/over_log10_cluster.pdf"), width = 15, height = 12) 
pheatmap(corr_over_log10, na_col = "grey80", cluster_rows = T, cluster_cols = T, border=NA, 
         breaks = seq(-1, 1, length.out = 100),
         annotation_row = sub_anno_all, annotation_col = sub_anno_all,
         annotation_colors = ann_colors_all,
         display_numbers = T, number_color = "black", fontsize_number = 5)
dev.off()

pdf(("../figures/correlation/over_log10.pdf"), width = 15, height = 12) 
pheatmap(corr_over_log10, na_col = "grey80", cluster_rows = F, cluster_cols = F, border=NA, 
         breaks = seq(-1, 1, length.out = 100),
         annotation_row = sub_anno_all, annotation_col = sub_anno_all,
         annotation_colors = ann_colors_all,
         display_numbers = T, number_color = "black", fontsize_number = 5)
dev.off()


### Spearman’s correlations on the raw levels
pest <- 10^(pest_log10)
log10(pest[1,1]) == pest_log10[1,1]
pahs <- 10^(pahs_log10)
log10(pahs[100,5]) == pahs_log10[100,5]

corr_pest <- cor(pest, method = "spearman", use="pairwise.complete.obs")

pdf(("../figures/correlation/pest_cluster.pdf"), width = 15, height = 12) 
pheatmap(corr_pest, na_col = "grey80", cluster_rows = T, cluster_cols = T, border=NA, 
         breaks = seq(-1, 1, length.out = 100),
         annotation_row = sub_anno_1, annotation_col = sub_anno_1,
         annotation_colors = ann_colors_1,
         display_numbers = T, number_color = "black", fontsize_number = 5)
dev.off()

pdf(("../figures/correlation/pest.pdf"), width = 15, height = 12) 
pheatmap(corr_pest, na_col = "grey80", cluster_rows = F, cluster_cols = F, border=NA, 
         breaks = seq(-1, 1, length.out = 100),
         annotation_row = sub_anno_1, annotation_col = sub_anno_1,
         annotation_colors = ann_colors_1,
         display_numbers = T, number_color = "black", fontsize_number = 5)
dev.off()


corr_pahs <- cor(pahs, method = "spearman", use="pairwise.complete.obs")

pdf(("../figures/correlation/pahs_cluster.pdf"), width = 15, height = 12) 
pheatmap(corr_pahs, na_col = "grey80", cluster_rows = T, cluster_cols = T, border=NA, 
         breaks = seq(-1, 1, length.out = 100),
         annotation_row = sub_anno, annotation_col = sub_anno,
         annotation_colors = ann_colors,
         display_numbers = T, number_color = "black", fontsize_number = 7)
dev.off()

pdf(("../figures/correlation/pahs.pdf"), width = 15, height = 12) 
pheatmap(corr_pahs, na_col = "grey80", cluster_rows = F, cluster_cols = F, border=NA, 
         breaks = seq(-1, 1, length.out = 100),
         annotation_row = sub_anno, annotation_col = sub_anno,
         annotation_colors = ann_colors,
         display_numbers = T, number_color = "black", fontsize_number = 7)
dev.off()


##### Correlations for clean data (after imputation) --
### use log-transformation
corr_pest_imp_log10 <- cor(pest_imp_log10, use="pairwise.complete.obs")

pdf(("../figures/correlation/pest_imp_log10_cluster.pdf"), width = 15, height = 12) 
pheatmap(corr_pest_imp_log10, na_col = "grey80", cluster_rows = T, cluster_cols = T, border=NA, 
         breaks = seq(-1, 1, length.out = 100),
         annotation_row = sub_anno_1, annotation_col = sub_anno_1,
         annotation_colors = ann_colors_1,
         display_numbers = T, number_color = "black", fontsize_number = 5)
dev.off()

pdf(("../figures/correlation/pest_imp_log10.pdf"), width = 15, height = 12) 
pheatmap(corr_pest_imp_log10, na_col = "grey80", border=NA, cluster_rows = F, cluster_cols = F,
         breaks = seq(-1, 1, length.out = 100),
         annotation_row = sub_anno_1, annotation_col = sub_anno_1,
         annotation_colors = ann_colors_1,
         display_numbers = T, number_color = "black", fontsize_number = 5)
dev.off()

corr_pahs_imp_log10 <- cor(pahs_imp_log10, use="pairwise.complete.obs")

pdf(("../figures/correlation/pahs_imp_log10_cluster.pdf"), width = 15, height = 12) 
pheatmap(corr_pahs_imp_log10, na_col = "grey80", cluster_rows = T, cluster_cols = T, border=NA, 
         breaks = seq(-1, 1, length.out = 100),
         annotation_row = sub_anno, annotation_col = sub_anno,
         annotation_colors = ann_colors,
         display_numbers = T, number_color = "black", fontsize_number = 8)
dev.off()

pdf(("../figures/correlation/pahs_imp_log10.pdf"), width = 15, height = 12) 
pheatmap(corr_pahs_imp_log10, na_col = "grey80", cluster_rows = F, cluster_cols = F, border=NA, 
         breaks = seq(-1, 1, length.out = 100),
         annotation_row = sub_anno, annotation_col = sub_anno,
         annotation_colors = ann_colors,
         display_numbers = T, number_color = "black", fontsize_number = 8)
dev.off()


corr_over_imp_log10 <- cor(over_imp_log10, use="pairwise.complete.obs")

pdf(("../figures/correlation/over_imp_log10_cluster.pdf"), width = 15, height = 12) 
pheatmap(corr_over_imp_log10, na_col = "grey80", cluster_rows = T, cluster_cols = T, border=NA, 
         breaks = seq(-1, 1, length.out = 100),
         annotation_row = sub_anno_all, annotation_col = sub_anno_all,
         annotation_colors = ann_colors_all,
         display_numbers = T, number_color = "black", fontsize_number = 5)
dev.off()

pdf(("../figures/correlation/over_imp_log10.pdf"), width = 15, height = 12) 
pheatmap(corr_over_imp_log10, na_col = "grey80", cluster_rows = F, cluster_cols = F, border=NA, 
         breaks = seq(-1, 1, length.out = 100),
         annotation_row = sub_anno_all, annotation_col = sub_anno_all,
         annotation_colors = ann_colors_all,
         display_numbers = T, number_color = "black", fontsize_number = 5)
dev.off()


### Spearman’s correlations on the raw levels
pest_imp <- 10^(pest_imp_log10)
log10(pest_imp[1,1]) == pest_imp_log10[1,1]
pahs_imp <- 10^(pahs_imp_log10)
log10(pahs_imp[100,5]) == pahs_imp_log10[100,5]


corr_pest_imp <- cor(pest_imp, method = "spearman", use="pairwise.complete.obs")

pdf(("../figures/correlation/pest_imp_cluster.pdf"), width = 15, height = 12) 
pheatmap(corr_pest_imp, na_col = "grey80", cluster_rows = T, cluster_cols = T, border=NA, 
         breaks = seq(-1, 1, length.out = 100),
         annotation_row = sub_anno_1, annotation_col = sub_anno_1,
         annotation_colors = ann_colors_1,
         display_numbers = T, number_color = "black", fontsize_number = 5)
dev.off()

pdf(("../figures/correlation/pest_imp.pdf"), width = 15, height = 12) 
pheatmap(corr_pest_imp, na_col = "grey80", cluster_rows = F, cluster_cols = F, border=NA, 
         breaks = seq(-1, 1, length.out = 100),
         annotation_row = sub_anno_1, annotation_col = sub_anno_1,
         annotation_colors = ann_colors_1,
         display_numbers = T, number_color = "black", fontsize_number = 5)
dev.off()


corr_pahs_imp <- cor(pahs_imp, method = "spearman", use="pairwise.complete.obs")

pdf(("../figures/correlation/pahs_imp_cluster.pdf"), width = 15, height = 12) 
pheatmap(corr_pahs_imp, na_col = "grey80", cluster_rows = T, cluster_cols = T, border=NA, 
         breaks = seq(-1, 1, length.out = 100),
         annotation_row = sub_anno, annotation_col = sub_anno,
         annotation_colors = ann_colors,
         display_numbers = T, number_color = "black", fontsize_number = 8)
dev.off()

pdf(("../figures/correlation/pahs_imp.pdf"), width = 15, height = 12) 
pheatmap(corr_pahs_imp, na_col = "grey80", cluster_rows = F, cluster_cols = F, border=NA, 
         breaks = seq(-1, 1, length.out = 100),
         annotation_row = sub_anno, annotation_col = sub_anno,
         annotation_colors = ann_colors,
         display_numbers = T, number_color = "black", fontsize_number = 8)
dev.off()
