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
