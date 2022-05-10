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
                 rep("Organophosphorus", 2),
                 rep("Metabolits_of_Organophosphorus", 4),
                 rep("Pyrethroid", 1),
                 rep("Metabolits_of_Pyrethroid", 2),
                 rep("Phenylpyrazole", 2),
                 rep("Carbomate", 1),
                 rep("Carboxamide", 1),
                 rep("Dinitroaniline", 1))

sub_anno <- structure(list(classes = c(variable_cat_1)), .Names = "Family", row.names = c(colnames(corr50per)),
                      class = "data.frame")

### specify colours
ann_colors = list(Family = c(Tabacco_related = "palevioletred2", PAHs = "cadetblue1", 
                             PCBs= "mediumpurple1", Organochlorines = "yellow3",
                             Organophosphorus = "tan", 
                             Metabolits_of_Organophosphorus = "turquoise3", 
                             Pyrethroid = "navyblue", Metabolits_of_Pyrethroid = "orange",
                             Phenylpyrazole = "coral2", Carboxamide = "violetred", 
                             Dinitroaniline = "springgreen2", Carbomate = "grey50"))


### plot

pdf(("../figures/Overlap/heatmap_over_50per_display.pdf"), width = 15, height = 12)
pheatmap(corr50per, na_col = "grey80", cluster_rows = T, cluster_cols = T, border=NA, 
         breaks = seq(-1, 1, length.out = 100),
         annotation_row = sub_anno, annotation_col = sub_anno,
         annotation_colors = ann_colors,
         display_numbers = T, number_color = "black", fontsize_number = 9)
dev.off()


pdf(("../figures/Overlap/heatmap_over_50per_B_display.pdf"), width = 15, height = 12)
pheatmap(corr50per_B, na_col = "grey80", cluster_rows = T, cluster_cols = T, border=NA, 
         breaks = seq(-1, 1, length.out = 100),
         annotation_row = sub_anno, annotation_col = sub_anno,
         annotation_colors = ann_colors,
         display_numbers = T, number_color = "black", fontsize_number = 9)
dev.off()


pdf(("../figures/Overlap/heatmap_over_50per_L_display.pdf"), width = 15, height = 12)
pheatmap(corr50per_L, na_col = "grey80", cluster_rows = T, cluster_cols = T, border=NA, 
         breaks = seq(-1, 1, length.out = 100),
         annotation_row = sub_anno, annotation_col = sub_anno,
         annotation_colors = ann_colors,
         display_numbers = T, number_color = "black", fontsize_number = 9)
dev.off()




