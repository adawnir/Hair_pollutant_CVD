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





