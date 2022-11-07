## Spearman correlation matrices for exposures

rm(list=ls())
project_path=dirname(rstudioapi::getActiveDocumentContext()$path)
setwd(project_path)

source("graph_colours.R")
library(tidyverse)
library(pheatmap)
library(RColorBrewer)
library(grid)

## Exposure correlation matrix ----
filepaths = c("PesticidesPCBBDEs_train","PAHs_train","Overlap_test")
for (i in 1:3){
  expo = readRDS(paste0("../Processed/",filepaths[i],"_expo_ndimp_naimp_log.rds"))
  print(ncol(expo))
  covar = readRDS(paste0("../Processed/",filepaths[i],"_covar_naimp.rds"))
  covar = covar %>% select(Energy, Proteins, Fats, Carbohydrates, Fibers)
  
  annot_sub = annot[colnames(expo)]
  mat_col = data.frame(Family = c(rep("Diet intake", ncol(covar)),annot_sub))
  rownames(mat_col) = c(colnames(covar), colnames(expo))
  mat_colors = list(Family = c("#ffcc00",annot.colours[unique(annot_sub)]))
  names(mat_colors$Family) = c("Diet intake", unique(annot_sub))
  
  cor = cor(cbind(covar, expo), method = "spearman")
  width = height = 0.1*nrow(cor)+3.5
  mybreaks = seq(-1, 1, length = 100)
  {pdf(paste0("../Figures/Descriptive_statistics/Spearman_correlation_matrix_",filepaths[i],".pdf"), width = width, height = height)
    pheatmap(cor,
             cellwidth = 8, cellheight = 8,
             border_color = NA, breaks = mybreaks,
             treeheight_row = 0, treeheight_col = 0,
             annotation_row = mat_col,annotation_col = mat_col, annotation_colors = mat_colors,
             legend = FALSE, annotation_legend = FALSE,
             annotation_names_row = FALSE, annotation_names_col = FALSE)
    dev.off()
  }
  tmp = cor
  tmp[lower.tri(tmp, diag = T)] = NA
  high_corr = cbind(rownames(tmp)[which(abs(tmp) > 0.5, arr.ind = TRUE)[,1]],
                    colnames(tmp)[which(abs(tmp) > 0.5, arr.ind = TRUE)[,2]],
                    tmp[which(abs(tmp) > 0.5, arr.ind = TRUE)])
  write.csv(high_corr, file = paste0("../Exports/Descriptive_statistics/Highly_correlated_compounds_",filepaths[i],".csv"))
}

## Outcome correlation matrix ----
response = readRDS(paste0("../Data/PesticidesPCBBDEs_train_response.rds"))
out = response[,1:9]
print(ncol(out))

cor = cor(out, method = "spearman")
width = height = 0.1*nrow(cor)+1
mybreaks = seq(-1, 1, length = 100)
{pdf(paste0("../Figures/Descriptive_statistics/Spearman_correlation_matrix_outcome.pdf"), width = width, height = height)
  pheatmap(cor,
           cellwidth = 8, cellheight = 8,
           border_color = NA, breaks = mybreaks,
           treeheight_row = 0, treeheight_col = 0,
           legend = FALSE)
  dev.off()
}

tmp = cor
tmp[lower.tri(tmp, diag = T)] = NA
high_corr = cbind(rownames(tmp)[which(abs(tmp) > 0.5, arr.ind = TRUE)[,1]],
                  colnames(tmp)[which(abs(tmp) > 0.5, arr.ind = TRUE)[,2]],
                  tmp[which(abs(tmp) > 0.5, arr.ind = TRUE)])
write.csv(high_corr, file = paste0("../Exports/Descriptive_statistics/Highly_correlated_outcomes_All_participants.csv"))


## Legend for correlation matrices ----
draw_legend = function(color, breaks, legend, ...){
  color = color[!is.infinite(breaks)]
  breaks = breaks[!is.infinite(breaks)]
  
  height = min(unit(1, "npc"), unit(150, "bigpts"))
  
  legend_pos = (legend - min(breaks)) / (max(breaks) - min(breaks))
  legend_pos = height * legend_pos + (unit(1, "npc") - height)
  
  breaks = (breaks - min(breaks)) / (max(breaks) - min(breaks))
  breaks = height * breaks + (unit(1, "npc") - height)
  
  h = breaks[-1] - breaks[-length(breaks)]
  
  rect = rectGrob(x = 0, y = breaks[-length(breaks)], width = unit(10, "bigpts"), height = h, hjust = 0, vjust = 0, gp = gpar(fill = color, col = "#FFFFFF00"))
  text = textGrob(names(legend), x = unit(14, "bigpts"), y = legend_pos, hjust = 0, gp = gpar(...))
  title = textGrob("Spearman's correlation coefficient", x = 0, y = 1.05, hjust = 0.5, vjust = 0, gp = gpar(...))
  
  res = grobTree(rect, text, title,
                 vp = viewport(x = unit(0.7, "npc"), height=unit(0.8, "npc"),width=unit(4, "cm")))
  
  return(res)
}

legend = grid.pretty(range(as.vector(mybreaks)))
names(legend) = legend
color = colorRampPalette(rev(brewer.pal(n = 7, name = "RdYlBu")))(100)

annot.colours.plus = c("#ffcc00",annot.colours)
names(annot.colours.plus) = c("Diet intake",names(annot.colours))

{pdf("../Figures/Descriptive_statistics/Spearman_correlation_matrix_outcome_legend.pdf", height = 3, width = 3)
  grid.newpage()
  grid.draw(draw_legend(color, mybreaks, legend))
  dev.off()
}

{pdf("../Figures/Descriptive_statistics/Spearman_correlation_matrix_exposure_legend.pdf", height = 7, width = 4)
  grid.newpage()
  grid.draw(draw_legend(color, mybreaks, legend))
  for(e in 1:length(annot.colours.plus)) {
    grid.rect(
      2,
      e*0.55+2,
      0.5,
      0.5,
      default.units = "cm",
      gp=gpar( 
        fill=rev(annot.colours.plus)[e],
        lwd=0
      )
    )
    grid.text(
      label = rev(names(annot.colours.plus))[e],
      x = 2.5,
      y = e*0.55+2,
      default.units = "cm",
      hjust = 0
    )
  }
  dev.off()
}
