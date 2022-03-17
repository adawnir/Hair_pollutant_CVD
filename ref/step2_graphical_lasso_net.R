### CE Project -- Graphical Lasso Network
## Programme created by Rin on 14 April 


# ## Parameters
# args=commandArgs(trailingOnly=TRUE)
# m=as.numeric(args[1])

rm(list=ls())
project_path="/rds/general/project/hda_students_data/live/Group3/CE_Group3/Scripts"
setwd(project_path)

## Load packages and source functions
LoadPackages=function(packages){
  for (i in 1:length(packages)){
    suppressPackageStartupMessages(library(packages[i], character.only=TRUE))
  }
}

LoadPackages(c("pheatmap","corpcor","abind","parallel",
               "RColorBrewer","igraph","ppcor","mvtnorm",
               "pROC","glasso","stabs","huge","pulsar",
               "QUIC","glassoFast","colorspace","glmnet","tidyverse"))

source("penalisation_functions.R")

## Load dataset
proteins = readRDS("../Processed/proteins_denoised.rds")
meta_pos = readRDS("../Processed/meta_pos_denoised_selected.rds")
meta_neg = readRDS("../Processed/meta_neg_denoised_selected.rds")
cpgs = readRDS("../Processed/cpgs_denoised_selected.rds")

x=cbind(proteins, cpgs, meta_pos, meta_neg)


# Data frame with column annotations
var_cat=c(rep("Proteins",ncol(proteins)),
          rep("CpG sites", ncol(cpgs)),
          rep("Clustered metabolites (positive)", ncol(meta_pos)),
          rep("Clustered metabolites (negative)", ncol(meta_neg)))
mat_col = data.frame(Signatures = var_cat)
rownames(mat_col) = colnames(x)

# List with colors for each annotation
Signatures = c("lightgreen" ,"lightgoldenrod","lightcoral","lightskyblue")
names(Signatures) = unique(var_cat)
mat_colors = list(Signatures=Signatures)

# Heatmap of correlation
pdf("../Figures/step2_corr_matrix.pdf", width = 9.5, height = 6.5)
pheatmap(cor(x), show_rownames = FALSE, show_colnames = FALSE,
         cellwidth = 1, cellheight = 1,
         annotation_names_row = FALSE, annotation_names_col = FALSE,
         annotation_row = mat_col,annotation_col = mat_col, annotation_colors = mat_colors,
         cluster_rows=FALSE, cluster_cols=FALSE, breaks=seq(-1,1,length.out=100), border=NA)
dev.off()

# Running stability selection
t0=Sys.time()
pk=c(ncol(proteins), ncol(cpgs), ncol(meta_pos), ncol(meta_neg))
Lambda=LambdaGridNetwork(data=x, pk=pk, lambda_path_refined_cardinal=50, PFER_thr=50)
out=CalibrateNetwork(data=x, pk=pk, Lambda=Lambda, lambda_other_blocks=apply(Lambda,2,min), 
                     K=100, tau=0.5, PFER_thr=50,
                     refine_calib_grid=FALSE, verbose=FALSE)
t1=Sys.time()
print(t1-t0)
saveRDS(out, "../Results/step2_calib_net_obj.rds")

block_names=c("Protein-protein", "Protein-CpG","CpG-CpG",
              "MetaboliteP-protein","MetaboliteP-CpG",
              "MetaboliteP-metaboliteP","MetaboliteN-protein",
              "MetaboliteN-CpG","MetaboliteN-metaboliteP",
              "MetaboliteN-metaboliteN")
for (i in 1:10){
  print(block_names[i])
  pdf(paste0("../Figures/step2_calib_",block_names[i],".pdf"))
  CalibrationPlot(out, bi_dim = TRUE, block_id=i)
  dev.off()
}

# Adjacency matrix of the calibrated network
A=CalibratedAdjacency(out)
saveRDS(A, "../Results/step2_adj_mat.rds")

# Using different colours for the proteins and transcripts
mynode_colours=c(rep("lightgreen",ncol(proteins)),
                 rep("lightgoldenrod",ncol(cpgs)),
                 rep("lightcoral",ncol(meta_pos)),
                 rep("lightskyblue",ncol(meta_neg)))

# Using gene symbols as labels (CpGs)
cpg_genes = gsub(".*_","",colnames(cpgs))
mynode_labels=c(colnames(proteins),cpg_genes,colnames(meta_pos),colnames(meta_neg))
names(mynode_labels)=colnames(x)

# Getting igraph object
mygraph=GetGraph(adjacency=A, node_color=mynode_colours, node_label=mynode_labels)

pdf("../Figures/step2_network.pdf")
par(mar=rep(0,4))
set.seed(1)
plot(mygraph, layout=layout_with_fr(mygraph))
legend("topleft",pch=19,
       col = c("lightgreen","lightgoldenrod","lightcoral","lightskyblue"),
       legend = c("Proteins","CpG sites","Clustered metabolites (positive node)",
                  "Clustered metabolites (negative node)"), cex = 0.6)
dev.off()

block_mat=GetBlockMatrix(pk=pk)
for (i in 1:10){
  print(block_names[i])
  print(sum(A[block_mat==i])/2)
}
