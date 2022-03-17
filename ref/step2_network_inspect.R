### CE Project -- Graphical Lasso Network Inspection
## Programme created by Rin on 22 April 

rm(list=ls())
project_path="/rds/general/project/hda_students_data/live/Group3/CE_Group3/Scripts"
setwd(project_path)

source("penalisation_functions.R")

### Selection proportion check ----
## Load dataset
proteins = readRDS("../Processed/proteins_denoised.rds")
meta_pos = readRDS("../Processed/meta_pos_denoised_selected.rds")
meta_neg = readRDS("../Processed/meta_neg_denoised_selected.rds")
cpgs = readRDS("../Processed/cpgs_denoised_selected.rds")

## Load dataset
out=readRDS("../Results/step2_calib_net_obj.rds")

argmax_id=GetArgmaxId(out)
selprop = out$selprop[,,argmax_id[1]]

pk=c(ncol(proteins), ncol(cpgs), ncol(meta_pos), ncol(meta_neg))

nblocks=sum(upper.tri(matrix(NA, ncol=length(pk), nrow=length(pk)), diag=TRUE))
blocks=matrix(NA, nrow=length(pk), ncol=length(pk))
blocks[upper.tri(blocks, diag=TRUE)]=1:nblocks
colnames(blocks) = c("proteins","cpgs","meta_pos","meta_neg")
rownames(blocks) = c("proteins","cpgs","meta_pos","meta_neg")

# CDCP1
out$selprop[1:71,"CDCP1",argmax_id[1,1]]
head(out$selprop[72:312,"CDCP1",argmax_id[2,1]] %>% sort(decreasing = T))
head(out$selprop[313:349,"CDCP1",argmax_id[4,1]] %>% sort(decreasing = T))
head(out$selprop[350:417,"CDCP1",argmax_id[7,1]] %>% sort(decreasing = T))

# CpG sites
apply(out$selprop[72:312,1:71,argmax_id[2,1]],2,max) %>% sort(decreasing=T) %>% head
apply(out$selprop[72:312,313:349,argmax_id[5,1]],2,max) %>% sort(decreasing=T) %>% head
apply(out$selprop[72:312,350:417,argmax_id[8,1]],2,max) %>% sort(decreasing=T) %>% head


out$selprop[72:312,"CCL23",argmax_id[2,1]] %>% sort(decreasing=T) %>% head
out$selprop[72:312,"P371",argmax_id[5,1]] %>% sort(decreasing=T) %>% head
out$selprop[72:312,"N1204",argmax_id[8,1]] %>% sort(decreasing=T) %>% head

# N1222
out$selprop[1:71,"N1222",argmax_id[7,1]]
head(out$selprop[72:312,"N1222",argmax_id[8,1]] %>% sort(decreasing = T))
head(out$selprop[313:349,"N1222",argmax_id[9,1]] %>% sort(decreasing = T))
head(out$selprop[350:417,"N1222",argmax_id[10,1]] %>% sort(decreasing = T))

# N1418
out$selprop[1:71,"N1418",argmax_id[7,1]]
head(out$selprop[72:312,"N1418",argmax_id[8,1]] %>% sort(decreasing = T))
head(out$selprop[313:349,"N1418",argmax_id[9,1]] %>% sort(decreasing = T))
head(out$selprop[350:417,"N1418",argmax_id[10,1]] %>% sort(decreasing = T))

GetArgmax(out)

## Adjacency matrix ----
adj_mat = readRDS("../Results/step2_adj_mat.rds")
dim(adj_mat)

head(sort(rowSums(adj_mat),decreasing = TRUE),10)
selected = rownames(adj_mat)[rowSums(adj_mat)>0]
cpgs_selected = selected[grepl("^cg",selected)]
genes = gsub(".*_","",cpgs_selected)
cat(genes)
