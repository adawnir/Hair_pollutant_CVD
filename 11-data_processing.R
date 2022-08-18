### Data processing: Composite score and reconstructing data sets

## Load data ----
pest = readRDS("../Data/PesticidesPCBBDEs.rds")
pahs = readRDS("../Data/PAHs.rds")
comb = readRDS("../Data/Combined.rds")

## Nutrient information ----
nutri = readRDS("../Processed/All_participants_nutri_naimp.rds")
mypca = PCA(nutri, graph = FALSE)
saveRDS(mypca, paste0("../Results/",filepaths[i],"_nutri_pca.rds"))
loadings = mypca$ind$coord
colnames(loadings) = paste0("NutriScore",1:ncol(loadings))
ev=mypca$eig[,2]/100

# Scree plot
pdf(paste0("../Figures/Nutrition_PCA_scree_plot.pdf"), width=5, height=5)
par(mar=c(5,5,1,1))
plot(ev, pch=19, col="navy", las=1, type="b", ylim=c(0,1),
     ylab="Proportion of explained variance", xlab="PCs")
points(cumsum(ev), pch=19, col="red", type="b")
legend("right", pch=19, col=c("navy", "red"),
       legend=c("Proportion of e.v.", "Cumulative proportion of e.v."))
dev.off()

## Reconstruct dataset ----
pest_XY = cbind(pest[,c(1:4,6:11)], loadings[pest$ID,1:2],pest[,c(5,12:19,23:27,30:ncol(pest))])
pahs_XY = cbind(pahs[,c(1:4,6:11)], loadings[pahs$ID,1:2],pahs[,c(5,12:19,23:27,30:ncol(pahs))])
comb_XY = cbind(comb[,c(1:4,6:11)], loadings[comb$ID,1:2],comb[,c(5,12:19,23:27,30:ncol(comb))])

saveRDS(pest_XY, "../Processed/PesticidesPCBBDEs_XY.rds")
write.csv(pest_XY, paste0("../Exports/PesticidesPCBBDEs_XY.csv"))
saveRDS(pahs_XY, "../Processed/PAHs_XY.rds")
write.csv(pahs_XY, paste0("../Exports/PAHs_XY.csv"))
saveRDS(comb_XY, "../Processed/Combined_XY.rds")
write.csv(comb_XY, paste0("../Exports/Combined_XY.csv"))
