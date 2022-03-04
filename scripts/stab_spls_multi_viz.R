
### Sumemr Project -- Stability selection sPLS with multivariate outcomes visualisation
## Created by Vivian on 05 August

rm(list=ls())
project_path="~/hair/scripts/"
setwd(project_path)


##### Load data--
pahs <- readRDS("../processed/imputation/imp_pahs.rds")
pest <- readRDS("../processed/imputation/imp_pest.rds")

unpen_ev_pest <- readRDS("../results/spls_multi/unpen_ev_pest.rds")
unpen_load_y_pest <- readRDS("../results/spls_multi/unpen_load_y_pest.rds")
unpen_ev_pahs <- readRDS("../results/spls_multi/unpen_ev_pahs.rds")
unpen_load_y_pahs <- readRDS("../results/spls_multi/unpen_load_y_pahs.rds")
out_x_pest <- readRDS("../results/spls_multi/out_x_pest.rds")
ev_x_pest <- readRDS("../results/spls_multi/ev_x_pest.rds")
out_xy_pest <- readRDS("../results/spls_multi/out_xy_pest.rds")
ev_xy_pest <- readRDS("../results/spls_multi/ev_xy_pest.rds")


##### Settings --
mycolours <- brewer.pal(12, "Paired")
outcomes <- c("WC", rownames(unpen_load_y_pest)[2:4], "TC", "LDL", "HDL", "TG", "FPG")
        

##### Unpenalised models --
pdf("../figures/spls_multi/unpen_ev_pest.pdf", height = 7, width = 7)
barplot(height=t(unpen_ev_pest$ev_each_Y_by_X), beside = T,
        cex.axis = 0.7, cex.names=0.7, main = "Explained Variance",
        cex.main = 0.7,
        col=rep(mycolours[1:8], times=3),
        names = c("C1X(1.37%)", "C2X(1.45%)", "C1X(0.52%)"))
legend("topright", bty = "n", cex = 0.7,
       legend = outcomes, 
       fill = mycolours[1:8])
dev.off()

test <- unpen_ev_pest$ev_each_Y_by_X[1,]
pdf("../figures/spls_multi/unpen_ev_pest_1.pdf", height = 7, width = 7)
par(mar = c(5, 3, 3, 2))
barplot(height=test, 
        cex.axis = 0.7, cex.names=0.7, main = "Explained Variance",
        cex.main = 0.7,
        col=mycolours[1:9],
        names = outcomes)
dev.off()


pdf("../figures/spls_multi/unpen_ev_pahs.pdf", height = 5, width = 5)
barplot(height=t(unpen_ev_pahs$ev_each_Y_by_X), beside = T,
        cex.axis = 0.7, cex.names=0.7, main = "Explained Variance",
        cex.main = 0.7,
        col=rep(mycolours[1:8], times=3),
        names = c("C1X(0.95%)", "C2X(0.66%)", "C1X(0.39%)"))
legend("topright", bty = "n", cex = 0.7,
       legend = outcomes, 
       fill = mycolours[1:8])
dev.off()


##### sPLS models sparsity on x --
ind <- rownames(out_x_pest$summary_full[which.max(out_x_pest$summary_full[,4]),])
pix_pest <- out_x_pest$summary_full$pix[which.max(out_x_pest$summary_full[,4])]
selprop_x_pest <- out_x_pest$selpropX_full[12,]

pdf("../figures/spls_multi/selprop_x_pest.pdf", height = 7, width = 13)
par(mar = c(10, 2, 2, 6))
plot(selprop_x_pest, type = "h", xaxt="n", yaxt="n", ylab="", xlab="", 
     lwd=2.5, col = "darkgoldenrod3")
abline(h=pix_pest, lty=4, lwd=1, col="darkred")
axis(side=4, at=axTicks(2), cex.axis=0.7)
mtext(side=4, text="Selection Proportion", line=2, cex.lab=0.7)
labels_pest <- c("CountryBelgium", "CountryFrance",
                 colnames(pest)[1:4], 
                 "p,p-DDT", "p,p-DDE", 
                 "Alpha-endosulfan", "Beta-endosulfan",
                 "Beta-HCH", "Gamma-HCH",
                 colnames(pest)[11:33])
axis(side=1, at=1:35 , labels=labels_pest, las=2, cex.axis=0.8)
dev.off()

pdf("../figures/spls_multi/ev_x_pest.pdf", height = 7, width = 7)
par(mar = c(5, 3, 3, 2))
barplot(height=ev_x_pest$ev_each_Y_by_X[1,], beside = T,
        cex.axis = 0.7, cex.names=0.7, main = "Explained Variance",
        cex.main = 0.7, 
        col=mycolours[1:9],
        names = outcomes)
dev.off()


##### sPLS models sparsity on x and y --
ind_xy <- rownames(out_xy_pest$summary_full[which.max(out_xy_pest$summary_full[,6]),])
pix_pest_xy <- out_xy_pest$summary_full$pix[which.max(out_xy_pest$summary_full[,6])]
piy_pest_xy <- out_xy_pest$summary_full$piy[which.max(out_xy_pest$summary_full[,6])]
selprop_xy_pest_x <- out_xy_pest$selpropX_full[249,]
selprop_xy_pest_y <- out_xy_pest$selpropY_full[249,]

pdf("../figures/spls_multi/selprop_xy_pest_x.pdf", height = 7, width = 13)
par(mar = c(10, 2, 2, 6))
plot(selprop_xy_pest_x, type = "h", xaxt="n", yaxt="n", ylab="", xlab="", 
     lwd=2.5, col = "darkgoldenrod3")
abline(h=pix_pest_xy, lty=4, lwd=1, col="darkred")
axis(side=4, at=axTicks(2), cex.axis=0.7)
mtext(side=4, text="Selection Proportion", line=2, cex.lab=0.7)
labels_pest <- c("CountryBelgium", "CountryFrance",
                 colnames(pest)[1:4], 
                 "p,p-DDT", "p,p-DDE", 
                 "Alpha-endosulfan", "Beta-endosulfan",
                 "Beta-HCH", "Gamma-HCH",
                 colnames(pest)[11:33])
axis(side=1, at=1:35 , labels=labels_pest, las=2, cex.axis=0.8)
dev.off()


pdf("../figures/spls_multi/ev_xy_pest.pdf", height = 7, width = 7)
par(mar = c(5, 3, 3, 2))
barplot(height=c(ev_xy_pest$ev_each_Y_by_X[1,1:4], 0, ev_xy_pest$ev_each_Y_by_X[1,5:8]), 
        beside = T,
        cex.axis = 0.7, cex.names=0.7, main = "Explained Variance",
        cex.main = 0.7, 
        col=mycolours[1:9],
        names = outcomes)
dev.off()



##### Scatter plot --
foo2 = function(l1, l2, order){
        x=data.frame(l1)
        x[,2] <- l2[match(rownames(x),names(l2))]
        x = slice(x, match(order,rownames(x)))
        df=data.frame(sel_x=unlist(x[,1]),sel_xy=unlist(x[,2])) # change accordingly
}


selprop_pest <- data.frame(selprop_x_pest)
selprop_pest[,2] <- selprop_xy_pest_x[match(rownames(selprop_pest),names(selprop_xy_pest_x))]
colnames(selprop_pest) <- c("sel_x", "sel_xy")


#coef_lung$var_cat=rep(variable_cat)
selprop_pest$label=rep(1:35)
labels_pest <- c("CountryBelgium", "CountryFrance",
                 colnames(pest)[1:4], 
                 "p,p-DDT", "p,p-DDE", 
                 colnames(pest)[7:33])
selprop_pest$label <- labels_pest

pix_pest_xy
pix_pest
id1 <- ifelse(selprop_pest$sel_x[1:35]<0.89 & selprop_pest$sel_xy[1:35]<0.88, "N", "Y") 
selprop_pest$label_ny <- c(id1)

xlim=c(0, 1)
ylim=c(0, 1)
p3=ggplot(selprop_pest, aes(sel_x, sel_xy,
                            label=ifelse(label_ny == "N","",label))) +
        geom_abline(slope = 1,linetype = "dotted",colour = "grey") +
        geom_vline(aes(xintercept = 0.86),linetype = "dashed",colour = "darkred") +
        geom_hline(aes(yintercept = 0.87),linetype = "dashed",colour = "darkred") +
        geom_point(size = 3, col = "goldenrod3") +
        geom_text_repel(color = darken("goldenrod3", amount = 0.5),
                        nudge_y = 0.03,
                        nudge_x = 0.02,
                        size=3.5, segment.color="grey",
                        segment.size=0.2) + 
        xlab("Selection Proportion (sparsity on predictors)")+
        ylab("Selection Proportion (sparsity on predictors and outcomes)") +
        labs(title = "POPs and non-persistent pesticides") +
        xlim(xlim) +
        ylim(ylim) +
        theme_bw() +
        theme(legend.position = "none")

cairo_pdf("../figures/spls_multi/spls_multi_selprop.pdf", width=7, height=7)
p3
dev.off()


##### Loadings --

ld_x_pest <- readRDS("../results/spls_multi/unpen_load_x_pest.rds")
ld_y_pest <- readRDS("../results/spls_multi/unpen_load_y_pest.rds")
ld_x_x_pest <- readRDS("../results/spls_multi/loading_x_x_pest.rds")
ld_x_y_pest <- readRDS("../results/spls_multi/loading_x_y_pest.rds")
ld_xy_x_pest <- readRDS("../results/spls_multi/loading_xy_x_pest.rds")
ld_xy_y_pest <- readRDS("../results/spls_multi/loading_xy_y_pest.rds")

