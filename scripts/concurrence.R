
### Sumemr Project --  Concurrence of disease(risk factors)
## Programme created by Vivian on 10 July

rm(list=ls())
project_path="~/hair/scripts"
setwd(project_path)

#install.packages("VennDiagram")

##### Load data --
pest <- readRDS("../processed/uni_multi_pest.rds")
pahs <- readRDS("../processed/uni_multi_pahs.rds")
over <- readRDS("../processed/uni_multi_overlap.rds")


##### Extract data--
colnames(pest)
pest <- pest %>% dplyr::select(c(1:3,5,17:49, 51:55))
colnames(pahs)
pahs <- pahs %>% dplyr::select(c(1:3,5,17:29, 31:35))
colnames(over)
over <- over %>% dplyr::select(c(1:3,5,17:62, 64:68))
class(pest$identifiant)

##### Calculated
ob <- pest$identifiant[which(pest$Obesity == "1")]
ld <- pest$identifiant[which(pest$Lipid_disorder == "1")]
hy <- pest$identifiant[which(pest$hypertension == "1")]
di <- pest$identifiant[which(pest$diabetes == "1")]
ms <- pest$identifiant[which(pest$MS== "1")]

ob1 <- pahs$identifiant[which(pahs$Obesity == "1")]
ld1 <- pahs$identifiant[which(pahs$Lipid_disorder == "1")]
hy1 <- pahs$identifiant[which(pahs$hypertension == "1")]
di1 <- pahs$identifiant[which(pahs$diabetes == "1")]
ms1 <- pahs$identifiant[which(pahs$MS== "1")]

ob2 <- over$identifiant[which(over$Obesity == "1")]
ld2 <- over$identifiant[which(over$Lipid_disorder == "1")]
hy2 <- over$identifiant[which(over$hypertension == "1")]
di2 <- over$identifiant[which(over$diabetes == "1")]
ms2 <- over$identifiant[which(over$MS== "1")]


##### Union --
length(union(ms, union(di, union(hy,union(ob, ld)))))   # 802
length(union(ms1, union(di1, union(hy1,union(ob1, ld1)))))   # 508


##### Presented as figures
### Pest
suppressPackageStartupMessages(library(VennDiagram))
#dir.create("figures", showWarnings = FALSE)
rf = list(Obesity = ob, Lipid_disorder = ld, Hypertension = hy, 
          Diabetes = di, Metabolic_syndrome = ms)
colors = c("skyblue","forestgreen", "tomato", "gold", "navy")
names = paste0(c("Obesity", "Lipid disorder", "Hypertension", "Diabetes Mellitus", "Metabolic syndrome"), 
               " (N=", unlist(lapply(rf, length)), ")")
venn.diagram(rf, filename = "../figures/Venn/Venn_pest.png", fill = colors, 
             print.mode=c("raw","percent"), cex = 0.5,
             category.names = names, imagetype = "png", 
             cat.just = list(c(0.5, 1), c(0, -6), c(0, 0), c(0.5, 0.5), c(1, -5)))

### Pahs
rf1 = list(Obesity = ob1, Lipid_disorder = ld1, Hypertension = hy1, 
          Diabetes = di1, Metabolic_syndrome = ms1)
colors = c("skyblue","forestgreen", "tomato", "gold", "navy")
names = paste0(c("Obesity", "Lipid disorder", "Hypertension", "Diabetes Mellitus", "Metabolic syndrome"), 
               " (N=", unlist(lapply(rf1, length)), ")")
venn.diagram(rf1, filename = "../figures/Venn/Venn_pahs.png", fill = colors, 
             print.mode=c("raw","percent"), cex = 0.5,
             category.names = names, imagetype = "png", 
             cat.just = list(c(0.5, 1), c(0, -6), c(0, 0), c(0.5, 0.5), c(1, -5)))

### Overlap
rf2 = list(Obesity = ob2, Lipid_disorder = ld2, Hypertension = hy2, 
           Diabetes = di2, Metabolic_syndrome = ms2)
colors = c("tomato", "gold", "forestgreen", "skyblue","navy")
names = paste0(c("Obesity", "Lipid_disorder", "Hypertension", "Diabetes", "Metabolic_syndrome"), 
               " (N=", unlist(lapply(rf2, length)), ")")
venn.diagram(rf2, filename = "../figures/Venn/Venn_over.png", fill = colors, 
             category.names = names, imagetype = "png", 
             cat.just = list(c(0.5, 1), c(0, -6), c(0, 0), c(0.5, 0.5), c(1, -5)))


