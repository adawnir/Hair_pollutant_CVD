
### Sumemr Project -- pollutant annotation
## Created by Vivian on 124 June

rm(list=ls())
project_path="~/hair/scripts"
setwd(project_path)


##### Load data --
mydata <- readRDS("../data/PesticidesPCBBDEs.rds")
colnames(mydata)[16:48]


##### Make annotation
variable_cat <- c(rep("PCB", 3),
                 rep("BDE", 1),
                 rep("Organochlorine", 9),
                 rep("Organophosphate", 3),
                 rep("Metabolites_of_Organophosphate", 6),
                 rep("Pyrethroid", 2),
                 rep("Metabolits_of_Pyrethroid", 2),
                 rep("Phenylpyrazole", 2),
                 rep("Carbamate", 2),
                 rep("Carboxamide", 1),
                 rep("Dinitroaniline", 1),
                 rep("Oxadiazole", 1))

sub_anno <- structure(list(classes = c(variable_cat)), .Names = "Family", row.names = c(colnames(mydata)[16:48]),
                      class = "data.frame")

saveRDS(sub_anno, "../data/pest_annotation.rds")

