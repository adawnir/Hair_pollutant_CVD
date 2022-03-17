
### Sumemr Project -- Stability selection sPLS with multivariate outcomes inspection
## Created by Vivian on 27 July

rm(list=ls())
project_path="~/hair/scripts/"
setwd(project_path)


##### Load data set --
pest_load_x <- readRDS("../results/spls_multi/loading_x_pest_multi_redu.rds") 
pest_load_y <- readRDS("../results/spls_multi/loading_y_pest_multi_redu.rds") 
view(pest_load_x)
unclass(pest_load_x)
unclass(pest_load_y)
pest_ev <- readRDS("../results/spls_multi/ev_X_X_pest_multi_redu.rds")

pahs_load_x <- readRDS("../results/spls_multi/loading_x_pahs_multi_redu.rds") 
pahs_load_y <- readRDS("../results/spls_multi/loading_y_pahs_multi_redu.rds") 
pahs_ev <- readRDS("../results/spls_multi/ev_X_X_pahs_multi_redu.rds")

unclass(pahs_load_x)
unclass(pahs_load_y)


