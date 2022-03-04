
### Sumemr Project -- Graphical Lasso Network inspection
## Created by Vivian on 29 June 

rm(list=ls())
project_path="~/hair/scripts"
setwd(project_path)

source("penalisation_functions.R")

##### Load dataset
out_pest <- readRDS("../results/Network/calib_network_obj_pest.rds")
adj_mat_pest = readRDS("../results/Network/adj_mat_pest.rds")

out_pahs <- readRDS("../results/Network/calib_network_obj_pahs.rds")
adj_mat_pahs <- readRDS("../results/Network/adj_mat_pahs.rds")

out_pest_L <- readRDS("../results/Network/calib_network_obj_pest_L.rds")
GetArgmax(out_pest_L)

out_pest_B <- readRDS("../results/Network/calib_network_obj_pest_B.rds")
GetArgmax(out_pest_B)

out_pest_F <- readRDS("../results/Network/calib_network_obj_pest_F.rds")
GetArgmax(out_pest_F)

out_pahs_L <- readRDS("../results/Network/calib_network_obj_pahs_L.rds")
GetArgmax(out_pahs_L)

out_pahs_B <- readRDS("../results/Network/calib_network_obj_pahs_B.rds")
GetArgmax(out_pahs_B)


##### Selection proportion --
### Pest
argmax_id <- GetArgmaxId(out_pest)
selprop_pest  <- out_pest$selprop[,,argmax_id[1]]
GetArgmax(out_pest)

# γ-HCH
out_pest$selprop[, "gamma_HCH", argmax_id[1,1]] %>% sort(decreasing = T)

# Cl2CA  
out_pest$selprop[, "Cl2CA", argmax_id[1,1]] %>% sort(decreasing = T)

# Diflufenican 
out_pest$selprop[, "Fipronil", argmax_id[1,1]] %>% sort(decreasing = T)

# TCPy   
out_pest$selprop[, "PCP", argmax_id[1,1]] %>% sort(decreasing = T)

# PBC series
out_pest$selprop[, "PCB_138", argmax_id[1,1]] %>% sort(decreasing = T)
out_pest$selprop[, "PCB_153", argmax_id[1,1]] %>% sort(decreasing = T)
out_pest$selprop[, "PCB_180", argmax_id[1,1]] %>% sort(decreasing = T)


### PAHs
argmax_id <- GetArgmaxId(out_pahs)
selprop_pahs <- out_pahs$selprop[,,argmax_id[1]]
GetArgmax(out_pahs)

# Cotinine (ng/mg)
out_pahs$selprop[, "Cotinine", argmax_id[1,1]] %>% sort(decreasing = T)

##### Adjacency matrix --
dim(adj_mat_pest)

sort(rowSums(adj_mat_pest),decreasing = TRUE)
selected = rownames(adj_mat_pest)[rowSums(adj_mat_pest)>0]

sort(rowSums(adj_mat_pahs),decreasing = TRUE)

