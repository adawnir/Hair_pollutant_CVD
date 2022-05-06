
### Sumemr Project -- Stability selection sPLS with multivariate outcomes visualisation
## Created by Vivian on 05 August

rm(list=ls())
project_path="~/hair/scripts/"
setwd(project_path)


##### Load data--
pahs <- readRDS("../processed/imputation/imp_pahs.rds")
pest <- readRDS("../processed/imputation/imp_pest.rds")

out_xy_pest_l <- readRDS("../results/strat_country_spls_multi/out_xy_pest_l.rds")
ev_xy_pest_l <- readRDS("../results/strat_country_spls_multi/ev_xy_pest_l.rds")
ld_xy_x_pest_l <- readRDS("../results/strat_country_spls_multi/loading_xy_x_pest_l.rds")
ld_xy_y_pest_l <- readRDS("../results/strat_country_spls_multi/loading_xy_y_pest_l.rds")

out_xy_pest_b <- readRDS("../results/strat_country_spls_multi/out_xy_pest_b.rds")
ev_xy_pest_b <- readRDS("../results/strat_country_spls_multi/ev_xy_pest_b.rds")
ld_xy_x_pest_b <- readRDS("../results/strat_country_spls_multi/loading_xy_x_pest_b.rds")
ld_xy_y_pest_b <- readRDS("../results/strat_country_spls_multi/loading_xy_y_pest_b.rds")

out_xy_pest_f <- readRDS("../results/strat_country_spls_multi/out_xy_pest_f.rds")
ev_xy_pest_f <- readRDS("../results/strat_country_spls_multi/ev_xy_pest_f.rds")
ld_xy_x_pest_f <- readRDS("../results/strat_country_spls_multi/loading_xy_x_pest_f.rds")
ld_xy_y_pest_f <- readRDS("../results/strat_country_spls_multi/loading_xy_y_pest_f.rds")


##### Settings --
mycolours <- brewer.pal(12, "Paired")
outcomes <- c("WC", "BMI", "SBP", "DBP", "TC", "LDL", "HDL", "TG", "FPG")



