
### Sumemr Project --  univariate analysis considering categorical variables visualisation (Forest plot and Manhattan plot)
## Programme created by Vivian on 22 July

rm(list=ls())
project_path="~/hair/scripts"
setwd(project_path)

##### Loading packages
library(RColorBrewer)
library(tidyverse)
library(plotrix)
library(colorspace)
library(ggrepel)
#install.packages("viridis") 
library("viridis") 

##### Load data --
pest <- readRDS("../cat_results/pest_cat.rds")
pahs <- readRDS("../cat_results/pahs_cat.rds")

manhat_pahs_l <- as.data.frame(readRDS("../cat_results/manhattan_pahs_l.rds"))
manhat_pahs_b <- as.data.frame(readRDS("../cat_results/manhattan_pahs_b.rds"))
manhat_pest_l <- as.data.frame(readRDS("../cat_results/manhattan_pest_l.rds"))
manhat_pest_b <- as.data.frame(readRDS("../cat_results/manhattan_pest_b.rds"))
manhat_pest_f <- as.data.frame(readRDS("../cat_results/manhattan_pest_f.rds"))

manhat_pahs_cat_l <- as.data.frame(readRDS("../cat_results/manhattan_pahs_cat_l.rds"))
manhat_pahs_cat_b <- as.data.frame(readRDS("../cat_results/manhattan_pahs_cat_b.rds"))
manhat_pest_cat_l <- as.data.frame(readRDS("../cat_results/manhattan_pest_cat_l.rds"))
manhat_pest_cat_b <- as.data.frame(readRDS("../cat_results/manhattan_pest_cat_b.rds"))
manhat_pest_cat_f <- as.data.frame(readRDS("../cat_results/manhattan_pest_cat_f.rds"))

forest_pahs_l <- as.data.frame(readRDS("../cat_results/forest_pahs_l.rds"))
forest_pahs_b <- as.data.frame(readRDS("../cat_results/forest_pahs_b.rds"))
forest_pest_l <- as.data.frame(readRDS("../cat_results/forest_pest_l.rds"))
forest_pest_b <- as.data.frame(readRDS("../cat_results/forest_pest_b.rds"))
forest_pest_f <- as.data.frame(readRDS("../cat_results/forest_pest_f.rds"))

forest_pahs_cat_l <- as.data.frame(readRDS("../cat_results/forest_pahs_cat_l.rds"))
forest_pahs_cat_b <- as.data.frame(readRDS("../cat_results/forest_pahs_cat_b.rds"))
forest_pest_cat_l <- as.data.frame(readRDS("../cat_results/forest_pest_cat_l.rds"))
forest_pest_cat_b <- as.data.frame(readRDS("../cat_results/forest_pest_cat_b.rds"))
forest_pest_cat_f <- as.data.frame(readRDS("../cat_results/forest_pest_cat_f.rds"))

##### Change data structure --
myorder <- c("obesity", "lipid_disorder", "hypertension", "diabetes","MS")
manhat_pahs_l <- manhat_pahs_l[, myorder]
manhat_pahs_b <- manhat_pahs_b[, myorder]
manhat_pest_l <- manhat_pest_l[, myorder]
manhat_pest_b <- manhat_pest_b[, myorder]
manhat_pest_f <- manhat_pest_f[, myorder]

manhat_pest_cat_l <- manhat_pest_cat_l[, myorder]
manhat_pest_cat_b <- manhat_pest_cat_b[, myorder]
manhat_pest_cat_f <- manhat_pest_cat_f[, myorder]
manhat_pahs_cat_l <- manhat_pahs_cat_l[, myorder]
manhat_pahs_cat_b <- manhat_pahs_cat_b[, myorder]

### pest L
manhat_pest_cat_l_tmp <- manhat_pest_cat_l %>%
  add_column(Empty1 = NA, .after="obesity") %>% 
  add_column(Empty2 = NA, .after="lipid_disorder") %>%
  add_column(Empty3 = NA, .after="hypertension") %>%
  add_column(Empty4 = NA, .after="diabetes") %>%
  add_column(Empty5 = NA, .after="MS")  %>%
  add_column(Empty0 = NA, .before = "obesity")

manhat_pest_cat_l_tmp1 <- data.frame(t(manhat_pest_cat_l_tmp))
manhat_pest_cat_l_1 <- unlist(manhat_pest_cat_l_tmp1)

manhat_pest_l_tmp <- manhat_pest_l %>%
  add_column(Empty1 = NA, .after="obesity") %>% 
  add_column(Empty2 = NA, .after="lipid_disorder") %>%
  add_column(Empty3 = NA, .after="hypertension") %>%
  add_column(Empty4 = NA, .after="diabetes") %>%
  add_column(Empty5 = NA, .after="MS")  %>%
  add_column(Empty0 = NA, .before = "obesity")

manhat_pest_l_tmp1 <- data.frame(t(manhat_pest_l_tmp))
manhat_pest_l_1 <- unlist(manhat_pest_l_tmp1)

### pest B
manhat_pest_cat_b_tmp <- manhat_pest_cat_b %>%
  add_column(Empty1 = NA, .after="obesity") %>% 
  add_column(Empty2 = NA, .after="lipid_disorder") %>%
  add_column(Empty3 = NA, .after="hypertension") %>%
  add_column(Empty4 = NA, .after="diabetes") %>%
  add_column(Empty5 = NA, .after="MS")  %>%
  add_column(Empty0 = NA, .before = "obesity")

manhat_pest_cat_b_tmp1 <- data.frame(t(manhat_pest_cat_b_tmp))
manhat_pest_cat_b_1 <- unlist(manhat_pest_cat_b_tmp1)

manhat_pest_b_tmp <- manhat_pest_b %>%
  add_column(Empty1 = NA, .after="obesity") %>% 
  add_column(Empty2 = NA, .after="lipid_disorder") %>%
  add_column(Empty3 = NA, .after="hypertension") %>%
  add_column(Empty4 = NA, .after="diabetes") %>%
  add_column(Empty5 = NA, .after="MS")  %>%
  add_column(Empty0 = NA, .before = "obesity")

manhat_pest_b_tmp1 <- data.frame(t(manhat_pest_b_tmp))
manhat_pest_b_1 <- unlist(manhat_pest_b_tmp1)


### pest F
manhat_pest_cat_f_tmp <- manhat_pest_cat_f %>%
  add_column(Empty1 = NA, .after="obesity") %>% 
  add_column(Empty2 = NA, .after="lipid_disorder") %>%
  add_column(Empty3 = NA, .after="hypertension") %>%
  add_column(Empty4 = NA, .after="diabetes") %>%
  add_column(Empty5 = NA, .after="MS")  %>%
  add_column(Empty0 = NA, .before = "obesity")

manhat_pest_cat_f_tmp1 <- data.frame(t(manhat_pest_cat_f_tmp))
manhat_pest_cat_f_1 <- unlist(manhat_pest_cat_f_tmp1)
manhat_pest_cat_f_1 <- c(manhat_pest_cat_f_1[1:121], rep(NA, 11), manhat_pest_cat_f_1[122:352])

manhat_pest_f_tmp <- manhat_pest_f %>%
  add_column(Empty1 = NA, .after="obesity") %>% 
  add_column(Empty2 = NA, .after="lipid_disorder") %>%
  add_column(Empty3 = NA, .after="hypertension") %>%
  add_column(Empty4 = NA, .after="diabetes") %>%
  add_column(Empty5 = NA, .after="MS")  %>%
  add_column(Empty0 = NA, .before = "obesity")

manhat_pest_f_tmp1 <- data.frame(t(manhat_pest_f_tmp))
manhat_pest_f_1 <- unlist(manhat_pest_f_tmp1)


### pahs L
manhat_pahs_cat_l_tmp <- manhat_pahs_cat_l %>%
  add_column(Empty1 = NA, .after="obesity") %>% 
  add_column(Empty2 = NA, .after="lipid_disorder") %>%
  add_column(Empty3 = NA, .after="hypertension") %>%
  add_column(Empty4 = NA, .after="diabetes") %>%
  add_column(Empty5 = NA, .after="MS")  %>%
  add_column(Empty0 = NA, .before = "obesity")

manhat_pahs_cat_l_tmp1 <- data.frame(t(manhat_pahs_cat_l_tmp))
manhat_pahs_cat_l_1 <- unlist(manhat_pahs_cat_l_tmp1)

manhat_pahs_l_tmp <- manhat_pahs_l %>%
  add_column(Empty1 = NA, .after="obesity") %>% 
  add_column(Empty2 = NA, .after="lipid_disorder") %>%
  add_column(Empty3 = NA, .after="hypertension") %>%
  add_column(Empty4 = NA, .after="diabetes") %>%
  add_column(Empty5 = NA, .after="MS")  %>%
  add_column(Empty0 = NA, .before = "obesity")

manhat_pahs_l_tmp1 <- data.frame(t(manhat_pahs_l_tmp))
manhat_pahs_l_1 <- unlist(manhat_pahs_l_tmp1)


### pahs B
manhat_pahs_cat_b_tmp <- manhat_pahs_cat_b %>%
  add_column(Empty1 = NA, .after="obesity") %>% 
  add_column(Empty2 = NA, .after="lipid_disorder") %>%
  add_column(Empty3 = NA, .after="hypertension") %>%
  add_column(Empty4 = NA, .after="diabetes") %>%
  add_column(Empty5 = NA, .after="MS")  %>%
  add_column(Empty0 = NA, .before = "obesity")

manhat_pahs_cat_b_tmp1 <- data.frame(t(manhat_pahs_cat_b_tmp))
manhat_pahs_cat_b_1 <- unlist(manhat_pahs_cat_b_tmp1)
manhat_pahs_cat_b_1 <- c(rep(NA, 11), manhat_pahs_cat_b_1[1:22], rep(NA, 11),
                         manhat_pahs_cat_b_1[23:33], rep(NA, 11),  manhat_pahs_cat_b_1[34:55],
                         rep(NA, 11), manhat_pahs_cat_b_1[56:99])

manhat_pahs_b_tmp <- manhat_pahs_b %>%
  add_column(Empty1 = NA, .after="obesity") %>% 
  add_column(Empty2 = NA, .after="lipid_disorder") %>%
  add_column(Empty3 = NA, .after="hypertension") %>%
  add_column(Empty4 = NA, .after="diabetes") %>%
  add_column(Empty5 = NA, .after="MS")  %>%
  add_column(Empty0 = NA, .before = "obesity")

manhat_pahs_b_tmp1 <- data.frame(t(manhat_pahs_b_tmp))
manhat_pahs_b_1 <- unlist(manhat_pahs_b_tmp1)


### OR
# pahs L
or_pahs_cat_l_tmp <- forest_pahs_cat_l %>% 
  dplyr::select(c("or_pahs_cat_l_ob", "or_pahs_cat_l_ld", "or_pahs_cat_l_hyper", 
                  "or_pahs_cat_l_dia", "or_pahs_cat_l_ms"))
or_pahs_cat_l_tmp <- or_pahs_cat_l_tmp %>%
  add_column(Empty1 = NA, .after="or_pahs_cat_l_ob") %>% 
  add_column(Empty2 = NA, .after="or_pahs_cat_l_ld") %>%
  add_column(Empty3 = NA, .after="or_pahs_cat_l_hyper") %>%
  add_column(Empty4 = NA, .after="or_pahs_cat_l_dia") %>%
  add_column(Empty5 = NA, .after="or_pahs_cat_l_ms")  %>%
  add_column(Empty0 = NA, .before = "or_pahs_cat_l_ob")  
or_pahs_cat_l_tmp1 <- data.frame(t(or_pahs_cat_l_tmp))
or_pahs_cat_l <- unlist(or_pahs_cat_l_tmp1)

up_pahs_cat_l_tmp <- forest_pahs_cat_l %>% 
  dplyr::select(c("u95_pahs_cat_l_ob", "u95_pahs_cat_l_ld", "u95_pahs_cat_l_hyper", 
                  "u95_pahs_cat_l_dia", "u95_pahs_cat_l_ms"))
up_pahs_cat_l_tmp <- up_pahs_cat_l_tmp %>%
  add_column(Empty1 = NA, .after="u95_pahs_cat_l_ob") %>% 
  add_column(Empty2 = NA, .after="u95_pahs_cat_l_ld") %>%
  add_column(Empty3 = NA, .after="u95_pahs_cat_l_hyper") %>%
  add_column(Empty4 = NA, .after="u95_pahs_cat_l_dia") %>%
  add_column(Empty5 = NA, .after="u95_pahs_cat_l_ms")  %>%
  add_column(Empty0 = NA, .before = "u95_pahs_cat_l_ob")  
up_pahs_cat_l_tmp1 <- data.frame(t(up_pahs_cat_l_tmp))
up_pahs_cat_l <- unlist(up_pahs_cat_l_tmp1)

lo_pahs_cat_l_tmp <- forest_pahs_cat_l %>% 
  dplyr::select(c("l95_pahs_cat_l_ob", "l95_pahs_cat_l_ld", "l95_pahs_cat_l_hyper", 
                  "l95_pahs_cat_l_dia", "l95_pahs_cat_l_ms"))
lo_pahs_cat_l_tmp <- lo_pahs_cat_l_tmp %>%
  add_column(Empty1 = NA, .after="l95_pahs_cat_l_ob") %>% 
  add_column(Empty2 = NA, .after="l95_pahs_cat_l_ld") %>%
  add_column(Empty3 = NA, .after="l95_pahs_cat_l_hyper") %>%
  add_column(Empty4 = NA, .after="l95_pahs_cat_l_dia") %>%
  add_column(Empty5 = NA, .after="l95_pahs_cat_l_ms")  %>%
  add_column(Empty0 = NA, .before = "l95_pahs_cat_l_ob")  
lo_pahs_cat_l_tmp1 <- data.frame(t(lo_pahs_cat_l_tmp))
lo_pahs_cat_l <- unlist(lo_pahs_cat_l_tmp1)


or_pahs_l_tmp <- forest_pahs_l %>% 
  dplyr::select(c("or_pahs_l_ob", "or_pahs_l_ld", "or_pahs_l_hyper", 
                  "or_pahs_l_dia", "or_pahs_l_ms"))
or_pahs_l_tmp <- or_pahs_l_tmp %>%
  add_column(Empty1 = NA, .after="or_pahs_l_ob") %>% 
  add_column(Empty2 = NA, .after="or_pahs_l_ld") %>%
  add_column(Empty3 = NA, .after="or_pahs_l_hyper") %>%
  add_column(Empty4 = NA, .after="or_pahs_l_dia") %>%
  add_column(Empty5 = NA, .after="or_pahs_l_ms")  %>%
  add_column(Empty0 = NA, .before = "or_pahs_l_ob")  
or_pahs_l_tmp1 <- data.frame(t(or_pahs_l_tmp))
or_pahs_l <- unlist(or_pahs_l_tmp1)

up_pahs_l_tmp <- forest_pahs_l %>% 
  dplyr::select(c("u95_pahs_l_ob", "u95_pahs_l_ld", "u95_pahs_l_hyper", 
                  "u95_pahs_l_dia", "u95_pahs_l_ms"))
up_pahs_l_tmp <- up_pahs_l_tmp %>%
  add_column(Empty1 = NA, .after="u95_pahs_l_ob") %>% 
  add_column(Empty2 = NA, .after="u95_pahs_l_ld") %>%
  add_column(Empty3 = NA, .after="u95_pahs_l_hyper") %>%
  add_column(Empty4 = NA, .after="u95_pahs_l_dia") %>%
  add_column(Empty5 = NA, .after="u95_pahs_l_ms")  %>%
  add_column(Empty0 = NA, .before = "u95_pahs_l_ob")  
up_pahs_l_tmp1 <- data.frame(t(up_pahs_l_tmp))
up_pahs_l <- unlist(up_pahs_l_tmp1)

lo_pahs_l_tmp <- forest_pahs_l %>% 
  dplyr::select(c("l95_pahs_l_ob", "l95_pahs_l_ld", "l95_pahs_l_hyper", 
                  "l95_pahs_l_dia", "l95_pahs_l_ms"))
lo_pahs_l_tmp <- lo_pahs_l_tmp %>%
  add_column(Empty1 = NA, .after="l95_pahs_l_ob") %>% 
  add_column(Empty2 = NA, .after="l95_pahs_l_ld") %>%
  add_column(Empty3 = NA, .after="l95_pahs_l_hyper") %>%
  add_column(Empty4 = NA, .after="l95_pahs_l_dia") %>%
  add_column(Empty5 = NA, .after="l95_pahs_l_ms")  %>%
  add_column(Empty0 = NA, .before = "l95_pahs_l_ob")  
lo_pahs_l_tmp1 <- data.frame(t(lo_pahs_l_tmp))
lo_pahs_l <- unlist(lo_pahs_l_tmp1)


# pahs B
or_pahs_cat_b_tmp <- forest_pahs_cat_b %>% 
  dplyr::select(c("or_pahs_cat_b_ob", "or_pahs_cat_b_ld", "or_pahs_cat_b_hyper", 
                  "or_pahs_cat_b_dia", "or_pahs_cat_b_ms"))
or_pahs_cat_b_tmp <- or_pahs_cat_b_tmp %>%
  add_column(Empty1 = NA, .after="or_pahs_cat_b_ob") %>% 
  add_column(Empty2 = NA, .after="or_pahs_cat_b_ld") %>%
  add_column(Empty3 = NA, .after="or_pahs_cat_b_hyper") %>%
  add_column(Empty4 = NA, .after="or_pahs_cat_b_dia") %>%
  add_column(Empty5 = NA, .after="or_pahs_cat_b_ms")  %>%
  add_column(Empty0 = NA, .before = "or_pahs_cat_b_ob")  
or_pahs_cat_b_tmp1 <- data.frame(t(or_pahs_cat_b_tmp))
or_pahs_cat_b <- unlist(or_pahs_cat_b_tmp1)
or_pahs_cat_b <- c(rep(NA, 11), or_pahs_cat_b[1:22], rep(NA, 11), or_pahs_cat_b[23:33],
                   rep(NA, 11), or_pahs_cat_b[34:55], rep(NA, 11), 
                   or_pahs_cat_b[56:length(or_pahs_cat_b)])

up_pahs_cat_b_tmp <- forest_pahs_cat_b %>% 
  dplyr::select(c("u95_pahs_cat_b_ob", "u95_pahs_cat_b_ld", "u95_pahs_cat_b_hyper", 
                  "u95_pahs_cat_b_dia", "u95_pahs_cat_b_ms"))
up_pahs_cat_b_tmp <- up_pahs_cat_b_tmp %>%
  add_column(Empty1 = NA, .after="u95_pahs_cat_b_ob") %>% 
  add_column(Empty2 = NA, .after="u95_pahs_cat_b_ld") %>%
  add_column(Empty3 = NA, .after="u95_pahs_cat_b_hyper") %>%
  add_column(Empty4 = NA, .after="u95_pahs_cat_b_dia") %>%
  add_column(Empty5 = NA, .after="u95_pahs_cat_b_ms")  %>%
  add_column(Empty0 = NA, .before = "u95_pahs_cat_b_ob")  
up_pahs_cat_b_tmp1 <- data.frame(t(up_pahs_cat_b_tmp))
up_pahs_cat_b <- unlist(up_pahs_cat_b_tmp1)
up_pahs_cat_b <- c(rep(NA, 11), up_pahs_cat_b[1:22], rep(NA, 11), up_pahs_cat_b[23:33],
                   rep(NA, 11), up_pahs_cat_b[34:55], rep(NA, 11), 
                   up_pahs_cat_b[56:length(up_pahs_cat_b)])

lo_pahs_cat_b_tmp <- forest_pahs_cat_b %>% 
  dplyr::select(c("l95_pahs_cat_b_ob", "l95_pahs_cat_b_ld", "l95_pahs_cat_b_hyper", 
                  "l95_pahs_cat_b_dia", "l95_pahs_cat_b_ms"))
lo_pahs_cat_b_tmp <- lo_pahs_cat_b_tmp %>%
  add_column(Empty1 = NA, .after="l95_pahs_cat_b_ob") %>% 
  add_column(Empty2 = NA, .after="l95_pahs_cat_b_ld") %>%
  add_column(Empty3 = NA, .after="l95_pahs_cat_b_hyper") %>%
  add_column(Empty4 = NA, .after="l95_pahs_cat_b_dia") %>%
  add_column(Empty5 = NA, .after="l95_pahs_cat_b_ms")  %>%
  add_column(Empty0 = NA, .before = "l95_pahs_cat_b_ob")  
lo_pahs_cat_b_tmp1 <- data.frame(t(lo_pahs_cat_b_tmp))
lo_pahs_cat_b <- unlist(lo_pahs_cat_b_tmp1)
lo_pahs_cat_b <- c(rep(NA, 11), lo_pahs_cat_b[1:22], rep(NA, 11), lo_pahs_cat_b[23:33],
                   rep(NA, 11), lo_pahs_cat_b[34:55], rep(NA, 11), 
                   lo_pahs_cat_b[56:length(lo_pahs_cat_b)])


or_pahs_b_tmp <- forest_pahs_b %>% 
  dplyr::select(c("or_pahs_b_ob", "or_pahs_b_ld", "or_pahs_b_hyper", 
                  "or_pahs_b_dia", "or_pahs_b_ms"))
or_pahs_b_tmp <- or_pahs_b_tmp %>%
  add_column(Empty1 = NA, .after="or_pahs_b_ob") %>% 
  add_column(Empty2 = NA, .after="or_pahs_b_ld") %>%
  add_column(Empty3 = NA, .after="or_pahs_b_hyper") %>%
  add_column(Empty4 = NA, .after="or_pahs_b_dia") %>%
  add_column(Empty5 = NA, .after="or_pahs_b_ms")  %>%
  add_column(Empty0 = NA, .before = "or_pahs_b_ob")  
or_pahs_b_tmp1 <- data.frame(t(or_pahs_b_tmp))
or_pahs_b <- unlist(or_pahs_b_tmp1)

up_pahs_b_tmp <- forest_pahs_b %>% 
  dplyr::select(c("u95_pahs_b_ob", "u95_pahs_b_ld", "u95_pahs_b_hyper", 
                  "u95_pahs_b_dia", "u95_pahs_b_ms"))
up_pahs_b_tmp <- up_pahs_b_tmp %>%
  add_column(Empty1 = NA, .after="u95_pahs_b_ob") %>% 
  add_column(Empty2 = NA, .after="u95_pahs_b_ld") %>%
  add_column(Empty3 = NA, .after="u95_pahs_b_hyper") %>%
  add_column(Empty4 = NA, .after="u95_pahs_b_dia") %>%
  add_column(Empty5 = NA, .after="u95_pahs_b_ms")  %>%
  add_column(Empty0 = NA, .before = "u95_pahs_b_ob")  
up_pahs_b_tmp1 <- data.frame(t(up_pahs_b_tmp))
up_pahs_b <- unlist(up_pahs_b_tmp1)

lo_pahs_b_tmp <- forest_pahs_b %>% 
  dplyr::select(c("l95_pahs_b_ob", "l95_pahs_b_ld", "l95_pahs_b_hyper", 
                  "l95_pahs_b_dia", "l95_pahs_b_ms"))
lo_pahs_b_tmp <- lo_pahs_b_tmp %>%
  add_column(Empty1 = NA, .after="l95_pahs_b_ob") %>% 
  add_column(Empty2 = NA, .after="l95_pahs_b_ld") %>%
  add_column(Empty3 = NA, .after="l95_pahs_b_hyper") %>%
  add_column(Empty4 = NA, .after="l95_pahs_b_dia") %>%
  add_column(Empty5 = NA, .after="l95_pahs_b_ms")  %>%
  add_column(Empty0 = NA, .before = "l95_pahs_b_ob")  
lo_pahs_b_tmp1 <- data.frame(t(lo_pahs_b_tmp))
lo_pahs_b <- unlist(lo_pahs_b_tmp1)



# pest L
or_pest_cat_l_tmp <- forest_pest_cat_l %>% 
  dplyr::select(c("or_pest_cat_l_ob", "or_pest_cat_l_ld", "or_pest_cat_l_hyper", 
                  "or_pest_cat_l_dia", "or_pest_cat_l_ms"))
or_pest_cat_l_tmp <- or_pest_cat_l_tmp %>%
  add_column(Empty1 = NA, .after="or_pest_cat_l_ob") %>% 
  add_column(Empty2 = NA, .after="or_pest_cat_l_ld") %>%
  add_column(Empty3 = NA, .after="or_pest_cat_l_hyper") %>%
  add_column(Empty4 = NA, .after="or_pest_cat_l_dia") %>%
  add_column(Empty5 = NA, .after="or_pest_cat_l_ms")  %>%
  add_column(Empty0 = NA, .before = "or_pest_cat_l_ob")  
or_pest_cat_l_tmp1 <- data.frame(t(or_pest_cat_l_tmp))
or_pest_cat_l <- unlist(or_pest_cat_l_tmp1)

up_pest_cat_l_tmp <- forest_pest_cat_l %>% 
  dplyr::select(c("u95_pest_cat_l_ob", "u95_pest_cat_l_ld", "u95_pest_cat_l_hyper", 
                  "u95_pest_cat_l_dia", "u95_pest_cat_l_ms"))
up_pest_cat_l_tmp <- up_pest_cat_l_tmp %>%
  add_column(Empty1 = NA, .after="u95_pest_cat_l_ob") %>% 
  add_column(Empty2 = NA, .after="u95_pest_cat_l_ld") %>%
  add_column(Empty3 = NA, .after="u95_pest_cat_l_hyper") %>%
  add_column(Empty4 = NA, .after="u95_pest_cat_l_dia") %>%
  add_column(Empty5 = NA, .after="u95_pest_cat_l_ms")  %>%
  add_column(Empty0 = NA, .before = "u95_pest_cat_l_ob")  
up_pest_cat_l_tmp1 <- data.frame(t(up_pest_cat_l_tmp))
up_pest_cat_l <- unlist(up_pest_cat_l_tmp1)

lo_pest_cat_l_tmp <- forest_pest_cat_l %>% 
  dplyr::select(c("l95_pest_cat_l_ob", "l95_pest_cat_l_ld", "l95_pest_cat_l_hyper", 
                  "l95_pest_cat_l_dia", "l95_pest_cat_l_ms"))
lo_pest_cat_l_tmp <- lo_pest_cat_l_tmp %>%
  add_column(Empty1 = NA, .after="l95_pest_cat_l_ob") %>% 
  add_column(Empty2 = NA, .after="l95_pest_cat_l_ld") %>%
  add_column(Empty3 = NA, .after="l95_pest_cat_l_hyper") %>%
  add_column(Empty4 = NA, .after="l95_pest_cat_l_dia") %>%
  add_column(Empty5 = NA, .after="l95_pest_cat_l_ms")  %>%
  add_column(Empty0 = NA, .before = "l95_pest_cat_l_ob")  
lo_pest_cat_l_tmp1 <- data.frame(t(lo_pest_cat_l_tmp))
lo_pest_cat_l <- unlist(lo_pest_cat_l_tmp1)


or_pest_l_tmp <- forest_pest_l %>% 
  dplyr::select(c("or_pest_l_ob", "or_pest_l_ld", "or_pest_l_hyper", "or_pest_l_dia", "or_pest_l_ms"))
or_pest_l_tmp <- or_pest_l_tmp %>%
  add_column(Empty1 = NA, .after="or_pest_l_ob") %>% 
  add_column(Empty2 = NA, .after="or_pest_l_ld") %>%
  add_column(Empty3 = NA, .after="or_pest_l_hyper") %>%
  add_column(Empty4 = NA, .after="or_pest_l_dia") %>%
  add_column(Empty5 = NA, .after="or_pest_l_ms")  %>%
  add_column(Empty0 = NA, .before = "or_pest_l_ob")  
or_pest_l_tmp1 <- data.frame(t(or_pest_l_tmp))
or_pest_l <- unlist(or_pest_l_tmp1)

up_pest_l_tmp <- forest_pest_l %>% 
  dplyr::select(c("u95_pest_l_ob", "u95_pest_l_ld", "u95_pest_l_hyper", "u95_pest_l_dia", "u95_pest_l_ms"))
up_pest_l_tmp <- up_pest_l_tmp %>%
  add_column(Empty1 = NA, .after="u95_pest_l_ob") %>% 
  add_column(Empty2 = NA, .after="u95_pest_l_ld") %>%
  add_column(Empty3 = NA, .after="u95_pest_l_hyper") %>%
  add_column(Empty4 = NA, .after="u95_pest_l_dia") %>%
  add_column(Empty5 = NA, .after="u95_pest_l_ms")  %>%
  add_column(Empty0 = NA, .before = "u95_pest_l_ob")  
up_pest_l_tmp1 <- data.frame(t(up_pest_l_tmp))
up_pest_l <- unlist(up_pest_l_tmp1)

lo_pest_l_tmp <- forest_pest_l %>% 
  dplyr::select(c("l95_pest_l_ob", "l95_pest_l_ld", "l95_pest_l_hyper", "l95_pest_l_dia", "l95_pest_l_ms"))
lo_pest_l_tmp <- lo_pest_l_tmp %>%
  add_column(Empty1 = NA, .after="l95_pest_l_ob") %>% 
  add_column(Empty2 = NA, .after="l95_pest_l_ld") %>%
  add_column(Empty3 = NA, .after="l95_pest_l_hyper") %>%
  add_column(Empty4 = NA, .after="l95_pest_l_dia") %>%
  add_column(Empty5 = NA, .after="l95_pest_l_ms")  %>%
  add_column(Empty0 = NA, .before = "l95_pest_l_ob")  
lo_pest_l_tmp1 <- data.frame(t(lo_pest_l_tmp))
lo_pest_l <- unlist(lo_pest_l_tmp1)


# pest B
or_pest_cat_b_tmp <- forest_pest_cat_b %>% 
  dplyr::select(c("or_pest_cat_b_ob", "or_pest_cat_b_ld", "or_pest_cat_b_hyper", 
                  "or_pest_cat_b_dia", "or_pest_cat_b_ms"))
or_pest_cat_b_tmp <- or_pest_cat_b_tmp %>%
  add_column(Empty1 = NA, .after="or_pest_cat_b_ob") %>% 
  add_column(Empty2 = NA, .after="or_pest_cat_b_ld") %>%
  add_column(Empty3 = NA, .after="or_pest_cat_b_hyper") %>%
  add_column(Empty4 = NA, .after="or_pest_cat_b_dia") %>%
  add_column(Empty5 = NA, .after="or_pest_cat_b_ms")  %>%
  add_column(Empty0 = NA, .before = "or_pest_cat_b_ob")  
or_pest_cat_b_tmp1 <- data.frame(t(or_pest_cat_b_tmp))
or_pest_cat_b <- unlist(or_pest_cat_b_tmp1)

up_pest_cat_b_tmp <- forest_pest_cat_b %>% 
  dplyr::select(c("u95_pest_cat_b_ob", "u95_pest_cat_b_ld", "u95_pest_cat_b_hyper", 
                  "u95_pest_cat_b_dia", "u95_pest_cat_b_ms"))
up_pest_cat_b_tmp <- up_pest_cat_b_tmp %>%
  add_column(Empty1 = NA, .after="u95_pest_cat_b_ob") %>% 
  add_column(Empty2 = NA, .after="u95_pest_cat_b_ld") %>%
  add_column(Empty3 = NA, .after="u95_pest_cat_b_hyper") %>%
  add_column(Empty4 = NA, .after="u95_pest_cat_b_dia") %>%
  add_column(Empty5 = NA, .after="u95_pest_cat_b_ms")  %>%
  add_column(Empty0 = NA, .before = "u95_pest_cat_b_ob")  
up_pest_cat_b_tmp1 <- data.frame(t(up_pest_cat_b_tmp))
up_pest_cat_b <- unlist(up_pest_cat_b_tmp1)

lo_pest_cat_b_tmp <- forest_pest_cat_b %>% 
  dplyr::select(c("l95_pest_cat_b_ob", "l95_pest_cat_b_ld", "l95_pest_cat_b_hyper", 
                  "l95_pest_cat_b_dia", "l95_pest_cat_b_ms"))
lo_pest_cat_b_tmp <- lo_pest_cat_b_tmp %>%
  add_column(Empty1 = NA, .after="l95_pest_cat_b_ob") %>% 
  add_column(Empty2 = NA, .after="l95_pest_cat_b_ld") %>%
  add_column(Empty3 = NA, .after="l95_pest_cat_b_hyper") %>%
  add_column(Empty4 = NA, .after="l95_pest_cat_b_dia") %>%
  add_column(Empty5 = NA, .after="l95_pest_cat_b_ms")  %>%
  add_column(Empty0 = NA, .before = "l95_pest_cat_b_ob")  
lo_pest_cat_b_tmp1 <- data.frame(t(lo_pest_cat_b_tmp))
lo_pest_cat_b <- unlist(lo_pest_cat_b_tmp1)


or_pest_b_tmp <- forest_pest_b %>% 
  dplyr::select(c("or_pest_b_ob", "or_pest_b_ld", "or_pest_b_hyper", "or_pest_b_dia", "or_pest_b_ms"))
or_pest_b_tmp <- or_pest_b_tmp %>%
  add_column(Empty1 = NA, .after="or_pest_b_ob") %>% 
  add_column(Empty2 = NA, .after="or_pest_b_ld") %>%
  add_column(Empty3 = NA, .after="or_pest_b_hyper") %>%
  add_column(Empty4 = NA, .after="or_pest_b_dia") %>%
  add_column(Empty5 = NA, .after="or_pest_b_ms")  %>%
  add_column(Empty0 = NA, .before = "or_pest_b_ob")  
or_pest_b_tmp1 <- data.frame(t(or_pest_b_tmp))
or_pest_b <- unlist(or_pest_b_tmp1)

up_pest_b_tmp <- forest_pest_b %>% 
  dplyr::select(c("u95_pest_b_ob", "u95_pest_b_ld", "u95_pest_b_hyper", "u95_pest_b_dia", "u95_pest_b_ms"))
up_pest_b_tmp <- up_pest_b_tmp %>%
  add_column(Empty1 = NA, .after="u95_pest_b_ob") %>% 
  add_column(Empty2 = NA, .after="u95_pest_b_ld") %>%
  add_column(Empty3 = NA, .after="u95_pest_b_hyper") %>%
  add_column(Empty4 = NA, .after="u95_pest_b_dia") %>%
  add_column(Empty5 = NA, .after="u95_pest_b_ms")  %>%
  add_column(Empty0 = NA, .before = "u95_pest_b_ob")  
up_pest_b_tmp1 <- data.frame(t(up_pest_b_tmp))
up_pest_b <- unlist(up_pest_b_tmp1)

lo_pest_b_tmp <- forest_pest_b %>% 
  dplyr::select(c("l95_pest_b_ob", "l95_pest_b_ld", "l95_pest_b_hyper", "l95_pest_b_dia", "l95_pest_b_ms"))
lo_pest_b_tmp <- lo_pest_b_tmp %>%
  add_column(Empty1 = NA, .after="l95_pest_b_ob") %>% 
  add_column(Empty2 = NA, .after="l95_pest_b_ld") %>%
  add_column(Empty3 = NA, .after="l95_pest_b_hyper") %>%
  add_column(Empty4 = NA, .after="l95_pest_b_dia") %>%
  add_column(Empty5 = NA, .after="l95_pest_b_ms")  %>%
  add_column(Empty0 = NA, .before = "l95_pest_b_ob")  
lo_pest_b_tmp1 <- data.frame(t(lo_pest_b_tmp))
lo_pest_b <- unlist(lo_pest_b_tmp1)


# pest F
or_pest_cat_f_tmp <- forest_pest_cat_f %>% 
  dplyr::select(c("or_pest_cat_f_ob", "or_pest_cat_f_ld", "or_pest_cat_f_hyper", 
                  "or_pest_cat_f_dia", "or_pest_cat_f_ms"))
or_pest_cat_f_tmp <- or_pest_cat_f_tmp %>%
  add_column(Empty1 = NA, .after="or_pest_cat_f_ob") %>% 
  add_column(Empty2 = NA, .after="or_pest_cat_f_ld") %>%
  add_column(Empty3 = NA, .after="or_pest_cat_f_hyper") %>%
  add_column(Empty4 = NA, .after="or_pest_cat_f_dia") %>%
  add_column(Empty5 = NA, .after="or_pest_cat_f_ms")  %>%
  add_column(Empty0 = NA, .before = "or_pest_cat_f_ob")  
or_pest_cat_f_tmp1 <- data.frame(t(or_pest_cat_f_tmp))
or_pest_cat_f <- unlist(or_pest_cat_f_tmp1)
or_pest_cat_f <- c(or_pest_cat_f[1:121], rep(NA, 11), or_pest_cat_f[122:374], rep(NA, 22),
                   or_pest_cat_f[375:396], rep(NA, 22),
                   or_pest_cat_f[397:length(or_pest_cat_f)])

up_pest_cat_f_tmp <- forest_pest_cat_f %>% 
  dplyr::select(c("u95_pest_cat_f_ob", "u95_pest_cat_f_ld", "u95_pest_cat_f_hyper", 
                  "u95_pest_cat_f_dia", "u95_pest_cat_f_ms"))
up_pest_cat_f_tmp <- up_pest_cat_f_tmp %>%
  add_column(Empty1 = NA, .after="u95_pest_cat_f_ob") %>% 
  add_column(Empty2 = NA, .after="u95_pest_cat_f_ld") %>%
  add_column(Empty3 = NA, .after="u95_pest_cat_f_hyper") %>%
  add_column(Empty4 = NA, .after="u95_pest_cat_f_dia") %>%
  add_column(Empty5 = NA, .after="u95_pest_cat_f_ms")  %>%
  add_column(Empty0 = NA, .before = "u95_pest_cat_f_ob")  
up_pest_cat_f_tmp1 <- data.frame(t(up_pest_cat_f_tmp))
up_pest_cat_f <- unlist(up_pest_cat_f_tmp1)
up_pest_cat_f <- c(up_pest_cat_f[1:121], rep(NA, 11), up_pest_cat_f[122:374], rep(NA, 22),
                   up_pest_cat_f[375:396], rep(NA, 22),
                   up_pest_cat_f[397:length(up_pest_cat_f)])


lo_pest_cat_f_tmp <- forest_pest_cat_f %>% 
  dplyr::select(c("l95_pest_cat_f_ob", "l95_pest_cat_f_ld", "l95_pest_cat_f_hyper", 
                  "l95_pest_cat_f_dia", "l95_pest_cat_f_ms"))
lo_pest_cat_f_tmp <- lo_pest_cat_f_tmp %>%
  add_column(Empty1 = NA, .after="l95_pest_cat_f_ob") %>% 
  add_column(Empty2 = NA, .after="l95_pest_cat_f_ld") %>%
  add_column(Empty3 = NA, .after="l95_pest_cat_f_hyper") %>%
  add_column(Empty4 = NA, .after="l95_pest_cat_f_dia") %>%
  add_column(Empty5 = NA, .after="l95_pest_cat_f_ms")  %>%
  add_column(Empty0 = NA, .before = "l95_pest_cat_f_ob")  
lo_pest_cat_f_tmp1 <- data.frame(t(lo_pest_cat_f_tmp))
lo_pest_cat_f <- unlist(lo_pest_cat_f_tmp1)
lo_pest_cat_f <- c(lo_pest_cat_f[1:121], rep(NA, 11), lo_pest_cat_f[122:374], rep(NA, 22),
                   lo_pest_cat_f[375:396], rep(NA, 22),
                   lo_pest_cat_f[397:length(lo_pest_cat_f)])


or_pest_f_tmp <- forest_pest_f %>% 
  dplyr::select(c("or_pest_f_ob", "or_pest_f_ld", "or_pest_f_hyper", "or_pest_f_dia", "or_pest_f_ms"))
or_pest_f_tmp <- or_pest_f_tmp %>%
  add_column(Empty1 = NA, .after="or_pest_f_ob") %>% 
  add_column(Empty2 = NA, .after="or_pest_f_ld") %>%
  add_column(Empty3 = NA, .after="or_pest_f_hyper") %>%
  add_column(Empty4 = NA, .after="or_pest_f_dia") %>%
  add_column(Empty5 = NA, .after="or_pest_f_ms")  %>%
  add_column(Empty0 = NA, .before = "or_pest_f_ob")  
or_pest_f_tmp1 <- data.frame(t(or_pest_f_tmp))
or_pest_f <- unlist(or_pest_f_tmp1)

up_pest_f_tmp <- forest_pest_f %>% 
  dplyr::select(c("u95_pest_f_ob", "u95_pest_f_ld", "u95_pest_f_hyper", "u95_pest_f_dia", "u95_pest_f_ms"))
up_pest_f_tmp <- up_pest_f_tmp %>%
  add_column(Empty1 = NA, .after="u95_pest_f_ob") %>% 
  add_column(Empty2 = NA, .after="u95_pest_f_ld") %>%
  add_column(Empty3 = NA, .after="u95_pest_f_hyper") %>%
  add_column(Empty4 = NA, .after="u95_pest_f_dia") %>%
  add_column(Empty5 = NA, .after="u95_pest_f_ms")  %>%
  add_column(Empty0 = NA, .before = "u95_pest_f_ob")  
up_pest_f_tmp1 <- data.frame(t(up_pest_f_tmp))
up_pest_f <- unlist(up_pest_f_tmp1)

lo_pest_f_tmp <- forest_pest_f %>% 
  dplyr::select(c("l95_pest_f_ob", "l95_pest_f_ld", "l95_pest_f_hyper", "l95_pest_f_dia", "l95_pest_f_ms"))
lo_pest_f_tmp <- lo_pest_f_tmp %>%
  add_column(Empty1 = NA, .after="l95_pest_f_ob") %>% 
  add_column(Empty2 = NA, .after="l95_pest_f_ld") %>%
  add_column(Empty3 = NA, .after="l95_pest_f_hyper") %>%
  add_column(Empty4 = NA, .after="l95_pest_f_dia") %>%
  add_column(Empty5 = NA, .after="l95_pest_f_ms")  %>%
  add_column(Empty0 = NA, .before = "l95_pest_f_ob")  
lo_pest_f_tmp1 <- data.frame(t(lo_pest_f_tmp))
lo_pest_f <- unlist(lo_pest_f_tmp1)




##### MANHATTAN --
### pest
labels_pest <- c("PCB 138", "PCB 153", "PCB 180", "BDE 47", colnames(pest)[c(14:22)],
                 "Carbofuran phenol", colnames(pest)[24:27], "2-IPP", colnames(pest)[c(29:37)], 
                 "3Me4NP", colnames(pest)[39],
                 "3-PBA", "Fipronil sulfone", colnames(pest)[42])

labels_pest <- c(colnames(pest)[24:27], "2-IPP",colnames(pest)[c(29:37)], 
                 "3Me4NP", colnames(pest)[39],
                 "3-PBA", "Fipronil sulfone", colnames(pest)[42])

mycolours = brewer.pal(10, "Paired")
#mycolours <- c("lightcoral","lightgoldenrod", "palegreen2","steelblue1", "lightslateblue")
outcomes <- c("Obesity", "Lipid disorder", "Hypertension", "Diabetes","Metabolic syndrome")

n=5 # Number of lines per variable
myspacing=n*2+1
xseq=seq((n+1),33*myspacing, by=myspacing) 
xseq=seq((n+1),18*myspacing, by=myspacing)   # change accordingly
background=TRUE
myrange <- c(0,max(manhat_pest_cat_l_1, na.rm = T))  # change accordingly
myrange <- c(0,max(manhat_pest_cat_b_1, na.rm = T))
myrange <- c(0,max(manhat_pest_cat_f_1, na.rm = T))
myrange <- c(0,max(manhat_pest_l_1, na.rm = T)+0.5)  # change accordingly
myrange <- c(0,max(manhat_pest_b_1, na.rm = T)+0.5)
myrange <- c(0,max(manhat_pest_f_1, na.rm = T)+0.5)

background_colour="darkgrey"

pdf("../cat_figures/univ/manhat_pest_cat_l.pdf", height = 7, width = 13)
pdf("../cat_figures/univ/manhat_pest_cat_b.pdf", height = 7, width = 13)
pdf("../cat_figures/univ/manhat_pest_cat_f.pdf", height = 7, width = 13)
pdf("../cat_figures/univ/manhat_pest_l.pdf", height = 7, width = 13)
pdf("../cat_figures/univ/manhat_pest_b.pdf", height = 7, width = 13)
pdf("../cat_figures/univ/manhat_pest_f.pdf", height = 7, width = 13)
par(mar = c(10, 2, 2, 5))
plot(manhat_pest_f_1, ylim=myrange, xlim=c(1,length(manhat_pest_f_1)), type="n",
     xaxt="n", yaxt="n",
     col=c(mycolours[2], NA, mycolours[4],NA, mycolours[6], NA, mycolours[8],NA, mycolours[10],NA),
     lwd=1, ylab="",xlab="")
xseqgreysep=c(min(xseq)-myspacing/2,apply(rbind(xseq[-1],xseq[-length(xseq)]),2,mean),max(xseq)+myspacing/2)

if (background){
  for (k in seq(1,length(xseqgreysep),by=2)){
    polygon(x=c(xseqgreysep[k],xseqgreysep[k+1],xseqgreysep[k+1],xseqgreysep[k]),
            y=c(-5,-5,15,15), col=lighten(background_colour,0.90), border=NA)
  }
  for (k in seq(2,length(xseqgreysep),by=2)){
    polygon(x=c(xseqgreysep[k],xseqgreysep[k+1],xseqgreysep[k+1],xseqgreysep[k]),
            y=c(-5,-5,15,15), col=lighten(background_colour,0.95), border=NA)
  }
  box()
}
par(new=TRUE)
plot(manhat_pest_f_1, ylim=myrange,
     xaxt="n", yaxt="n", cex = 0.8, 
     col=c(NA, mycolours[2], NA, mycolours[4],NA, mycolours[6], NA, mycolours[8],NA, mycolours[10],NA),
     pch = c(NA, 15, NA, 16,NA, 17, NA, 18,NA, 25,NA),
     lwd=1, ylab="", xlab="")

axis(side=4, at=axTicks(2))
mtext(side=4, text=expression(-log[10](italic(p))), line=2)
#xseqblack=c(xseq[!duplicated(variable_cat)]-myspacing/2, max(xseq)+myspacing/2)
#for (k in 1:(length(xseqblack)-1)){
#  axis(side=3, at=xseqblack[c(k,k+1)]+c(2,-2), line=0.5, labels=NA)
#}
#for (k in 1:(length(xseqblack)-1)){
#  axis(side=3, at=mean(xseqblack[c(k,k+1)]), line=0.2, tick=FALSE,
#       labels=unique(variable_cat)[k])
#}
#par(xpd=TRUE)
abline(h=-log10(0.05/18), lty = 2, col="darkred")
abline(h=-log10(0.05), lty = 2, col="darkgrey")
abline(v=xseqgreysep,lty=3,col="black",lwd=0.5)
for (k in 1:length(xseq)){
  mytext=labels_pest[k]
  if (grepl("m\\^", mytext)){
    mytext=gsub("m\\^","'~m^", mytext)
    mytext=sub(")","~')", mytext)
  }
  if (grepl("\\[", mytext)){
    mytext=gsub("\\[","'[", mytext)
    mytext=sub("\\(ug","~'(ug", mytext)
  }
  mytmp=eval(parse(text=paste0("expression(","'", mytext,"'",")")))
  #labcol=ifelse((manhat_pahs_1[k,1]>=bonf & manhat[k,2]>=bonf), mycolours[3],
  #ifelse(manhat[k,1]>=bonf,mycolours[1],
  #ifelse(manhat[k,2]>=bonf, mycolours[2],"black")))
  #if(is.na(labcol)){
  #labcol=ifelse(manhat[k,1]>=bonf,mycolours[1],
  #ifelse(manhat[k,2]>=bonf, mycolours[2],"black"))
  #}
  #labfont=ifelse(labcol=="black",1,2)
  axis(side=1, at=xseq[k], labels=mytmp, las=2, cex.axis=0.8)
}
legend("topright", pch=c(15, 16, 17, 18, 25, NA, NA), lty=c(NA, NA, NA, NA, NA, 2, 2), 
       col=c(mycolours[c(2,4,6,8,10)],"darkred","darkgrey"),
       legend=c(outcomes,"Bonferroni threshold","Nominal threshold"),
       cex=0.7, bg = "white", bty = "n")
dev.off()


### PAHs
labels_pahs <- c("9-OH-Fluorene", "3-OH-Fluorene", "4-OH-Phenanthrene", "3-OH-Phenanthrene",
                 "1-OH-Phenanthrene", "2-OH-Phenanthrene", "1-OH-Pyrene",       
                 "6-OH-Chrysene11-OH-Benz(a)anthrac", "Cotinine", "2-OH-Fluorene",                    
                 "Nicotine", "1-OH-Naphthalene", "2-OH-Naphthalene")
labels_pahs <- c("Cotinine", "2-OH-Fluorene",                    
                 "Nicotine", "1-OH-Naphthalene", "2-OH-Naphthalene")

outcomes <- c("Obesity", "Lipid disorder", "Hypertension", "Diabetes","Metabolic syndrome")

n=5 # Number of lines per variable
myspacing=n*2+1
xseq=seq((n+1),13*myspacing, by=myspacing)
xseq=seq((n+1),5*myspacing, by=myspacing)
background=TRUE
myrange <- c(0,max(manhat_pahs_cat_l_1, na.rm = T))    # change accordingly
myrange <- c(0,max(manhat_pahs_cat_b_1, na.rm = T)) 
myrange <- c(0,max(manhat_pahs_l_1, na.rm = T) + 0.5)    # change accordingly
myrange <- c(0,max(manhat_pahs_b_1, na.rm = T) + 0.5) 

background_colour="darkgrey"

pdf("../cat_figures/univ/manhat_pahs_cat_l.pdf", height = 7, width = 13)
pdf("../cat_figures/univ/manhat_pahs_cat_b.pdf", height = 7, width = 13)
pdf("../cat_figures/univ/manhat_pahs_l.pdf", height = 7, width = 13)
pdf("../cat_figures/univ/manhat_pahs_b.pdf", height = 7, width = 13)
par(mar = c(12, 2, 2, 5))
plot(manhat_pahs_b_1, ylim=myrange, xlim=c(1,length(manhat_pahs_b_1)), type="n",
     xaxt="n", yaxt="n",
     col=c(mycolours[2], NA, mycolours[4],NA, mycolours[6], NA, mycolours[8],NA, mycolours[10],NA),
     lwd=1, ylab="",xlab="")
xseqgreysep=c(min(xseq)-myspacing/2,apply(rbind(xseq[-1],xseq[-length(xseq)]),2,mean),max(xseq)+myspacing/2)

if (background){
  for (k in seq(1,length(xseqgreysep),by=2)){
    polygon(x=c(xseqgreysep[k],xseqgreysep[k+1],xseqgreysep[k+1],xseqgreysep[k]),
            y=c(-10,-10,15,15), col=lighten(background_colour,0.90), border=NA)
  }
  for (k in seq(2,length(xseqgreysep),by=2)){
    polygon(x=c(xseqgreysep[k],xseqgreysep[k+1],xseqgreysep[k+1],xseqgreysep[k]),
            y=c(-10,-10,15,15), col=lighten(background_colour,0.95), border=NA)
  }
  box()
}
par(new=TRUE)
plot(manhat_pahs_b_1, ylim=myrange,
     xaxt="n", yaxt="n",
     col=c(NA, mycolours[2], NA, mycolours[4],NA, mycolours[6], NA, mycolours[8],NA, mycolours[10],NA),
     pch = c(NA, 15, NA, 16,NA, 17, NA, 18,NA, 25,NA),
     lwd=1, ylab="", xlab="")

axis(side=4, at=axTicks(2))
mtext(side=4, text=expression(-log[10](italic(p))), line=2)
#xseqblack=c(xseq[!duplicated(variable_cat)]-myspacing/2, max(xseq)+myspacing/2)
#for (k in 1:(length(xseqblack)-1)){
#  axis(side=3, at=xseqblack[c(k,k+1)]+c(2,-2), line=0.5, labels=NA)
#}
#for (k in 1:(length(xseqblack)-1)){
#  axis(side=3, at=mean(xseqblack[c(k,k+1)]), line=0.2, tick=FALSE,
#       labels=unique(variable_cat)[k])
#}
#par(xpd=TRUE)
abline(h=-log10(0.05/13), lty = 2, col="darkred")
abline(h=-log10(0.05), lty = 2, col="gray48")
abline(v=xseqgreysep,lty=3,col="black",lwd=0.5)
for (k in 1:length(xseq)){
  mytext=labels_pahs[k]
  if (grepl("m\\^", mytext)){
    mytext=gsub("m\\^","'~m^", mytext)
    mytext=sub(")","~')", mytext)
  }
  if (grepl("\\[", mytext)){
    mytext=gsub("\\[","'[", mytext)
    mytext=sub("\\(ug","~'(ug", mytext)
  }
  mytmp=eval(parse(text=paste0("expression(","'", mytext,"'",")")))
  labcol=ifelse(k<=8, "black", ifelse(k>8 & k<=10, "goldenrod", "lightpink2"))
  #if(is.na(labcol)){
  #labcol=ifelse(manhat[k,1]>=bonf,mycolours[1],
  #ifelse(manhat[k,2]>=bonf, mycolours[2],"black"))
  #}
  axis(side=1, at=xseq[k], labels=mytmp, las=2, cex.axis=0.7, col = labcol, col.axis=labcol)
}
legend("topright", pch=c(15, 16, 17, 18, 25, NA, NA), lty=c(NA, NA, NA, NA, NA, 2, 2), 
       col=c(mycolours[c(2,4,6,8,10)],"darkred","gray48"),
       legend=c(outcomes,"Bonferroni threshold","Nominal threshold"),
       cex=0.7, bg = "white", bty = "n")
dev.off()




##### OR plot --
### PAHs OR
rownames(forest_pahs_cat)
labels_pahs <-c("9-OH-Fluorene", "3-OH-Fluorene", "4-OH-Phenanthrene", "3-OH-Phenanthrene",
                "1-OH-Phenanthrene", "2-OH-Phenanthrene", "1-OH-Pyrene", 
                "6-OH-Chrysene11-OH-Benz(a)anthrac", "Cotinine", "2-OH-Fluorene",                    
                "Nicotine2", "Nicotine3", "Nicotine4",
                "1-OH-Naphthalene2", "1-OH-Naphthalene3", "1-OH-Naphthalene4", 
                "2-OH-Naphthalene2", "2-OH-Naphthalene3", "2-OH-Naphthalene4")

n=5 # Number of lines per variable
myspacing=n*2+1
xseq=seq((n+1),19*myspacing, by=myspacing)
background=TRUE
myrange <- c(0,33)
max(up_pahs_cat_b, na.rm = T)

background_colour="darkgrey"

pdf("../cat_figures/univ/OR_pahs_cat_l.pdf", height = 7, width = 13)
pdf("../cat_figures/univ/OR_pahs_cat_b.pdf", height = 7, width = 13)
par(mar = c(12, 2, 2, 5))
plot(or_pahs_cat_b, ylim=myrange, xlim=c(1,length(or_pahs_cat_b)), type="n",
     xaxt="n", yaxt="n",
     col=c(mycolours[2], NA, mycolours[4],NA, mycolours[6], NA, mycolours[8],NA, mycolours[10],NA),
     lwd=1, ylab="",xlab="")
xseqgreysep=c(min(xseq)-myspacing/2,apply(rbind(xseq[-1],xseq[-length(xseq)]),2,mean),max(xseq)+myspacing/2)

if (background){
  for (k in seq(1,length(xseqgreysep),by=2)){
    polygon(x=c(xseqgreysep[k],xseqgreysep[k+1],xseqgreysep[k+1],xseqgreysep[k]),
            y=c(-10,-10,15,15), col=lighten(background_colour,0.95), border=NA)
  }
  for (k in seq(2,length(xseqgreysep),by=2)){
    polygon(x=c(xseqgreysep[k],xseqgreysep[k+1],xseqgreysep[k+1],xseqgreysep[k]),
            y=c(-10,-10,15,15), col=lighten(background_colour,0.99), border=NA)
  }
  box()
}
par(new=TRUE)
plot(or_pahs_cat_b, ylim=myrange,
     xaxt="n", yaxt="n",
     col=c(NA, mycolours[2], NA, mycolours[4],NA, mycolours[6], NA, mycolours[8],NA, mycolours[10],NA),
     pch = c(NA, 15, NA, 16,NA, 17, NA, 18,NA, 25,NA),
     lwd=1, ylab="", xlab="")
x_pahs <- 1:209
arrows(x0=x_pahs, y0=up_pahs_cat_b, x1=x_pahs, y1=lo_pahs_cat_b, code=3, angle=90, length=0.05, 
       col=c(NA, mycolours[2], NA, mycolours[4],NA, mycolours[6], NA, mycolours[8],NA, mycolours[10],NA))
axis(side=4, at=axTicks(2))
mtext(side=4, text="Odds Ratio", line=2)
#xseqblack=c(xseq[!duplicated(variable_cat)]-myspacing/2, max(xseq)+myspacing/2)
#for (k in 1:(length(xseqblack)-1)){
#  axis(side=3, at=xseqblack[c(k,k+1)]+c(2,-2), line=0.5, labels=NA)
#}
#for (k in 1:(length(xseqblack)-1)){
#  axis(side=3, at=mean(xseqblack[c(k,k+1)]), line=0.2, tick=FALSE,
#       labels=unique(variable_cat)[k])
#}
#par(xpd=TRUE)

abline(h=1, lty = 2, col="gray48")
abline(v=xseqgreysep,lty=3,col="black",lwd=0.5)
for (k in 1:length(xseq)){
  mytext=labels_pahs[k]
  if (grepl("m\\^", mytext)){
    mytext=gsub("m\\^","'~m^", mytext)
    mytext=sub(")","~')", mytext)
  }
  if (grepl("\\[", mytext)){
    mytext=gsub("\\[","'[", mytext)
    mytext=sub("\\(ug","~'(ug", mytext)
  }
  mytmp=eval(parse(text=paste0("expression(","'", mytext,"'",")")))
  labcol=ifelse(k<=8, "black", ifelse(k>8 & k<=10, "goldenrod", "lightpink3"))
  axis(side=1, at=xseq[k], labels=mytmp, las=2, cex.axis=0.7, col = labcol, col.axis=labcol)
}
legend("topright", pch=c(15, 16, 17, 18, 25), 
       col=c(mycolours[c(2,4,6,8,10)]),
       legend=c(outcomes),
       cex=0.7, bg = "white", bty = "n")
dev.off()


rownames(forest_pahs)
labels_pahs <- c("Cotinine", "2-OH-Fluorene", "Nicotine", "1-OH-Naphthalene", "2-OH-Naphthalene")
n=5 # Number of lines per variable
myspacing=n*2+1
xseq=seq((n+1),5*myspacing, by=myspacing)
background=TRUE
myrange <- c(0,max(up_pahs_b, na.rm = T)+0.5)
myrange <- c(0,4)

background_colour="darkgrey"

pdf("../cat_figures/univ/OR_pahs_l.pdf", height = 7, width = 13)
pdf("../cat_figures/univ/OR_pahs_b.pdf", height = 7, width = 13)
par(mar = c(12, 2, 2, 5))
plot(or_pahs_l, ylim=myrange, xlim=c(1,length(or_pahs_l)), type="n",
     xaxt="n", yaxt="n",
     col=c(mycolours[2], NA, mycolours[4],NA, mycolours[6], NA, mycolours[8],NA, mycolours[10],NA),
     lwd=1, ylab="",xlab="")
xseqgreysep=c(min(xseq)-myspacing/2,apply(rbind(xseq[-1],xseq[-length(xseq)]),2,mean),max(xseq)+myspacing/2)

if (background){
  for (k in seq(1,length(xseqgreysep),by=2)){
    polygon(x=c(xseqgreysep[k],xseqgreysep[k+1],xseqgreysep[k+1],xseqgreysep[k]),
            y=c(-10,-10,15,15), col=lighten(background_colour,0.95), border=NA)
  }
  for (k in seq(2,length(xseqgreysep),by=2)){
    polygon(x=c(xseqgreysep[k],xseqgreysep[k+1],xseqgreysep[k+1],xseqgreysep[k]),
            y=c(-10,-10,15,15), col=lighten(background_colour,0.99), border=NA)
  }
  box()
}
par(new=TRUE)
plot(or_pahs_l, ylim=myrange,
     xaxt="n", yaxt="n",
     col=c(NA, mycolours[2], NA, mycolours[4],NA, mycolours[6], NA, mycolours[8],NA, mycolours[10],NA),
     pch = c(NA, 15, NA, 16,NA, 17, NA, 18,NA, 25,NA),
     lwd=1, ylab="", xlab="")
x_pahs <- 1:55
arrows(x0=x_pahs, y0=up_pahs_l, x1=x_pahs, y1=lo_pahs_l, code=3, angle=90, length=0.05, 
       col=c(NA, mycolours[2], NA, mycolours[4],NA, mycolours[6], NA, mycolours[8],NA, mycolours[10],NA))
axis(side=4, at=axTicks(2))
mtext(side=4, text="Odds Ratio", line=2)
#xseqblack=c(xseq[!duplicated(variable_cat)]-myspacing/2, max(xseq)+myspacing/2)
#for (k in 1:(length(xseqblack)-1)){
#  axis(side=3, at=xseqblack[c(k,k+1)]+c(2,-2), line=0.5, labels=NA)
#}
#for (k in 1:(length(xseqblack)-1)){
#  axis(side=3, at=mean(xseqblack[c(k,k+1)]), line=0.2, tick=FALSE,
#       labels=unique(variable_cat)[k])
#}
#par(xpd=TRUE)

abline(h=1, lty = 2, col="gray48")
abline(v=xseqgreysep,lty=3,col="black",lwd=0.5)
for (k in 1:length(xseq)){
  mytext=labels_pahs[k]
  if (grepl("m\\^", mytext)){
    mytext=gsub("m\\^","'~m^", mytext)
    mytext=sub(")","~')", mytext)
  }
  if (grepl("\\[", mytext)){
    mytext=gsub("\\[","'[", mytext)
    mytext=sub("\\(ug","~'(ug", mytext)
  }
  mytmp=eval(parse(text=paste0("expression(","'", mytext,"'",")")))
  labcol=ifelse(k<=2,  "goldenrod", "lightpink3")
  axis(side=1, at=xseq[k], labels=mytmp, las=2, cex.axis=0.7, col = labcol, col.axis=labcol)
}
legend("topright", pch=c(15, 16, 17, 18, 25), 
       col=c(mycolours[c(2,4,6,8,10)]),
       legend=c(outcomes),
       cex=0.7, bg = "white", bty = "n")
dev.off()





### pest OR
rownames(forest_pest_cat_b)
labels_pest_1 <- c("PCB 138", "PCB 153", "PCB 180", "BDE 47", colnames(pest)[c(14:22)],
                   "Carbofuran phenol", colnames(pest)[24:27], "2 IPP", colnames(pest)[29])
labels_pest_2 <- rownames(forest_pest_cat_b)[21:41]
labels_pest_3 <- c(rownames(forest_pest_cat_b)[42:44], "3Me4NP2", "3Me4NP3", "3Me4NP4",
                   rownames(forest_pest_cat_b)[48:50], "3PBA2", "3PBA3", "3PBA4",
                   "Fipronil sulfone2", "Fipronil_sulfone3", "Fipronil sulfone4",
                   rownames(forest_pest_cat_b)[57:59])


n=5 # Number of lines per variable
myspacing=n*2+1
xseq=seq((n+1),20*myspacing, by=myspacing)
background=TRUE
myrange <- c(0,30)
myrange <- c(0,max(or_pest_cat_f, na.rm = T)+3)
myrange <- c(0,max(or_pest_cat, na.rm = T)+3)
max(up_pest_cat_l, na.rm = T)

background_colour="darkgrey"

pdf("../cat_figures/univ/OR_pest_cat_l1.pdf", height = 7, width = 13)
pdf("../cat_figures/univ/OR_pest_cat_b1.pdf", height = 7, width = 13)
pdf("../cat_figures/univ/OR_pest_cat_b1_zoom.pdf", height = 7, width = 13)
pdf("../cat_figures/univ/OR_pest_cat_f1.pdf", height = 7, width = 13)
pdf("../cat_figures/univ/OR_pest_cat_f1_zoom.pdf", height = 7, width = 13)
par(mar = c(10, 2, 2, 5))
plot(or_pest_cat_f[1:220], ylim=myrange, xlim=c(1,length(or_pest_cat_f[1:220])), type="n",
     xaxt="n", yaxt="n",
     col=c(mycolours[1], NA, mycolours[2],NA, mycolours[3], NA, mycolours[4],NA, mycolours[5],NA),
     lwd=1, ylab="",xlab="")
xseqgreysep=c(min(xseq)-myspacing/2,apply(rbind(xseq[-1],xseq[-length(xseq)]),2,mean),max(xseq)+myspacing/2)

if (background){
  for (k in seq(1,length(xseqgreysep),by=2)){
    polygon(x=c(xseqgreysep[k],xseqgreysep[k+1],xseqgreysep[k+1],xseqgreysep[k]),
            y=c(-10,-10,15,15), col=lighten(background_colour,0.95), border=NA)
  }
  for (k in seq(2,length(xseqgreysep),by=2)){
    polygon(x=c(xseqgreysep[k],xseqgreysep[k+1],xseqgreysep[k+1],xseqgreysep[k]),
            y=c(-10,-10,15,15), col=lighten(background_colour,0.99), border=NA)
  }
  box()
}
par(new=TRUE)
plot(or_pest_cat_f[1:220], ylim=myrange,
     xaxt="n", yaxt="n",
     col=c(NA, mycolours[2], NA, mycolours[4],NA, mycolours[6], NA, mycolours[8],NA, mycolours[10],NA),
     pch = c(NA, 15, NA, 16,NA, 17, NA, 18,NA, 25,NA),
     lwd=1, ylab="", xlab="", cex=0.8)
x_pest <- 1:220
arrows(x0=x_pest, y0=up_pest_cat_f[1:220], x1=x_pest, y1=lo_pest_cat_f[1:220], code=3, angle=90, length=0.03, 
       col=c(NA, mycolours[2], NA, mycolours[4],NA, mycolours[6], NA, mycolours[8],NA, mycolours[10],NA))
axis(side=4, at=axTicks(2))
mtext(side=4, text="Odds Ratio", line=2)
#xseqblack=c(xseq[!duplicated(variable_cat)]-myspacing/2, max(xseq)+myspacing/2)
#for (k in 1:(length(xseqblack)-1)){
#  axis(side=3, at=xseqblack[c(k,k+1)]+c(2,-2), line=0.5, labels=NA)
#}
#for (k in 1:(length(xseqblack)-1)){
#  axis(side=3, at=mean(xseqblack[c(k,k+1)]), line=0.2, tick=FALSE,
#       labels=unique(variable_cat)[k])
#}
#par(xpd=TRUE)

abline(h=1, lty = 2, col="gray48")
abline(v=xseqgreysep,lty=3,col="black",lwd=0.5)
for (k in 1:length(xseq)){
  mytext=labels_pest_1[k]
  if (grepl("m\\^", mytext)){
    mytext=gsub("m\\^","'~m^", mytext)
    mytext=sub(")","~')", mytext)
  }
  if (grepl("\\[", mytext)){
    mytext=gsub("\\[","'[", mytext)
    mytext=sub("\\(ug","~'(ug", mytext)
  }
  mytmp=eval(parse(text=paste0("expression(","'", mytext,"'",")")))
  labcol=ifelse(k<=15, "black", "goldenrod")
  axis(side=1, at=xseq[k], labels=mytmp, las=2, cex.axis=0.8, col = labcol, col.axis=labcol)
}
legend("topright", pch=c(15, 16, 17, 18, 25), 
       col=c(mycolours[c(2,4,6,8,10)]),
       legend=c(outcomes),
       cex=0.7, bg = "white", bty = "n")
dev.off()


n=5 # Number of lines per variable
myspacing=n*2+1
xseq=seq((n+1),21*myspacing, by=myspacing)
background=TRUE
myrange <- c(0,max(up_pest_cat_l, na.rm = T)+1)
myrange <- c(0,150)

background_colour="darkgrey"

pdf("../cat_figures/univ/OR_pest_cat_l2.pdf", height = 7, width = 13)
pdf("../cat_figures/univ/OR_pest_cat_b2.pdf", height = 7, width = 13)
pdf("../cat_figures/univ/OR_pest_cat_f2.pdf", height = 7, width = 13)
par(mar = c(10, 2, 2, 5))
plot(or_pest_cat_f[221:451], ylim=myrange, xlim=c(1,length(or_pest_cat_f[220:451])), type="n",
     xaxt="n", yaxt="n",
     col=c(mycolours[2], NA, mycolours[4],NA, mycolours[6], NA, mycolours[8],NA, mycolours[10],NA),
     lwd=1, ylab="",xlab="")
xseqgreysep=c(min(xseq)-myspacing/2,apply(rbind(xseq[-1],xseq[-length(xseq)]),2,mean),max(xseq)+myspacing/2)

if (background){
  for (k in seq(1,length(xseqgreysep),by=2)){
    polygon(x=c(xseqgreysep[k],xseqgreysep[k+1],xseqgreysep[k+1],xseqgreysep[k]),
            y=c(-10,-10,15,15), col=lighten(background_colour,0.95), border=NA)
  }
  for (k in seq(2,length(xseqgreysep),by=2)){
    polygon(x=c(xseqgreysep[k],xseqgreysep[k+1],xseqgreysep[k+1],xseqgreysep[k]),
            y=c(-10,-10,15,15), col=lighten(background_colour,0.99), border=NA)
  }
  box()
}
par(new=TRUE)
plot(or_pest_cat_f[221:451], ylim=myrange,
     xaxt="n", yaxt="n",
     col=c(NA, mycolours[2], NA, mycolours[4],NA, mycolours[6], NA, mycolours[8],NA, mycolours[10],NA),
     pch = c(NA, 15, NA, 16,NA, 17, NA, 18,NA, 25,NA),
     lwd=1, ylab="", xlab="", cex=0.8)
x_pest <- 1:231
arrows(x0=x_pest, y0=up_pest_cat_f[221:451], x1=x_pest, y1=lo_pest_cat_f[221:451], code=3, angle=90, length=0.03, 
       col=c(NA, mycolours[2], NA, mycolours[4],NA, mycolours[6], NA, mycolours[8],NA, mycolours[10],NA))
axis(side=4, at=axTicks(2))
mtext(side=4, text="Odds Ratio", line=2)
#xseqblack=c(xseq[!duplicated(variable_cat)]-myspacing/2, max(xseq)+myspacing/2)
#for (k in 1:(length(xseqblack)-1)){
#  axis(side=3, at=xseqblack[c(k,k+1)]+c(2,-2), line=0.5, labels=NA)
#}
#for (k in 1:(length(xseqblack)-1)){
#  axis(side=3, at=mean(xseqblack[c(k,k+1)]), line=0.2, tick=FALSE,
#       labels=unique(variable_cat)[k])
#}
#par(xpd=TRUE)

abline(h=1, lty = 2, col="gray48")
abline(v=xseqgreysep,lty=3,col="black",lwd=0.5)
for (k in 1:length(xseq)){
  mytext=labels_pest_2[k]
  if (grepl("m\\^", mytext)){
    mytext=gsub("m\\^","'~m^", mytext)
    mytext=sub(")","~')", mytext)
  }
  if (grepl("\\[", mytext)){
    mytext=gsub("\\[","'[", mytext)
    mytext=sub("\\(ug","~'(ug", mytext)
  }
  mytmp=eval(parse(text=paste0("expression(","'", mytext,"'",")")))
  #labcol=ifelse((manhat_pahs_1[k,1]>=bonf & manhat[k,2]>=bonf), mycolours[3],
  #ifelse(manhat[k,1]>=bonf,mycolours[1],
  #ifelse(manhat[k,2]>=bonf, mycolours[2],"black")))
  #if(is.na(labcol)){
  #labcol=ifelse(manhat[k,1]>=bonf,mycolours[1],
  #ifelse(manhat[k,2]>=bonf, mycolours[2],"black"))
  #}
  #labfont=ifelse(labcol=="black",1,2)
  axis(side=1, at=xseq[k], labels=mytmp, las=2, cex.axis = 0.8, col = "lightpink3", col.axis="lightpink3")
}
legend("topright", pch=c(15, 16, 17, 18, 25), 
       col=c(mycolours[c(2,4,6,8,10)]),
       legend=c(outcomes),
       cex=0.7, bg = "white", bty = "n")
dev.off()


n=5 # Number of lines per variable
myspacing=n*2+1
xseq=seq((n+1),18*myspacing, by=myspacing)
background=TRUE
myrange <- c(0,max(or_pest_cat_l, na.rm = T)+3)
myrange <- c(0, 50)

background_colour="darkgrey"

pdf("../cat_figures/univ/OR_pest_cat_l3.pdf", height = 7, width = 13)
pdf("../cat_figures/univ/OR_pest_cat_b3.pdf", height = 7, width = 13)
pdf("../cat_figures/univ/OR_pest_cat_f3.pdf", height = 7, width = 13)
pdf("../cat_figures/univ/OR_pest_cat_f3_zoom.pdf", height = 7, width = 13)
par(mar = c(10, 2, 2, 5))
plot(or_pest_cat_f[452:649], ylim=myrange, xlim=c(1,length(or_pest_cat_f[452:649])), type="n",
     xaxt="n", yaxt="n",
     col=c(mycolours[2], NA, mycolours[4],NA, mycolours[6], NA, mycolours[8],NA, mycolours[10],NA),
     lwd=1, ylab="",xlab="")
xseqgreysep=c(min(xseq)-myspacing/2,apply(rbind(xseq[-1],xseq[-length(xseq)]),2,mean),max(xseq)+myspacing/2)

if (background){
  for (k in seq(1,length(xseqgreysep),by=2)){
    polygon(x=c(xseqgreysep[k],xseqgreysep[k+1],xseqgreysep[k+1],xseqgreysep[k]),
            y=c(-10,-10,15,15), col=lighten(background_colour,0.95), border=NA)
  }
  for (k in seq(2,length(xseqgreysep),by=2)){
    polygon(x=c(xseqgreysep[k],xseqgreysep[k+1],xseqgreysep[k+1],xseqgreysep[k]),
            y=c(-10,-10,15,15), col=lighten(background_colour,0.99), border=NA)
  }
  box()
}
par(new=TRUE)
plot(or_pest_cat_f[452:649], ylim=myrange,
     xaxt="n", yaxt="n",
     col=c(NA, mycolours[2], NA, mycolours[4],NA, mycolours[6], NA, mycolours[8],NA, mycolours[10],NA),
     pch = c(NA, 15, NA, 16,NA, 17, NA, 18,NA, 25,NA),
     lwd=1, ylab="", xlab="", cex=0.8)
x_pest <- 1:198
arrows(x0=x_pest, y0=up_pest_cat_f[452:649], x1=x_pest, y1=lo_pest_cat_f[452:649], code=3, angle=90, length=0.03, 
       col=c(NA, mycolours[2], NA, mycolours[4],NA, mycolours[6], NA, mycolours[8],NA, mycolours[10],NA))
axis(side=4, at=axTicks(2))
mtext(side=4, text="Odds Ratio", line=2)
#xseqblack=c(xseq[!duplicated(variable_cat)]-myspacing/2, max(xseq)+myspacing/2)
#for (k in 1:(length(xseqblack)-1)){
#  axis(side=3, at=xseqblack[c(k,k+1)]+c(2,-2), line=0.5, labels=NA)
#}
#for (k in 1:(length(xseqblack)-1)){
#  axis(side=3, at=mean(xseqblack[c(k,k+1)]), line=0.2, tick=FALSE,
#       labels=unique(variable_cat)[k])
#}
#par(xpd=TRUE)

abline(h=1, lty = 2, col="gray48")
abline(v=xseqgreysep,lty=3,col="black",lwd=0.5)
for (k in 1:length(xseq)){
  mytext=labels_pest_3[k]
  if (grepl("m\\^", mytext)){
    mytext=gsub("m\\^","'~m^", mytext)
    mytext=sub(")","~')", mytext)
  }
  if (grepl("\\[", mytext)){
    mytext=gsub("\\[","'[", mytext)
    mytext=sub("\\(ug","~'(ug", mytext)
  }
  mytmp=eval(parse(text=paste0("expression(","'", mytext,"'",")")))
  #labcol=ifelse((manhat_pahs_1[k,1]>=bonf & manhat[k,2]>=bonf), mycolours[3],
  #ifelse(manhat[k,1]>=bonf,mycolours[1],
  #ifelse(manhat[k,2]>=bonf, mycolours[2],"black")))
  #if(is.na(labcol)){
  #labcol=ifelse(manhat[k,1]>=bonf,mycolours[1],
  #ifelse(manhat[k,2]>=bonf, mycolours[2],"black"))
  #}
  #labfont=ifelse(labcol=="black",1,2)
  axis(side=1, at=xseq[k], labels=mytmp, las=2, cex.axis = 0.8, col = "lightpink3", col.axis="lightpink3")
}
legend("topright", pch=c(15, 16, 17, 18, 25), 
       col=c(mycolours[c(2,4,6,8,10)]),
       legend=c(outcomes),
       cex=0.7, bg = "white", bty = "n")
dev.off()



rownames(forest_pest_b)
labels_pest <- c(rownames(forest_pest_b)[1:3], "2-IPP", rownames(forest_pest_b)[5:13],
                 "3Me4NP", "Cl2CA", "3-PBA", "Fipronil sulfone", "Trifluralin")
n=5 # Number of lines per variable
myspacing=n*2+1
xseq=seq((n+1),18*myspacing, by=myspacing)
background=TRUE
myrange <- c(0,100)
myrange <- c(0,max(up_pest_f, na.rm = T)+1)

background_colour="darkgrey"

pdf("../cat_figures/univ/OR_pest_l.pdf", height = 7, width = 13)
pdf("../cat_figures/univ/OR_pest_b.pdf", height = 7, width = 13)
pdf("../cat_figures/univ/OR_pest_f.pdf", height = 7, width = 13)
par(mfrow=c(3,1))
par(mar = c(10, 2, 2, 5))
plot(or_pest_f, ylim=myrange, xlim=c(1,length(or_pest_f)), type="n",
     xaxt="n", yaxt="n",
     col=c(mycolours[1], NA, mycolours[2],NA, mycolours[3], NA, mycolours[4],NA, mycolours[5],NA),
     lwd=1, ylab="",xlab="")
xseqgreysep=c(min(xseq)-myspacing/2,apply(rbind(xseq[-1],xseq[-length(xseq)]),2,mean),max(xseq)+myspacing/2)

if (background){
  for (k in seq(1,length(xseqgreysep),by=2)){
    polygon(x=c(xseqgreysep[k],xseqgreysep[k+1],xseqgreysep[k+1],xseqgreysep[k]),
            y=c(-10,-10,15,15), col=lighten(background_colour,0.95), border=NA)
  }
  for (k in seq(2,length(xseqgreysep),by=2)){
    polygon(x=c(xseqgreysep[k],xseqgreysep[k+1],xseqgreysep[k+1],xseqgreysep[k]),
            y=c(-10,-10,15,15), col=lighten(background_colour,0.99), border=NA)
  }
  box()
}
par(new=TRUE)
plot(or_pest_f, ylim=myrange,
     xaxt="n", yaxt="n",
     col=c(NA, mycolours[2], NA, mycolours[4],NA, mycolours[6], NA, mycolours[8],NA, mycolours[10],NA),
     pch = c(NA, 15, NA, 16,NA, 17, NA, 18,NA, 25,NA),
     lwd=1, ylab="", xlab="", cex=0.8)
x_pest <- 1:198
arrows(x0=x_pest, y0=up_pest_f, x1=x_pest, y1=lo_pest_f, code=3, angle=90, length=0.03, 
       col=c(NA, mycolours[2], NA, mycolours[4],NA, mycolours[6], NA, mycolours[8],NA, mycolours[10],NA))
axis(side=4, at=axTicks(2))
mtext(side=4, text="Odds Ratio", line=2)
#xseqblack=c(xseq[!duplicated(variable_cat)]-myspacing/2, max(xseq)+myspacing/2)
#for (k in 1:(length(xseqblack)-1)){
#  axis(side=3, at=xseqblack[c(k,k+1)]+c(2,-2), line=0.5, labels=NA)
#}
#for (k in 1:(length(xseqblack)-1)){
#  axis(side=3, at=mean(xseqblack[c(k,k+1)]), line=0.2, tick=FALSE,
#       labels=unique(variable_cat)[k])
#}
#par(xpd=TRUE)

abline(h=1, lty = 2, col="gray48")
abline(v=xseqgreysep,lty=3,col="black",lwd=0.5)
for (k in 1:length(xseq)){
  mytext=labels_pest[k]
  if (grepl("m\\^", mytext)){
    mytext=gsub("m\\^","'~m^", mytext)
    mytext=sub(")","~')", mytext)
  }
  if (grepl("\\[", mytext)){
    mytext=gsub("\\[","'[", mytext)
    mytext=sub("\\(ug","~'(ug", mytext)
  }
  mytmp=eval(parse(text=paste0("expression(","'", mytext,"'",")")))
  labcol=ifelse(k<=5, "goldenrod", "lightpink3")
  axis(side=1, at=xseq[k], labels=mytmp, las=2, cex.axis=0.8, col = labcol, col.axis=labcol)
}
legend("topright", pch=c(15, 16, 17, 18, 25), 
       col=c(mycolours[c(2,4,6,8,10)]),
       legend=c(outcomes),
       cex=0.7, bg = "white", bty = "n")
dev.off()



#############################


##### Pest cont.
labels_pest <- c(rownames(forest_pest_b)[1:3], "2-IPP", rownames(forest_pest_b)[5:13],
                 "3Me4NP", "Cl2CA", "3-PBA", "Fipronil sulfone", "Trifluralin")
n=5 # Number of lines per variable
myspacing=n*2+1
xseq=seq((n+1),18*myspacing, by=myspacing)
background=TRUE


background_colour="darkgrey"

pdf("../cat_figures/univ/ppt/OR_pest_cont.pdf", height = 7, width = 13)
par(oma=c(10, 1, 1, 6), mfrow=c(3,1),las=0)
par(mar=c(0.25, 0, 0, 0),xpd=FALSE)
myrange <- c(0,4)
plot(or_pest_l, ylim=myrange, xlim=c(1,length(or_pest_l)), type="n",
     xaxt="n", yaxt="n",
     col=c(mycolours[1], NA, mycolours[2],NA, mycolours[3], NA, mycolours[4],NA, mycolours[5],NA),
     lwd=1, ylab="",xlab="")
xseqgreysep=c(min(xseq)-myspacing/2,apply(rbind(xseq[-1],xseq[-length(xseq)]),2,mean),max(xseq)+myspacing/2)

if (background){
  for (k in seq(1,length(xseqgreysep),by=2)){
    polygon(x=c(xseqgreysep[k],xseqgreysep[k+1],xseqgreysep[k+1],xseqgreysep[k]),
            y=c(-10,-10,15,15), col=lighten(background_colour,0.95), border=NA)
  }
  for (k in seq(2,length(xseqgreysep),by=2)){
    polygon(x=c(xseqgreysep[k],xseqgreysep[k+1],xseqgreysep[k+1],xseqgreysep[k]),
            y=c(-10,-10,15,15), col=lighten(background_colour,0.99), border=NA)
  }
  box()
}
par(new=TRUE)
plot(or_pest_l, ylim=myrange,
     xaxt="n", yaxt="n",
     col=c(NA, mycolours[2], NA, mycolours[4],NA, mycolours[6], NA, mycolours[8],NA, mycolours[10],NA),
     pch = c(NA, 15, NA, 16,NA, 17, NA, 18,NA, 25,NA),
     lwd=1, ylab="", xlab="", cex=0.8)
x_pest <- 1:198
arrows(x0=x_pest, y0=up_pest_l, x1=x_pest, y1=lo_pest_l, code=3, angle=90, length=0.03, 
       col=c(NA, mycolours[2], NA, mycolours[4],NA, mycolours[6], NA, mycolours[8],NA, mycolours[10],NA))
axis(side=4, at=axTicks(2))
abline(h=1, lty = 2, col="gray48")
abline(v=xseqgreysep,lty=3,col="black",lwd=0.5)
legend("topright", pch=c(15, 16, 17, 18, 25), 
       col=c(mycolours[c(2,4,6,8,10)]),
       legend=c(outcomes), ncol = 2,
       cex=0.7, bg = "white", bty = "n")

par(mar=c(0, 0, 0.25, 0),xpd=FALSE)
myrange <- c(0,8)
plot(or_pest_b, ylim=myrange, xlim=c(1,length(or_pest_b)), type="n",
     xaxt="n", yaxt="n",
     col=c(mycolours[1], NA, mycolours[2],NA, mycolours[3], NA, mycolours[4],NA, mycolours[5],NA),
     lwd=1, ylab="",xlab="")
xseqgreysep=c(min(xseq)-myspacing/2,apply(rbind(xseq[-1],xseq[-length(xseq)]),2,mean),max(xseq)+myspacing/2)

if (background){
  for (k in seq(1,length(xseqgreysep),by=2)){
    polygon(x=c(xseqgreysep[k],xseqgreysep[k+1],xseqgreysep[k+1],xseqgreysep[k]),
            y=c(-10,-10,15,15), col=lighten(background_colour,0.95), border=NA)
  }
  for (k in seq(2,length(xseqgreysep),by=2)){
    polygon(x=c(xseqgreysep[k],xseqgreysep[k+1],xseqgreysep[k+1],xseqgreysep[k]),
            y=c(-10,-10,15,15), col=lighten(background_colour,0.99), border=NA)
  }
  box()
}
par(new=TRUE)
plot(or_pest_b, ylim=myrange,
     xaxt="n", yaxt="n",
     col=c(NA, mycolours[2], NA, mycolours[4],NA, mycolours[6], NA, mycolours[8],NA, mycolours[10],NA),
     pch = c(NA, 15, NA, 16,NA, 17, NA, 18,NA, 25,NA),
     lwd=1, ylab="", xlab="", cex=0.8)
x_pest <- 1:198
arrows(x0=x_pest, y0=up_pest_b, x1=x_pest, y1=lo_pest_b, code=3, angle=90, length=0.03, 
       col=c(NA, mycolours[2], NA, mycolours[4],NA, mycolours[6], NA, mycolours[8],NA, mycolours[10],NA))
axis(side=4, at=axTicks(2))
mtext(side=4, text="Odds Ratio", line=2)

abline(h=1, lty = 2, col="gray48")
abline(v=xseqgreysep,lty=3,col="black",lwd=0.5)

par(mar=c(0, 0, 0.5, 0),xpd=FALSE)
myrange <- c(0, 10)
plot(or_pest_f, ylim=myrange, xlim=c(1,length(or_pest_f)), type="n",
     xaxt="n", yaxt="n",
     col=c(mycolours[1], NA, mycolours[2],NA, mycolours[3], NA, mycolours[4],NA, mycolours[5],NA),
     lwd=1, ylab="",xlab="")
xseqgreysep=c(min(xseq)-myspacing/2,apply(rbind(xseq[-1],xseq[-length(xseq)]),2,mean),max(xseq)+myspacing/2)

if (background){
  for (k in seq(1,length(xseqgreysep),by=2)){
    polygon(x=c(xseqgreysep[k],xseqgreysep[k+1],xseqgreysep[k+1],xseqgreysep[k]),
            y=c(-10,-10,15,15), col=lighten(background_colour,0.95), border=NA)
  }
  for (k in seq(2,length(xseqgreysep),by=2)){
    polygon(x=c(xseqgreysep[k],xseqgreysep[k+1],xseqgreysep[k+1],xseqgreysep[k]),
            y=c(-10,-10,15,15), col=lighten(background_colour,0.99), border=NA)
  }
  box()
}
par(new=TRUE)
plot(or_pest_f, ylim=myrange,
     xaxt="n", yaxt="n",
     col=c(NA, mycolours[2], NA, mycolours[4],NA, mycolours[6], NA, mycolours[8],NA, mycolours[10],NA),
     pch = c(NA, 15, NA, 16,NA, 17, NA, 18,NA, 25,NA),
     lwd=1, ylab="", xlab="", cex=0.8)
x_pest <- 1:198
arrows(x0=x_pest, y0=up_pest_f, x1=x_pest, y1=lo_pest_f, code=3, angle=90, length=0.03, 
       col=c(NA, mycolours[2], NA, mycolours[4],NA, mycolours[6], NA, mycolours[8],NA, mycolours[10],NA))
axis(side=4, at=axTicks(2))

abline(h=1, lty = 2, col="gray48")
abline(v=xseqgreysep,lty=3,col="black",lwd=0.5)

for (k in 1:length(xseq)){
  mytext=labels_pest[k]
  if (grepl("m\\^", mytext)){
    mytext=gsub("m\\^","'~m^", mytext)
    mytext=sub(")","~')", mytext)
  }
  if (grepl("\\[", mytext)){
    mytext=gsub("\\[","'[", mytext)
    mytext=sub("\\(ug","~'(ug", mytext)
  }
  mytmp=eval(parse(text=paste0("expression(","'", mytext,"'",")")))
  labcol=ifelse(k<=5, "goldenrod", "lightpink3")
  axis(side=1, at=xseq[k], labels=mytmp, las=2, cex.axis=0.8, col = labcol, col.axis=labcol)
}

dev.off()


##### PAHs cont. --
labels_pahs <- c("Cotinine", "2-OH-Fluorene", "Nicotine", "1-OH-Naphthalene", "2-OH-Naphthalene")
n=5 # Number of lines per variable
myspacing=n*2+1
xseq=seq((n+1),5*myspacing, by=myspacing)
background=TRUE
myrange <- c(0,max(up_pahs_b, na.rm = T)+0.5)
myrange <- c(0,4)

background_colour="darkgrey"

pdf("../cat_figures/univ/ppt/OR_pah_cont.pdf", height = 7, width = 13)
par(oma=c(10, 1, 1, 6), mfrow=c(2,1),las=0)
par(mar=c(0.5, 0, 0, 0),xpd=FALSE)
# Luxembourg
plot(or_pahs_l, ylim=myrange, xlim=c(1,length(or_pahs_l)), type="n",
     xaxt="n", yaxt="n",
     col=c(mycolours[2], NA, mycolours[4],NA, mycolours[6], NA, mycolours[8],NA, mycolours[10],NA),
     lwd=1, ylab="",xlab="")
xseqgreysep=c(min(xseq)-myspacing/2,apply(rbind(xseq[-1],xseq[-length(xseq)]),2,mean),max(xseq)+myspacing/2)

if (background){
  for (k in seq(1,length(xseqgreysep),by=2)){
    polygon(x=c(xseqgreysep[k],xseqgreysep[k+1],xseqgreysep[k+1],xseqgreysep[k]),
            y=c(-10,-10,15,15), col=lighten(background_colour,0.95), border=NA)
  }
  for (k in seq(2,length(xseqgreysep),by=2)){
    polygon(x=c(xseqgreysep[k],xseqgreysep[k+1],xseqgreysep[k+1],xseqgreysep[k]),
            y=c(-10,-10,15,15), col=lighten(background_colour,0.99), border=NA)
  }
  box()
}
par(new=TRUE)
plot(or_pahs_l, ylim=myrange,
     xaxt="n", yaxt="n",
     col=c(NA, mycolours[2], NA, mycolours[4],NA, mycolours[6], NA, mycolours[8],NA, mycolours[10],NA),
     pch = c(NA, 15, NA, 16,NA, 17, NA, 18,NA, 25,NA),
     lwd=1, ylab="", xlab="")
x_pahs <- 1:55
arrows(x0=x_pahs, y0=up_pahs_l, x1=x_pahs, y1=lo_pahs_l, code=3, angle=90, length=0.05, 
       col=c(NA, mycolours[2], NA, mycolours[4],NA, mycolours[6], NA, mycolours[8],NA, mycolours[10],NA))
axis(side=4, at=axTicks(2))
mtext(side=4, text="Odds Ratio", line=2)

abline(h=1, lty = 2, col="gray48")
abline(v=xseqgreysep,lty=3,col="black",lwd=0.5)

legend("topright", pch=c(15, 16, 17, 18, 25), 
       col=c(mycolours[c(2,4,6,8,10)]),
       legend=c(outcomes), ncol = 2,
       cex=0.7, bg = "white", bty = "n")
# Belgium
par(mar=c(0, 0, 0.5, 0),xpd=FALSE)
plot(or_pahs_b, ylim=myrange, xlim=c(1,length(or_pahs_b)), type="n",
     xaxt="n", yaxt="n",
     col=c(mycolours[2], NA, mycolours[4],NA, mycolours[6], NA, mycolours[8],NA, mycolours[10],NA),
     lwd=1, ylab="",xlab="")
xseqgreysep=c(min(xseq)-myspacing/2,apply(rbind(xseq[-1],xseq[-length(xseq)]),2,mean),max(xseq)+myspacing/2)

if (background){
  for (k in seq(1,length(xseqgreysep),by=2)){
    polygon(x=c(xseqgreysep[k],xseqgreysep[k+1],xseqgreysep[k+1],xseqgreysep[k]),
            y=c(-10,-10,15,15), col=lighten(background_colour,0.95), border=NA)
  }
  for (k in seq(2,length(xseqgreysep),by=2)){
    polygon(x=c(xseqgreysep[k],xseqgreysep[k+1],xseqgreysep[k+1],xseqgreysep[k]),
            y=c(-10,-10,15,15), col=lighten(background_colour,0.99), border=NA)
  }
  box()
}
par(new=TRUE)
plot(or_pahs_b, ylim=myrange,
     xaxt="n", yaxt="n",
     col=c(NA, mycolours[2], NA, mycolours[4],NA, mycolours[6], NA, mycolours[8],NA, mycolours[10],NA),
     pch = c(NA, 15, NA, 16,NA, 17, NA, 18,NA, 25,NA),
     lwd=1, ylab="", xlab="")
x_pahs <- 1:55
arrows(x0=x_pahs, y0=up_pahs_b, x1=x_pahs, y1=lo_pahs_b, code=3, angle=90, length=0.05, 
       col=c(NA, mycolours[2], NA, mycolours[4],NA, mycolours[6], NA, mycolours[8],NA, mycolours[10],NA))
axis(side=4, at=axTicks(2))
mtext(side=4, text="Odds Ratio", line=2)

abline(h=1, lty = 2, col="gray48")
abline(v=xseqgreysep,lty=3,col="black",lwd=0.5)

for (k in 1:length(xseq)){
  mytext=labels_pahs[k]
  if (grepl("m\\^", mytext)){
    mytext=gsub("m\\^","'~m^", mytext)
    mytext=sub(")","~')", mytext)
  }
  if (grepl("\\[", mytext)){
    mytext=gsub("\\[","'[", mytext)
    mytext=sub("\\(ug","~'(ug", mytext)
  }
  mytmp=eval(parse(text=paste0("expression(","'", mytext,"'",")")))
  labcol=ifelse(k<=2,  "goldenrod", "lightpink3")
  axis(side=1, at=xseq[k], labels=mytmp, las=2, cex.axis=0.7, col = labcol, col.axis=labcol)
}
dev.off()



##### Pest cat --
labels_pest_1 <- c("PCB 138", "PCB 153", "PCB 180", "BDE 47", colnames(pest)[c(14:22)],
                   "Carbofuran phenol", colnames(pest)[24:27], "2 IPP", colnames(pest)[29])
labels_pest_2 <- rownames(forest_pest_cat_b)[21:41]
labels_pest_3 <- c(rownames(forest_pest_cat_b)[42:44], "3Me4NP2", "3Me4NP3", "3Me4NP4",
                   rownames(forest_pest_cat_b)[48:50], "3PBA2", "3PBA3", "3PBA4",
                   "Fipronil sulfone2", "Fipronil_sulfone3", "Fipronil sulfone4",
                   rownames(forest_pest_cat_b)[57:59])


n=5 # Number of lines per variable
myspacing=n*2+1
xseq=seq((n+1),20*myspacing, by=myspacing)
background=TRUE
myrange <- c(0,30)
myrange <- c(0,max(or_pest_cat_f, na.rm = T)+3)
myrange <- c(0,max(or_pest_cat, na.rm = T)+3)

background_colour="darkgrey"

pdf("../cat_figures/univ/ppt/OR_pest_cat_contry1.pdf", height = 7, width = 13)
par(oma=c(12, 1, 1, 6), mfrow=c(3,1),las=0)
par(mar=c(0.5, 0, 0, 0),xpd=FALSE)
plot(or_pest_cat_l[1:220], ylim=myrange, xlim=c(1,length(or_pest_cat_l[1:220])), type="n",
     xaxt="n", yaxt="n",
     col=c(mycolours[1], NA, mycolours[2],NA, mycolours[3], NA, mycolours[4],NA, mycolours[5],NA),
     lwd=1, ylab="",xlab="")
xseqgreysep=c(min(xseq)-myspacing/2,apply(rbind(xseq[-1],xseq[-length(xseq)]),2,mean),max(xseq)+myspacing/2)

if (background){
  for (k in seq(1,length(xseqgreysep),by=2)){
    polygon(x=c(xseqgreysep[k],xseqgreysep[k+1],xseqgreysep[k+1],xseqgreysep[k]),
            y=c(-10,-10,15,15), col=lighten(background_colour,0.95), border=NA)
  }
  for (k in seq(2,length(xseqgreysep),by=2)){
    polygon(x=c(xseqgreysep[k],xseqgreysep[k+1],xseqgreysep[k+1],xseqgreysep[k]),
            y=c(-10,-10,15,15), col=lighten(background_colour,0.99), border=NA)
  }
  box()
}
par(new=TRUE)
plot(or_pest_cat_l[1:220], ylim=myrange,
     xaxt="n", yaxt="n",
     col=c(NA, mycolours[2], NA, mycolours[4],NA, mycolours[6], NA, mycolours[8],NA, mycolours[10],NA),
     pch = c(NA, 15, NA, 16,NA, 17, NA, 18,NA, 25,NA),
     lwd=1, ylab="", xlab="", cex=0.8)
x_pest <- 1:220
arrows(x0=x_pest, y0=up_pest_cat_l[1:220], x1=x_pest, y1=lo_pest_cat_l[1:220], code=3, angle=90, length=0.03, 
       col=c(NA, mycolours[2], NA, mycolours[4],NA, mycolours[6], NA, mycolours[8],NA, mycolours[10],NA))
axis(side=4, at=axTicks(2))

abline(h=1, lty = 2, col="gray48")
abline(v=xseqgreysep,lty=3,col="black",lwd=0.5)
legend("topright", pch=c(15, 16, 17, 18, 25), 
       col=c(mycolours[c(2,4,6,8,10)]),
       legend=c(outcomes), ncol = 2,
       cex=0.7, bg = "white", bty = "n")

par(mar=c(0.25, 0, 0, 0),xpd=FALSE)
plot(or_pest_cat_b[1:220], ylim=myrange, xlim=c(1,length(or_pest_cat_b[1:220])), type="n",
     xaxt="n", yaxt="n",
     col=c(mycolours[1], NA, mycolours[2],NA, mycolours[3], NA, mycolours[4],NA, mycolours[5],NA),
     lwd=1, ylab="",xlab="")
xseqgreysep=c(min(xseq)-myspacing/2,apply(rbind(xseq[-1],xseq[-length(xseq)]),2,mean),max(xseq)+myspacing/2)

if (background){
  for (k in seq(1,length(xseqgreysep),by=2)){
    polygon(x=c(xseqgreysep[k],xseqgreysep[k+1],xseqgreysep[k+1],xseqgreysep[k]),
            y=c(-10,-10,15,15), col=lighten(background_colour,0.95), border=NA)
  }
  for (k in seq(2,length(xseqgreysep),by=2)){
    polygon(x=c(xseqgreysep[k],xseqgreysep[k+1],xseqgreysep[k+1],xseqgreysep[k]),
            y=c(-10,-10,15,15), col=lighten(background_colour,0.99), border=NA)
  }
  box()
}
par(new=TRUE)
plot(or_pest_cat_b[1:220], ylim=myrange,
     xaxt="n", yaxt="n",
     col=c(NA, mycolours[2], NA, mycolours[4],NA, mycolours[6], NA, mycolours[8],NA, mycolours[10],NA),
     pch = c(NA, 15, NA, 16,NA, 17, NA, 18,NA, 25,NA),
     lwd=1, ylab="", xlab="", cex=0.8)
x_pest <- 1:220
arrows(x0=x_pest, y0=up_pest_cat_b[1:220], x1=x_pest, y1=lo_pest_cat_b[1:220], code=3, angle=90, length=0.03, 
       col=c(NA, mycolours[2], NA, mycolours[4],NA, mycolours[6], NA, mycolours[8],NA, mycolours[10],NA))
axis(side=4, at=axTicks(2))
abline(h=1, lty = 2, col="gray48")
mtext(side=4, text="Odds Ratio", line=2)
abline(v=xseqgreysep,lty=3,col="black",lwd=0.5)

par(mar=c(0.5, 0, 0, 0),xpd=FALSE)
plot(or_pest_cat_f[1:220], ylim=myrange, xlim=c(1,length(or_pest_cat_f[1:220])), type="n",
     xaxt="n", yaxt="n",
     col=c(mycolours[1], NA, mycolours[2],NA, mycolours[3], NA, mycolours[4],NA, mycolours[5],NA),
     lwd=1, ylab="",xlab="")
xseqgreysep=c(min(xseq)-myspacing/2,apply(rbind(xseq[-1],xseq[-length(xseq)]),2,mean),max(xseq)+myspacing/2)

if (background){
  for (k in seq(1,length(xseqgreysep),by=2)){
    polygon(x=c(xseqgreysep[k],xseqgreysep[k+1],xseqgreysep[k+1],xseqgreysep[k]),
            y=c(-10,-10,15,15), col=lighten(background_colour,0.95), border=NA)
  }
  for (k in seq(2,length(xseqgreysep),by=2)){
    polygon(x=c(xseqgreysep[k],xseqgreysep[k+1],xseqgreysep[k+1],xseqgreysep[k]),
            y=c(-10,-10,15,15), col=lighten(background_colour,0.99), border=NA)
  }
  box()
}
par(new=TRUE)
plot(or_pest_cat_f[1:220], ylim=myrange,
     xaxt="n", yaxt="n",
     col=c(NA, mycolours[2], NA, mycolours[4],NA, mycolours[6], NA, mycolours[8],NA, mycolours[10],NA),
     pch = c(NA, 15, NA, 16,NA, 17, NA, 18,NA, 25,NA),
     lwd=1, ylab="", xlab="", cex=0.8)
x_pest <- 1:220
arrows(x0=x_pest, y0=up_pest_cat_f[1:220], x1=x_pest, y1=lo_pest_cat_f[1:220], code=3, angle=90, length=0.03, 
       col=c(NA, mycolours[2], NA, mycolours[4],NA, mycolours[6], NA, mycolours[8],NA, mycolours[10],NA))
axis(side=4, at=axTicks(2))
abline(h=1, lty = 2, col="gray48")
abline(v=xseqgreysep,lty=3,col="black",lwd=0.5)

for (k in 1:length(xseq)){
  mytext=labels_pest_1[k]
  if (grepl("m\\^", mytext)){
    mytext=gsub("m\\^","'~m^", mytext)
    mytext=sub(")","~')", mytext)
  }
  if (grepl("\\[", mytext)){
    mytext=gsub("\\[","'[", mytext)
    mytext=sub("\\(ug","~'(ug", mytext)
  }
  mytmp=eval(parse(text=paste0("expression(","'", mytext,"'",")")))
  labcol=ifelse(k<=15, "black", "goldenrod")
  axis(side=1, at=xseq[k], labels=mytmp, las=2, cex.axis=0.8, col = labcol, col.axis=labcol)
}

dev.off()



n=5 # Number of lines per variable
myspacing=n*2+1
xseq=seq((n+1),21*myspacing, by=myspacing)
background=TRUE
myrange <- c(0,max(up_pest_cat_l, na.rm = T)+1)
myrange <- c(0,40)

background_colour="darkgrey"

pdf("../cat_figures/univ/ppt/OR_pest_cat_country2.pdf", height = 7, width = 13)
par(oma=c(12, 1, 1, 6), mfrow=c(3,1),las=0)
par(mar=c(0.25, 0, 0, 0),xpd=FALSE)
plot(or_pest_cat_l[221:451], ylim=myrange, xlim=c(1,length(or_pest_cat_l[220:451])), type="n",
     xaxt="n", yaxt="n",
     col=c(mycolours[2], NA, mycolours[4],NA, mycolours[6], NA, mycolours[8],NA, mycolours[10],NA),
     lwd=1, ylab="",xlab="")
xseqgreysep=c(min(xseq)-myspacing/2,apply(rbind(xseq[-1],xseq[-length(xseq)]),2,mean),max(xseq)+myspacing/2)

if (background){
  for (k in seq(1,length(xseqgreysep),by=2)){
    polygon(x=c(xseqgreysep[k],xseqgreysep[k+1],xseqgreysep[k+1],xseqgreysep[k]),
            y=c(-10,-10,15,15), col=lighten(background_colour,0.95), border=NA)
  }
  for (k in seq(2,length(xseqgreysep),by=2)){
    polygon(x=c(xseqgreysep[k],xseqgreysep[k+1],xseqgreysep[k+1],xseqgreysep[k]),
            y=c(-10,-10,15,15), col=lighten(background_colour,0.99), border=NA)
  }
  box()
}
par(new=TRUE)
plot(or_pest_cat_l[221:451], ylim=myrange,
     xaxt="n", yaxt="n",
     col=c(NA, mycolours[2], NA, mycolours[4],NA, mycolours[6], NA, mycolours[8],NA, mycolours[10],NA),
     pch = c(NA, 15, NA, 16,NA, 17, NA, 18,NA, 25,NA),
     lwd=1, ylab="", xlab="", cex=0.8)
x_pest <- 1:231
arrows(x0=x_pest, y0=up_pest_cat_l[221:451], x1=x_pest, y1=lo_pest_cat_l[221:451], code=3, angle=90, length=0.03, 
       col=c(NA, mycolours[2], NA, mycolours[4],NA, mycolours[6], NA, mycolours[8],NA, mycolours[10],NA))
axis(side=4, at=axTicks(2))
#mtext(side=4, text="Odds Ratio", line=2)
abline(h=1, lty = 2, col="gray48")
abline(v=xseqgreysep,lty=3,col="black",lwd=0.5)
legend("topright", pch=c(15, 16, 17, 18, 25), 
       col=c(mycolours[c(2,4,6,8,10)]),
       legend=c(outcomes), ncol = 2,
       cex=0.7, bg = "white", bty = "n")
# Belgium
par(mar=c(0.25, 0, 0, 0),xpd=FALSE)
plot(or_pest_cat_b[221:451], ylim=myrange, xlim=c(1,length(or_pest_cat_b[220:451])), type="n",
     xaxt="n", yaxt="n",
     col=c(mycolours[2], NA, mycolours[4],NA, mycolours[6], NA, mycolours[8],NA, mycolours[10],NA),
     lwd=1, ylab="",xlab="")
xseqgreysep=c(min(xseq)-myspacing/2,apply(rbind(xseq[-1],xseq[-length(xseq)]),2,mean),max(xseq)+myspacing/2)

if (background){
  for (k in seq(1,length(xseqgreysep),by=2)){
    polygon(x=c(xseqgreysep[k],xseqgreysep[k+1],xseqgreysep[k+1],xseqgreysep[k]),
            y=c(-10,-10,15,15), col=lighten(background_colour,0.95), border=NA)
  }
  for (k in seq(2,length(xseqgreysep),by=2)){
    polygon(x=c(xseqgreysep[k],xseqgreysep[k+1],xseqgreysep[k+1],xseqgreysep[k]),
            y=c(-10,-10,15,15), col=lighten(background_colour,0.99), border=NA)
  }
  box()
}
par(new=TRUE)
plot(or_pest_cat_b[221:451], ylim=myrange,
     xaxt="n", yaxt="n",
     col=c(NA, mycolours[2], NA, mycolours[4],NA, mycolours[6], NA, mycolours[8],NA, mycolours[10],NA),
     pch = c(NA, 15, NA, 16,NA, 17, NA, 18,NA, 25,NA),
     lwd=1, ylab="", xlab="", cex=0.8)
x_pest <- 1:231
arrows(x0=x_pest, y0=up_pest_cat_b[221:451], x1=x_pest, y1=lo_pest_cat_b[221:451], code=3, angle=90, length=0.03, 
       col=c(NA, mycolours[2], NA, mycolours[4],NA, mycolours[6], NA, mycolours[8],NA, mycolours[10],NA))
axis(side=4, at=axTicks(2))
mtext(side=4, text="Odds Ratio", line=2)
abline(h=1, lty = 2, col="gray48")
abline(v=xseqgreysep,lty=3,col="black",lwd=0.5)

# France
par(mar=c(0.5, 0, 0, 0),xpd=FALSE)
plot(or_pest_cat_f[221:451], ylim=myrange, xlim=c(1,length(or_pest_cat_f[220:451])), type="n",
     xaxt="n", yaxt="n",
     col=c(mycolours[2], NA, mycolours[4],NA, mycolours[6], NA, mycolours[8],NA, mycolours[10],NA),
     lwd=1, ylab="",xlab="")
xseqgreysep=c(min(xseq)-myspacing/2,apply(rbind(xseq[-1],xseq[-length(xseq)]),2,mean),max(xseq)+myspacing/2)

if (background){
  for (k in seq(1,length(xseqgreysep),by=2)){
    polygon(x=c(xseqgreysep[k],xseqgreysep[k+1],xseqgreysep[k+1],xseqgreysep[k]),
            y=c(-10,-10,15,15), col=lighten(background_colour,0.95), border=NA)
  }
  for (k in seq(2,length(xseqgreysep),by=2)){
    polygon(x=c(xseqgreysep[k],xseqgreysep[k+1],xseqgreysep[k+1],xseqgreysep[k]),
            y=c(-10,-10,15,15), col=lighten(background_colour,0.99), border=NA)
  }
  box()
}
par(new=TRUE)
plot(or_pest_cat_f[221:451], ylim=myrange,
     xaxt="n", yaxt="n",
     col=c(NA, mycolours[2], NA, mycolours[4],NA, mycolours[6], NA, mycolours[8],NA, mycolours[10],NA),
     pch = c(NA, 15, NA, 16,NA, 17, NA, 18,NA, 25,NA),
     lwd=1, ylab="", xlab="", cex=0.8)
x_pest <- 1:231
arrows(x0=x_pest, y0=up_pest_cat_f[221:451], x1=x_pest, y1=lo_pest_cat_f[221:451], code=3, angle=90, length=0.03, 
       col=c(NA, mycolours[2], NA, mycolours[4],NA, mycolours[6], NA, mycolours[8],NA, mycolours[10],NA))
axis(side=4, at=axTicks(2))
#mtext(side=4, text="Odds Ratio", line=2)
abline(h=1, lty = 2, col="gray48")
abline(v=xseqgreysep,lty=3,col="black",lwd=0.5)

for (k in 1:length(xseq)){
  mytext=labels_pest_2[k]
  if (grepl("m\\^", mytext)){
    mytext=gsub("m\\^","'~m^", mytext)
    mytext=sub(")","~')", mytext)
  }
  if (grepl("\\[", mytext)){
    mytext=gsub("\\[","'[", mytext)
    mytext=sub("\\(ug","~'(ug", mytext)
  }
  mytmp=eval(parse(text=paste0("expression(","'", mytext,"'",")")))
  axis(side=1, at=xseq[k], labels=mytmp, las=2, cex.axis = 0.8, col = "lightpink3", col.axis="lightpink3")
}

dev.off()


n=5 # Number of lines per variable
myspacing=n*2+1
xseq=seq((n+1),18*myspacing, by=myspacing)
background=TRUE
myrange <- c(0,max(or_pest_cat_l, na.rm = T)+3)
myrange <- c(0, 50)

background_colour="darkgrey"

pdf("../cat_figures/univ/ppt/OR_pest_cat_country3.pdf", height = 7, width = 13)
par(oma=c(12, 1, 1, 6), mfrow=c(3,1),las=0)
# Luxembourg
par(mar=c(0.25, 0, 0, 0),xpd=FALSE)
plot(or_pest_cat_l[452:649], ylim=myrange, xlim=c(1,length(or_pest_cat_l[452:649])), type="n",
     xaxt="n", yaxt="n",
     col=c(mycolours[2], NA, mycolours[4],NA, mycolours[6], NA, mycolours[8],NA, mycolours[10],NA),
     lwd=1, ylab="",xlab="")
xseqgreysep=c(min(xseq)-myspacing/2,apply(rbind(xseq[-1],xseq[-length(xseq)]),2,mean),max(xseq)+myspacing/2)

if (background){
  for (k in seq(1,length(xseqgreysep),by=2)){
    polygon(x=c(xseqgreysep[k],xseqgreysep[k+1],xseqgreysep[k+1],xseqgreysep[k]),
            y=c(-10,-10,15,15), col=lighten(background_colour,0.95), border=NA)
  }
  for (k in seq(2,length(xseqgreysep),by=2)){
    polygon(x=c(xseqgreysep[k],xseqgreysep[k+1],xseqgreysep[k+1],xseqgreysep[k]),
            y=c(-10,-10,15,15), col=lighten(background_colour,0.99), border=NA)
  }
  box()
}
par(new=TRUE)
plot(or_pest_cat_l[452:649], ylim=myrange,
     xaxt="n", yaxt="n",
     col=c(NA, mycolours[2], NA, mycolours[4],NA, mycolours[6], NA, mycolours[8],NA, mycolours[10],NA),
     pch = c(NA, 15, NA, 16,NA, 17, NA, 18,NA, 25,NA),
     lwd=1, ylab="", xlab="", cex=0.8)
x_pest <- 1:198
arrows(x0=x_pest, y0=up_pest_cat_l[452:649], x1=x_pest, y1=lo_pest_cat_l[452:649], code=3, angle=90, length=0.03, 
       col=c(NA, mycolours[2], NA, mycolours[4],NA, mycolours[6], NA, mycolours[8],NA, mycolours[10],NA))
axis(side=4, at=axTicks(2))
#mtext(side=4, text="Odds Ratio", line=2)
abline(h=1, lty = 2, col="gray48")
abline(v=xseqgreysep,lty=3,col="black",lwd=0.5)
legend("topright", pch=c(15, 16, 17, 18, 25), 
       col=c(mycolours[c(2,4,6,8,10)]),
       legend=c(outcomes), ncol = 2,
       cex=0.7, bg = "white", bty = "n")

# Belgium
par(mar=c(0.25, 0, 0, 0),xpd=FALSE)
plot(or_pest_cat_b[452:649], ylim=myrange, xlim=c(1,length(or_pest_cat_b[452:649])), type="n",
     xaxt="n", yaxt="n",
     col=c(mycolours[2], NA, mycolours[4],NA, mycolours[6], NA, mycolours[8],NA, mycolours[10],NA),
     lwd=1, ylab="",xlab="")
xseqgreysep=c(min(xseq)-myspacing/2,apply(rbind(xseq[-1],xseq[-length(xseq)]),2,mean),max(xseq)+myspacing/2)

if (background){
  for (k in seq(1,length(xseqgreysep),by=2)){
    polygon(x=c(xseqgreysep[k],xseqgreysep[k+1],xseqgreysep[k+1],xseqgreysep[k]),
            y=c(-10,-10,15,15), col=lighten(background_colour,0.95), border=NA)
  }
  for (k in seq(2,length(xseqgreysep),by=2)){
    polygon(x=c(xseqgreysep[k],xseqgreysep[k+1],xseqgreysep[k+1],xseqgreysep[k]),
            y=c(-10,-10,15,15), col=lighten(background_colour,0.99), border=NA)
  }
  box()
}
par(new=TRUE)
plot(or_pest_cat_b[452:649], ylim=myrange,
     xaxt="n", yaxt="n",
     col=c(NA, mycolours[2], NA, mycolours[4],NA, mycolours[6], NA, mycolours[8],NA, mycolours[10],NA),
     pch = c(NA, 15, NA, 16,NA, 17, NA, 18,NA, 25,NA),
     lwd=1, ylab="", xlab="", cex=0.8)
x_pest <- 1:198
arrows(x0=x_pest, y0=up_pest_cat_b[452:649], x1=x_pest, y1=lo_pest_cat_l[452:649], code=3, angle=90, length=0.03, 
       col=c(NA, mycolours[2], NA, mycolours[4],NA, mycolours[6], NA, mycolours[8],NA, mycolours[10],NA))
axis(side=4, at=axTicks(2))
mtext(side=4, text="Odds Ratio", line=2)
abline(h=1, lty = 2, col="gray48")
abline(v=xseqgreysep,lty=3,col="black",lwd=0.5)

# France
par(mar=c(0.25, 0, 0, 0),xpd=FALSE)
plot(or_pest_cat_f[452:649], ylim=myrange, xlim=c(1,length(or_pest_cat_f[452:649])), type="n",
     xaxt="n", yaxt="n",
     col=c(mycolours[2], NA, mycolours[4],NA, mycolours[6], NA, mycolours[8],NA, mycolours[10],NA),
     lwd=1, ylab="",xlab="")
xseqgreysep=c(min(xseq)-myspacing/2,apply(rbind(xseq[-1],xseq[-length(xseq)]),2,mean),max(xseq)+myspacing/2)

if (background){
  for (k in seq(1,length(xseqgreysep),by=2)){
    polygon(x=c(xseqgreysep[k],xseqgreysep[k+1],xseqgreysep[k+1],xseqgreysep[k]),
            y=c(-10,-10,15,15), col=lighten(background_colour,0.95), border=NA)
  }
  for (k in seq(2,length(xseqgreysep),by=2)){
    polygon(x=c(xseqgreysep[k],xseqgreysep[k+1],xseqgreysep[k+1],xseqgreysep[k]),
            y=c(-10,-10,15,15), col=lighten(background_colour,0.99), border=NA)
  }
  box()
}
par(new=TRUE)
plot(or_pest_cat_f[452:649], ylim=myrange,
     xaxt="n", yaxt="n",
     col=c(NA, mycolours[2], NA, mycolours[4],NA, mycolours[6], NA, mycolours[8],NA, mycolours[10],NA),
     pch = c(NA, 15, NA, 16,NA, 17, NA, 18,NA, 25,NA),
     lwd=1, ylab="", xlab="", cex=0.8)
x_pest <- 1:198
arrows(x0=x_pest, y0=up_pest_cat_f[452:649], x1=x_pest, y1=lo_pest_cat_f[452:649], code=3, angle=90, length=0.03, 
       col=c(NA, mycolours[2], NA, mycolours[4],NA, mycolours[6], NA, mycolours[8],NA, mycolours[10],NA))
axis(side=4, at=axTicks(2))
#mtext(side=4, text="Odds Ratio", line=2)
abline(h=1, lty = 2, col="gray48")
abline(v=xseqgreysep,lty=3,col="black",lwd=0.5)
       
for (k in 1:length(xseq)){
  mytext=labels_pest_3[k]
  if (grepl("m\\^", mytext)){
    mytext=gsub("m\\^","'~m^", mytext)
    mytext=sub(")","~')", mytext)
  }
  if (grepl("\\[", mytext)){
    mytext=gsub("\\[","'[", mytext)
    mytext=sub("\\(ug","~'(ug", mytext)
  }
  mytmp=eval(parse(text=paste0("expression(","'", mytext,"'",")")))
  axis(side=1, at=xseq[k], labels=mytmp, las=2, cex.axis = 0.8, col = "lightpink3", col.axis="lightpink3")
}

dev.off()


##### PAHs cat --

labels_pahs <-c("9-OH-Fluorene", "3-OH-Fluorene", "4-OH-Phenanthrene", "3-OH-Phenanthrene",
                "1-OH-Phenanthrene", "2-OH-Phenanthrene", "1-OH-Pyrene", 
                "6-OH-Chrysene11-OH-Benz(a)anthrac", "Cotinine", "2-OH-Fluorene",                    
                "Nicotine2", "Nicotine3", "Nicotine4",
                "1-OH-Naphthalene2", "1-OH-Naphthalene3", "1-OH-Naphthalene4", 
                "2-OH-Naphthalene2", "2-OH-Naphthalene3", "2-OH-Naphthalene4")

n=5 # Number of lines per variable
myspacing=n*2+1
xseq=seq((n+1),19*myspacing, by=myspacing)
background=TRUE

background_colour="darkgrey"

pdf("../cat_figures/univ/ppt/OR_pah_cat.pdf", height = 7, width = 13)
par(oma=c(12, 1, 1, 6), mfrow=c(2,1),las=0)
par(mar=c(0.5, 0, 0, 0),xpd=FALSE)
# Luxembourg
myrange <- c(0,33)
plot(or_pahs_cat_l, ylim=myrange, xlim=c(1,length(or_pahs_cat_l)), type="n",
     xaxt="n", yaxt="n",
     col=c(mycolours[2], NA, mycolours[4],NA, mycolours[6], NA, mycolours[8],NA, mycolours[10],NA),
     lwd=1, ylab="",xlab="")
xseqgreysep=c(min(xseq)-myspacing/2,apply(rbind(xseq[-1],xseq[-length(xseq)]),2,mean),max(xseq)+myspacing/2)

if (background){
  for (k in seq(1,length(xseqgreysep),by=2)){
    polygon(x=c(xseqgreysep[k],xseqgreysep[k+1],xseqgreysep[k+1],xseqgreysep[k]),
            y=c(-10,-10,15,15), col=lighten(background_colour,0.95), border=NA)
  }
  for (k in seq(2,length(xseqgreysep),by=2)){
    polygon(x=c(xseqgreysep[k],xseqgreysep[k+1],xseqgreysep[k+1],xseqgreysep[k]),
            y=c(-10,-10,15,15), col=lighten(background_colour,0.99), border=NA)
  }
  box()
}
par(new=TRUE)
plot(or_pahs_cat_l, ylim=myrange,
     xaxt="n", yaxt="n",
     col=c(NA, mycolours[2], NA, mycolours[4],NA, mycolours[6], NA, mycolours[8],NA, mycolours[10],NA),
     pch = c(NA, 15, NA, 16,NA, 17, NA, 18,NA, 25,NA),
     lwd=1, ylab="", xlab="")
x_pahs <- 1:209
arrows(x0=x_pahs, y0=up_pahs_cat_l, x1=x_pahs, y1=lo_pahs_cat_l, code=3, angle=90, length=0.05, 
       col=c(NA, mycolours[2], NA, mycolours[4],NA, mycolours[6], NA, mycolours[8],NA, mycolours[10],NA))
axis(side=4, at=axTicks(2))
mtext(side=4, text="Odds Ratio", line=2)

abline(h=1, lty = 2, col="gray48")
abline(v=xseqgreysep,lty=3,col="black",lwd=0.5)

legend("topright", pch=c(15, 16, 17, 18, 25), 
       col=c(mycolours[c(2,4,6,8,10)]),
       legend=c(outcomes), ncol = 2,
       cex=0.7, bg = "white", bty = "n")
# Belgium
myrange <- c(0,80)
par(mar=c(0, 0, 0.5, 0),xpd=FALSE)
plot(or_pahs_cat_b, ylim=myrange, xlim=c(1,length(or_pahs_cat_b)), type="n",
     xaxt="n", yaxt="n",
     col=c(mycolours[2], NA, mycolours[4],NA, mycolours[6], NA, mycolours[8],NA, mycolours[10],NA),
     lwd=1, ylab="",xlab="")
xseqgreysep=c(min(xseq)-myspacing/2,apply(rbind(xseq[-1],xseq[-length(xseq)]),2,mean),max(xseq)+myspacing/2)

if (background){
  for (k in seq(1,length(xseqgreysep),by=2)){
    polygon(x=c(xseqgreysep[k],xseqgreysep[k+1],xseqgreysep[k+1],xseqgreysep[k]),
            y=c(-10,-10,15,15), col=lighten(background_colour,0.95), border=NA)
  }
  for (k in seq(2,length(xseqgreysep),by=2)){
    polygon(x=c(xseqgreysep[k],xseqgreysep[k+1],xseqgreysep[k+1],xseqgreysep[k]),
            y=c(-10,-10,15,15), col=lighten(background_colour,0.99), border=NA)
  }
  box()
}
par(new=TRUE)
plot(or_pahs_cat_b, ylim=myrange,
     xaxt="n", yaxt="n",
     col=c(NA, mycolours[2], NA, mycolours[4],NA, mycolours[6], NA, mycolours[8],NA, mycolours[10],NA),
     pch = c(NA, 15, NA, 16,NA, 17, NA, 18,NA, 25,NA),
     lwd=1, ylab="", xlab="")
x_pahs <- 1:209
arrows(x0=x_pahs, y0=up_pahs_cat_b, x1=x_pahs, y1=lo_pahs_cat_b, code=3, angle=90, length=0.05, 
       col=c(NA, mycolours[2], NA, mycolours[4],NA, mycolours[6], NA, mycolours[8],NA, mycolours[10],NA))
axis(side=4, at=axTicks(2))
mtext(side=4, text="Odds Ratio", line=2)

abline(h=1, lty = 2, col="gray48")
abline(v=xseqgreysep,lty=3,col="black",lwd=0.5)

for (k in 1:length(xseq)){
  mytext=labels_pahs[k]
  if (grepl("m\\^", mytext)){
    mytext=gsub("m\\^","'~m^", mytext)
    mytext=sub(")","~')", mytext)
  }
  if (grepl("\\[", mytext)){
    mytext=gsub("\\[","'[", mytext)
    mytext=sub("\\(ug","~'(ug", mytext)
  }
  mytmp=eval(parse(text=paste0("expression(","'", mytext,"'",")")))
  labcol=ifelse(k<=8, "black", ifelse(k>8 & k<=10, "goldenrod", "lightpink3"))
  axis(side=1, at=xseq[k], labels=mytmp, las=2, cex.axis=0.7, col = labcol, col.axis=labcol)
}
dev.off()

