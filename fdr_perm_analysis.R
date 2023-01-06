# R2GUESS
rm(list=ls())
project_path="/rds/general/user/rw1317/home/Hair_pollutant_CVD/Scripts"
setwd(project_path)
  
library(R2GUESS)
library(tidyverse)

filepath = "PesticidesPCBBDEs"
trait = readRDS(paste0("../Data/",filepath,"_trait.rds"))
Y = trait %>% dplyr::select(BMI, WC, TG, TC, LDLC, HDLC, SBP, DBP, FPG)
trait_group = c(colnames(Y),"Core","Plus_FPG","All_traits")

f = 3

## Single trait: Combined ----
for (m in 1:9){
  model = readRDS(paste0("../Results/GUESS-",trait_group[m],"/model",f,"_ESS.rds"))
  
  namePermut = "Permut-model3"
  npermut = 50
  perm.input = sprintf("%s/perm_input/", model$path.output)
  perm.output = sprintf("%s/perm_output/", model$path.output)
  
  # FDRthresh = Analysis.permutation(x = model, threshold = 0.05,
  #                                  namePermut=namePermut,
  #                                  Npermut = npermut, number.cutoff = model$p,
  #                                  path.output = perm.output)
  
  threshold = 0.05
  Npermut = npermut
  number.cutoff = model$p
  path.output = perm.output
  
  NameMarg <- file.path(model$path.output, paste(model$root.file.output,"output_marg_prob_incl.txt",sep="_"))
  Marg <- read.table(NameMarg,header=TRUE)
  MPI <- Marg[,2]
  MPI.sort <- sort(MPI,decreasing=TRUE)
  
  JF.best <- NULL
  MPI.perm <- NULL
  
  vecPermut <- 1:Npermut
  Npermut <- length(vecPermut)
  
  for (m in 1:9){
    x = readRDS(paste0("../Results/GUESS-",trait_group[m],"/model",f,"_ESS.rds"))
    for (i in vecPermut){
      print(i)
      root.file.output <- paste(namePermut,i,sep="")
      res.model.perm  <- as.ESS.object(dataY=x$dataY,dataX=x$dataX,path.input=x$path.input,path.output=path.output,
                                       root.file.output=root.file.output,label.X=x$label.X,
                                       label.Y=x$label.Y,path.par=x$path.par,path.init=x$path.init,file.par=x$file.par,file.init=x$file.init,file.log=x$file.log,MAP.file=x$MAP.file,command=FALSE)
      
      NameMarg.perm <- file.path(res.model.perm$path.output, paste(res.model.perm$root.file.output,"output_marg_prob_incl.txt",sep="_"))
      Marg.perm <- read.table(NameMarg.perm,header=TRUE)
      MPI.perm <- c(MPI.perm,Marg.perm[,2])
      JF.best <- c(JF.best,max(res.model.perm$BestModels$jeffries)) 
    }
  }
  
  MPI.perm.sort <- sort(MPI.perm,decreasing=TRUE)
  
  flag_emp  <- 0
  cutoff_St <- NULL
  FDR_emp_St <- NULL
  s_pp <- MPI.sort
  n_pp <- length(s_pp)
  s_pp_S <- MPI.perm.sort 
  
  for (s in 1:number.cutoff) {
    n_s_pp <- sum(s_pp >= s_pp[s])
    n_s_pp_S <- sum(s_pp_S >= s_pp[s]) / Npermut
    # print(c(s, s_pp[s], n_s_pp_S / n_s_pp))
    
    if (s == 1) {
      cutoff_St <- s_pp[s]
      FDR_emp_St <- n_s_pp_S / n_s_pp
    }
    
    if (s > 1) {
      cutoff_St <- c(cutoff_St, s_pp[s])
      FDR_emp_St <- c(FDR_emp_St, n_s_pp_S / n_s_pp)
      
      if (((n_s_pp_S / n_s_pp) > threshold) & flag_emp == 0) {
        n_s_pp <- sum(s_pp >= s_pp[(s - 1)])
        n_s_pp_S <-  sum(s_pp_S >= s_pp[(s - 1)]) / Npermut
        
        cutoff <- s_pp[s - 1]
        FDR_emp <- n_s_pp_S / n_s_pp
        
        flag_emp <- 1   # ***
      }
      
    }
    
  }
  
  int <- approx(FDR_emp_St, cutoff_St, n = 10000)
  int_idx <- max(which(int$x < threshold))
  FDR_emp_int <- int$x[int_idx]
  cutoff_int <- int$y[int_idx]
  
  FDRthresh = list(cutoff.MPI = cutoff, cutoff_int = cutoff_int, cutoff_St = cutoff_St, FDR_emp = FDR_emp, FDR_emp_int = FDR_emp_int, FDR_emp_St = FDR_emp_St)
  saveRDS(FDRthresh,paste0(model$path.output,"/model",f,"_fdr_perm450.rds"))
}

for (m in 1:9){
  fdr.perm = readRDS(paste0("../Results/GUESS-",trait_group[m],"/model",f,"_fdr_perm450.rds"))
  print(fdr.perm$cutoff_int)
  print(fdr.perm$FDR_emp_int)
}


