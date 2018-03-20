rm(list = ls())
gc()

library(data.table)
library(dplyr)
library(bit64)
library(stringr)
library(readxl)
library(reshape2)
library(knitr)
library(ggplot2)
library(plyr)
library(gplots)
library(lubridate)
library(tcltk)
library(googlesheets)
library(gsheet)
library(corrplot)
library(xtable)
library(tidyverse)

library(magrittr)
library(RnavGraphImageData)
library(dplyr)
library(e1071)
library(kernlab)
library(quadprog)


arquivos <- list.files("data","csv",full.names = T)
i <- arquivos[1]

options(scipen=999)

vec_parms <- NULL
for(i in arquivos){
  base_par <- tolower(gsub(".*data.(.).*csv","\\1",i))
  
  if(!file.exists(paste0("data/",base_par,"_daily.rds"))){
    
    dados <- data.table(read_csv2(i,
                                  skip = 1))
    nomes_colunas <- names(read_csv2(i,n_max = 2))
    
    colnames(dados) <- nomes_colunas
    nomes_colunas_transform <- setdiff(nomes_colunas,"X1")
    
    setDT(dados)[, (nomes_colunas_transform):= lapply(.SD,  function(x) as.numeric(gsub(",",".",x))),
                 .SDcols=nomes_colunas_transform]
    saveRDS(dados,paste0("data/",base_par,"_daily.rds"))
  }else{
    dados <- readRDS(paste0("data/",base_par,"_daily.rds"))
  }
  

  
  dados <- dados %>%  mutate(data_hora = as.Date(X1,"%d/%m/%Y")) %>% select(-X1) %>% data.table() 
  dados <- dados[order(data_hora)]
  setcolorder(dados,c("data_hora", setdiff(names(dados), "data_hora")))
  
  a <- apply(dados %>% select(-data_hora),1,function(x) sum(is.na(x)))
  dados <- dados[a!=(ncol(dados)-1),]
  rm(a)

  


  
  ## counting NA
  dados_faltantes <- t(dados[, lapply(.SD, function(x) sum(is.na(x)))])
  dados_faltantes <- cbind(data.table(dados_faltantes),
                           rownames(dados_faltantes)) %>% data.table()
  dados_faltantes <- dados_faltantes[order(V1)] %>% filter(V1==0)
  N <- nrow(dados_faltantes)-1


  dados <- dados %>% select(dados_faltantes$V2[1:(N+1)])  %>%
      gather(key = "var", value = "value",-data_hora) %>% data.table() 
    
    dados <- dados[order(var,data_hora)]
    dados[,r:=log(value)-lag(log(value)),by="var"]
    dados[,m:=mean(r,na.rm=T),by = "var"]
    dados[,v:=sd(r,na.rm=T),by = "var"]
    dados[,value_z:=(r-m)/v]
    dados <- dados[order(value_z,data_hora)] %>%
      select(-value,-r,-m,-v) %>%
      spread(key = var,value = value_z)
    
    # dados <- dados[order(var,data_hora)] %>%
    #   select(-value) %>%
    #   spread(key = var,value = r)
    # 
    ## Removendo a primeira linha 
    dados <- dados[-1,]
    
    
    dados_out_of_sample <- dados %>% filter(year(data_hora)>2016) %>% data.table()
    dados_in_sample <- dados %>% filter(year(data_hora)<=2016) %>% data.table()
    
    
    
    ## Pearson ----
    mpearson <- var(dados_in_sample %>% select(-data_hora))
    
    var_analise <- setdiff(names(dados_in_sample),"data_hora")
    ## media
    mu <- as.matrix(dados_in_sample[,lapply(.SD,function(x) mean(x,na.rm=T)),.SDcols = var_analise])
    
    ### Variância total (pooled)
    S <- var(dplyr::select(dados_in_sample,one_of(var_analise)),na.rm = T)
    d <- rep(0, ncol(S))
    A <- t(rbind(diag(1,ncol(S)),diag(1,ncol(S))))
    # A <- t(rbind(-diag(1,ncol(S)),diag(1,ncol(S))))
    b <- c(rep(-1,ncol(S)),rep(0,ncol(S)))
    # b <- c(rep(1,ncol(S)),rep(0,ncol(S)))
    Aeq <- cbind(matrix(1,nrow = ncol(S),ncol = 1),t(mu))
    mu_meta <- 0.0001
    beq <- c(1,mu_meta) 
    
    Dmat <- S
    Amat <- cbind(Aeq,A)
    bvec <- c(beq,b) 
    dvec <- d
    meq  <- ncol(Aeq)
    
    n <- nrow(Dmat)
    q <- ncol(Amat)
    
    # (n != ncol(Dmat)) 
    # (n != length(dvec)) 
    # (n != nrow(Amat)) 
    # (q != length(bvec)) 
    # ((meq > q) || (meq < 0)) 
    
    wm <- quadprog::solve.QP(Dmat = Dmat, dvec = dvec,
                             Amat = Amat, bvec = bvec,
                             meq = meq)
    # Conferindo valores
    sum(round(wm$solution,10))

    # out_of_sample
    dados_out_of_sample <- dados_out_of_sample %>% select(-data_hora)
    
    A <-  matrix(data.matrix(dados_out_of_sample),
                 ncol = ncol(dados_out_of_sample)) %*%
      matrix(round(wm$solution,10))
    plot(cumsum(A),type = "l",main="return acum out-of-sample")
    abline(h=0,lty = 2 , col= 2)
    
    ## ref: J. Bouchaud, M. Potters, Theory of Financial Risks—From Statistical Physics to Risk Management, Cambridge University Press, UK, 2000.
    Q <- nrow(dados_in_sample)/(ncol(dados_in_sample) - 1)
    ## falta entender sigma!!!
    lamda_max <- (1 + 1/Q + (1/Q)^.5)
    
    
    eigen0 <- eigen(mpearson)
    g1 <- eigen0$values[eigen0$values>=lamda_max]
    g2 <- eigen0$values[eigen0$values<lamda_max]
    eigen1<- diag(c(g1,rep(mean(g2),length(g2))))
    cor1 <- eigen0$vectors %*% eigen1 %*% t(eigen0$vectors)

    
    
    ### Variância total filtrada
    S <- cor1
    d <- rep(0, ncol(S))
    A <- t(rbind(diag(1,ncol(S)),diag(1,ncol(S))))
    # A <- t(rbind(-diag(1,ncol(S)),diag(1,ncol(S))))
    b <- c(rep(-1,ncol(S)),rep(0,ncol(S)))
    # b <- c(rep(1,ncol(S)),rep(0,ncol(S)))
    Aeq <- cbind(matrix(1,nrow = ncol(S),ncol = 1),t(mu))
    mu_meta <- 0.0001
    beq <- c(1,mu_meta) 
    
    Dmat <- S
    Amat <- cbind(Aeq,A)
    bvec <- c(beq,b) 
    dvec <- d
    meq  <- ncol(Aeq)
    
    n <- nrow(Dmat)
    q <- ncol(Amat)
    
    # (n != ncol(Dmat)) 
    # (n != length(dvec)) 
    # (n != nrow(Amat)) 
    # (q != length(bvec)) 
    # ((meq > q) || (meq < 0)) 
    
    wm <- quadprog::solve.QP(Dmat = Dmat, dvec = dvec,
                             Amat = Amat, bvec = bvec,
                             meq = meq)
    # Conferindo valores
    sum(round(wm$solution,10))
    
    # out_of_sample
    dados_out_of_sample <- dados_out_of_sample %>% select(-data_hora)
    
    B <-  matrix(data.matrix(dados_out_of_sample),
                 ncol = ncol(dados_out_of_sample)) %*%
      matrix(round(wm$solution,10))
    points(cumsum(B),type = "l",col="blue")
    
    
    
    
    vec_parms_temp <- data.frame(data = base_par,
                                 cov_method = "Pearson",
                                 time_freq = "daily",
                                 initial = min(dados_temp$data_hora),
                                 end = max(dados_temp$data_hora),
                                 N = nrow(dados_temp),
                                 X = ncol(dados_temp) - 1,
                                 Q = Q, lamda_max = lamda_max,
                                 eigen_up = paste0(length(g1)," (",round(length(g1)/(ncol(dados_temp) - 1)*100,2),"%)"),
                                 eigen_v_up = paste0(round(sum(g1)/(sum(g1)+sum(g2))*100,2),"%"),
                                 eigen_max = g1[1]/lamda_max,
                                 eigen_v_max = paste0(round(sum(g1[1])/(sum(g1)+sum(g2))*100,2),"%"),
                                 top5tradable = paste0(gsub("(.*)\\s.*\\s.*","\\1",dados_faltantes$V2[(1:5)+1]),collapse = ","))
    
    vec_parms <- rbind(vec_parms,vec_parms_temp)
    
    ## RMT ----
    thetas <- c(.25,.75)
    rbf1 <- rbfdot(sigma = thetas[1])
    rbf2 <- rbfdot(sigma = thetas[2])
    
    K11 <- kernelMatrix(rbf1,mpearson)
    eigen0 <- eigen(K11)
    
    ## ref: J. Bouchaud, M. Potters, Theory of Financial Risks—From Statistical Physics to Risk Management, Cambridge University Press, UK, 2000.
    Q <- nrow(dados_temp)/(ncol(dados_temp) - 1)
    lamda_max <- (1 + 1/Q + (1/Q)^.5)
    
    g1 <- eigen0$values[eigen0$values>=lamda_max]
    g2 <- eigen0$values[eigen0$values<lamda_max]
    eigen1<- diag(c(g1,rep(mean(g2),length(g2))))
    cor1 <- eigen0$vectors %*% eigen1 %*% t(eigen0$vectors)
    
    
    vec_parms_temp <- data.frame(data = base_par,
                                 cov_method = "Kernel",
                                 time_freq = "daily",
                                 initial = min(dados_temp$data_hora),
                                 end = max(dados_temp$data_hora),
                                 N = nrow(dados_temp),
                                 X = ncol(dados_temp) - 1,
                                 Q = Q, lamda_max = lamda_max,
                                 eigen_up = paste0(length(g1)," (",round(length(g1)/(ncol(dados_temp) - 1)*100,2),"%)"),
                                 eigen_v_up = paste0(round(sum(g1)/(sum(g1)+sum(g2))*100,2),"%"),
                                 eigen_max = g1[1]/lamda_max,
                                 eigen_v_max = paste0(round(sum(g1[1])/(sum(g1)+sum(g2))*100,2),"%"),
                                 top5tradable = paste0(gsub("(.*)\\s.*\\s.*","\\1",dados_faltantes$V2[(1:5)+1]),collapse = ","))
    
    vec_parms <- rbind(vec_parms,vec_parms_temp)
    
    
  print(paste0("Fim ",i))
}

print(xtable(vec_parms %>% select(-initial,-end,-top5tradable),
             digits = c(0,0,1,1,0,0,2,2,1,1,2,2),caption = "sasa"),include.rownames = F)
saveRDS(vec_parms,"program/results/DAILY_analysis.rds")



## Conferindo quantidade de ativos 
d_n <- readRDS("data/n_daily.rds")
d_s <- readRDS("data/s_daily.rds")
h_n <- readRDS("data/intraday/n_INTRADAY.rds")
h_s <- readRDS("data/intraday/s_INTRADAY.rds")

length(names(d_n))
length(unique(h_n$var))
c_n <- intersect(names(d_n),unique(h_n$var))
length(c_n)

length(names(d_s))
length(unique(h_s$var))
c_s <- intersect(names(d_s),unique(h_s$var))
length(c_s)
