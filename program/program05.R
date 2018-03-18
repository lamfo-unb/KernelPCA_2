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


options(scipen=999)

arquivos <- list.files("data/intraday/","INTRADAY",full.names =T)
vec_parms <- NULL
for(i in arquivos){
  base_par <- gsub(".*(.)_INTRADAY.*","\\1",i)
  
  dados <- unique(readRDS(i) %>% data.table())
  dados <- data.table(dados, key = c("data_hora","var"))
  
  dados[,N:=length(data_hora),by=key(dados)]
  dados <- dados %>% select(-base,-data,-N)   %>%
    spread(key = var,value = valor) %>% data.table()
  
  dados_faltantes <- t(dados[, lapply(.SD, function(x) sum(is.na(x)))])
  dados_faltantes <- cbind(data.table(dados_faltantes),
                           rownames(dados_faltantes)) %>% data.table()
  dados_faltantes <- dados_faltantes[order(V1)]
  
  
    ### Top 200, 500 , 700 ativos
  for(j in c(200,250,500,700,800)){
    vars_select  <- dados_faltantes$V2[1:(j+1)]
    
    dados_temp <- dados %>% select(vars_select) %>% 
        gather(key = "var", value = "value",-data_hora) %>% data.table()
    
    
    ### Corrigindo base: ultimo valor negociado
    dados_temp <- dados_temp[order(var,data_hora)]
    dados_temp <- data.table(dados_temp, key = "var")
    dados_temp[, csum := cumsum(!is.na(value)), by = key(dados_temp)]
    dados_temp <- data.table(dados_temp, key = c("var","csum"))
    dados_temp[, value_lag := mean(value,na.rm = T), by = key(dados_temp)]
    dados_temp <- dados_temp %>% mutate(value = value_lag) %>% select(-value_lag,-csum)
    
    ### Corrigindo base: primeir valor negociado
    dados_temp <- dados_temp[order(var,rev(data_hora))]
    dados_temp <- data.table(dados_temp, key = "var")
    dados_temp[, csum := cumsum(!is.na(value)), by = key(dados_temp)]
    dados_temp <- data.table(dados_temp, key = c("var","csum"))
    dados_temp[, value_lag := mean(value,na.rm = T), by = key(dados_temp)]
    dados_temp <- dados_temp %>% mutate(value = ifelse(is.na(value),value_lag,value)) %>% select(-value_lag,-csum)
    dados_temp <- data.table(dados_temp, key = "var")
    dados_temp[,m:=mean(value,na.rm=T),by = "var"]
    dados_temp[,v:=sd(value,na.rm=T),by = "var"]
    dados_temp[,value_z:=(value-m)/v]
    
    
    
    dados_temp <- dados_temp[order(value_z,data_hora)] %>%
      mutate(data_hora_formato = as.POSIXct(data_hora, origin = "1970-01-01", tz = "America/Sao_Paulo"))  %>%
      select(-value,-m,-v) %>%
      spread(key = var,value = value_z)
      
    
      ## Pearson ----
      mpearson <- var(dados_temp %>% select(-data_hora,-data_hora_formato))
      eigen0 <- eigen(mpearson)
      
      ## ref: J. Bouchaud, M. Potters, Theory of Financial Risks—From Statistical Physics to Risk Management, Cambridge University Press, UK, 2000.
      Q <- nrow(dados_temp)/(ncol(dados_temp) - 2)
      lamda_max <- (1 + 1/Q + (1/Q)^.5)
      
      g1 <- eigen0$values[eigen0$values>=lamda_max]
      g2 <- eigen0$values[eigen0$values<lamda_max]
      eigen1<- diag(c(g1,rep(mean(g2),length(g2))))
      cor1 <- eigen0$vectors %*% eigen1 %*% t(eigen0$vectors)
      
      
      vec_parms_temp <- data.frame(data = base_par,
                              cov_method = "Pearson",
                              time_freq = "hour",
                              initial = min(dados_temp$data_hora_formato),
                              end = max(dados_temp$data_hora_formato),
                              N = nrow(dados_temp),X = ncol(dados_temp) - 2,
                              Q = Q, lamda_max = lamda_max,
                              eigen_up = paste0(length(g1)," (",round(length(g1)/(ncol(dados_temp) - 2)*100,2),"%)"),
                              eigen_v_up = paste0(round(sum(g1)/(sum(g1)+sum(g2))*100,2),"%"),
                              eigen_max = g1[1]/lamda_max,
                              eigen_v_max = paste0(round(sum(g1[1])/(sum(g1)+sum(g2))*100,2),"%"),
                              top5tradable = paste0(gsub("(.*)\\s.*\\s.*","\\1",dados_faltantes$V2[(1:5)+1]),collapse = ","))
      
      vec_parms <- rbind(vec_parms,vec_parms_temp)
      
      ## RMT ----
      thetas <- c(.25,.75)
      rbf1 <- rbfdot(sigma = thetas[1])
      rbf2 <- rbfdot(sigma = thetas[2])
      
      K11 <- kernelMatrix(rbf1,as.matrix(dados_temp %>% select(-data_hora,-data_hora_formato)))
      
      eigen0 <- eigen(K11)
      
      ## ref: J. Bouchaud, M. Potters, Theory of Financial Risks—From Statistical Physics to Risk Management, Cambridge University Press, UK, 2000.
      Q <- nrow(dados_temp)/(ncol(dados_temp) - 2)
      lamda_max <- (1 + 1/Q + (1/Q)^.5)
      
      g1 <- eigen0$values[eigen0$values>=lamda_max]
      g2 <- eigen0$values[eigen0$values<lamda_max]
      eigen1<- diag(c(g1,rep(mean(g2),length(g2))))
      cor1 <- eigen0$vectors %*% eigen1 %*% t(eigen0$vectors)
      
      
      vec_parms_temp <- data.frame(data = base_par,
                                   cov_method = "Kernel",
                                   time_freq = "hour",
                                   initial = min(dados_temp$data_hora_formato),
                                   end = max(dados_temp$data_hora_formato),
                                   N = nrow(dados_temp),X = ncol(dados_temp) - 2,
                                   Q = Q, lamda_max = lamda_max,
                                   eigen_up = paste0(length(g1)," (",round(length(g1)/(ncol(dados_temp) - 2)*100,2),"%)"),
                                   eigen_v_up = paste0(round(sum(g1)/(sum(g1)+sum(g2))*100,2),"%"),
                                   eigen_max = g1[1]/lamda_max,
                                   eigen_v_max = paste0(round(sum(g1[1])/(sum(g1)+sum(g2))*100,2),"%"),
                                   top5tradable = paste0(gsub("(.*)\\s.*\\s.*","\\1",dados_faltantes$V2[(1:5)+1]),collapse = ","))
      
      vec_parms <- rbind(vec_parms,vec_parms_temp)
      
      
    print(paste0("Fim ",j))
  }
  print(paste0("Fim ",i))
}

print(xtable(vec_parms %>% select(-initial,-end,-top5tradable),
             digits = c(0,0,1,1,0,0,2,2,1,1,2,2),caption = "sasa"),include.rownames = F)
saveRDS(vec_parms,"program/results/INTRADAY_analysis.rds")





