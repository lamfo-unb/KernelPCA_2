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

arquivos <- list.files("data","csv",full.names = T)
i <- arquivos[2]
  base_par <- tolower(gsub(".*data.(.).*csv","\\1",i))

  dados <- data.table(read_csv2(i,
                                 skip = 1))
  nomes_colunas <- names(read_csv2(i,n_max = 2))
  
  colnames(dados) <- nomes_colunas
  nomes_colunas_transform <- setdiff(nomes_colunas,"X1")
  
  setDT(dados)[, (nomes_colunas_transform):= lapply(.SD,  function(x) as.numeric(gsub(",",".",x))),
                .SDcols=nomes_colunas_transform]
  

  
  ## counting NA
  dados_faltantes <- t(dados[, lapply(.SD, function(x) sum(is.na(x)))])
  dados_faltantes <- cbind(data.table(dados_faltantes),
                           rownames(dados_faltantes)) %>% data.table()
  dados_faltantes <- dados_faltantes[order(V1)]
  saveRDS(dados,paste0("data/",base_par,"_daily.rds"))
  j <- 200
  ### Top 200, 500 , 700 e 800 ativos
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
    
    
    
  ## Respeitando regra 
  select_vars <- names(b)[which(b<=165)]
  set.seed(1635190218)
  select_vars <- union("X1",sample(select_vars,200,replace = F))
  base_temp <- data.table(base_temp %>% select(select_vars))
  
  base_temp[, NA_freq := Reduce(`+`, lapply(.SD,function(x) is.na(x)))]
  base_temp <- base_temp %>% filter(NA_freq==0)
  base_temp <- data.table(base_temp)
  base_temp <- base_temp[,`:=`(year = gsub("\\d{2}.\\d{2}.(\\d{4})","\\1",X1),
                     month = gsub("\\d{2}.(\\d{2}).\\d{4}","\\1",X1),
                     day = gsub("(\\d{2}).\\d{2}.\\d{4}","\\1",X1))]
  
  base_temp <- base_temp[,last_day := max(day), by = .(year,month)]
  # base_temp <- base_temp[(day == last_day),]
  base_temp <- base_temp[,DT:=as.Date(paste0(day,"/",month,"/",year),format ="%d/%m/%Y")]
  base_temp <- base_temp[order(DT)]
  
  
  var_ativos <- select_vars[-1]
  
  # Calculando retorno dos ativos
  base_temp <- base_temp[, c(var_ativos):=lapply(.SD, function(x) (x/shift(x, 1)-1)*100),.SDcols =var_ativos ]
  
  base_temp <- base_temp %>% select(c("DT",var_ativos))
  
  
  ## ref: J. Bouchaud, M. Potters, Theory of Financial Risks—From Statistical Physics to Risk Management, Cambridge University Press, UK, 2000.
  Q <- nrow(base_temp)/(ncol(base_temp) - 1)
  lamda_max <- (1 + 1/Q + (1/Q)^.5)
  
  
  base_temp <- data.table(gather(base_temp,"var","value",-DT)%>%filter(!is.na(value)))
  base_temp[,m:=mean(value,na.rm=T),by = "var"]
  base_temp[,v:=sd(value,na.rm=T),by = "var"]
  base_temp[,value:=(value-m)/v]
  
  base_temp <- spread(base_temp %>% select(-m,-v),"var","value")
  
  
  cor0 <- cor(base_temp[,-1])
  eigen0 <- eigen(cor0)
  
  pdf(file.path("eigen.pdf"),height=8,width = 12)
  
  
  plot(eigen0$values,type="l",xlim = c(0,25),col=1,lwd = 2,xlab = "Eigenvalue Index",ylab="Eigenvalue")
  abline(h=lamda_max,col=2,lty=3)
  
  g1 <- eigen0$values[eigen0$values>=lamda_max]
  g2 <- eigen0$values[eigen0$values<lamda_max]
  eigen1<- diag(c(g1,rep(mean(g2),length(g2))))
  cor1 <- eigen0$vectors %*% eigen1 %*% t(eigen0$vectors)
  points(c(g1,rep(mean(g2),length(g2))),col=4,type="l",lwd = 2)
  
  
  
  dev.off()
  
