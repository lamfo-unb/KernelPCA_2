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
options(scipen=999)

j <- "n"
i <- 26
for(j in c("s","n")){
  data_full <- NULL
  arquivos <- list.files("data/intraday/",paste0(j,".*_.*rds"),full.names =T)
  for(i in 1:length(arquivos)){
    dados_temp <- readRDS( arquivos[i])
    dados_temp <- dados_temp %>%
      mutate(data_hora = as.POSIXct(data - minute(as.POSIXct(data, origin = "1970-01-01", tz = "America/Sao_Paulo"))*60
                                    , origin = "1970-01-01", tz = "America/Sao_Paulo") %>% as.numeric()) %>%  data.table()
    dados_temp <- dados_temp[,data_max := max(data),by = data_hora] %>% 
      filter(data_max==data) %>% 
      select(-data_max)
      
    data_full <- rbind(data_full,dados_temp)
    print(paste0("Fim ",i))
  }
  print(paste0("Fim ",j))
  saveRDS(data_full,paste0("data/intraday/",j,"_INTRADAY.rds"))
}

length(unique(data_full$var))
