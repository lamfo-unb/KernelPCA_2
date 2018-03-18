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

arquivos <- gsub("(.*).csv","\\1",list.files("data/intraday/","csv"))
base_import <- "s2"

for(base_import in  arquivos[5]){
  
  nasdaq_temp <- data.table(read_csv2(paste0("data/intraday/",base_import,".csv"),col_names = TRUE))
  num_row <- nrow(nasdaq_temp)
  b <- nasdaq_temp[, lapply(.SD, function(x) sum(is.na(x)))]
  nasdaq_temp <- nasdaq_temp %>% select(-one_of(c(names(nasdaq_temp)[which(b %in% c(num_row,num_row-1))])))
  names(nasdaq_temp)[(1:(length(nasdaq_temp)/2))*2-1] <- paste0("data_",names(nasdaq_temp)[(1:(length(nasdaq_temp)/2))*2])
  
  
  saveRDS(nasdaq_temp,paste0("data/intraday/",base_import,".rds"))
  K <- min(100,ncol(nasdaq_temp))
  i_to <- trunc(ncol(nasdaq_temp)/2/K)
  for(i in 0:i_to){
    cols_select <- (i*(K*2)) + 1:(K*2) 
    cols_select <- cols_select[cols_select<=ncol(nasdaq_temp)]
    nasdaq_temp_2 <- nasdaq_temp[,cols_select,with=FALSE] 
    nasdaq_temp_2$id <- 1:num_row
    
    nasdaq_temp_2 <- gather(nasdaq_temp_2,key = "var", value = "value",-id)
    

    
    nasdaq_temp_2 <- nasdaq_temp_2 %>%
      mutate(tipo_var = ifelse(grepl("^data_",var),"data","valor"),
             value = ifelse(tipo_var=="data",as.POSIXct(strptime(value, format="%d/%m/%Y %H:%M")),as.numeric(value)),
             var = gsub("data_(.*)","\\1",var))  %>%
      spread(key = tipo_var,value = value) %>% 
      mutate(base = base_import) %>% filter(!(id==1|is.na(data))) %>% select(-id)
    
    
    saveRDS(nasdaq_temp_2,paste0("data/intraday/",base_import,"_",i,".rds"));rm(nasdaq_temp_2);gc()
  }
  
}

