

library(data.table)
library(dplyr)

rm(list=ls())
# setwd('/Users/wenrurumon/Documents/fenglin')
# files <- dir(pattern='csv')
# pdata <- lapply(files,fread,encoding='UTF-8')
# names(pdata) <- files
setwd('E:/fenglin/')
load("E:/fenglin/pdata.rda")
pdata <- lapply(pdata,as.data.table)

x.base <- pdata$base
x.idx <- pdata$idx
x.exe <- pdata$exe
x.med <- pdata$med
p <- unique(x.base$patient_id)

x.e <- filter(x.exe,item_name%in%c('血透','呼吸机'))
x.e <- rbind(rbind(
  select(x.e,patient_id,interval_days=interval_days1,item=item_name),
  select(x.e,patient_id,interval_days=interval_days2,item=item_name)) %>% mutate(
    value = 170
  ),filter(x.idx,item=='肌酐')) %>% arrange(patient_id,item,interval_days)
x.e1 <- data.table(idx=1:nrow(x.e),x.e)
x.e2 <- data.table(idx=(1:nrow(x.e))-1,x.e)
x.e2 <- merge(x.e1,x.e2,by='idx')

x.e_se <- filter(x.e2,!((patient_id.x==patient_id.y)&(item.x==item.y))) %>% as.data.frame()
x.e_s <- data.table(x.e_se[,1:5],x.e_se[,2:5])
x.e_e <- data.table(x.e_se[,c(1,6:9)],x.e_se[,6:9])
x.e_m <- filter(x.e2,((patient_id.x==patient_id.y)&(item.x==item.y)))
colnames(x.e_s) <- colnames(x.e_e) <- colnames(x.e_m)
x.e <- rbind(x.e_s,x.e_m,x.e_e) %>% select(
  idx,patient_id=patient_id.x,item=item.x,interval_days.x,interval_days.y,value.x,value.y
) %>% mutate(
  status.x=(value.x>=170)+0,status.y=(value.y>=170)+0,
  days=(interval_days.y-interval_days.x)) %>% 
  arrange(idx) %>% filter(days>0)

x.jgr <- filter(x.e,item=='肌酐'&status.x+status.y==2) %>% mutate(rate = (days>=2)+1)
x.er <- filter(x.e,item!='肌酐'&status.x+status.y==2) %>% mutate()
