
rm(list=ls())
library(data.table)
library(dplyr)
library(reshape2)

load('C:/Users/WenluluSens/Downloads/jm1220.rda')
load('C:/Users/WenluluSens/Downloads/pdata.rda')
jm.sum <- sapply(jm,function(x){x[[1]]})
table(unlist(jm.sum[9,]))/ncol(jm.sum)
idx <- unique(pdata$idx$item)
x <- (1:100)/100

#######################################

jmi <- jm[[127]]

i.idx <- jmi$journal %>% mutate(x=round(date,2)) %>% filter(status%in%idx)
