

library(data.table)
library(dplyr)

rm(list=ls())
setwd('E:/fenglin/')
load("E:/fenglin/pdata.rda")
pdata <- lapply(pdata,as.data.table)

x.base <- pdata$base
x.idx <- pdata$idx
x.exe <- pdata$exe
x.med <- pdata$med
p <- unique(x.base$patient_id)

############
x.jg <- filter(x.idx,item=='肌酐') %>% mutate(rate=(value>=170))
p.jg <- unique(filter(x.jg,rate>0)$patient_id)
x.jg <- filter(x.jg, patient_id%in%p.jg) %>% mutate(
  lagid=lag(patient_id,1),lagrate=lag(rate,1),
  check=(lagid!=patient_id)|(rate!=lagrate)
  )
x.jg$check[[1]] <- TRUE
x.jg$check <- cumsum(x.jg$check)
x.jg <- x.jg %>% group_by(patient_id,check) %>% summarise(
  min=min(interval_days),max=max(interval_days)
)

x.xt <- filter(x.exe,item_name=='血透')
