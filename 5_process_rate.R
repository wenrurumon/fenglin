

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
  interval_days1=min(interval_days),
  interval_days2=max(interval_days)
) %>% select(-check) %>% mutate(
  hours = (interval_days2-interval_days1)*24,
  rate = (hours>=48)+1,
  item = '肌酐'
)

#############
x.xt <- filter(x.exe,item_name=='血透') %>% mutate(
  lagid=lag(patient_id,1), 
  interval_days3=lag(interval_days2,1),
  nextinterval=interval_days1-interval_days3,
  checkid = (patient_id!=lagid),
  checkint = (nextinterval>1)
)
x.xt$checkid[[1]] <- FALSE
x.xt$checkint[[1]] <- FALSE
x.xt <- x.xt %>% mutate(check=checkid|checkint)
x.xt$check <- cumsum(x.xt$check)
x.xt <- x.xt %>% group_by(patient_id,check) %>% summarise(
  interval_days1=min(interval_days1), interval_days2=max(interval_days2)
) %>% mutate(hours=(interval_days2-interval_days1)*24,
             rate=(hours>=48)+1, item='血透') %>% select(-check)

# filter(x.exe,patient_id=='BCF0F9A1D1BEDAD9E5A2AC094A40AB33')
# filter(x.xt,patient_id=='BCF0F9A1D1BEDAD9E5A2AC094A40AB33') 
# filter(x.exe,patient_id=='C879DF3DF8E41B1E809ACD6084D78AA2'&item_name=='血透')
# filter(x.xt,patient_id=='C879DF3DF8E41B1E809ACD6084D78AA2')

#######
x.hxj <- filter(x.exe,item_name=='呼吸机') %>% mutate(
  lagid=lag(patient_id,1), 
  interval_days3=lag(interval_days2,1),
  nextinterval=interval_days1-interval_days3,
  checkid = (patient_id!=lagid),
  checkint = (nextinterval>1)
)
x.hxj$checkid[[1]] <- FALSE
x.hxj$checkint[[1]] <- FALSE
x.hxj <- x.hxj %>% mutate(check=checkid|checkint)
x.hxj$check <- cumsum(x.hxj$check)
x.hxj <- x.hxj %>% group_by(patient_id,check) %>% summarise(
  interval_days1=min(interval_days1), interval_days2=max(interval_days2)
) %>% mutate(hours=(interval_days2-interval_days1)*24,
             rate=(hours>=48)+1, item='呼吸机') %>% select(-check)

########
x.rate <- rbind(x.jg,x.xt,x.hxj)

