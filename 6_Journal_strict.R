
rm(list=ls())
library(data.table)
library(dplyr)
#load('C:/Users/WenluluSens/Downloads/jm.rda')
load('C:/Users/WenluluSens/Downloads/pdata.rda')

pdata <- lapply(pdata,as.data.table)

x.base <- pdata$base
x.idx <- pdata$idx
x.exe <- pdata$exe
x.med <- pdata$med
p <- unique(x.base$patient_id)

#################
# RATING
#################

#JIGAN RATE
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
#XUETOU RATE
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
#HUXIJI RATE
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
#RATE FILING
x.rate <- rbind(x.jg,x.xt,x.hxj)
# filter(x.exe,patient_id=='BCF0F9A1D1BEDAD9E5A2AC094A40AB33')
# filter(x.xt,patient_id=='BCF0F9A1D1BEDAD9E5A2AC094A40AB33') 
# filter(x.exe,patient_id=='C879DF3DF8E41B1E809ACD6084D78AA2'&item_name=='血透')
# filter(x.xt,patient_id=='C879DF3DF8E41B1E809ACD6084D78AA2')

#################
# JMI2
#################

i <- 'C879DF3DF8E41B1E809ACD6084D78AA2'

j <- 0
system.time(
  jm <- lapply(p[1:300],function(i){
    print(j <<- j+1)
    i.info <- i.base <- filter(x.base,patient_id==i)
    i.idx <- filter(x.idx,patient_id==i)
    i.exe <- filter(x.exe,patient_id==i)
    i.base <- data.frame(date=c(0,as.numeric(as.POSIXct(i.base$discharge_date_time) - as.POSIXct(i.base$admission_date_time))),
                         status = c('admission','discharge'),
                         note = c(NA,as.numeric(as.POSIXct(i.base$discharge_date_time) - as.POSIXct(i.base$admission_date_time))))
    i.idx <- select(i.idx,date=interval_days,status=item,note=value)
    i.exe1 <- select(i.exe,date=interval_days1,status=item_name,note=hours) %>% mutate(status=paste('start',status))
    i.exe2 <- select(i.exe,date=interval_days2,status=item_name,note=hours) %>% mutate(status=paste('end',status))
    i.rate <- (filter(x.rate,patient_id==i))
    i.rate2 <- (i.exe %>% filter(item_name=='ICU') %>% 
                  select(-item_name) %>% 
                  mutate(rate=1, item='ICU'))
    i.rate <- rbind(as.data.frame(i.rate),as.data.frame(i.rate2)) %>% as.data.table() %>% select(-patient_id)
    i.con <- data.table(interval_days1=0,interval_days2=i.info$interval_days,
                        hours=i.info$interval_days*24,rate=i.info$com_or_not,
                        item='com_or_not')
    i.diag <-   data.table(interval_days1=0,interval_days2=i.info$interval_days,hours=i.info$interval_days*24,
                           rate=(i.info$diag_no==14)*2+(i.info$diag_no%in%c(12,9,15))*1,
                           item=paste(i.info$diag_no,i.info$diag))
    i.rate <- mutate(i.rate,interval_days1=interval_days1/i.info$interval_days,interval_days2=interval_days2/i.info$interval_days)
    i.rate <- rbind(i.diag,i.con,i.rate)
    i.info <- data.table(i.info,overall_rate=max(i.rate$rate))
    i <- rbind(i.base,i.idx,i.exe1,i.exe2) %>% arrange(date,status) %>% mutate(date=date/max(date))
    list(info=i.info,journal=i,i.rate)
  })
)
# save(jm,file='jm1220.rda')
