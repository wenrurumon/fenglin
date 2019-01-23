rm(list=ls())
library(data.table)
library(dplyr)
# library(openxlsx)
# library(lubridate)
# library(fda)
# library(flare)
# library(corpcor)

############################
# 数据导入
############################

setwd('e:\\fenglin\\data')
files <- dir(pattern='csv')
jyzb  <- do.call(rbind,lapply(c('jianyanzhibiao1.csv','jianyanzhibiao2.csv'),fread))
jxk <- fread('jixianku.csv',encoding='UTF-8')
yizhu <- do.call(rbind,lapply(c('yizhu1.csv','yizhu2.csv'),fread,encoding='UTF-8'))
yaowu <- do.call(rbind,lapply(c('xueguanhuoxingyaowu.csv','xueguanhuoxingyaowufeiyong.csv'),fread,encoding='UTF-8'))
# others <- lapply(files[6],fread,encoding='UTF-8')

#检验指标
idx <- jyzb %>% select(pid=patient_id,
                       hours=interval_hours,
                       item=report_name,
                       value=result_final)
# idx <- idx %>% group_by(pid,hours,item) %>% summarise(value=mean(value))
#基线库
#如果diagno为14则重，12/9/15则中，有并发症则中，取最重。
base <- select(jxk,pid=patient_id,com_or_not,admission_date_time,
               discharge_date_time,diag,diag_no) %>% mutate(
                 brate = (diag_no==14)*2+diag_no%in%c(12,9,15)*1
               )
base <- filter(base,grepl(' ',base$admission_date_time)&grepl(' ',base$discharge_date_time))
base$hours <- 24*as.numeric(as.POSIXct(base$discharge_date_time)-as.POSIXct(base$admission_date_time))
base <- base %>% mutate(brate = ifelse(brate>com_or_not,brate,com_or_not))
#医嘱
exe <- yizhu %>% select(
  pid=PATIENT_ID,hour1=ST, hour2=SP,item=ITEM_NAME
) %>% mutate(hours=hour2-hour1)
#血管活性药物
med <- yaowu %>% select(pid=PATIENT_ID,hour1=INTERVAL_HOURS
) %>% mutate(
  hour2=hour1,item='药物',hours = 0
)
exe <- rbind(exe,med)

############################
# Temp Rating
############################

#肌酐
#两次连续的肌酐超过170时间如果超过48小时，则这个区间内为重症，否则为中
jg <- filter(idx,item=='肌酐'&pid%in%filter(idx,item=='肌酐'&value>=170)$pid
) %>% arrange(pid,hours) %>% mutate(
  lagid = lag(pid,default='0'), 
  rate = value >= 170,
  lagrate = lag(rate,default=0),
  newid = (lagid!=pid),
  newrate = (rate!=lagrate),
  check = newid|newrate
)
jg$check <- cumsum(jg$check)
jg <- jg %>% group_by(pid,check,item) %>% summarise(
  hour1=min(hours),hour2=max(hours),rate=mean(rate)) %>% mutate(
    hours = hour2-hour1, rate=rate+(hours>=48)
  ) %>% filter(rate>0) %>% select(pid,item,hour1,hour2,rate)
jg <- select(as.data.frame(jg),-check) %>% as.data.table()
#血透
#持续血透时间超过48小时，则为重，否则为中。两次血透之间间隔24小时内认为是持续血透
xt <- unique(filter(exe,item=='血透')) %>% arrange(pid,hour1,hour2) %>% mutate(
  lagh2 = lag(hour2,default=0),
  lagid = lag(pid,default='0'),
  newid = (lagid!=pid),
  newjn = (hour1-lagh2)>24,
  check = newid|newjn
)
xt$check <- cumsum(xt$check)
xt <- xt %>% group_by(pid,item,check) %>% summarise(
  hour1=min(hour1),hour2=max(hour2)) %>% mutate(
    rate = (hour2-hour1>=48)+1
  ) %>% select(-check)
#呼吸机
#同血透
hxj <- unique(filter(exe,item=='呼吸机')) %>% arrange(pid,hour1,hour2) %>% mutate(
  lagh2 = lag(hour2,default=0),
  lagid = lag(pid,default='0'),
  newid = (lagid!=pid),
  newjn = (hour1-lagh2)>24,
  check = newid|newjn
)
hxj$check <- cumsum(hxj$check)
hxj <- hxj %>% group_by(pid,item,check) %>% summarise(
  hour1=min(hour1),hour2=max(hour2)) %>% mutate(
    rate = (hour2-hour1>=48)+1
  ) %>% select(-check)
#ICU
icu <- unique(filter(exe,item=='ICU') %>% select(pid,item,hour1,hour2)) %>% mutate(
  lagh2 = lag(hour2,default=0),
  lagid = lag(pid,default='0'),
  newid = (lagid!=pid),
  newjn = (hour1-lagh2)>24,
  check = newid|newjn
)
icu$check <- cumsum(icu$check)
icu <- icu %>% group_by(pid,item,check) %>% summarise(
  hour1=min(hour1),hour2=max(hour2)) %>% mutate(
    rate = (hour2-hour1>=48)+1
  ) %>% select(-check)
#血管活性药物
#如果有发生则当天为中
med <- select(med,pid,item,hour1,hour2) %>% mutate(rate=1)

#合成数据
rate <- rbind(as.data.table(jg),as.data.table(xt),as.data.table(hxj),as.data.table(med),as.data.table(icu))
rate2 <- rate %>% group_by(pid) %>% summarise(trate=max(rate))
base2 <- sqldf::sqldf('select a.*, b.trate from base a left join rate2 b on a.pid = b.pid')
base2$diag <- base$diag
base2$trate[is.na(base2$trate)] <- 0
base2$orate <- ifelse(base2$trate>base2$brate,base2$trate,base2$brate)
base <- base2 %>% select(-brate,-trate)

############################
# SPLINE IMPUTATION
############################

fexe <- rbind(filter(rate,item!='肌酐') %>% select(pid,hour1,hour2,item),
              filter(exe,item%in%c('ICU','血气','药物')) %>% select(pid,hour1,hour2,item))
tag.idx <- unique(idx$item)
tag.exe <- unique(fexe$item)

############

pidi <- unique(base$pid)[[10901]]
k <- 0
options(warn = 1)
system.time(jms <- lapply(unique(base$pid),jm))
jms <- do.call(rbind,jms);dim(jms)
save(jms,file='jms_20190119.rda')


