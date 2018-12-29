
library(data.table)
library(dplyr)

rm(list=ls())
setwd('/Users/wenrurumon/Documents/fenglin')
files <- dir(pattern='csv')
pdata <- lapply(files,fread,encoding='UTF-8')
names(pdata) <- files

x.base <- pdata$base.csv
x.idx <- pdata$idx.csv
x.exe <- pdata$exe.csv
x.med <- pdata$med.csv
p <- unique(x.base$patient_id)

#################
# JMi
#################

# i <- x.base$patient_id[[1]] #轻度
# i <- p[10] #重度
# i <- '003E85EAB3D5BF55F66BAB7D023C0B95'

# j <- 0
# system.time(
#     jm <- lapply(p[1:10],function(i){
#     print(j <<- j+1)
#     i.info <- i.base <- filter(x.base,patient_id==i)
#     i.idx <- filter(x.idx,patient_id==i)
#     i.exe <- filter(x.exe,patient_id==i)
#     i.base <- data.frame(date=c(0,as.numeric(as.POSIXct(i.base$discharge_date_time) - as.POSIXct(i.base$admission_date_time))),
#                          status = c('admission','discharge'),
#                          note = c(NA,as.numeric(as.POSIXct(i.base$discharge_date_time) - as.POSIXct(i.base$admission_date_time))))
#     i.idx <- select(i.idx,date=interval_days,status=item,note=value)
#     i.exe1 <- select(i.exe,date=interval_days1,status=item_name,note=hours) %>% mutate(status=paste('start',status))
#     i.exe2 <- select(i.exe,date=interval_days2,status=item_name,note=hours) %>% mutate(status=paste('end',status))
#     i <- rbind(i.base,i.idx,i.exe1,i.exe2) %>% arrange(date,status) %>% mutate(date=date/max(date))
#     list(info=t(i.info),journal=i)
#   })
# )

#################
# TAG
#################

# 2. 如果是，再逐个状态判断（逐个状态判断的严重程度不会超过总体）
# 2.1 重症急性胰腺炎：该状态时间内使用呼吸机持续时间48小时以上，
#   或者使用血透持续时间48小时以上，
#   或者使用血管活性药物持续时间48小时以上， #药物数据没有时间信息所以无法标记
#   或者血肌酐值>=170 umol/L持续时间48小时以上。


# 2.2 中重症急性胰腺炎：该状态时间内使用呼吸机持续时间48小时以内，
#   或者使用血透持续时间48小时以内，
#   或者使用血管活性药物持续时间48小时以内，#药物数据没有时间信息所以无法标记
#   或者血肌酐值>=170 umol/L持续时间48小时以内，或者曾转入ICU。

# 2.3 轻症急性胰腺炎：该状态时间内排除了以上中重症和重症急性胰腺炎所有条件。

x.exe_rate <- as.data.table(x.exe %>% filter(item_name!='血气') %>% mutate(rate=(hours>=48)+1))
x.jg <- filter(x.idx,item=='肌酐') %>% arrange(patient_id,interval_days) %>% select(-item)
x.jg1 <- data.table(idx=1:nrow(x.jg),x.jg)
x.jg2 <- data.table(idx=(1:nrow(x.jg))-1,x.jg)
x.jg2 <- merge(x.jg1,x.jg2,by='idx')
x.jg_se <- filter(x.jg2,patient_id.x!=patient_id.y)
x.jg_s <- data.table(x.jg_se[,1:4],x.jg_se[,2:4])
x.jg_e <- data.table(x.jg_se[,c(1,5:7)],x.jg_se[,5:7])
x.jg_m <- filter(x.jg2,patient_id.x==patient_id.y)
colnames(x.jg_s) <- colnames(x.jg_e) <- colnames(x.jg_m)
x.jg <- rbind(x.jg_s,x.jg_m,x.jg_e) %>% arrange(idx) %>% select(
  idx,patient_id=patient_id.x,interval_days.x,interval_days.y,value.x,value.y
) %>% mutate(status.x=(value.x>=170)+1,status.y=(value.y>=170)+1,note=(status.x!=status.y)+0,status = (status.x+status.y)/2)
x.jg$idx <- 1:nrow(x.jg); x.jg$note <- cumsum(x.jg$note)
x.jg_rate <- x.jg %>% group_by(patient_id,note,status.x,status.y) %>% summarise(
  interval_days.x = min(interval_days.x),interval_days.y = max(interval_days.y),
) %>% mutate(hours=(interval_days.y-interval_days.x)*24,rate=(status.x+status.y)/2)
x.jg_rate <- data.table(x.jg_rate,item='肌酐')
# head(filter(x.jg,patient_id=='AD9D23803DA89BC99DF378087B3B0155'))
# head(filter(x.jg2,patient_id=='AD9D23803DA89BC99DF378087B3B0155'))
# i <- p[7]

#################
# JMI2
#################

j <- 0
system.time(
  jm <- lapply(p[1:10],function(i){
    print(j <<- j+1)
    i.exe_rate <- filter(x.exe_rate,patient_id==i) %>% select(-patient_id)
    i.jg_rate <- as.data.frame(filter(x.jg_rate,patient_id==i)) %>%
      select(-patient_id) %>% filter(status.x==2) %>% mutate(
      rate = (((rate==2) * hours)>=48)+1
    )
    i.info <- i.base <- filter(x.base,patient_id==i)
    i.idx <- filter(x.idx,patient_id==i)
    i.exe <- filter(x.exe,patient_id==i)
    i.base <- data.frame(date=c(0,as.numeric(as.POSIXct(i.base$discharge_date_time) - as.POSIXct(i.base$admission_date_time))),
                         status = c('admission','discharge'),
                         note = c(NA,as.numeric(as.POSIXct(i.base$discharge_date_time) - as.POSIXct(i.base$admission_date_time))))
    i.idx <- select(i.idx,date=interval_days,status=item,note=value)
    i.exe1 <- select(i.exe,date=interval_days1,status=item_name,note=hours) %>% mutate(status=paste('start',status))
    i.exe2 <- select(i.exe,date=interval_days2,status=item_name,note=hours) %>% mutate(status=paste('end',status))
    i.rate <- rbind(
      select(i.exe_rate,item=item_name,interval_days1,interval_days2,rate),
      select(i.jg_rate,item,interval_days1=interval_days.x,interval_days2=interval_days.y,rate))
    i.rate$interval_days1 <- i.rate$interval_days1/i.info$interval_days
    i.rate$interval_days2 <- i.rate$interval_days2/i.info$interval_days
    i.rate <- i.rate %>% arrange(interval_days1,interval_days2)
    i <- rbind(i.base,i.idx,i.exe1,i.exe2) %>% arrange(date,status) %>% mutate(date=date/max(date))
    list(info=t(i.info),journal=i,rate=i.rate)
  })
)
