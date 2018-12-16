
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

j <- 0
system.time(
    jm <- lapply(p[1:10],function(i){
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
    i <- rbind(i.base,i.idx,i.exe1,i.exe2) %>% arrange(date,status) %>% mutate(date=date/max(date))
    list(info=i.info,journal=i)
  })
)

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


