
library(data.table)
library(dplyr)
library(openxlsx)
# library(lubridate)

rm(list=ls())
# setwd('e:\\fenglin\\data')
setwd('/Users/wenrurumon/Documents/fenglin/data')
files <- dir(pattern='csv')
jyzb  <- do.call(rbind,lapply(files[1:2],fread,encoding='UTF-8'))
jxk <- fread(files[[3]],encoding='UTF-8')
yizhu <- do.call(rbind,lapply(files[7:8],fread,encoding='UTF-8'))
others <- lapply(files[4:6],fread,encoding='UTF-8')

##########################
#检验指标 x.idx
##########################

# x <- select(jyzb,patient_id,spcm_sample_date_time,report_name,result_final)
# x <- filter(x,spcm_sample_date_time!='')
# x$date_error <- grepl(' ',x$spcm_sample_date_time); mean(x$date_error)
# filter(x, date_error==0)
# x <- filter(x, patient_id %in% filter(x %>% group_by(patient_id) %>% summarise(r=mean(date_error)),r==1)$patient_id)
# #删除了所有时间记录有问题的样本
x <- jyzb %>% select(patient_id,day=interval_days,item=report_name,result_final)
x.idx <- unique(x$item)
x <- lapply(x.idx,function(i){
  filter(x,item==i)
})
names(x) <- x.idx
x.min <- x[names(x) %in% c('血钙','血小板计数','白蛋白')]
x.min <- lapply(x.min, function(x){
  x %>% group_by(patient_id,day,item) %>% summarise(value=min(result_final))
})
x.max <- x[names(x)%in%c('白细胞计数','血淀粉酶','肌酐','乳酸脱氢酶','总胆红素','甘油三酯','脂肪酶','谷丙转氨酶','降钙素原')]
x.max <- lapply(x.max, function(x){
  x %>% group_by(patient_id,day,item) %>% summarise(value=max(result_final))
})
x.idx <- c(x.min,x.max)
x.idx <- do.call(rbind,x.idx) %>% select(patient_id,interval_days=day,item,value)
patient.idx <- unique(x.idx$patient_id)

# 1.<最小值>
#   血钙
# 血小板
# 白蛋白
#
# 2.<最大值>
#   白细胞计数
# 血淀粉酶
# 血尿素氮
# 肌酐
# 乳酸脱氢酶
# C-反应蛋白（CRP）
# 谷草转氨酶
# 总胆红素
# 甘油三酯
# 脂肪酶
# 谷丙转氨酶
# 降钙素原
#
# 3.<特殊,可以不考虑，取偏离正常值最远的值(正常值:男：0.40-0.50;女：0.35-0.45)>
#   红细胞压积
#
# 4.<不考虑>
#   abnormal_indicator


##########################
#基线库 x.base
##########################

x <- select(jxk,patient_id,com_or_not,admission_date_time,discharge_date_time,discharge_diag)
filter(x,(admission_date_time==''|discharge_date_time==''))
#有十条记录的入院时间为空
x.base <- filter(x,grepl(' ',x$admission_date_time)&grepl(' ',x$discharge_date_time))
x.base$interval_days <- as.numeric(as.POSIXct(x.base$discharge_date_time)-as.POSIXct(x.base$admission_date_time))
x.base <- select(x.base,patient_id,admission_date_time,discharge_date_time,com_or_not,discharge_diag,interval_days)
patient.base <- unique(x.base$patient_id)
#参数比基线库的病人id更多。

##########################
#医嘱 exe
##########################

colnames(yizhu) <- tolower(colnames(yizhu))
x <- filter(yizhu,
               grepl(' ',admission_date_time)&
                 grepl(' ',start_date_time)&
                 grepl(' ',stop_date_time)) %>%
  mutate(hours=sp-st) %>%
  select(patient_id,item_name,hours,admission_date_time,start_date_time,stop_date_time)

x$t1 = as.numeric(as.POSIXct(x$start_date_time)-as.POSIXct(x$admission_date_time))/3600/24
x$t2 = as.numeric(as.POSIXct(x$stop_date_time)-as.POSIXct(x$admission_date_time))/3600/24
x.exe <- unique(x) %>%
  select(patient_id,item_name,interval_days1=t1,interval_days2=t2,hours)
patient.exe <- unique(x.exe$patient_id)

##########################
#血管活性药物
##########################

x <- others[[1]]
colnames(x) <- tolower(colnames(x))
x.med <- mutate(x,interval_days=interval_hours/24) %>% select(
  patient_id,interval_days
)
x.med %>% group_by(patient_id) %>% summarise(
  out=paste(interval_days,collapse=' '),
  n=n()) %>% arrange(desc(n))

##########################
#Merge Data
##########################

uniqueid <- patient.base[patient.base%in%patient.exe]
uniqueid <- uniqueid[uniqueid%in%patient.idx]
sapply(list(uniqueid,patient.base,patient.idx,patient.exe),length)
#共有的样本一共有11775个，基线34608个，检验指标36934个，医嘱12843个

##########################
#Journal Data
##########################

lapply(pdata <- list(x.base,x.idx,x.exe),head)
names(pdata) <- c('base','idx','exe')
lapply(pdata,head)
