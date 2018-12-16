
library(data.table)
library(dplyr)

rm(list=ls())
setwd('/Users/wenrurumon/Documents/fenglin')
files <- dir(pattern='csv')
pdata <- lapply(files,fread,sep=',',encoding='UTF-8')
names(pdata) <- files

x.base <- pdata$base.csv
x.idx <- pdata$idx.csv
x.exe <- pdata$exe.csv
x.med <- pdata$med.csv
x.all <- lapply(list(x.base,x.idx,x.exe,x.med),head)
p.all <- unique(c(x.base$patient_id,x.idx$patient_id,x.exe$patient_id,x.med$patient_id))

# 14：急性重症胰腺
# 12：急性坏死性胰腺炎
# 9：急性出血坏死性胰腺炎
# 15：化脓性胰腺炎

# 1.1 重症急性胰腺炎：
# 出院诊断为“急性重症胰腺炎”，
# 或者使用呼吸机持续48小时以上，
# 或者使用血透持续48小时以上，
# 或者使用血管活性药物持续48小时以上，
# 或者血肌酐值>=170 umol/L（依据为Marshall评分）持续48小时以上。

x.base_h <- filter(x.base,diag_no==14)
x.exe_h <- filter(x.exe,hours>=48)
x.idx_h <- filter(x.idx,item=='肌酐')
x.idx_h <- filter(x.idx_h,patient_id%in%unique(filter(x.idx_h,value>=170)$patient_id))
  x.idx_h1 <- data.table(x.idx_h,h=1:nrow(x.idx_h))
  x.idx_h2 <- data.table(x.idx_h,h=(1:nrow(x.idx_h))-1)
  x.idx_h <- merge(x.idx_h1,x.idx_h2,by=c('h')) %>%
    filter(patient_id.x==patient_id.y&value.x>=170&value.y>=170) %>%
    mutate(interval_hours=(interval_days.y-interval_days.x)*24) %>%
    filter(interval_hours>=48)
p.base <- unique(x.base_h$patient_id)
p.exe <- unique(x.exe_h$patient_id)
p.idx <- unique(x.idx_h$patient_id.x)
p.heavy <- table(c(p.base,p.exe,p.idx))

# 1.2 中重症急性胰腺炎：
# 存在并发症、诊断为急性重症胰腺炎或急性出血坏死性胰腺炎或急性坏死性胰腺炎或化脓性胰腺炎、
# 病程中曾转入过ICU、病程中使用过呼吸机、病程中使用过血透，
# 以上五项中存在一项即可，并且排除了以上重症急性胰腺炎的所有条件。

x.base_m <- filter(x.base,com_or_not==1)
x.base_m2 <- filter(x.base,diag_no%in%c(12,9,15))
x.idx_m <- filter(x.idx,item=='肌酐'&value>=170)
x.exe_m <- x.exe
p.base_m <- unique(c(x.base_m$patient_id,x.base_m2$patient_id))
p.exe_m <- unique(x.exe_m$patient_id)
p.idx_m <- unique(x.idx_m$patient_id)
p.mid <- table(c(p.base_m,p.exe_m,p.idx_m))
p.mid <- p.mid[!names(p.mid)%in%names(p.heavy)]

#总体病情判定，共34608个病人，5175个重度(15%)，10462个中重度(30%)

# 2. 如果是，再逐个状态判断（逐个状态判断的严重程度不会超过总体）
# 2.1 重症急性胰腺炎：该状态时间内使用呼吸机持续时间48小时以上，
#   或者使用血透持续时间48小时以上，
#   或者使用血管活性药物持续时间48小时以上， #药物数据没有时间信息所以无法标记
#   或者血肌酐值>=170 umol/L持续时间48小时以上。

x.exe2_h <- filter(x.exe,hours>=48)
x.idx2_h <- select(x.idx_h,patient_id = patient_id.x,interval_days.x,interval_days.y,item.x,value.x,value.y,interval_hours)

# 2.2 中重症急性胰腺炎：该状态时间内使用呼吸机持续时间48小时以内，
#   或者使用血透持续时间48小时以内，
#   或者使用血管活性药物持续时间48小时以内，
#   或者血肌酐值>=170 umol/L持续时间48小时以内，或者曾转入ICU。
x.exe2_m <- x.exe
x.idx2_m <- filter(x.idx,item=='肌酐'&value>=170)

# 2.3 轻症急性胰腺炎：该状态时间内排除了以上中重症和重症急性胰腺炎所有条件。



