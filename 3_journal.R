
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
# i <- '09BB90F86C55828BC75D84E0C45E361B' #重度

j <- 0
system.time(
    jm <- lapply(p[1:100],function(i){
    print(j <<- j+1)
    i.base <- filter(x.base,patient_id==i)
    i.idx <- filter(x.idx,patient_id==i)
    i.exe <- filter(x.exe,patient_id==i)
    i.base <- data.frame(date=c(0,as.numeric(as.POSIXct(i.base$discharge_date_time) - as.POSIXct(i.base$admission_date_time))),
                         status = c('admission','discharge'),
                         note = c(NA,as.numeric(as.POSIXct(i.base$discharge_date_time) - as.POSIXct(i.base$admission_date_time))))
    i.idx <- select(i.idx,date=interval_days,status=item,note=value)
    i.exe1 <- select(i.exe,date=interval_days1,status=item_name,note=hours) %>% mutate(status=paste('start',status))
    i.exe2 <- select(i.exe,date=interval_days2,status=item_name,note=hours) %>% mutate(status=paste('end',status))
    i <- rbind(i.base,i.idx,i.exe1,i.exe2) %>% arrange(date,status) %>% mutate(date=date/max(date))
    i
  })
)
