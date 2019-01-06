
rm(list=ls())
setwd('C:\\Users\\WenluluSens\\Downloads\\output20190102')
library(dplyr)
library(data.table)
library(MatrixCompletion)

id <- as.data.table(openxlsx::read.xlsx('idmap.xlsx') %>% 
                      select(pid=patient_id,grp=group16))
x <- data.table::fread('daily_final.csv') %>% mutate(y=(
  (exp_rate==0)*0.505 + (exp_rate==1)*0.261 + (exp_rate==2) * 0.114
))
# x <- x %>% group_by(pid) %>% 
#   summarise(day=max(day),orate=mean(orate),
#             srate=sum(exp_rate),mrate=mean(exp_rate))

rawimspline <- function(y,x=1:length(y),pos=24,ifplot=F){
  # x <- 1:length(y)
  # y <- y
  f <- splinefun(x,y,method='monoH.FC')
  x2 <- (1:(max(x)*pos)/pos)
  y2 <- f(x2)
  if(ifplot){
    plot(x2,y2,type='l',col=2)
    lines(x,y,type='p')
  }
  y2
}

p <- unique(x$pid)

# par(mfrow=c(3,3))
# test <- lapply(p[1:9+9],function(i){
#   print(i)
#   x <- filter(x,pid==i)$y
#   rawimspline(x,ifplot=T)
# })

system.time(
  test <- lapply(p,function(i){
    print(i)
    x <- filter(x,pid==i)$y
    rawimspline(x)
  })
)
test <- sapply(test,sum)

x <- x %>% group_by(pid) %>%
  summarise(day=max(day),orate=mean(orate),
            rate1 = sum(y)) %>% mutate(rate2=test)
x <- x %>% mutate(rate1=rate1/day,rate2=rate2/24/day)

x2 <- unique(data.table::fread('fdata.temp',encoding='UTF-8') 
             %>% select(pid,orate,hours) %>% 
               mutate(day=ceiling(hours/24)) %>% select(-hours)) %>% filter(
    orate==0) %>% select(-orate) %>% mutate(orate=0,rate1=0.505,rate2=0.505)
x <- rbind(x,x2)
 
x <- merge(x,id,by=c('pid'))
grp <- x$grp
grp[grp%in%c(1,5,6,11)] <- 'A'
grp[grp%in%c(2,3,8.16)] <- 'B'
grp[!(grp%in%c('A',"B"))] <- 'OTHERS'
x$grp <- grp

x <- x %>% mutate(rate1=rate2*day)
x %>% group_by(grp) %>% summarise(n=n(),day=mean(day),rate1=mean(rate1),rate2=mean(rate2))
