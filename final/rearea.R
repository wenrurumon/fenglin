i <- 0
out <- data.table((as.data.table(data2day2) %>% select(-obs,-lpid,-lrate,-check,-trate)),ori_rate=data2day$trate,exp_rate=itv_hmm$V1)

ret <- function(x,ret=0.3){
  x.ori <- x
  for(i in 2:length(x)){
    x[i] <- x[i-1] * ret + x[i]
  }
  x
}
ret2 <- function(x,ret2=0.3){
  x1 <- x
  x2 <- ret(x[length(x):1],ret2)[length(x):1]+ret(x,ret2)
  # x2 * max(x1)/max(x2)
  x2 <- x2 * sum(x1)/sum(x2)
  sel <- rowMeans(cbind(lag(x1),x1,lead(x1)),na.rm=T)==x1
  x2[sel] <- x1[sel]
  x2
}

i <- 0
pred_area <- t(sapply(pids,function(pidi){
  print(i<<-i+1)
  x <- filter(out,pid==pidi)$exp_rate
  x <- (x==0)*0.717354839 + (x==1)*0.465709677 + (x==2) * 0.093
  x.hour <- floor(filter(base,pid==pidi)$hours*100)
  x.rate <- filter(base,pid==pidi)$orate
  f.y <- ret2(x)
  if(length(f.y)<101){
    f.x <- filter(out,pid==pidi)$day * 6
    f.x[length(f.x)] <- x.hour/100
  } else {
    f.x <- 0:(length(f.y)) * (x.hour/(length(f.y))) / 100
    f.x <- f.x[-length(f.x)]
  }
  f <- splinefun(f.x,f.y)
  f1 <- f((0:ceiling(x.hour/100)))
  f2 <- f(1:ceiling(x.hour/200)*2-1)
  f6 <- f(1:ceiling(x.hour/600)*6-1)
  f24 <- f(1:ceiling(x.hour/2400)*24-1)
  f <- f((0:x.hour)*length(x)/x.hour)
  f0 <- mean(f)
  addarea <- sapply(list(f,f1,f2,f6,f24),mean)
  area <- f0
    # plot((1:x.hour)*length(x)/x.hour,f,type='l',col=2)
  # lines(1:length(x),ret2(x))
  c(orate=x.rate,hour=x.hour,area=area,adjarea=addarea)
}))
pred_area <- data.table(pid=pids,pred_area)

head(base)
base_area <- base %>% filter(orate==0) %>% select(pid,orate,hour=hours) %>% mutate(
  orate = 0, hour = floor(hour*100)/100, area = 0.717354839
) %>% mutate(adjarea1=area,adjarea2=area,adjarea3=area,adjarea4=area,adjarea5=area)

area <- rbind(as.data.table(base_area),as.data.table(pred_area)) %>% mutate(
  avg_area = area/hour
)



jxk <- fread('jixianku.csv',encoding='UTF-8')
jxk$patient_id
test <- select(jxk,pid=patient_id,age_group,sex,province,hos_level)
test <- merge(test,area,by=c('pid'))

test %>% group_by(age_group,orate) %>% summarise(
  n(),
  min(hour),mean(hour),median(hour),max(hour),
  min(avg_area),mean(avg_area),median(avg_area),max(avg_area)
) %>% as.data.frame()

test %>% group_by(province,orate) %>% summarise(
  n(),
  min(hour),mean(hour),median(hour),max(hour),
  min(avg_area),mean(avg_area),median(avg_area),max(avg_area)
) %>% as.data.frame()

test %>% group_by(hos_level,orate) %>% summarise(
  n(),
  min(hour),mean(hour),median(hour),max(hour),
  min(avg_area),mean(avg_area),median(avg_area),max(avg_area)
) %>% as.data.frame()

########################################
# 
# write.csv(transpro2,'transpro2.out3',row.names=F)
# write.csv(transpro,'transpro1.out3',row.names=F)
# write.csv(emisspr2,'emisspr2.out3',row.names=F)
# write.csv(emisspr,'emisspr.out3',row.names=F)
# write.csv(cbind(tag.obs),'tag_obs.out3',row.names=F)
# write.csv(area,'patient_area.out3',row.names=F)

#######################
# 
# x <- fread('prob_hid2obs2.out3')
# x <- merge(x,,by='trate') %>% mutate(con=n/con)
# write.csv(x,'prob_hid2obs2.out3')
# x <- fread('prob_hid2obs.out3')
# write.csv(merge(x,x %>% group_by(trate) %>% summarise(con=sum(n)),by='trate') %>% mutate(con=n/con),'prob_hid2obs.out3')
#           
#######################

i <- 0
area2 <- function(pidi){
  # print(i<<-i+1)
  # print(pidi)
  x <- filter(out,pid==pidi)$exp_rate
  x <- (x==0)*0.717354839 + (x==1)*0.465709677 + (x==2) * 0.093
  x.hour <- floor(filter(base,pid==pidi)$hours*100)
  x.rate <- filter(base,pid==pidi)$orate
  x.int <- filter(out,pid==pidi)$day
  f.x <- x.int*6
  f.y <- ret2(x)
  # plot(f.x,f.y,type='l')
  f <- splinefun(f.x,f.y)
  f <- f(s <- (0:ceiling(x.hour/100)))
  f <- ifelse(f>0.717354839,0.717354839,ifelse(f<0.093,0.093,f))
  # lines(s,f,type='l',col=2)
  f
}
area2 <- lapply(pids,area2)

area2_2hour <- unlist(lapply(area2,function(test){
  s <- rep(1:ceiling(length(test)/2),each=2)[1:length(test)]
  paste(tapply(test,s,mean),collapse=',')
}))
area2_6hour <- unlist(lapply(area2,function(test){
  s <- rep(1:ceiling(length(test)/6),each=6)[1:length(test)]
  paste(tapply(test,s,mean),collapse=',')
}))
area2_24hour <- unlist(lapply(area2,function(test){
  s <- rep(1:ceiling(length(test)/24),each=24)[1:length(test)]
  paste(tapply(test,s,mean),collapse=',')
}))

tarea <- function(i,k=2,f=area2_2hour){
  pidi <- pids[i]
  pidx <- filter(test,pid==pidi)
  x <- as.numeric(strsplit(f[[i]],',')[[1]])
  x <- (x[-1]+x[-length(x)])/2
  (sum(x[-length(x)]*k) + x[length(x)]*(pidx$hour%%k))/pidx$hour*100
}
tarea2_2hour <- sapply(1:length(pids),tarea,k=2,f=area2_2hour)
tarea2_6hour <- sapply(1:length(pids),tarea,k=6,f=area2_6hour)
tarea2_24hour <- sapply(1:length(pids),tarea,k=24,f=area2_24hour)
test2 <- cbind(test[match(pids,test$pid)],tarea2_2hour,tarea2_6hour,tarea2_24hour)

#############

status2 <- function(pidi){
  # print(i<<-i+1)
  # print(pidi)
  x <- filter(out,pid==pidi)$exp_rate
  x.hour <- floor(filter(base,pid==pidi)$hours*100)
  x.rate <- filter(base,pid==pidi)$orate
  x.int <- filter(out,pid==pidi)$day
  f.x <- x.int*6
  f.y <- ret2(x)
  # plot(f.x,f.y,type='l')
  f <- splinefun(f.x,f.y)
  f <- f(s <- (0:ceiling(x.hour/100)))
  f <- ifelse(f>2,2,ifelse(f<0,0,f))
  # lines(s,f,type='l',col=2)
  f
}
status2 <- lapply(pids,status2)
status2_2hour <- unlist(lapply(status2,function(test){
  s <- rep(1:ceiling(length(test)/2),each=2)[1:length(test)]
  paste(round(tapply(test,s,mean)),collapse=',')
}))
status2_6hour <- unlist(lapply(status2,function(test){
  s <- rep(1:ceiling(length(test)/6),each=6)[1:length(test)]
  paste(round(tapply(test,s,mean)),collapse=',')
}))
status2_24hour <- unlist(lapply(status2,function(test){
  s <- rep(1:ceiling(length(test)/24),each=24)[1:length(test)]
  paste(round(tapply(test,s,mean)),collapse=',')
}))

