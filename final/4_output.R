i <- 0

pred_area <- data.table(pid=pids,pred_area)

head(base)
base_area <- base %>% filter(orate==0) %>% select(pid,orate,hour=hours) %>% mutate(
  orate = 0, hour = floor(hour*100)/100, area = hour * 0.505*100
)

area <- rbind(as.data.table(base_area),as.data.table(pred_area)) %>% mutate(
  avg_area = area/hour
)

write.csv(transpro2,'transpro2.out3',row.names=F)
write.csv(transpro,'transpro1.out3',row.names=F)
write.csv(emisspr2,'emisspr2.out3',row.names=F)
write.csv(emisspr,'emisspr.out3',row.names=F)
write.csv(cbind(tag.obs),'tag_obs.out3',row.names=F)

#######################

i <- 0
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
write.csv(cbind(pid=pids,area=area2_2hour),'area_2hour.out3',row.names=F)
write.csv(cbind(pid=pids,area=area2_6hour),'area_6hour.out3',row.names=F)
write.csv(cbind(pid=pids,area=area2_24hour),'area_24hour.out3',row.names=F)

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
write.csv(cbind(pid=pids,status=status2_2hour),'status_2hour.out3',row.names=F)
write.csv(cbind(pid=pids,status=status2_6hour),'status_6hour.out3',row.names=F)
write.csv(cbind(pid=pids,status=status2_24hour),'status_24hour.out3',row.names=F)
