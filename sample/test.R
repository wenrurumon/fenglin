
rm(list=ls())
library(MatrixCompletion)
library(data.table)
library(dplyr)
library(sqldf)
# setwd('..')#set your working folder here
# load('sample1.rda')
load("/Users/wenrurumon/Downloads/sample1.rda")

#Pending
#orate calcualtion 代码准备好
#Data dictionary 表头的意义
#Missing Value% 
#Correlation Test 有结果的我去找找
#Trate calculation with claimed method 加一个金标准的计算 mrate trate取最大值
#面积的计算 两小时accumulated value。

##############################
##############################

library(HMM)
inithmm <- function(prob.hid2obs,probhid2hid,prob.orihid){
  state <- paste(unique(data2day$trate))
  Symbols <- paste(unique(data2day$obs))
  startprob <- prob.orihid$n
  transpro <- matrix(0,3,3)
  for(j in 1:nrow(prob.hid2hid)){
    transpro[prob.hid2hid$lrate[j]+1,prob.hid2hid$trate[j]+1] <- prob.hid2hid$n[j]
  }
  transpro <- transpro / rowSums(transpro)
  emisspr <- do.call(rbind,lapply(0:2,function(i){
    x <- filter(prob.hid2obs,trate==i)
    x <- x$n[match(Symbols,x$obs)]
    x[is.na(x)] <- 0
    x/sum(x)
  }))
  system.time(hmm <- initHMM(States = state,Symbols = Symbols,startProbs = startprob,transProbs = transpro,
                             emissionProbs = emisspr))
  hmm
}
predhmm <- function(j){
  oberi <- paste(filter(data2day,pid==j)$obs)
  ratei <- paste(filter(data2day,pid==j)$trate)
  V1=as.numeric(viterbi(hmm,oberi))
  V1
}

#Processing raw data to Journey data
system.time(jms <- lapply(unique(base$pid),jm)) #impute on the scale of timeseries
jms <- do.call(rbind,jms);dim(jms)

#Journey data to Model data
mdata <- as.data.frame(jms)[,which(colSums(is.na(jms))>1)]
rdata <- as.data.table(as.data.frame(jms)[,-c(1,3)])
mdata2 <- apply(mdata,2,imlms,x=rdata) # 
fdata <- as.data.frame(jms)
fdata[,which(colSums(is.na(jms))>1)] <- mdata2

########end of data imputation##############

#filter the sample * time splot with determined information
sel1 <- merge(unique(select(idx,pid,hours)),
              select(base,pid,thours=hours),by=c('pid')) %>% mutate(
                idx = floor(hours/thours*100), check = (idx>100|idx<0)
              ) %>% filter(!check) %>% select(-hours,-thours,-check)
sel1 <- unique(paste(sel1$pid,sel1$idx))
sel2 <- merge(unique(select(fexe,pid,hour1,hour2)),
              select(base,pid,thours=hours),by=c('pid')) %>% mutate(
                idx1 = floor(hour1/thours*100),idx2=ceiling(hour2/thours*100),
                idx3 = round((hour1+hour2)/2/thours*100,0),
                check = (idx2>100|idx1<0)
              ) 
sel2 <- unique(c(paste(sel2$pid,sel2$idx1),
                 paste(sel2$pid,sel2$idx2),
                 paste(sel2$pid,sel2$idx3)))
sel1 <- unique(c(sel1,sel2))
sel2 <- paste(fdata$pid,rep(0:100,length=nrow(fdata)))
mdata <- fdata[sel2%in%sel1,]

#Label Prediction
base <- unique(select(fdata,pid,orate,hours))
base %>% group_by(orate) %>% summarise(n(),mean(hours))
fdata.rate <- fdata$trate
fdata2 <- scale(select(fdata,-pid,-trate)) #scale
data2.x <- qpca(fdata2,12)$X # low rank pca
colnames(data2.x) <- paste0('CS',1:ncol(data2.x))

data2.t <- fdata$trate
fdata2.x <- data.table(data2.x,trate=data2.t)
mdata2.x <- fdata2.x[sel2%in%sel1,]
model <- predict(MASS::lda(trate~.,data=fdata2.x))$class
fdata <- mutate(fdata,mrate=as.numeric(paste(model)))
fdata <- mutate(fdata,rate=ifelse(mrate > trate,mrate,trate))

########end of Linear label prediction##############

#HMM variable preprocess -> turn numerical into logical
data_hmm <- as.data.table(data2.x) %>% mutate(rate=fdata$orate)
test <-  data_hmm %>% group_by(pid=fdata$pid) %>% summarise(
  CS1=mean(CS1),CS2=mean(CS2),CS3=mean(CS3),CS4=mean(CS4),CS5=mean(CS5),CS6=mean(CS6),
  CS7=mean(CS7),CS8=mean(CS8),CS9=mean(CS9),CS10=mean(CS10),CS11=mean(CS11),CS12=mean(CS12),
  orate = mean(rate)
)
data_hmm <- data_hmm %>% group_by(pid=fdata$pid) %>% summarise(
  CS1=mean(CS1),CS2=mean(CS2),CS3=mean(CS3),CS4=mean(CS4),CS5=mean(CS5),CS6=mean(CS6),
  CS7=mean(CS7),CS8=mean(CS8),CS9=mean(CS9),CS10=mean(CS10),CS11=mean(CS11),CS12=mean(CS12),
  orate = (mean(rate)>0)+0
)
test <- test %>% as.data.frame()
rownames(test) <- test$pid
test <- select(test,-pid)
table(predict=predict(MASS::lda(orate~.,data=test))$class,raw=test$orate)

#correlation test between CS and overall rate
cbind(pvalue=apply(select(data_hmm,-pid,-orate),2,function(x){t.test(x~data_hmm$orate)$p.value}),
      thred=colMeans(apply(select(data_hmm,-pid,-orate),2,function(x){MASS::lda(data_hmm$orate~x)$means})))

#transform cs data from numeric into logical
data_hmm.thred <- colMeans(apply(select(data_hmm,-pid,-orate),2,function(x){MASS::lda(data_hmm$orate~x)$means}))
data_hmm <- as.data.frame(data2.x)
for(i in 1:ncol(data2.x)){data_hmm[,i] <- (data_hmm[,i] >= data_hmm.thred[i])+0} # if CSi > THREDHOLDi then provide 1 otherwise 
data_hmm <- data.table(data_hmm,rate=fdata$rate,pid=fdata$pid)
data_hmm <- mutate(data_hmm,lrate=lag(rate,d1efault=0),lpid=lag(pid,default='0'))

#HMM to determined time interval scale
#Every two hours
data2day <- data.table(select(fdata,pid,hours,orate,trate),
                   t=rep(0:100,length=nrow(data2.x)),data2.x) %>% filter(
                     orate>0) %>% mutate(day=floor(hours/24/100*t+1)) %>% group_by(
                       pid,orate,day
                     ) %>% summarise(
                       trate = max(trate),
                       CS1=mean(CS1),CS2=mean(CS2),CS3=mean(CS3),CS4=mean(CS4),CS5=mean(CS5),CS6=mean(CS6),
                       CS7=mean(CS7),CS8=mean(CS8),CS9=mean(CS9),CS10=mean(CS10),CS11=mean(CS11),CS12=mean(CS12)
                     ) %>% as.data.frame()
for(i in 5:ncol(data2day)){data2day[,i] <- (data2day[,i] > data_hmm.thred[i-4])+0}
summary2day <- data2day %>% group_by(trate) %>% summarise(
  n = n(),
  CS1=sum(CS1),CS2=sum(CS2),CS3=sum(CS3),CS4=sum(CS4),CS5=sum(CS5),CS6=sum(CS6),
  CS7=sum(CS7),CS8=sum(CS8),CS9=sum(CS9),CS10=sum(CS10),CS11=sum(CS11),CS12=sum(CS12)
)
summary2day <- rbind(as.data.frame(summary2day),colSums(summary2day)) %>% mutate(
  CS1 = CS1/n, CS2 = CS2/n, CS3 = CS3/n, CS4 = CS4/n, CS5 = CS5/n, CS6 = CS6/n, 
  CS7 = CS7/n, CS8 = CS8/n, CS9 = CS9/n, CS10 = CS10/n, CS11 = CS11/n, CS12 = CS12/n 
)
data2day <- as.data.table(data2day) %>% mutate(obs = paste(CS1,CS2,CS3,CS4,CS5,CS6,CS7,CS8,CS9,CS10,CS11,CS12),
                                               lpid=lag(pid,default='0'), lrate=lag(trate,default=0), check=(lpid==pid))
tag.obs <- unique(data2day$obs)
data2day <- data2day %>% mutate(obs = match(obs,tag.obs))

#HMM input
pids <- unique(data2day$pid)
(prob.hid2obs <- data2day %>% group_by(trate,obs) %>% summarise(n=n()))
(prob.hid2hid <- data2day %>% filter(check) %>% group_by(lrate,trate) %>% summarise(n=n()))
(prob.orihid <- data2day %>% filter(day==1) %>% group_by(trate) %>% summarise(n=n()))

#HMM Moodule
hmm <- inithmm(prob.hid2obs,prob.hid2hid,prob.orihid)
data2day2 <- data2day %>% mutate(trate = unlist(lapply(pids,predhmm)), 
                                 lrate = lag(unlist(lapply(pids,predhmm)),default=0))

#Aggregate to daily
(prob.hid2obs2 <- data2day2 %>% group_by(trate,obs) %>% summarise(n=n()))
(prob.hid2hid2 <- data2day2 %>% filter(check) %>% group_by(lrate,trate) %>% summarise(n=n()))
(prob.orihid2 <- data2day2 %>% filter(day==1) %>% group_by(trate) %>% summarise(n=n()))

fhmm <- inithmm(prob.hid2obs2,prob.hid2hid2,prob.orihid2)
fhmm[1:4]
