
rm(list=ls())
library(data.table)
library(dplyr)
setwd('/Users/wenrurumon/Downloads/data')
files <- dir(pattern='temp')
files <- lapply(files,function(x){fread(x) %>% select(-V1)})

exe <- files[[1]]
fdata <- files[[2]]
idx <- files[[3]]
mdata <- files[[4]]
rate <- files[[5]]

qpca <- function(A,rank=0){
  # A <- scale(A)
  A.svd <- svd(A)
  if(rank==0){
    d <- A.svd$d
  } else {
    d <- A.svd$d-A.svd$d[min(rank+1,nrow(A),ncol(A))]
  }
  d <- d[d > 1e-10]
  r <- length(d)
  prop <- d^2; prop <- cumsum(prop/sum(prop))
  d <- diag(d,length(d),length(d))
  u <- A.svd$u[,1:r,drop=F]
  v <- A.svd$v[,1:r,drop=F]
  x <- u%*%sqrt(d)
  y <- sqrt(d)%*%t(v)
  z <- x %*% y
  rlt <- list(rank=r,X=x,Y=y,Z=x%*%y,prop=prop)
  return(rlt)
}

###########################

base <- unique(select(fdata,pid,hours,orate))
fdata2 <- select(fdata,-pid)
fdata2.mean <- colMeans(fdata2)
fdata2.sd <- apply(fdata2,2,sd)
fdata2 <- qpca(scale(fdata2),14)$Z
for(i in 1:ncol(fdata2)){
  fdata2[,i] <- (fdata2[,i] * fdata2.sd[i]) + fdata2.mean[i]
  print(cor(fdata2[,i],fdata[,i+1,with=F]))
}
fdata <- as.data.frame(fdata)
colnames(fdata2) <- colnames(fdata)[-1]
fdata[,6:21] <- fdata2[,5:20]
fdata <- as.data.table(fdata)

sel1 <- merge(unique(select(idx,pid,hours)),
              select(base,pid,thours=hours),by=c('pid')) %>% mutate(
                idx = floor(hours/thours*100), check = (idx>100|idx<0)
              ) %>% filter(!check) %>% select(-hours,-thours,-check)
sel1 <- unique(paste(sel1$pid,sel1$idx))
sel2 <- merge(unique(select(exe,pid,hour1,hour2)),
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

########################

base <- unique(select(fdata,pid,orate,hours))
base %>% group_by(orate) %>% summarise(n(),mean(hours))
set.seed(12345); train <- sample(1:nrow(mdata))
train <- lapply(unique(cut(1:nrow(mdata),10)),function(i){
  train[cut(1:nrow(mdata),10)==i]
})
fdata.rate <- fdata$trate
fdata2 <- scale(select(fdata,-pid,-trate))
data2.x <- qpca(fdata2,12)$X
colnames(data2.x) <- paste0('CS',1:ncol(data2.x))
data2.t <- fdata$trate
fdata2.x <- data.table(data2.x,trate=data2.t)
mdata2.x <- fdata2.x[sel2%in%sel1,]

model <- sapply(1:10,function(i){
  print(i)
  traindf <- as.data.frame(mdata2.x[train[[i]],])
  testdf <- as.data.frame(mdata2.x[-train[[i]],])
  # modeli <- glm(trate~.,data=traindf)
  # predi <- predict(modeli,newdata=as.data.frame(mdata.x),type='response')
  modeli <- MASS::lda(trate~.,data=traindf)
  predi <- predict(modeli,newdata=fdata2.x)$class
  print(table(predi=predi,trate=fdata2.x$trate))
  as.numeric(predi)-1
})
model <- rowMeans(model10 <- model)

# model10[!model10%in%c(0,2)] <- 1
# test <- rowMeans(model10)
# test[!test%in%c(0,2)] <- 1
#  
# test <- table(predict=round(model,0)[sel2%in%sel1],actual=fdata.rate[sel2%in%sel1]);test;sum(diag(test))/sum(test)
# test <- table(predict=predict(MASS::lda(fdata.rate[sel2%in%sel1]~model10[sel2%in%sel1,]))$class,actual=fdata.rate[sel2%in%sel1]);test;sum(diag(test))/sum(test)
# test <- table(predict=round(model,0),actual=fdata.rate);test;sum(diag(test))/sum(test)
# model10.rate <- data.frame(model10,rate=fdata.rate)
# test <- predict(MASS::lda(rate~.,data=model10.rate[sel2%in%sel1,]),newdata=model10.rate)$class
# test <- table(predict=test,actual=fdata.rate);test;sum(diag(test))/sum(test)
# model10.rate <- data.frame(x=model,rate=fdata$rate)
# test <- predict(MASS::lda(rate~.,data=model10.rate[sel2%in%sel1,]),newdata=model10.rate)$class
# table(test[sel2%in%sel1],fdata.rate[sel2%in%sel1])
# test <- table(predict=test,actual=fdata.rate);test;sum(diag(test))/sum(test)
# fdata <- mutate(fdata,rate=as.numeric(test)-1)

test <- model
test[!test%in%c(0,2)] <- 1
table(predict=test[sel2%in%sel1],actual=fdata$trate[sel2%in%sel1])
table(predict=test,actual=fdata$trate)
fdata <- mutate(fdata,mrate=test)
fdata <- mutate(fdata,rate=ifelse(mrate>trate,mrate,trate))

(fdata %>% group_by(pid) %>% summarise(orate=mean(orate),rate=max(rate),trate=max(trate))) %>% group_by(
  orate,rate
) %>% summarise(n())

test <- apply(((fdata %>% group_by(pid) %>% summarise(orate=mean(orate),rate=max(rate),trate=max(trate))) %>% select(
  -pid
)),2,function(x){as.numeric(table(x))})
test / colSums(test)

table(predict=fdata$rate,raw=fdata$trate)

########################
# HMM
########################

data_hmm <- as.data.table(data2.x) %>% mutate(rate=fdata$orate)
data_hmm <- data_hmm %>% group_by(pid=fdata$pid) %>% summarise(
  CS1=mean(CS1),CS2=mean(CS2),CS3=mean(CS3),CS4=mean(CS4),CS5=mean(CS5),CS6=mean(CS6),
  CS7=mean(CS7),CS8=mean(CS8),CS9=mean(CS9),CS10=mean(CS10),CS11=mean(CS11),CS12=mean(CS12),
  orate = (mean(rate)>0)+0
)
cbind(pvalue=apply(select(data_hmm,-pid,-orate),2,function(x){t.test(x~data_hmm$orate)$p.value}),
      thred=colMeans(apply(select(data_hmm,-pid,-orate),2,function(x){MASS::lda(data_hmm$orate~x)$means})))

data_hmm.thred <- colMeans(apply(select(data_hmm,-pid,-orate),2,function(x){MASS::lda(data_hmm$orate~x)$means}))
for(i in 1:ncol(data2.x)){
  data2.x[,i] <- (data2.x[,i] > data_hmm.thred[i])+0
}

