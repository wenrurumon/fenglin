rm(list=ls())
library(data.table)
library(dplyr)
library(reshape2)
library(reshape)

setwd('E:/fenglin/')
load('jm1224.rda')
load('pdata.rda')
jm.sum <- sapply(jm,function(x){x[[1]]})
table(unlist(jm.sum[9,]))/ncol(jm.sum)
idx <- unique(pdata$idx$item)
# tag <- do.call(rbind,lapply(jm,function(x){x[[3]]}))$item
# table(tag)
tag <- c('呼吸机','ICU','血透')

test <- sapply(jm,function(x){sum(tag%in%x[[3]]$item)})

# 0          1          2
# 0.69992487 0.21807097 0.08200416

library(qfcca)
library(GenABEL)
library(flare)
library(fda)
library(corpcor)

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

imspline <- function(x,y,pos=(0:100)/100,method="monoH.FC",thred=0.1){
  if(length(x)==0){
    return(rep(NA,101))
  } else {
    x <- c(0,x,1)
    # y <- c(mean(y[1:3],na.rm=T),y,mean(y[length(y)-1:3],na.rm=T))
    y <- c(y[1],y,y[length(y)])
    fun <- splinefun(x,y, method = method)
    rlt <- fun(pos)
    rlt <- ifelse(rlt>max(y)*(1+thred),max(y)*(1+thred),rlt)
    rlt <- ifelse(rlt<min(y)*(1-thred),min(y)*(1-thred),rlt)
    rlt
  }
}

imlm <- function(y,x){
  # y <- mdata[,1]
  # x <- mdata[,2]
  sel.lm <- coef(lm(y~x))
  sel.lm[1]+sel.lm[2]*x
}

#TEST IMPUT
# jmi <- jm[[127]]
# jmi <- lapply(idx,function(i){
#   jmi$journal %>% filter(status==i)
# })
# x <- jmi[[4]]$date
# y <- jmi[[4]]$note
# x <- c(0,x,1)
# y <- c(mean(y[1:3],na.rm=T),y,mean(y[length(y)-1:3],na.rm=T))
# impute(x,y)
# plot(x,y)
# lines((0:100)/100,impute(x,y),col=2)


#######################################
# 缺失时间补齐
#######################################

#检验指标数据拟合
i <- 0
system.time(
  x.idx <- lapply(jm[1:300],function(jmi){
    print(i<<-i+1)
    info <- data.table(i=i,diag=jmi$info$diag_no,
                       days=jmi$info$interval_days,
                       com=jmi$info$com_or_not,
                       rate=jmi$info$overall_rate)
    jmi <- lapply(idx,function(i){
      jmi$journal %>% filter(status==i)
    })
    cbind(info,sapply(1:12,function(j){
      x <- jmi[[j]]$date
      y <- jmi[[j]]$note
      imspline(x,y)
      # plot(x,y)
      # lines((0:100)/100,imspline(x,y))
    }))
  })
)
x.idx <- do.call(rbind,x.idx) %>% as.data.frame
colnames(x.idx)[-1:-5] <- idx
#filter(x.idx,i==234)

#医嘱数据
#[1]    55   234   485   566   667   784  1118

j <- 0
x.exe <- lapply(jm[1:300],function(jmi){
  print(j<<-j+1)
  jmi[[3]] <- jmi[[3]] %>% mutate(
    interval_days1=ifelse(floor(interval_days1*100)<0,0,floor(interval_days1*100)),
    interval_days2=ifelse(ceiling(interval_days2*100)>101,101,ceiling(interval_days2*100))) %>% filter(
      item%in%tag
    )
  jmi <- lapply(tag,function(i){
    jmi[[3]] %>% filter(item==i) %>% arrange(rate)
  })
  temp <- matrix(0,101,3,dimnames=list(NULL,tag))
  for(i in 1:3){
    if(nrow(jmi[[i]])==0){next}
    for(k in 1:nrow(jmi[[i]])){
      temp[jmi[[i]][k,,]$interval_days1:jmi[[i]][k,,]$interval_days2,i] <- jmi[[i]][k,,]$rate
    }
  }
  temp
})
x.exe <- as.data.frame(do.call(rbind,x.exe))
x.exe$crate <- apply(x.exe,1,max)

#Test

# jm.plot <- jm[c(55,234,485,566,667,784,1118)]
# j <- 6
# jm.plot <- lapply(idx,function(i){filter(jm.plot[[j]]$journal,status==i)})
# jm.plot <- jm.plot[sapply(jm.plot,nrow)>0]
# 
# par(mfrow=c(3,2))
# for(i in 1:length(jm.plot)){
# x <- jm.plot[[i]]$date
# y <- jm.plot[[i]]$note
# y2 <- imspline(x,y)
# plot((0:100)/100,y2,col=2,type='lines'); lines(x,y,type='p')}
# 

#######################################
# 缺失变量补齐
#######################################

#合成数据
fdata2 <- fdata <- data.table(x.idx,x.exe,time=rep((0:100)/100,length=nrow(x.exe))) %>% as.data.frame()
# filter(fdata,i==234)

#数据补全
#补全数据的初始化
mdata <- fdata[,6:17]
mdata2 <- fdata[,-1:-2];# mdata2[,1] <- paste(mdata2[,1])
temp <- array(NA,dim=c(30300,20,12))
for(i in 1:12){
  print(i)
  temp[,,i] <- sapply(1:ncol(mdata2),function(j){
    imlm(mdata[,i],mdata2[,j])
  })
}
mdata2 <- apply(temp,c(1,3),function(x){mean(x,na.rm=T)})
mdata2.test <- sapply(1:12,function(i){
  x <- mdata[,i]
  y <- mdata2[,i]
  c(cor=cor(x[!is.na(x)],y[!is.na(x)]),
    corp=cor.test(x[!is.na(x)],y[!is.na(x)])$p.value,
    mape=mean(abs(x[!is.na(x)]-y[!is.na(x)])/x[!is.na(x)]))
}); print(t(mdata2.test))
mdata2[!is.na(mdata)] <- mdata[!is.na(mdata)]
fdata2[,6:17] <- mdata2

#LOW RANK Matrix Compeletion
fdata4qpca <- fdata2[,-1:-2]
fdata4qpca.colm <- colMeans(fdata4qpca)
fdata4qpca.colv <- apply(fdata4qpca,2,sd)
# qpca(scale(fdata2[,-1:-2]))$prop
fdata4qpca.rlt <- qpca(scale(fdata2[,-1:-2]),rank=14)$Z
for(i in 1:length(fdata4qpca.colm)){
  fdata4qpca.rlt[,i] <- fdata4qpca.rlt[,i]*fdata4qpca.colv[i]+fdata4qpca.colm[i]
}
fdata2[,-1:-2] <- fdata4qpca.rlt
mdata2 <- fdata2[,6:17]
mdata2.test <- sapply(1:12,function(i){
  x <- mdata[,i]
  y <- mdata2[,i]
  c(cor=cor(x[!is.na(x)],y[!is.na(x)]),
    corp=cor.test(x[!is.na(x)],y[!is.na(x)])$p.value,
    mape=mean(abs(x[!is.na(x)]-y[!is.na(x)])/x[!is.na(x)]))
}); print(t(mdata2.test))
fdata2[!is.na(fdata)] <- fdata[!is.na(fdata)]

#######################################
# 基本预测
#######################################

sel <- unlist(lapply(1:300,function(i){
  jmi.date <- round((jm[[i]]$journal)$date,2)
  paste(i,jmi.date)
}))
fdata2[,1] <- as.factor(fdata2[,1])
mdata <- filter(fdata2,paste(i,time)%in%sel)[,-1]
fdata <- fdata2[,-1]

m.lda <- MASS::lda(crate~.,data=mdata)
p.lda <- predict(m.lda,fdata)$class
table(crate=fdata$crate,predict=p.lda)

m.logi <- glm(crate~.,data=mdata)
p.logi <- round(predict(m.logi,newdata=fdata,type='response'))
p.logi[p.logi==3] <- 2
table(crate=fdata$crate,predict=p.logi)

# table(mdata$crate)
# table(mdata$crate)/sum(table(mdata$crate))
# table(mdata$rate)
# table(mdata$rate)/sum(table(mdata$rate))


