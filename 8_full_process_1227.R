
rm(list=ls())
library(data.table)
library(dplyr)
# library(openxlsx)
# library(lubridate)
# library(fda)
# library(flare)
# library(corpcor)

############################
# 函数
############################

imspline <- function(x,y,pos=max(x),thred=0.1,ifplot=F){
  pos <- ceiling(pos)
  if(length(x)==0){
    return(rep(NA,pos+1))
  }
  if(length(x)==1){
    return(rep(x,pos+1))
  }
  f <- splinefun(x,y,method='monoH.FC')
  f <- f(0:pos)
  f <- ifelse(f>max(y)*(1+thred),max(y)*(1+thred),f)
  f <- ifelse(f<min(y)*(1-thred),min(y)*(1-thred),f)
  if(ifplot){
    plot.ts((0:pos),f,type='l',col=2)
    lines(x,y,type='p')
  }
  f
}
imfourier <- function(x,y,pos=max(x),ifplot=F){
  y <- y[order(x)]
  x <- x[order(x)]
  pos <- ceiling(pos)
  fbasis <- create.fourier.basis(range(x)/pos,length(x)*2-1)
  phi <- eval.basis(x/pos,fbasis)
  fcoef <- myinv(t(phi)%*%phi)%*%t(phi)%*%cbind(x/pos)
  phi2 <- eval.basis((0:pos)/pos,fbasis)
  f <- phi2 %*% fcoef
  if(ifplot){
    plot((0:pos)/pos,f,type='l',col=2)
    lines(x/pos,y,type='p')
  }
  f
}
imrange <- function(i,pos){
  pos <- ceiling(pos)
  out <- rep(0,length=pos+1)
  if(nrow(i)==0){
    return(out)
  }
  for(j in 1:nrow(i)){
    out[(floor(i$hour1[j])*(i$hour1[1]>0)):min(ceiling(i$hour2[j]),(pos+1))] <- 1
  }  
  out <- out[1:(pos+1)]
  return(out)
}
imlm <- function(y,x){
  sel.lm <- coef(lm(y~x))
  sel.lm[1]+sel.lm[2]*x
}
myinv<-function(A){
  A_svd<-fast.svd(A)
  if(length(A_svd$d)==1){
    A_inv<-A_svd$v%*%as.matrix(1/A_svd$d)%*%t(A_svd$u)
  }else{
    A_inv<-A_svd$v%*%diag(1/A_svd$d)%*%t(A_svd$u)
  }
  return(A_inv)
}
imcor <- function(i,j,test=F){
  sel <- !(is.na(i))&!(is.na(j))
  if(test){
    cor.test(i[sel],j[sel])$p.value
  } else {
    cor(i[sel],j[sel])
  }
}
imlms <- function(y,xs){
  temp <- apply(xs,2,function(x){
    imlm(y,x)
  })
  rowMeans(temp,na.rm=T)
}

############################
# 数据导入
############################

setwd('e:\\fenglin\\data')
files <- dir(pattern='csv')
jyzb  <- do.call(rbind,lapply(files[1:2],fread))
jxk <- fread(files[[3]],encoding='UTF-8')
yizhu <- do.call(rbind,lapply(files[7:8],fread,encoding='UTF-8'))
yaowu <- do.call(rbind,lapply(files[4:5],fread,encoding='UTF-8'))
others <- lapply(files[6],fread,encoding='UTF-8')

#检验指标
idx <- jyzb %>% select(pid=patient_id,
                       hours=interval_hours,
                       item=report_name,
                       value=result_final)
# idx <- idx %>% group_by(pid,hours,item) %>% summarise(value=mean(value))
#基线库
#如果diagno为14则重，12/9/15则中，有并发症则中，取最重。
base <- select(jxk,pid=patient_id,com_or_not,admission_date_time,
               discharge_date_time,diag,diag_no) %>% mutate(
                 brate = (diag_no==14)*2+diag_no%in%c(12,9,15)*1
               )
base <- filter(base,grepl(' ',base$admission_date_time)&grepl(' ',base$discharge_date_time))
base$hours <- 24*as.numeric(as.POSIXct(base$discharge_date_time)-as.POSIXct(base$admission_date_time))
base <- base %>% mutate(brate = ifelse(brate>com_or_not,brate,com_or_not))
#医嘱
exe <- yizhu %>% select(
  pid=PATIENT_ID,hour1=ST, hour2=SP,item=ITEM_NAME
  ) %>% mutate(hours=hour2-hour1)
#血管活性药物
med <- yaowu %>% select(pid=PATIENT_ID,hour1=INTERVAL_HOURS
                        ) %>% mutate(
                          hour2=hour1,item='药物',hours = 0
                        )
exe <- rbind(exe,med)

############################
# Temp Rating
############################

#肌酐
#两次连续的肌酐超过170时间如果超过48小时，则这个区间内为重症，否则为中
jg <- filter(idx,item=='肌酐'&pid%in%filter(idx,item=='肌酐'&value>=170)$pid
             ) %>% arrange(pid,hours) %>% mutate(
  lagid = lag(pid,default=0), 
  rate = value >= 170,
  lagrate = lag(rate,default=0),
  newid = (lagid!=pid),
  newrate = (rate!=lagrate),
  check = newid|newrate
)
jg$check <- cumsum(jg$check)
jg <- jg %>% group_by(pid,check,item) %>% summarise(
  hour1=min(hours),hour2=max(hours),rate=mean(rate)) %>% mutate(
    hours = hour2-hour1, rate=rate+(hours>=48)
  ) %>% filter(rate>0) %>% select(pid,item,hour1,hour2,rate)
#血透
#持续血透时间超过48小时，则为重，否则为中。两次血透之间间隔24小时内认为是持续血透
xt <- unique(filter(exe,item=='血透')) %>% arrange(pid,hour1,hour2) %>% mutate(
  lagh2 = lag(hour2,default=0),
  lagid = lag(pid,default=0),
  newid = (lagid!=pid),
  newjn = (hour1-lagh2)>24,
  check = newid|newjn
)
xt$check <- cumsum(xt$check)
xt <- xt %>% group_by(pid,item,check) %>% summarise(
  hour1=min(hour1),hour2=max(hour2)) %>% mutate(
    rate = (hour2-hour1>=48)+1
  ) %>% select(-check)
#呼吸机
#同血透
hxj <- unique(filter(exe,item=='呼吸机')) %>% arrange(pid,hour1,hour2) %>% mutate(
  lagh2 = lag(hour2,default=0),
  lagid = lag(pid,default=0),
  newid = (lagid!=pid),
  newjn = (hour1-lagh2)>24,
  check = newid|newjn
)
hxj$check <- cumsum(hxj$check)
hxj <- hxj %>% group_by(pid,item,check) %>% summarise(
  hour1=min(hour1),hour2=max(hour2)) %>% mutate(
    rate = (hour2-hour1>=48)+1
  ) %>% select(-check)
#血管活性药物
#如果有发生则当天为中
med <- select(med,pid,item,hour1,hour2) %>% mutate(rate=1)

#合成数据
rate <- rbind(jg,xt,hxj,med)
rate2 <- rate %>% group_by(pid) %>% summarise(trate=max(rate))

base2 <- sqldf::sqldf('select a.*, b.trate from base a left join rate2 b on a.pid = b.pid')
base2$diag <- base$diag
base2$trate[is.na(base2$trate)] <- 0
base2$orate <- ifelse(base2$trate>base2$brate,base2$trate,base2$brate)
base <- base2 %>% select(-brate,-trate)

############################
# SPLINE IMPUTATION
############################

fexe <- rbind(filter(rate,item!='肌酐') %>% select(pid,hour1,hour2,item),
              filter(exe,item%in%c('ICU','血气','药物')) %>% select(pid,hour1,hour2,item))
tag.idx <- unique(idx$item)
tag.exe <- unique(fexe$item)

############

# pidi <- unique(base$pid)[[16]]
# 
# jm <- function(pidi){
#   print(k<<-k+1)
#   i.base <- filter(base,pid==pidi)
#   i.idx <- filter(idx,pid==pidi)
#   i.idx <- sapply(tag.idx,function(i){
#     i <- filter(i.idx,item==i) %>% 
#       group_by(pid,hours,item) %>% summarise(value=mean(value))
#     imspline(i$hours,i$value,i.base$hours,0.1,F)
#   })
#   i.exe <- filter(fexe,pid==pidi)
#   i.exe <- sapply(tag.exe,function(i){
#     i <- filter(i.exe,item==i)
#     imrange(i,i.base$hours)
#   })
#   rlt <- data.table(pid=pidi,com=i.base$com_or_not,diag_no=i.base$diag_no,
#         orate=i.base$orate,hours=i.base$hours,
#         i.idx,i.exe)
#   rlt
# }
# k <- 0
# options(warn = 1)
# system.time(jms <- lapply(unique(base$pid),jm))
# jms <- do.call(rbind,jms)
# save(jms,file='jms_20181227.rda')

############################
# Descriptive Summary
load('jms_20181227.rda')
############################

#data validation
pid <- unique(jms$pid)
jms.colmeans <- as.data.frame(jms)
colnames(jms.colmeans)[-1] <- paste0('V',1:(ncol(jms)-1))

system.time(jms.colmeans <- jms.colmeans %>% group_by(pid) %>% summarise(
  V1=mean(V1),V2=mean(V2),V3=mean(V3),V4=mean(V4),V5=mean(V5),
  V6=mean(V6),V7=mean(V7),V8=mean(V8),V9=mean(V9),V10=mean(V10),
  V11=mean(V11),V12=mean(V12),V13=mean(V13),V14=mean(V14),V15=mean(V15),
  V16=mean(V16),V17=mean(V17),V18=mean(V18),V19=mean(V19),V20=mean(V20),
  V21=mean(V21),V22=mean(V22),V23=mean(V23),V24=mean(V24),V25=mean(V25)
))
rownames(jms.colmeans) <- jms.colmeans$pid
jms.colmeans <- as.data.table(jms.colmeans %>% select(-pid))
colnames(jms.colmeans) <- colnames(jms)[-1]
jms.saturation <- apply(jms.colmeans,2,function(i){
  apply(jms.colmeans,2,function(j){
    mean(!(is.na(i))&!(is.na(j)))
  })
})
jms.cor <- apply(jms.colmeans,2,function(i){
  apply(jms.colmeans,2,function(j){
    imcor(i,j)
  })
})
jms.corp <- apply(jms.colmeans,2,function(i){
  apply(jms.colmeans,2,function(j){
    imcor(i,j,test=T)
  })
})
jms.corp <- (jms.corp<=(0.05/ncol(jms.corp)/(ncol(jms.corp)-1)))
diag(jms.corp) <- FALSE

par(mfrow=c(1,2))
heatmap(1-jms.saturation,main='Saturation Matrix')
heatmap(jms.cor,main='Correlation Matrix')
plot(igraph::graph_from_adjacency_matrix(jms.corp,mode='undirected'),
     edge.arrow.size=.1,vertex.size=3,vertex.label.cex=1,edge.width=1)
data.table(saturation=cbind(diag(jms.saturation)),variable=colnames(jms.corp))

############################
# CROSS IDX PREDICTION
############################

# gc()
# mdata <- as.data.frame(jms)[,colnames(jms)%in%names(which(diag(jms.saturation)<1))]
# rdata <- as.data.table(as.data.frame(jms)[,-c(1,3)])
# save(mdata,rdata,imlms,file='4model.rda')
# mdata2 <- matrix(NA,nrow(mdata),ncol(mdata))
# for(k in 1:ncol(mdata2)){
#   gc()
#   print(k)
#   mdata2[,k] <- imlms(mdata[,k],rdata)
# }
# save(mdata2,file='mdata2_1.rda')
load('mdata2_1.rda')
mdata <- as.data.frame(jms)[,colnames(jms)%in%names(which(diag(jms.saturation)<1))]

mdata2.cor <- apply(mdata,2,function(i){
  apply(mdata2,2,function(j){
    imcor(i,j)
  })
})

jms <- as.data.frame(jms)


