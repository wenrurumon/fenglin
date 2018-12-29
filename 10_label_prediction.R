
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

mdata <- fdata[sel2%in%sel1,]
set.seed(12345); train <- sample(1:nrow(mdata))
train <- lapply(unique(cut(1:nrow(mdata),10)),function(i){
  train[cut(1:nrow(mdata),10)==i]
})

trate <- mdata$trate
mdata <- scale(select(mdata,-pid,-trate))
mdata.x <- qpca(mdata,12)$X; colnames(mdata.x) <- paste0('CS',1:ncol(mdata.x))
mdata.x <- cbind(mdata.x,trate=trate)

model <- sapply(1:10,function(i){
  print(i)
  traindf <- as.data.frame(mdata.x[train[[i]],])
  testdf <- as.data.frame(mdata.x[-train[[i]],])
  # modeli <- glm(trate~.,data=traindf)
  # predi <- predict(modeli,newdata=as.data.frame(mdata.x),type='response')
  modeli <- MASS::lda(trate~.,data=traindf)
  predi <- predict(modeli,newdata=as.data.frame(mdata.x))$class
  print(table(predi,trate))
  as.numeric(predi)-1
})
model <- rowMeans(model)
table(model=round(model,0),trate)
