
rm(list=ls())
library(MatrixCompletion)
setwd('..')#set your working folder here
load('sample1.rda')

#Processing raw data to Journey data
system.time(jms <- lapply(unique(base$pid),jm))
jms <- do.call(rbind,jms);dim(jms)

#Journey data to Model data
mdata <- as.data.frame(jms)[,which(colSums(is.na(jms))>1)]
rdata <- as.data.table(as.data.frame(jms)[,-c(1,3)])
mdata2 <- apply(mdata,2,imlms,x=rdata)
fdata <- as.data.frame(jms)
fdata[,which(colSums(is.na(jms))>1)] <- mdata2

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
fdata2 <- scale(select(fdata,-pid,-trate))
data2.x <- qpca(fdata2,12)$X
colnames(data2.x) <- paste0('CS',1:ncol(data2.x))
data2.t <- fdata$trate
fdata2.x <- data.table(data2.x,trate=data2.t)
mdata2.x <- fdata2.x[sel2%in%sel1,]
model <- predict(MASS::lda(trate~.,data=fdata2.x))$class
fdata <- mutate(fdata,mrate=as.numeric(paste(model)))
fdata <- mutate(fdata,rate=ifelse(mrate > trate,mrate,trate))


