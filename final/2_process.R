rm(list=ls())
library(data.table)
library(dplyr)
# library(openxlsx)
# library(lubridate)
# library(fda)
# library(flare)
# library(corpcor)

############################
# Descriptive Summary
load('jms_20190119.rda')
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
  V21=mean(V21),V22=mean(V22),V23=mean(V23),V24=mean(V24),V25=mean(V25),
  V26=mean(V26)
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
jms.corp <- (jms.corp<=(0.05/ncol(jms.corp)/(ncol(jms.corp))))
diag(jms.corp) <- FALSE

heatmap(1-jms.saturation,main='Saturation Matrix')
heatmap(jms.cor,main='Correlation Matrix')
plot(igraph::graph_from_adjacency_matrix(jms.corp,mode='undirected'),
     edge.arrow.size=.1,vertex.size=3,vertex.label.cex=1,edge.width=1)
data.table(saturation=cbind(diag(jms.saturation)),variable=colnames(jms.corp))

############################
# CROSS IDX PREDICTION
############################

gc()
#TEN FOLDER CROSS IDX PREDICTION
mdata <- as.data.frame(jms)[,colnames(jms)%in%names(which(diag(jms.saturation)<1))]
rdata <- as.data.table(as.data.frame(jms)[,-c(1,3)])
set.seed(12345);train <- sample(pid)
train <- lapply(unique(cut(1:length(train),10)),function(i){
  train[cut(1:length(train),10)==i]
})
system.time(
  model <- lapply(1:10,function(k){
    print(k)
    mdatak <- mdata[jms$pid%in%train[[k]],]
    rdatak <- rdata[jms$pid%in%train[[k]],]
    apply(mdatak,2,function(i){
      apply(rdatak,2,function(j){
        s <- !(is.na(i)|is.na(j))
        j <- cbind(1,j[s])
        i <- cbind(i[s])
        s <- solve(t(j)%*%j)%*%t(j) %*% cbind(i)
        s
      })
    })
  })
)
system.time(
  modelk <- lapply(model,function(x){
    sapply(1:16,function(k){
      k <- x[,k]
      rowMeans(do.call(cbind,lapply(1:12,function(i){
        k[i*2]+rdata[,i,with=F]
      })),na.rm=T)
    })
  })
)
system.time(
  modelk.cor <- do.call(cbind,lapply(modelk,function(modelki){
    sapply(1:16,function(k){
      c(imcor(mdata[,k],modelki[,k]))
    })
  }))
)
rownames(modelk.cor) <- colnames(mdata)
gc()
mdata2 <- modelk[[1]]
for(i in 2:10){mdata2 <- mdata2 + modelk[[i]]}
mdata2 <- mdata2/10
modelk.cor <- cbind(modelk.cor,sapply(1:16,function(i){
  imcor(mdata[,i],mdata2[,i])
}))
mdata2[!is.na(mdata)] <- mdata[!is.na(mdata)]
modelk.cor <- cbind(modelk.cor,sapply(1:16,function(i){
  imcor(mdata[,i],mdata2[,i])
}))

############################
# Final Data Set
############################

fdata <- as.data.frame(jms)
fdata[,colnames(jms)%in%names(which(diag(jms.saturation)<1))] <- mdata2

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
mdata <- fdata[sel2%in%sel1,]

write.csv(fdata,'fdata.temp',fileEncoding='UTF-8')
write.csv(rate,'rate.temp',fileEncoding='UTF-8')
write.csv(idx,'idx.temp',fileEncoding='UTF-8')
write.csv(exe,'exe.temp',fileEncoding='UTF-8')
write.csv(mdata,'mdata.temp',fileEncoding='UTF-8')
