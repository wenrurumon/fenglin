rm(list=ls())
library(data.table)
library(dplyr)
library(reshape2)
library(reshape)

setwd('E:/fenglin/')
load('jm1220.rda')
load('pdata.rda')
jm.sum <- sapply(jm,function(x){x[[1]]})
table(unlist(jm.sum[9,]))/ncol(jm.sum)
idx <- unique(pdata$idx$item)

# 0          1          2
# 0.69992487 0.21807097 0.08200416

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

imput <- function(x,y,pos=(0:100)/100){
  if(length(x)==0){
    return(rep(NA,101))
  } else if(length(x)==1){
    return(rep(y,101))
  } else {
    fun <- splinefun(x,y, method =  "monoH.FC")
    fun(pos)  
  }
}

#######################################
# 函数拟合
#######################################

library(qfcca)
library(GenABEL)
library(flare)
library(fda)
library(corpcor)

i <- 0
system.time(
  x.idx <- lapply(jm,function(jmi){
    print(i<<-i+1)
    jmi <- lapply(idx,function(i){
      jmi$journal %>% filter(status==i)
    })
    cbind(i,sapply(1:12,function(j){
      x <- jmi[[j]]$date
      y <- jmi[[j]]$note
      imput(x,y)
    }))
  })
)



# nbasis <- ceiling(length(pos))
# frange <- c(min(pos),max(pos))
# rlt <- list()
# cbasis <- create.fourier.basis
# phi <- eval.basis(pos,cbasis(frange,nbasis=nbasis))
# phi2 <- eval.basis((0:100)/100,cbasis(frange,nbasis=nbasis))
# x2 <- phi2 %*% myinv(t(phi) %*% phi) %*% t(phi) %*% cbind(x)


