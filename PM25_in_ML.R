setwd("C:/Users/Administrator/Desktop")
#tmpvec<-scan("广州历史天气查询.txt",what="",sep=",");
#t=data.frame(matrix(tmpvec[-length(tmpvec)],ncol=4,byrow=T),"城市"="广州",stringsAsFactors=F)
#colnames(t)<-c("日期","天气","温度","风向","城市")
library(plyr)
library("sqldf")
library(lars)
library(nnet)
library(e1071)
library(randomForest)


lag=function(data,n){
  col.lay=function(d,name,n){
    label=1:n
    col.name=paste(name,label,sep ="-")
    ret=vector()
    l=length(d)
    for(i in 1:n){
      tem=c(rep(NA,i),d[1:(l-i)])
      ret=cbind(ret,tem)
    }
    colnames(ret)=col.name
    return(ret)
  }
  nc=ncol(data)
  data.lag=col.lay(data[,1],colnames(data)[1],n)
  for(i in 2:nc){
    data.lag=cbind(data.lag,col.lay(data[,i],colnames(data)[i],n))
  }
  return(as.data.frame(data.lag))
}

tmpvec2<-scan("广州.txt",what="",sep=",");
t2=data.frame(matrix(tmpvec2[-length(tmpvec2)],ncol=7,byrow=T),"城市"="广州",stringsAsFactors=F)
colnames(t2)=c("日期","AQI指数","PM2.5","PM10","Co","No2","So2","城市")
for(i in 2:7)
{
  t2[,i]<-as.numeric(t2[,i])
}

dt1<-read.table("广州气象数据.txt",sep=",",head=T)
dt1[,1]<-as.character(as.Date(dt1[,1]),format="%Y-%m-%d")
colnames(dt1)<-c("日期",colnames(dt1)[-1])

dt_merge<-merge(dt1,t2,by="日期",stringsAsFactors=F)
dt_merge<-data.frame(dt_merge[,-c(19,21,22,30)])

x<-as.matrix(lag(dt_merge[,colnames(dt_merge)!="PM2.5" & colnames(dt_merge)!="日期"],7)) #延迟7阶
y<-as.matrix(dt_merge$PM2.5)
index<-complete.cases(x)
x<-x[index,]
y<-y[index]
sizex<-nrow(x)
##index<-sample(1:sizex,sizex*1/2)
index<-c(1:(3/4*sizex))
trainx<-x[index,]
trainy<-y[index]
testx<-x[-index,]
testy<-y[-index]

laa<-lars(trainx,trainy)
plot(laa)
summary(laa)
cva<-cv.lars(trainx,trainy,K=10)
best=cva$index[which.min(cva$cv)]
coef=coef.lars(laa,mode="fraction",s=best)
Cp.min=which(laa$Cp==min(laa$Cp))
coef1=coef.lars(laa,mode="step",s=Cp.min)
pre<-predict(laa,newx=data.frame(testx),mode="step",s=Cp.min)
plot(data.frame(pre$fit,testy))
plot(testy,type="l")
lines(pre$fit,col="red")

#####模型乱试
nnet.sol<-nnet(trainx,trainy,size=5,rang=0.1,decay=5e-6,linout=T)
nnet.prd<-predict(nnet.sol,testx)
#plot(cbind(pre,testy))
plot(testy,type="l")
lines(pre$fit,col="red")

randomForest.sol<-randomForest(trainx,trainy)
randomForest.prd<-predict(randomForest.sol,testx)
plot(data.frame(randomForest.prd,testy))
plot(testy,type="l")
lines(pre$fit,col="red")

svm.sol<-svm(trainx,trainy)
svm.prd<-predict(svm.sol,testx)
plot(data.frame(svm.prd,testy))
plot(testy,type="l")
lines(pre$fit,col="red")

ap=plsr(trainy~trainx,validation="CV")
ap$loadings
ap$coef
validationplot(ap)
RMSEP(ap)
MSEP(ap)
R2(ap)
py<-predict(ap,newdata=testx,comps=1:8)
plot(py,testy)
plot(py,type="l",col="red")
lines(testy)

