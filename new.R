tmpvec<-scan("广州历史天气查询.txt",what="",sep=",");
t=data.frame(matrix(tmpvec[-length(tmpvec)],ncol=4,byrow=T),"城市"="广州",stringsAsFactors=F)
colnames(t)<-c("日期","天气","温度","风向","城市")

tmpvec2<-scan("广州.txt",what="",sep=",");
t2=data.frame(matrix(tmpvec2[-length(tmpvec2)],ncol=7,byrow=T),"城市"="广州",stringsAsFactors=F)
colnames(t2)=c("日期","AQI指数","PM2.5","PM10","Co","No2","So2","城市")
for(i in 2:7)
{
  t2[,i]<-as.numeric(t2[,i])
}

dt1<-read.table("广州气象数据.txt",sep=",",head=T)
dt1[,1]<-as.character(as.Date(dt1$CST),format="%Y-%m-%d")
colnames(dt1)<-c("日期",colnames(dt1)[-1])

dt_merge<-merge(dt1,t2,by="日期",stringsAsFactors=F)
dt_merge<-data.frame(dt_merge[,-c(19,21,22,30)])

x<-as.matrix(dt_merge[,c(2:ncol(dt_merge))])
y<-as.matrix(dt_merge$PM2.5[-1])
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
testx<-testx[-nrow(testx),]
testy<-testy[-length(testy)]

##svm
tag_t<-function(y)
{
	std_value<-c(51,101,151,201,301)
	##std_value<-c(75,150)
	for(i in 1:length(std_value))
		if(y < std_value[i]) return(i)
	return(i+1)
}

kmc_test<-sapply(testy, tag_t)
kmc_train<-sapply(trainy, tag_t)

library(e1071)
kmcy<-cbind(kmc_train, trainx)
svmc<-svm(kmc_train~.,data=kmcy, cost = 22,type="C-classification")
table(predict(svmc), kmc_train)
pred<-predict(svmc, testx)
table(kmc_test,pred)