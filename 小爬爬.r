library(XML)
library(RCurl)
get_aqi=function(date,file_dir=NULL){
  top="http://www.tianqihoubao.com/aqi/guangzhou-"
  bottom=".html"
  url=paste0(top,date,bottom)
  read_xml=function(url){
    wp=getURL(url,.encoding="gbk")
    doc<-htmlParse(wp,encoding="gbk")
    a=gsub(" |\r|\n","",iconv(sapply(getNodeSet(doc = doc, path = "//tr/td"), xmlValue),"UTF-8","gbk"))
    dim(a)=c(9,length(a)/9)
    a=a[,-1] #去除变量名
    a=a[-c(3,4),] #去除质量等级以及排名
    dim(a)=NULL
    return (a)
  }
  aqi=vector()
  for(i in 1:length(date)){
    aqi=c(aqi,read_xml(url[i]))
  }
  if(!is.null(file_dir))
    write(aqi,file_dir,ncolumns=length(aqi),sep=",")
  return(aqi)
}


get_weather=function(dayend=27,monthend=12,yearend=2015,begin_date="2013/11/1",file_dir=NULL){
  #抓取数据的开始时间是2013/11/1
  end_date=as.Date(paste(yearend,monthend,dayend,sep="/"))
  craw=function(dayend,monthend,yearend,begin_date){
    top=paste0("http://www.wunderground.com/history/airport/ZGGG/",begin_date,"/CustomHistory.html?")
    date_end=paste0("dayend=",dayend,"&monthend=",monthend,"&yearend=",yearend)
    bottom="&format=1"
    url=paste0(top,date_end,bottom)
    wheather=gsub("<br />","",getURL(url))
    wheather=substr(wheather,3,nchar(wheather)) #删除开头的换行符
  }
  if(as.Date(begin_date)+360-end_date>=0){
    wheather=craw(dayend,monthend,yearend,begin_date)
    if(!is.null(file_dir))
      write(wheather,file_dir)
    return(wheather)
  }
  else{
    tem=strsplit(as.character(as.Date(begin_date)+360),"-")[[1]]
    yearend_new=tem[1]
    monthend_new=tem[2]
    dayend_new=tem[3]
    wheather=craw(dayend_new,monthend_new,yearend_new,begin_date)
    wheather=substr(wheather,1,nchar(wheather)-2)
    begin_date=gsub("-","/",as.character(as.Date(begin_date)+361))
    while(as.Date(begin_date)+360-end_date<0){
      tem=strsplit(as.character(as.Date(begin_date)+360),"-")[[1]]
      yearend_new=tem[1]
      monthend_new=tem[2]
      dayend_new=tem[3]
      wheather_new=craw(dayend_new,monthend_new,yearend_new,begin_date)
      wheather=paste0(wheather,substr(wheather_new,386,nchar(wheather_new)-2))
      begin_date=gsub("-","/",as.character(as.Date(begin_date)+361))
    }
    tem=strsplit(as.character(as.Date(begin_date)+360),"-")[[1]]
    yearend_new=tem[1]
    monthend_new=tem[2]
    dayend_new=tem[3]
    wheather_new=craw(dayend,monthend,yearend,begin_date)
    wheather=paste0(wheather,substr(wheather_new,386,nchar(wheather_new)))
  }
  if(!is.null(file_dir))
    write(wheather,file_dir)
  return(wheather)
}

file1="C:/Users/Administrator/Desktop/广州.txt"
file2="C:/Users/Administrator/Desktop/广州气象数据.txt"
date=c('201311','201312','201401','201402','201403','201404','201405','201406'
       ,'201407','201408','201409','201410','201411','201412','201501','201502'
       ,'201503','201504','201505','201506','201507','201508','201509','201510'
       ,'201511','201512')
get_aqi(date,file1)
get_weather(dayend=27,monthend=12,yearend=2015,file_dir=file2)