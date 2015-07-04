#Created by SK Pan 2015/07#

#初始化載入套件bnlearn相關套件#
initialBnlearn <-function(){
  library(bnlearn)
  library(Rgraphviz)
}

#尋找資料在第幾個Interval#
findIntervalIndex <- function (IntervalSet,Data){
  RightHandLimitSet<- as.numeric( sub("[^,]*,([^]]*)\\]", "\\1", levels(IntervalSet)))
  return (findInterval(Data,RightHandLimitSet, all.inside=TRUE)+1)
}

#查詢資料屬於怎樣的state#
queryState <- function (IntervalSet,Data){
  return (levels(IntervalSet)[findIntervalIndex(IntervalSet = IntervalSet,Data = Data)])
}

#將輸入的證據轉成離散型資料的list給 likelihood weight sampling使用 更改$運算元後屬性即可套用其他應用
inputEvidence <- function(IntervalSet,E1,E2,E3,E4){
  ES1<-queryState(IntervalSet$Hour,E1)
  ES2<-queryState(IntervalSet$WeatherTemp,E2)
  ES3<-queryState(IntervalSet$RelatedHum,E3)
  ES4<-queryState(IntervalSet$WindSpeed,E4)
  return (list(Hour=ES1,WeatherTemp=ES2,RelatedHum=ES3,WindSpeed=ES4))
}
