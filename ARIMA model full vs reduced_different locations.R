#Compare full model with reduced model

library(ggplot2)
library(xts)
library(forecast)
library(dplyr)
library(colortools)

setwd("~/Dropbox/Side projects/JSM/data")
#setwd("//eu.boehringer.com/users/rdg/users4/jhui/Documents/Conferences/JSM Data Challenge")
locations <- read.csv("locations.csv", stringsAsFactors=FALSE)
histWeather <- read.csv("histWeather.csv", stringsAsFactors=FALSE)
forecast <- read.table("forecast.dat", sep = " ", stringsAsFactors=FALSE)

## to do list 1
locations$cityID <- 1:nrow(locations)

names(forecast) <- c("cityID", "forecastFOR", "value", "variable", "forecastON")

## to do list 2
forecast$daysfromforecast = as.numeric(as.Date(forecast$forecastFOR)) - as.numeric(as.Date(forecast$forecastON))

#merge histWeather and locations 
hist.new=histWeather[order(histWeather$AirPtCd),]
locations.new=locations[order(locations$AirPtCd),]
hist.loc=merge(x=hist.new,y=locations.new,by="AirPtCd",all.x=T)
hist.loc$forecastFOR=hist.loc$Date
for.hist.loc=merge(x=forecast,y=hist.loc,by=c("cityID","forecastFOR"),all.x=T)
for.hist.loc.maxT=for.hist.loc[which(for.hist.loc$variable=="MaxTemp"),]
NA.rows=which(is.na(for.hist.loc.maxT$Max_TemperatureF))
M.rows=which(for.hist.loc.maxT$value=='M')
clean.for.hist.loc.maxT=for.hist.loc.maxT[-union(NA.rows,M.rows),]

out=out.reduce=vector('list',113)
for (i in 1:113){
  temp=clean.for.hist.loc.maxT
  temp=temp[which(temp$cityID==i),]
  temp=temp[which(temp$daysfromforecast==0),]
  temp$value=as.numeric(temp$value)
  temp$PrecipitationIn=as.numeric(temp$PrecipitationIn)
  temp$AirPtCd=as.factor(temp$AirPtCd)
  temp=na.omit(temp)
  M.rows=which(temp$Max_TemperatureF!='M')
  clean.temp=temp[M.rows,]
  if (dim(clean.temp)[1]<1){
    next
  }
  mat=clean.temp[,c(3,12:28)]
  tryCatch({
    model=auto.arima(clean.temp$Max_TemperatureF,xreg=mat)
      }, error=function(e){}
    )

  mat_reduce=clean.temp[,3]
  tryCatch({
  model.reduce=auto.arima(clean.temp$Max_TemperatureF,xreg=mat_reduce)
  }, error=function(e){}
  )
  out[[i]]=summary(model)
  out.reduce[[i]]=summary(model.reduce)
}
final.max=do.call(rbind.data.frame,out)
final.max.reduce=do.call(rbind.data.frame,out.reduce)

write.csv(final.max,"error_max.csv")
write.csv(final.max.reduce,"error_max_reduce.csv")

#minTemp
for.hist.loc.minT=for.hist.loc[which(for.hist.loc$variable=="MinTemp"),]
NA.rows=which(is.na(for.hist.loc.minT$Min_TemperatureF))
M.rows=which(for.hist.loc.minT$value=='M')
clean.for.hist.loc.minT=for.hist.loc.minT[-union(NA.rows,M.rows),]

out=out.reduce=vector('list',113)
for (i in 1:113){
  temp=clean.for.hist.loc.minT
  temp=temp[which(temp$cityID==i),]
  temp=temp[which(temp$daysfromforecast==0),]
  temp$value=as.numeric(temp$value)
  temp$PrecipitationIn=as.numeric(temp$PrecipitationIn)
  temp$AirPtCd=as.factor(temp$AirPtCd)
  temp=na.omit(temp)
  M.rows=which(temp$Min_TemperatureF!='M')
  clean.temp=temp[M.rows,]
  if (dim(clean.temp)[1]<1){
    next
  }
  mat=clean.temp[,c(3,12:28)]
  tryCatch({
    model=auto.arima(clean.temp$Min_TemperatureF,xreg=mat)
  }, error=function(e){}
  )
  
  mat_reduce=clean.temp[,3]
  tryCatch({
    model.reduce=auto.arima(clean.temp$Min_TemperatureF,xreg=mat_reduce)
  }, error=function(e){}
  )
  out[[i]]=summary(model)
  out.reduce[[i]]=summary(model.reduce)
}
final.min=do.call(rbind.data.frame,out)
final.min.reduce=do.call(rbind.data.frame,out.reduce)

write.csv(final.min,"error_min.csv")
write.csv(final.min.reduce,"error_min_reduce.csv")