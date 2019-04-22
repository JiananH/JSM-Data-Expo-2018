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
#moving window prediction, based on historical temperature data only
pred_dates <- seq(as.Date("2016-09-01"), as.Date("2017-09-08"), by = "day")
pred_datesnew <- pred_dates[-((length(pred_dates)-8):length(pred_dates))]

#Try out for max temperature
#113 is the number of locations
out=vector('list',length(pred_dates))
outout=vector('list',113)
for (i in 1:113){
  AirPTCd.cur=unique(clean.for.hist.loc.maxT$AirPtCd)[i]
  part=for.hist.loc.maxT[which(clean.for.hist.loc.maxT$AirPtCd==AirPTCd.cur),]
  part=na.omit(part)
  #NA.rows=which(!is.na(part))
  M.rows=which(part$Max_TemperatureF!='M')
  clean.part=part[M.rows,]
  clean.part=part[which(clean.part$daysfromforecast==0),]
  #mat=clean.part[,c(12,13)]
  if(dim(clean.part)[1]<1){
    next
  }
  for (j in seq_along(pred_datesnew)){
    hdata <- clean.part %>% filter(as.Date(Date) < pred_dates[j])
    fdata <- clean.part %>% filter(as.Date(Date) >= pred_dates[j] & as.Date(Date) < pred_dates[j+7])
    if(dim(hdata)[1]<1){
      next
    }
    mat=hdata[,c(3,12:28)]
    model=auto.arima(hdata$Max_TemperatureF,xreg=mat)
    value=forecast(model,h = 7)
    dateFOR=seq(pred_dates[j+1],pred_dates[j+7],by='day')
    dateON=rep(pred_dates[j],7)
    ID=rep(i,7)
    variable=rep("MaxTemp",7)
    out[[j]]=data.frame("cityID"=ID,"forcastFOR"=dateFOR,"value"=as.numeric(value$mean),"variable"=variable,"forecastON"=dateON)
  }
  outout[[i]]=do.call(rbind.data.frame,out)
}
final.maxT=do.call(rbind.data.frame,outout)

write.csv(final.maxT,"maxT.csv")

#min temp
for.hist.loc.minT=for.hist.loc[which(for.hist.loc$variable=="MinTemp"),]
NA.rows=which(is.na(for.hist.loc.minT$Min_TemperatureF))
M.rows=which(for.hist.loc.minT$value=='M')
clean.for.hist.loc.minT=for.hist.loc.minT[-union(NA.rows,M.rows),]
#moving window prediction, based on historical temperature data only
pred_dates <- seq(as.Date("2016-09-01"), as.Date("2017-09-08"), by = "day")
pred_datesnew <- pred_dates[-((length(pred_dates)-8):length(pred_dates))]

#Try out for min temperature
#113 is the number of locations
out=vector('list',length(pred_dates))
outout=vector('list',113)
for (i in 18:113){
  AirPTCd.cur=unique(clean.for.hist.loc.minT$AirPtCd)[i]
  part=for.hist.loc.minT[which(clean.for.hist.loc.minT$AirPtCd==AirPTCd.cur),]
  part=na.omit(part)
  #NA.rows=which(!is.na(part))
  M.rows=which(part$Min_TemperatureF!='M')
  clean.part=part[M.rows,]
  clean.part=part[which(clean.part$daysfromforecast==0),]
  if(dim(clean.part)[1]<1){
    next
  }
  #mat=clean.part[,c(12,13)]
  for (j in seq_along(pred_datesnew)){
    hdata <- clean.part %>% filter(as.Date(Date) < pred_dates[j])
    if(dim(hdata)[1]<1){
      next
    }
    model=auto.arima(hdata$Min_TemperatureF)
    value=forecast(model,h = 7)
    dateFOR=seq(pred_dates[j+1],pred_dates[j+7],by='day')
    dateON=rep(pred_dates[j],7)
    ID=rep(i,7)
    variable=rep("MinTemp",7)
    out[[j]]=data.frame("cityID"=ID,"forcastFOR"=dateFOR,"value"=as.numeric(value$mean),"variable"=variable,"forecastON"=dateON)
  }
  outout[[i]]=do.call(rbind.data.frame,out)
}
final.minT=do.call(rbind.data.frame,outout)

write.csv(final.minT,"minT.csv")

final=rbind(final.maxT,final.minT)
write.csv(final,"temp.csv")
