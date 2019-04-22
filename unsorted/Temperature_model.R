setwd("//eu.boehringer.com/users/rdg/users4/jhui/Documents/Conferences/JSM Data Challenge")

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

#min
for.hist.loc.minT=for.hist.loc[which(for.hist.loc$variable=="MinTemp"),]
NA.rows=which(is.na(for.hist.loc.minT$Min_TemperatureF))
M.rows=which(for.hist.loc.minT$value=='M')
clean.for.hist.loc.minT=for.hist.loc.minT[-union(NA.rows,M.rows),]

#Fit models
clean.for.hist.loc.maxT=subset(clean.for.hist.loc.maxT,daysfromforecast==0)
clean.for.hist.loc.minT=subset(clean.for.hist.loc.minT,daysfromforecast==0)
lm_model=lm(value~as.factor(AirPtCd)+daysfromforecast+Max_TemperatureF+Mean_TemperatureF+Min_TemperatureF+Max_Dew_PointF+MeanDew_PointF+Min_DewpointF+Max_Humidity+Min_Humidity,data=clean.for.hist.loc.maxT)
# lm_model_rd1=lm(value~as.factor(AirPtCd)+daysfromforecast+Max_TemperatureF+Mean_TemperatureF+Min_TemperatureF,data=clean.for.hist.loc.maxT)
# lm_model_rd2=lm(value~as.factor(AirPtCd)+daysfromforecast+Max_TemperatureF,data=clean.for.hist.loc.maxT)
# lm_model_rd3=lm(value~as.factor(AirPtCd)+Max_TemperatureF,data=clean.for.hist.loc.maxT)
lm_model_rd4=lm(value~Max_TemperatureF,data=clean.for.hist.loc.maxT)
lm_model_rd5=lm(value~Min_TemperatureF,data=clean.for.hist.loc.maxT)
summary(lm_model_rd4)
summary(lm_model_rd5)

#min
lm_model_rd6=lm(value~Max_TemperatureF,data=clean.for.hist.loc.minT)
lm_model_rd7=lm(value~Min_TemperatureF,data=clean.for.hist.loc.minT)
summary(lm_model_rd6)
summary(lm_model_rd7)


#Time series model
library(ggplot2)
library(forecast)
library(dplyr)
library(colortools)
head(clean.for.hist.loc.maxT)
class(clean.for.hist.loc.maxT$forecastFOR)
clean.for.hist.loc.maxT$forecastFOR=as.Date(clean.for.hist.loc.maxT$forecastFOR)
clean.for.hist.loc.maxT$forecastON=as.Date(clean.for.hist.loc.maxT$forecastON)
#clean.for.hist.loc.maxT$forecastFOR_m=format(clean.for.hist.loc.maxT$forecastFOR,)

#visual time series data by location
pdf("visual1.pdf")
for (i in 1:length(unique(clean.for.hist.loc.maxT$AirPtCd))){
  part=clean.for.hist.loc.maxT[which(clean.for.hist.loc.maxT$AirPtCd==unique(clean.for.hist.loc.maxT$AirPtCd)[i]),]
  q=ggplot(part, aes(x = forecastFOR, y = value)) +
    geom_point() +
    scale_x_date(date_labels = "%m", date_breaks = "1 month") + ggtitle(paste(unique(clean.for.hist.loc.maxT$AirPtCd)[i])) 
  print(q)
}
dev.off()

#transform to "ts" class
monthly_part_ts=ts(part$Max_TemperatureF,start=1,end=6755,freq=365)
monthly_part_stl=stl(monthly_part_ts,s.window="period")
plot(monthly_part_stl) 


pdf("visual2.pdf")
for (i in 1:length(unique(clean.for.hist.loc.maxT$AirPtCd))){
  part=clean.for.hist.loc.maxT[which(clean.for.hist.loc.maxT$AirPtCd==unique(clean.for.hist.loc.maxT$AirPtCd)[i]),]
  q=ggplot(part, aes(x = forecastFOR, y = value)) +
    geom_point() +
    stat_smooth(method = "lm",formula=y~x)+
    scale_x_date(date_labels = "%m", date_breaks = "1 month") + ggtitle(paste(unique(clean.for.hist.loc.maxT$AirPtCd)[i])) 
  print(q)
}
dev.off()

#fitting time series model
part=histWeather[which(histWeather$AirPtCd==unique(histWeather$AirPtCd)[i]),]
NA.rows=which(is.na(part$Max_TemperatureF))
M.rows=which(part$Max_TemperatureF=='M')
clean.part=part[-union(NA.rows,M.rows),]
mat=clean.part[,c(5,8)]
model=auto.arima(clean.part$Max_TemperatureF,xreg=mat)
forecast(model, xreg=c(1,2), h = 1)


#with prediction data
i=1
part=clean.for.hist.loc.maxT[which(clean.for.hist.loc.maxT$AirPtCd==unique(clean.for.hist.loc.maxT$AirPtCd)[i]),]
part=part[,c()]
part=na.omit(part)
#NA.rows=which(!is.na(part))
M.rows=which(part$Max_TemperatureF!='M')
clean.part=part[M.rows,]
clean.part=part[which(clean.part$daysfromforecast==0),]
mat=clean.part[,c(12,13)]
model=auto.arima(clean.part$Max_TemperatureF,xreg=mat)

temp=clean.for.hist.loc.maxT
temp=temp[which(temp$cityID==1),]
temp=temp[which(temp$daysfromforecast==0),]
temp$value=as.numeric(temp$value)
temp$PrecipitationIn=as.numeric(temp$PrecipitationIn)
temp$AirPtCd=as.factor(temp$AirPtCd)
temp=na.omit(temp)
M.rows=which(temp$Max_TemperatureF!='M')
clean.temp=temp[M.rows,]
mat=clean.temp[,c(3,12:28)]
model=auto.arima(clean.temp$Max_TemperatureF,xreg=mat)
mat_reduce=clean.temp[,3]
model.reduce=auto.arima(clean.temp$Max_TemperatureF,xreg=mat_reduce)


#moving window prediction, based on historical temperature data only
pred_dates <- seq(as.Date("2016-09-01"), as.Date("2017-09-01"), by = "day")

for (i in 1:113){
  part=for.hist.loc[which(clean.for.hist.loc.maxT$AirPtCd==unique(clean.for.hist.loc.maxT$AirPtCd)[i]),"Max_TemperatureF)"]
  part=na.omit(part)
  #NA.rows=which(!is.na(part))
  M.rows=which(part$Max_TemperatureF!='M')
  clean.part=part[M.rows,]
  #clean.part=part[which(clean.part$daysfromforecast==0),]
  #mat=clean.part[,c(12,13)]
  for (j in seq_along(pred_dates)){
    hdata <- clean.part %>% filter(as.Date(Date) < pred_dates[j])
  }
  model=auto.arima(clean.part$Max_TemperatureF)
  forecast(model,h = 7)
}
