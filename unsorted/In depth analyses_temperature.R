#In-depth analyses on temperature 
library(dplyr)
library(lme4)
#library(tidyverse)


#Read in multiple datasets
setwd("~/Dropbox/Side projects/JSM/data")
locations <- read.csv("locations.csv", stringsAsFactors=FALSE)
histWeather <- read.csv("histWeather.csv", stringsAsFactors=FALSE)
forecast <- read.table("forecast.dat", sep = " ", stringsAsFactors=FALSE)

#Format datasets
locations$cityID <- 1:nrow(locations)
names(forecast) <- c("cityID", "forecastFOR", "value", "variable", "forecastON")
forecast$daysfromforecast = as.numeric(as.Date(forecast$forecastFOR)) - as.numeric(as.Date(forecast$forecastON))

#merge histWeather and locations 
hist.new=histWeather[order(histWeather$AirPtCd),]
locations.new=locations[order(locations$AirPtCd),]
hist.loc=merge(x=hist.new,y=locations.new,by="AirPtCd",all.x=T)
hist.loc$forecastFOR=hist.loc$Date
for.hist.loc=merge(x=forecast,y=hist.loc,by=c("cityID","forecastFOR"),all.x=T)

#subset for maxT
for.hist.loc.maxT=for.hist.loc[which(for.hist.loc$variable=="MaxTemp"),]
NA.rows=which(is.na(for.hist.loc.maxT$Max_TemperatureF))
M.rows=which(for.hist.loc.maxT$value=='M')
clean.for.hist.loc.maxT=for.hist.loc.maxT[-union(NA.rows,M.rows),]

#subset for minT
for.hist.loc.minT=for.hist.loc[which(for.hist.loc$variable=="MinTemp"),]
NA.rows=which(is.na(for.hist.loc.minT$Min_TemperatureF))
M.rows=which(for.hist.loc.minT$value=='M')
clean.for.hist.loc.minT=for.hist.loc.minT[-union(NA.rows,M.rows),]

#check distribution of datasets
table(for.hist.loc.maxT$daysfromforecast)
table(for.hist.loc.minT$daysfromforecast)

unique(for.hist.loc$value)
length(sum(is.na(for.hist.loc$value)))
length(sum(for.hist.loc$value=='M'))


#Start the models
significance=function(x){
  test<- summary(x)$coefficients
  betaHat <- test[2,1]
  betaSE <- test[2,2]
  t <- (betaHat-1)/ betaSE
  pVal <- pnorm(abs(t),lower.tail = FALSE)*2
  return(pVal)
}

#with intercept
max.slope=min.slope=double(7)
for (i in 1:7){
  cleaned.maxT=subset(clean.for.hist.loc.maxT,daysfromforecast==i-1)
  cleaned.minT=subset(clean.for.hist.loc.minT,daysfromforecast==i-1)
  lm_model_1=lm(Max_TemperatureF~as.numeric(value),data=cleaned.maxT)
  print(i-1)
  print(summary(lm_model_1))
  max.slope[i]=significance(lm_model_1)
  lm_model_4=lm(Min_TemperatureF~as.numeric(value),data=cleaned.minT)
  print(summary(lm_model_4))
  min.slope[i]=significance(lm_model_4)
}

#without intercept
max.wo.slope=min.wo.slope=double(7)
for (i in 1:7){
  cleaned.maxT=subset(clean.for.hist.loc.maxT,daysfromforecast==i-1)
  cleaned.minT=subset(clean.for.hist.loc.minT,daysfromforecast==i-1)
  lm_model_1=lm(Max_TemperatureF~-1+as.numeric(value),data=cleaned.maxT)
  print(i-1)
  print(summary(lm_model_1))
  max.wo.slope[i]=pnorm(abs((coef(lm_model_1)-1)/summary(lm_model_1)$coefficients[2]),lower.tail = FALSE)*2
  lm_model_4=lm(Min_TemperatureF~-1+as.numeric(value),data=cleaned.minT)
  print(summary(lm_model_4))
  min.wo.slope[i]=pnorm(abs((coef(lm_model_4)-1)/summary(lm_model_4)$coefficients[2]),lower.tail = FALSE)*2
}


#location identification
lm_loc_minT=lm_loc_maxT=vector('list',7)
for (i in 1:7){
cleaned.minT=subset(clean.for.hist.loc.minT,daysfromforecast==i-1)
lm_loc_minT[[i]]=lmList(Min_TemperatureF~as.numeric(value)|AirPtCd,data=cleaned.minT)
cleaned.maxT=subset(clean.for.hist.loc.maxT,daysfromforecast==i-1)
lm_loc_maxT[[i]]=lmList(Max_TemperatureF~as.numeric(value)|AirPtCd,data=cleaned.maxT)
}


#Binning temperature and see which parts perform well
lm_temp_minT=lm_temp_maxT=vector('list',7)
for (i in 1:7){
  cleaned.minT=subset(clean.for.hist.loc.minT,daysfromforecast==i-1)
  lm_temp_minT[[i]]=lmList(Min_TemperatureF~as.numeric(value)|as.factor(as.numeric(value)),data=cleaned.minT)
  cleaned.maxT=subset(clean.for.hist.loc.maxT,daysfromforecast==i-1)
  lm_temp_maxT[[i]]=lmList(Max_TemperatureF~as.numeric(value)|as.factor(as.numeric(value)),data=cleaned.maxT)
}

#Build a model together with data from previous day can already improve prediction
