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


#MaxTemp
for.maxT=forecast[which(forecast$variable=="MaxTemp"),]
hist.loc.maxT=hist.loc[,c("Max_TemperatureF","AirPtCd","cityID","forecastFOR")]
for.hist.loc.maxT=merge(x=for.maxT,y=hist.loc.maxT,by=c("cityID","forecastFOR"),all.x=T)


#delete 'NA' and 'M'
NA.rows=which(is.na(for.hist.loc.maxT$Max_TemperatureF))
M.rows=which(for.hist.loc.maxT$value=='M')
clean.for.hist.loc.maxT=for.hist.loc.maxT[-union(NA.rows,M.rows),]

len=7
par(mfrow=c(1,len))

#length(unique(for.hist.loc.maxT$daysfromforecast))
for (i in 1:len){
  sub=clean.for.hist.loc.maxT[clean.for.hist.loc.maxT$daysfromforecast==unique(clean.for.hist.loc.maxT$daysfromforecast)[i],]
  
  # plot(sub$cityID,sub$Max_TemperatureF,col='blue',ylim=c(-200,200),pch='.')
  # lines(sub$cityID,sub$value,col='red',pch='.')
  
  subsub=na.omit(sub)
  plot(subsub$Max_TemperatureF,subsub$value,xlim=c(-10,120),ylim=c(-10,120),pch='.',type='n',xlab="Actual",ylab='Predicted',main=paste("Predicted ", i-1," days away", sep = ""))
  points(subsub$Max_TemperatureF,subsub$value,pch='.')
  abline(c(0,0),c(1,1),col='red')
  
}