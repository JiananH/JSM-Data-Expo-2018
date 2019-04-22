setwd("~/Dropbox/Side projects/JSM")

library(ggplot2)
library(hexbin)
library(data.table)

#forecast <- read.table("final.csv", sep = " ", stringsAsFactors=FALSE)
forecast <- final
names(forecast) <- c("cityID", "forecastFOR", "value", "variable", "forecastON")
locations <- read.csv("locations.csv", stringsAsFactors=FALSE)
locations$cityID <- 1:nrow(locations)
histWeather <- read.csv("histWeather.csv", stringsAsFactors=FALSE)
forecast$daysfromforecast = as.numeric(forecast$forecastFOR) - as.numeric(forecast$forecastON)

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


#delete 'NA', 'M', and incorrect entries 
NA.rows=which(is.na(for.hist.loc.maxT$Max_TemperatureF))
M.rows=which(for.hist.loc.maxT$value=='M')
clean.for.hist.loc.maxT=for.hist.loc.maxT[-union(NA.rows, M.rows),]
###################################
## cleaning out the hist temps that didn't make sense (Greg; 5/16/2018)
erroneous.rows <- which(clean.for.hist.loc.maxT$Max_TemperatureF > 200)
clean.for.hist.loc.maxT=clean.for.hist.loc.maxT[-erroneous.rows,]

## adding code for Min Temp (Greg; 5/16/2018)
#MinTemp
for.minT=forecast[which(forecast$variable=="MinTemp"),]
hist.loc.minT=hist.loc[,c("Min_TemperatureF","AirPtCd","cityID","forecastFOR")]
for.hist.loc.minT=merge(x=for.minT,y=hist.loc.minT,by=c("cityID","forecastFOR"),all.x=T)


#delete 'NA' and 'M'
NA.rows=which(is.na(for.hist.loc.minT$Min_TemperatureF))
M.rows=which(for.hist.loc.minT$value=='M')
clean.for.hist.loc.minT=for.hist.loc.minT[-union(NA.rows,M.rows),]



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


################
## testing out different plot options (Greg; 5/4/2018)



## make a data.table
tempTable <- data.table(clean.for.hist.loc.maxT)
## bin temperatures
tempTable <- tempTable[,c("forecastBin", "histBin"):=list(floor(as.numeric(value)/3)*2.5 +1.25,floor(Max_TemperatureF/3)*2.5 +1.25)]
## get counts for each bin combination
tempCounts <- tempTable[,.N,by = .(forecastBin,histBin, daysfromforecast)]
tempCounts2 <- tempTable[,.N,by = .(value = as.numeric(value),Max_TemperatureF, daysfromforecast)]



bubble1 <- ggplot(tempCounts[daysfromforecast<=6 & daysfromforecast >=0], aes(forecastBin, histBin))+ 
  geom_point(aes( size = N, color = N)) + 
  ylim(-10,110) + 
  facet_wrap(~daysfromforecast, nrow =1) + 
  scale_colour_gradient(low = "blue", high = "red")
bubble1
ggsave("bubblePlot.pdf",bubble1)


heatmap1 <- ggplot(tempCounts2[daysfromforecast<=6 & daysfromforecast >=0 & Max_TemperatureF <120], aes(value, Max_TemperatureF))+
  stat_density_2d(aes(fill = ..level..),geom = "polygon") +
  facet_wrap(~daysfromforecast, nrow =1) + 
  scale_fill_gradient(low = "blue", high = "red")
heatmap1
ggsave("densityPlot.pdf", heatmap1)

hex1 <- ggplot(tempCounts2[daysfromforecast<=6 & daysfromforecast >=0 & Max_TemperatureF <120], aes(value, Max_TemperatureF))+
  scale_fill_gradient(low = "blue", high = "red")+
  facet_wrap(~daysfromforecast, nrow =1) + 
  geom_hex() 
hex1
ggsave("hexPlot.pdf", hex1)


#######################
## Making Max and Min temp bubble plots (Greg; 5/16/2018)
nHistBins <- 20
nForecastBins <- 30

## make a data.table for max temp
maxTempTable <- data.table(clean.for.hist.loc.maxT)
## make sequence of breaks for hist and forecast temps
histBreaks <- seq(min(maxTempTable$Max_TemperatureF), max(maxTempTable$Max_TemperatureF), length = nHistBins)
forecastBreaks <- seq(min(as.numeric(maxTempTable$value)), max(as.numeric(maxTempTable$value)), length = nForecastBins)
## bin temperatures
maxTempTable <- maxTempTable[,c("forecastBinLABEL"):=findInterval(as.numeric(value), forecastBreaks)]
maxTempTable <- maxTempTable[,c("histBinLABEL"):=findInterval(Max_TemperatureF, histBreaks)]
#maxTempTable <- maxTempTable[,c("forecastBin", "histBin"):=list(floor(as.numeric(value)/3)*2.5 +1.25,floor(Max_TemperatureF/3)*2.5 +1.25)]
## Label Bin temps
maxTempTable$forecastBin <- ((forecastBreaks[-1] + forecastBreaks[-(nForecastBins)])/2)[maxTempTable$forecastBinLABEL]
maxTempTable$histBin <- ((histBreaks[-1] + histBreaks[-(nHistBins)])/2)[maxTempTable$histBinLABEL]
## get counts for each bin combination
maxTempCounts <- maxTempTable[,.N,by = .(forecastBin,histBin, daysfromforecast)]
maxTempCounts$daysfromforecastLABEL <- paste0(maxTempCounts$daysfromforecast, " Days")



## make a data.table for min temp
minTempTable <- data.table(clean.for.hist.loc.minT)
## make sequence of breaks for hist and forecast temps
histBreaks <- seq(min(minTempTable$Min_TemperatureF), max(minTempTable$Min_TemperatureF), length = nHistBins)
forecastBreaks <- seq(min(as.numeric(minTempTable$value)), max(as.numeric(minTempTable$value)), length = nForecastBins)
## bin temperatures
minTempTable <- minTempTable[,c("forecastBinLABEL"):=findInterval(as.numeric(value), forecastBreaks)]
minTempTable <- minTempTable[,c("histBinLABEL"):=findInterval(Min_TemperatureF, histBreaks)]
#minTempTable <- minTempTable[,c("forecastBin", "histBin"):=list(floor(as.numeric(value)/3)*2.5 +1.25,floor(Min_TemperatureF/3)*2.5 +1.25)]
## Label Bin temps
minTempTable$forecastBin <- ((forecastBreaks[-1] + forecastBreaks[-(nForecastBins)])/2)[minTempTable$forecastBinLABEL]
minTempTable$histBin <- ((histBreaks[-1] + histBreaks[-(nHistBins)])/2)[minTempTable$histBinLABEL]
## get counts for each bin combination
minTempCounts <- minTempTable[,.N,by = .(forecastBin,histBin, daysfromforecast)]
minTempCounts$daysfromforecastLABEL <- paste0(minTempCounts$daysfromforecast, " Days")



## make bubble plots
legendTitle <- "Count"

# max Temp
maxTempBubble <- ggplot(maxTempCounts[daysfromforecast<=6 & daysfromforecast >=0], aes(histBin, forecastBin))+ 
  geom_point(aes( size = N, color = N)) + 
  #ylim(-10,110) + xlim(-10,110) +
  facet_wrap(~daysfromforecastLABEL, nrow =1) + 
  scale_colour_gradient(low = "blue", high = "red", name = legendTitle) +
  scale_size(name = legendTitle) +
  xlab("Actual Max Temp") + ylab("Predicted Max Temp") +
  theme(axis.text.x = element_text(angle = -45, hjust = 0.05))  +
  geom_abline(intercept = 1, slope =1)
maxTempBubble

ggsave("Max_Temp.pdf",maxTempBubble, width = 7, height = 7/2)

# min temp
minTempBubble <- ggplot(minTempCounts[daysfromforecast<=6 & daysfromforecast >=0], aes(histBin, forecastBin))+ 
  geom_point(aes( size = N, color = N)) + 
  #ylim(-26,80) + xlim(-100,90) +
  facet_wrap(~daysfromforecastLABEL, nrow =1) + 
  scale_colour_gradient(low = "blue", high = "red", name = legendTitle) +
  scale_size(name = legendTitle) +
  xlab("Actual Min Temp") + ylab("Predicted Min Temp")+
  theme(axis.text.x = element_text(angle = -45, hjust = 0.05)) +
  geom_abline(intercept = 1, slope =1)
minTempBubble



ggsave("Min_Temp.pdf",minTempBubble, width = 7, height = 7/2)

