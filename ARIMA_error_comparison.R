library(readr)
library(reshape)
library(ggplot2)

#Read in data
setwd("~/Dropbox/Side projects/JSM/data")
error_max <- read_csv("error_max.csv")
error_min <- read_csv("error_min.csv")
error_max_reduce <- read_csv("error_max_reduce.csv")
error_min_reduce <- read_csv("error_min_reduce.csv")

setwd("~/Dropbox/Side projects/JSM/output")
# library(sm)
# boxplot(error_max$ME)
# 
# hist(error_max$ME)
# hist(error_max_reduce$ME)

#taking out non-fit models
error_max=error_max[-(76:78),]
error_max_reduce=error_max_reduce[-(76:78),]

error_min=error_min[-(76:78),]
error_min_reduce=error_min_reduce[-(76:78),]

# data=data.frame("error"=c(error_max$MAE,error_max_reduce$MAE),"label"=c(rep(1,dim(error_max)[1]),rep(2,dim(error_max_reduce)[1])))
# sm.density.compare(data$error,data$label,xlab="Full vs. Reduced",col=c('royalblue2',"salmon"),lty=c(1,1),lwd=2,title="MAE_Max_Temp")

#median summary
summary_max=apply(error_max[,-1],2,median)[-c(1,4,7)]
summary_max_reduce=apply(error_max_reduce[,-1],2,median)[-c(1,4,7)]
summary_min=apply(error_min[,-1],2,median)[-c(1,4,7)]
summary_min_reduce=apply(error_min_reduce[,-1],2,median)[-c(1,4,7)]

data=data.frame("maxTemp_original"=summary_max,"maxTemp_updated"=summary_max_reduce,"minTemp_original"=summary_max,"minTemp_updated"=summary_min_reduce)

ggplot(data, aes(1:4)) + 
  geom_line(aes(y = maxTemp_updated, colour = "royalbule2")) + 
  geom_line(aes(y = maxTemp_original, colour = "salmon")) + xlab(c("RMSE","MAE","MAPE","MASE"))


#boxplot for MaxTemp
error_max$class=rep(1,dim(error_max)[1])
error_max_reduce$class=rep(2,dim(error_max_reduce)[1])
sum_max=rbind(error_max,error_max_reduce)
#final=data.frame("value"=c(sum_max$ME,sum_max$RMSE,sum_max$MAE,sum_max$MPE,sum_max$MAPE,sum_max$MASE),"error.measure"=c(rep("ME",216),rep("RMSE",216),rep("MAE",216),rep("MPE",216),rep("MAPE",216),rep("MASE",216)),"class"=rep(c(rep("Updated",108),rep("Original",108)),6))

final=data.frame("value"=c(sum_min$RMSE,sum_min$MAE,sum_min$MAPE,sum_min$MASE),"error.measure"=c(rep("Root Mean Sqaured Error",216),rep("Mean Absolute Error",216),rep("Mean Absolute Percentage Error",216),rep("Mean Absolute Scaled Error",216)),"class"=rep(c(rep("Updated",108),rep("Original",108)),4))

df=melt(final,id.vars=c("value","error.measure","class"))
p <- ggplot(df, aes(error.measure, value,fill=class))
p + geom_boxplot() + labs(title = "Error comparison between original model and updated model_Max Temperature")

#boxplot for MinTemp
error_min$class=rep(1,dim(error_min)[1])
error_min_reduce$class=rep(2,dim(error_min_reduce)[1])
sum_min=rbind(error_min,error_min_reduce)
#final=data.frame("value"=c(sum_min$ME,sum_min$RMSE,sum_min$MAE,sum_min$MPE,sum_min$MAPE,sum_min$MASE),"error.measure"=c(rep("ME",216),rep("RMSE",216),rep("MAE",216),rep("MPE",216),rep("MAPE",216),rep("MASE",216)),"class"=rep(c(rep("Updated",108),rep("Original",108)),6))

final1=data.frame("value"=c(sum_min$RMSE,sum_min$MAE,sum_min$MAPE,sum_min$MASE),"error.measure"=c(rep("Root Mean Sqaured Error",216),rep("Mean Absolute Error",216),rep("Mean Absolute Percentage Error",216),rep("Mean Absolute Scaled Error",216)),"class"=rep(c(rep("Updated",108),rep("Original",108)),4))
library(reshape)
df=melt(final1,id.vars=c("value","error.measure","class"))
p <- ggplot(df, aes(error.measure, value,fill=class))
p + geom_boxplot() + labs(title = "Error comparison between original model and updated model_Min Temperature")
