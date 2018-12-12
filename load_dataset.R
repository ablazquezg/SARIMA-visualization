
setwd("D:/prj/GitHub Energy and Buildings")

##########################
#### LOAD DATA SET #######
##########################

library(feather)
library(data.table)
DT <- as.data.table(read_feather("DT_4_ind"))
DT_type1 <- DT[(type == "Commercial Property")]
data_set <- DT_type1[,1:2] # 17520
data_set <- data_set[1:5000,] # 5000
plot(data_set, type="l")


###########################
#### ADJUST MODELS ########
###########################

library(forecast)
library(ggplot2)

# Weekly seasonality: 7*24*2 observations per week
data_set.ts <- ts(data_set$value, frequency = 7*24*2) 

# Training and test sets
train.ts <- subset(data_set.ts, end=length(data_set.ts)-7*24*2) 
test.ts <- subset(data_set.ts, start=length(data_set.ts)-7*24*2+1)

ggplot() + 
  geom_line(data=train.ts, aes(x=head(data_set$date_time,-7*24*2), y=train.ts, color='Training set')) +
  geom_line(data=test.ts, aes(x=tail(data_set$date_time,7*24*2), y = tail(data_set$value,7*24*2), color='Test set')) +
  theme(panel.border = element_blank(),
        panel.background = element_blank(),
        panel.grid.minor = element_line(colour = "grey90"),
        panel.grid.major = element_line(colour = "grey90"),
        panel.grid.major.x = element_line(colour = "grey90"),
        axis.text = element_text(size = 10),
        axis.title = element_text(size = 12, face = "bold"))+
  scale_color_manual(name="",values = c("blue", "violet"))+
  labs(title = "Half hourly energy consumption",subtitle = "02-01-2012 00:00:00 / 25-03-2012 07:30:00",x = "Date", y = "Load (kW)")+
  guides(fill=guide_legend(title=NULL))

summary(c(train.ts, test.ts))
ggseasonplot(train.ts, year.labels = TRUE, main="Seasonal plot of the training set")

# As it has double seasonality, we consider the working days as explained in the paper.
library(timeDate)
data_set_wd <- data_set[isWeekday(data_set$date_time),]; summary(data_set_wd) # 
data_set_wd.ts <- ts(data_set_wd$value, frequency = 24*2)
train.ts_wd <- subset(data_set_wd.ts, end=length(data_set_wd.ts)-5*24*2) 
test.ts_wd <- subset(data_set_wd.ts, start=length(data_set_wd.ts)-5*24*2+1)

ggplot() + 
  geom_line(data=train.ts_wd, aes(x=head(data_set_wd$date_time,-5*24*2), y=train.ts_wd, color='Training set')) +
  geom_line(data=test.ts_wd, aes(x=tail(data_set_wd$date_time,5*24*2), y = tail(data_set_wd$value,5*24*2), color='Test set')) +
  theme(panel.border = element_blank(),
        panel.background = element_blank(),
        panel.grid.minor = element_line(colour = "grey90"),
        panel.grid.major = element_line(colour = "grey90"),
        panel.grid.major.x = element_line(colour = "grey90"),
        axis.text = element_text(size = 10),
        axis.title = element_text(size = 12, face = "bold"))+
  scale_color_manual(name="",values = c("blue", "violet"))+
  labs(title = "Half hourly energy consumption during working days",subtitle = "02-01-2012 00:00:00 / 23-03-2012 23:30:00",x = "Date", y = "Load (kW)")+
  guides(fill=guide_legend(title=NULL))


ggseasonplot(train.ts_wd, year.labels=TRUE)


# Stationarity check

par(mfrow = c(2,1))
Acf(train.ts_wd, lag=24*2*2, main="") 
Pacf(train.ts_wd, lag=24*2*2, main="")
par(mfrow = c(1,1)) # clearly daily seasonal component --> D=1

library(tseries)
kpss.test(train.ts_wd) # The p-value is 0.1, so the null hypothesis is not rejected at the usual 5% level.
#ndiffs(train.ts_wd) 
#nsdiffs(train.ts_wd) 

# Stationarity transformation

train.ts_diff_wd <- diff(train.ts_wd,lag=24*2) # difference to remove the seasonal component
plot(train.ts_diff_wd, ylab="Difference", xlab="Time")
par(mfrow = c(2,1))
Acf(train.ts_diff_wd, lag=24*2+10, main="") 
Pacf(train.ts_diff_wd, lag=24*2+10, main="")
par(mfrow = c(1,1))

# Genetic algorithm for order selection
library(GA)
decode <- function(string, bitOrders){ 
  string <- split(string, rep.int(seq.int(bitOrders), times = bitOrders)) 
  orders <- sapply(string, function(x){binary2decimal(x)}) 
  return(unname(orders)) 
}
fitness <- function(string, data, bitOrders, maxp, maxd, maxq, maxP, maxD, maxQ){  
  orders <- decode(string, bitOrders) 
  if(orders[1]>maxp || orders[2]>maxd || orders[3]>maxq || orders[4]>maxP || orders[5]>maxD || orders[5]<maxD || orders[6]>maxQ) NA 
  else {
    print(orders)
    mod <- try(Arima(data, order = orders[1:3], seasonal=orders[4:6]), silent = TRUE) 
    if(inherits(mod, "try-error")) NA else -mod$aicc 
  }
}

start_time.ga <- Sys.time()
GA <- ga(type = "binary", fitness = fitness, bitOrders = c(2,1,2,1,1,1), maxp=2, maxd=0, maxq=2, maxP=1, maxD=1,maxQ=1, 
          data = train.ts_wd, nBits=8, popSize = 25, maxiter = 5, seed=20)
end_time.ga <- Sys.time()
end_time.ga - start_time.ga # 4.540149 mins

plot(GA)
summary(GA)
decode(GA@solution[1,], bitOrders = c(2,1,2,1,1,1))  # 2 0 2 0 1 1

arima.ga <- Arima(train.ts_wd, order=c(2,0,2), seasonal=c(0,1,1))
Box.test(arima.ga$residuals, lag=min(length(train.ts_wd)/5, 2*24*2), 'Ljung')
fore.ga <- forecast(arima.ga, h=24*2*5)
accuracy(fore.ga, test.ts_wd)
checkresiduals(arima.ga) 

arima.ga %>%  residuals() %>% ggtsdisplay(main="ARIMA(2,0,2)(0,1,1)[48] residuals", plot.type = "partial")
mean(residuals(fore.ga), na.rm=TRUE)
nortest::lillie.test(residuals(arima.ga)) 
tsdiag(arima.ga)

a<-autoplot(fore.ga, main="5 working days forecast ARIMA(2,0,2)(0,1,1)[48]", ylab="Energy", xlab="Date")
a+theme(panel.border = element_blank(),
        panel.background = element_blank(),
        panel.grid.minor = element_line(colour = "grey90"),
        panel.grid.major = element_line(colour = "grey90"),
        panel.grid.major.x = element_line(colour = "grey90"),
        axis.text = element_text(size = 10),
        axis.title = element_text(size = 12, face = "bold"))+
  scale_color_manual(name="",values = c("blue", "violet"))+
  labs(title = "5 working days forecast ARIMA(2,0,2)(0,1,1)[96]",x = "Date", y = "Load (kW)")+
  guides(fill=guide_legend(title=NULL))

ggplot() +geom_line(data=train.ts_wd, aes(x=as.numeric(time(train.ts_wd)), y=as.numeric(train.ts_wd), colour="observation")) + 
  geom_line(data=train.ts_wd, aes(x=as.numeric(time(train.ts_wd)), y=as.numeric(fitted(fore.ga)), colour="prediction")) + 
  theme(panel.border = element_blank(),
        panel.background = element_blank(),
        panel.grid.minor = element_line(colour = "grey90"),
        panel.grid.major = element_line(colour = "grey90"),
        panel.grid.major.x = element_line(colour = "grey90"),
        axis.text = element_text(size = 10),
        axis.title = element_text(size = 12, face = "bold"))+
  scale_color_manual(name="",values = c(observation="black",prediction="red")) +
  labs(title = "1-step forecast in the training set",x = "Date", y = "Load (kW)")+
  guides(fill=guide_legend(title=NULL))

ggplot() +geom_line(data=test.ts_wd, aes(x=tail(data_set_wd$date_time,24*5*2), y=as.numeric(test.ts_wd), colour="observation")) + 
  geom_line(data=test.ts_wd, aes(x=tail(data_set_wd$date_time,24*5*2), y=as.numeric(fore.ga$mean), colour="prediction")) + 
  theme(panel.border = element_blank(),
        panel.background = element_blank(),
        panel.grid.minor = element_line(colour = "grey90"),
        panel.grid.major = element_line(colour = "grey90"),
        panel.grid.major.x = element_line(colour = "grey90"),
        axis.text = element_text(size = 10),
        axis.title = element_text(size = 12, face = "bold"))+
  scale_color_manual(name="",values = c(observation="black",prediction="red")) +
  labs(title = "240-step forecast in the test set",x = "Date", y = "Load (kW)")+
  guides(fill=guide_legend(title=NULL))

fit.wd <- Arima(data_set_wd.ts, model=arima.ga)
onestepwd <-fitted(fit.wd)
ggplot() +geom_line(data=tail(data_set_wd.ts,24*5*2), aes(x=tail(data_set_wd$date_time,24*5*2), y=as.numeric(tail(data_set_wd.ts,24*5*2)), colour="observation")) + 
  geom_line(data=tail(data_set_wd.ts,24*5*2), aes(x=tail(data_set_wd$date_time,24*5*2), y=as.numeric(tail(onestepwd,24*5*2)), colour="prediction")) + 
  theme(panel.border = element_blank(),
        panel.background = element_blank(),
        panel.grid.minor = element_line(colour = "grey90"),
        panel.grid.major = element_line(colour = "grey90"),
        panel.grid.major.x = element_line(colour = "grey90"),
        axis.text = element_text(size = 10),
        axis.title = element_text(size = 12, face = "bold"))+
  scale_color_manual(name="",values = c(observation="black",prediction="red")) +
  labs(title = "1-step forecast in the test set", x = "Date", y = "Load (kW)")+
  guides(fill=guide_legend(title=NULL))

######################
### Interactive plots
######################

gen_array <- function(forecast_obj, timest, test_set, freq){
  actuals <- xts(forecast_obj$x, order.by=timest[1:length(forecast_obj$x)])
  fitted <- xts(forecast_obj$fitted, order.by=timest[1:length(forecast_obj$fitted)])
  test <- xts(test_set, order.by=tail(timest,freq))
  point_forecast <- xts(forecast_obj$mean, order.by = tail(timest,freq))

  cbind(fitted, actuals, test, point_forecast)
}
library(xts)
data_plot <- xts(gen_array(fore.ga, data_set_wd$date_time, test.ts_wd, 24*2*5), order.by=data_set_wd$date_time)
colnames(data_plot) <- c("fitted","actuals", "actual_test", "point_forecast")

# Add absolute and relative errors
data_plot$error_abs <- abs(data_plot$fitted-data_plot$actuals)
data_plot$error_rel <- abs(data_plot$fitted-data_plot$actuals)/abs(data_plot$actuals)
data_plot$error_abs_test <- abs(data_plot$actual_test-data_plot$point_forecast)
data_plot$error_rel_test <- abs(data_plot$actual_test-data_plot$point_forecast)/abs(data_plot$actual_test)

# Create the three graphics
library(dygraphs)
result_graph <-list(
  dygraph(data_plot[,1:4], main = "30-minute forecast",ylab="Load (kW)", group="data_plot") %>%
    dySeries(c("point_forecast")) %>%
    dyRangeSelector(height = 40)%>%
    dyOptions(colors = RColorBrewer::brewer.pal(3, "Set1"))%>%
    dyLegend(show = "onmouseover",width = 400),
  dygraph(data_plot[,c("error_abs","error_abs_test")], main = "",ylab="Absolute error (kW)", group="data_plot") %>%
    dyLimit( limit=mean(c(data_plot$error_abs,data_plot$error_abs_test),na.rm=TRUE), label=paste0("Mean=", round(mean(c(data_plot$error_abs,data_plot$error_abs_test),na.rm=TRUE),4)), 
             labelLoc = c("left", "right"), color = "purple", strokePattern = "dashed")%>%
    dyOptions(colors = RColorBrewer::brewer.pal(3, "Set1"))%>%
    dyLegend(show = "onmouseover",width = 400),
  dygraph(data_plot[,c("error_rel","error_rel_test")], main = "",ylab="Relative error (kW)", group="data_plot") %>%
    dyLimit( limit=mean(c(data_plot$error_rel,data_plot$error_rel_test),na.rm=TRUE), label=paste0("Mean=", round(mean(c(data_plot$error_rel,data_plot$error_rel_test),na.rm=TRUE),4)), 
             labelLoc = c("left", "right"), color = "purple", strokePattern = "dashed")%>%
    dyOptions(colors = RColorBrewer::brewer.pal(3, "Set1"))%>%
    dyLegend(show = "onmouseover",width = 400)
)

#################################
### Application in a new data set
#################################

newdata_set <- DT_type1[7001:12000,1:2] # 5000
newdata_set_wd <- newdata_set[isWeekday(newdata_set$date_time),]
newdata_set_wd.ts <- ts(newdata_set_wd$value, frequency = 24*2)
#newtrain.ts_wd <- subset(newdata_set_wd.ts, end=length(newdata_set_wd.ts)-5*24*2) 
#newtest.ts_wd <- subset(newdata_set_wd.ts, start=length(newdata_set_wd.ts)-5*24*2+1)
plot(newdata_set_wd.ts,type="l")
# one-step forecast
newfit <- Arima(newdata_set_wd.ts, model=arima.ga)
onestep_newfit <- fitted(newfit)
accuracy(newfit)

gen_array_1 <- function(fit, datav,datats){ # datav: data value // datats: data time stamp
  actuals <- xts(datav, order.by=datats[1:length(fit)])
  fitted <- xts(as.numeric(fit), order.by=datats[1:length(fit)])
  cbind(fitted, actuals)
}

new_data_plot <- xts(gen_array_1(onestep_newfit,newdata_set_wd$value,newdata_set_wd$date_time), order.by=newdata_set_wd$date_time)
colnames(new_data_plot) <- c("fitted","actuals")

# Add absolute and relative errors
new_data_plot$error_abs <- abs(new_data_plot$fitted-new_data_plot$actuals)
new_data_plot$error_rel <- abs(new_data_plot$fitted-new_data_plot$actuals)/abs(new_data_plot$actuals)

new_results_graph <-list(
  dygraph(new_data_plot[,c("fitted","actuals")], main = "1-step forecast in a new data set",ylab="Load (kW)", group="data") %>%
    dyRangeSelector(height = 40)%>%
    dyOptions(colors = RColorBrewer::brewer.pal(3, "Set1"))%>%
    dyLegend(show = "onmouseover",width = 400),
  dygraph(new_data_plot$error_abs, main = "",ylab="Absolute error (kW)", group="data") %>%
    dyLimit( limit=mean(new_data_plot$error_abs,na.rm=TRUE), label=paste0("Mean=", round(mean(c(new_data_plot$error_abs),na.rm=TRUE),4)), 
             labelLoc = c("left", "right"), color = "purple", strokePattern = "dashed")%>%
    dyOptions(colors = RColorBrewer::brewer.pal(3, "Set1"))%>%
    dyLegend(show = "onmouseover",width = 400),
  dygraph(new_data_plot$error_rel, main = "",ylab="Relative error (kW)", group="data") %>%
    dyLimit( limit=mean(c(new_data_plot$error_rel),na.rm=TRUE), label=paste0("Mean=", round(mean(c(new_data_plot$error_rel),na.rm=TRUE),4)),
             labelLoc = c("left", "right"), color = "purple", strokePattern = "dashed")%>%
    dyOptions(colors = RColorBrewer::brewer.pal(3, "Set1"))%>%
    dyLegend(show = "onmouseover",width = 400)
)


