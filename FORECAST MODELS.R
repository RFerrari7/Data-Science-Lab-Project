remove.packages("rlang")
install.packages("rlang")
library("rlang")
install.packages("tseries")
library("tseries")
install.packages("forecast")
library("forecast")
install.packages("lubridate")
library("lubridate")
install.packages("moments")
library(moments)
library("ggplot2")
library(stats)
library("tsoutliers")
require(forecast)


#<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
#<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<PRE AND POST COVID-19<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
#<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<

############## DAILY REVENUE -- PRE AND POST COVID-19 ###############################
data<- read.csv("C:/Users/Francesco/Downloads/fat_r000.csv", header=T) #Change here the dataset  *******CHANGE RESTAURANT NUMBER HERE*******

#For all restaurants except R002 use these two rows
x<-data[-c(1677:1706) ,]  #remove part from 3/4/2023
x<-x[-c(488:1218),]       #remove part from 1/1/2020 to 31/12/2021

#For the restaurant R002 use these two rows instead of the previous two
#x<-data[-c(1244:1273) ,] #remove part from 3/4/2023 (FOR R002)
#x<-x[-c(55:785),]       #remove part from 1/1/2020 to 31/12/2021 (FOR R002)

ts_R_raw <- ts(x$R000, start=1,frequency=7) #time series *******CHANGE RESTAURANT NUMBER HERE*******
ts_R <- tsclean(ts_R_raw) #cleaning of outliers

par(mfrow = c(2, 1)) #ACF and PACF
acf(ts_R, col = "purple", lwd = 4, ci.col = "red", main = "")
pacf(ts_R, col = "purple", lwd = 4, ci.col = "red", main = "")
print(Box.test(ts_R, type = "Ljung-Box"))

#+++++++++++++++++++++++++++++++ SARIMA +++++++++++++++++++++++++++++++++++
mod_R <-auto.arima(ts_R) #creation of the model
summary(mod_R)

arimafore_R <- forecast(mod_R, h=30, level=95) #forecast at 30 days

par(mfrow=c(2,1)) #plot of time series and forecast
plot(arimafore_R, col = "green4", main = " ",xlab = "Time [7 days]", ylab = "Daily revenue [€]")
polygon(c(time(arimafore_R$lower), rev(time(arimafore_R$upper))),c(arimafore_R$lower, rev(arimafore_R$upper)),col = "moccasin",border = NA)  
lines(arimafore_R$mean, col = "red", lwd = 2)
plot(arimafore_R, xlim = c(132,141), col = "green4",main = " ", xlab = "Time [7 days]", ylab = "Daily revenue [€]")
#in the previous "xlim" use:
#(132,141) for all restaurants except R002
#(71,79) for restaurant R002
polygon(c(time(arimafore_R$lower), rev(time(arimafore_R$upper))),c(arimafore_R$lower, rev(arimafore_R$upper)),col = "moccasin",border = NA) 
lines(arimafore_R$mean, col = "red", lwd = 2)

shapiro.test(mod_R$residuals) #Study of residuals
par(mfrow = c(1, 2))
qqnorm(mod_R$residuals, pch = 1, frame = FALSE)
qqline(mod_R$residuals, col = "green3", lwd = 3)
residui <- mod_R$residuals
residui_standardizzati <- (residui - mean(residui)) / sd(residui)
hist(residui_standardizzati, breaks = 15, probability = TRUE, col = "rosybrown1", main = " ",xlab = "standardalized residuals")
curve(dnorm(x, mean = mean(residui_standardizzati), sd = sd(residui_standardizzati)),col = "blue", lwd = 3, add = TRUE)

w<-as.numeric(arimafore_R$mean) #forecasted values
y<-data[c(1677:1706) ,]$R000 #real values   *******CHANGE RESTAURANT NUMBER HERE*******
#in the previous row use:
#(1677:1706) for all restaurants except R002
#(1244:1273) for restaurant R002
non_zero_indices <- y != 0 
w_n <- w[non_zero_indices]
y_n <- y[non_zero_indices]
print(mean(abs(w_n - y_n) / y_n)) #calculation of MAPE

#++++++++++++++++++++++++++++++++++ TBATS ++++++++++++++++++++++++++++++++++++
mod_R_tbats <- tbats(ts_R) #creation of the model
summary(mod_R_tbats)

arimafore_R <- forecast(mod_R_tbats, h=30, level=95) #forecast at 30 days

par(mfrow=c(2,1)) #plot of time series and forecast
plot(arimafore_R, col = "green4", main = " ",xlab = "Time [7 days]", ylab = "Daily revenue [€]")
polygon(c(time(arimafore_R$lower), rev(time(arimafore_R$upper))),c(arimafore_R$lower, rev(arimafore_R$upper)),col = "lightcyan1",border = NA)  
lines(arimafore_R$mean, col = "purple", lwd = 2)
plot(arimafore_R, xlim = c(132,141), col = "green4",main = " ", xlab = "Time [7 days]", ylab = "Daily revenue [€]")
#in the previous "xlim" use:
#(132,141) for all restaurants except R002
#(71,79) for restaurant R002
polygon(c(time(arimafore_R$lower), rev(time(arimafore_R$upper))),c(arimafore_R$lower, rev(arimafore_R$upper)),col = "lightcyan1",border = NA) 
lines(arimafore_R$mean, col = "purple", lwd = 2)

shapiro.test(mod_R$residuals) #Study of residuals
par(mfrow = c(1, 2))
qqnorm(mod_R$residuals, pch = 1, frame = FALSE)
qqline(mod_R$residuals, col = "green3", lwd = 3)
residui <- mod_R$residuals
residui_standardizzati <- (residui - mean(residui)) / sd(residui)
hist(residui_standardizzati, breaks = 15, probability = TRUE, col = "rosybrown1", main = " ",xlab = "standardalized residuals")
curve(dnorm(x, mean = mean(residui_standardizzati), sd = sd(residui_standardizzati)),col = "blue", lwd = 3, add = TRUE)

w<-as.numeric(arimafore_R$mean) #forecasted values
non_zero_indices <- y != 0 
w_n <- w[non_zero_indices]
y_n <- y[non_zero_indices]
print(mean(abs(w_n - y_n) / y_n)) #calculation of MAPE


################DAILY RECEIPTS -- PRE AND POST COVID-19#############################
data<- read.csv("C:/Users/Francesco/Downloads/scon_r000.csv", header=T) #Change here the dataset  *******CHANGE RESTAURANT NUMBER HERE*******

#For all restaurants except R002 use these two rows
x<-data[-c(1677:1706) ,]  #remove part from 3/4/2023
x<-x[-c(488:1218),]       #remove part from 1/1/2020 to 31/12/2021

#For the restaurant R002 use these two rows instead of the previous two
#x<-data[-c(1244:1273) ,] #remove part from 3/4/2023 (FOR R002)
#x<-x[-c(55:785),]       #remove part from 1/1/2020 to 31/12/2021 (FOR R002)

ts_R_raw <- ts(x$R000, start=1,frequency=7) #time series  *******CHANGE RESTAURANT NUMBER HERE*******
ts_R <- tsclean(ts_R_raw) #cleaning of outliers

par(mfrow = c(2, 1)) #ACF and PACF
acf(ts_R, col = "purple", lwd = 4, ci.col = "red", main = "")
pacf(ts_R, col = "purple", lwd = 4, ci.col = "red", main = "")
print(Box.test(ts_R, type = "Ljung-Box"))

#+++++++++++++++++++++++++++++++ SARIMA +++++++++++++++++++++++++++++++++++
mod_R <-auto.arima(ts_R) #creation of the model
summary(mod_R)

arimafore_R <- forecast(mod_R, h=30, level=95) #forecast at 30 days

par(mfrow=c(2,1)) #plot of time series and forecast
plot(arimafore_R, col = "blue", main = " ",xlab = "Time [7 days]", ylab = "Daily number of receipts")
polygon(c(time(arimafore_R$lower), rev(time(arimafore_R$upper))),c(arimafore_R$lower, rev(arimafore_R$upper)),col = "moccasin",border = NA)  
lines(arimafore_R$mean, col = "red", lwd = 2)
plot(arimafore_R, xlim = c(132,141), col = "blue",main = " ", xlab = "Time [7 days]", ylab = "Daily number of receipts")
#in the previous "xlim" use:
#(132,141) for all restaurants except R002
#(71,79) for restaurant R002
polygon(c(time(arimafore_R$lower), rev(time(arimafore_R$upper))),c(arimafore_R$lower, rev(arimafore_R$upper)),col = "moccasin",border = NA) 
lines(arimafore_R$mean, col = "red", lwd = 2)

shapiro.test(mod_R$residuals) #Study of residuals
par(mfrow = c(1, 2))
qqnorm(mod_R$residuals, pch = 1, frame = FALSE)
qqline(mod_R$residuals, col = "green3", lwd = 3)
residui <- mod_R$residuals
residui_standardizzati <- (residui - mean(residui)) / sd(residui)
hist(residui_standardizzati, breaks = 15, probability = TRUE, col = "rosybrown1", main = " ",xlab = "standardalized residuals")
curve(dnorm(x, mean = mean(residui_standardizzati), sd = sd(residui_standardizzati)),col = "blue", lwd = 3, add = TRUE)

w<-as.numeric(arimafore_R$mean) #forecasted values
y<-data[c(1677:1706) ,]$R000 #real values  *******CHANGE RESTAURANT NUMBER HERE*******
#in the previous row use:
#(1677:1706) for all restaurants except R002
#(1244:1273) for restaurant R002
non_zero_indices <- y != 0 
w_n <- w[non_zero_indices]
y_n <- y[non_zero_indices]
print(mean(abs(w_n - y_n) / y_n)) #calculation of MAPE

#++++++++++++++++++++++++++++++++++ TBATS ++++++++++++++++++++++++++++++++++++
mod_R_tbats <- tbats(ts_R) #creation of the model
summary(mod_R_tbats)

arimafore_R <- forecast(mod_R_tbats, h=30, level=95) #forecast at 30 days

par(mfrow=c(2,1)) #plot of time series and forecast
plot(arimafore_R, col = "blue", main = " ",xlab = "Time [7 days]", ylab = "Daily number of receipts")
polygon(c(time(arimafore_R$lower), rev(time(arimafore_R$upper))),c(arimafore_R$lower, rev(arimafore_R$upper)),col = "lightcyan1",border = NA)  
lines(arimafore_R$mean, col = "purple", lwd = 2)
plot(arimafore_R, xlim = c(132,141), col = "blue",main = " ", xlab = "Time [7 days]", ylab = "Daily number of receipts")
#in the previous "xlim" use:
#(132,141) for all restaurants except R002
#(71,79) for restaurant R002
polygon(c(time(arimafore_R$lower), rev(time(arimafore_R$upper))),c(arimafore_R$lower, rev(arimafore_R$upper)),col = "lightcyan1",border = NA) 
lines(arimafore_R$mean, col = "purple", lwd = 2)

shapiro.test(mod_R$residuals) #Study of residuals
par(mfrow = c(1, 2))
qqnorm(mod_R$residuals, pch = 1, frame = FALSE)
qqline(mod_R$residuals, col = "green3", lwd = 3)
residui <- mod_R$residuals
residui_standardizzati <- (residui - mean(residui)) / sd(residui)
hist(residui_standardizzati, breaks = 15, probability = TRUE, col = "rosybrown1", main = " ",xlab = "standardalized residuals")
curve(dnorm(x, mean = mean(residui_standardizzati), sd = sd(residui_standardizzati)),col = "blue", lwd = 3, add = TRUE)

w<-as.numeric(arimafore_R$mean) #forecasted values
non_zero_indices <- y != 0 
w_n <- w[non_zero_indices]
y_n <- y[non_zero_indices]
print(mean(abs(w_n - y_n) / y_n)) #calculation of MAPE



#<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
#<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<POST COVID-19<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
#<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<

############### DAILY REVENUE -- POST COVID-19 ###################################
data<- read.csv("C:/Users/Francesco/Downloads/fat_r000.csv", header=T) #Change here the dataset *******CHANGE RESTAURANT NUMBER HERE*******

#For all restaurants except R002 use the following row
x<-data[c(1219:1676) ,]  #select only part after 1/1/2022

#For the restaurant R002 use the following row instead of the previous one
#x<-data[c(786:1243) ,]  #select only part after 1/1/2022 (FOR R002)

ts_R_raw <- ts(x$R000, start=1,frequency=7) #time series  *******CHANGE RESTAURANT NUMBER HERE*******
ts_R <- tsclean(ts_R_raw) #cleaning of outliers

par(mfrow = c(2, 1)) #ACF and PACF
acf(ts_R, col = "purple", lwd = 4, ci.col = "red", main = "")
pacf(ts_R, col = "purple", lwd = 4, ci.col = "red", main = "")
print(Box.test(ts_R, type = "Ljung-Box"))

#+++++++++++++++++++++++++++++++ SARIMA +++++++++++++++++++++++++++++++++++
mod_R <-auto.arima(ts_R) #creation of the model
summary(mod_R)

arimafore_R <- forecast(mod_R, h=30, level=95) #forecast at 30 days

par(mfrow=c(2,1)) #plot of time series and forecast
plot(arimafore_R, col = "green4", main = " ",xlab = "Time [7 days]", ylab = "Daily revenue [€]")
polygon(c(time(arimafore_R$lower), rev(time(arimafore_R$upper))),c(arimafore_R$lower, rev(arimafore_R$upper)),col = "moccasin",border = NA)  
lines(arimafore_R$mean, col = "red", lwd = 2)
plot(arimafore_R, xlim = c(63,72), col = "green4",main = " ", xlab = "Time [7 days]", ylab = "Daily revenue [€]")
polygon(c(time(arimafore_R$lower), rev(time(arimafore_R$upper))),c(arimafore_R$lower, rev(arimafore_R$upper)),col = "moccasin",border = NA) 
lines(arimafore_R$mean, col = "red", lwd = 2)

shapiro.test(mod_R$residuals) #Study of residuals
par(mfrow = c(1, 2))
qqnorm(mod_R$residuals, pch = 1, frame = FALSE)
qqline(mod_R$residuals, col = "green3", lwd = 3)
residui <- mod_R$residuals
residui_standardizzati <- (residui - mean(residui)) / sd(residui)
hist(residui_standardizzati, breaks = 15, probability = TRUE, col = "rosybrown1", main = " ",xlab = "standardalized residuals")
curve(dnorm(x, mean = mean(residui_standardizzati), sd = sd(residui_standardizzati)),col = "blue", lwd = 3, add = TRUE)

w<-as.numeric(arimafore_R$mean) #forecasted values
y<-data[c(1677:1706) ,]$R000 #real values  *******CHANGE RESTAURANT NUMBER HERE*******
#in the previous row use:
#(1677:1706) for all restaurants except R002
#(1244:1273) for restaurant R002
non_zero_indices <- y != 0 
w_n <- w[non_zero_indices]
y_n <- y[non_zero_indices]
print(mean(abs(w_n - y_n) / y_n)) #calculation of MAPE

#++++++++++++++++++++++++++++++++++ TBATS ++++++++++++++++++++++++++++++++++++
mod_R_tbats <- tbats(ts_R) #creation of the model
summary(mod_R_tbats)

arimafore_R <- forecast(mod_R_tbats, h=30, level=95) #forecast at 30 days

par(mfrow=c(2,1)) #plot of time series and forecast
plot(arimafore_R, col = "green4", main = " ",xlab = "Time [7 days]", ylab = "Daily revenue [€]")
polygon(c(time(arimafore_R$lower), rev(time(arimafore_R$upper))),c(arimafore_R$lower, rev(arimafore_R$upper)),col = "lightcyan1",border = NA)  
lines(arimafore_R$mean, col = "purple", lwd = 2)
plot(arimafore_R, xlim = c(63,72), col = "green4",main = " ", xlab = "Time [7 days]", ylab = "Daily revenue [€]")
polygon(c(time(arimafore_R$lower), rev(time(arimafore_R$upper))),c(arimafore_R$lower, rev(arimafore_R$upper)),col = "lightcyan1",border = NA) 
lines(arimafore_R$mean, col = "purple", lwd = 2)

shapiro.test(mod_R$residuals) #Study of residuals
par(mfrow = c(1, 2))
qqnorm(mod_R$residuals, pch = 1, frame = FALSE)
qqline(mod_R$residuals, col = "green3", lwd = 3)
residui <- mod_R$residuals
residui_standardizzati <- (residui - mean(residui)) / sd(residui)
hist(residui_standardizzati, breaks = 15, probability = TRUE, col = "rosybrown1", main = " ",xlab = "standardalized residuals")
curve(dnorm(x, mean = mean(residui_standardizzati), sd = sd(residui_standardizzati)),col = "blue", lwd = 3, add = TRUE)

w<-as.numeric(arimafore_R$mean) #forecasted values
non_zero_indices <- y != 0 
w_n <- w[non_zero_indices]
y_n <- y[non_zero_indices]
print(mean(abs(w_n - y_n) / y_n)) #calculation of MAPE


################DAILY RECEIPTS -- POST COVID-19 ##################################
data<- read.csv("C:/Users/Francesco/Downloads/scon_r000.csv", header=T) #Change here the dataset  *******CHANGE RESTAURANT NUMBER HERE*******

#For all restaurants except R002 use the following row
x<-data[c(1219:1676) ,]  #select only part after 1/1/2022

#For the restaurant R002 use the following row instead of the previous one
#x<-data[c(786:1243) ,]  #select only part after 1/1/2022 (FOR R002)

ts_R_raw <- ts(x$R000, start=1,frequency=7) #time series  *******CHANGE RESTAURANT NUMBER HERE*******
ts_R <- tsclean(ts_R_raw) #cleaning of outliers

par(mfrow = c(2, 1)) #ACF and PACF
acf(ts_R, col = "purple", lwd = 4, ci.col = "red", main = "")
pacf(ts_R, col = "purple", lwd = 4, ci.col = "red", main = "")
print(Box.test(ts_R, type = "Ljung-Box"))

#+++++++++++++++++++++++++++++++ SARIMA +++++++++++++++++++++++++++++++++++
mod_R <-auto.arima(ts_R) #creation of the model
summary(mod_R)

arimafore_R <- forecast(mod_R, h=30, level=95) #forecast at 30 days

par(mfrow=c(2,1)) #plot of time series and forecast
plot(arimafore_R, col = "blue", main = " ",xlab = "Time [7 days]", ylab = "Daily number of receipts")
polygon(c(time(arimafore_R$lower), rev(time(arimafore_R$upper))),c(arimafore_R$lower, rev(arimafore_R000$upper)),col = "moccasin",border = NA)  
lines(arimafore_R$mean, col = "red", lwd = 2)
plot(arimafore_R, xlim = c(63,72), col = "blue",main = " ", xlab = "Time [7 days]", ylab = "Daily number of receipts")
polygon(c(time(arimafore_R$lower), rev(time(arimafore_R$upper))),c(arimafore_R$lower, rev(arimafore_R$upper)),col = "moccasin",border = NA) 
lines(arimafore_R$mean, col = "red", lwd = 2)

shapiro.test(mod_R$residuals) #Study of residuals
par(mfrow = c(1, 2))
qqnorm(mod_R$residuals, pch = 1, frame = FALSE)
qqline(mod_R$residuals, col = "green3", lwd = 3)
residui <- mod_R$residuals
residui_standardizzati <- (residui - mean(residui)) / sd(residui)
hist(residui_standardizzati, breaks = 15, probability = TRUE, col = "rosybrown1", main = " ",xlab = "standardalized residuals")
curve(dnorm(x, mean = mean(residui_standardizzati), sd = sd(residui_standardizzati)),col = "blue", lwd = 3, add = TRUE)

w<-as.numeric(arimafore_R$mean) #forecasted values
y<-data[c(1677:1706) ,]$R000 #real values  *******CHANGE RESTAURANT NUMBER HERE*******
#in the previous row use:
#(1677:1706) for all restaurants except R002
#(1244:1273) for restaurant R002
non_zero_indices <- y != 0 
w_n <- w[non_zero_indices]
y_n <- y[non_zero_indices]
print(mean(abs(w_n - y_n) / y_n)) #calculation of MAPE

#++++++++++++++++++++++++++++++++++ TBATS ++++++++++++++++++++++++++++++++++++
mod_R_tbats <- tbats(ts_R) #creation of the model
summary(mod_R_tbats)

arimafore_R <- forecast(mod_R_tbats, h=30, level=95) #forecast at 30 days

par(mfrow=c(2,1)) #plot of time series and forecast
plot(arimafore_R, col = "blue", main = " ",xlab = "Time [7 days]", ylab = "Daily number of receipts")
polygon(c(time(arimafore_R$lower), rev(time(arimafore_R$upper))),c(arimafore_R$lower, rev(arimafore_R$upper)),col = "lightcyan1",border = NA)  
lines(arimafore_R$mean, col = "purple", lwd = 2)
plot(arimafore_R, xlim = c(63,72), col = "blue",main = " ", xlab = "Time [7 days]", ylab = "Daily number of receipts")
polygon(c(time(arimafore_R000$lower), rev(time(arimafore_R$upper))),c(arimafore_R$lower, rev(arimafore_R$upper)),col = "lightcyan1",border = NA) 
lines(arimafore_R$mean, col = "purple", lwd = 2)

shapiro.test(mod_R$residuals) #Study of residuals
par(mfrow = c(1, 2))
qqnorm(mod_R$residuals, pch = 1, frame = FALSE)
qqline(mod_R$residuals, col = "green3", lwd = 3)
residui <- mod_R$residuals
residui_standardizzati <- (residui - mean(residui)) / sd(residui)
hist(residui_standardizzati, breaks = 15, probability = TRUE, col = "rosybrown1", main = " ",xlab = "standardalized residuals")
curve(dnorm(x, mean = mean(residui_standardizzati), sd = sd(residui_standardizzati)),col = "blue", lwd = 3, add = TRUE)

w<-as.numeric(arimafore_R$mean) #forecasted values
non_zero_indices <- y != 0 
w_n <- w[non_zero_indices]
y_n <- y[non_zero_indices]
print(mean(abs(w_n - y_n) / y_n)) #calculation of MAPE

