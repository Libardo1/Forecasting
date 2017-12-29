# Load Necessary Libraries
library(lubridate)
library(urca)
library(forecast)

# Set Working Directory
setwd("~/Sem_4/Econ_5337/Project")

# Read the data
ecom = read.csv("ECOMPCTNSA.csv")

# Check the beginning of dataset
head(ecom,4)

# Full Dataset
full=ts(ecom[,2], start=c(2000,4), frequency = 4)

# End of the dataset
tail(ecom, 5)

# Holdout period
hold=ts(ecom[,2], start=c(2000,4), end=c(2016, 3), frequency = 4)

# Plots
plot(full, ylab = "Percent of Sales")
plot(hold, ylab = "Percent of Sales")

# Log Transformed data
plot(log(full), ylab = "Percent of Sales")
plot(log(hold))

# To make sure you want to use log transformation, the value of lamda you might want to take
BoxCox.lambda(hold) # -0.3991852

'''
Deterministic Components to include:
Constant
'''

# Dickey Fuller Test
summary(ur.df(log(hold), type = "drift", lags = 20, selectlags = "AIC"))
summary(ur.df(log(hold), type = "drift", lags = 20, selectlags = "BIC"))

# Seasonal differencing
acf(diff(log(hold,4)),20)
pacf(diff(log(hold,4)),20)

# Verify presence of seasonality
seasonplot(log(hold))

# Model Estimation
############ Final Candidate Models Chosen ####################
m1 = Arima(log(hold), order=c(1,0,0), seasonal = c(1,1,0), include.drift = TRUE)
acf(m1$residuals)
summary(m1)

m2 = Arima(log(hold), order=c(1,0,0), seasonal = c(2,1,0), include.drift = TRUE)
acf(m2$residuals)
summary(m2)

m3 = Arima(log(hold), order=c(1,0,0), seasonal = c(1,1,1), include.drift = TRUE)
acf(m3$residuals)
summary(m3)

m4 = Arima(log(hold), order=c(3,0,0), seasonal = c(2,1,0), include.drift = TRUE)
acf(m4$residuals)
summary(m4)

m5 = Arima(log(hold), order=c(2,0,0), seasonal = c(1,1,0), include.drift = TRUE)
acf(m5$residuals)
summary(m5)

m6 = Arima(log(hold), order=c(1,0,1), seasonal = c(1,1,0), include.drift = TRUE)
acf(m6$residuals)
summary(m6)

# Ljung Box Test for the candidate model 1
Box.test(m1$residuals, type = "Ljung-Box", lag = 24)
# X-squared = 9.5567, df = 24, p-value = 0.9961

# Forecast for holdout 
fore=forecast(m1,h=4,lambda=0,biasadj=TRUE)
upper=ts(fore$upper[,2],start=c(2016,4),frequency=4)
lower=ts(fore$lower[,2],start=c(2016,4),frequency=4)
plotFULL=window(full,start=c(2016,1))

plot(cbind(plotFULL, upper, lower, fore$mean), main="Holdout Forecast",xlab="Years",ylab="Percent of Sales", col=c("BLACK","GREEN","RED","BLUE"),lty=c("solid","dotted","dotted","solid"), plot.type="single", lwd=2)
legend("topleft",legend=c("Actual Sales","Upper 95% CI","Lower 95% CI","Forecasted Sales"),fill=c("BLACK","GREEN","RED","BLUE"))

fore$mean
window(full,start=c(2016,4))

# Out of Sample forecast
fm = Arima(log(full), order=c(1,0,0), seasonal = c(1,1,0), include.drift = TRUE)
foreout = forecast(fm,h=2, lambda = 0, biasadj = TRUE)
upperout=ts(foreout$upper[,2],start=c(2017,4),frequency=4)
lowerout=ts(foreout$lower[,2],start=c(2017,4),frequency=4)
plotFULLout=window(full,start=c(2016,1))

plot(cbind(plotFULLout, upperout, lowerout, foreout$mean), main="2 step Ahead Forecast",xlab="Years",ylab="Percent of Sales", col=c("BLACK","GREEN","RED","BLUE"),lty=c("solid","dotted","dotted","solid"), plot.type="single", lwd=2)
legend("topleft",legend=c("Actual Sales","Upper 95% CI","Lower 95% CI","Forecasted Sales"),fill=c("BLACK","GREEN","RED","BLUE"))

foreout$mean

# Accuracy check
accuracy(fore, window(full, start=c(2016,4)))

