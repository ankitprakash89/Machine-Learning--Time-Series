#######################################################
library(tseries)
library(forecast)
nifty <- read.csv("C:/Users/Administrator/Desktop/Machine Learning/Datasets/NIFTY-I_NFO.csv")
nifty_close <- ts(nifty$Close, start= c(1,1), frequency = 375)
fit1 <- decompose(nifty_close)
plot(fit1)

adf.test(nifty_close)
nifty_stationary <- diff(log(nifty_close))
adf.test(nifty_stationary)
plot(nifty_stationary)

# to fund p,d,q value
acf(nifty_stationary)
# it implies q value is 0

pacf(nifty_stationary)
#  it implies p value is also 0

ndiffs(nifty_close)
# it implies d value is 1

# Now fitting the model using ARIMA
fit <- arima(log(nifty_close), c(0,1,0), list(order= c(0,1,0)))
predicttt <- predict(fit,n.ahead = 75)
predic <- 2.718^predicttt$pred
ts.plot(nifty_close,predic,log="y",lty=c(1,3))

# by using Auto Arima 
fit1 <- auto.arima(nifty_close)
summary(fit1)
forecast(fit1, h=5)

# by using exponential smoothening
fit2 <- ets(nifty_close)
summary(fit2)
forecast(fit2, h=5)

# by using neural network
library(caret)
fit3 <- nnetar(nifty_close)
summary(fit3)
plot(forecast(fit3, h =5))

#  testing 
View(nifty)
nifty_test_close <- ts(as.numeric(nifty$Close[1:16150]), start= c(1,1), frequency = 375)
fit4 <- nnetar(nifty_test_close)
summary(fit4)
val <- forecast(fit4, h=7)

original <- as.vector(tail(nifty_close, 7))
df <- data.frame(original, val)
View(df)
