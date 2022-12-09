# ARIMA Model

# install package: ARIMAANN,forcats,tseries,GGally
# d=difference l= lag time

data1<- read_excel("C:/Users/niraj/Downloads/timeseries_ppi.xlsx")
data1

class(data1)
attach(data1)

#defining variable
Y<- ppi
# difference
d.Y = diff(Y)
t= yearqrt
t

# Describing the statistics and plot the data points
summary(Y)
summary(d.Y)
plot(t,Y)
plot(d.Y)

# Dickey fular test for variable
# k=0 mean=0
adf.test(Y,alternative = "stationary",k=0)
adf.test(Y,alternative = "explosive",k=0)
summary(lm(dppi~lppi,na.action = na.omit))
summary(lm(dppi~lppi+trend,na.action=na.omit))

# Augmented DICKEY Fuller Test
adf.test(Y,alternative = "stationary")

# DF and ADF TETST for difference variable
adf.test(d.Y,k=0) # which is less then previous p value (0,01)
adf.test(d.Y)

#ACF and PACF 
# acf=auto co-relation function which hepls to control the moving average (q)
acf(Y)
# pacf = partial auto co-relation
pacf(Y)

# ARIMA (1,0,0) OR AR=1, (p,d,q),(AR,difference,moving average)

arima(Y,order = c(1,0,0))

# ARIMA (2,0,0) OR AR=2, # coefficient =-ve
arima(Y,order = c(2,0,0))

# ARIMA (0,0,1) OR MA =1
arima(Y,order = c(0,0,1)) # coefficient =1

# ARIMA model on difference =1 an d AR=1
arima(d.Y,order = c(1,1,0))
arima(d.Y,order = c(0,1,1)) #-0.5230
arima(d.Y,order = c(1,1,1)) #0.4983
arima(d.Y,order = c(0,0,1)) #0.4872
arima(d.Y,order = c(0,1,0))
arima(d.Y,order = c(1,0,1)) #0.7245

# ARIMA (1,0,1) For forcasting
data1.arima101=arima(Y,order = c(1,0,1))
data1.pred1=predict(data1.arima101,n.ahead = 100)
plot(Y)
lines(data1.pred1$pred,col="red")
lines(data1.pred1$pred+2*data1.pred1$se,col="blue")

#trail 2
data1.arima101=arima(Y,order = c(0,0,1))
dara1.pred1=predict(data1.arima101,n.ahead = 100)
plot(Y)

#trail 3
data1.arima101=arima(Y,order = c(1,1,1))
data1.pred1=predict(data1.arima101,n.ahead = 100)
plot(Y)
lines(data1.pred1$pred,col="red")
lines(data1.pred1$pred+2*data1.pred1$se,col="blue")















