## IMPORTING AND PROCESSING TIME SERIES DATA

# Source: https://www.r-bloggers.com/getting-data-from-an-online-source/

# reading data from an R package
install.packages("nycflights13")
head(flights)

# reading .dat from a website
www <- "https://raw.githubusercontent.com/dallascard/Introductory_Time_Series_with_R_datasets/master/Maine.dat" 
Maine.month <- read.table(www, header = TRUE)
attach(Maine.month) 

# reading .dat from local directory
varnish <- read.table("C:/Users/reneq/Documents/2019SP Time-Series Analysis/w5-varnish.dat", header = TRUE)
v <-data.frame(varnish)

# reading csv file local directory
income <- read.csv("C:/Users/reneq/Documents/2019SP Time-Series Analysis/income.csv", header = TRUE)
head(income)

#reading excel file local directory
income <- read.excel("C:/Users/reneq/Documents/2019SP Time-Series Analysis/income.xlsx", header = TRUE)
head(income)

# creating a time series object
choc.ts <- ts(choc,st = c(1958,1), fr = 12)

# Source: https://iu.instructure.com/courses/1774378/pages/week-6-practice?module_item_id=18406333
# Great xts resource:  https://www.datacamp.com/community/blog/r-xts-cheat-sheet

# transforming time series with zoo & xts 
#install zoo & xts
install.packages("zoo")
install.packages("xts")
library(zoo)
library(xts)

# The default format is “YYYY/m/d” or “YYYY-m-d”
my.date <- as.Date('1979/1/1')
my.date
class(my.date)

myDates <- c('2013-12-19','2003-12-20')
myDates
as.Date(myDates)

# Inputing dates to R that are not in the devault format
as.Date('1/1/1970', format='%m/%d/%Y')
as.Date('January 1, 1970', format='%B %d, %Y')
as.Date('01JAN70', format='%d%b%y')

# Extracting for the date
myYear = format(my.date, '%Y')
myYear
class(myYear)
as.numeric(myYear)
as.numeric(format(my.date, '%Y'))

#Other components of data
# the weekdays(), months(), quarters() and julian() functions can be used to extract specific components of Date objects

weekdays(my.date)
months(my.date)
quarters(my.date)

# Manipulating dates
my.date
my.date + 1
my.date - 1
my.date + 31

# Read using read.zoo
Z <- read.zoo(inData[3:10],format = "%d-%b-%y",index.column = "Date")
Z
#Get the mean of Z$DeliveryVolume to Z$TotalVolume per quarter, by using Aggregate function.
aggregate(Z$DeliveryVolume/Z$TotalVolume,as.yearqtr,mean)
#Get the mean of Z$DeliveryVolume to Z$TotalVolume per month, by using Aggregate function.
aggregate(Z$DeliveryVolume/Z$TotalVolume,as.yearmon,mean)
#Extract only the rows from 2015-Feb-01 to 2015-Feb-15 from Zoo object Z
window(Z, start = as.Date("2015-02-01"),end = as.Date("2015-02-15"))
"- convert zoo data to xts
- change the format to 01/02/19 from 2019-01-02
- estimate the periodicity of data
- count months
- count years"
z_xts <- as.xts(Z)
indexFormat(z_xts) <- "%m/%d/%y"
z_xts
periodicity(z_xts)
nmonths(z_xts)
nyears(z_xts)
#split xts data by month and calculate mean"
z_xts_monthly <- split(z_xts,f="month")
lapply(z_xts_monthly,FUN=mean)



# EXPLORATORY VISUALIZATION OF A TIME SERIES
# read data file
cbe <- read.table("C:/Users/reneq/Documents/2019SP Time-Series Analysis/w5-cbe.dat", header = TRUE)
# name column variable for analysis
choc <- cbe[["choc"]]
# create time series object 
# st=c(starting year, starting period (i.e. month/quarter), fr= frequency (i.e. monthly or quarterly)
choc.ts <- ts(choc,st = c(1958,1), fr = 12)
# plot acf of a time series
acf(choc.ts)
# decomopose to separate time series into observed, trend, seasonal, & error 
# can be multiplicative or additive
choc.decom <- decompose(choc.ts, type = "mult") 
# plots the decomposition 
plot(choc.decom)
# assign trend variable
Trend <- choc.decom$trend 
# assign seasonal variable
Seasonal <- choc.decom$seasonal
# plot of treand and seasonal on same plot 
ts.plot(cbind(Trend, Trend*Seasonal), lty = 1:2)

# TIME SERIES ANALYSIS

# Source: https://www.datacamp.com/courses/forecasting-using-r 

# I really like using this package I learned from Data Camp "Forecasting Using R" 
# need to install this packag to used the functions below
install.packages('fpp2', dependencies = TRUE)
library("fpp2")

# time series transformations
# transformation scale (ascending order of effect) to make fluctuations approximately even over time
# square root
# cube root
# logarithm
# inverse

#look into box-cox transformation (lambda values parameter)
BoxCox.lambda(mydata)
#lambda= 1; no transformation
#lambda=0.5; square root plus linear transformation
#lambda=0.33; cube root plus linear transformation
#lambda=0; natural log transformation
#lambda=-1; inverse transformation

# Plot the series
autoplot(a10)


# Try four values of lambda in Box-Cox transformations
a10 %>% BoxCox(lambda = 0.0) %>% autoplot()
a10 %>% BoxCox(lambda = 0.1) %>% autoplot()
a10 %>% BoxCox(lambda = 0.2) %>% autoplot()
a10 %>% BoxCox(lambda = 0.3) %>% autoplot()
# Compare with BoxCox.lambda()
BoxCox.lambda(a10)

# ARIMA Models 
# AR = multiple regression with lagged observations as predictors
# MA = multiple regression with lagged errors as predictors
# ARMA = multiple regression with lagged observations and errors as predictors (only work with stationary data so you need to difference, ie diff(), the data FIRST)
# I = integrated
# ARIMA combine ARMA model with d-lots of differencing ...ARIMA(p,d,q); optional "c" intercept parameter
# Seasonal part has parameters P,D,Q corresponding to (P,D,Q)m where: 
#P=seasonal AR lags, D=seasonal differences, Q= seasonal MA lags
# fit <- auto.arima(mydata)
# selects number of differences d via unit root tests
# selects p & q by minimizing AICc (AICc can only be used to compare models of the same class)
# estimates parameters based on maximum likelyhood estimation
# uses stepwise search to travese model space, to save time (may not be best model)

# Fit an automatic ARIMA model to the austa series
fit <- auto.arima(austa)
# Check that the residuals look like white noise
checkresiduals(fit) #p-value in output should be >=0.05
# Summarize the model
summary(fit)
# Plot forecasts of fit
fit %>% forecast(h = 10) %>% autoplot()
# Plot forecasts from an ARIMA(0,1,1) model with no drift
austa %>% Arima(order = c(0, 1, 1), include.constant = FALSE) %>% forecast() %>% autoplot()
# Plot forecasts from an ARIMA(2,1,3) model with drift
austa %>% Arima(order = c(2, 1, 3), include.constant = TRUE) %>% forecast() %>% autoplot()
# Plot forecasts from an ARIMA(0,0,1) model with a constant
austa %>% Arima(order = c(0, 0, 1), include.constant = TRUE) %>% forecast() %>% autoplot()
# Plot forecasts from an ARIMA(0,2,1) model with no constant
austa %>% Arima(order = c(0, 2, 1), include.constant = FALSE) %>% forecast() %>% autoplot()

# NON SEASONAL ARIMA
# Set up forecast functions for ETS and ARIMA models

fets <- function(x, h) {
  forecast(ets(x), h = h)
}

farima <- function(x, h) {
  forecast(auto.arima(x), h=h)
}


# Compute CV errors for ETS as e1
e1 <- tsCV(austa, fets, h=1)
# Compute CV errors for ARIMA as e2
e2 <- tsCV(austa, farima, h=1)
# Find MSE of each model class
mean(e1^2, na.rm = TRUE)
mean(e2^2, na.rm = TRUE)
# Plot 10-year forecasts using the best model class
austa %>% farima(h=10) %>% autoplot()

# SEASONAL ARIMA
# Check that the logged h02 data have stable variance

h02 %>% log() %>% autoplot()
# Fit a seasonal ARIMA model to h02 with lambda = 0
fit <- auto.arima(h02,lambda=0.0)
# Summarize the fitted model
summary(fit)
# Plot 2-year forecasts
fit %>% forecast(h = 24) %>% autoplot()

# Find an ARIMA model for euretail
fit1 <- auto.arima(euretail)
# Don't use a stepwise search
fit2 <- auto.arima(euretail, stepwise = FALSE)
# Compute 2-year forecasts from better model
fit2 %>% forecast(h = 8) %>% autoplot()

# Use 20 years of the qcement data beginning in 1988
train <- window(qcement, start = 1988, end = c(2007,4))
# Fit an ARIMA and an ETS model to the training data
fit1 <- auto.arima(train)
fit2 <- ets(train)
# Check that both models have white noise residuals
checkresiduals(fit1)
checkresiduals(fit2)
# Produce forecasts for each model
fc1 <- forecast(fit1, h = 25)
fc2 <- forecast(fit2, h = 25)

# Use accuracy() to find better model based on RMSE
accuracy(fc1, qcement)
accuracy(fc2, qcement)

-----------------------
#  DYNAMIC REGRESSION
# ordinary model use only historical data
# dynamic regression uses predictive variables in error term
# includes xreg parameter

# Time plot of both variables
autoplot(advert, facets = TRUE)
# Fit ARIMA model
fit <- auto.arima(advert[, "sales"], xreg = advert[, "advert"], stationary = TRUE)
# Check model. Increase in sales for each unit increase in advertising
salesincrease <- coefficients(fit)[3]
# Forecast fit as fc
fc <- forecast(fit, xreg = rep(10,6))
# Plot fc with x and y labels
autoplot(fc) + xlab("Month") + ylab("Sales")

# Time plots of demand and temperatures
autoplot(elec[, c(1, 2)], facets = TRUE)
# Matrix of regressors
xreg <- cbind(MaxTemp = elec[, "Temperature"], 
              MaxTempSq = elec[, "Temperature"]^2,
              Workday = elec[, "Workday"])
# Fit model
fit <- auto.arima(elec[, "Demand"], xreg = xreg)
# Forecast fit one day ahead
forecast(fit, xreg = cbind(20, 20^2, 1))

# DYNAMIC HARMONIC REGRESSION

# start with K=1 and increase 1 by 1 to minimize AICc
# K cannot by more that m/2, ie can't be more than the seasonal period
# fourier(x, K, h = NULL)

fit <- auto.arima(cafe, xreg = fourier(cafe, K = 6), seasonal = FALSE, lambda = 0)
fit %>% forecast(xreg = fourier(cafe, K = 6, h = 24)) %>%
autoplot() + ylim(1.6, 5.1)

# Set up harmonic regressors of order 13
harmonics <- fourier(gasoline, K = 13)
# Fit regression model with ARIMA errors
fit <- auto.arima(gasoline, xreg = harmonics, seasonal = FALSE)
#seasonality should always be FALSE because it's handled by the xreg regressors
# Forecasts next 3 years
newharmonics <- fourier(gasoline, K = 13, h = 52*3)
fc <- forecast(fit, xreg = newharmonics)
# Plot forecasts fc
autoplot(fc)

# Fit a harmonic regression using order 10 for each type of seasonality
fit <- tslm(taylor ~ fourier(taylor, K = c(10, 10)))
# Forecast 20 working days ahead
fc <- forecast(fit, newdata = data.frame(fourier(taylor, K = c(10,10), h = 20*48)))
# Plot the forecasts
autoplot(fc)
# Check the residuals of fit
checkresiduals(fit)


