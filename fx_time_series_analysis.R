rm(list=ls(all=TRUE))
library(openxlsx)
library(ggplot2)
library(dplyr)
library(zoo)
library(chron)
library(xts)
library(cts)
library(prophet)
library(forecast)
#---------------read input
fx_data <- read.xlsx("I:\\Transfer\\Analytics\\Input\\new_Trades_complete.xlsx", sheet = 1 , detectDates = TRUE)
#---------------fix date from excel
fx_data$DATE <- 
  as.POSIXct(fx_data$FXAH_TRANS_TIMESTAMP * (60*60*24)
             , origin="1899-12-30"
             , tz="GMT")
#---------------subset usdchf------------------------------
fx_usdchf <- subset(fx_data, CCY_PAIR == "USDCHF")
#---------------convert base ccy---------------------------
fx_usdchf$INV_AMT_BASE_CCY <- fx_usdchf$INVESTMENT_AMT
fx_usdchf[fx_usdchf$ISCCCY1BASE == "No","INV_AMT_BASE_CCY"] <- 
  fx_usdchf[fx_usdchf$ISCCCY1BASE == "No","INVESTMENT_AMT"]/fx_usdchf[fx_usdchf$ISCCCY1BASE == "No","MARKET_RATE"]
fx_usdchf[fx_usdchf$FXAH_SIDE == "BID","INV_AMT_BASE_CCY"] <- 
  -fx_usdchf[fx_usdchf$FXAH_SIDE == "BID","INV_AMT_BASE_CCY"]
#----get the cum sum of the flow---------------------------
fx_usdchf$cs <- cumsum(fx_usdchf$INV_AMT_BASE_CCY)
#----get the time series object
fx_usdchf.zoo <- zoo(fx_usdchf$cs, fx_usdchf$DATE)
#-----regular time sequence
hours <- seq(from = as.POSIXct('2016-01-04 00:00:00',tz='GMT'), 
      to = as.POSIXct('2017-12-29 20:00:00',tz='GMT'), by = "hour")
#-----empty regular zoo object
regular.zoo <- zoo(rep(NaN,length(hours)),as.POSIXct(hours))  # 
#-----merge zoo objects
z.tmp <- merge(fx_usdchf.zoo,regular.zoo)
#-----interpolate zoo object
z.tmp$fx_usdchf.zoo <- na.approx(z.tmp$fx_usdchf.zoo, rule=2)
#-----only keep regular times
fx_usdchf.zoo.regular <- z.tmp[index(regular.zoo),"fx_usdchf.zoo"]
#-----convert into dataframe
fx_usdchf.df.regular <- data.frame(ds = index(fx_usdchf.zoo.regular),
                                   y = coredata(fx_usdchf.zoo.regular))

#----train set data frame
fx_train <- subset(fx_usdchf.df.regular, ds > as.POSIXct('2017-11-01 00:00:00',tz='GMT'))
#----train set zoo
ind <- which(index(fx_usdchf.zoo.regular)> as.POSIXct('2017-12-01 00:00:00',tz='GMT'))
fx_train.zoo <- fx_usdchf.zoo.regular[ind]


#============modeling==================================================================

#----the facebook time series predictor (prohpet)
m <- prophet(fx_train, weekly.seasonality=FALSE,yearly.seasonality = FALSE)
test_periods <- as.numeric( as.POSIXct('2018-01-05 20:00:00',tz='GMT') - max(fx_usdchf.df.regular$ds) )
future <- make_future_dataframe(m, periods = test_periods)
forecast <- predict(m, future)
plot(m, forecast)
#prophet_plot_components(m, forecast)
#------Arima model with drift--------------------------------
n.ahead <- 10
fit.arima <- Arima(fx_train.zoo, order=c(0,1,0), include.drift = TRUE)
fore <- forecast(fit.arima, h=n.ahead)
plot(fore)
#------stl DOES NOT WORK YET
fit.stl <- stl(fx_train.zoo, s.window=12)
sts <- fit.stl$time.series
trend <- sts[,"trend"]
fore <- forecast(fit.stl, h=n.ahead, level=95)
plot(fore)
