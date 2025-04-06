library(dlm)
library(dplyr)
library(xts)
library(ggplot2)
df <- read.csv('powerConsumption.csv', header = TRUE)
str(df)
nrow(df) / 6

### pre-processing ###
df <- df %>%
  mutate(DateTime_dt = as.POSIXct(DateTime, format = '%m/%d/%Y %H:%M', tz = 'GMT')) %>%
  mutate(ts_zone1 = xts(Zone1, order.by = DateTime_dt),
         ts_zone2 = xts(Zone2, order.by = DateTime_dt),
         ts_zone3 = xts(Zone3, order.by = DateTime_dt))
# Datetime_dt <- as.POSIXct(df$DateTime, format = '%m/%d/%Y %H:%M', tz = 'GMT')
# df$DateTime[is.na(Datetime_dt)]
# sum(is.na(Datetime_dt))


### single time series ###
ggplot(df, aes(x = DateTime_dt, y = Zone1)) + geom_line() + theme_minimal() +
  labs(x = 'DateTime', 
       y = 'Power consumption', 
       title = 'Time series of power consumption in Zone1') +
  theme(text = element_text(size = 16),
        axis.text = element_text(size = 12))
ggplot(df, aes(x = DateTime_dt, y = Zone2)) + geom_line() + theme_minimal() +
  labs(x = 'DateTime', 
       y = 'Power consumption', 
       title = 'Time series of power consumption in Zone2') +
  theme(text = element_text(size = 16),
        axis.text = element_text(size = 12))
ggplot(df, aes(x = DateTime_dt, y = Zone3)) + geom_line() + theme_minimal() +
  labs(x = 'DateTime', 
       y = 'Power consumption', 
       title = 'Time series of power consumption in Zone3') +
  theme(text = element_text(size = 16),
        axis.text = element_text(size = 12))
ggplot(df, aes(x = DateTime_dt, y = Temp)) + geom_line() + theme_minimal() +
  labs(x = 'DateTime', 
       y = 'Temperature', 
       title = 'Temperature') +
  theme(text = element_text(size = 16),
        axis.text = element_text(size = 12))
ggplot(df, aes(x = DateTime_dt, y = Humidity)) + geom_line() + theme_minimal() +
  labs(x = 'DateTime', 
       y = 'Humidity', 
       title = 'Humidity') +
  theme(text = element_text(size = 16),
        axis.text = element_text(size = 12))
ggplot(df, aes(x = DateTime_dt, y = WindSpeed)) + geom_line() + theme_minimal() +
  labs(x = 'DateTime', 
       y = 'Windspeed', 
       title = 'Windspeed') +
  theme(text = element_text(size = 16),
        axis.text = element_text(size = 12))
ggplot(df, aes(x = DateTime_dt, y = GDF)) + geom_line() + theme_minimal() +
  labs(x = 'DateTime', 
       y = 'GDF', 
       title = 'General diffuse flows') +
  theme(text = element_text(size = 16),
        axis.text = element_text(size = 12))
ggplot(df, aes(x = DateTime_dt, y = DF)) + geom_line() + theme_minimal() +
  labs(x = 'DateTime', 
       y = 'DF', 
       title = 'Diffuse flows') +
  theme(text = element_text(size = 16),
        axis.text = element_text(size = 12))


### correlations ###
library(tseries)
library(forecast)
library(TSA)
df %>%
  pull(ts_zone1) %>%
  adf.test() # < 0.01
df %>%
  pull(Zone1) %>%
  acf(main = 'ACF for Zone1') # long tail
df %>%
  pull(Zone1) %>%
  pacf(lag.max = 1000,
       main = 'PACF for Zone1') # lag 2
#
df %>%
  pull(Zone2) %>%
  adf.test() # < 0.01
df %>%
  pull(Zone2) %>%
  acf(main = 'ACF for Zone2') # long tail
df %>%
  pull(Zone2) %>%
  pacf(lag.max = 1000, main = 'PACF for Zone2') # lag 2-3
#
df %>%
  pull(ts_zone3) %>%
  adf.test() # < 0.01
df %>%
  pull(Zone3) %>%
  acf(main = 'ACF for Zone3') # long tail
df %>%
  pull(Zone3) %>%
  pacf(lag.max = 1000, main = 'PACF for Zone3') # lag 2
#
df %>% 
  pull(Zone1) %>%
  diff(lag = 6 * 24) %>% 
  ggtsdisplay(theme = theme_minimal()) 
df %>% 
  pull(Zone2) %>%
  diff(lag = 6 * 24) %>% 
  ggtsdisplay(theme = theme_minimal()) 
df %>% 
  pull(Zone3) %>%
  diff(lag = 6 * 24) %>% 
  ggtsdisplay(theme = theme_minimal()) 

#
ccf(ts(df$Zone1, frequency = 144), ts(df$Temp, frequency = 144), lag.max = 200,
    main = 'Zone1 vs Temperature')
ccf(df$Zone2, df$Temp, lag.max = 200,
    main = 'Zone2 vs Temperature')
ccf(df$Zone3, df$Temp, lag.max = 200,
    main = 'Zone3 vs Temperature')

### arima ###
train_idx_1 <- df$DateTime_dt < (as.POSIXct('2017/12/1 0:00', foramt = '%Y/%m/%d %H:%M', tz = 'GMT'))
sum(train_idx_1)
train_idx_2 <- df$DateTime_dt < (as.POSIXct('2017/11/1 0:00', foramt = '%Y/%m/%d %H:%M', tz = 'GMT'))
sum(train_idx_2)
#
eacf(ts(diff(df$Zone1, lag = 144)))
acf(ts(diff(df$Zone1, lag = 144)), lag.max = 1000)
pacf(ts(df$Zone1), lag.max = 5000, ylim = c(-.2, .2))
pacf(ts(diff(df$Zone1, lag = 144)), lag.max = 2000)
zone1_diff <- ts(diff(df$Zone1, lag = 144))
acf(diff(zone1_diff, lag = 144))
pacf(diff(zone1_diff, lag = 144))
# adf.test(ts(diff(df$Zone1, lag = 144)))
arima1 <- stats::arima(ts(df$Zone1), order = c(1, 0, 0), seasonal = list(order = c(1, 1, 0), period = 6 * 24))
summary(arima1)
checkresiduals(arima1)
#
arima2 <- stats::arima(ts(df$Zone1, frequency = 144), order = c(3, 0, 0), seasonal = list(order = c(0, 1, 1), period = 6 * 24))
summary(arima2)
checkresiduals(arima2)
#
arima3 <- stats::arima(ts(df$Zone1, frequency = 144),
                       order = c(3, 0, 0), 
                       seasonal = list(order = c(1, 0, 0), period = 6 * 24),
                       method = "CSS")
summary(arima3)
checkresiduals(arima3)
#
arima4 <- stats::arima(ts(df$Zone1, frequency = 144),
                       order = c(3, 0, 0), 
                       seasonal = list(order = c(1, 1, 0), period = 6 * 24),
                       method = "CSS")
summary(arima4)
checkresiduals(arima4)
#
arima5 <- stats::arima(ts(df$Zone1, frequency = 144),
                       order = c(3, 0, 0), 
                       seasonal = list(order = c(1, 1, 1), period = 6 * 24),
                       method = "CSS")
summary(arima5)
checkresiduals(arima5)
#
arima6 <- stats::arima(ts(df$Zone1, frequency = 144),
                       order = c(3, 0, 0), 
                       seasonal = list(order = c(2, 1, 1), period = 6 * 24),
                       method = "CSS")
summary(arima6)
checkresiduals(arima6)
# 
arima7 <- Arima(ts(df$Zone1),
                       order = c(3, 0, 0), 
                       seasonal = list(order = c(2, 1, 0), period = 6 * 24),
                method = 'CSS')
summary(arima7)
checkresiduals(arima7) # 0.0005502
# 
arima8 <- Arima(ts(df$Zone1),
                order = c(3, 0, 0), 
                seasonal = list(order = c(2, 2, 0), period = 6 * 24),
                method = 'CSS')
summary(arima8)
checkresiduals(arima8) # 0.04199
#
ts_zone1 <- ts(df$Zone1, frequency = 144)
auto_arima1 <- auto.arima(ts_zone1, seasonal = TRUE)
summary(auto_arima1)
checkresiduals(auto_arima1, theme = theme_minimal())
#
arima_zone1 <- Arima(ts(df$Zone1[train_idx_1]),
                     order = c(3, 0, 0), 
                     seasonal = list(order = c(2, 2, 0), period = 6 * 24),
                     method = 'CSS')
summary(arima_zone1)
checkresiduals(arima_zone1, theme = theme_minimal()) 
#
delta1 <- nrow(df) - sum(train_idx_1)
delta2 <- nrow(df) - sum(train_idx_2)
fc_zone1 <- forecast(arima_zone1, h = delta1)
autoplot(fc_zone1)
df_zone1_fc <- data.frame(
  time = time(fc_zone1$mean),
  predicted = as.numeric(fc_zone1$mean),
  actual = as.numeric(ts((df$Zone1[!train_idx_1]), frequency = 144)),
  lower_95 = fc_zone1$lower[, 2],
  upper_95 = fc_zone1$upper[, 2]
)
ggplot(df_zone1_fc, aes(x = time)) +
  geom_ribbon(aes(ymin = lower_95, ymax = upper_95), fill = "lightblue", alpha = 0.4) +
  geom_line(aes(y = actual), color = "black", linewidth = 1, linetype = "solid") +
  geom_line(aes(y = predicted), color = "blue", linewidth = 1, linetype = "dashed") +
  labs(title = "Actual vs Predicted",
       y = "Value", x = "Time") +
  theme_minimal()
#
auto_arimax1 <- auto.arima(ts(df$Zone1[train_idx_1], frequency = 144), 
                           xreg = as.matrix(
                             df[train_idx_1, c('Temp', 'Humidity', 'WindSpeed','GDF', 'DF')]),
                           seasonal = TRUE)
summary(auto_arimax1)
checkresiduals(auto_arimax1, theme = theme_minimal())
#
arimax1 <- Arima(ts(df$Zone1[train_idx_1], frequency = 144), 
                 order = c(3, 0, 0), 
                 seasonal = list(order = c(0, 1, 1), period = 6 * 24),
                 xreg = as.matrix(
                   df[train_idx_1, c('Temp', 'Humidity', 'WindSpeed','GDF', 'DF')]))
summary(arimax1)
autoplot(arimax1)
checkresiduals(arimax1, theme = theme_minimal())
#
arimax2 <- Arima(ts(df$Zone1[train_idx_1], frequency = 144), 
                 order = c(3, 0, 1), 
                 seasonal = list(order = c(0, 1, 1), period = 6 * 24),
                 xreg = as.matrix(
                   df[train_idx_1, c('Temp', 'Humidity', 'WindSpeed','GDF', 'DF')]))
summary(arimax2)
autoplot(arimax2)
checkresiduals(arimax2, theme = theme_minimal(df$Zone1[train_idx_1], frequency = 144),
               )
#
arimax_zone1 <- Arima(ts(df$Zone1[train_idx_1], frequency = 144),
                      order = c(3, 0, 0), 
                      seasonal = list(order = c(2, 2, 0), period = 6 * 24),
                      method = 'CSS')
