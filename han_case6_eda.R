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
# arima1 <- stats::arima(ts(df$Zone1), order = c(1, 0, 0), seasonal = list(order = c(1, 1, 0), period = 6 * 24))
# summary(arima1)
# checkresiduals(arima1)
# #
# arima2 <- stats::arima(ts(df$Zone1, frequency = 144), order = c(3, 0, 0), seasonal = list(order = c(0, 1, 1), period = 6 * 24))
# summary(arima2)
# checkresiduals(arima2)
# #
# arima3 <- stats::arima(ts(df$Zone1, frequency = 144),
#                        order = c(3, 0, 0), 
#                        seasonal = list(order = c(1, 0, 0), period = 6 * 24),
#                        method = "CSS")
# summary(arima3)
# checkresiduals(arima3)
# #
# arima4 <- stats::arima(ts(df$Zone1, frequency = 144),
#                        order = c(3, 0, 0), 
#                        seasonal = list(order = c(1, 1, 0), period = 6 * 24),
#                        method = "CSS")
# summary(arima4)
# checkresiduals(arima4)
# #
# arima5 <- stats::arima(ts(df$Zone1, frequency = 144),
#                        order = c(3, 0, 0), 
#                        seasonal = list(order = c(1, 1, 1), period = 6 * 24),
#                        method = "CSS")
# summary(arima5)
# checkresiduals(arima5)
# #
# arima6 <- stats::arima(ts(df$Zone1, frequency = 144),
#                        order = c(3, 0, 0), 
#                        seasonal = list(order = c(2, 1, 1), period = 6 * 24),
#                        method = "CSS")
# summary(arima6)
# checkresiduals(arima6)
# # 
# arima7 <- Arima(ts(df$Zone1),
#                        order = c(3, 0, 0), 
#                        seasonal = list(order = c(2, 1, 0), period = 6 * 24),
#                 method = 'CSS')
# summary(arima7)
# checkresiduals(arima7) # 0.0005502
# # 
# arima8 <- Arima(ts(df$Zone1),
#                 order = c(3, 0, 0), 
#                 seasonal = list(order = c(2, 2, 0), period = 6 * 24),
#                 method = 'CSS')
# summary(arima8)
# checkresiduals(arima8) # 0.04199
# #
# ts_zone1 <- ts(df$Zone1, frequency = 144)
# auto_arima1 <- auto.arima(ts_zone1, seasonal = TRUE)
# summary(auto_arima1)
# checkresiduals(auto_arima1, theme = theme_minimal())
# #
# arima_zone1 <- Arima(ts(df$Zone1[train_idx_1]),
#                      order = c(3, 0, 0), 
#                      seasonal = list(order = c(2, 2, 0), period = 6 * 24),
#                      method = 'CSS')
# summary(arima_zone1)
# checkresiduals(arima_zone1, theme = theme_minimal()) 
#
delta1 <- nrow(df) - sum(train_idx_1)
delta2 <- nrow(df) - sum(train_idx_2)
# fc_zone1 <- forecast(arima_zone1, h = delta1)
# autoplot(fc_zone1)
# df_zone1_fc <- data.frame(
#   time = time(fc_zone1$mean),
#   predicted = as.numeric(fc_zone1$mean),
#   actual = as.numeric(ts((df$Zone1[!train_idx_1]), frequency = 144)),
#   lower_95 = fc_zone1$lower[, 2],
#   upper_95 = fc_zone1$upper[, 2]
# )
# ggplot(df_zone1_fc, aes(x = time)) +
#   geom_ribbon(aes(ymin = lower_95, ymax = upper_95), fill = "lightblue", alpha = 0.4) +
#   geom_line(aes(y = actual), color = "black", linewidth = 1, linetype = "solid") +
#   geom_line(aes(y = predicted), color = "blue", linewidth = 1, linetype = "dashed") +
#   labs(title = "Actual vs Predicted",
#        y = "Value", x = "Time") +
#   theme_minimal()
#
library(xtable)
auto_arimax1 <- auto.arima(ts(df$Zone1[train_idx_1], frequency = 144), 
                           xreg = as.matrix(
                             df[train_idx_1, c('Temp', 'Humidity', 'WindSpeed','GDF', 'DF')]),
                           seasonal = TRUE)
summary(auto_arimax1)
xtable(as.data.frame(t(auto_arimax1$coef)))
checkresiduals(auto_arimax1, theme = theme_minimal())
#
# arimax1 <- Arima(ts(df$Zone1[train_idx_1], frequency = 144), 
#                  order = c(3, 0, 0), 
#                  seasonal = list(order = c(0, 1, 1), period = 6 * 24),
#                  xreg = as.matrix(
#                    df[train_idx_1, c('Temp', 'Humidity', 'WindSpeed','GDF', 'DF')]))
# summary(arimax1)
# autoplot(arimax1)
# checkresiduals(arimax1, theme = theme_minimal())
# #
# arimax2 <- Arima(ts(df$Zone1[train_idx_1], frequency = 144), 
#                  order = c(3, 0, 1), 
#                  seasonal = list(order = c(0, 1, 1), period = 6 * 24),
#                  xreg = as.matrix(
#                    df[train_idx_1, c('Temp', 'Humidity', 'WindSpeed','GDF', 'DF')]))
# summary(arimax2)
# autoplot(arimax2)
# checkresiduals(arimax2, theme = theme_minimal(df$Zone1[train_idx_1], frequency = 144),
#                )
#
# arimax_zone1 <- Arima(ts(df$Zone1[train_idx_1], frequency = 144),
#                       order = c(3, 0, 0), 
#                       seasonal = list(order = c(2, 2, 0), period = 6 * 24),
#                       method = 'CSS')
#
fc_auto_arimax_zone1 <- forecast(auto_arimax1, 
                                 xreg = as.matrix(
                                   df[!train_idx_1, c('Temp', 'Humidity', 'WindSpeed','GDF', 'DF')]), 
                                 h = delta1)
autoplot(fc_auto_arimax_zone1)
df_zone1_auto_arimax_fc <- data.frame(
  time = time(fc_auto_arimax_zone1$mean),
  predicted = as.numeric(fc_auto_arimax_zone1$mean),
  actual = as.numeric(ts((df$Zone1[!train_idx_1]), frequency = 144)),
  lower_95 = fc_auto_arimax_zone1$lower[, 2],
  upper_95 = fc_auto_arimax_zone1$upper[, 2]
)

ggplot(df_zone1_auto_arimax_fc, aes(x = time)) +
  geom_ribbon(
    aes(ymin = lower_95, ymax = upper_95, fill = "95% Confidence Interval"),
    alpha = 0.4
  ) +
  geom_line(
    aes(y = actual, color = "Actual"),
    linewidth = 1
  ) +
  geom_line(
    aes(y = predicted, color = "Predicted"),
    linewidth = 1,
    linetype = "dashed"
  ) +
  scale_color_manual(
    name = "",
    values = c("Actual" = "black", "Predicted" = "blue")
  ) +
  scale_fill_manual(
    name = "",
    values = c("95% Confidence Interval" = "lightblue")
  ) +
  labs(
    title = "Actual vs Predicted for Zone 1",
    x = "Day",
    y = "Power consumption"
  ) +
  theme_minimal() +
  theme(legend.position = "bottom",
        text = element_text(size = 20),
        axis.text = element_text(size = 12))

#--- zone2 ---#
auto_arimax2 <- auto.arima(ts(df$Zone2[train_idx_1], frequency = 144), 
                           xreg = as.matrix(
                             df[train_idx_1, c('Temp', 'Humidity', 'WindSpeed','GDF', 'DF')]),
                           seasonal = TRUE)
summary(auto_arimax2)
checkresiduals(auto_arimax2, theme = theme_minimal())
#
fc_auto_arimax_zone2 <- forecast(auto_arimax2, 
                                 xreg = as.matrix(
                                   df[!train_idx_1, c('Temp', 'Humidity', 'WindSpeed','GDF', 'DF')]), 
                                 h = delta1)
autoplot(fc_auto_arimax_zone2)
df_zone2_auto_arimax_fc <- data.frame(
  time = time(fc_auto_arimax_zone2$mean),
  predicted = as.numeric(fc_auto_arimax_zone2$mean),
  actual = as.numeric(ts((df$Zone2[!train_idx_1]), frequency = 144)),
  lower_95 = fc_auto_arimax_zone2$lower[, 2],
  upper_95 = fc_auto_arimax_zone2$upper[, 2]
)

ggplot(df_zone2_auto_arimax_fc, aes(x = time)) +
  geom_ribbon(
    aes(ymin = lower_95, ymax = upper_95, fill = "95% Confidence Interval"),
    alpha = 0.4
  ) +
  geom_line(
    aes(y = actual, color = "Actual"),
    linewidth = 1
  ) +
  geom_line(
    aes(y = predicted, color = "Predicted"),
    linewidth = 1,
    linetype = "dashed"
  ) +
  scale_color_manual(
    name = "",
    values = c("Actual" = "black", "Predicted" = "blue")
  ) +
  scale_fill_manual(
    name = "",
    values = c("95% Confidence Interval" = "lightblue")
  ) +
  labs(
    title = "Actual vs Predicted for Zone 2",
    x = "Day",
    y = "Power consumption"
  ) +
  theme_minimal() +
  theme(legend.position = "bottom",
        text = element_text(size = 20),
        axis.text = element_text(size = 12))

#--- zone3 ---#
auto_arimax3 <- auto.arima(ts(df$Zone3[train_idx_2], frequency = 144), 
                           xreg = as.matrix(
                             df[train_idx_2, c('Temp', 'Humidity', 'WindSpeed','GDF', 'DF')]),
                           seasonal = TRUE)
summary(auto_arimax3)
checkresiduals(auto_arimax3, theme = theme_minimal())
#
fc_auto_arimax_zone3 <- forecast(auto_arimax3, 
                                 xreg = as.matrix(
                                   df[!train_idx_2, c('Temp', 'Humidity', 'WindSpeed','GDF', 'DF')]), 
                                 h = delta2)
autoplot(fc_auto_arimax_zone3)
df_zone3_auto_arimax_fc <- data.frame(
  time = time(fc_auto_arimax_zone3$mean),
  predicted = as.numeric(fc_auto_arimax_zone3$mean),
  actual = as.numeric(ts((df$Zone3[!train_idx_2]), frequency = 144)),
  lower_95 = fc_auto_arimax_zone3$lower[, 2],
  upper_95 = fc_auto_arimax_zone3$upper[, 2]
)

ggplot(df_zone3_auto_arimax_fc, aes(x = time)) +
  geom_ribbon(
    aes(ymin = lower_95, ymax = upper_95, fill = "95% Confidence Interval"),
    alpha = 0.4
  ) +
  geom_line(
    aes(y = actual, color = "Actual"),
    linewidth = 1
  ) +
  geom_line(
    aes(y = predicted, color = "Predicted"),
    linewidth = 1,
    linetype = "dashed"
  ) +
  scale_color_manual(
    name = "",
    values = c("Actual" = "black", "Predicted" = "blue")
  ) +
  scale_fill_manual(
    name = "",
    values = c("95% Confidence Interval" = "lightblue")
  ) +
  labs(
    title = "Actual vs Predicted for Zone 3",
    x = "Day",
    y = "Power consumption"
  ) +
  theme_minimal() +
  theme(legend.position = "bottom",
        text = element_text(size = 20),
        axis.text = element_text(size = 12))

### auto-regression-spline ###
library(dynlm)
library(splines)
zone1 <- ts(df$Zone1)
zone2 <- ts(df$Zone2)
zone3 <- ts(df$Zone3)
temp <- ts(df$Temp)
humidity <- ts(df$Humidity)
windspeed <- ts(df$WindSpeed)
gdf <- ts(df$GDF)
dff <- ts(df$DF)
#
n <- length(zone1)
time_index <- 1:n
ns_temp <- ns(temp, df = 3)
ns_humidity <- ns(humidity, df = 3)
ns_windspeed <- ns(windspeed, df = 3)
ns_gdf <- ns(gdf, df = 3)
ns_dff <- ns(dff, df = 3)
#
df_all <- data.frame(
  zone1 = zone1,
  temp = temp,
  humidity = humidity,
  windspeed = windspeed,
  gdf = gdf,
  dff = dff,
  ns_temp,
  ns_humidity,
  ns_windspeed,
  ns_gdf,
  ns_dff
)
zdf <- zoo(df_all, order.by = time_index)
#
df_all_diff <- data.frame(
  zone1_diff = zone1_diff,
  temp = temp[-(1:144)],
  humidity = humidity[-(1:144)],
  windspeed = windspeed[-(1:144)],
  gdf = gdf[-(1:144)],
  dff = dff[-(1:144)],
  ns_temp[-(1:144), ],
  ns_humidity[-(1:144), ],
  ns_windspeed[-(1:144), ],
  ns_gdf[-(1:144), ],
  ns_dff[-(1:144), ]
)
zdf_diff <- zoo(df_all_diff, order.by = time_index)
#
dynlm2_zone1 <- dynlm(zone1 ~ 
                        L(zone1, 1) + L(zone1, 2) + L(zone1, 3) + L(zone1, 144) +
                        X1 + X2 + X3 +        # ns_temp
                        X1.1 + X2.1 + X3.1 +        # ns_humidity
                        X1.2 + X2.2 + X3.2 +        # ns_windspeed
                        X1.3 + X2.3 + X3.3 +        # ns_gdf
                        X1.4 + X2.4 + X3.4,         # ns_dff
                      data = zdf)

summary(dynlm2_zone1)
plot(dynlm2_zone1)
resid_zone1 <- residuals(dynlm2_zone1)
plot(resid_zone1, type = "l", main = "Residuals", ylab = "Residuals")
acf(resid_zone1, lag.max = 300)
pacf(resid_zone1, lag.max = 300)
Box.test(resid_zone1, lag = 300, type = "Ljung-Box")
#
dynlm2_zone1_diff <- dynlm(zone1_diff ~ 
                        L(zone1_diff, 1) + L(zone1_diff, 2) + L(zone1_diff, 3) +
                        X1 + X2 + X3 +        # ns_temp
                        X1.1 + X2.1 + X3.1 +        # ns_humidity
                        X1.2 + X2.2 + X3.2 +        # ns_windspeed
                        X1.3 + X2.3 + X3.3 +        # ns_gdf
                        X1.4 + X2.4 + X3.4,         # ns_dff
                      data = zdf_diff)

summary(dynlm2_zone1_diff)
plot(dynlm2_zone1_diff)
resid_zone1 <- residuals(dynlm2_zone1_diff)
plot(resid_zone1, type = "l", main = "Residuals", ylab = "Residuals")
acf(resid_zone1, lag.max = 300)
pacf(resid_zone1, lag.max = 300)
Box.test(resid_zone1, lag = 300, type = "Ljung-Box")

### OLS-spline ###
lm_df_zone1 <- data.frame(
  zone1 = df$Zone1[-(1:144)],
  zone1_lag1 = lag(df$Zone1)[-(1:144)],
  zone1_lag2 = lag(df$Zone1, 2)[-(1:144)],
  zone1_lag3 = lag(df$Zone1, 3)[-(1:144)],
  zone1_lags = lag(df$Zone1, 144)[-(1:144)],
  temp = temp[-(1:144)],
  humidity = humidity[-(1:144)],
  windspeed = windspeed[-(1:144)],
  gdf = gdf[-(1:144)],
  dff = dff[-(1:144)]
)
lm_fit1 <- lm(zone1 ~ zone1_lag1 + zone1_lag2 + zone1_lag3 + zone1_lags + 
                bs(temp, 3) + bs(humidity, 3) + bs(windspeed, 3) +
                bs(gdf, 3) + bs(dff, 3), 
              data = lm_df_zone1)
summary(lm_fit1)
plot(lm_fit1)
resid1 <- residuals(lm_fit1)
acf(resid1)
pacf(resid1)
#
lm_df_zone1_diff <- data.frame(
  zone1_diff = df$Zone1[-(1:144)] - lag(df$Zone1, 144)[-(1:144)],
  zone1_lag1 = lag(df$Zone1)[-(1:144)] - lag(df$Zone1, 145)[-(1:144)],
  zone1_lag2 = lag(df$Zone1, 2)[-(1:144)] - lag(df$Zone1, 146)[-(1:144)],
  zone1_lag3 = lag(df$Zone1, 3)[-(1:144)] - lag(df$Zone1, 147)[-(1:144)],
  temp = temp[-(1:144)],
  humidity = humidity[-(1:144)],
  windspeed = windspeed[-(1:144)],
  gdf = gdf[-(1:144)],
  dff = dff[-(1:144)]
)
lm_fit1_diff <- lm(zone1_diff ~ zone1_lag1 + zone1_lag2 + zone1_lag3 + 
                bs(temp, 3) + bs(humidity, 3) + bs(windspeed, 3) +
                bs(gdf, 3) + bs(dff, 3), 
              data = lm_df_zone1_diff)
summary(lm_fit1_diff)
plot(lm_fit1_diff)
resid1_diff <- residuals(lm_fit1_diff)
acf(resid1_diff, lag.max = 500)
pacf(resid1_diff, lag.max = 500)

#
zone1_mat <- matrix(df$Zone1, nrow = 144)
zone1_daily_mean <- rowMeans(zone1_mat)
plot(zone1_daily_mean, type = 'l', ylab = 'Average power consumption')
zone1_std <- df$Zone1 - rep(zone1_daily_mean, nrow(df) / 144)
plot(zone1_std, type = 'l', ylab = 'Difference in power consumption')
lm_df_zone1_std <- data.frame(
  zone1 = zone1_std[-(1:3)],
  zone1_lag1 = lag(zone1_std)[-(1:3)],
  zone1_lag2 = lag(zone1_std, 2)[-(1:3)],
  zone1_lag3 = lag(zone1_std, 3)[-(1:3)],
  temp = temp[-(1:3)],
  humidity = humidity[-(1:3)],
  windspeed = windspeed[-(1:3)],
  gdf = gdf[-(1:3)],
  dff = dff[-(1:3)]
)
lm_fit1_std <- lm(zone1 ~ zone1_lag1 + zone1_lag2 + zone1_lag3 + 
                bs(temp, 3) + bs(humidity, 3) + bs(windspeed, 3) +
                bs(gdf, 3) + bs(dff, 3), 
              data = lm_df_zone1_std)
summary(lm_fit1_std)
plot(lm_fit1_std)
resid1 <- residuals(lm_fit1_std)
acf(resid1, lag.max = 300)
pacf(resid1, lag.max = 300)








