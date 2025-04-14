lower <- NULL
upper <- NULL
tm <- 47952
for (i in 1:tm) {
  lower <- rbind(lower, apply(gibbs.out1$theta[i, , ], 1, function(x) sort(x)[250]))
  upper <- rbind(upper, apply(gibbs.out1$theta[i, , ], 1, function(x) sort(x)[9750]))
}

mean <- NULL
for (i in 1:tm) {
  mean <- rbind(mean, apply(gibbs.out1$theta[i, , ], 1, mean))
}

saveRDS(upper, 'RData/upper1.RData')
saveRDS(lower, 'RData/lower1.RData')
saveRDS(mean, 'RData/mean1.RData')

#####
par(mfrow = c(2, 3))
hist(gibbs.out1$theta[, 1, ], freq = FALSE, xlab = '', main = 'Autocorrelation (Lag = 1)')
hist(gibbs.out1$theta[, 2, ], freq = FALSE, xlab = '', main = 'Autocorrelation (Lag = 2)')
hist(gibbs.out1$theta[, 3, ], freq = FALSE, xlab = '', main = 'Autocorrelation (Lag = 144)')
hist(gibbs.out1$theta[, 4, ], freq = FALSE, xlab = '', main = 'Autocorrelation (Lag = 145)')
hist(gibbs.out1$theta[, 5, ], freq = FALSE, xlab = '', main = 'Cross-correlation (Zone 2)')
hist(gibbs.out1$theta[, 6, ], freq = FALSE, xlab = '', main = 'Cross-correlation (Zone 3)')
#####

#####
par(mfrow = c(2, 3))
hist(gibbs.out1$theta[, 7, ], freq = FALSE, xlab = '', main = 'Temp (Lag = 1)')
hist(gibbs.out1$theta[, 8, ], freq = FALSE, xlab = '', main = 'Humidity (Lag = 1)')
hist(gibbs.out1$theta[, 9, ], freq = FALSE, xlab = '', main = 'WindSpeed(Lag = 1)')
hist(gibbs.out1$theta[, 10, ], freq = FALSE, xlab = '', main = 'GDF (Lag = 1)')
hist(gibbs.out1$theta[, 11, ], freq = FALSE, xlab = '', main = 'DF (Lag = 1)')
#####

data.frame(
  time = 1:tm,
  mean = mean[, 6],
  lower = lower[, 6],
  upper = upper[, 6]
) %>%
  ggplot(df, aes(x = time)) +
  geom_ribbon(aes(ymin = lower, ymax = upper), fill = "lightblue", alpha = 0.4) +
  geom_line(aes(y = mean), color = "blue", size = 1) +
  labs(
    title = "Estimated autocorrelation",
    x = "Time",
    y = "Value"
  ) +
  theme_minimal()


lower2 <- NULL
upper2 <- NULL
tm <- 47952
for (i in 1:tm) {
  lower2 <- rbind(lower2, apply(gibbs.out2$theta[i, , ], 1, function(x) sort(x)[250]))
  upper2 <- rbind(upper2, apply(gibbs.out2$theta[i, , ], 1, function(x) sort(x)[9750]))
}

mean2 <- NULL
for (i in 1:tm) {
  mean2 <- rbind(mean2, apply(gibbs.out2$theta[i, , ], 1, mean))
}

saveRDS(upper2, 'RData/upper2.RData')
saveRDS(lower2, 'RData/lower2.RData')
saveRDS(mean2, 'RData/mean2.RData')


time_seq <- 1:tm
p6 <- data.frame(
  time_seq = time_seq,
  mean = mean2[, 1],
  lower = lower2[, 1],
  upper = upper2[, 1]
) %>%
  ggplot(aes(x = time_seq)) +
  geom_ribbon(aes(ymin = lower, ymax = upper), fill = "lightblue", alpha = 0.4) +
  geom_line(aes(y = mean), color = "blue", linewidth = 1) +
  labs(
    title = "Autocorrelation (Lag = 1)",
    x = "Time",
    y = "Value"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    plot.title = element_text(size = 16, face = "bold")  # Title larger and bold
  )

p7 <- data.frame(
  time_seq = time_seq,
  mean = mean2[, 2],
  lower = lower2[, 2],
  upper = upper2[, 2]
) %>%
  ggplot(aes(x = time_seq)) +
  geom_ribbon(aes(ymin = lower, ymax = upper), fill = "lightblue", alpha = 0.4) +
  geom_line(aes(y = mean), color = "blue", linewidth = 1) +
  labs(
    title = "Autocorrelation (Lag = 2)",
    x = "Time",
    y = "Value"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    plot.title = element_text(size = 16, face = "bold")  # Title larger and bold
  )

p8 <- data.frame(
  time_seq = time_seq,
  mean = mean2[, 3],
  lower = lower2[, 3],
  upper = upper2[, 3]
) %>%
  ggplot(aes(x = time_seq)) +
  geom_ribbon(aes(ymin = lower, ymax = upper), fill = "lightblue", alpha = 0.4) +
  geom_line(aes(y = mean), color = "blue", linewidth = 1) +
  labs(
    title = "Autocorrelation (Lag = 144)",
    x = "Time",
    y = "Value"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    plot.title = element_text(size = 16, face = "bold")  # Title larger and bold
  )

p9 <- data.frame(
  time_seq = time_seq,
  mean = mean2[, 4],
  lower = lower2[, 4],
  upper = upper2[, 4]
) %>%
  ggplot(aes(x = time_seq)) +
  geom_ribbon(aes(ymin = lower, ymax = upper), fill = "lightblue", alpha = 0.4) +
  geom_line(aes(y = mean), color = "blue", linewidth = 1) +
  labs(
    title = "Autocorrelation (Lag = 145)",
    x = "Time",
    y = "Value"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    plot.title = element_text(size = 16, face = "bold")  # Title larger and bold
  )

p10 <- data.frame(
  time_seq = time_seq,
  mean = mean2[, 5],
  lower = lower2[, 5],
  upper = upper2[, 5]
) %>%
  ggplot(aes(x = time_seq)) +
  geom_ribbon(aes(ymin = lower, ymax = upper), fill = "lightblue", alpha = 0.4) +
  geom_line(aes(y = mean), color = "blue", linewidth = 1) +
  labs(
    title = "Cross-correlation (Zone 1)",
    x = "Time",
    y = "Value"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    plot.title = element_text(size = 16, face = "bold")  # Title larger and bold
  )

p11 <- data.frame(
  time_seq = time_seq,
  mean = mean2[, 6],
  lower = lower2[, 6],
  upper = upper2[, 6]
) %>%
  ggplot(aes(x = time_seq)) +
  geom_ribbon(aes(ymin = lower, ymax = upper), fill = "lightblue", alpha = 0.4) +
  geom_line(aes(y = mean), color = "blue", linewidth = 1) +
  labs(
    title = "Cross-correlation (Zone 3)",
    x = "Time",
    y = "Value"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    plot.title = element_text(size = 16, face = "bold")  # Title larger and bold
  )

library(cowplot)
plot_grid(p6, p7, p8, p9, p10, p11, nrow = 2)

#####
par(mfrow = c(2, 3))
hist(gibbs.out2$theta[, 1, ], freq = FALSE, xlab = '', main = 'Autocorrelation (Lag = 1)')
hist(gibbs.out2$theta[, 2, ], freq = FALSE, xlab = '', main = 'Autocorrelation (Lag = 2)')
hist(gibbs.out2$theta[, 3, ], freq = FALSE, xlab = '', main = 'Autocorrelation (Lag = 144)')
hist(gibbs.out2$theta[, 4, ], freq = FALSE, xlab = '', main = 'Autocorrelation (Lag = 145)')
hist(gibbs.out2$theta[, 5, ], freq = FALSE, xlab = '', main = 'Cross-correlation (Zone 1)')
hist(gibbs.out2$theta[, 6, ], freq = FALSE, xlab = '', main = 'Cross-correlation (Zone 3)')
#####

#####
par(mfrow = c(2, 3))
hist(gibbs.out2$theta[, 7, ], freq = FALSE, xlab = '', main = 'Temp (Lag = 1)')
hist(gibbs.out2$theta[, 8, ], freq = FALSE, xlab = '', main = 'Humidity (Lag = 1)')
hist(gibbs.out2$theta[, 9, ], freq = FALSE, xlab = '', main = 'WindSpeed (Lag = 1)')
hist(gibbs.out2$theta[, 10, ], freq = FALSE, xlab = '', main = 'GDF (Lag = 1)')
hist(gibbs.out2$theta[, 11, ], freq = FALSE, xlab = '', main = 'DF (Lag = 1)')
#####

lower3 <- NULL
upper3 <- NULL
tm <- 43632
for (i in 1:tm) {
  lower3 <- rbind(lower3, apply(gibbs.out3$theta[i, , ], 1, function(x) sort(x)[500]))
  upper3 <- rbind(upper3, apply(gibbs.out3$theta[i, , ], 1, function(x) sort(x)[9750 * 2]))
}

mean3 <- NULL
for (i in 1:tm) {
  mean3 <- rbind(mean3, apply(gibbs.out3$theta[i, , ], 1, mean))
}
saveRDS(upper3, 'RData/upper3.RData')
saveRDS(lower3, 'RData/lower3.RData')
saveRDS(mean3, 'RData/mean3.RData')

#####
par(mfrow = c(2, 3))
hist(gibbs.out3$theta[, 1, ], freq = FALSE, xlab = '', main = 'Autocorrelation (Lag = 1)')
hist(gibbs.out3$theta[, 2, ], freq = FALSE, xlab = '', main = 'Autocorrelation (Lag = 2)')
hist(gibbs.out3$theta[, 3, ], freq = FALSE, xlab = '', main = 'Autocorrelation (Lag = 144)')
hist(gibbs.out3$theta[, 4, ], freq = FALSE, xlab = '', main = 'Autocorrelation (Lag = 145)')
hist(gibbs.out3$theta[, 5, ], freq = FALSE, xlab = '', main = 'Cross-correlation (Zone 1)')
hist(gibbs.out3$theta[, 6, ], freq = FALSE, xlab = '', main = 'Cross-correlation (Zone 2)')
#####

#####
par(mfrow = c(2, 3))
hist(gibbs.out3$theta[, 7, ], freq = FALSE, xlab = '', main = 'Temp (Lag = 1)')
hist(gibbs.out3$theta[, 8, ], freq = FALSE, xlab = '', main = 'Humidity (Lag = 1)')
hist(gibbs.out3$theta[, 9, ], freq = FALSE, xlab = '', main = 'WindSpeed (Lag = 1)')
hist(gibbs.out3$theta[, 10, ], freq = FALSE, xlab = '', main = 'GDF (Lag = 1)')
hist(gibbs.out3$theta[, 11, ], freq = FALSE, xlab = '', main = 'DF (Lag = 1)')
#####
