# This is a badly-constructed package...
library(bayesforecast)
library(tidyverse)
df <- read.csv('powerConsumption.csv')
zone1 <- ts(df$Zone1, frequency = 144)
Xreg <- df %>%
  select(Temp, Humidity, WindSpeed, GDF, DF) %>%
  as.matrix()
system.time(
  auto_sarimax1 <- auto.sarima(zone1,
                               seasonal = TRUE,
                               xreg = Xreg, 
                               chains = 4,
                               iter = 5e3, warmup = floor(iter / 2), 
                               adapt.delta = 0.9,
                               tree.depth = 10,
                               stepwise = TRUE, 
                               series.name = NULL, 
                               prior_mu0 = NULL,
                               prior_sigma0 = NULL,
                               prior_ar = NULL, 
                               prior_ma = NULL, 
                               prior_sar = NULL,
                               prior_sma = NULL, 
                               prior_breg = NULL)
)
# Error : object 'iter' not found
# In addition: Warning message:
#  In Sarima(ts, order = ord, seasonal = season, period = arma[5],  :
#              seasonal difference is not allowed in dynamic regressions D  = 0 
#            
# error in specifying arguments; sampling not done
# Error: object 'iter' not found
# In addition: Warning messages:
# 1: In varstan(model = dat, chains = chains, iter = iter, warmup = warmup,  :
#   restarting interrupted promise evaluation
# 2: In varstan(model = dat, chains = chains, iter = iter, warmup = warmup,  :
#   restarting interrupted promise evaluation
# Timing stopped at: 7072 1.388e+04 5103



