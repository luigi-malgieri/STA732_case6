set.seed(408)
library(dplyr)
library(lubridate)
library(ggplot2)
library(dlm)

# Load and preprocess data
powcon <- read.csv("powerConsumption.csv")
train_data <- powcon
train_data$DateTime <- as.POSIXct(train_data$DateTime, format = "%m/%d/%Y %H:%M")
train_data$month <- month(train_data$DateTime)
train_data$year <- year(train_data$DateTime)

# Time index in hours from start
train_data$time_index <- as.numeric(difftime(train_data$DateTime, min(train_data$DateTime), units = "hours"))

# # Define test set: set future Zone3 to NA
# train_data[as.numeric(train_data$month) == 12, c("Zone1", "Zone2", "Zone3")] <- NA
# train_data[train_data$month == 11, "Zone3"] <- NA

train_data_zone1 <- train_data %>%
  filter(month < 12) %>%
  arrange(time_index) %>%
  mutate(
    Zone1_lag1   = lag(Zone1, 1),
    Zone1_lag2   = lag(Zone1, 2),
    Zone1_lag144 = lag(Zone1, 144),
    Zone1_lag145 = lag(Zone1, 145),
    Zone2_lag = lag(Zone2, 1),
    Zone3_lag = lag(Zone3, 1),
    Temp_lag = lag(Temp, 1),
    Humidity_lag = lag(Humidity, 1),
    WindSpeed_lag = lag(WindSpeed, 1),
    GDF_lag = lag(GDF, 1),
    DF_lag = lag(DF, 1)
  ) %>%
  na.omit() %>%
  select(Zone1, Zone1_lag1, Zone1_lag2, Zone1_lag144, Zone1_lag145, Zone2_lag, Zone3_lag,
         Temp_lag, Humidity_lag, WindSpeed_lag, GDF_lag, DF_lag)

# train_data <- train_data[ , !(names(train_data) %in% c("DateTime", "month", "year", "time_index")) ]
# train_data <- train_data %>%
#   select(Zone1, Zone1_lag1, Zone1_lag2, Zone1_lag144, Zone1_lag145, Zone2_lag, Zone3_lag,
#          Temp_lag, Humidity_lag, WindSpeed_lag, GDF_lag, DF_lag)
train_data_zone1_scale <- as.data.frame(scale(train_data_zone1))

train_data_zone2 <- train_data %>%
  filter(month < 12) %>%
  arrange(time_index) %>%
  mutate(
    Zone2_lag1   = lag(Zone2, 1),
    Zone2_lag2   = lag(Zone2, 2),
    Zone2_lag144 = lag(Zone2, 144),
    Zone2_lag145 = lag(Zone2, 145),
    Zone1_lag = lag(Zone1, 1),
    Zone3_lag = lag(Zone3, 1),
    Temp_lag = lag(Temp, 1),
    Humidity_lag = lag(Humidity, 1),
    WindSpeed_lag = lag(WindSpeed, 1),
    GDF_lag = lag(GDF, 1),
    DF_lag = lag(DF, 1)
  ) %>%
  na.omit() %>%
  select(Zone2, Zone2_lag1, Zone2_lag2, Zone2_lag144, Zone2_lag145, Zone1_lag, Zone3_lag,
         Temp_lag, Humidity_lag, WindSpeed_lag, GDF_lag, DF_lag)

train_data_zone2_scale <- as.data.frame(scale(train_data_zone2))

#
train_data_zone3 <- train_data %>%
  filter(month < 11) %>%
  arrange(time_index) %>%
  mutate(
    Zone3_lag1   = lag(Zone3, 1),
    Zone3_lag2   = lag(Zone3, 2),
    Zone3_lag144 = lag(Zone3, 144),
    Zone3_lag145 = lag(Zone3, 145),
    Zone1_lag = lag(Zone1, 1),
    Zone2_lag = lag(Zone2, 1),
    Temp_lag = lag(Temp, 1),
    Humidity_lag = lag(Humidity, 1),
    WindSpeed_lag = lag(WindSpeed, 1),
    GDF_lag = lag(GDF, 1),
    DF_lag = lag(DF, 1)
  ) %>%
  na.omit() %>%
  select(Zone3, Zone3_lag1, Zone3_lag2, Zone3_lag144, Zone3_lag145, Zone1_lag, Zone2_lag,
         Temp_lag, Humidity_lag, WindSpeed_lag, GDF_lag, DF_lag)

train_data_zone3_scale <- as.data.frame(scale(train_data_zone3))

###################################################################################

lm.out1 <- lm(Zone1 ~., train_data_zone1_scale)

Y1 <- as.vector(train_data_zone1_scale[["Zone1"]])
X1 <- select(train_data_zone1_scale, -Zone1)

#
lm.out2 <- lm(Zone2 ~., train_data_zone2_scale)

Y2 <- as.vector(train_data_zone2_scale[["Zone2"]])
X2 <- select(train_data_zone2_scale, -Zone2)

#
lm.out3 <- lm(Zone3 ~., train_data_zone3_scale)

Y3 <- as.vector(train_data_zone3_scale[["Zone3"]])
X3 <- select(train_data_zone3_scale, -Zone3)

# Y <- Y[1:50]
# X <- X[1:50,]

# Initialising state vector at the linear regression coefficient
mu0.1 <- as.numeric(coefficients(lm.out1))[-1]
p1 <- length(mu0.1) # dimension of state vector
sigma0.1 <- summary(lm.out1)$sigma
c0.1 <- diag((sigma0.1 ^ 2) * summary(lm.out1)$cov.unscaled)[-1]

dlmFun1 <- function(z){
  return(
    dlmModReg(X1, addInt = F, dV = exp(z[1]), dW = exp(z[2:length(z)]),
              m0 = mu0.1, C0 = diag(c0.1))
  )
}
ldw1 <- c0.1
ldw1 <- (ldw1 / 1000) ^ 2
initials1 <- log(c(sigma0.1 ^ 2,ldw1))


#
mu0.2 <- as.numeric(coefficients(lm.out2))[-1]
p2 <- length(mu0.2) # dimension of state vector
sigma0.2 <- summary(lm.out2)$sigma
c0.2 <- diag((sigma0.2 ^ 2) * summary(lm.out2)$cov.unscaled)[-1]

dlmFun2 <- function(z){
  return(
    dlmModReg(X2, addInt = F, dV = exp(z[1]), dW = exp(z[2:length(z)]),
              m0 = mu0.2, C0 = diag(c0.2))
  )
}
ldw2 <- c0.2
ldw2 <- (ldw2 / 1000) ^ 2
initials2 <- log(c(sigma0.2 ^ 2,ldw2))


#
mu0.3 <- as.numeric(coefficients(lm.out3))[-1]
p3 <- length(mu0.3) # dimension of state vector
sigma0.3 <- summary(lm.out3)$sigma
c0.3 <- diag((sigma0.3 ^ 2) * summary(lm.out3)$cov.unscaled)[-1]

dlmFun3 <- function(z){
  return(
    dlmModReg(X3, addInt = F, dV = exp(z[1]), dW = exp(z[2:length(z)]),
              m0 = mu0.3, C0 = diag(c0.3))
  )
}
ldw3 <- c0.3
ldw3 <- (ldw3 / 1000)^2
initials3 <- log(c(sigma0.3 ^ 2,ldw3))

#### MLE ####
system.time(
  mle.out1 <- dlmMLE(y = Y1,parm = initials1, build = dlmFun1, hessian = TRUE,
                     control = list(maxit = 500))
)

parVar1 <- solve(mle.out1$hessian)
cbind(mle.out1$par,sqrt(diag(parVar1)))

#
system.time(
  mle.out2 <- dlmMLE(y = Y2,parm = initials2, build = dlmFun2, hessian = TRUE,
                     control = list(maxit = 500))
)

parVar2 <- solve(mle.out2$hessian)
cbind(mle.out2$par,sqrt(diag(parVar2)))

#
system.time(
  mle.out3 <- dlmMLE(y = Y3,parm = initials3, build = dlmFun3, hessian = TRUE,
                     control = list(maxit = 500))
)

parVar3 <- solve(mle.out3$hessian)
cbind(mle.out3$par,sqrt(diag(parVar3)))


### Gibbs ####
system.time(
  gibbs.out1 <- dlmGibbsDIG(y=Y1, mod = dlmModReg(X1, addInt = F,dV = 0.15,
                                           ##dW=rep(0.01,ncol(X)+1),
                                           dW = c(rep(0, 5), rep(0.01, 6)),
                                           m0 = mu0.1, C0 = diag(c0.1)),
                         shape.y = 1.0, rate.y = 1.0,
                         shape.theta = 1.0, rate.theta = 1.0, n.sample = 2e4,
                         ind = 6:11)
)

system.time(
  gibbs.out2 <- dlmGibbsDIG(y=Y2, mod = dlmModReg(X2, addInt = F,dV = 0.15,
                                                  ##dW=rep(0.01,ncol(X)+1),
                                                  dW = c(rep(0, 5), rep(0.01, 6)),
                                                  m0 = mu0.2, C0 = diag(c0.2)),
                            shape.y = 1.0, rate.y = 1.0,
                            shape.theta = 1.0, rate.theta = 1.0, n.sample = 2e4,
                            ind = 6:11)
)

system.time(
  gibbs.out3 <- dlmGibbsDIG(y=Y3, mod = dlmModReg(X3, addInt = F,dV = 0.15,
                                                  ##dW=rep(0.01,ncol(X)+1),
                                                  dW = c(rep(0, 5), rep(0.01, 6)),
                                                  m0 = mu0.3, C0 = diag(c0.3)),
                            shape.y = 1.0, rate.y = 1.0,
                            shape.theta = 1.0, rate.theta = 1.0, n.sample = 2e4,
                            ind = 6:11)
)
