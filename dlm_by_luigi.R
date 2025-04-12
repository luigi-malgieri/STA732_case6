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

# Define test set: set future Zone3 to NA
train_data[as.numeric(train_data$month) == 12, c("Zone1", "Zone2", "Zone3")] <- NA
train_data[train_data$month == 11, "Zone3"] <- NA

# Add lag features
train_data <- train_data %>%
  arrange(time_index) %>%
  mutate(
    Zone1_lag1   = lag(Zone1, 1),
    Zone1_lag2   = lag(Zone1, 2),
    Zone1_lag144 = lag(Zone1, 144),
    Zone1_lag145 = lag(Zone1, 145)
  )
train_data <- na.omit(train_data)

train_data <- train_data[ , !(names(train_data) %in% c("DateTime", "month", "year", "time_index")) ]
train_data <- as.data.frame(scale(train_data))

###################################################################################

lm.out<-lm(Zone1 ~., train_data)

Y <- as.vector(train_data[["Zone1"]])
X <- select(train_data, -Zone1)

# Y <- Y[1:50]
# X <- X[1:50,]

# Initialising state vector at the linear regression coefficient
mu0<-as.numeric(coefficients(lm.out))[-1]
p<-length(mu0) # dimension of state vector
sigma0<-summary(lm.out)$sigma
c0<-diag((sigma0^2)*summary(lm.out)$cov.unscaled)[-1]

dlmFun<-function(z){
  return(
    dlmModReg(X,addInt=F,dV=exp(z[1]),dW=exp(z[2:length(z)]),
              m0=mu0,C0=diag(c0))
  )
}
ldw<-c0
ldw<-(ldw/1000)^2
initials<-log(c(sigma0^2,ldw))

par 
for (i in 1:6) {
  plot(gibbs.out$dW[, i], type = 'l')
}

apply(gibbs.out$dW, 2, effectiveSize)


#### MLE ####
system.time(
  mle.out<-dlmMLE(y=Y,parm=initials,build=dlmFun,hessian=TRUE,
                  control=list(maxit=500))
)

parVar<-solve(mle.out$hessian)
cbind(mle.out$par,sqrt(diag(parVar)))

### Gibbs ####
system.time(
  gibbs.out<-dlmGibbsDIG(y=Y,mod=dlmModReg(X,addInt=F,dV=0.15,
                                           ##dW=rep(0.01,ncol(X)+1),
                                           dW=c(rep(0,5),rep(0.01,6)),
                                           m0=mu0,C0=diag(c0)),
                         shape.y=1.0,rate.y=1.0,
                         shape.theta=1.0,rate.theta=1.0,n.sample=2000,
                         ind=6:11)
)
plot(gibbs.out$theta[5,1,])
