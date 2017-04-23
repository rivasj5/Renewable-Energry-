# Needed libraries
library("tseries")
library(MASS)
source("bc2.R")
source("bulkfit.R")
source("decom1.R")

# Bring in renewable energy production data for world from 1960 - 2015
renew <- read.csv("~/RENEWABLE.csv")

# Subset on Untied States Production and remove variable not needed
renew.us <- renew[which(renew$LOCATION=='USA'),7]

# Convert production data into a time series for analysis
renew.us.ts <- ts(data = renew.us , start = c(1960,1), frequency = 1)

# Subset for training and testing 
train <-  ts(renew.us.ts[1:53],start = c(1960,1), frequency = 1)
test <- ts(renew.us.ts[54:55],start = c(2013,1), frequency = 1)

# Box-Cox transformation is not needed
bc2(train)

# Run bulkfit to see best model to fit
bulkfit(train)

# Train best models
mod1 <- arima(train,order = c(0,2,1)) 
mod2 <- arima(train,order = c(0,2,2)) 

# Predict last two periods
renew.pred1 <- predict(mod1, n.ahead = 2)
renew.pred2 <- predict(mod2, n.ahead = 2)

# Create lower and higher limit
renew.pred1.low <- renew.pred1$pred - 1.96*(renew.pred1$se/(as.numeric(length(train))))
renew.pred1.high <- renew.pred1$pred + 1.96*(renew.pred1$se/(as.numeric(length(train))))

renew.pred2.low <- renew.pred2$pred - 1.96*(renew.pred2$se/(as.numeric(length(train))))
renew.pred2.high <- renew.pred2$pred + 1.96*(renew.pred2$se/(as.numeric(length(train))))

# Matrix to combine all
pred1.mat <- cbind(renew.pred1$pred,renew.pred1.low,renew.pred1.high)
pred2.mat <- cbind(renew.pred2$pred,renew.pred2.low,renew.pred2.high)

# Decom on train
renew.dec <- decom1(train, fore1 = 2, se1= 1)
renew.final <- ts(renew.dec$pred.df,start = 2013, end = 2014, frequency = 1)

# Test actuals vs predicted
# Add actuals to final.mat & gnp.dec.final
final.mat1 <- cbind(test,pred1.mat)
final.mat2 <- cbind(test,pred2.mat)
final.mat3 <- cbind(test, renew.final)

# Column names
colnames(final.mat1) <- c("Actual","Predicted","Lower Limit","Upper Limit")
colnames(final.mat2) <- c("Actual","Predicted","Lower Limit","Upper Limit")
colnames(final.mat3) <- c("Actual","Predicted","Lower Limit","Upper Limit")

# Final matrix with actuals and predictions
final.mat1
final.mat2
final.mat3

