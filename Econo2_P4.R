## ----setup, include=FALSE----------------------------------------------------------------------------------------------------------------------------
knitr::opts_chunk$set(echo = TRUE)
library(ggplot2)
library(forecast)
library(AER)
library(tidyverse)
library(tidyr)
library(dplyr)
library(corrplot)
library(ggthemes)
library(gridExtra)
library(gtsummary)
library(readxl)
library(ggthemes)
library(mdscore)
library(readxl)
library(fpp)


## ----database, include = FALSE-----------------------------------------------------------------------------------------------------------------------

# Loading the dataframe:


df <- read_excel("Base de dados Problema 4.xlsx")

names(df)[1] <- "t"
names(df)[2] <- "value"


# df <- ts(df)





## ----plots-------------------------------------------------------------------------------------------------------------------------------------------

df <- data.frame(df)

pplot <- ggplot(data = df, aes(x = t, y = value)) + geom_line() + ggtitle("Time series plot") + theme_few()
pplot 

acf_ts <- Acf(df$value, lag.max = 5000)
acf_test_values <- acf_ts$acf/sd(acf_ts$acf)

head(data.frame(acf_test_values))

facst <- ggAcf(df$value, type = "correlation", lag.max = 20, plot = T) + theme_few()
faclt <- ggAcf(df$value, type = "correlation", lag.max = 5000, plot = T) + theme_few()

facpst <- ggPacf(df$value, type = "correlation", lag.max = 100, plot = T) + theme_few()
facplt <- ggPacf(df$value, type = "correlation", lag.max = 5000, plot = T) + theme_few()

facst
faclt

facpst
facplt



## ----estimation autoarima----------------------------------------------------------------------------------------------------------------------------


aa_model <- auto.arima(df$value, num.cores = 24, max.d = 0, stepwise = F)

summary(aa_model)

print("t-values: ")

aa_t <- matrix(NA, nrow = aa_model$arma[1] + aa_model$arma[2])

for (i in c(1:4)) { 

aa_t[i] <- aa_model$coef[i]/sqrt(aa_model$var.coef[i,i])

}

aa_t <- data.frame(aa_t)

aa_t

aa_q <- Box.test(aa_model$residuals, lag = aa_model$arma[1] + aa_model$arma[2])
aa_q

criteria <- matrix(NA, nrow = 1, ncol = 3)

aa_criteria <- data.frame("MA(3)*", aa_model$aic, aa_model$bic)

names(aa_criteria) <- c("Model", "AIC", "BIC")

aa_criteria

fac_e <- ggAcf(aa_model$residuals, type = "correlation", lag.max = 20, plot = T) + theme_few()

facp_e <- ggPacf(aa_model$residuals, type = "correlation", lag.max = 20, plot = T) + theme_few()

fac_e
facp_e
mean(aa_model$residuals)



## ----cross validation manual-------------------------------------------------------------------------------------------------------------------------

h <- 5

frac <- 0.2

T <- length(df$value)

k <- floor((1-frac)*T)

# Estimating MA(3) with k = 80
fit <- Arima(df$value[1:k], order = c(0,0,3))

# Generating predictions from the model
pred <- predict(fit, n.ahead = h)

# Calculating errors between the predicted values of the model and the actual values of the testing database

e <- df$value[(k+h)] - pred$pred[h]

e


## ----cross validation manual 2-----------------------------------------------------------------------------------------------------------------------

e <- matrix(NA, nrow = 100)

# Updating the model

for (i in k:(T-h)) {

  fit <- Arima(df$value[1:i], order = c(0,0,3))
  
  pred <- predict(fit, n.ahead = h)
  
  e[i,1] <- df$value[(i+h)] - pred$pred[h]

}





## ----mse cross validation----------------------------------------------------------------------------------------------------------------------------

mse <- mean(e^2, na.rm = T)




## ----cross validation manual comparison--------------------------------------------------------------------------------------------------------------

max_p <- 5

max_q <- 5

e <- matrix(NA, nrow = 100, ncol = (max_p + 1) * (max_q + 1))

pred <- vector("list", (max_p + 1) * (max_q + 1))

fit <- vector("list", (max_p + 1) * (max_q + 1))

# Updating the model
for (u in 0:max_q) {

for (j in 0:max_p) { 


  for (i in k:(T-h)) {
  
 fit[[(((max_p+1)*j)+u + 1)]] <- Arima(df$value[1:i], order = c(j,0,u))
  
#  fit <- append(fit, Arima(df$value[1:i], order = c(j,0,u)))
  
# pred <- append(pred, predict(fit[[(j+u)]], n.ahead = h))
  
 pred[[(((max_p+1)*j)+u + 1)]] <- predict(fit[[(((max_p+1)*j)+u + 1)]], n.ahead = h)
  
 e[i,(((max_p+1)*j)+u + 1)] <- df$value[(i+h)] - pred[[(((max_p+1)*j)+u + 1)]]$pred[h]

  }
  
}
  
}
  
mse <- matrix(NA, nrow = ((max_p + 1) * (max_q + 1)), ncol = 1)




mse <- colMeans(e^2, na.rm = T)

mse

optimal_index <- which.min(mse)

cv_model <- fit[[optimal_index]]

  


## ----bootstrapping artesanal-------------------------------------------------------------------------------------------------------------------------

S <- 1000

m <- 100

optimal_p <- aa_model$arma[1]

optimal_q <- aa_model$arma[2]

e_sample <- data.frame(matrix(NA, nrow = S, ncol = (length(df$value)+m)))

y_star <- data.frame(matrix(NA, nrow = S, ncol = (length(df$value)+m + max(aa_model$arma[1], aa_model$arma[2]))))

arima_star <- data.frame(matrix(NA, nrow = S, ncol = (length(df$value)+ m + max(aa_model$arma[1], aa_model$arma[2]))))

for (i in 1:S) {
  
  e_sample[i] <- sample(aa_model$residuals, replace = T, size = (length(df$value)+m))
  
for (j in ((aa_model$arma[1] + aa_model$arma[2] + 1):(length(df$value)+m))) { 
  
  arima_star[i,j] <- (aa_model$coef[4] + (aa_model$coef[1] * e_sample[i,j-1]) + (aa_model$coef[2] * e_sample[i,j-2]) + (aa_model$coef[3] * e_sample[i,j-3]) + e_sample[i,j])

}

}


y_fixed <- data.frame(matrix(NA, nrow = S, ncol = (aa_model$arma[1] + aa_model$arma[2])))   
  
for (i in 1:S) {
  y_fixed[i,1] <- data.frame(df$value[1])
  y_fixed[i,2] <- data.frame(df$value[2])
  y_fixed[i,3] <- data.frame(df$value[3])
}


y_star <- data.frame(y_fixed, arima_star[,-(1:3)])

y_m <- y_star[,-(1:100)]

y_m <- y_m[,-(101:103)]

y_mt <- t(y_m)

y_matrix <- as.matrix(y_m)



## ----forecast bootstrapping--------------------------------------------------------------------------------------------------------------------------

fc_list <- vector("list", S)

for (i in 1:S) {
  
  fc_list[[i]] <- forecast(ts(y_matrix[i,]), model = aa_model, h = h)

}

fc_list[[1]]

fc_mean <- data.frame(matrix(NA, nrow = S, ncol = 5))

for (i in 1:S) {
  
  fc_mean[i,] <- fc_list[[i]]$mean
  
}



## ----mean ic-----------------------------------------------------------------------------------------------------------------------------------------

head(fc_mean)

hist_x1 <- ggplot(data = fc_mean, aes(x = X1)) + geom_histogram(bins = 40) + theme_few() 
hist_x1

qq_x1 <- qqnorm(fc_mean$X1); qqline(fc_mean$X1)

hist_x2 <- ggplot(data = fc_mean, aes(x = X2)) + geom_histogram(bins = 40) + theme_few() 
hist_x2

qq_x2 <- qqnorm(fc_mean$X2); qqline(fc_mean$X2)

hist_x3 <- ggplot(data = fc_mean, aes(x = X3)) + geom_histogram(bins = 40) + theme_few() 
hist_x3

qq_x3 <- qqnorm(fc_mean$X3); qqline(fc_mean$X3)

hist_x4 <- ggplot(data = fc_mean, aes(x = X4)) + geom_histogram(bins = 40) + theme_few() 
hist_x4

qq_x4 <- qqnorm(fc_mean$X4); qqline(fc_mean$X4)

hist_x5 <- ggplot(data = fc_mean, aes(x = X5)) + geom_histogram(bins = 100) + theme_few() 
hist_x5

qq_x5 <- qqnorm(fc_mean$X5); qqline(fc_mean$X5)




## ----forecast arima plots----------------------------------------------------------------------------------------------------------------------------

fc <- forecast(df$value, model = aa_model, h = h)

autoplot(fc) + theme_few()

autoplot(fc_list[[1]]) + theme_few()

autoplot(fc_list[[66]]) + theme_few()

autoplot(fc_list[[796]]) + theme_few()



