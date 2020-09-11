library(readxl)
library(ggplot2)
library(forecast)
library(dynlm)
library(ggthemes)
library(strucchange)
library(lmtest)
library(car)
library(dplyr)

df <- read_excel("C:/Users/vinic/OneDrive/Documents/QuantEconEESP/pibeua_real.xlsx")

series <- ts(df$`Cresimento percentual`[13:236], start = c(1950,1), end = c(2005,4), frequency = 4) # 1950-2005

autoplot(series) + theme_few() + ggtitle("US GDP, 1950-2005")

# We can clearly see a reduction in variance during the 80s.

df$observ <- 1:length(df$`PIB nominal`)

# Suppose that the break happens at time t = 153 (Q1, 1985).

df$d <- as.numeric(df$observ > 152)

m1 <- lm(series ~ df$d[13:236])

summary(m1)

m2 <- dynlm(series ~ df$d[13:236] + L(series, 1) + L(series, 1)*df$d[13:236])

df$d[13:236]

dummy

summary(m2)

# ARIMA model

arima_unr <- arima(series, order = c(1,0,0))

arima_r1 <- arima(series[1:152], order = c(1,0,0))

arima_r2 <- arima(series[153:length(series)], order = c(1,0,0))

ssr_unr <- sum(arima_unr$residuals^2)

ssr_r1 <- sum(arima_r1$residuals^2)

ssr_r2 <- sum(arima_r2$residuals^2)

# We will now define the Chow test for the null H0: \beta_m1 - \beta_m2 = 0 (no structural break)

chow <- function(SSR_unr, SSR_r1, SSR_r2, t, n) {
  
  ((SSR_unr - SSR_r1 - SSR_r2)/n)/((SSR_r1 + SSR_r2)/(t-2*n))
  
}


chow(SSR_unr = ssr_unr, SSR_r1 = ssr_r1, SSR_r2 = ssr_r2, t = length(series), n = length(arima_unr$coef)) # T statistic (n, T - 2n).

# Now, suppose that we do not know when the break happened.

t0 = 45
tf = 180 # Boundaries for the process.

models = list(NA)

coefs = matrix(NA, nrow = length(t0:tf), ncol = 2)

forecasts = list(NA)

ci = data.frame(matrix(NA, nrow = length(t0:tf), ncol = 5))

e = matrix(NA, nrow = length(t0:tf), ncol = 1)


# 1. Plotting coefficients. 

for (i in (1:(tf-t0))) {
  
  models[[i]] = arima(series[1:(i+t0)], order = c(1,0,0))
  
  coefs[i,] = models[[i]]$coef
  
  forecasts[[i]] = forecast(series[1:(i+t0)], model = models[[i]], h = 1)
  
  e[i,] = forecasts[[i]]$mean - series[(i+t0+1)]
  
}

coefs = data.frame(coefs)

coefs = data.frame(coefs, (1:length(coefs$X1)))

df.coefs = na.omit(data.frame(coefs))

names(df.coefs) = c("ar1", "intercept", "index")

ggplot(df.coefs, aes(x = index, y = ar1)) + geom_line() + theme_few()

ggplot(df.coefs, aes(x = index, y = intercept)) + geom_line() + theme_few()


# 2. Cusum test.

cusums = matrix(NA, nrow = length(e), ncol = 1)

e = na.omit(e)

for (i in 1:(length(e))) {
  
cusums[i,] = sum(e[1:i])/sd(e)  
  
}


cusums = na.omit(cusums)

df.cusums = data.frame(cusums)

df.cusums

ggplot(df.cusums, aes(x = (1:length(cusums)), y = cusums)) + geom_line() + theme_few()


# 3. Recursive F-tests. (Vinicius Marcial protesta formalmente contra o uso da palavra recursivo quando isso é claramente iterativo)

models_unr = list(NA)

models_r = list(NA)


#dummy <- df$d[13:236]

num_series = as.numeric(series)

f_values = matrix(NA, nrow = length(num_series), ncol = 2)

hyp = c(0,-1,1,0)

rhs = 0

length((t0:tf))

dummies = data.frame(matrix(NA, ncol = 224, nrow = 224))

for (i in (13:236)) {
  dummies[i] = as.numeric(df$observ[13:236] >= i)
}

dummies



for (i in (1:(tf-t0))) {
  
  adummy <- rep(0, 1)
  j <- 0
  k <- t0 + i
  while (j <= length(num_series)){
    if (j > k){#não sei se isso deveria ser maior ou igual ou só maior
      adummy[j] <- 1
    }
    else{
      adummy[j] <- 0
    }
    j <- j+1
  }
  models_unr[[i]] <- dynlm(num_series ~ adummy + lag(num_series) + lag(num_series)*adummy)
  f_values[i,1] = linearHypothesis(models_unr[[i]], hyp, rhs)$F[1]
  f_values[i,2] = linearHypothesis(models_unr[[i]], hyp, rhs)$F[2] #eu acho que funcionou?????
  
}

models_unr

f_values

coeftest()

modelttt = dynlm(series ~ dummies[1:tf,100] + lag(series[(1:tf)]) + lag(series[(1:tf)])*dummies[1:tf,100])



ffffff = linearHypothesis(m2, hyp, rhs)

ffffff$F

m2

dummies = data.frame(matrix(NA, ncol = 135, nrow = 290))



datas <- dados[133:172,1]
chows <- numeric(length = 40)
testes <- cbind.data.frame(datas, chows)

for(i in 1:39){
  
  arima_total <- arima(serie, order = c(1,0,0))
  arima_antes <- arima(serie[1:132+i], order = c(1,0,0))
  arima_depois <- arima(serie[132+i+1:233], order = c(1,0,0))
  
  SSR_total <- sum((arima_total$residuals)^2)
  SSR_antes <- sum((arima_antes$residuals)^2)
  SSR_depois <- sum((arima_depois$residuals)^2)
  
  chows[i] <- chow(SSR_total, SSR_antes, SSR_depois, Size = Size, n = n)
}