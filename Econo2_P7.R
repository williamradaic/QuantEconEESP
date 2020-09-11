library(readxl)
library(ggplot2)
library(forecast)
library(dynlm)
library(ggthemes)

df <- read_excel("pibeua_real.xlsx")

series <- ts(df$`Cresimento percentual`[13:236], start = c(1950,1), end = c(2005,4), frequency = 4) # 1950-2005

autoplot(series) + theme_few() + ggtitle("US GDP, 1950-2005")

# We can clearly see a reduction in variance during the 80s.

df$observ <- 1:length(df$`PIB nominal`)

# Suppose that the break happens at time t = 153 (Q1, 1985).

df$d <- as.numeric(df$observ > 152)

m1 <- lm(series ~ df$d[13:236])

summary(m1)

m2 <- dynlm(series ~ df$d[13:236] + L(series, 1) + L(series, 1)*df$d[13:236])

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


chow(ssr_unr, ssr_r1, ssr_r2, length(series), n = length(arima_unr$coef)) # T statistic (n, T - 2n).






























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