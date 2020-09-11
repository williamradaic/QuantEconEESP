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

m2 <- dynlm(series ~ df$d[13:236] + L(series, 1) + L(series, 1)*df$d[13:236])

summary(lm(serie ~ after_break[2:234]))
summary(dynlm(serie ~ after_break[2:234] + lag(serie) + after_break[2:234]*lag(serie)))

arima_total <- arima(serie, order = c(1,0,0))
arima_antes <- arima(serie[1:172], order = c(1,0,0))
arima_depois <- arima(serie[173:233], order = c(1,0,0))

SSR_total <- sum((arima_total$residuals)^2)
SSR_antes <- sum((arima_antes$residuals)^2)
SSR_depois <- sum((arima_depois$residuals)^2)

chow <- function(SSR, SSR1, SSR2, Size, n){
  ((SSR - SSR1 - SSR2)/n)/((SSR1 + SSR2)/(Size - 2*n))
}

Size <- length(serie)
p <- 1
q <- 0
n <- p + q + 1

chow(SSR_total, SSR_antes, SSR_depois, Size = Size, n = n)
#Estatística F com (n, T - 2n) graus de liberdade

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