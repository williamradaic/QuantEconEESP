library(ggplot2)
library(forecast)
library(vars)
library(readr)
library(readxl)
library(forecast)
library(ggthemes)

library(urca)
library(dynlm)
library(tsDyn)
library(tseries)

df <- read_excel("Base de Dados - Problema 12.xlsx")

colnames(df) = c("Date", "USD", "IPCA", "CPI")

serie <- ts(log(df[2:4]), start = c(1995, 1), end = c(2020, 8), frequency = 12)

usd <- serie[,1]
ipca <- serie[,2]
cpi <- serie[,3]

plot(serie)

ggAcf(serie) + theme_few()
ggPacf(serie) + theme_few()

### PPP implies: p_t(BR) = s_t(exchange) + p_t^*(US) ### (log)
### z_t = p_t(BR) - s_t(exchange) - p_t^*(US) ### (log)
### PPP postulates that z_t is stationary. ### 

# Unit root tests for all ts

summary(ur.df(usd))
summary(ur.df(ipca))
summary(ur.df(cpi))

adf.test(usd)
adf.test(ipca)
adf.test(cpi)

# Unable to reject H_0 (unit root) at all conventional levels 

summary(ur.df(diff(usd)))
summary(ur.df(diff(ipca)))
summary(ur.df(diff(cpi)))

# Engle-Granger 

# Step 1: OLS 

eg <- dynlm(usd ~ ipca + cpi) # OLS shall capture the linear combination of cointegration basis vectors for optimal linear projection 
summary(eg)

autoplot(eg$residuals) + theme_few()

# Step 2: Unit root test

summary(ur.df(eg$residuals)) # still unit root. Note: Adjusted critical values? Hamilton 
adf.test(eg$residuals)


# Johansen test

trace <- ca.jo(serie, type = "trace", ecdet = "const", K = 12, spec = "transitory", season = 12)
summary(trace)

eigen <- ca.jo(serie, type = "eigen", ecdet = "const", K = 12, spec = "transitory", season = 12)
summary(eigen)

# VECM 

model <- VECM(serie, lag = 12, r = 1, include = "const", estim = "ML")
summary(model) # Sinais esquisitos

# Testing PPP for known a = (1, -1, -1)' -- Hamilton, p. 582.

z <- (log(ipca/1000) - log(usd) - log(cpi/1000))
adf.test(z) # Can't reject unit root for z. PPP is weak. Cf. Hamilton p. 585.
autoplot(z) + theme_few()
ggAcf(z) +theme_few()

# Forecast

fc = predict(model)
fc












