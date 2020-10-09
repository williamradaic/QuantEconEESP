library(tseries)
library(forecast)
library(ggplot2)
library(ggthemes)
library(quantmod)
library(dplyr)
library(tidyverse)
library(data.table)
library(readr)
library(plotly)


#

petr3 <- getSymbols("PETR3.SA", src = "yahoo", from = "2015-01-01", to = "2020-10-02", auto.assign = F)

vale3 <- getSymbols("VALE3.SA", src = "yahoo", from = "2015-01-01", to = "2020-10-02", auto.assign = F)

itub4 <- getSymbols("ITUB4.SA", src = "yahoo", from = "2015-01-01", to = "2020-10-02", auto.assign = F)

oibr4 <- getSymbols("OIBR4.SA", src = "yahoo", from = "2015-01-01", to = "2020-10-02", auto.assign = F)

show3 <- getSymbols("SHOW3.SA", src = "yahoo", from = "2015-01-01", to = "2020-10-02", auto.assign = F)

intraday_b3_2 <- read_csv("~/econo/intraday_b3_2.csv")

#Fato #1: testar a série, série ao quadrado e log da série

serie <- petr3

colnames(serie) <- c("open", "high", "low", "close", "volume", "adjusted")

dserie <- Delt(serie$adjusted)

ggplotly(autoplot(dserie))

plotly::ggplotly()

nk <- auto.arima(serie$adjusted)

nk2 <- auto.arima((serie$adjusted^2))

nkl <- auto.arima(log(serie$adjusted))

k <- auto.arima(dserie)

k2 <- auto.arima((dserie^2))

kl <- auto.arima(log(dserie)) #Isso aqui não funcionou porque R hurr durr

#Fato #2: média temporal dos retornos; usar o TLC para construir um intervalo de confiança

f2 <- lm(serie$adjusted ~ 1)

f22 <- lm(dserie ~ 1)


#Fato #3: teste de Jarque-Bera (testa assimetria e curtose de uma dist. normal) -> unicaudal pois são leptocurticas

#Determinar a distribuição da curtose amostral

#Fato #4: correlograma ao quadrado

#Modelos ARCH e GARCH?

#Fato #5: dummy de segunda-feira (entra no modelo da variância)

#Fato #6: modelar por um TARCH para abertura e fechamento (dados intra-day)

#Fato #7: inserir uma função indicadora para 1 quando o erro for negativo e 0 caso contrário
