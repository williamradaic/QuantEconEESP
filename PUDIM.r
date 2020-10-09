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
library(rugarch)

#

petr3 <- getSymbols("PETR3.SA", src = "yahoo", from = "2015-01-01", to = "2020-10-02", auto.assign = F)

vale3 <- getSymbols("VALE3.SA", src = "yahoo", from = "2015-01-01", to = "2020-10-02", auto.assign = F)

itub4 <- getSymbols("ITUB4.SA", src = "yahoo", from = "2015-01-01", to = "2020-10-02", auto.assign = F)

oibr4 <- getSymbols("OIBR4.SA", src = "yahoo", from = "2015-01-01", to = "2020-10-02", auto.assign = F)

show3 <- getSymbols("SHOW3.SA", src = "yahoo", from = "2015-01-01", to = "2020-10-02", auto.assign = F)


#Fato #1: testar a série, série ao quadrado e log da série

serie <- petr3

colnames(serie) <- c("open", "high", "low", "close", "volume", "adjusted")

dserie <- Delt(serie$adjusted)

ggplotly(autoplot(dserie))

nk <- auto.arima(serie$adjusted)

nk2 <- auto.arima((serie$adjusted^2))

nkl <- auto.arima(log(serie$adjusted))

k <- auto.arima(dserie)

k2 <- auto.arima((dserie^2))

kl <- auto.arima(log(dserie)) #Isso aqui não funcionou porque R hurr durr

#Fato #2: média temporal dos retornos; usar o TLC para construir um intervalo de confiança


f22 <- lm(dserie ~ 1) # Não rejeitamos a nula


#Fato #3: teste de Jarque-Bera (testa assimetria e curtose de uma dist. normal) -> unicaudal pois são leptocurticas

#Determinar a distribuição da curtose amostral

#Fato #4: correlograma ao quadrado

#Modelos ARCH e GARCH?

#Fato #5: dummy de segunda-feira (entra no modelo da variância)

#Fato #6: modelar por um TARCH para abertura e fechamento (dados intra-day)

intraday.df <- read_csv("~/intraday_b3_3.csv")
intraday = intraday.df[1:6]
colnames(intraday) = c("Date", "ITUB4.SA", "OIBR4.SA", "PETR4.SA", "SHOW3.SA", "VALE3.SA")
intraday = intraday[3:length(intraday$ITUB4.SA),]

intraday = intraday %>% mutate(ITUB4.SA = as.numeric(ITUB4.SA)) %>% mutate(OIBR4.SA = as.numeric(OIBR4.SA)) %>% mutate(PETR4.SA = as.numeric(PETR4.SA)) %>% mutate(SHOW3.SA = as.numeric(SHOW3.SA)) %>% mutate(VALE3.SA = as.numeric(VALE3.SA)) 

i = 1
oc = matrix(NA, nrow =0, ncol = 0)
while (i <= 34) { 
  oc = append(oc, c(rep(1, 4), rep(0, 20), rep(1,4)))
  i = i+1
  }
intraday$oc = oc

intraday = na.omit(intraday)

model_spec= ugarchspec(variance.model = (list(model = "fGARCH", submodel = "TGARCH", external.regressors = as.matrix(intraday$oc), garchOrder = c(2,2))))

tarch.itub4 = ugarchfit(model_spec, data = intraday$ITUB4.SA)
tarch.vale3 = ugarchfit(model_spec, data = intraday$VALE3.SA)
tarch.show3 = ugarchfit(model_spec, data = intraday$SHOW3.SA)
tarch.petr4 = ugarchfit(model_spec, data = intraday$PETR4.SA)
tarch.oibr4 = ugarchfit(model_spec, data = intraday$OIBR4.SA)

#Fato #7: inserir uma função indicadora para 1 quando o erro for negativo e 0 caso contrário

neg = matrix(NA, nrow = length(dserie), ncol = 1)

dserie = na.omit(dserie)

for (i in (1:length(dserie))) {
  neg[i] = as.numeric(dserie[i] < 0) 
  }
neg

model_spec_neg = ugarchspec(variance.model = (list(model = "eGARCH", external.regressors = as.matrix(neg), garchOrder = c(2,2))))

dserie.num = as.numeric(dserie)


egarch.dserie = ugarchfit(model_spec_neg, data = dserie.num)













