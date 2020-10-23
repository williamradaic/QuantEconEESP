library(vars)
library(forecast)
library(ggplot2)


library(readr)
df <- read_csv("VAR_dados.csv")


df$X1=NULL
df = ts(df,frequency = 12)

plot.ts(df)

ind = diff(df[,"Industria"])
com = diff(df[,"Commodities"])
df = df[-1,]
df[,"Industria"] = ind
df[,"Commodities"] = com

plot.ts(df)

VARselect(df, lag.max = 12, season = 12)



# Cross validation

df <- data.frame(df)

max_p <- 20

frac = 0.2

h = 12

T = 187

k <- floor((1 - frac) * T)

error <- matrix(NA, nrow = (T-h)-k, ncol = 1)
mse <- matrix(NA, nrow = max_p, ncol = 1)

# CV para +12m de previsao 

for (p in 1:max_p) { 


for (i in k:(T-h)) {
  fit <- VAR(df[1:i,], p = p, season = 12)
  pred <- predict(fit, n.ahead = h)
  error[i] = df$Industria[(i+h)] - pred$fcst$Industria[h]
}
  
  mse[p] <- sum(error^2, na.rm = T)
  
}
  
which.min(mse)



# CV com erro acumulado mes a mes -- faz sentido isso?

# for (p in 1:max_p) { 
  
  
#  for (i in k:(T-h)) {
 #   fit <- VAR(df[1:i,], p = p, season = 12)
 #   pred <- predict(fit, n.ahead = h)
#    error[i] = sum(df$Industria[i:(i+h)]) - sum(pred$fcst$Industria[1:h])
#  }
  
 # mse[p] <- sum(error^2, na.rm = T)
  
#}

## forecasting plots

optimal_model = VAR(df, which.min(mse), season = 12)

fcst = predict(optimal_model, n.ahead = h)

vars::fanchart(fcst)

vars::fanchart(fcst, plot.type ="single")

final_ind = fcst$fcst$Industria[12]
final_ind














