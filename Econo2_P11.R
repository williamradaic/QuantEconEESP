library(ggplot2)
library(forecast)
library(vars)
library(readr)

# Documentação pra depois
# https://cran.r-project.org/web/packages/vars/vignettes/vars.pdf
# https://cran.r-project.org/web/packages/vars/vars.pdf


base <- read.csv("C:\\Users\\vinic\\OneDrive\\Documents\\Codigos R(4 Semestre)\\VAR_dados.csv")
base <- read_csv("VAR_dados.csv")


ind <- diff(base$Industria)
ibov <- base$Ibovespa
comm <- diff(base$Commodities)
ipca <- base$IPCA
juros <- diff(base$Juros)


endIPCA <- cbind(juros, ipca, ibov, ind, comm)
endIPCA

k <- VARselect(endIPCA, season = 12)

amat <- cbind(c(NA, NA, NA, NA, NA),c(NA, NA, NA, NA, NA),c(NA, NA, NA, NA, NA),c(NA, NA, NA, NA, NA),c(NA, NA, NA, NA, NA))
amat <- diag(5)
for (i in 1:5){
  for (j in 1:5){
    if (i<j){
      amat[i,j] <- NA
    }
  }
}
bmat <- diag(5)
for (i in 1:5){
  for (j in 1:5){
    if (i==j){
      bmat[i,j] <- NA
    }
  }
}

tIND <- VAR(y = endIPCA, p=4, season = 12)
stIND <- SVAR(tIND, Amat = amat, Bmat = bmat)

irf(stIND, impulse = "juros")

plot(irf(stIND, impulse = "juros"))

plot(fevd(stIND))

causality(tIND)

predict(tIND)

vars::fanchart(predict(tIND))

?fevd
