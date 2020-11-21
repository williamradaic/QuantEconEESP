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

df <- read_excel("Dados - Problema 13.xlsx")
df = df[-1,-1]

View(df)

df[] = lapply(df, function(x) as.numeric(as.character(x)))

# função de PCA

PCA = function(M,k=dim(M)[1]){
  out = eigen(M)
  Pi = out$vector[,1:k]%*%sqrt(diag(out$values[1:k], nrow = length(out$values[1:k])))
  return = list(eigenvalues = out$values,
                eigenvectors = out$vectors,
                Pi= Pi)
}


n = dim(df)[2]
I0 = c(4,5,6,10,15,16) #Index de variáveis que devem ser I(0)
df[,-I0] = rbind(rep(NA,n-length(I0)), apply(df[,-I0],2,diff)) # Calcula a primeira diferença nas séries
df = df[!apply(is.na(df),1,any),] # Remove colunas com NA
T = dim(df)[1]

eigen(var(df))

x = matrix(NA, nrow = T, ncol = 1)

for (i in 1:T) for (j in 1:1:length(df*eigen(var(df))$vectors[,1])){ 
  x[i,] = df[i,]*eigen(var(df))$vectors[i,1]
  }






c = apply(df,2,mean) # media (centrar)

pca = PCA(var(df))
barplot(pca$eigenvalues/sum(pca$eigenvalues),
        xlab = "Componentes Principais",
        ylab = "% Exolicado da variância",
        names.arg = 1:dim(df)[2],
        ylim = c(0,1))

# 1o autovalor explica a maior parte da variância. Selecionamos apenas ele.

k = 1

d= rep(0,n)
Omega = var(df)
epsilon = 1e-3
continue = TRUE
count = 0
check = {}
while(continue){
  Omega = Omega - diag(d)
  Pi = PCA(Omega,k)$Pi
  d_next = diag(Omega - Pi%*%t(Pi))
  error = norm(as.matrix(d-d_next))
  check = c(check,error)
  continue = error>epsilon
  count = count + 1
  d = d_next
}
Pi = Pi
D = diag(d)



# Y_t = c + Pi * F_t + epsilon_t

# F_t = Phi1 F_t-1 + ... + Phip F_t-p + u_t

Y_cent = t(df) - c
Y_ibc = Y_cent[1,]

F_hat = matrix(NA, nrow = length(Y_cent[1,]), ncol = 1)

for (i in 1:length(Y_cent[1,])) {
  modelo = lm(Y_cent[i,] ~ Pi)
  F_hat[i] = (Pi * D**(-1) * t(Pi))**(-1) * t(Pi) * D**(-1) * Y_cent[,i]
}

summary(modelo)

modelo$rank

df_t = t(df) - c
View(df_t)

F_hat = ((Pi) * dd**(-1) * t(Pi))**(-1) * t(Pi) * D[1,1]**(-1) * Y_cent[1,]


length(Pi)
length(Y_ibc)



