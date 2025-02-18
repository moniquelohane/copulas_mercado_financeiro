
# RESULTADOS


require(pacman)
p_load(tidyverse,lubridate,copula,fBasics,StableEstim,stabledist, knitr,extRemes,ismev,evmix,copula,extremis,DT,kableExtra,VGAM,evd,fExtremes, PerformanceAnalytics)

## leitura dos dados

if (length(args) > 1){
  pasta_raiz = args[1]
}else{
  pasta_raiz = dirname(rstudioapi::getSourceEditorContext()$path)
}

setwd(paste0(pasta_raiz,"./../data/raw"))

russell <- read.csv("RUT.csv")
msci <- read.csv("ECNS - Dados Históricos.csv", encoding = "UTF-8")


## RUSSELL - criando variavel de logretorno (serao utilizados como padrao o logretorno)
russell$retorno <- russell$High/dplyr::lag(russell$High)
russell$logretorno <- log(russell$retorno)
russell$logretorno <- ifelse(is.na(russell$logretorno),0, russell$logretorno)

## RUSSELL - selecionando as variaveis de estudo
logret_rus <- russell$logretorno
ret_rus <- russell$retorno
High_rus <- russell$High

## MSCI - criando variavel de logretorno (serao utilizados como padrao o logretorno) 
msci$retorno <- msci$High/dplyr::lag(msci$High)
msci$logretorno <- log(msci$retorno)
msci$logretorno <- ifelse(is.na(msci$logretorno),0, msci$logretorno)

# verificando variaveis criadas
head(msci)
head(russell)

## MSCI - selecionando as variaveis de estudo
logret_msci <- msci$logretorno
ret_msci <- msci$retorno
High_msci <- msci$High


# grafico dos valores reais
plot(High_rus, type = "l")
plot(High_msci, type = "l")

# grafico dos logretornos
data_rus <- as.Date(russell$Date)
plot(data_rus,logret_rus, type = "l", ylab = "Logretornos", xlab = "Tempo",
     main = "Logretonos diários do Índice Russell 2000 (EUA), 2018-2021")
data_msci <- as.Date(msci$Date)
plot(data_msci,logret_msci, type = "l", ylab = "Logretornos", xlab = "Tempo",
     main = "Logretonos diários do Índice MSCI (China), 2018-2021")

## histograma dos dados

par(mfrow = c(1,2))
x <- logret_rus
hist_dados<-hist(x,n=50,breaks = 30, prob=T,ylim=c(0,50),main = "Histograma dos logretornos - Russell 2000, EUA", ylab = "Densidade",xlab="Logretorno")
lines(seq(min(x,na.rm=T),max(x,na.rm=T),length=1000),
      dnorm(seq(min(x,na.rm=T),max(x,na.rm=T),
                length=1000),mean(x,na.rm=T),sd(x,na.rm=T)),lwd=2)

y <- logret_msci
hist_dados<-hist(y,n=50,breaks = 30, prob=T,ylim=c(0,50),main = "Histograma dos logretornos - MSCI, China", ylab = "Densidade",xlab="Logretorno")
lines(seq(min(x,na.rm=T),max(x,na.rm=T),length=1000),
      dnorm(seq(min(x,na.rm=T),max(x,na.rm=T),
                length=1000),mean(x,na.rm=T),sd(x,na.rm=T)),lwd=2)


# tabelas de medidas resumo

media_r <- mean(logret_rus)
varianc_r <- var(logret_rus)
dp_r <- sqrt(varianc_r)
indice_r <- "Russell 2000"
df_r <- data.frame(indice_r, media_r, varianc_r, dp_r)

media_m <- mean(logret_msci)
varianc_m <- var(logret_msci)
dp_m <- sqrt(varianc_m)
indice_m <- "MSCI"
df_m <- data.frame(indice_m, media_m, varianc_m, dp_m)
names(df_m) <- names(df_r)

df_final <- rbind(df_r, df_m)
names(df_final) <- c("Índice", "Média", "Variância", "Desvio padrão")

# falar que normal nao se ajusta e 
# introduzir gev

## Blocos máximos - russell

# teste_anterior <- NA
# existe_anterior <- FALSE
# primeira_nao_rejeicao <- TRUE

ret <- logret_rus
N<-length(ret) 
result <- data.frame() 

for (k in 1:50) {
  
  # calculando blocos maximos
  n<-k
  tau<-floor(N/n)
  m<-numeric(tau) ; j<-1
  for (i in 1:tau){
    m[i]<-max(ret[j:(j+n-1)])
    j<-j+n }
  m<-m[-1]
  
  # aplicando o teste de hipotese na k-ésima observacao
  teste<-Box.test(m, lag = 1, 
                  type = c("Box-Pierce", "Ljung-Box"), 
                  fitdf = 0)
  teste$indice <- k
  
  # salvando apenas variaveis de interesse
  teste <- c(teste$indice,teste$p.value)
  
  
  #verificando se a k-ésima variavel foi nao-rejeitada
  if(teste[2]>0.05){
    
    # caso ela seja a primeira a nao ser rejeitada
    # guarda-se tambem o caso anterior; caso ela seja a segunda ou mais
    # a ser nao rejeitada, ignora-se essa verificacao do caso anterior
    # if(primeira_nao_rejeicao == TRUE){
    #   
    #   primeira_nao_rejeicao = FALSE
    #   
    #   # verificando se a nao rejeicao ocorreu quando k=1;
    #   # nesse caso nao tem como guardar o valor anterior
    #   if(existe_anterior == TRUE){
    #     
    #     result <- rbind(result, teste_anterior)
    #     
    #   }
    # }
    # 
    
    
    result <- rbind(result, teste)
    
  }
  
  # teste_anterior <- teste
  # 
  # existe_anterior = TRUE
  # result <- rbind(result, teste)
  print(k)
}


result <- tibble(result)
names(result) <- c("Tamanho do bloco","P-valor (teste de Ljung-Box)")
# 37 

## Blocos máximos - MSCI

# teste_anterior <- NA
# existe_anterior <- FALSE
# primeira_nao_rejeicao <- TRUE

ret <- logret_msci
N<-length(ret) 
result <- data.frame() 

for (k in 1:50) {
  
  # calculando blocos maximos
  n<-k
  tau<-floor(N/n)
  m<-numeric(tau) ; j<-1
  for (i in 1:tau){
    m[i]<-max(ret[j:(j+n-1)])
    j<-j+n }
  m<-m[-1]
  
  # aplicando o teste de hipotese na k-ésima observacao
  teste<-Box.test(m, lag = 1, 
                  type = c("Box-Pierce", "Ljung-Box"), 
                  fitdf = 0)
  teste$indice <- k
  
  # salvando apenas variaveis de interesse
  teste <- c(teste$indice,teste$p.value)
  
  
  #verificando se a k-ésima variavel foi nao-rejeitada
  if(teste[2]>0.05){
    
    # caso ela seja a primeira a nao ser rejeitada
    # guarda-se tambem o caso anterior; caso ela seja a segunda ou mais
    # a ser nao rejeitada, ignora-se essa verificacao do caso anterior
    # if(primeira_nao_rejeicao == TRUE){
    #   
    #   primeira_nao_rejeicao = FALSE
    #   
    #   # verificando se a nao rejeicao ocorreu quando k=1;
    #   # nesse caso nao tem como guardar o valor anterior
    #   if(existe_anterior == TRUE){
    #     
    #     result <- rbind(result, teste_anterior)
    #     
    #   }
    # }
    # 
    
    
    result <- rbind(result, teste)
    
  }
  
  # teste_anterior <- teste
  # 
  # existe_anterior = TRUE
  # result <- rbind(result, teste)
  print(k)
}


result <- tibble(result)
names(result) <- c("Tamanho do bloco","P-valor (teste de Ljung-Box)")
# 10

## blocos maximos russell

ret <- russell$logretorno
N <- length(ret) ; n <- 37 # tamanho do bloco
tau <- floor(N/n) # numero de blocos = 20
m <- numeric(tau) ; j<-1
for (i in 1:tau){
  m[i]<-max(ret[j:(j+n-1)])
  j<-j+n }
m<-m[-1]

m_rus <- m

plot.new() 
par(mfrow=c(1,2))
{hist(m_rus, prob=T,
      ylim=c(0,60), 
      main = "Histograma dos máximos dos\nblocos de 37 dias da cotação\ndo Russell 2000 (EUA)",
      ylab = "Densidade",xlab = "Logretornos")
  lines(density(m_rus))}
plot(m_rus, type="l",main="Valores máximos do Índice Russell 2000 (EUA)", ylab="Logretornos") #Some extremes


## blocos maximos msci

ret_m <- msci$logretorno
N <- length(ret_m) ; n <- 37 # tamanho do bloco
tau <- floor(N/n) # numero de blocos = 20
m <- numeric(tau) ; j<-1
for (i in 1:tau){
  m[i]<-max(ret_m[j:(j+n-1)])
  j<-j+n }
m<-m[-1]

m_msci <- m

#plot.new() 
par(mfrow=c(1,2))
{hist(m_msci, 
      prob=T,
      ylim=c(0,60),
      main = "Histograma dos máximos dos\nblocos de 10 dias da cotação\ndo MSCI (China)",
      ylab = "Densidade",xlab = "Logretornos")
  lines(density(m_msci))}
plot(m_msci, type="l",main="Valores máximos do Índice MSCI (China)", ylab="Logretornos") #Some extremes

par(mfrow=c(1,1))

## Estimação por máxima verossimilhança da GEV

# russell

fit_rus <- fevd(m_rus,type="GEV")
par_gev_r<- as.data.frame(fit_rus$results$par)#positive shape estimate but 
par_gev_r$parametros <- c("mu","sigma","csi")
row.names(par_gev_r) <- NULL

par_gev_r$parametros <- factor(par_gev_r$parametros, 
                             levels=c("csi","mu","sigma"))
par_gev_r <- par_gev_r[order(par_gev_r$parametros),]
par_gev_r <- par_gev_r[,c(2,1)]
names(par_gev_r) <- c("Parâmetros","GEV")

#    Parâmetros         GEV
# 3        csi 0.645807777
# 1         mu 0.018178213
# 2      sigma 0.006690044


par(mfrow=c(2,2))
#x11()
plot(fit_rus)

library(fitdistrplus)
fw <- fitdist(m_rus, "weibull")
summary(fw)

plot(fw)

#x11()
plot(fw)
# ajuste ruim, rever numero de blocos
plot(fit_rus)

# msci

fit_msci <- fevd(m_msci,type="GEV")
par_gev_m<- as.data.frame(fit_msci$results$par)#positive shape estimate but 
par_gev_m$parametros <- c("mu","sigma","csi")
row.names(par_gev_m) <- NULL

par_gev_m$parametros <- factor(par_gev_m$parametros, 
                               levels=c("csi","mu","sigma"))
par_gev_m <- par_gev_m[order(par_gev_m$parametros),]
par_gev_m <- par_gev_m[,c(2,1)]
names(par_gev_m) <- c("Parâmetros","GEV")

# Parâmetros         GEV
# 3        csi 0.106610915 (forma)
# 1         mu 0.0256341003
# 2      sigma 0.007891253 (escala)


#x11()
plot(fit_msci)


### histograma com dados silumados versus dados reais

# histograma com as densidades aplicadas c devidos parametros
set.seed(2)
n <- 100

# uniforme
y <- runif(n,min=0,max=1)

amostra_r <- mu_r - (sigma_r/csi_r)*(1-(-log(y))^(csi_r))

amostra_m <- mu_m - (sigma_m/csi_m)*(1-(-log(y))^(csi_m))

gev.padrao <- function(y,csi){
  
  if (csi!=0){
    # essa é a densidade da gev padrao quando csi != 0 (derivada da FDA)
    y <- (((1+(csi*(y)))^((-1/csi)-1))*(exp(-(1+(csi*y))^(-1/csi))))
  }else{
    # densidade da gev padrao quando csi = 0
    y <- exp(-y-exp(-y))
  }
  return(y)
}


gev. <-function(y,csi,s,mu){
  
  y <- (gev.padrao(((y-mu)/s),csi))/s
  
  return(y)
}


par(mfrow = c(2,2))
#x11()
hist(amostra_r, prob = T, border = "grey51",
     col = "lightblue", main="", xlab="Amostra", ylab= "Densidade")
curve(gev.(x,csi=csi_r, s=sigma_r ,mu=mu_r ), add= T, col = 'black')


hist(amostra_m, prob = T, border = "grey51",
     col = "lightblue", main="", xlab="Amostra", ylab= "Densidade")
curve(gev.(x,csi=csi_m, s=sigma_m ,mu=mu_m ), add= T, col = 'black')



## VaR russell

# #dados originais
# mod1 <- fevd(m_rus,  type="GEV",method ="MLE")
# mod1
# #x11()
# plot(mod1)

##funcao para estimar o var
varpp<-function(var, u, p, sigma, xi){
  n<-length(var)
  NU<-length(var[var>u])
  varp=u+ (sigma/xi)*(( (n/NU)*(1-p) )^(-xi) -1)
  # print(NU)
  # print(varp)
  # print(n)
  
  return(varp)
  }
# estimativas var  gev

mu_r <- par_gev_r$GEV[par_gev_r$Parâmetros == "mu"]
sigma_r <- par_gev_r$GEV[par_gev_r$Parâmetros == "sigma"]
csi_r <- par_gev_r$GEV[par_gev_r$Parâmetros == "csi"]

v1_r <- varpp(logret_rus, mu_r, 0.9, sigma_r, csi_r)
v12_r <- varpp(logret_rus,mu_r, 0.95, sigma_r,csi_r)
v13_r <- varpp(logret_rus,mu_r, 0.975, sigma_r,csi_r)
v14_r <- varpp(logret_rus,mu_r, 0.999, sigma_r,csi_r)

Var_r <- c(v1_r,v12_r,v13_r,v14_r)

# Var msci

mu_m <- par_gev_m$GEV[par_gev_m$Parâmetros == "mu"]
sigma_m <- par_gev_m$GEV[par_gev_m$Parâmetros == "sigma"]
csi_m <- par_gev_m$GEV[par_gev_m$Parâmetros == "csi"]

v1_m <- varpp(logret_rus, mu_m, 0.9, sigma_m, csi_m)
v12_m <- varpp(logret_rus,mu_m, 0.95, sigma_m,csi_m)
v13_m <- varpp(logret_rus,mu_m, 0.975, sigma_m,csi_m)
v14_m <- varpp(logret_rus,mu_m, 0.999, sigma_m,csi_m)

Var_m <- c(v1_m,v12_m,v13_m,v14_m)
Var_m <- as.data.frame(Var_m)
rownames(Var_m) <- c("0,9","0,95","0,975","0,999")

###### COPULAS - RUSSELL E MSCI

## Análise  bivariada dos dados (Scatterplot)

#x11()
plot(logret_rus, logret_msci)


### tau de kendal

a.0 <- sin(cor(m_rus,m_msci, method = "kendal")*pi/2)

start <- c(a.0)

p_load(TLMoments)
udat <- cbind(pgev(m_rus, scale = csi_r, loc = mu_r, shape = sigma_r),
              pgev(m_msci, scale = csi_m, loc = mu_m, shape = sigma_m))

myCop.clayton <- claytonCopula(dim = 2)
myCop.frank <- frankCopula(dim = 2)
myCop.gumbel <- gumbelCopula(dim = 2)

fit_clayton <- fitCopula(myCop.clayton, udat, start = a.0) 
fit_frank <- fitCopula(myCop.frank, udat, start = a.0)
fit_gumbel <- fitCopula(myCop.gumbel, udat, start = a.0)


# salvando parametros estimados de cada copula

par_cleyton <- fit_clayton@estimate
par_frank <- fit_frank@estimate
par_gumbel <- fit_gumbel@estimate


# Copulas com seus respectivos parametros

cc <- claytonCopula(par_cleyton)
sample1 <- rCopula (1000 , cc)

gu <- gumbelCopula(par_gumbel)
sample2 <- rCopula (1000 , gu)

fr <- frankCopula(par_frank)
sample3 <- rCopula (1000 , fr)


## Cleyton 
### Usando as marginais GEV

n <- 1000 
x <- numeric(n)
y <- numeric(n)

for (i in 1:n) {
  
  x[i] <- (mu_r - (sigma_r/csi_r))*(1-(-log(sample1[i,1]))^(-csi_r))
  y[i] <- (mu_m - (sigma_m/csi_m))*(1-(-log(sample1[i,2]))^(-csi_m))
  
}

par(mfrow = c(1,2))
#x11()
plot(sample1, xlab = "U", ylab = "V", pch = ".", main = "Cópula de Clayton", cex = 1.5)
plot(x,y, xlab = "Marginal GEV - Copula de Cleyton", ylab = "Y", pch = ".", cex = 1.5)

par(mfrow = c(1,1))
plot(m_rus, m_msci, pch = ". ", cex = 5 , col="black", xlab="Temperature(C)" , ylab="Humidity" ) 
points( x, y, ylab="Y", pch = ". ", cex = 5, col="red")
legend("topright", legend=c("Dados Ajustados", "Dados reais"), col=c("red", "black"), pch = 15) 


## Gumbel
### Usando as marginais GEV

x <- numeric(n)
y <- numeric(n)

for (i in 1:n) {
  
  x[i] <- (mu_r - (sigma_r/csi_r))*(1-(-log(sample2[i,1]))^(-csi_r))
  y[i] <- (mu_m - (sigma_m/csi_m))*(1-(-log(sample2[i,2]))^(-csi_m))
  
}

par(mfrow = c(1,2))
#x11()
plot(sample2, xlab = "U", ylab = "V", pch = ".", main = "Cópula de Gumbel", cex = 1.5)
plot(x,y, xlab = "Marginal GEV - Copula de Gumbel", ylab = "Y", pch = ".", cex = 1.5)

par(mfrow = c(1,1))
plot(m_rus, m_msci, pch = ". ", cex = 5 , col="black", xlab="Temperature(C)" , ylab="Humidity" ) 
points( x, y, ylab="Y", pch = ". ", cex = 5, col="red")
legend("topright", legend=c("Dados Ajustados", "Dados reais"), col=c("red", "black"), pch = 15) 


## Frank
### Usando as marginais GEV

x <- numeric(n)
y <- numeric(n)

for (i in 1:n) {
  
  x[i] <- (mu_r - (sigma_r/csi_r))*(1-(-log(sample3[i,1]))^(-csi_r))
  y[i] <- (mu_m - (sigma_m/csi_m))*(1-(-log(sample3[i,2]))^(-csi_m))
  
}

par(mfrow = c(1,2))
plot(sample3, xlab = "U", ylab = "V", pch = ".", main = "Cópula de Frank", cex = 1.5)
plot(x,y, xlab = "Marginal GEV - Copula de Frank", ylab = "Y", pch = ".", cex = 1.5)

par(mfrow = c(1,1))
plot(m_rus, m_msci, pch = ". ", cex = 5 , col="black", xlab="Temperature(C)" , ylab="Humidity" ) 
points( x, y, ylab="Y", pch = ". ", cex = 5, col="red")
legend("topright", legend=c("Dados Ajustados", "Dados reais"), col=c("red", "black"), pch = 15) 



##Tabelas com os parâmetros estimados das marginais e cópula

# maxima verossimilhança
ll_clayton <- fit_clayton@loglik
ll_gumbel <- fit_gumbel@loglik
ll_frank <- fit_frank@loglik

# alpha
par_cleyton
par_frank
par_gumbel


## AIC = 2- 2 * LL ; 
aic_clayton <- 2 - 2*ll_clayton
aic_gumbel <- 2 - 2*ll_gumbel # menor
aic_frank <- 2 - 2*ll_frank

#  aic_clayton
#  -71.93392
#  aic_gumbel
#  -69.88928
#  aic_frank
#  -69.98254

##BIC = log(n) - 2 * LL
bic_clayton <- log(n) - 2*ll_clayton
bic_gumbel <- log(n) - 2*ll_gumbel # menor
bic_frank <- log(n) - 2*ll_frank

# bic_clayton
#  -67.02616
#  bic_gumbel 
#  -64.98152
#  bic_frank
#  -65.07479

#### VaR bivariado




