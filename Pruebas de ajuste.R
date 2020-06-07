library(rriskDistributions)
library(quantmod)
library(PortfolioAnalytics)
library(DistributionUtils)
library(MASS)
library(stats)
library(reshape2)
library(ggplot2)
#Leyendo archivo con los rendimientos del portafolio
Ra <- read.csv("Rendimientos.csv")

summary(Ra)

#Los rendimientos los multiplicamos por 100
Ra <- na.omit(Ra)
Re <- Ra*100

#---------------Obteniendo los momentos de las series
#Maximo de Sharpe
Pmaxme <- Mean.arithmetic(Ra$Maximo.de.Sharpe)
Pmaxrisk <- StdDev(Ra$Maximo.de.Sharpe)

#Benchmark
PBme<- Mean.arithmetic(Ra$IPC)
PBrisk <- StdDev(Ra$IPC)


#Min varaianza
Pmime<- Mean.arithmetic(Ra$Minima.Varianza)
Pmirisk <- StdDev(Ra$Minima.Varianza)

#-------Analisis Exploratorio Maximo sharpe----------------#
par(mfrow=c(1,3))
hist(Ra$Maximo.de.Sharpe, ylab="Frecuencia",xlab = "Rendimientos", main = "Histograma", col = "gray")
plot(density(Ra$Maximo.de.Sharpe), lwd=3, col="red",xlab="Rendimientos", main = "Densidad de Kernel")
qqnorm(Ra$Maximo.de.Sharpe, xlab="Cuantiles Teóricos", ylab="Cuantiles muestrales", main = "QQPlot Normal vs Muestral" )
qqline(Ra$Maximo.de.Sharpe)


#-------Analisis Exploratorio Benchmark----------------------#
par(mfrow=c(1,3))
hist(Ra$IPC, ylab="Frecuencia",xlab = "Rendimientos", main = "Histograma", col = "gray")
plot(density(Ra$IPC), lwd=3, col="red",xlab="Rendimientos", main = "Densidad de Kernel")
qqnorm(Ra$IPC, xlab="Cuantiles Teóricos", ylab="Cuantiles muestrales", main = "QQPlot Normal vs Muestral" )
qqline(Ra$IPC)



#--------Analisis exploratorio Minima Varianza
par(mfrow=c(1,3))
hist(Ra$Minima.Varianza, ylab="Frecuencia",xlab = "Rendimientos", main = "Histograma", col = "gray")
plot(density(Ra$Minima.Varianza), lwd=3, col="red",xlab="Rendimientos", main = "Densidad de Kernel")
qqnorm(Ra$Minima.Varianza, xlab="Cuantiles Teóricos", ylab="Cuantiles muestrales", main = "QQPlot Normal vs Muestral" )
qqline(Ra$Minima.Varianza)


#----------------------Prueba distribución -------------------#
fit.cont(data2fit=Re$Maximo.de.Sharpe)
fit.cont(Re$IPC)
fit.cont(Re$Minima.Varianza)


#----------Simulando numeros con distribucion logistica
#Portafolio maximo sharpe
n<-10000
p <- runif(n, min=0, max=1)
RsimuladoL_M<- qlogis(p,location = Pmaxme, scale = Pmaxrisk)

#Portafolio minima varianza
RsimuladoL_m<- qlogis(p,location = Pmime, scale = Pmirisk)

#Benchmark
RsimuladoL_b<- qlogis(p,location = PBme, scale = PBrisk)


#------------Generar numeros distribuidos normalmente
#Portaoflio maximo Sharpe
RsimuladoN_M <- qnorm(p,Pmaxme,Pmaxrisk)
#Portafolio minima Varianza
RsimuladoN_m <- qnorm(p,Pmime,Pmirisk)
#Benchmark
RsimuladoN_b <- qnorm(p,PBme,PBrisk)


#-----------Comparacion de distribuciones
#Portafolio maximo de Sharpe
par(mfrow=c(1,3))
chart.Histogram(RsimuladoN_M, main = "Distribución Normal", col="blue")
chart.Histogram(RsimuladoL_M, main ="Distribución Logistica", col="green")
chart.Histogram(Ra$Maximo.de.Sharpe, main = "Distribución Muestral", col="yellow")

#Portafolio minima varinza
par(mfrow=c(1,3))
chart.Histogram(RsimuladoN_m, main = "Distribución Normal", col="blue")
chart.Histogram(RsimuladoL_m, main ="Distribución Logistica", col="green")
chart.Histogram(Ra$Minima.Varianza, main = "Distribución Muestral", col="red")

#Benchmark
par(mfrow=c(1,3))
chart.Histogram(RsimuladoN_b, main = "Distribución Normal", col="blue")
chart.Histogram(RsimuladoL_b, main ="Distribución Logistica", col="green")
chart.Histogram(Ra$IPC, main = "Distribución Muestral", col="orange")


#VAR Monte Carlo Distribución normal
NVARMonteCarlo_M <- VaR(RsimuladoN_M, p=0.95,method = "gaussian") #Maximo Sharpe
NVARMonteCarlo_m <- VaR(RsimuladoN_m, p=0.95,method = "gaussian") #Minima Varianza
NVARMonteCarlo_b <- VaR(RsimuladoN_b, p=0.95,method = "gaussian") #Benchmark

#VAR Monte Carlo Distribución logistica
LVARMonteCarlo_M <- VaR(RsimuladoL_M, p=0.95,method = "modified") #Maximo Sharpe
LVARMonteCarlo_m <- VaR(RsimuladoL_m, p=0.95,method = "modified") #Minima varianza
LVARMonteCarlo_b <- VaR(RsimuladoL_b, p=0.95,method = "modified") #Benchmark

#Expected shortfall Monte Carlo Distribución normal
NES_M <- ES(R=RsimuladoN_M,p=0.95, method = "gaussian") #Maximo de Sharpe
NES_m <- ES(R=RsimuladoN_m,p=0.95, method = "gaussian") #Minima Varianza
NES_b <- ES(R=RsimuladoN_b,p=0.95, method = "gaussian") #Benchmark


#Expected shortfall Monte Carlo Distribución Logistica
LES_M <- ES(R=RsimuladoL_M,p=0.95, method = "modified") #Maximo de Sharpe
LES_m <- ES(R=RsimuladoL_m,p=0.95, method = "modified") #Minima Varianza
LES_b <- ES(R=RsimuladoL_b,p=0.95, method = "modified") #Benchmark

#VAR Historico
VARH_M <- VaR(R=Ra$Maximo.de.Sharpe, p=0.95, method = "historical") #Maximo de Sharpe
VARH_m <- VaR(R=Ra$Minima.Varianza, p=0.95, method = "historical") #Minima Varianza
VARH_b <- VaR(R=Ra$IPC, p=0.95, method = "historical") #Benchmark

#VAR Modificado ajustado a su distribución muestral
VARM_M <- VaR(R=Ra$Maximo.de.Sharpe, p=0.95, method = "modified") #Maximo de Sharpe
VARM_m <- VaR(R=Ra$Minima.Varianza, p=0.95, method = "modified") #Minima Varianza
VARM_b <- VaR(R=Ra$IPC, p=0.95, method = "modified") #Benchmark

#VAR Gaussiano
VARG_M <- VaR(R=Ra$Maximo.de.Sharpe, p=0.95, method = "gaussian") #Maximo de Sharpe
VARG_m <- VaR(R=Ra$Minima.Varianza, p=0.95, method = "gaussian") #Minima Varianza
VARG_b <- VaR(R=Ra$IPC, p=0.95, method = "gaussian") #Benchmark

#Expected shortfall Historico
ESH_M <- ES(R=Ra$Maximo.de.Sharpe,p=0.95, method = "historical") #Maximo de Sharpe
ESH_m <- ES(R=Ra$Minima.Varianza,p=0.95, method = "historical") #Minima Varianza
ESH_b <- ES(R=Ra$IPC,p=0.95, method = "historical") #Benchmark

#Expected shortfall modificado ajustado a su distribución muestral
ESM_M <- ES(R=Ra$Maximo.de.Sharpe,p=0.95, method = "modified") #Maximo de Sharpe
ESM_m <- ES(R=Ra$Minima.Varianza,p=0.95, method = "modified") #Minima Varianza
ESM_b <- ES(R=Ra$IPC,p=0.95, method = "modified") #Benchmark

#Expected shortfall Gaussiano
ESG_M <- ES(R=Ra$Maximo.de.Sharpe,p=0.95, method = "gaussian") #Maximo de Sharpe
ESG_m <- ES(R=Ra$Minima.Varianza,p=0.95, method = "gaussian") #Minima Varianza
ESG_b <- ES(R=Ra$IPC,p=0.95, method = "gaussian") #Benchmark


#Tabla VAR Maximo de Sharpe
Max.VARs <- data.frame(cbind(VARG_M,VARH_M,VARM_M, NVARMonteCarlo_M, LVARMonteCarlo_M))
colnames(Max.VARs) <- c("Gaussiano", "Historico", "Ajustado", "Monte Carlo Normal", "Monte Carlo Logistico" )
row.names(Max.VARs) <- c("Maximo de Sharpe")


#Tabla VAR  Minima Varianza
Min.VARs <- data.frame(cbind(VARG_m,VARH_m,VARM_m, NVARMonteCarlo_m, LVARMonteCarlo_m))
colnames(Min.VARs) <- c("Gaussiano", "Historico", "Ajustado", "Monte Carlo Normal", "Monte Carlo Logistico" )
row.names(Min.VARs) <- c("Minima Varianza")

#Tabla VAR  Benchmark
B.VARs <- data.frame(cbind(VARG_b,VARH_b,VARM_b, NVARMonteCarlo_b, LVARMonteCarlo_b))
colnames(B.VARs) <- c("Gaussiano", "Historico", "Ajustado", "Monte Carlo Normal", "Monte Carlo Logistico" )
row.names(B.VARs) <- c("Benchmark")

#Agrupación VAR
ALL.VAR <- data.frame(rbind(Max.VARs, Min.VARs, B.VARs))*100


#Tabla ES Maximo de Sharpe
Max.ES <- data.frame(cbind(ESG_M,ESH_M,ESM_M, NES_M, LES_M))
colnames(Max.ES) <- c("Gaussiano", "Historico", "Ajustado", "Monte Carlo Normal", "Monte Carlo Logistico" )
row.names(Max.ES) <- c("Maximo de Sharpe")


#Tabla ES  Minima Varianza
Min.ES <- data.frame(cbind(ESG_m,ESH_m,ESM_m, NES_m, LES_m))
colnames(Min.ES) <- c("Gaussiano", "Historico", "Ajustado", "Monte Carlo Normal", "Monte Carlo Logistico" )
row.names(Min.ES) <- c("Minima Varianza")

#Tabla ES  Benchmark
B.ES <- data.frame(cbind(ESG_b,ESH_b,ESM_b, NES_b, LES_b))
colnames(B.ES) <- c("Gaussiano", "Historico", "Ajustado", "Monte Carlo Normal", "Monte Carlo Logistico" )
row.names(B.ES) <- c("Benchmark")


#Agrupación ES
ALL.ES <- data.frame(rbind(Max.ES, Min.ES, B.ES))*100

ALL.VAR

ALL.ES


