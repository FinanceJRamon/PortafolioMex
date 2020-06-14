library(zoo)
library(tseries)
library(PortfolioAnalytics)
library(quantmod)
library(ROI.plugin.glpk)
library(ROI.plugin.quadprog)
library(ROI)
library(ggplot2)


#Creando vector de empresas 
tickers <- c("LABB.MX", "LIVEPOLC-1.MX",	"PE&OLES.MX",	"CEMEXCPO.MX",	"OMAB.MX",	"IENOVA.MX",	"ORBIA.MX",
             "GRUMAB.MX",	"KIMBERA.MX",	"BIMBOA.MX",	"RA.MX",	"GMEXICOB.MX",	"GFNORTEO.MX", "GCARSOA1.MX",
             "AMXL.MX",	"ASURB.MX",	"AC.MX",	"GAPB.MX",	"PINFRA.MX",	"TLEVISACPO.MX",	"GENTERA.MX","ALPEKA.MX",	"MEGACPO.MX",	
             "GCC.MX",	"BOLSAA.MX",	"ALSEA.MX")


inicio <- "2010-01-01"
fin="2020-04-30"
#Tasa Libre de riesgo diaria
rfr= (0.05/252)

#Calculando Rendimientos: Diarios
portfolioPrecios <- NULL
for (Ticker in tickers)
  portfolioPrecios <- cbind(portfolioPrecios,
                            getSymbols.yahoo(Ticker, from=inicio, to=fin, auto.assign=FALSE)[,4]) 


#Borrando todos los datos que no tienen precios 
portfolioPrecios <- portfolioPrecios[apply(portfolioPrecios,1,function(x) all(!is.na(x))),]
#Renombrar Columnas
colnames(portfolioPrecios) <- tickers

#Calculo de  Retornos: Daily RoC
portfolioRetornos <- na.omit(ROC(portfolioPrecios, type="continuo"))


#Portafolio Minima varianza
init.portf.minvar <- portfolio.spec(assets=tickers)
init.portf.minvar <- add.constraint(portfolio=init.portf.minvar, type="full_investment")
init.portf.minvar <- add.constraint(portfolio=init.portf.minvar, type="long_only")
init.portf.minvar <- add.objective(portfolio=init.portf.minvar, type="risk", name="StdDev")
init.portf.minvar

minvar.lo.ROI <- optimize.portfolio(R=portfolioRetornos, portfolio=init.portf.minvar, 
                                    optimize_method="ROI",trace=TRUE)


#Portafolio maximo de de Sharpe

init.portf.maxsharpe <- portfolio.spec(assets=tickers)
init.portf.maxsharpe <- add.constraint(portfolio=init.portf.maxsharpe, type="full_investment")
init.portf.maxsharpe <- add.constraint(portfolio=init.portf.maxsharpe, type="long_only")
init.portf.maxsharpe <- add.objective(portfolio=init.portf.maxsharpe, type="return", name="mean")
init.portf.maxsharpe <- add.objective(portfolio=init.portf.maxsharpe, type="risk", name="StdDev")

maxSR.lo.ROI <- optimize.portfolio(R=portfolioRetornos, portfolio=init.portf.maxsharpe, 
                                   optimize_method="ROI", 
                                   maxSR=TRUE, trace=TRUE)

eficientes <- extractEfficientFrontier(maxSR.lo.ROI,match.col = "StdDev", n.portfolios = 200)

x11()
chart.EfficientFrontier(eficientes, match.col = "StdDev", n.portfolios = 200, rf=rfr, tangent.line = TRUE, 
                          labels.assets = TRUE, chart.assets = TRUE)

#Extrayendo ponderaciones de los portafolios
Pomin <- extractWeights(minvar.lo.ROI)
Pomax <- extractWeights(maxSR.lo.ROI)

#Graficando las ponderaciones
x11()
chart.Weights(minvar.lo.ROI)

x11()
chart.Weights(maxSR.lo.ROI)


#Portafolio dinamico

# Portafolios aleatorios
pa <- random_portfolios(init.portf.maxsharpe, 10000, "sample") #genera 10000 portafolios

# Rebalanceo
op_reb <- optimize.portfolio.rebalancing(portfolioRetornos,
                                         init.portf.maxsharpe, 
                                         optimize_method = "random",
                                         rp=pa,
                                         rebalance_on = "months",
                                         training_period = 36, 
                                         rolling_window = 12) 

# Visualización los portafolios rebalanceados {pesos dinámicos}
x11()
chart.Weights(op_reb, main="Pesos Rebalanceados en el tiempo")

#Extrayendo ponderaciones

Podin <- extractWeights(op_reb)


#Benchmark
IPC <- getSymbols("^MXX", scr= "yahoo", from= inicio, to=fin ,auto.assign=FALSE)[,4]

#Calculando rendimientos mensuales y eliminando datos en blanco
RIPC <- na.omit(periodReturn(IPC,period='daily',type='arithmetic'))


#*************Creando rendimientos del portafolio
Maximo_Sharpe <- Return.portfolio(portfolioRetornos,Pomax)

Min_Varianza <- Return.portfolio(portfolioRetornos,Pomin)
                                 
Dinamico<- Return.portfolio(portfolioRetornos,Podin)

# Grafico Capture Ratio

desempeño <- cbind(Maximo_Sharpe, Min_Varianza, Dinamico)
colnames(desempeño) <- c("Max_Sharpe", "Min_Varianza", "Dinamico")
x11()
chart.CaptureRatios(Ra=desempeño, Rb=RIPC)

table.CaptureRatios(Ra=desempeño, Rb=RIPC, digits=4)

#Rendimientos
x11()
chart.Boxplot(desempeño, main="Distribución de los Rendimientos de los portafolios")


#Graficando Renndimiento acumulado vs Benchmark
desempeño_1 <- cbind(Maximo_Sharpe, Min_Varianza, Dinamico,RIPC)
x11()
charts.PerformanceSummary(desempeño_1, main = "Desempeño en el tiempo", legend.loc = "center" )

#Performance
table.CAPM(Ra=desempeño, Rb=RIPC)

#Exportando a excel las ponderaciones
ponder <- data.frame(Pomax, Pomin)
colnames(ponder) <- c("Max_sharpe", "Min_Var")
write.csv(ponder, file = "Ponderaciones.csv")

#Exportar a excel resultados de ponderaciones dinamica
ponDi <- data.frame(Podin)
colnames(ponDi) <- c("Dinamico")
write.csv(ponDi, file = "Ponde_Dina.csv")

#Exportar rendimientos a excel
desempeño_2 <- cbind(Maximo_Sharpe, Min_Varianza,RIPC)
colnames(desempeño_2) <- c("Max_Sharpe","Min_Varianza","IPC")
Ra <- data.frame(desempeño_2)
colnames(Ra) <- c("Max_Sharpe", "Min_Var", "IPC")
write.csv(Ra, file = "Rendimientos.csv")

#***********VAR vs ES diario**************************#

#Maximo Sharpe
x11()
chart.BarVaR(Maximo_Sharpe, methods=c("ModifiedVaR","ModifiedES"), p=0.95, colorset = 1:12, main="VaR vs ES del Portafolio Maximo de Sharpe")

#Min Varianza
x11()
chart.BarVaR(Min_Varianza, methods=c("ModifiedVaR","ModifiedES"), p=0.95, colorset = 1:12, main="VaR vs ES del Portafolio Minima Varianza")

#Benchmark
x11()
chart.BarVaR(RIPC, methods=c("ModifiedVaR","ModifiedES"), p=0.95, colorset = 1:12, main="VaR vs ES del Benchmark")
