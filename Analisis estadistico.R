###########Analisis estadistico de acciones del IPC################
#install.packages("quantmod")
#install.packages("PerformanceAnalytics")
#install.packages("ggplot2")
#install.packages("reshape2")
#install.packages("lattice")
#install.packages("rugarch")
#install.packages("parallel")
#install.packages("plyr")
#install.packages("scales")
#install.packages("ggcorrplot")
#install.packages("fBasics")
##Librerias que vamos a ocupar
library(quantmod)
library(PerformanceAnalytics)
library(ggplot2)
library(reshape2)
library(quantmod)
library(PerformanceAnalytics)
library(lattice)
library(rugarch)
library(parallel)
library(plyr)
library(scales)
library(ggcorrplot)
library(fBasics)
######Indice#####
#1.- Grafico rendimientos anuales del IPC
#2.- Grafico rendimientos mensuales del IPC
#3.- Calendario rendimientos diarios del IPC
#4.- Graficas rendimientos diarios Acciones
#5.- Cuadro estadisticas de acciones
#6.- calendario de rendimientos de acciones*
#*Solo a un numero limitado de acciones
#-----------Datos-----------------------#
###---------Grafico de rendimientos anuales del IPC------------------##
inicio <- "2010-01-01"
fin="2020-04-30"

#Benchmark
Benchm <- getSymbols("^MXX", scr= "yahoo", from= inicio, to= fin,auto.assign=FALSE)[,4]

#Calculando rendimientos mensuales y eliminando datos en blanco
Benchm_ret <- na.omit(periodReturn(Benchm,
                                   period='monthly',
                                   type='arithmetic'))
#Graficando Benchmark
plot(Benchm)
#Creando Calendario de Rendimientos 
cal_rets <- table.CalendarReturns(Benchm_ret, digits = 1, as.perc = TRUE)
#Borrando datos inecesarios
cal_rets$monthly.returns = NULL

df = data.frame(cal_rets)
#-------------------------Box Plot---------------------------------#
#versión básica
boxplot(df)
#Agregando  años o meses
cal_rets$group <- row.names(cal_rets)
cal_rets.m <- melt(cal_rets, id.vars = "group")

#Eligiendo variables para agregar, agrupando años (pueden ser variables width as well)
bp <- ggplot(cal_rets.m, aes(group, value, fill=group)) +
  geom_boxplot(alpha=0.5, varwidth = TRUE)  +
  theme(legend.position="none") +
  ggtitle("Rendimientos Anuales del IPC") +
  theme(plot.title = element_text(hjust = 0.5)) +
  stat_summary(fun.y=mean, geom="point", shape=20, size=5, color="red", fill="red") +
  labs(x=NULL, y=NULL) +
  #Add % to labels
  scale_y_continuous(labels=function(x) paste0(x,".00%")) +
  
  #Customize theme
  theme(
    panel.background = element_rect(fill = "lightblue",
                                    colour = "lightblue",
                                    size = 0.5, linetype = "solid"),
    panel.grid.major = element_line(size = 0.5, linetype = 'solid',
                                    colour = "white"),
    panel.grid.minor = element_line(size = 0.25, linetype = 'solid',
                                    colour = "white"))
bp + theme(plot.background = element_rect(fill = "green"))
bp

##-------------Grafico rendimientos mensuales IPC----------------------------##
simbolo <- "^MXX"
simbolo <- toString(simbolo)
Ben_datos <- getSymbols.yahoo(simbolo, From = inicio, to= fin, auto.assign=FALSE)[,4]

Ben_ret <- na.omit(periodReturn(Ben_datos,
                                period='monthly',
                                type='arithmetic'))

cal_rets <- table.CalendarReturns(Ben_ret, digits = 1, as.perc = TRUE)

#Violin Chart: Rendimientos mensuales#
#Eliminar rendimientos mensuales sin datos
cal_rets$monthly.returns = NULL
#tranponer
cal_rets <- t(cal_rets)
#make so ggplot can understand
cal_rets <- melt(cal_rets, id.vars = NULL)
#Grafica rendimientos mensuales 
p <- ggplot(cal_rets, aes(x=Var1, y=value, fill = Var1)) + 
  geom_violin() +
  theme(legend.position="none") +
  ggtitle(title) +
  theme(plot.title = element_text(hjust = 0.5)) +
  labs(x=NULL, y=NULL) + 
  ggtitle(paste(simbolo, "Rendimientos mensuales")) + 
  geom_boxplot(width=0.1, fill = "white") +
  scale_y_continuous(labels=function(x) paste0(x,".00%")) 
p

##-------Calendario de los rendimientos diarios del IPC--------------------##
#Benchm <- getSymbols("^MXX", scr= "yahoo", from= "2007-01-01", auto.assign=FALSE)[,4]
BMK_day <- na.omit(periodReturn(Benchm,
                                period='daily',
                                type='arithmetic'))

dat<-data.frame(date=index(BMK_day),BMK_day$daily.returns)
dat$year<-as.numeric(as.POSIXlt(dat$date)$year+1900)
dat$month<-as.numeric(as.POSIXlt(dat$date)$mon+1)


dat$monthf<-factor(dat$month,levels=as.character(1:12),labels=c("Ene","Feb","Mar","Abr","May","Jun","Jul","Ago","Sep","Oct","Nov","Dic"),ordered=TRUE)
dat$weekday = as.POSIXlt(dat$date)$wday
dat$weekdayf<-factor(dat$weekday,levels=rev(1:7),labels=rev(c("Lun","Mar","Mier","Jue","Vie","Sab","Dom")),ordered=TRUE)
dat$yearmonth<-as.yearmon(dat$date)
dat$yearmonthf<-factor(dat$yearmonth)
dat$week <- as.numeric(format(dat$date,"%W"))
dat<-ddply(dat,.(yearmonthf),transform,monthweek=1+week-min(week))

ggplot(dat, aes(monthweek, weekdayf, fill = dat$daily.returns)) + 
  geom_tile(colour = "white") + facet_grid(year~monthf) + scale_fill_gradient('IPC \n Rendimientos', low="red", high="green", labels = percent) +  xlab("Week of Month") + ylab("")

##-----------------Graficas rendimientos diarios Acciones-----------------------##
#Creando vector de empresas 
tickers <- c("LABB.MX", "LIVEPOLC-1.MX",	"PE&OLES.MX",	"CEMEXCPO.MX",	"OMAB.MX",	"IENOVA.MX",	"ORBIA.MX",
             "GRUMAB.MX",	"KIMBERA.MX",	"BIMBOA.MX",	"RA.MX",	"GMEXICOB.MX",	"GFNORTEO.MX", "GCARSOA1.MX",
             "AMXL.MX",	"ASURB.MX",	"AC.MX",	"GAPB.MX",	"PINFRA.MX",	"TLEVISACPO.MX",	"GENTERA.MX","ALPEKA.MX",	"MEGACPO.MX",	
             "GCC.MX",	"BOLSAA.MX",	"ALSEA.MX")


#Calculando Rendimientos: Diarios
portfolioPrecios <- NULL
for (Ticker in tickers)
  portfolioPrecios <- cbind(portfolioPrecios,
                            getSymbols.yahoo(Ticker, from=inicio, to=fin, auto.assign=FALSE)[,4]) 


#Borrando todos los datos que no tienen precios 
portfolioPrecios <- portfolioPrecios[apply(portfolioPrecios,1,function(x) all(!is.na(x))),]
#Renombrar Columnas
colnames(portfolioPrecios) <- tickers

#creando matriz de correlaciones
corr <- cor(portfolioPrecios)

#------------------------Correlograma---------------------------#
ggcorrplot(corr, hc.order = TRUE, 
           type = "lower", 
           lab = TRUE, 
           lab_size = 3, 
           method="circle", 
           colors = c("springgreen3", "white", "tomato2"), 
           title="Correlograma de Acciones Méxicanas",
           legend.title = 'Correlación de Pearson',
           ggtheme=theme_gray) +
  theme(plot.title = element_text(hjust = 0.5))#####################################

