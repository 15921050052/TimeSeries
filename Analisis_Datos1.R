source("Main.R")
source("Funciones.R")
#library("nortest");
#librerias de time series
library("dyn");
library("ArDec");
library("forecast");
library("fBasics");
#library("fCalendar");
#library("fSeries");
library("tseries");
#Datos1<-log(Datos1)
serie.log<-log(Datos1-min(Datos1)+1)#+mean(Datos1))
#serie.log<-Datos1
serie.ts<- ts(serie.log[-c(1,2,3,4,5,6,7,8,9,10,11,12,13,14)],frequency=4,start = c(1,2))
Datos1<-Datos1[-c(1,2,3)]
fit <- auto.arima(serie.ts)
#serie.ts<- ts(Datos1,frequency=4,start = c(1,2))
#serie.log<-log(serie.ts-min(serie.ts)+1)
#Datos1<-c(Datos1[1],Datos1)
#serie.ts<- ts(Datos1,frequency=4)
plot(decompose(serie.ts))
#Los siguientes datos corresponden a:
# -x: Serie original
# -seasonal: Componente estacional
# -trend: Tendencia
# -random: lo que queda despues de eliminar tendencia y estacionalida
decompose(serie.ts)

##################################################################
##      Division en conjuntos Tes y Training                     #
##################################################################

serieTr <- serie.ts[1:(length(serie.ts)-NTest)]
tiempoTr <- 1:length(serieTr)

serieTs <-  serie.ts[(length(serie.ts)-NTest+1):length(Datos1)]
tiempoTs <- (tiempoTr[length(tiempoTr)]+1):(tiempoTr[length(tiempoTr)]+NTest)

#Mostar serie de entrenamiento(negro) y serie de test en rojo
plot.ts(serieTr, xlim=c(1,tiempoTs[length(tiempoTs)]))
lines(tiempoTs,serieTs,col="red")

##################################################################
##                      Modelar la tendencia                     #
##################################################################

# Hipótesis: Modelo x= a + b*t+c*t^2 (x=serie; t=tiempo; a,b=parámetros a estimar)
tiempoTr<- 1:length(serie.ts) # Creamos la variable que modela al tiempo
parametros.H1 <- lm (serie.ts ~ tiempoTr)+I(tiempoTr^2) ) # Ajustamos modelo lineal

# Calculamos la estimación de la tendencia
TendEstimadaTr.H1<-parametros.H1$coefficients[1]+tiempoTr*
  parametros.H1$coefficients[2]+
  (tiempoTr^2)*parametros.H1$coefficients[3]

# Mostramos en la misma figura la serie y la tendencia estimada
series<-matrix(c(t(serie.ts), t(TendEstimadaTr.H1)), ncol=2); # Mostramos resultado
matplot(series, pch=1, type= "l")

parametros.H1<-FiltrarSerie(serieTr,3)
parametros.H1[parametros.H1[is.na(parametros.H1)]]
parametros.H1 <- lm(serieTr ~ tiempoTr)


TendEstimadaTr.H1 <- parametros.H1$coefficients[1]+tiempoTr*parametros.H1$coefficients[2]
TendEstimadaTs.H1 <- parametros.H1$coefficients[1]+tiempoTs*parametros.H1$coefficients[2]


tiempoTr<- 1:length(serie.ts) # Creamos la variable que modela al tiempo
parametros.H1 <- lm (serie.ts ~ I(log(tiempoTr)) ) # Ajustamos modelo lineal

# Calculamos la estimación de la tendencia
TendEstimadaTr.H1<-parametros.H1$coefficients[1]+log(tiempoTr)*parametros.H1$coefficients[2]

# Mostramos en la misma figura la serie y la tendencia estimada
series<-matrix(c(t(serie.ts), t(TendEstimada)), ncol=2); # Mostramos resultado
matplot(series, pch=1, type= "l")


plot.ts(serieTr,xlim=c(1,tiempoTs[length(tiempoTs)]),ylim=c(min(serieTr),(max(serieTr)+2)))
lines(tiempoTr, TendEstimadaTr.H1,col="blue")
lines(tiempoTs,serieTs,col="red")
lines(tiempoTs,TendEstimadaTs.H1,col="green")

###Validacion del error###
# el error es normal si se distribulle igual a lo largo del tiempo
#aplicaremos e test de normalidad de JarqueBera para ver si esto ocurre
# lo aplicaremos tanto en los residuos del entrenamiento y en los del test
#calculados mediante la tendencia que hemos estimado

JBtr <- jarque.bera.test(parametros.H1$residuals)
print(JBtr)
#0.4158 valor superior a 0.05 por lo tanto los datos no son diferentes
JBts <- jarque.bera.test(TendEstimadaTs.H1-serieTs)
print(JBts)
# 0.6442 no hay diferencais significativas , podemos decir que no hay diferencias significativas entre el error
#Test de Student
TT <- t.test(c(parametros.H1$residuals,TendEstimadaTs.H1-serieTs))
print(TT$p.value)
#0.5415 no hay una desviacion siginicativa entre los errores de train y test,
# por lo tanto podemos asumir que la tendencia obtenida por el modelo lineal es buena

##################################################################
#             Eliminamos la tendencia                            #
##################################################################

serieTr.SinTend<- serieTr - TendEstimadaTr.H1
#serieTr.SinTend<- serieTr[-c(1,length(serieTr))] -parametros.H1
#serieTr.SinTend<-filter(serie.ts,filter=filtro,sides=2,method="convolution")
serieTs.SinTend<- serieTs -TendEstimadaTs.H1

plot.ts(serieTr.SinTend,xlim=c(1,tiempoTs[length(tiempoTs)]))
lines(tiempoTs, serieTs.SinTend,col="blue")

##################################################################
# liminacion de la estacionalidad
##################################################################

#asumimos u epriodo de 4 meses
#ulilizamos descompose para eliminar la estacionalidad
k<-4
estacionalidad <- decompose(serie.ts)$seasonal[1:k]


#Asta qui tenemos un serie sintendencia ahora devemos quiitarnos las estacionalidad
#aqui eliminaremos la estacioalidad

aux<- c(estacionalidad[2:4],rep(estacionalidad,length(serieTr)/length(estacionalidad)))
#aux<- rep(estacionalidad,length(serieTr)/length(estacionalidad))

serieTr.SinTend.SinEst<- serieTr.SinTend-aux

serieTs.SinTend.SinEst <- serieTs.SinTend- estacionalidad

 # Mostramos ACF
#parece que decrece, no es totalmente seguro que sea estacionaria
acf(serieTr.SinTend.SinEst)
pacf(serieTr.SinTend.SinEst)
#como el valor es mayor de 0.05 es no estacionaria

ADFTr <- adf.test(serieTr.SinTend.SinEst)

#utilizando la diferenciacion

serieTr.SinTend.SinEst.Diff <- diff(serieTr.SinTend.SinEst,differences = 2)
serieTs.SinTend.SinEst.Diff <- diff(serieTs.SinTend.SinEst,differences = 2)

ADFTr.try2 <- adf.test(serieTr.SinTend.SinEst.Diff)

acf(serieTr.SinTend.SinEst.Diff)
pacf(serieTr.SinTend.SinEst.Diff)

# como podemos ver aqui el p value nos da inferior a 0.05 y ademas
#vemos como en el acf es mejor

# viendo el acf y pacf es de un sistema autoregresivo
# como hemos diferenciado una vez , tendremos un modelo arima()

modelo<- arima(serieTr.SinTend.SinEst,order=c(2,0,0))
valoresReconstruidos <- serieTr.SinTend.SinEst+modelo$residuals


#prediccion:

predicciones <- predict(modelo, n.ahead = 4)

valores.predichos <- predicciones$pred

#podemos calcular el error

errorTr <- sum(modelo$residuals^2)
errorTs <- sum((valores.predichos-serieTs.SinTend.SinEst)^2)

#Visualizacion del modelo


plot.ts(serieTr.SinTend.SinEst ,
        xlim=c(1,tiempoTs[length(tiempoTs)]),
        ylim=c(min(serieTr.SinTend.SinEst),(max(serieTr.SinTend.SinEst))))
lines(valoresReconstruidos,col="blue")
lines(tiempoTs,serieTs.SinTend.SinEst,col="red")
lines(tiempoTs,valores.predichos,col="green")

#reconstruciion es buena aunque para el test es mala.

##################################################################
# validacion del modelo
##################################################################

#Test de Box-Pierce
BTest <-Box.test(modelo$residuals)
#P value , de 0.94 los errores son aleatorios
#pues el p value es mayor a 0.05

#Test JarqueBera y Shapiro-Wilk
JBTest <- jarque.bera.test(modelo$residuals)
# aqui nos da un 0.81 por lo tanto los residuos son normales

SWTest <- shapiro.test(modelo$residuals)
# en este test tambien el pvalue es mayor a 0.05 , por lo tanto lo pasa

# despues de todo esto podemos decir que los errores son aleatorios

hist(modelo$residuals, col="blue", prob=T,ylim=c(0,20),xlim=c(-0.2,0.2))
lines(density(modelo$residuals))
#podemos observar mediante el histograma que tenemos los errores en una media 0 y una varianza. 


#reconstruccion + estacionalidad
valoresReconstruidos.Est <- valoresReconstruidos+aux

valores.predichos.Est <- valores.predichos+estacionalidad

#añadimos la tendencia
valoresReconstruidos.Est.Tend<- valoresReconstruidos.Est+TendEstimadaTr.H1

valores.predichos.Est.Tend <- valores.predichos.Est + TendEstimadaTs.H1

#desacemos en log

valoresReconstruidos.Est.Tend.exp <- exp(valoresReconstruidos.Est.Tend)

valores.predichos.Est.Tend.exp <- exp(valores.predichos.Est.Tend)


plot.ts(serie)
lines(tiempoTr, valoresReconstruidos.Est.Tend.exp  , col="blue")
lines(tiempoTs, valores.predichos.Est.Tend.exp, col="red")

##################################################################
# Aplicacion del modelo con todo el conjutno para predecir el año siguiente
##################################################################

#CArgamos la serie completa (con la transformacion log)
SerieEntera <- serie.log
tiempo<- 1:length(SerieEntera)

#Calculamos la tendencia de la seriem mediante una regresion lineal
parametros<- lm(SerieEntera ~ tiempo)
TendEstimada <- parametros$coefficients[1]+tiempo*parametros$coefficients[2]
#Eliminamos la tendencia de la serie
serieSinTend <- SerieEntera-TendEstimada
#Extraemos la estacionalidad
aux <- ts(SerieEntera,frequency=4)
aux <- decompose(aux)$seasonal
estacionalidad <- as.numeric(aux[1:4])
aux<-rep(estacionalidad,length(serieSinTend)/length(estacionalidad))
#eliminamos la estacionalidad
serieSinTendEst <- serieSinTend - aux

#Construimos el modelo
modelo <- arima(serieSinTendEst,order=c(4,1,0))

#Calculamos los valores de los valores de la serie predichos por el modelo
#Prediccion de los 4 siguientes valores
valoresAjustados <- serieSinTendEst + modelo$residuals
Predicciones <- predict(modelo, n.ahead = NPred)
valoresPredichos <- Predicciones$pred

#A partir de aqui a los valores predichos le añadimos la estacionalidad
valoresAjustados <- valoresAjustados+ aux
valoresPredichos <- valoresPredichos +estacionalidad

# la tendencia
valoresAjustados <- valoresAjustados+TendEstimada
tiempoPred <- (tiempo[length(tiempo)]+(1:NPred))
TendEstimadaPred <- parametros$coefficients[1]+tiempoPred*parametros$coefficients[2]
valoresPredichos <- valoresPredichos+TendEstimadaPred

#y por ultimo eliminamos la transformacion log mediante exp
valoresAjustados <-  exp(valoresPredichos)
valoresPredichos <- exp(valoresPredichos) 

# Aqui podemos vr los valores , los reales y los predichos por el modelo
plot.ts(serie, xlim=c(1, max(tiempoPred)))
lines(valoresAjustados, col="blue")
lines(tiempoPred,valoresPredichos, col= "red")
lines(tiempoPred, predReales, col="green")




DATOS.TS.AR <- ar(Datos1);
plot(forecast(DATOS.TS.AR,4,conf=c(20,55)))




