source("Main.R")
source("Funciones.R")
library(forecast)
#Datos2<-log(Datos2)
serie.ts<- ts(Datos2,frequency=12)#,start = c(1,2))
fit <- auto.arima(Datos2)
plot.ts(Datos2)
plot(decompose(serie.ts))

seasonplot(serie.ts)
# por lo antes comentado trasnformaremos a formato logarimico
serie.ts.log<-log(serie.ts)
serie.log<-log(Datos2)

#podemos observar que la varianza ya no cambia natto a lo largo del tiempo
plot(decompose(serie.ts.log))

#Los siguientes datos corresponden a:
# -x: Serie original
# -seasonal: Componente estacional
# -trend: Tendencia
# -random: lo que queda despues de eliminar tendencia y estacionalida

decompose(serie.ts.log)


### NOTAS###
# En seasonal podemos ver como todos los meses enero tienen el mismo valor
# en feb pasa igual... asi asta diciembre, Esto nos quiere decir que lo svalores de mes 1 a 12 se repiten
#Hara falta esto para calclar la componente estacional y restarselo despues


##################################################################
##      Division en conjuntos Tes y Training
##################################################################

serieTr <- serie.log[1:(length(serie.log)-NTest)]
tiempoTr <- 1:length(serieTr)

serieTs <-  serie.log[(length(serie.log)-NTest+1):length(Datos2)]
tiempoTs <- (tiempoTr[length(tiempoTr)]+1):(tiempoTr[length(tiempoTr)]+NTest)

#Mostar serie de entrenamiento(negro) y serie de test en rojo
plot.ts(serieTr, xlim=c(1,tiempoTs[length(tiempoTs)]))
lines(tiempoTs,serieTs,col="red")

##################################################################
##      Modelar la tendencia
##################################################################


parametros.H1 <- lm(serieTr ~ tiempoTr)

TendEstimadaTr.H1 <- parametros.H1$coefficients[1]+tiempoTr*parametros.H1$coefficients[2]
TendEstimadaTs.H1 <- parametros.H1$coefficients[1]+tiempoTs*parametros.H1$coefficients[2]


plot.ts(serieTr,xlim=c(1,tiempoTs[length(tiempoTs)]))
lines(tiempoTr, TendEstimadaTr.H1,col="blue")
lines(tiempoTs,serieTs,col="red")
lines(tiempoTs,TendEstimadaTs.H1,col="green")

###Validacion del error###
# el error es normal si se distribulle igual a lo largo del tiempo
#aplicaremos e test de normalidad de JarqueBera para ver si esto ocurre
# lo aplicaremos tanto en los residuos del entrenamiento y en los del test
#calculados mediante la tendencia que hemos estimado

JBtr <- jarque.bera.test(parametros.H1$residuals)
#0.4158 valor superior a 0.05 por lo tanto los datos no son diferentes
JBts <- jarque.bera.test(TendEstimadaTs.H1-serieTs)
# 0.6442 no hay diferencais significativas , podemos decir que no hay diferencias significativas entre el error
#Test de Student
TT <- t.test(c(parametros.H1$residuals,TendEstimadaTs.H1-serieTs))
#0.5415 no hay una desviacion siginicativa entre los errores de train y test,
# por lo tanto podemos asumir que la tendencia obtenida por el modelo lineal es buena

##################################################################
#Eliminamos la tendencia
##################################################################

serieTr.SinTend<- serieTr -TendEstimadaTr.H1
serieTs.SinTend<- serieTs -TendEstimadaTs.H1

plot.ts(serieTr.SinTend,xlim=c(1,tiempoTs[length(tiempoTs)]))
lines(tiempoTs, serieTs.SinTend,col="blue")

##################################################################
# liminacion de la estacionalidad
##################################################################

#asumimos u epriodo de 12 meses
#ulilizamos descompose para eliminar la estacionalidad
k<-12
estacionalidad <- decompose(serie.ts.log)$seasonal[1:k]


#Asta qui tenemos un serie sintendencia ahora devemos quiitarnos las estacionalidad
#aqui eliminaremos la estacioalidad

aux<- rep(estacionalidad,length(serieTr)/length(estacionalidad))

serieTr.SinTend.SinEst<- serieTr.SinTend-c(aux,aux[1:10])#c(aux,aux[1:2])

serieTs.SinTend.SinEst <- serieTs.SinTend- estacionalidad

#parece que decrece, no es totalmente seguro que sea estacionaria
acf(serieTr.SinTend.SinEst)
pacf(serieTr.SinTend.SinEst)

#como el valor es mayor de 0.05 es no estacionaria

ADFTr <- adf.test(serieTr.SinTend.SinEst)

#utilizando la diferenciacion

serieTr.SinTend.SinEst.Diff <- diff(serieTr.SinTend.SinEst)
serieTs.SinTend.SinEst.Diff <- diff(serieTs.SinTend.SinEst)

ADFTr.try2 <- adf.test(serieTr.SinTend.SinEst.Diff)

acf(serieTr.SinTend.SinEst.Diff)
pacf(serieTr.SinTend.SinEst.Diff)

# como podemos ver aqui el p value nos da inferior a 0.05 y ademas
#vemos como en el acf es mejor

# viendo el acf y pacf es de un sistema autoregresivo
# como hemos diferenciado una vez , tendremos un modelo arima()

modelo<- arima(serieTr.SinTend.SinEst,order=c(4,1,0))
valoresReconstruidos <- serieTr.SinTend.SinEst+modelo$residuals


#prediccion:

predicciones <- predict(modelo, n.ahead = 12)

valores.predichos <- predicciones$pred

#podemos calcular el error

errorTr <- sum(modelo$residuals^2)
errorTs <- sum((valores.predichos-serieTs.SinTend.SinEst)^2)

#Visualizacion del modelo


plot.ts(serieTr.SinTend.SinEst ,xlim=c(1,tiempoTs[length(tiempoTs)]))
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


plot.ts(Datos2)
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
aux <- ts(SerieEntera,frequency=12)
aux <- decompose(aux)$seasonal
estacionalidad <- as.numeric(aux[1:12])
aux<-rep(estacionalidad,length(serieSinTend)/length(estacionalidad))
#eliminamos la estacionalidad
serieSinTendEst <- serieSinTend - aux

#Construimos el modelo
modelo <- arima(serieSinTendEst,order=c(4,1,0))

#Calculamos los valores de los valores de la serie predichos por el modelo
#Prediccion de los 12 siguientes valores
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
plot.ts(Datos2, xlim=c(1, max(tiempoPred)))
lines(valoresAjustados, col="blue")
lines(tiempoPred,valoresPredichos, col= "red")
lines(tiempoPred, predReales, col="green")
