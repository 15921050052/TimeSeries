source("Main.R")
source("Funciones.R")
NPred<-2
NTest<-2
##################################################################
##                      Trasformaciones                          #
##################################################################
serie<-log(Datos2)

serie.ts<-ts(serie,frequency = 12)
DECOMPOSE(serie.ts)

serieTr<-serie.ts[1:(length(serie.ts)-NTest)]
tiempoTr<- 1:length(serieTr)

serieTs<-serie.ts[(length(serieTr)+1):length(serie)]
tiempoTs<-(length(tiempoTr)+1):(length(tiempoTr)+NTest)

#Dibujamos la serie, tanto la de entrenamiento como la de test (rojo)
PLOT_C2(serie.ts,tiempoTr)


#Probamos con un modelo cuadrático
parametros.H1<-lm(serieTr~tiempoTr+I(tiempoTr^2))
TendEstimadaTr.H1<-parametros.H1$coefficients[1]+parametros.H1$coefficients[2]*tiempoTr+
  parametros.H1$coefficients[3]*tiempoTr^2
TendEstimadaTs.H1<-parametros.H1$coefficients[1]+parametros.H1$coefficients[2]*tiempoTs+
  parametros.H1$coefficients[3]*tiempoTs^2

JBTr<-jarque.bera.test(parametros.H1$residuals)
JBTs<-jarque.bera.test((TendEstimadaTs.H1-serieTs))

#Aplicamos el test de Student
TT<-t.test(c(parametros.H1$residuals,TendEstimadaTs.H1-serieTs))

PLOT2(serie.ts ,tiempoTr,c(TendEstimadaTr.H1,TendEstimadaTs.H1))

#Le quitamos a la serie la tendencia calculada anteriormente
serieTr.SinTend.H1<-serieTr-TendEstimadaTr.H1
serieTs.SinTend.H1<-serieTs-TendEstimadaTs.H1

#Dibujamos ahora la serie sin la tendencia
PLOT_C2(c(serieTr.SinTend.H1,serieTs.SinTend.H1),tiempoTr)
ACF(serieTr.SinTend.H1)
ACF(diff(serieTr.SinTend.H1,lag=1))

#Eliminamos la estacionalidad
k<-9
estacionalidad.H1<-decompose(serie.ts)$seasonal[1:k]
aux<-rep(estacionalidad.H1,length(serieTr)/length(estacionalidad.H1))
serieTr.SinTendEst.H1<-serieTr.SinTend.H1-aux
serieTs.SinTendEst.H1<-serieTs.SinTend.H1-estacionalidad.H1

plot.ts(serieTr.SinTendEst.H1,xlim=c(1,tiempoTs[length(tiempoTs)]))
lines(tiempoTs,serieTs.SinTendEst.H1,col="red")

ACF(serieTr.SinTendEst.H1)
PACF(serieTr.SinTendEst.H1)

adf.test(serieTr.SinTendEst.H1)#Tiene que ser menor de 0.05

serieTr.SinTendDiff.H1<-diff(serieTr.SinTendEst.H1)
serieTs.SinTendDiff.H1<-diff(serieTs.SinTendEst.H1)
adf.test(serieTr.SinTendDiff.H1)

acf(serieTr.SinTendDiff.H1)
pacf(serieTr.SinTendDiff.H1)

modelo<-arima(serieTr.SinTendEst.H1,order=c(0,0,2))
valoresAjustados<-serieTr.SinTendEst.H1+modelo$residuals

predicciones<-predict(modelo,n.ahead=NPred)
valoresPredichos<-predicciones$pred

errorTr<-sum((modelo$residual)^2)
errorTs<-sum(valoresPredichos-serieTs.SinTendEst.H1)^2

plot.ts(serieTr.SinTendEst.H1,xlim=c(1,tiempoTs[length(tiempoTs)]),ylim=c(min(valoresAjustados),max(serieTs.SinTendEst.H1)))
lines(valoresAjustados,col="blue")
lines(tiempoTs,serieTs.SinTendEst.H1,col="red")
lines(tiempoTs,valoresPredichos,col="green")

#Tes para la selección del modelo y su validación. Comprobamos primeramente la aleatoriedad
Box.test(modelo$residuals)

#Ahora vamos a ver si los errores se distribuyen como una normal
jarque.bera.test(modelo$residuals)#podemos asumir la normalidad de los residuos

#Test de normalidad Shapiro-Wilk
shapiro.test(modelo$residuals)

#Cogemos la serie completa ahora

serie<-Datos2
serie<-log(serie)
tiempo<-1:length(serie)

serie.ts<-ts(serie,frequency = 9)
DECOMPOSE(serie.ts)
parametros<-lm(serie~tiempo+I(tiempo^2))
TendEstimada<-parametros$coefficients[1]+parametros$coefficients[2]*tiempo+
  parametros$coefficients[3]*tiempo^2
serieSinTend<-serie-TendEstimada
aux<-ts(serie,frequency=9)
aux<-decompose(aux)$seasonal
estacionalidad<-as.numeric(aux[1:9])
aux<-rep(estacionalidad,length(serieSinTend)/length(estacionalidad))
serieSinTendEst<-serieSinTend-aux
modelo<-arima(serieSinTendEst,order=c(0,0,2))
valoresAjustados<-serieSinTendEst+modelo$residuals
Predicciones<-predict(modelo,n.ahead=NPred)
valoresPredichos<-Predicciones$pred

#Deshacemos los cambios
valoresAjustados<-valoresAjustados+aux
valoresPredichos<-valoresPredichos+estacionalidad

valoresAjustados<-valoresAjustados+TendEstimada
tiempoPred<-tiempo[length(tiempo)]+(1:NPred)
TendEstimadaPred<-parametros$coefficients[1]+parametros$coefficients[2]*tiempoPred+
  parametros$coefficients[3]*tiempoPred^2
valoresPredichos<-valoresPredichos+TendEstimadaPred
valoresAjustados<-exp(valoresAjustados)
valoresPredichos<-exp(valoresPredichos)
serie.original<-scan("serie2.dat")

PLOT2(c(serie.original,NA,NA,NA,NA),tiempoTr,c(valoresAjustados,valoresPredichos))
plot.ts(serie.original,xlim=c(1,max(tiempoPred)))
lines(valoresAjustados,col="blue")
lines(valoresPredichos,col="green")
