source("Main.R")
source("Funciones.R")
NPred<-2
NTest<-2
##################################################################
##                      Trasformaciones                          #
##################################################################
serie.ts<-ts(Datos1,frequency = 4)
#serie.log<-log(Datos1-min(Datos1)+1)
#Dibujamos la serie
PLOT(serie.ts)
#Dibujamos la descomposicion de la serie
DECOMPOSE(serie.ts)
#Aplicamos el log a la serie para intentar reducir la varianza del random en su parte final
serie.log<-log(Datos1)
serie.orig<-Datos1 # Guardamos copia de seguridad de la serie original
serie<-serie.log
serie.ts<-ts(serie,frequency = 4)

##Dibujamos la descomposicion de la serie
DECOMPOSE(serie.ts)

##################################################################
##      Division en conjuntos Tes y Training                     #
##################################################################
serieTr<-serie[1:(length(serie)-NTest)]
tiempoTr<- 1:length(serieTr)

serieTs<-serie[(length(serie)-NTest+1):length(serie)]
tiempoTs<-(tiempoTr[length(tiempoTr)]+1):(tiempoTr[length(tiempoTr)]+NTest)

#Pintamos la serie de entrenamiento y la de test
PLOT_C2(serie.ts,tiempoTr)

##################################################################
##                      Modelar la tendencia                     #
##################################################################
parametros <- lm (serieTr ~ tiempoTr ) # Ajustamos modelo lineal

#tiempoTr<- 1:length(serie.ts) # Creamos la variable que modela al tiempo
#parametros <- lm (serieTr ~ I(log(tiempoTr)) ) # Ajustamos modelo lineal
# Calculamos la estimación de la tendencia
#TendEstimadaTr.H1<-parametros.H1$coefficients[1]+log(tiempoTr)*parametros.H1$coefficients[2]



# Calculamos la estimación de la tendencia
TendEstimadaTr<-parametros$coefficients[1]+tiempoTr*parametros$coefficients[2]
TendEstimadaTs<-parametros$coefficients[1]+tiempoTs*parametros$coefficients[2]


#Dibujamos la tendencia del entrenamiento y del test
PLOT2(serie.ts,tiempoTr,c(TendEstimadaTr,TendEstimadaTs))

##################################################################
#             Eliminamos la tendencia                            #
##################################################################
SerSinTendTr<-serieTr-TendEstimadaTr
SerSinTendTs<-serieTs-TendEstimadaTs

#Pintamos la serie sin la tendencia
PLOT_C2(c(SerSinTendTr,SerSinTendTs),tiempoTr)

ACF(SerSinTendTr) # Mostramos ACF
PACF(SerSinTendTr) # Mostramos PACF

jarque.bera.test(parametros$residuals) #Tiene que ser mayor que 0.05
jarque.bera.test(TendEstimadaTs-serieTs)
t.test(c(parametros$residuals,TendEstimadaTs-serieTs))

#Probamos con filtrado
k=5

SerFiltradaTr<-FiltrarSerie(serieTr,k)


#Dibujamos la serie
PLOT2(SerFiltradaTr,tiempoTr,serieTr)

##################################################################
#             Eliminamos la tendencia Filtro                     #
##################################################################
SerSinTendTr<-serieTr-SerFiltradaTr
PLOT(na.omit(SerSinTendTr))
ACF(na.omit(SerSinTendTr)) # Mostramos ACF del filtrado
PACF(na.omit(SerSinTendTr)) # Mostramos PACF

jarque.bera.test(na.omit(serieTr-SerSinTendTr)) #Tiene que ser mayor que 0.05

#Probamos con diferenciación
d_SerSinTendTr<-diff(serieTr,lag=1)
PLOT(d_SerSinTendTr)
jarque.bera.test(serieTr-d_SerSinTendTr)
SerSinTendTr<-serieTr-TendEstimadaTr

PLOT2(scale(c(d_SerSinTendTr,NA),center=T,scale=F),
      tiempoTr,scale(SerSinTendTr,scale=F))


SerSinTendTr<-serieTr-SerFiltradaTr
PLOT2(c(scale(na.omit(d_SerSinTendTr),center=T,scale=F),NA),
      tiempoTr,scale(SerSinTendTr,center=T,scale=F))


ACF(na.omit(d_SerSinTendTr)) # Mostramos ACF
PACF(na.omit(d_SerSinTendTr))
SerSinTendTr<-d_SerSinTendTr

#Diferenciamos una vez el resultado del filtrado para obtener el ACF y la estacionalidad
ACF(na.omit(diff(SerSinTendTr)))


##################################################################
###            eliminacion de la estacionalidad                ###
##################################################################
#Quitamos estacionalidad
k<-4
estacionalidad<-decompose(serie.ts)$seasonal[1:k]
aux<-rep(estacionalidad,length(serie)/length(estacionalidad))
SerSinTendEstTr<-SerSinTendTr-aux
SerSinTendEstTs<-SerSinTendTs-estacionalidad

PLOT2(SerSinTendEstTr,tiempoTr,SerSinTendTr)

ACF(na.omit(SerSinTendEstTr))
PACF(na.omit(SerSinTendEstTr))

#Quitamos estacionariedad 
adf.test(na.omit(SerSinTendEstTr)) 
#Es mayor que 0.05, por lo que habría que eliminar la estacionariedad

d_SerSinTendEstTr<-diff(na.omit(SerSinTendEstTr),lag = 1)

adf.test(d_SerSinTendEstTr)

ACF(d_SerSinTendEstTr)
PACF(d_SerSinTendEstTr)

PLOT2(scale(SerSinTendEstTr,center=T,scale=F),tiempoTr
      ,c(d_SerSinTendEstTr,rep(NA,length(SerSinTendEstTr)-length(d_SerSinTendEstTr))))

#################################################################
########                   MODELAMOS                      #######
#################################################################

modelo1<-arima(SerSinTendEstTr,order=c(0,1,2)) #(4,3,0)
valoresAjustados<-SerSinTendEstTr+modelo1$residuals

predicciones<-predict(modelo1,n.ahead=NPred)
valoresPredichos<-predicciones$pred

#Calculamos el error cuadrático acumulado del ajuste, tanto en ajuste como en test.
errorTr<-sum((modelo1$residual)^2)
errorTs<-sum(valoresPredichos-SerSinTendEstTs[1:2])^2

dibujatodo(SerSinTendEstTr,SerSinTendEstTs,valoresAjustados,valoresPredichos,
           "Entrenamiento","Test","Ajustados","Predichos")

#Test para la selección del modelo y su validación. Comprobamos primeramente la aleatoriedad
Box.test(modelo1$residuals)

#Ahora vamos a ver si los errores se distribuyen como una normal
jarque.bera.test(na.omit(modelo1$residuals))#podemos asumir la normalidad de los residuos

#Test de normalidad Shapiro-Wilk
shapiro.test(modelo1$residuals)#también podemos asumir la normalidad

#Probamos con otro modelo
modelo2<-arima(SerSinTendEstTr,order=c(1,1,0)) #(4,3,0)
valoresAjustados<-SerSinTendEstTr+modelo2$residuals

predicciones<-predict(modelo2,n.ahead=NPred)
valoresPredichos<-predicciones$pred

#Calculamos el error cuadrático acumulado del ajuste, tanto en ajuste como en test.
errorTr<-sum((modelo2$residual)^2)
errorTs<-sum(valoresPredichos-SerSinTendEstTs[1:2])^2

dibujatodo(SerSinTendEstTr,SerSinTendEstTs,valoresAjustados,valoresPredichos,
           "Entrenamiento","Test","Ajustados","Predichos")


###################################################################
###                   Tests                                     ###
###################################################################

#Tes para la selección del modelo y su validación. Comprobamos primeramente la aleatoriedad
Box.test(modelo2$residuals)

#Ahora vamos a ver si los errores se distribuyen como una normal
jarque.bera.test(na.omit(modelo2$residuals))#podemos asumir la normalidad de los residuos

#Test de normalidad Shapiro-Wilk
shapiro.test(modelo2$residuals)#también podemos asumir la normalidad

#Comparamos cuál de los dos modelos es mejor
AIC(modelo1,modelo2)

####################################################################
###                           Prediccion                         ###
####################################################################
serie<-Datos1
serieEntera<-log(serie) #cogemos toda la serie
tiempo<-1:length(serieEntera)
parametros<-lm(serieEntera~tiempo)#Ajustamos modelo de tendencia
TendEstimada<-parametros$coefficients[1]+tiempo*parametros$coefficients[2]
serieSinTend<-serieEntera-TendEstimada
aux<-ts(serieEntera,frequency=4)
aux<-decompose(aux)$seasonal
estacionalidad<-as.numeric(aux[1:4])
aux<-rep(estacionalidad,length(serieSinTend)/length(estacionalidad))
serieSinTendEst<-serieSinTend-aux
modelo<-arima(serieSinTendEst,order=c(0,1,1)) #(4,3,0)
valoresAjustados<-serieSinTendEst+modelo$residuals
Predicciones<-predict(modelo,n.ahead=NPred)
valoresPredichos<-Predicciones$pred


#Deshacemos los cambios
valoresAjustados<-valoresAjustados+aux
valoresPredichos<-valoresPredichos+estacionalidad[1:2]

valoresAjustados<-valoresAjustados+TendEstimada
tiempoPred<-tiempo[length(tiempo)]+(1:NPred)
parametros <- lm (serieEntera ~ tiempo ) # Ajustamos modelo lineal
TendEstimadaPred<-TendEstimadaPred<-parametros$coefficients[1]+tiempoPred*parametros$coefficients[2]
valoresPredichos<-valoresPredichos+TendEstimadaPred
valoresAjustados<-exp(valoresAjustados)
valoresPredichos<-exp(valoresPredichos)
serie.original<-Datos1


PLOT2(c(serie.original,NA,NA),length(serie.original) ,c(valoresAjustados,valoresPredichos))