if(!require(tseries)){
  install.packages("tseries")
  library(tseries)
}
require(forecast)
NTest<-4
NPred<-4

##################################################
#Cargar los datos
##################################################

#Serie 1 contiene el número de personas residentes en
#Australia (en miles), muestreados trimestralmente 
#desde el segundo trimestre de 1971 hasta el final
#de 1992.
Datos1 <- scan("serie1.dat")

#Serie 2 contiene las medidas anuales del nivel del
#lago Hurón, medidos en pies, entre 1875 y 1960.
Datos2 <- scan("serie2.dat")

##################################################

