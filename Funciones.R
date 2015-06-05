FiltrarSerie<-function(serie, k){
  filtro<-rep(1/k, k); # Creamos el filtro
  
  # Filtramos señal
  SerFiltrada<-filter(serie,filter=filtro,sides=2,method="convolution")
  
  # Mostramos en la misma figura la serie y la tendencia estimada
  series<-matrix(c(t(serie), t(SerFiltrada)), ncol=2);
  matplot(series, pch=1, type= "l")
  SerFiltrada
  }



CargarFlujosDatos<- function(Experimento,tipo){
  listaSalida<-list()
  if(tipo=="ADWIN"){
      listaSalida<-lapply(1:5,function(x){
      ADWIN<-read.csv(file = paste(sep = "","Flujo de datos/ADWIN Exp ",Experimento,
                              "/ADWIN_",Experimento,"_",x,".csv"),na.strings = "?")
        ADWIN
      })    
  }else if(tipo=="DDM"){
    listaSalida<-lapply(1:5,function(x){
      DDM<-read.csv(file = paste(sep = "","Flujo de datos/DDM Exp ",Experimento,
                                 "/DDM_",Experimento,"_",x,".csv"),na.strings = "?")
      DDM
    })   
  }else{
      listaSalida<-NULL
  }
  listaSalida
}


MTD<-function(Lista){
  mtd<-mean(unlist(lapply(1:5,function(x){
    Lista[[x]][is.na(Lista[[x]])] <- 0
    mean(Lista[[x]][,9])
  })))
  mtd
}

MDR<-function(Lista){
  mdr<-mean(unlist(lapply(1:5,function(x){
    Lista[[x]][is.na(Lista[[x]])] <- 0
    if(sum(Lista[[x]][,8])==0){
      sal<-1
    }else{
      sal<-1.0-(sum(Lista[[x]][,10])/sum(Lista[[x]][,8])) 
    }
    sal
  })))
  mdr
}