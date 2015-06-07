################################################################################
###                                 Librerias                                ###
################################################################################
require(ggplot2)
require(reshape2)
require(tseries)
require(timeSeries)
require(xts)
require(devtools)
#install_github('sinhrks/ggfortify')
require(ggfortify)
#install.packages("gridSVG", repos="http://R-Forge.R-project.org")
require(gridSVG)
require(XML)


################################################################################
###                       Funciones series temporales                        ###
################################################################################
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
      sal<-0
    }else{
      sal<-1.0-(sum(Lista[[x]][,10])/sum(Lista[[x]][,8])) 
    }
    sal
  })))
  mdr
}

################################################################################
###                                 Graficos                                 ###
################################################################################

ACF<-function(serie.ts){
  bacf <- acf(serie.ts, plot = FALSE)
  bacfdf <- with(bacf, data.frame(lag, acf))
  bacfdf$colour <- ifelse((bacfdf$acf < 0.2 & bacfdf$acf > -0.2), "red",
                          "green")
  
  if(sum(bacfdf$acf<=0)==0){
    q <- ggplot(data = bacfdf, mapping = aes(x = lag, y = acf)) +
      geom_hline(aes(yintercept = 0)) +
      geom_segment(mapping = aes(xend = lag, yend = 0),col=bacfdf$colour)+
      geom_line(aes(y = 0.21),col="blue",linetype="dashed")
  }else{
    q <- ggplot(data = bacfdf, mapping = aes(x = lag, y = acf)) +
      geom_hline(aes(yintercept = 0)) +
      geom_segment(mapping = aes(xend = lag, yend = 0),col=bacfdf$colour)+
      geom_line(aes(y = 0.21),col="blue",linetype="dashed")+
      geom_line(aes(y = -0.21),col="blue",linetype="dashed")
  }
  
  q
}

PACF<-function(serie.ts){
  bpacf <- pacf(serie.ts, plot = F)
  bpacfdf <- with(bpacf, data.frame(lag, acf))
  bpacfdf$colour <- ifelse((bpacfdf$acf < 0.2 & bpacfdf$acf > -0.2), "red",
                           "green")
  if(sum(bpacfdf$acf<=0)==0){
    q <- ggplot(data = bpacfdf, mapping = aes(x = lag, y = acf)) +
      ylab(label = "PACF")+
      geom_hline(aes(yintercept = 0)) +
      geom_segment(mapping = aes(xend = lag, yend = 0),col=bpacfdf$colour)+
      geom_line(aes(y = 0.21),col="blue",linetype="dashed")+
      theme(legend.position="none")
  }else{
    q <- ggplot(data = bpacfdf, mapping = aes(x = lag, y = acf)) +
      ylab(label = "PACF")+
      geom_hline(aes(yintercept = 0)) +
      geom_segment(mapping = aes(xend = lag, yend = 0),col=bpacfdf$colour)+
      geom_line(aes(y = 0.21),col="blue",linetype="dashed")+
      geom_line(aes(y = -0.21),col="blue",linetype="dashed")+
      theme(legend.position="none")
  }
  
  q
}

DESCOMPOSE<-function(serie.ts){
  datos<-data.frame(Tiempo=1:length(as.vector(as.matrix(decompose(serie.ts)$x))),
                    Original=as.vector(as.matrix(decompose(serie.ts)$x)))
  Tendencia<-as.vector(as.matrix(decompose(serie.ts)$trend))
  Estacionalidad<-as.vector(as.matrix(decompose(serie.ts)$seasonal))
  Aleatoriedad<-as.vector(as.matrix(decompose(serie.ts)$random))
  df<-data.frame(datos,Trend=Tendencia,Seasonal=Estacionalidad,Random=Aleatoriedad)
  
  dfm<-melt(df,id="Tiempo")
  
  ggplot(na.omit(dfm),aes(x=Tiempo,y=value,colour=variable,group=variable))+geom_line()+
    facet_grid(variable ~ . , scales = "free")
  
}

PLOT<-function(serie.ts){
  Serie<-data.frame(tiempo=1:length(serie.ts),valor=serie.ts)
  ggplot(Serie,aes(x=tiempo,y=valor))+geom_line(col="blue")+
    theme(axis.title.x = element_blank(),axis.title.y=element_blank())
}

PLOT2<-function(serie.ts,tiempoTr,serie.ts2){
  Serie<-data.frame(tiempo=1:length(serie.ts),valor=serie.ts,valor1=serie.ts2)
  ggplot(Serie,aes(x=tiempo,y=valor))+
    geom_line(aes(y=valor),col=ifelse(Serie$tiempo<=tiempoTr[length(tiempoTr)],"blue","red"),size=1)+
    geom_line(aes(y=valor1),col=ifelse(Serie$tiempo<=tiempoTr[length(tiempoTr)],"green","yellow"),size=1)+
    theme(axis.title.x = element_blank(),axis.title.y=element_blank())
  
}

HTML<-function(Grafico,Nombre){
  p<-Grafico
  
  
  htmlhead <-'<!DOCTYPE html>
  <head>
  <meta charset = "utf-8">
  <script src = "http://d3js.org/d3.v3.js"></script>
  </head>
  
  <body>
  '
  
  #use gridSVG to export our plot to SVG
  mysvg <- grid.export(paste(Nombre,".svg"))
  
  
  #define a simple pan zoom script using d3
  Script <-'  <script>
  var svg = d3.selectAll("#gridSVG");
  svg.call(d3.behavior.zoom().scaleExtent([1, 8]).on("zoom", zoom))
  
  function zoom() {
  svg.attr("transform", "translate(" + d3.event.translate + ")scale(" + d3.event.scale + ")");
  } 
  </script>
  </body>'
  
  #combine all the pieces into an html file
  sink(paste(Nombre,"_ggplot2.html"))
  cat(htmlhead,saveXML(mysvg$svg),Script)
  #close our file
  sink(file=NULL)
}














