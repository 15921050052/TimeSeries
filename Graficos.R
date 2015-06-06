library(ggplot2)
library(reshape2)
library(tseries)
library(timeSeries)
library(xts)
library(devtools)
#install_github('sinhrks/ggfortify')
library(ggfortify)


ACF<-function(serie.ts){
  bacf <- acf(serie.ts, plot = FALSE)
  bacfdf <- with(bacf, data.frame(lag, acf))
  bacfdf$colour <- ifelse((bacfdf$acf < 0.2 | bacfdf$acf < -0.2), "firebrick1",
                       "steelblue")
  #bacfdf$hjust <- ifelse(bacfdf$acf > 0, 0.2, -0.2)
#   ggplot(bacfdf, aes(month, value, label = month,
#                     hjust = hjust)) + geom_text(aes(y = 0,
#                     colour = colour)) + geom_bar(stat = "identity",
#                      aes(fill = colour))
  if(sum(bacfdf$acf<=0)==0){
    q <- ggplot(data = bacfdf, mapping = aes(x = lag, y = acf)) +
      geom_hline(aes(yintercept = 0)) +
      geom_segment(mapping = aes(xend = lag, yend = 0,colour=colour))+
      geom_line(aes(y = 0.21),col="blue",linetype="dashed")
  }else{
    q <- ggplot(data = bacfdf, mapping = aes(x = lag, y = acf)) +
      geom_hline(aes(yintercept = 0)) +
      geom_segment(mapping = aes(xend = lag, yend = 0,colour=colour))+
      geom_line(aes(y = 0.21),col="blue",linetype="dashed")+
      geom_line(aes(y = -0.21),col="blue",linetype="dashed")
  }
  
  q
}


PACF<-function(serie.ts){
  bpacf <- pacf(serie.ts, plot = F)
  bpacfdf <- with(bpacf, data.frame(lag, acf))
  bpacfdf$colour <- ifelse((bacfdf$acf < 0.2 | bacfdf$acf < -0.2), "red",
                          "green")
  if(sum(bpacfdf$acf<=0)==0){
    q <- ggplot(data = bpacfdf, mapping = aes(x = lag, y = acf)) +
      ylab(label = "PACF")+
      geom_hline(aes(yintercept = 0)) +
      geom_segment(mapping = aes(xend = lag, yend = 0),col=bpacfdf$colour,lineend = "butt")+
      geom_line(aes(y = 0.21),col="blue",linetype="dashed")+
      theme(legend.position="none")
  }else{
    q <- ggplot(data = bpacfdf, mapping = aes(x = lag, y = acf)) +
      ylab(label = "PACF")+
      geom_hline(aes(yintercept = 0)) +
      geom_segment(mapping = aes(xend = lag, yend = 0),col=bpacfdf$colour,lineend = "butt")+
      geom_line(aes(y = 0.21),col="blue",linetype="dashed")+
      geom_line(aes(y = -0.21),col="blue",linetype="dashed")+
      theme(legend.position="none")
  }
  
  q
}

DESCOMPOSE<-function(serie.ts){
  seriex<-data.frame(Tiempo=1:length(as.vector(as.matrix(decompose(serie.ts)$x))),
                     Original=as.vector(as.matrix(decompose(serie.ts)$x)))
  serietrend<-as.vector(as.matrix(decompose(serie.ts)$trend))
  serieseasonal<-as.vector(as.matrix(decompose(serie.ts)$seasonal))
  serierandom<-as.vector(as.matrix(decompose(serie.ts)$random))
  df<-data.frame(seriex,Trend=serietrend,Seasonal=serieseasonal,Random=serierandom)
  dfm<-melt(df,id="Tiempo")
  
  ggplot(na.omit(dfm),aes(x=Tiempo,y=value,colour=variable,group=variable))+geom_line()+
    facet_grid(variable ~ . , scales = "free")
  
}
