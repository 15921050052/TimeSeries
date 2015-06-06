library(ggplot2)
library(reshape2)
library(tseries)
library(timeSeries)
library(xts)
library(devtools)
#install_github('sinhrks/ggfortify')
library(ggfortify)
#install.packages("gridSVG", repos="http://R-Forge.R-project.org")
require(gridSVG)
require(XML)

ACF<-function(serie.ts){
  bacf <- acf(serie.ts, plot = FALSE)
  bacfdf <- with(bacf, data.frame(lag, acf))
  bacfdf$colour <- ifelse((bacfdf$acf < 0.2 | bacfdf$acf < -0.2), "firebrick1",
                       "steelblue")

  if(sum(bacfdf$acf<=0)==0){
    q <- ggplot(data = bacfdf, mapping = aes(x = lag, y = acf)) +
      geom_hline(aes(yintercept = 0)) +
      geom_segment(mapping = aes(xend = lag, yend = 0),col=bpacfdf$colour)+
      geom_line(aes(y = 0.21),col="blue",linetype="dashed")
  }else{
    q <- ggplot(data = bacfdf, mapping = aes(x = lag, y = acf)) +
      geom_hline(aes(yintercept = 0)) +
      geom_segment(mapping = aes(xend = lag, yend = 0),col=bpacfdf$colour)+
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







#get the latest version of gridSVG



#draw a ggplot2 graph
#thanks http://sharpstatistics.co.uk/r/ggplot2-guide/
p <- ACF(serie.ts)
p + facet_grid(. ~ Species) + stat_smooth(method = "lm")

#define a simple html head template
htmlhead <- 
  '<!DOCTYPE html>
<head>
<meta charset = "utf-8">
<script src = "http://d3js.org/d3.v3.js"></script>
</head>

<body>
'

#use gridSVG to export our plot to SVG
mysvg <- grid.export("panzoom1.svg")


#define a simple pan zoom script using d3
panzoomScript <-
  '  <script>
var svg = d3.selectAll("#gridSVG");
svg.call(d3.behavior.zoom().scaleExtent([1, 8]).on("zoom", zoom))

function zoom() {
svg.attr("transform", "translate(" + d3.event.translate + ")scale(" + d3.event.scale + ")");
} 
</script>
</body>
'

#combine all the pieces into an html file
sink("panzoom_ggplot2.html")
cat(htmlhead,saveXML(mysvg$svg),panzoomScript)
#close our file
sink(file=NULL)
