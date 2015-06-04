################################################################################
###                           Experimento 1                                  ###
################################################################################


### Cargar datos ADWIN
Experiment_1_ADWIN<-CargarFlujosDatos(Experimento = 1,tipo = "ADWIN")
print(MDR(Experiment_1_ADWIN ))
print(MTD(Experiment_1_ADWIN))
### Cargar datos DNN
Experiment_1_DDM<-CargarFlujosDatos(Experimento = 1,tipo = "DDM")
print(MDR(Experiment_1_DDM))
print(MTD(Experiment_1_DDM))





Meadias<-mean(unlist(lapply(1:5,function(x){
  Experiment_1_ADWIN[[x]][is.na(Experiment_1_ADWIN[[x]])] <- 0
  mean(Experiment_1_ADWIN[[x]][,9])
})))




Medias<-mean(unlist(lapply(1:5,function(x){
  Experiment_1_DDM[[x]][is.na(Experiment_1_DDM[[x]])] <- 0
  mean(Experiment_1_ADWIN[[x]][,9])
})))

################################################################################
###                           Experimento 2                                  ###
################################################################################












ADWIN_2_1<-read.csv("Flujo de datos/ADWIN Exp 2/ADWIN_2_1.csv",na.strings = "?")
ADWIN_3_1<-read.csv("Flujo de datos/ADWIN Exp 3/ADWIN_3_1.csv",na.strings = "?")
ADWIN_4_1<-read.csv("Flujo de datos/ADWIN Exp 4/ADWIN_4_1.csv",na.strings = "?")
ADWIN_5_1<-read.csv("Flujo de datos/ADWIN Exp 5/ADWIN_5_1.csv",na.strings = "?")



mean(ADWIN_5_1$delay.detection..average.,na.rm =T)

plot(ADWIN_5_1$delay.detection..average.)
plot(ADWIN_5_1$true.changes.detected)

plot(ADWIN_5_1$detected.changes)
plot(ADWIN_5_1$delay.detection..average.)
