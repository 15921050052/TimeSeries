source("Funciones.R")
################################################################################
###                           Experimento 1                                  ###
################################################################################
print("Experimento 1:")
### Cargar datos ADWIN
Experiment_1_ADWIN<-CargarFlujosDatos(Experimento = 1,tipo = "ADWIN")
Exp1_MDR_ADWIN<-MDR(Experiment_1_ADWIN)
print(Exp1_MDR_ADWIN)
Exp1_MTD_ADWIN<-MTD(Experiment_1_ADWIN)
print(Exp1_MTD_ADWIN)


### Cargar datos DNN
Experiment_1_DDM<-CargarFlujosDatos(Experimento = 1,tipo = "DDM")
Exp1_MDR_DMM<-MDR(Experiment_1_DDM)
print(Exp1_MDR_DMM)
Exp1_MTD_DMM<-MTD(Experiment_1_DDM)
print(Exp1_MTD_DMM)

datos<-data.frame(alg=c(1,1,1,1)
                  ,valores=c(Exp1_MDR_ADWIN,Exp1_MTD_ADWIN,Exp1_MDR_DMM,Exp1_MTD_DMM),
                  Medida=c("MDR","MTD","MDR","MTD"),Metodo=c("ADWIN","ADWIN","DDM","DDM"))



################################################################################
###                           Experimento 2                                  ###
################################################################################

print("Experimento 2:")
### Cargar datos ADWIN
Experiment_2_ADWIN<-CargarFlujosDatos(Experimento = 2,tipo = "ADWIN")
Exp2_MDR_ADWIN<-MDR(Experiment_2_ADWIN)
print(Exp2_MDR_ADWIN)
Exp2_MTD_ADWIN<-MTD(Experiment_2_ADWIN)
print(Exp2_MTD_ADWIN)
### Cargar datos DNN
Experiment_2_DDM<-CargarFlujosDatos(Experimento = 2,tipo = "DDM")
Exp2_MDR_DMM<-MDR(Experiment_2_DDM)
print(Exp2_MDR_DMM)
Exp2_MTD_DMM<-MTD(Experiment_2_DDM)
print(Exp2_MTD_DMM)

datos<-as.data.frame(rbind(datos,data.frame(alg=c(2,2,2,2)
                  ,valores=c(Exp2_MDR_ADWIN,Exp2_MTD_ADWIN,Exp2_MDR_DMM,Exp2_MTD_DMM),
                  Medida=c("MDR","MTD","MDR","MTD"),Metodo=c("ADWIN","ADWIN","DDM","DDM"))))


################################################################################
###                           Experimento 3                                  ###
################################################################################

print("Experimento 3:")
### Cargar datos ADWIN
Experiment_3_ADWIN<-CargarFlujosDatos(Experimento = 3,tipo = "ADWIN")
Exp3_MDR_ADWIN<-MDR(Experiment_3_ADWIN )
print(Exp3_MDR_ADWIN)
Exp3_MTD_ADWIN<-MTD(Experiment_3_ADWIN)
print(Exp3_MTD_ADWIN)
### Cargar datos DNN
Experiment_3_DDM<-CargarFlujosDatos(Experimento = 3,tipo = "DDM")
Exp3_MDR_DMM<-MDR(Experiment_3_DDM)
print(Exp3_MDR_DMM)
Exp3_MTD_DMM<-MTD(Experiment_3_DDM)
print(Exp3_MTD_DMM)

datos<-as.data.frame(rbind(datos,data.frame(alg=c(3,3,3,3)
                                            ,valores=c(Exp3_MDR_ADWIN,Exp3_MTD_ADWIN,Exp3_MDR_DMM,Exp3_MTD_DMM),
                                            Medida=c("MDR","MTD","MDR","MTD"),Metodo=c("ADWIN","ADWIN","DDM","DDM"))))


################################################################################
###                           Experimento 4                                  ###
################################################################################

print("Experimento 4:")
### Cargar datos ADWIN
Experiment_4_ADWIN<-CargarFlujosDatos(Experimento = 4,tipo = "ADWIN")
Exp4_MDR_ADWIN<-MDR(Experiment_4_ADWIN)
print(Exp4_MDR_ADWIN)
Exp4_MTD_ADWIN<-MTD(Experiment_4_ADWIN)
print(Exp4_MTD_ADWIN)
### Cargar datos DNN
Experiment_4_DDM<-CargarFlujosDatos(Experimento = 4,tipo = "DDM")
Exp4_MDR_DMM<-MDR(Experiment_4_DDM)
print(Exp4_MDR_DMM)
Exp4_MTD_DMM<-MTD(Experiment_4_DDM)
print(Exp4_MTD_DMM)

datos<-as.data.frame(rbind(datos,data.frame(alg=c(4,4,4,4)
                                            ,valores=c(Exp4_MDR_ADWIN,Exp4_MTD_ADWIN,Exp4_MDR_DMM,Exp4_MTD_DMM),
                                            Medida=c("MDR","MTD","MDR","MTD"),Metodo=c("ADWIN","ADWIN","DDM","DDM"))))

################################################################################
###                           Experimento 5                                  ###
################################################################################

print("Experimento 5:")
### Cargar datos ADWIN
Experiment_5_ADWIN<-CargarFlujosDatos(Experimento = 5,tipo = "ADWIN")
Exp5_MDR_ADWIN<-MDR(Experiment_5_ADWIN)
print(Exp5_MDR_ADWIN)
Exp5_MTD_ADWIN<-MTD(Experiment_5_ADWIN)
print(Exp5_MTD_ADWIN)
### Cargar datos DNN
Experiment_5_DDM<-CargarFlujosDatos(Experimento = 5,tipo = "DDM")
Exp5_MDR_DMM<-MDR(Experiment_5_DDM)
print(Exp5_MDR_DMM)
Exp5_MTD_DMM<-MTD(Experiment_5_DDM)
print(Exp5_MTD_DMM)



datos<-as.data.frame(rbind(datos,data.frame(alg=c(5,5,5,5)
                                                   ,valores=c(Exp5_MDR_ADWIN,Exp5_MTD_ADWIN,Exp5_MDR_DMM,Exp5_MTD_DMM),
                                                   Medida=c("MDR","MTD","MDR","MTD"),Metodo=c("ADWIN","ADWIN","DDM","DDM"))))
         

qplot(alg, valores, data=datos[datos$Medida=="MDR",],main = "MDR",
      geom="bar", stat="identity", fill=valores)+ facet_grid(Metodo ~ .)

qplot(alg, valores, data=datos[datos$Medida=="MTD",],main = "MTD",
      geom="bar", stat="identity", fill=valores)+ facet_grid(Metodo ~ .)

datos$Medida=="MDR"

##########################
##Graficas
##########################