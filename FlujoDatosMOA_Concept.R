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
                  Medida=c("MDR","MTD","MDR","MTD"))



################################################################################
###                           Experimento 2                                  ###
################################################################################

print("Experimento 2:")
### Cargar datos ADWIN
Experiment_2_ADWIN<-CargarFlujosDatos(Experimento = 2,tipo = "ADWIN")
Exp2_MDR_DMM<-MDR(Experiment_2_ADWIN)
print(Exp2_MDR_DMM)
Exp2_MTD_DMM<-MTD(Experiment_2_ADWIN)
print(Exp2_MTD_DMM)
### Cargar datos DNN
Experiment_2_DDM<-CargarFlujosDatos(Experimento = 2,tipo = "DDM")
Exp2_MDR_DMM<-MDR(Experiment_2_DDM)
print(Exp2_MDR_DMM)
Exp2_MTD_DMM<-MTD(Experiment_2_DDM)
print(Exp2_MTD_DMM)

datos<-as.data.frame(rbind(datos,data.frame(alg=c(2,2,2,2)
                  ,valores=c(Exp2_MDR_ADWIN,Exp2_MTD_ADWIN,Exp2_MDR_DMM,Exp2_MTD_DMM),
                  Medida=c("MDR","MTD","MDR","MTD"))))


################################################################################
###                           Experimento 3                                  ###
################################################################################

print("Experimento 3:")
### Cargar datos ADWIN
Experiment_3_ADWIN<-CargarFlujosDatos(Experimento = 3,tipo = "ADWIN")
Exp1_MDR_DMM
print(MDR(Experiment_3_ADWIN ))
Exp5_MTD_DMM
print(MTD(Experiment_3_ADWIN))
### Cargar datos DNN
Experiment_3_DDM<-CargarFlujosDatos(Experimento = 3,tipo = "DDM")
Exp1_MDR_DMM
print(MDR(Experiment_3_DDM))
Exp5_MTD_DMM
print(MTD(Experiment_3_DDM))

datos<-data.frame(alg=c(3,3,3,3)
                  ,valores=c(Exp1_MDR_ADWIN,Exp1_MTD_ADWIN,Exp1_MDR_DMM,Exp1_MTD_DMM),
                  Medida=c("MDR","MTD","MDR","MTD"))


################################################################################
###                           Experimento 4                                  ###
################################################################################

print("Experimento 4:")
### Cargar datos ADWIN
Experiment_4_ADWIN<-CargarFlujosDatos(Experimento = 4,tipo = "ADWIN")
Exp1_MDR_DMM
print(MDR(Experiment_4_ADWIN))
Exp5_MTD_DMM
print(MTD(Experiment_4_ADWIN))
### Cargar datos DNN
Experiment_4_DDM<-CargarFlujosDatos(Experimento = 4,tipo = "DDM")
Exp1_MDR_DMM
print(MDR(Experiment_4_DDM))
Exp5_MTD_DMM
print(MTD(Experiment_4_DDM))

datos<-data.frame(alg=c(4,4,4,4)
                  ,valores=c(Exp1_MDR_ADWIN,Exp1_MTD_ADWIN,Exp1_MDR_DMM,Exp1_MTD_DMM),
                  Medida=c("MDR","MTD","MDR","MTD"))


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







datos<-data.frame(alg=c(1:4)
                  ,valores=c(Exp5_MDR_ADWIN,Exp5_MTD_ADWIN,Exp5_MDR_DMM,Exp5_MTD_DMM),
                  MDR=c(T,F,T,F))
qplot(alg, valores, data=datos, geom="bar", stat="identity", fill=MDR)




##########################
##Graficas
##########################