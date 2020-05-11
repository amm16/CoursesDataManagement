setwd("/")
setwd("Users/ANA RAQUEL ANDINO/Documents/2020/R")
getwd()
DataPaises <- read.csv("Universidades-Paises-NivelEducativo.csv",header = T,sep = ",",encoding = "UTF-8")

DataPaises<-DataPaises[,!names(DataPaises)=="NivelEducativo"]
names(DataPaises) #Nombre de columnas -- Devuelve un vector
library(dplyr)


##Excluyendo Registros de paises de europa
DataPaises<- DataPaises[!(DataPaises$pais %in% c("Portugal","Marruecos","Turquia","Alemania","Espania")),]

##Excluir NA
DataPaisesSinNA<- DataPaises[!is.na(DataPaises$ranking),]
summary(DataPaisesSinNA)

#Media-promedio del IDH
mediaIdh<- mean(DataPaises$indiceIDH)
#Mediana del Ranking
medianaRan<- median(DataPaises$ranking)


##-------------------------Comparaciones---------------
# Según explicación Allan 
#nivel_academico <- data_regiones$indice.IDH > promedio && data_regiones$ranking > mediana => '<nivel de desarrollo>'
#nivel_academico <- data_regiones$indice.IDH < promedio && data_regiones$ranking > mediana => '<nivel de desarrollo>'
#nivel_academico <- data_regiones$indice.IDH > promedio && data_regiones$ranking < mediana => '<nivel de desarrollo>'
#nivel_academico <- data_regiones$indice.IDH < promedio && data_regiones$ranking < mediana => '<nivel de desarrollo>'

#-----------DataFrame cuadrante IV Cat> Nivel EducativoAlto
dfIDHAlto<- DataPaisesSinNA[DataPaisesSinNA$indiceIDH >= mediaIdh[1] , ]
dfNivelEducativoAlto<- dfIDHAlto[dfIDHAlto$ranking <= medianaRan[1] , ]
##Aniadiendo Categoria
dfNivelEducativoAlto[,"NivelEducativo"]<- "Muy Alto"


#-----------DataFrame cuadrante II Cat> Nivel EducativoBajo
dfIDHBajo<- DataPaisesSinNA[DataPaisesSinNA$indiceIDH < mediaIdh[1] , ]
dfNivelEducativoBajo<- dfIDHBajo[dfIDHBajo$ranking > medianaRan[1] , ]
##Aniadiendo Categoria
dfNivelEducativoBajo[,"NivelEducativo"]<- "Muy Bajo"

#-----------DataFrame cuadrante I  Cat> Nivel EducativoMedio
dfIDHBajoM<- DataPaisesSinNA[DataPaisesSinNA$indiceIDH >= mediaIdh[1] , ]
dfNivelEducativoMedioII<- dfIDHBajoM[dfIDHBajoM$ranking > medianaRan[1] , ]
##Aniadiendo Categoria
dfNivelEducativoMedioII[,"NivelEducativo"]<- "Bajo"

#-----------DataFrame cuadrante III  Cat> Nivel EducativoMedio
dfIDHAltoM<- DataPaisesSinNA[DataPaisesSinNA$indiceIDH < mediaIdh[1] , ]
dfNivelEducativoMedioIV<- dfIDHAltoM[dfIDHAltoM$ranking <= medianaRan[1] , ]

##Aniadiendo Categoria (La cual se tratata en posterior)
dfNivelEducativoMedioIV[,"NivelEducativo"]<- "MedioAlto"


##Hacer el AIRbind
DFGeneral <- c()
DFGeneral <- rbind(DFGeneral,dfNivelEducativoAlto)
DFGeneral <- rbind(DFGeneral,dfNivelEducativoMedioII)
DFGeneral <- rbind(DFGeneral,dfNivelEducativoBajo)

table(DFGeneral$NivelEducativo)


#Dataframe incluyendo registros con Na en ranking
dfColUnir <- DFGeneral%>% select(columna.name,NivelEducativo) ##seleccionamos solo las col de interes
DataPaises <- left_join(DataPaises,dfColUnir, by=c("columna.name"="columna.name")) ##Hacemos un left join para no perder ningun registro

#Dataframe incluyendo registros con Na en ranking
dfColUnir <- DFGeneral%>% select(columna.name,NivelEducativo) ##seleccionamos solo las col de interes
DataPaisesSinNA <- left_join(DataPaisesSinNA,dfColUnir, by=c("columna.name"="columna.name")) ##Hacemos un left join para no perder ningun registro


#Tratar Medio Alto
DfMedioAlto<-dfNivelEducativoMedioIV



##-------------------------------------------***********##Aparte medio alto medio bajo*************--------------------------



Datax<-DfMedioAlto[,!names(DfMedioAlto)=="NivelEducativo"]

#Media-promedio del IDH
mediaIdh<- mean(Datax$indiceIDH)
#Mediana del Ranking
medianaRan<- median(Datax$ranking)


#-----------DataFrame cuadrante IV Cat> Nivel EducativoAlto
dfIDHAltoNuevo<- Datax[Datax$indiceIDH >= mediaIdh[1] , ]
dfNivelEducativoAlto<- dfIDHAltoNuevo[dfIDHAltoNuevo$ranking <= medianaRan[1] , ]
##Aniadiendo Categoria
dfNivelEducativoAlto[,"NivelEducativo"]<- "Alto"


#-----------DataFrame cuadrante II Cat> Nivel EducativoBajo
dfIDHBajoNuevo<- Datax[Datax$indiceIDH < mediaIdh[1] , ]
dfNivelEducativoBajo<- dfIDHBajoNuevo[dfIDHBajoNuevo$ranking > medianaRan[1] , ]
##Aniadiendo Categoria
dfNivelEducativoBajo[,"NivelEducativo"]<- "Medio"

#-----------DataFrame cuadrante I  Cat> Nivel EducativoMedio
dfIDHBajoM<- Datax[Datax$indiceIDH >= mediaIdh[1] , ]
dfNivelEducativoMedioII<- dfIDHBajoM[dfIDHBajoM$ranking > medianaRan[1] , ]
##Aniadiendo Categoria
dfNivelEducativoMedioII[,"NivelEducativo"]<- "Medio"

#-----------DataFrame cuadrante III  Cat> Nivel EducativoMedio
dfIDHAltoN<- Datax[Datax$indiceIDH < mediaIdh[1] , ]
dfNivelEducativoMedioIV<- dfIDHAltoN[dfIDHAltoN$ranking <= medianaRan[1] , ]
##Aniadiendo Categoria
dfNivelEducativoMedioIV[,"NivelEducativo"]<- "Medio"

DFGeneral <- c()
DFGeneral <- rbind(DFGeneral,dfNivelEducativoAlto)
DFGeneral <- rbind(DFGeneral,dfNivelEducativoMedioII)
DFGeneral <- rbind(DFGeneral,dfNivelEducativoBajo)
table(DFGeneral$NivelEducativo)


write.csv(DFGeneral,"Universidades-Paises-NivelEducativo2.csv",row.names = FALSE)


