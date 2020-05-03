
setwd("/")
setwd("Users/ANA RAQUEL ANDINO/Documents/2020/R")
getwd()
DataPaises <- read.csv("universidades-tratadas.csv",header = T,sep = ",",encoding = "UTF-8")


names(DataPaises) #Nombre de columnas -- Devuelve un vector
library(dplyr)


##Excluyendo Registros de paises de europa
DataPaises<- DataPaises[!(DataPaises$pais %in% c("Portugal","Marruecos","Turquia","Alemania","Espania")),]

##Excluir NA
DataPaisesSinNA<- DataPaises[!is.na(DataPaises$ranking),]
summary(DataPaisesSinNA)

#Media-promedio del IDH
mediaIdh<- mean(DataPaisesSinNA$indice.IDH)
#Mediana del Ranking
medianaRan<- median(DataPaisesSinNA$ranking)


##-------------------------Comparaciones---------------
# Según explicación Allan 
#nivel_academico <- data_regiones$indice.IDH > promedio && data_regiones$ranking > mediana => '<nivel de desarrollo>'
#nivel_academico <- data_regiones$indice.IDH < promedio && data_regiones$ranking > mediana => '<nivel de desarrollo>'
#nivel_academico <- data_regiones$indice.IDH > promedio && data_regiones$ranking < mediana => '<nivel de desarrollo>'
#nivel_academico <- data_regiones$indice.IDH < promedio && data_regiones$ranking < mediana => '<nivel de desarrollo>'

#-----------DataFrame cuadrante I Cat> Nivel EducativoAlto
dfIDHAlto<- DataPaisesSinNA[DataPaisesSinNA$indice.IDH > mediaIdh[1] , ]
dfNivelEducativoAlto<- dfIDHAlto[dfIDHAlto$ranking > medianaRan[1] , ]
##Aniadiendo Categoria
dfNivelEducativoAlto[,"NivelEducativo"]<- "Alto"


#-----------DataFrame cuadrante III Cat> Nivel EducativoBajo
dfIDHBajo<- DataPaisesSinNA[DataPaisesSinNA$indice.IDH < mediaIdh[1] , ]
dfNivelEducativoBajo<- dfIDHBajo[dfIDHBajo$ranking < medianaRan[1] , ]
##Aniadiendo Categoria
dfNivelEducativoBajo[,"NivelEducativo"]<- "Bajo"

#-----------DataFrame cuadrante II  Cat> Nivel EducativoMedio
dfIDHBajo<- DataPaisesSinNA[DataPaisesSinNA$indice.IDH < mediaIdh[1] , ]
dfNivelEducativoMedioII<- dfIDHBajo[dfIDHBajo$ranking > medianaRan[1] , ]
##Aniadiendo Categoria
dfNivelEducativoMedioII[,"NivelEducativo"]<- "Medio"

#-----------DataFrame cuadrante II  Cat> Nivel EducativoMedio
dfIDHAlto<- DataPaisesSinNA[DataPaisesSinNA$indice.IDH > mediaIdh[1] , ]
dfNivelEducativoMedioIV<- dfIDHAlto[dfIDHAlto$ranking < medianaRan[1] , ]
##Aniadiendo Categoria
dfNivelEducativoMedioIV[,"NivelEducativo"]<- "Medio"


##Hacer el AIRbind
DFGeneral <- c()
DFGeneral <- rbind(DFGeneral,dfNivelEducativoAlto)
DFGeneral <- rbind(DFGeneral,dfNivelEducativoMedioII)
DFGeneral <- rbind(DFGeneral,dfNivelEducativoMedioIV)
DFGeneral <- rbind(DFGeneral,dfNivelEducativoBajo)
table(DFGeneral$NivelEducativo)


#Dataframe incluyendo registros con Na en ranking
dfColUnir <- DFGeneral%>% select(columna.name,NivelEducativo) ##seleccionamos solo las col de interes
DataPaises <- left_join(DataPaises,dfColUnir, by=c("columna.name"="columna.name")) ##Hacemos un left join para no perder ningun registro

#Dataframe incluyendo registros con Na en ranking
dfColUnir <- DFGeneral%>% select(columna.name,NivelEducativo) ##seleccionamos solo las col de interes
DataPaisesSinNA <- left_join(DataPaisesSinNA,dfColUnir, by=c("columna.name"="columna.name")) ##Hacemos un left join para no perder ningun registro

write.csv(DataPaisesSinNA,"Universidades-Paises-NivelEducativo.csv",row.names = FALSE)
