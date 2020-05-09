setwd("/")
setwd("Users/ANA RAQUEL ANDINO/Documents/2020/R/Ranking-idh")
getwd()
DataF <- read.csv("Tratamiento-nulos-main.csv",header = T,sep = ",",encoding = "UTF-8")

library(dplyr)


##--------------------------Tratamiento Universidades---------------------

prop.table(table(DataF$institucion))
dfUni <- as.data.frame(prop.table(table(DataF$institucion)))

#1. tomoar el nombre de todas las instituciones diferentes (47 niveles)
dfUni<- dfUni %>% select(Var1) 

#2. Escribir un csv solo con las universidades
write.csv(dfUni,"UniversidadesDF.csv",row.names = FALSE)

#3. Leer archivo csv generado
dfUni <- read.csv("UniversidadesDF.csv",header = T,sep = ",",encoding = "UTF-8")

#4. Left join con csv original
DataF <- left_join(DataF,dfUni, by=c("institucion"="universidad")) ##Hacemos un left join para no perder ningun registro

#5 comprobar
table(DataF$equivalente)

#Cambiar nombre de columna
DataF <- DataF[,!(names(DataF) %in% c("institucion"))] ##Quitamos la columna que transformamos
names(DataF)[length(names(DataF))] <- "institucion" ##Cambiamos nombre a institucion, que es la ultima columna de survey y lo obtenemos con lenght

#6 Lectura de csv con ranking y IDH
DataIdhRan <- read.csv("Universidades-Paises-NivelEducativo.csv",header = T,sep = ",",encoding = "UTF-8")
names(DataIdhRan)
#7.Seleccionar variables de interes
DataIdhRan<-DataIdhRan%>% select(indiceIDH,ranking,universidadEquivalente)
# 8. Left Join con Data Frame General
DataF <- left_join(DataF,DataIdhRan, by=c("institucion"="universidadEquivalente"))
# 9. Eliminar registros NA en Ranking 
DataF$institucion<- as.factor(DataF$institucion)
DataF<- DataF[!is.na(DataF$ranking),]

write.csv(DataF,"courses_data_cleaned_ranking_idh.csv",row.names = FALSE)




##De acuerdo al analisi de nombre  equivalente, y no tienen ranking suman un total de 1224 registros


##----------------------------Tratamiento Nivel educativo--------------

#cargamos csv con el nivel educativo
DataG <- read.csv("tratamiento-institucion.csv",header = T,sep = ",",encoding = "UTF-8")
DataU <- read.csv("Universidades-Paises-NivelEducativo.csv",header = T,sep = ",",encoding = "UTF-8")
str(DataU)
#Columnas que nos interesan
DataU<-DataU%>% select(NivelEducativo,universidadEquivalente)
##Leftjoin con csv nivel educativo
DataF <- left_join(DataF,DataU, by=c("institucion"="universidadEquivalente")) ##Hacemos un left join para no perder ningun registro
##comprobar que hay 1224 registros
str(DataF)
DataF$institucion <- as.factor(DataF$institucion)
table(DataF$NivelEducativo)
names(DataF)

##Descartar archivos que no tienen nivel educativo
Sinnivel<- DataF[is.na(DataF$NivelEducativo),]
DataF<- DataF[!is.na(DataF$NivelEducativo),]

write.csv(DataF,"tratamiento-nivelEducativo.csv",row.names = FALSE)


