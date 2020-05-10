setwd("/")
setwd("Users/ANA RAQUEL ANDINO/Documents/2020/R/tratamientoNivelEducativo")
getwd()
DataF <- read.csv("courses_data_cleaned_institucion.csv",header = T,sep = ",",encoding = "UTF-8")


library(dplyr)
prop.table(table(DataF$institucion))
dfUni <- as.data.frame(prop.table(table(DataF$institucion)))
#1. tomoar el nombre de todas las instituciones diferentes (35 niveles)
dfUni<- dfUni %>% select(Var1) 
#2. Escribir un csv solo con las universidades Diferentes
write.csv(dfUni,"Universidades-Paises-NivelEducativo.csv",row.names = FALSE)



#4 Lectura de csv con ranking y IDH y Nivel educativo tratado
DataIdhRan <- read.csv("Universidades-Paises-NivelEducativo.csv",header = T,sep = ",",encoding = "UTF-8")
names(DataIdhRan)
#5.Seleccionar variables de interes
DataIdhRan<-DataIdhRan%>% select(indiceIDH,ranking,universidadEquivalente,NivelEducativo)
#6. Left Join con Data Frame General
DataF <- left_join(DataF,DataIdhRan, by=c("institucion"="universidadEquivalente"))
#7. Eliminar registros NA en Ranking 
##De acuerdo al analisi de universidad  equivalente, y no tienen ranking suman un total de 1224 registros
DataF$institucion<- as.factor(DataF$institucion)
DataF<- DataF[!is.na(DataF$ranking),]

write.csv(DataF,"courses_data_cleaned_ranking_idh.csv",row.names = FALSE)







