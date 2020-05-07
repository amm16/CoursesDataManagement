
DataF <- read.csv("courses_data_analisis_categorias_nivel_estado.csv",header = T,sep = ",",encoding = "UTF-8")

library(dplyr)
summary(DataF)
str(DataF)

#Analisis Descriptivo de instituciones
df_per_Institucion<-as.data.frame(prop.table(table(DataF$institucion))) %>% arrange(Freq)

boxplot(df_per_Institucion$Freq) #Se observaron valores atipicos por lo cual se decidio hacer un tramiento


##--------------------------Tratamiento Universidades---------------------
#Obtengo todas las universidades diferentes y su prop
dfUni <- as.data.frame(prop.table(table(DataF$institucion)))

#1. tomo el nombre de todas las instituciones diferentes (47 niveles)
dfUni<- dfUni %>% select(Var1) 

#2. Escribir un csv solo con las universidades
write.csv(dfUni,"UniversidadesDF.csv",row.names = FALSE)

#3. Leer archivo csv generado UniversidadesDF.csv es el csv, que tiene las universidades tratadas,
#eliminando las incongruencias 
dfUni <- read.csv("UniversidadesDF.csv",header = T,sep = ",",encoding = "UTF-8")

#4. Left join con csv original, a través de los filtros, solo copia la columna 
DataF <- left_join(DataF,dfUni, by=c("institucion"="universidad")) ##Hacemos un left join para no perder ningun registro

#5 comprobar
table(DataF$equivalente)

#Exportar archivo csv con intituciones tratdas
DataF <- DataF[,!(names(DataF) %in% c("institucion"))] ##Quitamos la columna que transformamos
names(DataF)[length(names(DataF))] <- "institucion" ##Cambiamos nombre a institucion, que es la ultima columna de survey y lo obtenemos con lenght
write.csv(DataF,"tratamiento-institucion.csv",row.names = FALSE)


#cambiar a factor
#DataF$institucion <- as.factor(DataF$institucion)
#prop.table(table(DataF$equivalente))

#Analisis Descriptivo de instituciones despues de tratamiento
df_per_Institucion<-as.data.frame(prop.table(table(DataF$institucion))) %>% arrange(Freq)

boxplot(df_per_Institucion$Freq)

hist(df_per_Institucion$Freq)

qqnorm(df_per_Institucion$Freq)