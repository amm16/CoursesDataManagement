DataF <- read.csv("Tratamiento-nulos-main.csv",header = T,sep = ",",encoding = "UTF-8")

##--------------------------Tratamiento Universidades---------------------

prop.table(table(DataF$institucion))
dfUni <- as.data.frame(prop.table(table(DataF$institucion)))

#1. tomoar el nombre de todas las instituciones diferentes (47 niveles)
dfUni<- dfUni %>% select(Var1) 

#2. Escribir un csv solo con las universidades
write.csv(dfUni,"UniversidadesDF.csv",row.names = FALSE)

#3. Leer archivo csv generado con el nuevo tratamiento (universidad equivalente)
dfUni <- read.csv("UniversidadesDF.csv",header = T,sep = ",",encoding = "UTF-8")

#4. Left join con csv original
DataF <- left_join(DataF,dfUni, by=c("institucion"="universidad")) ##Hacemos un left join para no perder ningun registro

#5 comprobar
table(DataF$equivalente)

#Cambiar nombre de columna
DataF <- DataF[,!(names(DataF) %in% c("institucion"))] ##Quitamos la columna que transformamos
names(DataF)[length(names(DataF))] <- "institucion" ##Cambiamos nombre a institucion, que es la ultima columna de survey y lo obtenemos con lenght
write.csv(DataF,"courses_data_cleaned_institucion.csv",row.names = FALSE)
