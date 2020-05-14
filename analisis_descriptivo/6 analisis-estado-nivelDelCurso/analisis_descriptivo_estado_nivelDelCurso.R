setwd("~/");
setwd("Documents/Seminario/CoursesDataManagement/analisis_descriptivo/6 analisis-estado-nivelDelCurso")
getwd()
############## Libraries ##############
library(dplyr)
#######################################

#Leer archivo csv despues de haber eliminado correo e insittucion
coursesData <- read.csv("courses_data_cleaned_version_2",header = T,sep = ",", encoding = "UTF-8")


############### Analisis Descriptivo nivelDelCurso ###############

#proporciones de Nivel de Cursos
prop.table(table(coursesData$nivelDelCurso))

#Operador pipe  %>% se ocupa libreria dplyr
#Convertir las proporciones en dataframe
df_per_Nivel <-as.data.frame(prop.table(table(coursesData$nivelDelCurso))) %>% arrange(Freq)

#Boxplot de frecuencia/ Caja de Bigotes para identificar valores atipicos y luego excluirlos
#Mandar como parametro un vector numero continuo
boxplot(df_per_Nivel$Freq)

#Histogramas - densidad de los valores
hist(df_per_Nivel$Freq)

#Verificar si la distribucion es normal QQ-plot - si se mira una diagonal es distribucion normal
qqnorm(df_per_Nivel$Freq)
#Agrupar niveles en categorias
df_per_Nivel[df_per_Nivel$Var1 %in%c("Advanced 1"),"categoriaNivelDelCurso"] <-"Advanced"
df_per_Nivel[df_per_Nivel$Var1 %in%c("Advanced 2","Advanced 3","English at Work 3"),"categoriaNivelDelCurso"] <-"Very Advanced"
df_per_Nivel[df_per_Nivel$Var1 %in%c("Basic 3","Basic 1","Basic 2","Basic 2 ","First Discoveries","First Discoveries "),"categoriaNivelDelCurso"] <-"Basic"
df_per_Nivel[df_per_Nivel$Var1 %in%c("Intermediate 1","Intermediate 1 ","Intermediate 2","Intermediate 3"),"categoriaNivelDelCurso"] <-"Intermediate"

#Asignar nuevamente el dataframe pero solo seleccionando Var1 y categoriaNivelDelCurso
df_per_Nivel <- df_per_Nivel %>% select(Var1,categoriaNivelDelCurso)

#Unir las categorias a coursesData con un left join donde nivelDelCurso sea igual a Var1
coursesData <- left_join(coursesData,df_per_Nivel,by=c("nivelDelCurso"="Var1"))

#Eliminar la columna de nivelDelCurso
coursesData <- coursesData[,!(names(coursesData) %in% c("nivelDelCurso"))]

#Cambiar el nombre de categoriaNivelDelCurso a nivelDelCurso

#length(names(coursesData)) -> Devuelve la longitud del vector

names(coursesData)[length(names(coursesData))] <- "nivelDelCurso"

#Hacer nuevamente los analisis con los nuevos cambios

df_per_Nivel <-as.data.frame(prop.table(table(coursesData$nivelDelCurso))) %>% arrange(Freq)

boxplot(df_per_Nivel$Freq)

hist(df_per_Nivel$Freq)

qqnorm(df_per_Nivel$Freq) #Ahora la data esta mejor distribuida

###########################################################


############### Analisis Descriptivo estado ###############

#Convertir las proporciones en dataframe
df_per_Estado <-as.data.frame(prop.table(table(coursesData$estado)))%>% arrange(Freq)

boxplot(df_per_Estado$Freq)

hist(df_per_Estado$Freq)

qqnorm(df_per_Estado$Freq)

write.csv(coursesData,"courses_data_cleaned_version_3.csv", row.names = FALSE)
