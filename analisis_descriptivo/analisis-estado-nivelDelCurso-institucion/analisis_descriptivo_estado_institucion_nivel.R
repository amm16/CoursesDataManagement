setwd("~/");
setwd("Documents/Seminario/CoursesDataManagement/analisis_descriptivo/tratamiento-estado-nivelDelCurso")
getwd()
############## Libraries ##############
library(dplyr)
#######################################

#Leer archivo csv con el tratamiento de nulos 
coursesData <- read.csv("courses_data_cleaned_ranking_idh.csv",header = T,sep = ",", encoding = "UTF-8")


############### Analisis Descriptivo institucion ###############
df_per_Institucion<-as.data.frame(prop.table(table(coursesData$institucion))) %>% arrange(Freq)

boxplot(df_per_Institucion$Freq)

hist(df_per_Institucion$Freq)

qqnorm(df_per_Institucion$Freq)

###########################################################

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
df_per_Nivel[df_per_Nivel$Var1 %in%c("Basic 3","Basic 1","Basic 2","First Discoveries"),"categoriaNivelDelCurso"] <-"Basic"
df_per_Nivel[df_per_Nivel$Var1 %in%c("Intermediate 1","Intermediate 2","Intermediate 3"),"categoriaNivelDelCurso"] <-"Intermediate"

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
#Agrupar "No Show" y "Dropout"
df_per_Estado[df_per_Estado$Var1 %in%c("No Show","Dropout"),"categoria"] <-"Dropout"
df_per_Estado[df_per_Estado$Var1 %in%c("Fail"),"categoria"] <-"Fail"
df_per_Estado[df_per_Estado$Var1 %in%c("Pass"),"categoria"] <-"Pass"
df_per_Estado <- df_per_Estado %>% select(Var1,categoria)

#Unir la categoria a coursesData con un left join
coursesData <- left_join(coursesData,df_per_Estado,by=c("estado"="Var1"))

#Eliminar la columna de estado
coursesData <- coursesData[,!(names(coursesData) %in% c("estado"))]

#Cambiar el nombre de categoria a estado

names(coursesData)[length(names(coursesData))] <- "estado"
#Hacer nuevamente el analisis de los cambios
df_per_Estado <-as.data.frame(prop.table(table(coursesData$estado))) %>% arrange(Freq)

boxplot(df_per_Estado$Freq)

hist(df_per_Estado$Freq)

qqnorm(df_per_Estado$Freq)




write.csv(coursesData,"analisis_descriptivo_estado_institucion_nivelDelCurso.csv", row.names = FALSE)
