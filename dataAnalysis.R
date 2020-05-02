#Libraries
library(dplyr)
#Asignar espacio de trabajo
setwd("/");
setwd("home/am/Documents/Seminario/CoursesDataManagement");
getwd();

#Leer archivo csv de la data limpia
coursesData <- read.csv("courses_data_cleaned.csv",header = T,sep = ",", encoding = "UTF-8")

#Analizar categorias nivel del curso

#proporciones de Nivel de Cursos
prop.table(table(coursesData$nivelDelCurso))

#Convertir las proporciones en dataframe
df_per <-as.data.frame(prop.table(table(coursesData$nivelDelCurso)))

#Operador pipe  %>% se ocupa libreria dplyr
#Ordenar por frecuencia
df_perc <- df_per %>% arrange(Freq)

#Boxplot de frecuencia/ Caja de Bigotes para identificar valores atipicos y luego excluirlos
#Mandar como parametro un vector numero continuo
boxplot(df_per$Freq)

#Histogramas - densidad de los valores
hist(df_per$Freq)

#Verificar si la distribucion es normal QQ-plot - si se mira una diagonal es distribucion normal
qqnorm(df_per$Freq)
#Agrupar niveles en categorias
df_per[df_per$Var1 %in%c("Advanced 1","Advanced 2","Advanced 3"),"categoriaNivelDelCurso"] <-"Advanced"
df_per[df_per$Var1 %in%c("Basic 1","Basic 2","Basic 3","First Discoveries"),"categoriaNivelDelCurso"] <-"Basic"
df_per[df_per$Var1 %in%c("Intermediate 1","Intermediate 2","Intermediate 3","English at Work 3"),"categoriaNivelDelCurso"] <-"Intermediate"

#Asignar nuevamente el dataframe pero solo seleccionando Var1 y categoriaNivelDelCurso
df_per <- df_per %>% select(Var1,categoriaNivelDelCurso)

#Unir las categorias a coursesData con un left join donde nivelDelCurso sea igual a Var1
coursesData <- left_join(coursesData,df_per,by=c("nivelDelCurso"="Var1"))

#Eliminar la columna de nivelDelCurso
coursesData <- coursesData[,!(names(coursesData) %in% c("nivelDelCurso"))]

#Cambiar el nombre de categoriaNivelDelCurso a nivelDelCurso

#length(names(coursesData)) -> Devuelve la longitud del vector

names(coursesData)[length(names(coursesData))] <- "nivelDelCurso"

#Hacer nuevamente los analisis con los nuevos cambios

df_per <-as.data.frame(prop.table(table(coursesData$nivelDelCurso)))

df_perc <- df_per %>% arrange(Freq)

boxplot(df_per$Freq)

hist(df_per$Freq)

qqnorm(df_per$Freq) #Ahora la data esta mejor distribuida




