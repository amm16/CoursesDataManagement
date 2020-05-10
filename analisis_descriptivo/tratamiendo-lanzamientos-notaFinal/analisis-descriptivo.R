# Analisis Descriptivo
lanzamientosConsecutivos <- read.csv('Tratamiento Rendimiento/lanzamientos-consecutivos.csv')
notasCategorizadas <- read.csv('Tratamiento Rendimiento/notaFinal-categorizada.csv')
str(lanzamientosConsecutivos)
str(notasCategorizadas)

#Libraries
library(dplyr)

#Analisis Nota Final
df_per <-as.data.frame(prop.table(table(notasCategorizadas$rendimiento)))
#Ordenar por frecuencia
df_per <- df_per %>% arrange(Freq)

#Boxplot de frecuencia/ Caja de Bigotes para identificar valores atipicos y luego excluirlos
#Mandar como parametro un vector numero continuo
boxplot(df_per$Freq)

#Histogramas - densidad de los valores
hist(df_per$Freq)

#Verificar si la distribucion es normal QQ-plot - si se mira una diagonal es distribucion normal
qqnorm(df_per$Freq)



#Analisis Asistencia Lanzamientos 
tableAsistencia <- table(lanzamientosConsecutivos$asistencia)
propAsistencia <- prop.table(tableAsistencia)

df_per <-as.data.frame(propAsistencia)
#Ordenar por frecuencia
df_per <- df_per %>% arrange(Freq)

#Boxplot de frecuencia/ Caja de Bigotes para identificar valores atipicos y luego excluirlos
#Mandar como parametro un vector numero continuo
boxplot(df_per$Freq)

#Histogramas - densidad de los valores
hist(df_per$Freq)

#Verificar si la distribucion es normal QQ-plot - si se mira una diagonal es distribucion normal
qqnorm(df_per$Freq)
qqline(df_per$Freq)

