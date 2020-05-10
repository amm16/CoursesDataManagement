#Libraries
library(dplyr)

datasetDesempeño <- read.csv('analisis_descriptivo/4 tratamiento-Desempeño/courses_data_cleaned_desempeño.csv')
summary(datasetDesempeño$desempeño)


#Analisis Desempeño
tableDesempeño <- table(datasetDesempeño$desempeño)
propDesempeño <- prop.table(tableDesempeño)

df_per <-as.data.frame(propDesempeño)
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

