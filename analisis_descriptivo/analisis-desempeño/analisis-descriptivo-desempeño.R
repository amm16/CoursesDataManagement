#Libraries
library(dplyr)

datasetDesempeño <- read.csv('analisis_descriptivo/4 tratamiento-Desempeño/courses_data_cleaned_desempeño.csv')

summary(datasetDesempeño$desempeño)

#Analisis Desempeño
tableDesempeño <- table(datasetDesempeño$desempeño)
propDesempeño <- prop.table(tableDesempeño)

df_per_Desempeño <-as.data.frame(propDesempeño)
#Ordenar por frecuencia
df_per_Desempeño <- df_per_Desempeño %>% arrange(Freq)

#Boxplot de frecuencia/ Caja de Bigotes para identificar valores atipicos y luego excluirlos
#Mandar como parametro un vector numero continuo
boxplot(df_per_Desempeño$Freq)

#Histogramas - densidad de los valores
hist(df_per_Desempeño$Freq)

#Verificar si la distribucion es normal QQ-plot - si se mira una diagonal es distribucion normal
qqnorm(df_per_Desempeño$Freq)
qqline(df_per_Desempeño$Freq)

