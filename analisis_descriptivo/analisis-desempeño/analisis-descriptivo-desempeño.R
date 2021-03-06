#Libraries
library(dplyr)

datasetDesempeņo <- read.csv('analisis_descriptivo/4 tratamiento-Desempeņo/courses_data_cleaned_desempeņo.csv')

summary(datasetDesempeņo$desempeņo)

#Analisis Desempeņo
tableDesempeņo <- table(datasetDesempeņo$desempeņo)
propDesempeņo <- prop.table(tableDesempeņo)

df_per_Desempeņo <-as.data.frame(propDesempeņo)
#Ordenar por frecuencia
df_per_Desempeņo <- df_per_Desempeņo %>% arrange(Freq)

#Boxplot de frecuencia/ Caja de Bigotes para identificar valores atipicos y luego excluirlos
#Mandar como parametro un vector numero continuo
boxplot(df_per_Desempeņo$Freq)

#Histogramas - densidad de los valores
hist(df_per_Desempeņo$Freq)

#Verificar si la distribucion es normal QQ-plot - si se mira una diagonal es distribucion normal
qqnorm(df_per_Desempeņo$Freq)
qqline(df_per_Desempeņo$Freq)

