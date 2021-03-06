library(dplyr)
library(ggplot2)

datasetDesempeņo <- read.csv('analisis_descriptivo/6 analisis-estado-nivelDelCurso/courses_data_cleaned_version_3.csv', sep=",", header = TRUE)

datasetDesempeņo$desertados <- "No"

datasetDesempeņo[ datasetDesempeņo$estado == "Dropout", "desertados" ] <- "Si"
datasetDesempeņo[ datasetDesempeņo$estado == "No Show", "desertados" ] <- "Si"

prop.table(table(datasetDesempeņo$desempeņo, datasetDesempeņo$desertados), 1)
prop.table(table(datasetDesempeņo$desempeņo, datasetDesempeņo$estado), 1)

ggplot(datasetDesempeņo) +
  aes(x = desempeņo, fill = factor(desertados)) +
  geom_bar(position = "fill") +
  labs(x="Categorías Desempeņo", y = "Porcentajes")

chisq.test(table(datasetDesempeņo$desempeņo, datasetDesempeņo$desertados))

#H_0: Las categorias desempeņo y estado son independientes
#H_1: Las categorias desempeņo y estado son dependientes

#Conclusion:
# Con p-value < 2.2e-16, menor que 0.05, no rechazamos nuestra hipotesis nula, por lo tanto decimos que nuestras variables categoricas son independientes