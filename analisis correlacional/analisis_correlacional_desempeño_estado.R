library(dplyr)
library(ggplot2)

datasetDesempeño <- read.csv('analisis_descriptivo/6 analisis-estado-nivelDelCurso/courses_data_cleaned_version_3.csv', sep=",", header = TRUE)

datasetDesempeño$desertados <- "No"

datasetDesempeño[ datasetDesempeño$estado == "Dropout", "desertados" ] <- "Si"
datasetDesempeño[ datasetDesempeño$estado == "No Show", "desertados" ] <- "Si"

prop.table(table(datasetDesempeño$desempeño, datasetDesempeño$desertados), 1)
prop.table(table(datasetDesempeño$desempeño, datasetDesempeño$estado), 1)

ggplot(datasetDesempeño) +
  aes(x = desempeño, fill = factor(desertados)) +
  geom_bar(position = "fill") +
  labs(x="Categorías Desempeño", y = "Porcentajes")

chisq.test(table(datasetDesempeño$desempeño, datasetDesempeño$desertados))

#H_0: Las categorias desempeño y estado son independientes
#H_1: Las categorias desempeño y estado son dependientes

#Conclusion:
# Con p-value < 2.2e-16, menor que 0.05, no rechazamos nuestra hipotesis nula, por lo tanto decimos que nuestras variables categoricas son independientes