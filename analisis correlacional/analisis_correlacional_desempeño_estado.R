library(dplyr)
library(ggplot2)

datasetDesempeño <- read.csv('analisis_descriptivo/4 tratamiento-Desempeño/courses_data_cleaned_desempeño.csv', sep=",", header = TRUE)

prop.table(table(datasetDesempeño$desempeño, datasetDesempeño$estado), 1)

ggplot(datasetDesempeño) +
  aes(x = desempeño, fill = factor(estado)) +
  geom_bar(position = "fill") +
  theme(axis.text.x = element_text(angle = 45))

chisq.test(table(datasetDesempeño$desempeño, datasetDesempeño$estado))

#H_0: Las categorias desempeño y estado son independientes
#H_1: Las categorias desempeño y estado son dependientes

#Conclusion:
# Con p-value < 2.2e-16, menor que 0.05, no rechazamos nuestra hipotesis nula, por lo tanto decimos que nuestras variables categoricas son independientes