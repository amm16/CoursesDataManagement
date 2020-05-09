

############## Libraries ##############
library(dplyr)
library(ggplot2)
#######################################

#Leer archivo csv generado despues de analisis descriptivo  
coursesData <- read.csv("analisis_descriptivo_estado_institucion_nivelDelCurso.csv",header = T,sep = ",", encoding = "UTF-8");


#Tabla de proporcioes de mas de una variable (dos es lo recomendable)

table_estado_nivel <- table(coursesData$estado,coursesData$nivelDelCurso)

#Tabla en proporciones
 prop.table(table_estado_nivel,2)

#GGplot
ggplot(coursesData) +
  aes(x = estado, fill = factor(nivelDelCurso)) +
  geom_bar(position = "fill") +
  theme(axis.text.x = element_text(angle = 45))

#H_0: Las categorias de estado y nivelDelCurso son independientes.
#H_A: Las categorias de estado y nivelDelCurso son dependientes.
#Regla: Aceptamos nuestra hipotesis nula cuando nuestro p-value de nuestra prueba chis.test es menor es menor a 0.05
chisq.test(table_estado_nivel)
# El valor de nuestro p-value es 2.2e-16
# Conclusion: Segun nuestro p-value, aceptamos nuestra hipotesis nula, por lo tanto las variables son independientes.