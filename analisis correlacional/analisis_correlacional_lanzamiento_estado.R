library(dplyr)
library(ggplot2)

setwd("/")
setwd("/Users/user/Seminario/")

survey <- read.csv("analisis_descriptivo_estado_institucion_nivelDelCurso.csv", sep = ",", header = T)

##sacar la proporcion entre lanzamiento y estado en funcion de fila
prop.table(table(survey$lanzamiento,survey$estado),2)

##sacar la proporcion entre lanzamiento y estado en funcion de columna
prop.table(table(survey$lanzamiento,survey$estado),1)

##Grafico
ggplot(survey) +
  aes(x = lanzamiento, fill = factor(estado)) +
  geom_bar(position = "stack") +
  theme(axis.text.x = element_text(angle = 45))

ggplot(survey) +
  aes(x = lanzamiento, fill = factor(estado)) +
  geom_bar(position = "fill") +
  theme(axis.text.x = element_text(angle = 45))

##Pearson's Chi-squared test p-value = 2.308e-12
chisq.test(table(survey$lanzamiento, survey$estado))

summary(survey)

##Hipotesis para las dos variables categoricas lanzamiento y estado 
##H_0: Las categorias de lanzamiento y estado son independientes. (conclusion en funcion de esta)
##H_A: Las categorias son dependientes.

##Conclusion: Segun nuestro p-value aceptamos nuestra hipotesis nula, por lo tanto las variables son independientes
  
