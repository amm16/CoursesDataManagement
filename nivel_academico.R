setwd("../Ejemplos R/Ejemplos-seminario/")
setwd("Users/Asus ROG i7/Desktop/UNAH/Seminario/Data/")

LLO <- read.csv('../../Repo Seminario/CoursesDataManagement/CSV_DATASET.csv', header = TRUE, sep = ',')
data_regiones <- read.csv('../../Repo Seminario/CoursesDataManagement/universidades-tratadas.csv', header = TRUE, sep = ',')

nombres <- names(LLO)
names(LLO) <- nombres[!(nombres %in% c("Language"))]
names(LLO) <- names(LLO)[!(names(LLO) %in% c("Level"))]

summary(data_regiones)
summary(LLO)

nivel_academico <- data_regiones$indice.IDH > promedio && data_regiones$ranking > mediana => '<nivel de desarrollo>'
nivel_academico <- data_regiones$indice.IDH < promedio && data_regiones$ranking > mediana => '<nivel de desarrollo>'

nivel_academico <- data_regiones$indice.IDH > promedio && data_regiones$ranking < mediana => '<nivel de desarrollo>'
nivel_academico <- data_regiones$indice.IDH < promedio && data_regiones$ranking < mediana => '<nivel de desarrollo>'