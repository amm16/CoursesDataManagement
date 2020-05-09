library(dplyr)
library(ggplot2)

coursesData <- read.csv('Tratamiento Rendimiento/dataset-asistencia-lanzamientos.csv')

prop.table(table(coursesData$lanzamientosMatriculados, coursesData$nivelDelCurso))