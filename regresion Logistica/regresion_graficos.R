# Regresion Logistica
library(dplyr)
library(caret)
library(ggplot2)

dataset <- read.csv('analisis_descriptivo/4 tratamiento-Desempeño/courses_data_cleaned_desempeño.csv')


# Se crea la columna referente a nuestra variable de resultados
dataset$estudiante_desertor <- "0"

# Se coloca en 1 a los estudiantes que desertaron
dataset[ dataset$estado == "Dropout", "estudiante_desertor" ] <- 1
dataset[ dataset$estado == "No Show", "estudiante_desertor" ] <- 1

prop.table(table(dataset$estudiante_desertor))

# Variables de importancia para la regresion
features <- c('desempeño', 
              'nivelDelCurso', 
              'lanzamiento',
              'asistencia',
              # 'nivelDesarrolloEducativo',
              'estudiante_desertor',
            )

# Se crea un dataset con las columnas de importancia
set <- dataset[, names(dataset) %in% features] 
set$estudiante_desertor <- as.factor(set$estudiante_desertor)

# Regresion lineal
model <- glm(estudiante_desertor ~ ., data = set, family = "binomial")
# Se guardan los nombres de las nuevas variables de la regresion
importances <- varImp(model)
importances$col <- row.names(importances)
importances <- importances %>% arrange(-Overall)
importances

# Graficos con respecto a cada variable

# DESEMPEÑO
ggplot(set) + 
  aes(x = desempeño, fill = factor(estudiante_desertor)) +
  geom_bar(position = "fill") +
  scale_fill_manual(values = c("#999999", "#E69F00"))

# NIVEL DEL CURSO
ggplot(set) + 
  aes(x = nivelDelCurso, fill = factor(estudiante_desertor)) +
  geom_bar(position = "fill") +
  scale_fill_manual(values = c("#999999", "#E69F00"))

# LANZAMIENTO
ggplot(set) + 
  aes(x = lanzamiento, fill = factor(estudiante_desertor)) +
  geom_bar(position = "fill") +
  scale_fill_manual(values = c("#999999", "#E69F00"))

# ASISTENCIA
ggplot(set) + 
  aes(x = asistencia, fill = factor(estudiante_desertor)) +
  geom_bar(position = "fill") +
  scale_fill_manual(values = c("#999999", "#E69F00"))

