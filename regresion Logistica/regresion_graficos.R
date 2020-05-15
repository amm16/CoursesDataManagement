# Regresion Logistica
library(dplyr)
library(caret)
library(ggplot2)

dataset <- read.csv('analisis_descriptivo/6 analisis-estado-nivelDelCurso/courses_data_cleaned_version_3.csv')

# Variables de importancia para la regresion
features <- c('desempeño', 
              'nivelDelCurso',
              'nivelEducativo',
              'lanzamiento',
              'estado'
            )

# Se crea un dataset con las columnas de importancia
set <- dataset[, names(dataset) %in% features] 
set$estado <- as.factor(set$estado)
str(set)

# Regresion lineal
model <- glm(estado ~ ., data = set, family = "binomial")
# Se guardan los nombres de las nuevas variables de la regresion
importances <- varImp(model)
importances$col <- row.names(importances)
importances <- importances %>% arrange(-Overall)
importances

# Graficos con respecto a cada variable


# DESEMPEÑO
ggplot(set) + 
  aes(x = desempeño, fill = factor(estado)) +
  geom_bar(position = "fill") +
  labs(title = "Regresión logística", x="Categorías Desempeño", y = "Porcentajes") +
  scale_fill_manual(values = c("#999999", "#E69F00"))

# NIVEL DEL CURSO
ggplot(set) + 
  aes(x = nivelDelCurso, fill = factor(estado)) +
  geom_bar(position = "fill") +
  labs(title = "Regresión logística", x="Niveles de los Cursos", y = "Porcentajes") +
  scale_fill_manual(values = c("#999999", "#E69F00"))

# LANZAMIENTO
ggplot(set) + 
  aes(x = lanzamiento, fill = factor(estado)) +
  geom_bar(position = "fill") +
  labs(title = "Regresión logística", x="Lanzamientos", y = "Porcentajes") +
  scale_fill_manual(values = c("#999999", "#E69F00"))

# NIVEL EDUCATIVO
ggplot(set) + 
  aes(x = nivelEducativo, fill = factor(estado)) +
  geom_bar(position = "fill") +
  labs(title = "Regresión logística", x="Niveles Educativos", y = "Porcentajes") +
  scale_fill_manual(values = c("#999999", "#E69F00"))

