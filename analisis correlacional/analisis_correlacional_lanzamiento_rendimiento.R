library(dplyr)
library(ggplot2)
library(corrplot)
library(factoextra)

coursesData <- read.csv('Tratamiento Rendimiento/dataset-asistencia-lanzamientos.csv')


# Se comprueba Normalidad
shapiro.test(coursesData$notaFinal)

# H_0: Nuestra distribucion es normal
# H_1: Nuestra distribucion NO es normal

# Conclusion:
  # Como p-value < 2.2e-16, menor a 0.05, podemos rechazar la hipotesis nula de que nuestros datos tienen una distribucion normal

# Para obtener las categorias
summary(coursesData$asistencia)

# Se hace un data frame por cada categoria de asistencia
grupo_abandono <- coursesData %>% filter(asistencia == 'abandono') %>% select(notaFinal)
grupo_buena <- coursesData %>% filter(asistencia == 'buena') %>% select(notaFinal)
grupo_faltante <- coursesData %>% filter(asistencia == 'faltante') %>% select(notaFinal)
grupo_sobresaliente <- coursesData %>% filter(asistencia == 'sobresaliente') %>% select(notaFinal)

# Grupo Abandono
boxplot(grupo_abandono$notaFinal)
qqnorm(grupo_abandono$notaFinal)
qqline(grupo_abandono$notaFinal)

shapiro.test(grupo_abandono$notaFinal)

# H_0: Nuestra distribucion es normal
# H_1: Nuestra distribucion NO es normal

# Conclusion:
# Como p-value < 2.2e-16, menor a 0.05, podemos rechazar la hipotesis nula de que nuestros datos tienen una distribucion normal

# Grupo Buena
boxplot(grupo_buena$notaFinal)
qqnorm(grupo_buena$notaFinal)
qqline(grupo_buena$notaFinal)

shapiro.test(grupo_buena$notaFinal)

# H_0: Nuestra distribucion es normal
# H_1: Nuestra distribucion NO es normal

# Conclusion:
# Como p-value < 4.301e-13, menor a 0.05, podemos rechazar la hipotesis nula de que nuestros datos tienen una distribucion normal

# Grupo Faltante
boxplot(grupo_faltante$notaFinal)
qqnorm(grupo_faltante$notaFinal)
qqline(grupo_faltante$notaFinal)

shapiro.test(grupo_faltante$notaFinal)

# H_0: Nuestra distribucion es normal
# H_1: Nuestra distribucion NO es normal

# Conclusion:
# Como p-value < 2.2e-16, menor a 0.05, podemos rechazar la hipotesis nula de que nuestros datos tienen una distribucion normal

# Grupo Sobresaliente
boxplot(grupo_sobresaliente$notaFinal)
qqnorm(grupo_sobresaliente$notaFinal)
qqline(grupo_sobresaliente$notaFinal)

shapiro.test(grupo_sobresaliente$notaFinal)

# H_0: Nuestra distribucion es normal
# H_1: Nuestra distribucion NO es normal

# Conclusion:
# Como p-value < 3e-14, menor a 0.05, podemos rechazar la hipotesis nula de que nuestros datos tienen una distribucion normal


# Prueba de Homocedasticidad

var.test(grupo_abandono$notaFinal, grupo_buena$notaFinal, grupo_faltante$notaFinal, grupo_sobresaliente$notaFinal)

# H_0: Tenemos homogeneidad de varianzas
# H_1: No tenemos homogeneidad de varianzas

# Interpretacion:
  # Con p-value , mayor a 0.05, no podemos rechazar la hipotesis nula. Por lo tanto suponemos homogeneidad de varianzas

t.test(grupo_abandono$notaFinal, grupo_buena$notaFinal, grupo_faltante$notaFinal, grupo_sobresaliente$notaFinal, # Cuatro muestras
       alternative = "two.sided", # contraste bilateral
       paired = FALSE, # muestras independientes
       var.equal = TRUE # se supone homocedasticidad
)