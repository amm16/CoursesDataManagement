#Libraries
library(dplyr)

#Obteniendo el CSV
coursesData <- read.csv('Tratamiento Valores Nulos/Tratamiento-limpieza-datos.csv')

#Se obtiene el data.frame de los correos adjuntos con el a??o y los lanzamientos
correos <- data.frame(Correos = coursesData$correo, Anios = coursesData$a??o, Lanzamientos = coursesData$lanzamiento)

#Se hace un conteo de cuantas veces aparece el correo en los datos
conteo <- aggregate(correos$Correos, correos, length)
colnames(conteo)[4]<-"Repeticiones"

#El orden de los lanzamientos seran tratados:
#Spring -> Summer -> Fall -> Winter

#Se valida si el correo tiene menos de 2 repeticiones pero son
#distintas de Fall y Winter de 2019 (por ser los ultimos y no sabemos si continua en 2020)

conteo[(conteo$Anios == '2019') && ((conteo$Lanzamientos %in% c('Winter')) && (conteo$Lanzamientos%in% c('Fall'))) && (conteo$Repeticiones < 2), 'Consecutivo'] <- ''
conteo[(conteo$Anios == '2019') && ((conteo$Lanzamientos %in% c('Winter')) && (conteo$Lanzamientos%in% c('Fall'))) && (conteo$Repeticiones < 2), 'Consecutivo'] <- 'faltante'
conteo[(conteo$Anios != '2019') && (conteo$Repeticiones < 2), 'Consecutivo'] <- 'faltante'
conteo[(conteo$Anios != '2019') && (conteo$Repeticiones < 2), 'Consecutivo'] <- 'faltante'

conteo[conteo$Repeticiones >= 2, 'Consecutivo'] <- 'recurrente'


summary(as.factor(conteo$Consecutivo))
