#setwd("C:/Users/Asus ROG i7/Desktop/UNAH/Seminario/Repo Seminario/CoursesDataManagement")
### Para ejecutar correctamente este archivo:
# 1) Seleccionar las funciones y ejecutarlas para que se guarden en cache
# 2) Ejecutar el codigo principal de cada seccion para generar los archivos necesarios


# Obteniendo el CSV base
coursesData <- read.csv('analisis_descriptivo/3 tratamiento-NivelEducativo/courses_data_cleaned_ranking_idh.csv')


# Validacion de los Lanzamientos Consecutivos
archivoResultante1 = 'analisis_descriptivo/4 tratamiento-Desempe�o/lanzamientos-consecutivos.csv'
cat(paste(c('correo'), c('asistencia'), sep=","), file=archivoResultante1, append = T, fill = T)
apply(coursesData, 1, validarLanzamientos, datos = coursesData, output = archivoResultante1)

# Clasificacion de las notas finales
archivoResultante2 = 'analisis_descriptivo/4 tratamiento-Desempe�o/notaFinal-categorizada.csv'
cat(paste(c('correo'), c('rendimiento'), c('mediaNotas'), sep=","), file=archivoResultante2, append = T, fill = T)
apply(coursesData, 1, validarNotaFinal, datos = coursesData, output = archivoResultante2)

library(dplyr)

lanzamientosConsecutivos <- read.csv('analisis_descriptivo/4 tratamiento-Desempe�o/lanzamientos-consecutivos.csv')
notasCategorizadas <- read.csv('analisis_descriptivo/4 tratamiento-Desempe�o/notaFinal-categorizada.csv')

# Se eliminan los registros repetidos de correos (ya que todos mantienen la misma informacion)
lanzamientosConsecutivos <- lanzamientosConsecutivos%>% distinct(correo, .keep_all = TRUE)
lanzamientosConsecutivos <- lanzamientosConsecutivos%>% select(correo, asistencia)

# Se a�ade la nueva columna al csv principal
coursesData <- left_join(coursesData, lanzamientosConsecutivos, by=c("correo"="correo"))

# Se eliminan los registros repetidos de correos (ya que todos mantienen la misma informacion)
notasCategorizadas <- notasCategorizadas%>% distinct(correo, .keep_all = TRUE)
notasCategorizadas <- notasCategorizadas%>% select(correo, rendimiento, mediaNotas)

# Se a�ade la nueva columna al csv principal
coursesData <- left_join(coursesData, notasCategorizadas, by=c("correo"="correo"))

write.csv(coursesData, "analisis_descriptivo/4 tratamiento-Desempe�o/dataset-asistencia-rendimiento.csv", row.names = FALSE)

# Clasificacion del desempe�o
datasetAsistenciaRendimiento <- read.csv('analisis_descriptivo/4 tratamiento-Desempe�o/dataset-asistencia-rendimiento.csv')

archivoResultante3 = 'analisis_descriptivo/4 tratamiento-Desempe�o/desempe�o.csv'
cat(paste(c('correo'), c('desempe�o'), sep=","), file=archivoResultante3, append = T, fill = T)
apply(datasetAsistenciaRendimiento, 1, validarRendimiento, datos = datasetAsistenciaRendimiento, output = archivoResultante3)

desempe�o <- read.csv('analisis_descriptivo/4 tratamiento-Desempe�o/desempe�o.csv')
summary(desempe�o$desempe�o)
# Se eliminan los registros repetidos de correos (ya que todos mantienen la misma informacion)
desempe�o <- desempe�o%>% distinct(correo, .keep_all = TRUE)
desempe�o <- desempe�o%>% select(correo, desempe�o)

# Se a�ade la nueva columna al csv principal
datasetAsistenciaRendimiento <- left_join(datasetAsistenciaRendimiento, desempe�o, by=c("correo"="correo"))

str(datasetAsistenciaRendimiento)

write.csv(datasetAsistenciaRendimiento, "analisis_descriptivo/4 tratamiento-Desempe�o/courses_data_cleaned_desempe�o.csv", row.names = FALSE)


validarLanzamientos <- function(registro, datos, output) {
  # El orden de los lanzamientos es:
  # Winter -> Spring -> Summer -> Fall
  # Excepto en 2019 que no hubo lanzamiento Summer
  
  infoPersona <- datos[which(datos$correo == registro[3]),]
  lanzamientosRegistrados <- data.frame(lanzamientos = infoPersona$lanzamiento, a�o = infoPersona$a.f1.o)
  
  
  if (c('2018') %in% lanzamientosRegistrados$a�o) {
    if (c('Winter') %in% lanzamientosRegistrados$lanzamientos) {
      menor <- data.frame(lanzamiento = c('Winter'), a�o = c('2018'))
    } else if (c('Spring') %in% lanzamientosRegistrados$lanzamientos) {
      menor <- data.frame(lanzamiento = c('Spring'), a�o = c('2018'))
    } else if (c('Summer') %in% lanzamientosRegistrados$lanzamientos) {
      menor <- data.frame(lanzamiento = c('Summer'), a�o = c('2018'))
    } else if (c('Fall') %in% lanzamientosRegistrados$lanzamientos) {
      menor <- data.frame(lanzamiento = c('Fall'), a�o = c('2018'))
    }
  }else {
    if (c('Winter') %in% lanzamientosRegistrados$lanzamientos) {
      menor <- data.frame(lanzamiento = c('Winter'), a�o = c('2019'))
    } else if (c('Spring') %in% lanzamientosRegistrados$lanzamientos) {
      menor <- data.frame(lanzamiento = c('Spring'), a�o = c('2019'))
    } else if (c('Fall') %in% lanzamientosRegistrados$lanzamientos) {
      menor <- data.frame(lanzamiento = c('Fall'), a�o = c('2019'))
    }
  }
  
  resultado = FALSE
  count = 1
  a�o = as.character(menor$a�o)
  siguiente = as.character(menor$lanzamiento) #La variable siguiente se usar� para tener el lanzamiento inicial y luego validar el que le sigue
  orden = paste(siguiente, a�o, sep="")
  
  # Mientras no se obtenga un resultado
  while (resultado == FALSE) {
    #Se evalua qu� a�o es el menor
    if (a�o == c('2018')) {
      #Si el lanzamiento que se esta evaluando es "fall" entonces se llego al final del a�o y se continua evaluando el otro
      if (siguiente == c('Fall')) {
        a�o = c('2019')
      }
      
      #Se obtiene cual es el siguiente lanzamiento a ser evaluado
      siguiente <- switch(siguiente, "Winter" = "Spring", "Spring" = "Summer", "Summer" = "Fall", "Fall" = "Winter")
      
      #Si el lanzamiento siguiente no aparece entre los registrados, se termina el ciclo
      if (!(siguiente %in% lanzamientosRegistrados$lanzamientos)) {
        resultado = 'porver'
        break
      }
      #Si el lanzamiento estaba entre los registrados el ciclo continua
      count = count + 1
      orden = paste(orden, paste(siguiente, a�o, sep=""), sep="-")
      
    } else {
      
      #Si se llego al final de los lanzamientos se cataloga al estudiante como "recurrente"
      if (siguiente == c('next')) {
        resultado = c('sobresaliente')
        break
      }
      
      #Se obtiene cual es el siguiente lanzamiento a ser evaluado
      siguiente <- switch(siguiente, "Winter" = "Spring", "Spring" = "Fall", "Fall" = "next")
      
      #Si el lanzamiento siguiente no aparece entre los registrados, se termina el ciclo
      if (!(siguiente %in% lanzamientosRegistrados$lanzamientos)) {
        resultado = 'porver'
        break
      }
      
      #Si el lanzamiento estaba entre los registrados el ciclo continua
      count = count + 1
      orden = paste(orden, paste(siguiente, a�o, sep=""), sep="-")
    }
  }
  
  if (resultado == 'porver') {
    #Si tiene al menos 3 lanzamientos consecutivos se le considera un estudiante recurrente
    if (count >= 5) {
      resultado = c('sobresaliente')
      
    } else if (count >= 4) {
      resultado = c('muy buena')
      
    } else if (count >= 3) {
      resultado = c('buena')
      
    } else if (count >= 2) {
      resultado = c('faltante')
      
    } else {
      resultado = c('abandono')
      
    }
  }
  
  cat(paste(registro[3], resultado, sep=","), file=output, append = T, fill = T)
}

validarNotaFinal <- function(registro, datos, output) {
  
  infoPersona <- datos[which(datos$correo == registro[3]),]
  lanzamientosRegistrados <- data.frame(lanzamientos = infoPersona$lanzamiento, a�o = infoPersona$a.f1.o, notaFinal = infoPersona$notaFinal)
  
  mediaNotas = mean(lanzamientosRegistrados$notaFinal)
  
  if (mediaNotas >= 90) {
    resultado = 'sobresaliente'
    
  } else if (mediaNotas >= 80){
    resultado = 'muy bueno'
    
  } else if (mediaNotas >= 70) {
    resultado = 'bueno'
    
  } else if (mediaNotas >= 60) {
    resultado = 'necesita mejorar'
    
  } else {
    resultado = 'reprobado'
  }
  
  cat(paste(registro[3], resultado, mediaNotas, sep=","), file=output, append = T, fill = T)
}

validarRendimiento <- function(registro, datos, output){
  
  infoPersona <- datos[which(datos$correo == registro[3]),]
  asistencia_rendimiento <- data.frame(asistencia = infoPersona$asistencia, rendimiento = infoPersona$rendimiento)
  
  if (c('sobresaliente') %in% asistencia_rendimiento$asistencia) {
    if (c('sobresaliente') %in% asistencia_rendimiento$rendimiento) {
      resultado <- 'excelente'
    } else if (c('muy bueno') %in% asistencia_rendimiento$rendimiento) {
      resultado <- 'excelente'
    } else if (c('bueno') %in% asistencia_rendimiento$rendimiento) {
      resultado <- 'excelente'
    } else if (c('necesita mejorar') %in% asistencia_rendimiento$rendimiento) {
      resultado <- 'muy buen'
    } else if (c('reprobado') %in% asistencia_rendimiento$rendimiento) {
      resultado <- 'buen'
    }
  } else if (c('muy buena') %in% asistencia_rendimiento$asistencia) {
    if (c('sobresaliente') %in% asistencia_rendimiento$rendimiento) {
      resultado <- 'excelente'
    } else if (c('muy bueno') %in% asistencia_rendimiento$rendimiento) {
      resultado <- 'excelente'
    } else if (c('bueno') %in% asistencia_rendimiento$rendimiento) {
      resultado <- 'muy buen'
    } else if (c('necesita mejorar') %in% asistencia_rendimiento$rendimiento) {
      resultado <- 'muy buen'
    } else if (c('reprobado') %in% asistencia_rendimiento$rendimiento) {
      resultado <- 'buen'
    }
  } else if (c('buena') %in% asistencia_rendimiento$asistencia) {
    if (c('sobresaliente') %in% asistencia_rendimiento$rendimiento) {
      resultado <- 'excelente'
    } else if (c('muy bueno') %in% asistencia_rendimiento$rendimiento) {
      resultado <- 'muy buen'
    } else if (c('bueno') %in% asistencia_rendimiento$rendimiento) {
      resultado <- 'muy buen'
    } else if (c('necesita mejorar') %in% asistencia_rendimiento$rendimiento) {
      resultado <- 'buen'
    } else if (c('reprobado') %in% asistencia_rendimiento$rendimiento) {
      resultado <- 'mal'
    }
  } else if (c('faltante') %in% asistencia_rendimiento$asistencia) {
    if (c('sobresaliente') %in% asistencia_rendimiento$rendimiento) {
      resultado <- 'muy buen'
    } else if (c('muy bueno') %in% asistencia_rendimiento$rendimiento) {
      resultado <- 'muy buen'
    } else if (c('bueno') %in% asistencia_rendimiento$rendimiento) {
      resultado <- 'buen'
    } else if (c('necesita mejorar') %in% asistencia_rendimiento$rendimiento) {
      resultado <- 'buen'
    } else if (c('reprobado') %in% asistencia_rendimiento$rendimiento) {
      resultado <- 'mal'
    }
  } else if (c('abandono') %in% asistencia_rendimiento$asistencia) {
    if (c('sobresaliente') %in% asistencia_rendimiento$rendimiento) {
      resultado <- 'buen'
    } else if (c('muy bueno') %in% asistencia_rendimiento$rendimiento) {
      resultado <- 'buen'
    } else if (c('bueno') %in% asistencia_rendimiento$rendimiento) {
      resultado <- 'mal'
    } else if (c('necesita mejorar') %in% asistencia_rendimiento$rendimiento) {
      resultado <- 'mal'
    } else if (c('reprobado') %in% asistencia_rendimiento$rendimiento) {
      resultado <- 'mal'
    }
  }
  
  cat(paste(registro[3], resultado, sep=","), file=output, append = T, fill = T)
}

