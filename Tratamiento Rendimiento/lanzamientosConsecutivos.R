#setwd("C:/Users/Asus ROG i7/Desktop/UNAH/Seminario/Repo Seminario/CoursesDataManagement")

validarRendimiento <- function(registro, datos, output) {
  
  infoPersona <- datos[which(datos$correo == registro[4]),]
  lanzamientosRegistrados <- data.frame(lanzamientos = infoPersona$lanzamiento, año = infoPersona$aÃ.o, notaFinal = infoPersona$notaFinal)
  
  mediaNotas = mean(lanzamientosRegistrados$notaFinal)

  if (mediaNotas >= 85) {
    resultado = c('rendimiento alto')
    
  } else if (mediaNotas >= 70){
    resultado = c('rendimiento medio')
    
  } else {
    resultado = c('rendimiento bajo')
    
  }

  cat(paste(registro[4], resultado, sep=","), file=output, append = T, fill = T)
}


validarOrden <- function(registro, datos, output) {
  # El orden de los lanzamientos es:
  # Winter -> Spring -> Summer -> Fall
  # Excepto en 2019 que no hubo lanzamiento Summer
  
  infoPersona <- datos[which(datos$correo == registro[4]),]
  lanzamientosRegistrados <- data.frame(lanzamientos = infoPersona$lanzamiento, año = infoPersona$aÃ.o)
  
  
  if (c('2018') %in% lanzamientosRegistrados$año) {
    if (c('Winter') %in% lanzamientosRegistrados$lanzamientos) {
      menor <- data.frame(lanzamiento = c('Winter'), año = c('2018'))
    } else if (c('Spring') %in% lanzamientosRegistrados$lanzamientos) {
      menor <- data.frame(lanzamiento = c('Spring'), año = c('2018'))
    } else if (c('Summer') %in% lanzamientosRegistrados$lanzamientos) {
      menor <- data.frame(lanzamiento = c('Summer'), año = c('2018'))
    } else if (c('Fall') %in% lanzamientosRegistrados$lanzamientos) {
      menor <- data.frame(lanzamiento = c('Fall'), año = c('2018'))
    }
  }else {
    if (c('Winter') %in% lanzamientosRegistrados$lanzamientos) {
      menor <- data.frame(lanzamiento = c('Winter'), año = c('2019'))
    } else if (c('Spring') %in% lanzamientosRegistrados$lanzamientos) {
      menor <- data.frame(lanzamiento = c('Spring'), año = c('2019'))
    } else if (c('Fall') %in% lanzamientosRegistrados$lanzamientos) {
      menor <- data.frame(lanzamiento = c('Fall'), año = c('2019'))
    }
  }
  
  resultado = FALSE
  count = 1
  año = as.character(menor$año)
  siguiente = as.character(menor$lanzamiento) #La variable siguiente se usará para tener el lanzamiento inicial y luego validar el que le sigue
  orden = paste(siguiente, año, sep="")
  
  # Mientras no se obtenga un resultado
  while (resultado == FALSE) {
    #Se evalua qué año es el menor
    if (año == c('2018')) {
      #Si el lanzamiento que se esta evaluando es "fall" entonces se llego al final del año y se continua evaluando el otro
      if (siguiente == c('fall')) {
        año = c('2019')
      }
      
      #Se obtiene cual es el siguiente lanzamiento a ser evaluado
      siguiente <- switch(siguiente, "Winter" = "Spring", "Spring" = "Summer", "Summer" = "Fall", "Fall" = "Winter")
      
      #Si el lanzamiento siguiente no aparece entre los registrados, se termina el ciclo
      if (!(siguiente %in% lanzamientosRegistrados$lanzamientos)) {
        resultado = c('Faltante')
        break
      }
      #Si el lanzamiento estaba entre los registrados el ciclo continua
      count = count + 1
      orden = paste(orden, paste(siguiente, año, sep=""), sep="-")
      
      #Si tiene al menos 3 lanzamientos consecutivos se le considera un estudiante recurrente
      if (count >= 3) {
        resultado = c('Recurrente')
        break
      }
    } else {
      
      #Si se llego al final de los lanzamientos se cataloga al estudiante como "recurrente"
      if (siguiente == c('next')) {
        resultado = c('Recurrente')
        break
      }
      
      #Se obtiene cual es el siguiente lanzamiento a ser evaluado
      siguiente <- switch(siguiente, "Winter" = "Spring", "Spring" = "Fall", "Fall" = "next")
      
      #Si el lanzamiento siguiente no aparece entre los registrados, se termina el ciclo
      if (!(siguiente %in% lanzamientosRegistrados$lanzamientos)) {
        resultado = c('Faltante')
        break
      }
      
      #Si el lanzamiento estaba entre los registrados el ciclo continua
      count = count + 1
      orden = paste(orden, paste(siguiente, año, sep=""), sep="-")
      
      #Si tiene al menos 3 lanzamientos consecutivos se le considera un estudiante recurrente
      if (count >= 3) {
        resultado = c('Recurrente')
        break
      }
    }
  }
  
  
  cat(paste(registro[4], resultado, orden, sep=","), file=output, append = T, fill = T)
}

# Obteniendo el CSV
coursesData <- read.csv('analisis_descriptivo/courses_data_analisis_categorias_nivel_estado.csv')

# Lanzamientos Consecutivos
archivoResultante1 = 'Tratamiento Rendimiento/estudiantes-consecutivos.csv'
cat(paste(c('correo'), c('resultado'), c('lanzamientos'), sep=","), file=archivoResultante1, append = T, fill = T)
apply(coursesData, 1, validarOrden, datos = coursesData, output = archivoResultante1)

# Clasificacion de las notas finales
archivoResultante2 = 'Tratamiento Rendimiento/rendimiento-estudiantes.csv'
cat(paste(c('correo'), c('resultado'), sep=","), file=archivoResultante2, append = T, fill = T)
apply(coursesData, 1, validarRendimiento, datos = coursesData, output = archivoResultante2)

#Buscar la informacion de un correo en especifico
infoPersona <- coursesData[which(coursesData$correo == c('CIBERTECMO@5130MOcibertec.pe')),]
lanzamientosRegistrados <- data.frame(lanzamientos = infoPersona$lanzamiento, año = infoPersona$aÃ.o, notaFinal = infoPersona$notaFinal)
median(lanzamientosRegistrados$notaFinal)
print(lanzamientosRegistrados)

