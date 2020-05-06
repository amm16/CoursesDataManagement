#setwd("C:/Users/Asus ROG i7/Desktop/UNAH/Seminario/Repo Seminario/CoursesDataManagement")

lanzamientoMasBajo <- function(registros) {
  
  
}

validarOrden <- function(registro, datos, output) {
  #El orden de los lanzamientos es:
  #Winter -> Spring -> Summer -> Fall
  # orden <- data.frame(Lanzamiento = c('Winter', 'Spring', 'Summer', 'Fall', 'Winter', 'Spring', 'Fall'), 
  #                     Año = c('2018', '2018', '2018', '2018', '2019', '2019', '2019'))
  

  infoPersona <- datos[which(datos$correo == registro[4]),]
  lanzamientosRegistrados <- data.frame(lanzamientos = infoPersona$lanzamiento, año = infoPersona$aÃ.o)
  
  lanzamientoMasBajo()
  
  print(lanzamientosRegistrados)
  
  
  # print(paste(year, launch, email, sep=","))
  # cat(paste(year, launch, email, sep=","), file=output, append = T, fill = T)
}

#Obteniendo el CSV
coursesData <- read.csv('analisis_descriptivo/courses_data_analisis_categorias_nivel_estado.csv')

#Se obtiene el data.frame de los correos adjuntos con el año y los lanzamientos
correos <- data.frame(Correos = coursesData$correo, Anios = coursesData$aÃ.o, Lanzamientos = coursesData$lanzamiento)

apply(coursesData, 1, validarOrden, datos = coursesData, output = 'estudiantes-consecutivos.csv')
