setwd("~/");
setwd("Documents/Seminario/CoursesDataManagement/analisis_descriptivo/5 eliminacion-columnas-correo-institucion")
getwd()

coursesData <- read.csv("courses_data_cleaned_desempeño.csv",header = T,sep = ",", encoding = "UTF-8")

#Se eliminan las columnas que ya no tienen utilidad ya que fueron utilizadas para generar otras
    # institucion, ranking, indiceIDH -> se utilizaron para obtener nivelEducativo 
    # correo, año -> se utilizo para obtener el numero de lanzamientos consecutivos, la cual se utilizo para obtener asistencia
    # notaFinal, mediaNotas -> se utilizaron para obtener desempeño

coursesData <- coursesData[,!(names(coursesData) %in% c("correo","institucion","notaFinal","ranking","indiceIDH","mediaNotas","año"))]
#Se corrigio nivel educativo que esta en mayuscula
names(coursesData)
names(coursesData)[4] <- "nivelEducativo"
write.csv(coursesData,"courses_data_cleaned_version_2", row.names = FALSE)

#Verificar resultado
names(coursesData)