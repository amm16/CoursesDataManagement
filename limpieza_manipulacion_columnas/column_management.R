
#Leer archivo csv y asignarlo a una variable
coursesData <- read.csv("CSV_DATASET.csv",header = T,sep = ",", encoding = "UTF-8")

#Asignar coursesData nuevamente pero exceptuando la columna 'level,'First.Name' y 'Language'
coursesData <- coursesData[,!(names(coursesData) %in% c("Level","First.Name","Language","Class"))]
#Asignar los nombres en una variable
coursesDataVariablesNames <- names(coursesData)



#Crear un nuevo dataframe que utiliza como nombre de columna los valores de courseColumn
df <- data.frame(column.name = coursesDataVariablesNames)

#Escribir dataframe en un archivo csv
#Mandar parametro como row.names como falso para evitar que se agregue una columna 
#extra con nombre vacio
write.csv(df,"column_names.csv", row.names = FALSE)

#Traducir el nombre de las columnas a espanol y llamar nuevamente el nuevo archivo csv
coursesDataVariablesNames <- read.csv("column_names_treatment.csv",header = T,sep = ",", encoding = "UTF-8")


#Antes de asignar los nombres convertir la columna de translations a character
coursesDataVariablesNames$translation <- as.character(coursesDataVariablesNames$translation)
#Asignarle los nuevos nombres
names(coursesData) <- coursesDataVariablesNames$translation


#Generar csv con data limpia
write.csv(coursesData,"courses_data_cleaned.csv", row.names = FALSE)