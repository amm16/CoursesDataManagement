##Tratamiento Valores Nulos

DataF <- read.csv("courses_data_cleaned.csv",header = T,sep = ",",encoding = "UTF-8")
##DataF <- read.csv("courses_data_analisis_categorias.csv",header = T,sep = ",",encoding = "UTF-8")
names(DataF) #Nombre de columnas -- Devuelve un vector
library(dplyr)
summary(DataF)
DataF$notaFinal
str(DataF)


table(DataF$nivelDelCurso)


DataF[DataF$nivelDelCurso=="Basic 2 ","nivelDelCurso"] <-"Basic 2"

##1. Eliminar Registros del 2020
DataF %>% filter(DataF$año==2020) ##Son 10 Registros
DataF <- DataF[!(DataF$año==2020),]
DataF %>% filter(DataF$lanzamiento=="Cycle l") 

##Final.Grade tiene NA y Valores Vacios

##Cremos un vactor Vacio que contendra el procentaje o freq de Las columnas con valores Nulos
na.summay <- c()

##Buscar Porcentajes de columnas
for (name in names(DataF)) { ##For each
  print(name) ##Nombre de cada columna name
  ## 1. table(DataF[,name] == "#N/A") Cuenta cuantos NA hay por columna
  ## 2. prop.table(table(DataF[,name] == "#N/A")) Proporcion de valores NA

  ##3. Crear un dataFrame con el %de valores Nulos
  s <- as.data.frame(prop.table(table(DataF[,name] == "#N/A")))
  d <- as.data.frame(prop.table(table(DataF[,name] == "")))
  
  ##-----Filtro de registros de cada name que si tienen valores Nulos, en un DF temporal--
  #1. Extraemos en un vector la freq de aquellas columnas que si tienen valores nulos
  extracionVectorNa <- s %>% filter(Var1 == T) %>% select (Freq)
  extracionVectorVa <- d %>% filter(Var1 == T) %>% select (Freq)

  ##2. Va a ver ocasiones en el que una columna no tenga ningun regsitro con NA Por lo tanto el length() del vector va a ser 0 o 1
  ##3. Asignar a un DFTemporal que agregue las columnas y su frequ cuando si tenga valore nulos
  DFTemporal <- data.frame(
    column.name=c(name),
    columNa.percentage = ifelse(length(extracionVectorNa$Freq) == 0, 0, extracionVectorNa$Freq[1]),
    columVacio.percentage = ifelse(length(extracionVectorVa$Freq) == 0, 0, extracionVectorVa$Freq[1])
  )
  
  ##4. Asignando a DF na.summay
  na.summay <- rbind(na.summay,DFTemporal)
}

##Mostrar solo las variables que tienen valores nulos
na.summay %>% arrange(-columNa.percentage) %>% filter(columNa.percentage>0) %>% filter(columVacio.percentage>0)

##Que trtamiento hacer
##PAra Na de Final.Grade reemplazar con 0 porque no tienen nota asignada


##Demostrar que esos NA est'an para estudiantes que est'an en un estado Dropdut, and No show
df.dropout<- DataF %>% filter(estado=="Dropout") %>% select(notaFinal)
df.no.show <- DataF %>% filter(estado=="No Show") %>% select(notaFinal)
df.fail <- DataF %>% filter(estado=="Fail") %>% select(notaFinal)
df.Pass <- DataF %>% filter(estado=="Pass") %>% select(notaFinal)


table( DataF %>% filter(notaFinal=="#N/A") %>% select(estado))
prop.table(table(DataF$notaFinal))

table( DataF %>% filter(notaFinal=="") %>% select(estado))


as.data.frame(prop.table(table(DataF[,"notaFinal"] == "#N/A")))
as.data.frame(prop.table(table(DataF[,"notaFinal"] == "")))

##Registros vacios y NA
DataF$notaFinal <- as.numeric(levels(DataF$notaFinal))[DataF$notaFinal]
DataF[DataF$notaFinal == "#N/A","notaFinal"] <- 0 ##Todos los NA de dropout y noshow se pueden dejar a 0




##---------------------------------Promedio de notas de estudiantes con estado=pass--------------------
##Dataframe con registros estado pass
df.Pass <- DataF %>% filter(estado=="Pass")
##Eliminar registros que tienen nota NA y Vacio
df.Pass <- df.Pass[!(df.Pass$notaFinal=="#NA"),]
df.Pass <- df.Pass[!(df.Pass$notaFinal==""),]
str(df.Pass)

##Conversiion de factor a numeric
df.Pass$notaFinal <- as.numeric(levels(df.Pass$notaFinal))[df.Pass$notaFinal]
##PRomedio de registros que pasaron
meanRegistrosPro<- mean(df.Pass$notaFinal)
meanRegistrosPro<- format(round(meanRegistrosPro, 2), nsmall = 2)

##---------------------------------Promedio de estudiantes estado=Fail--------------------
##Dataframe con registros estado pass
df.fail <- DataF %>% filter(estado=="Fail") 
##Eliminar registros que tienen nota NA y Vacio
df.fail <- df.fail[!(df.fail$notaFinal=="#NA"),]
df.fail <- df.fail[!(df.fail$notaFinal==""),]

##Conversiion de factor a numeric
df.fail$notaFinal <- as.numeric(levels(df.fail$notaFinal))[df.fail$notaFinal]
##PRomedio de registros que pasaron
meanRegistrosFail<- mean(df.fail$notaFinal)
meanRegistrosFail<- format(round(meanRegistrosFail, 2), nsmall = 2)


##---------------------------------Cambio valores vacios por promedio de notas---------------
##-------------Pass----------------
DFTemporalPass<- DataF %>% filter(estado=="Pass")
##Conversion de columna
DFTemporalPass$notaFinal <- as.numeric(levels(DFTemporalPass$notaFinal))[DFTemporalPass$notaFinal]
##Asignamos a NA porque convierte los valores vacios en NA
DFTemporalPass[is.na(DFTemporalPass$notaFinal),"notaFinal"] <- meanRegistrosPro
##DFTemporalPass[DFTemporalPass$notaFinal=="","notaFinal"] <- meanRegistrosPro


##Comprobar
table(DFTemporalPass[DFTemporalPass$notaFinal==meanRegistrosPro,"notaFinal"])



##---------Fail-------------------
DFTemporalFail<- DataF %>% filter(estado=="Fail")
DFTemporalFail$notaFinal <- as.numeric(levels(DFTemporalFail$notaFinal))[DFTemporalFail$notaFinal]
DFTemporalFail[is.na(DFTemporalFail$notaFinal),"notaFinal"] <- meanRegistrosFail

##Comprobar
table(DFTemporalFail[DFTemporalFail$notaFinal==meanRegistrosFail,"notaFinal"])

##------------Dropout-------------
DFTemporalDrop<- DataF %>% filter( (estado=="Dropout"))
DFTemporalDrop[DFTemporalDrop$estado=="Dropout","notaFinal"] <- 0


##-------------No show---------
DFTemporalNoshow<- DataF %>% filter(estado=="No Show")
DFTemporalNoshow[DFTemporalNoshow$estado=="No Show","notaFinal"] <- 0



##******************************************Unir Dataframes Temporales**************************
##Hacer el AIRbind
DFGeneralRows <- c()
DFGeneralRows <- rbind(DFGeneralRows,DFTemporalPass)
DFGeneralRows <- rbind(DFGeneralRows,DFTemporalFail)
DFGeneralRows <- rbind(DFGeneralRows,DFTemporalDrop)
DFGeneralRows <- rbind(DFGeneralRows,DFTemporalNoshow)

##Ejecutar For para comprobar Valores Nulos y vacios = 0
DataF<-DFGeneralRows

##Exportando a csv
write.csv(DataF,"Tratamiento-nulos-main.csv",row.names = FALSE)



