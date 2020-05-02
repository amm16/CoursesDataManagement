##Tratamiento Valores Nulos

setwd("/")
setwd("Users/ANA RAQUEL ANDINO/Documents/2020/R")
getwd()
DataF <- read.csv("LLO 2018-2019.csv",header = T,sep = ";",encoding = "UTF-8")
names(DataF) #Nombre de columnas -- Devuelve un vector
library(dplyr)



##Final.Grade tiene NA y Valores Vacios

##Cremos un vactor Vacio que contendra el procentaje o freq de 
##Las columnas con valores Nulos
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

##Que trtamiento hacer
##PAra Na de Final.Grade reemplazar con 0 porque no tienen nota asignada

##Demostrar que esos NA est'an para estudiantes que est'an en un estado Dropdut, and No show
df.dropout<- DataF %>% filter(Status=="Dropout") %>% select(Final.Grade)
df.no.show <- DataF %>% filter(Status=="No Show") %>% select(Final.Grade)
df.fail <- DataF %>% filter(Status=="Fail") %>% select(Final.Grade)
df.Pass <- DataF %>% filter(Status=="Pass") %>% select(Final.Grade)
summary(df.fail)
summary(df.Pass)

as.data.frame(prop.table(table(DataF[,"Final.Grade"] == "#N/A")))
as.data.frame(prop.table(table(DataF[,"Final.Grade"] == "")))

##Registros vacios y NA
DataF <- DataF[DataF$Final.Grade == "#N/A","Final.Grade"] <- 0