setwd("/")
setwd("Users/ANA RAQUEL ANDINO/Documents/2020/R/Ranking-idh")
getwd()

DataF <- read.csv("courses_data_cleaned_ranking_idh.csv",header = T,sep = ",",encoding = "UTF-8")

names(DataF) 
library(dplyr)
summary(DataF)
DataF$notaFinal
str(DataF)


#Resumen estadistico variable Lanzamiento

#1. Ver distribucion de la variable
# Table
tablaVariable <- table(DataF$lanzamiento)
##Prop
prop.table(tablaVariable)
# DF Lanzamiento
df_perLanzamiento <- as.data.frame(prop.table(tablaVariable))
# Orden
df_perLanzamiento<- df_perLanzamiento %>% arrange(-Freq)
# Boxplot
boxplot(df_perLanzamiento$Freq)
hist(df_perLanzamiento$Freq)
qqnorm(df_perLanzamiento$Freq)
qqline(df_porVar$Freq)

chisq.test(tablaVariable)

##-----------------------------Registros 2018: 2402--------------------------
table(DataF$a.f1.o)
DF2018 <- DataF[(DataF$a.f1.o==2018),]
# Table
tablaVariable<- table(DF2018$lanzamiento)
##Prop
dfPrueba<- prop.table(tablaVariable)
# DF Lanzamiento
df_perLan2018 <- as.data.frame(prop.table(tablaVariable))
# Orden
df_perLan2018<- df_perLan2018 %>% arrange(-Freq)
# Boxplot
boxplot(df_perLan2018$Freq)
hist(df_perLan2018$Freq)
qqnorm(df_perLan2018$Freq)
qqline(df_porVar$Freq)

##-----------------------------Registros 2019: 1150--------------------------

DF2019 <- DataF[(DataF$a.f1.o==2019),]
# Table
tablaVariable <- table(DF2019$lanzamiento)
##Prop
prop.table(tablaVariable)
# DF Lanzamiento
df_perLan2019 <- as.data.frame(prop.table(tablaVariable))
# Orden
df_perLan2019<- df_perLan2019 %>% arrange(-Freq)
# Boxplot
boxplot(df_perLan2019$Freq)
hist(df_perLan2019$Freq)
qqnorm(df_perLan2019$Freq)
qqline(df_perLan2019$Freq)

#------------------------- Qué lanzamiento del año presenta mayor numero de deserciones
#Separar Df a;os
DF2019 <- DataF[(DataF$a.f1.o==2019),]
DF2018 <- DataF[(DataF$a.f1.o==2018),]

# Cantidad de estudiantes que estan seg'un estado y lanzamiento
dfF2018<- prop.table(table(DF2018$estado,DF2018$lanzamiento))
dfF2019<- prop.table(table(DF2019$estado,DF2019$lanzamiento))

# Escoger solo los registros que estan en estado= dropout
DFDesertorees<- DataF[(DataF$estado=="Dropout"),]
prop.table(table(DFDesertorees$lanzamiento))



#********************************************************************************
##-----------------------------A;o--------------------------

## Frecuencia
tablaVariable <- table(DataF$a.f1.o)
