library(dplyr)
library(ggplot2)

setwd("~/");
setwd("Documents/Seminario/CoursesDataManagement/analisis_correlacional")
getwd()

survey <- read.csv("courses_data_cleaned_version_3.csv", sep = ",", header = T)

##sacar la proporcion entre lanzamiento y estado en funcion de fila
prop.table(table(survey$lanzamiento,survey$estado),2)

##sacar la proporcion entre lanzamiento y estado en funcion de columna
prop.table(table(survey$lanzamiento,survey$estado),1)

##Grafico
ggplot(survey) +
  aes(x = lanzamiento, fill = factor(estado)) +
  geom_bar(position = "stack") +
  theme(axis.text.x = element_text(angle = 45))

ggplot(survey) +
  aes(x = lanzamiento, fill = factor(estado)) +
  geom_bar(position = "fill") +
  theme(axis.text.x = element_text(angle = 45))

##Pearson's Chi-squared test p-value = 2.308e-12
chisq.test(table(survey$lanzamiento, survey$estado))

summary(survey)

##Hipotesis para las dos variables categoricas lanzamiento y estado 
##H_0: Las categorias de lanzamiento y estado son independientes. (conclusion en funcion de esta)
##H_A: Las categorias son dependientes.

##Conclusion: Segun nuestro p-value aceptamos nuestra hipotesis nula, por lo tanto las variables son independientes
  
##Grafico solo para mostrar que spring es el lanzamiento con el mayor numero
df_per <-as.data.frame(prop.table(table(survey$estado))) %>% arrange(Freq)
#Agrupar estado en Deserto y No Deserto
df_per[df_per$Var1 %in%c("Dropout","No Show"),"deserto"] <-"Deserto"
df_per[df_per$Var1 %in%c("Pass","Fail"),"deserto"] <-"No Deserto"

#Asignar nuevamente el dataframe pero solo seleccionando Var1 y deserto
df_per <- df_per %>% select(Var1,deserto)

#Unir las categorias a coursesData con un left join donde estado sea igual a Var1
survey <- left_join(survey,df_per,by=c("estado"="Var1"))
names(survey)
ggplot(survey) +
  aes(x = lanzamiento, fill = factor(deserto)) +
  geom_bar(position = "stack") +
  theme(axis.text.x = element_text(angle = 45))

names(coursesData)





