#Libraries
library(dplyr)
library(here)

#Set ablibrary(here)
#> here() starts at C:/test/someproject
here("data", "file.txt")
#> "C:/test/someproject/data/file.txt"
readLines(here("data", "file.txt"))
#> "The here package is awesome!"solute path
path <- "/"
setwd("/")
setwd("Users/ucaballero/Desktop/repositories/SEMINARIO/Survey analysis/")


survey <- read.csv("survey.csv",header = T,sep = ",", encoding = "UTF-8")
column_names <- read.csv("column_names_tratados.csv",header = T,sep = ";")
!(names(survey) %in% c("Numero.de.cuenta"))
survey <- survey[,!(names(survey) %in% c("Numero.de.cuenta"))]

str(survey)
summary(survey)

survey
head(survey,2)
tail(survey,2)

column_names <- column_names[ !(column_names$translation == "") , !(names(column_names) %in% c("X")) ]
column_names$translation <- as.character(column_names$translation)
names(survey) <- column_names$translation

head(survey)


write.csv(survey,"survey_cleaned.csv",row.names = F)