#-------------------- Se cargan las bibliotecas --------------------

#----- Se usa la biblioteca readxl para leer los archivos excel -----

#install.packages("readxl")
library(readxl)

#----- Se usa la biblioteca plyr para poder utilizar la funcion mapvalues -----

#install.packages("plyr")
library (plyr)

#----- Se usa la biblioteca dplyr para poder utilizar algunos verbos como select, mutate, filter, arrange, entre otros, y además para poder utilizar los pipes (%>%) -----

#install.packages("dplyr")
library (dplyr)

#----- Se usa la biblioteca lubridate para poder acceder al año de una fecha -----

#install.packages("lubridate")
library(lubridate)

#----- Se usa la biblioteca sqldf para realizar consultas SQL -----

#install.packages("sqldf")
library(sqldf)

#----- Se usa la biblioteca markovchain para realizar las cadenas de markov -----

#install.packages("markovchain")
library (markovchain)

#----- Se usa la biblioteca diagram para graficar las cadenas de Markov -----

#install.packages("diagram")
library(diagram)

#install.packages("ggplot2")
library(ggplot2)

#install.packages("languageserversetup")
#languageserversetup::languageserver_install()
languageserversetup::languageserver_add_to_rprofile()

#-------------------- Se definen las rutas que utiliza cada integrante del equipo --------------------

getwd()     #el resultado de esta linea es la ruta actual donde se esta trabajando

setwd("/home/fran/Git_WS/TPS-CsDatos")

## rutas relativas a los datos actualzados (hasta 2021)
ruta_1 <- "./Datos_2021/LegAlumnos_2021.xlsx"
ruta_2 <- "./Datos_2021/Notas_Cursadas_2021.xlsx"
ruta_3 <- "./Datos_2021/Notas_Finales_2021.xlsx"

#-------------------- Se cargan los archivos excel en variables y se renombra algunas columnas para luego tener un mejor manejo de ellas --------------------

datos_alumnos <- read_excel(ruta_1)
datos_guarani_cursadas <- read_excel(ruta_2)
datos_guarani_finales <- read_excel(ruta_3)

#-------------------- Se definen constantes del codigo--------------------#

cohorte_inicial <- 2011
cohorte_final <- 2020
numero_carrera_IS <- 206

#
#names(datos_alumnos)[names(datos_alumnos) == 'legajo'] <-
#    'LegajoT'
#names(datos_alumnos)[names(datos_alumnos) == 'fecha_ingreso'] <-
#    'fecha_ingreso'
#names(datos_alumnos)[names(datos_alumnos) == 'calidad'] <-
#    'calidad'
#
#names(datos_guarani_cursadas)[names(datos_guarani_cursadas) == 'Legajo'] <-
#    'LegajoT'
#names(datos_guarani_cursadas)[names(datos_guarani_cursadas) == 'nombre'] <-
#    'materia'
#
#names(datos_guarani_finales)[names(datos_guarani_finales) == 'legajo'] <-
#    'LegajoT'
#names(datos_guarani_finales)[names(datos_guarani_finales) == 'fecha'] <-
#    'fecha'
#names(datos_guarani_finales)[names(datos_guarani_finales) == 'nombre'] <-
#    'materia'
#
