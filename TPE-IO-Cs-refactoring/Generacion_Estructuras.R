

#-------------------- Se filtran los dataframes para que contengan los datos que son de interés para el trabajo --------------------


filtrar_datos_alumnos <- function(dataset, carrera_requerida, anio_empieza_considerar, anio_finaliza_considerar, tope_mes_ingreso)
{
  datos_alumnos_filtrado <- 
                            dataset %>%
                            filter(carrera == carrera_requerida, year(fecha_ingreso) >= anio_empieza_considerar, year(fecha_ingreso) <= anio_finaliza_considerar, month(fecha_ingreso) <= tope_mes_ingreso, calidad != 'E') %>%
                            select(LegajoT, fecha_ingreso) %>% 
                            mutate(fecha_ingreso = format(as.POSIXct(fecha_ingreso, format='%Y-%m-%d %H:%M:%S'), format='%Y-%m-%d')) %>%
                            arrange(LegajoT) 
  datos_alumnos_filtrado
}

#----- Se filtran los datos de los alumnos para que solamente se muestren aquellos que corresponden al código de carrera 206 (Ingeniería de Sistemas), al plan 2011, que hallan ingresado entre 2012 y 2015 (cohortes a evaluar) antes de mayo, y que no ingresen por equivalencias a la carrera -----

datos_alumnos_filtrados <- filtrar_datos_alumnos(datos_alumnos, numero_carrera_IS, cohorte_inicial, cohorte_final, 05)



#----- Estos son los nombres de las materias obligatorias de la carrera. Se plantean estas ya que son las que producen el desgranamiento -----

nombre_materias_obligatorias <- c ("Algebra I", "Análisis Matemático I", "Introducción a la Programación I", "Química", 
                                   "Algebra Lineal", "Ciencias de la Computación I", "Introducción a la Programación II", "Física General", "Matemática Discreta", 
                                   "Análisis Matemático II", "Análisis y Diseño de Algoritmos I", "Ciencias de la Computación II", "Electricidad y Magnetismo", "Introducción a la Arquitectura de Sistemas", 
                                   "Análisis y Diseño de Algoritmos II", "Comunicación de Datos I", "Electrónica Digital", "Inglés", "Probabilidades y Estadística", 
                                   "Arquitectura de Computadoras I", "Estructuras de Almacenamiento de Datos", "Metodologías de Desarrollo de Software I", "Programación Orientada a Objetos", 
                                   "Bases de Datos I", "Investigación Operativa I", "Lenguajes de Programación I", "Programación Exploratoria", "Sistemas Operativos I",
                                   "Arquitectura de Computadoras y Técnicas Digitales", "Comunicación de Datos II", "Introducción al Cálculo Diferencial e Integral", "Teoría de la Información", 
                                   "Diseño de Compiladores I", "Diseño de Sistemas de Software", 
                                   "Ingeniería de Software",
                                   "Fundamentos de Economía y Proyectos de Inversión", "Legislación y Gestión Ambiental", "Organización Empresarial")

names(nombre_materias_obligatorias) <- nombre_materias_obligatorias
nombre_materias_obligatorias
#A continuacion se definen los meses de cada materia para poder decir que se finalizó a tiempo
meses_para_hacer_a_tiempo_materias_obligatorias <- c(5, 5, 5, 29, 
                                                     17, 13, 13, 13, 13, 
                                                     17, 17, 17, 17, 17, 
                                                     25, 37, 25, 49, 25, 
                                                     29, 29, 29, 29,
                                                     41, 41, 41, 41, 37, 
                                                     53, 53, 53, 53,
                                                     61, 49,
                                                     53, 
                                                     61, 53, 53)

nombre_materias_finales <- c ("Algebra I - Final", "Análisis Matemático I - Final", "Introducción a la Programación I - Final", "Química - Final", 
                              "Algebra Lineal - Final", "Ciencias de la Computación I - Final", "Introducción a la Programación II - Final", "Física General - Final", "Matemática Discreta - Final", 
                              "Análisis Matemático II - Final", "Análisis y Diseño de Algoritmos I - Final", "Ciencias de la Computación II - Final", "Electricidad y Magnetismo - Final", "Introducción a la Arquitectura de Sistemas - Final", 
                              "Análisis y Diseño de Algoritmos II - Final", "Comunicación de Datos I - Final", "Electrónica Digital - Final", "Inglés - Final", "Probabilidades y Estadística - Final")

names(nombre_materias_finales) <- nombre_materias_finales

meses_para_hacer_a_tiempo_finales <- c(29, 29, 29, 29, 
                                       37, 37, 37, 37, 37, 
                                       41, 41, 41, 41, 41, 
                                       49, 49, 49, 49, 49)

#----- Se obtienen los datos de cursadas que corresponden únicamente a las materias obligatorias de la carrera, descartando por tanto las materias optativas ----- 

filtrar_datos_notas <- function(dataset, carrera_requerida)
{
  datos_notas_filtrado <- 
                          dataset %>%
                          filter(carrera == carrera_requerida, LegajoT %in% datos_alumnos$LegajoT
                                 , nombre %in% nombre_materias_obligatorias   
                                 ) %>%
                          select(LegajoT, nombre, fecha, resultado)  %>% 
                          mutate(fecha = format(as.POSIXct(fecha, format='%Y-%m-%d %H:%M:%S'), format='%Y-%m-%d'))
  
  datos_notas_filtrado
}

#--- fecha -> cuando se paso la nota de cursadas ---

datos_cursadas_filtrados <- filtrar_datos_notas(datos_guarani_cursadas, numero_carrera_IS)

#----- Se obtienen los datos de finales que corresponden únicamente a las materias obligatorias de la carrera, descartando por tanto las materias optativas ----- 

finales <- filtrar_datos_notas(datos_guarani_finales, numero_carrera_IS)

datos_finales_filtrados <- finales %>% 
                         mutate(nombre = paste(nombre, "Final", sep = " - ")) 


#----- Se obtienen los datos de finales libres para luego agregarle a los alumnos que rindieron libre la nota de cursada para que no figure como que nunca aprobaron la cursada ----- 

finales_aprobados <- finales %>% 
                     filter(resultado == "A")

finales_libres <- sqldf('SELECT * 
                         FROM finales_aprobados
                         WHERE NOT EXISTS( SELECT * 
                                          FROM datos_cursadas_filtrados
                                          WHERE (finales_aprobados.LegajoT = datos_cursadas_filtrados.LegajoT and
                                                 finales_aprobados.nombre = datos_cursadas_filtrados.nombre and 
                                                 finales_aprobados.resultado = datos_cursadas_filtrados.resultado)
                                         )
                         ')



#----- Se obtienen los datos de cursadas promocionadas, para luego agregarle a los alumnos que promocionaron la nota de final para que no figure como que nunca aprobaron el final de la materia promocionada ----- 

materias_promocionadas <- datos_cursadas_filtrados %>%
                          filter(resultado == "P")

#print(materias_promocionadas)

for (i in 1:nrow(materias_promocionadas))
{
  materias_promocionadas [i, 2] <- paste (materias_promocionadas [i, 2], "Final", sep = " - ")
}


# ----- Se unen todos los dataframes en uno solo para luego trabajar con este ----- #

datos_guarani_unidos <- datos_cursadas_filtrados %>% 
                        union_all(finales_libres) %>%
                        union_all(datos_finales_filtrados) %>%
                        union_all (materias_promocionadas) %>%
                        arrange(LegajoT, fecha)


#---- Funciones para determinar la cantidad de meses entre dos fechas ----#

obtener_meses_fecha <- function(fecha) 
{ 
  fecha_normalizada <- as.POSIXlt(as.Date(fecha, origin = "1900-01-01"))
  fecha_normalizada$year*12 + fecha_normalizada$mon 
} 

obtener_diferencia_meses_entre_fechas <- function(fecha_1, fecha_2) 
{ 
  obtener_meses_fecha(fecha_2) - obtener_meses_fecha(fecha_1) 
}

#--- Se agrega la columna fecha_ingreso proveniente del dataframe datos_alumnos. Además se crean nuevas columnas de interés que serán utilizadas luego ---

datos_guarani_unidos <-
                        datos_guarani_unidos %>%
                        left_join(datos_alumnos_filtrados, by = c ("LegajoT" = "LegajoT"), suffix = c(".alum", ".cursada")) %>% 
                        mutate(fecha = as.Date(fecha)) %>% 
                        mutate(fecha_ingreso = as.Date(fecha_ingreso)) %>%
                        filter(fecha - fecha_ingreso > 0) %>%
                        mutate(meses_requeridos_para_regularizar = mapvalues(nombre, 
                                                                             from = c(nombre_materias_obligatorias, nombre_materias_finales), 
                                                                             to = c(meses_para_hacer_a_tiempo_materias_obligatorias, meses_para_hacer_a_tiempo_finales)), 
                               meses_transcurridos_para_regularizar = obtener_diferencia_meses_entre_fechas(fecha_ingreso, fecha))

datos_guarani_unidos$meses_requeridos_para_regularizar <- as.numeric(datos_guarani_unidos$meses_requeridos_para_regularizar)
##chequear la linea de codigo anterior !!!

datos_guarani_unidos <- 
                        datos_guarani_unidos %>% 
                        mutate(regularizo_a_tiempo = ifelse(meses_transcurridos_para_regularizar <= meses_requeridos_para_regularizar, "si", "no"))


#-------------------- Obteniendo cada una de las cohortes --------------------

filtrar_por_cohortes <- function(dataset, anio_empieza_considerar, anio_finaliza_considerar)
{
  cohorte <- 
            dataset %>% 
            filter(year(fecha_ingreso) >= anio_empieza_considerar & year(fecha_ingreso) <= anio_finaliza_considerar)
  cohorte
}



#----- Cohorte 2011 -----

#agregamos nuestra nueva cohorte 2011
cohorte_2011 <- filtrar_por_cohortes(datos_guarani_unidos, 2011, 2011) # TO_DO

#----- Cohorte 2012 -----

cohorte_2012 <- filtrar_por_cohortes(datos_guarani_unidos, 2012, 2012)

#----- Cohorte 2013 -----

cohorte_2013 <- filtrar_por_cohortes(datos_guarani_unidos, 2013, 2013)

#----- Cohorte 2014 -----

cohorte_2014 <- filtrar_por_cohortes(datos_guarani_unidos, 2014, 2014)

#----- Cohorte 2015 -----

cohorte_2015 <- filtrar_por_cohortes(datos_guarani_unidos, 2015, 2015)





#-------------------- Comienza la generacion de las ramas de materias y correlatividades --------------------#

#Este analisis es independiente de la cohorte, por lo que se puede hacer en cualquier cohorte

#----- Rama Ids: Ingeniería de Software -----

rama_IdS_nodo_1 <- list ("Algebra I", "Análisis Matemático I", "Introducción a la Programación I")
names(rama_IdS_nodo_1) <- c ("Algebra I", "Análisis Matemático I", "Introducción a la Programación I")

rama_IdS_nodo_2 <- list ("Ciencias de la Computación I", "Matemática Discreta", "Introducción a la Programación II", "Física General")
names(rama_IdS_nodo_2)<- c ("Ciencias de la Computación I", "Matemática Discreta", "Introducción a la Programación II", "Física General")

rama_IdS_nodo_3 <- list ("Ciencias de la Computación II", "Análisis y Diseño de Algoritmos I", "Análisis Matemático II", "Algebra Lineal", "Electricidad y Magnetismo")
names(rama_IdS_nodo_3) <- c ("Ciencias de la Computación II", "Análisis y Diseño de Algoritmos I", "Análisis Matemático II", "Algebra Lineal", "Electricidad y Magnetismo")

rama_IdS_nodo_4 <- list ("Análisis y Diseño de Algoritmos II", "Probabilidades y Estadística", "Electrónica Digital", "Introducción a la Arquitectura de Sistemas")
names(rama_IdS_nodo_4) <- c ("Análisis y Diseño de Algoritmos II", "Probabilidades y Estadística", "Electrónica Digital", "Introducción a la Arquitectura de Sistemas")

rama_IdS_nodo_5 <- list ("Estructuras de Almacenamiento de Datos", "Metodologías de Desarrollo de Software I", "Arquitectura de Computadoras I", 
                         "Algebra I - Final", "Análisis Matemático I - Final", "Introducción a la Programación I - Final", "Química - Final")
names(rama_IdS_nodo_5) <- c ("Estructuras de Almacenamiento de Datos", "Metodologías de Desarrollo de Software I", "Arquitectura de Computadoras I", 
                             "Algebra I - Final", "Análisis Matemático I - Final", "Introducción a la Programación I - Final", "Química - Final")

rama_IdS_nodo_6 <- list ("Bases de Datos I", "Programación Orientada a Objetos", "Sistemas Operativos I", 
                         "Análisis Matemático II - Final", "Análisis y Diseño de Algoritmos I - Final", "Ciencias de la Computación II - Final", "Electricidad y Magnetismo - Final", "Introducción a la Arquitectura de Sistemas - Final", 
                         "Algebra Lineal - Final", "Ciencias de la Computación I - Final", "Introducción a la Programación II - Final", "Física General - Final", "Matemática Discreta - Final")
names(rama_IdS_nodo_6) <- c ("Bases de Datos I", "Programación Orientada a Objetos", "Sistemas Operativos I", 
                             "Análisis Matemático II - Final", "Análisis y Diseño de Algoritmos I - Final", "Ciencias de la Computación II - Final", "Electricidad y Magnetismo - Final", "Introducción a la Arquitectura de Sistemas - Final", 
                             "Algebra Lineal - Final", "Ciencias de la Computación I - Final", "Introducción a la Programación II - Final", "Física General - Final", "Matemática Discreta - Final")

rama_IdS_nodo_7 <- list ("Diseño de Sistemas de Software", 
                         "Análisis y Diseño de Algoritmos II - Final", "Comunicación de Datos I - Final", "Probabilidades y Estadística - Final", "Electrónica Digital - Final", "Inglés - Final")
names(rama_IdS_nodo_7) <- c ("Diseño de Sistemas de Software", 
                             "Análisis y Diseño de Algoritmos II - Final", "Comunicación de Datos I - Final", "Probabilidades y Estadística - Final", "Electrónica Digital - Final", "Inglés - Final")

rama_IdS_nodo_8 <- list ("Ingeniería de Software")
names(rama_IdS_nodo_8) <- c ("Ingeniería de Software")

#--- Agrupación de los nodos ---

rama_IdS <- list (rama_IdS_nodo_1, rama_IdS_nodo_2, rama_IdS_nodo_3, rama_IdS_nodo_4, 
                  rama_IdS_nodo_5, rama_IdS_nodo_6, rama_IdS_nodo_7, rama_IdS_nodo_8)

names(rama_IdS) <- c ("AG I, AM I, IPROG I",
                      "CC I, MD, IPROG II, FG",
                      "CC II, AyDA I, AM II, AL, EyM", 
                      "AyDA II, PyE, ED, IAdeS", 
                      "EAdeD, MDdeS I,\n AdeC I, AG I - F,\nAM I - F,\n IPROG I - F, Q - F", 
                      "BdeD I, PROGOO,\n SO I, AM II - F,\nAyDA I - F, CC II - F, EyM - F,\nIAdeS - F, AL - F, CC I - F,\nIPROG II - F, FG - F, MD - F", 
                      "DSdeS, AyDA II - F, CdD I - F,\nPyE - F, ED - F, IGL - F", 
                      "IdS")


  


#----- Rama OO: Optativas obligatorias -----

rama_OO_nodo_1 <- list ("Algebra I", "Análisis Matemático I", "Introducción a la Programación I")
names(rama_OO_nodo_1) <- c ("Algebra I", "Análisis Matemático I", "Introducción a la Programación I")

rama_OO_nodo_2 <- list ("Ciencias de la Computación I", "Introducción a la Programación II", "Matemática Discreta", "Física General")
names(rama_OO_nodo_2) <- c ("Ciencias de la Computación I", "Introducción a la Programación II", "Matemática Discreta", "Física General")

rama_OO_nodo_3 <- list ("Análisis y Diseño de Algoritmos I", "Ciencias de la Computación II", "Electricidad y Magnetismo", "Análisis Matemático II",  "Algebra Lineal")
names(rama_OO_nodo_3) <- c ("Análisis y Diseño de Algoritmos I", "Ciencias de la Computación II", "Electricidad y Magnetismo", "Análisis Matemático II",  "Algebra Lineal")

rama_OO_nodo_4 <- list ("Introducción a la Arquitectura de Sistemas", "Análisis y Diseño de Algoritmos II", "Electrónica Digital", "Probabilidades y Estadística")
names(rama_OO_nodo_4) <- c ("Introducción a la Arquitectura de Sistemas", "Análisis y Diseño de Algoritmos II", "Electrónica Digital", "Probabilidades y Estadística")

rama_OO_nodo_5 <- list ("Arquitectura de Computadoras I", "Estructuras de Almacenamiento de Datos", "Metodologías de Desarrollo de Software I", "Programación Orientada a Objetos")
names(rama_OO_nodo_5) <- c ("Arquitectura de Computadoras I", "Estructuras de Almacenamiento de Datos", "Metodologías de Desarrollo de Software I", "Programación Orientada a Objetos")

rama_OO_nodo_6 <- list ("Bases de Datos I", "Investigación Operativa I", "Lenguajes de Programación I", "Programación Exploratoria", "Sistemas Operativos I")
names(rama_OO_nodo_6) <- c ("Bases de Datos I", "Investigación Operativa I", "Lenguajes de Programación I", "Programación Exploratoria", "Sistemas Operativos I")

#--- Rama FEyPI: Fundamentos de Economía y Proyectos de Inversión ---

rama_FEyPI_nodo_7 <- list ("Fundamentos de Economía y Proyectos de Inversión")
names(rama_FEyPI_nodo_7) <- c ("Fundamentos de Economía y Proyectos de Inversión")

#--- Rama LyGA: Legislación y Gestión Ambiental ---

rama_LyGA_nodo_7 <- list ("Legislación y Gestión Ambiental")
names(rama_LyGA_nodo_7) <- c ("Legislación y Gestión Ambiental")

#--- Rama OE: Organización Empresarial ---

rama_OE_nodo_7 <- list ("Organización Empresarial")
names(rama_OE_nodo_7) <- c ("Organización Empresarial")

#--- Agrupacion de los nodos ---

rama_FEyPI <- list (rama_OO_nodo_1, rama_OO_nodo_2, rama_OO_nodo_3, rama_OO_nodo_4, 
                    rama_OO_nodo_5, rama_OO_nodo_6, rama_FEyPI_nodo_7)
names(rama_FEyPI) <- c ("AG I, AM I, IPROG I",
                        "CC I, MD, IPROG II, FG",
                        "CC II, AyDA I, AM II, AL, EyM",
                        "AyDA II, PyE, ED, IAdeS",
                        "EAdeD, MDdeS I,\n AdeC I, PROGOO",
                        "BdeD I, IO I, SO I, PExp, LP I",
                        "FEyPI")

rama_LyGA <- list (rama_OO_nodo_1, rama_OO_nodo_2, rama_OO_nodo_3, rama_OO_nodo_4, 
                   rama_OO_nodo_5, rama_OO_nodo_6, rama_LyGA_nodo_7)

names(rama_LyGA) <- c ("AG I, AM I, IPROG I",
                       "CC I, MD, IPROG II, FG",
                       "CC II, AyDA I, AM II, AL, EyM",
                       "AyDA II, PyE, ED, IAdeS",
                       "EAdeD, MDdeS I,\n AdeC I, PROGOO",
                       "BdeD I, IO I, SO I, PExp, LP I",
                       "LyGA")

rama_OE <- list (rama_OO_nodo_1, rama_OO_nodo_2, rama_OO_nodo_3, rama_OO_nodo_4, 
                 rama_OO_nodo_5, rama_OO_nodo_6, rama_OE_nodo_7)
names(rama_OE) <- c ("AG I, AM I, IPROG I",
                     "CC I, MD, IPROG II, FG",
                     "CC II, AyDA I, AM II, AL, EyM",
                     "AyDA II, PyE, ED, IAdeS",
                     "EAdeD, MDdeS I,\n AdeC I, PROGOO",
                     "BdeD I, IO I, SO I, PExp, LP I",
                     "OEmp")


#----- Rama DdC: Diseño de Compiladores I ----- 

rama_DdC_nodo_1 <- list ("Algebra I", "Introducción a la Programación I")
names(rama_DdC_nodo_1) <- c ("Algebra I", "Introducción a la Programación I")

rama_DdC_nodo_2 <- list ("Ciencias de la Computación I", "Introducción a la Programación II", "Matemática Discreta")
names(rama_DdC_nodo_2) <- c ("Ciencias de la Computación I", "Introducción a la Programación II", "Matemática Discreta")

rama_DdC_nodo_3 <- list ("Análisis y Diseño de Algoritmos I", "Ciencias de la Computación II")
names(rama_DdC_nodo_3) <- c ("Análisis y Diseño de Algoritmos I", "Ciencias de la Computación II")

rama_DdC_nodo_4 <- list ("Análisis y Diseño de Algoritmos II")
names(rama_DdC_nodo_4) <- c ("Análisis y Diseño de Algoritmos II")

rama_DdC_nodo_5 <- list ("Programación Orientada a Objetos", 
                         "Algebra I - Final", "Análisis Matemático I - Final", "Introducción a la Programación I - Final", "Química - Final")

names(rama_DdC_nodo_5) <- c ("Programación Orientada a Objetos", 
                             "Algebra I - Final", "Análisis Matemático I - Final", "Introducción a la Programación I - Final", "Química - Final")

rama_DdC_nodo_6 <- list ("Lenguajes de Programación I", 
                         "Análisis Matemático II - Final", "Análisis y Diseño de Algoritmos I - Final", "Ciencias de la Computación II - Final", "Electricidad y Magnetismo - Final", "Introducción a la Arquitectura de Sistemas - Final", 
                         "Algebra Lineal - Final", "Ciencias de la Computación I - Final", "Introducción a la Programación II - Final", "Física General - Final", "Matemática Discreta - Final")

names(rama_DdC_nodo_6) <- c ("Lenguajes de Programación I", 
                             "Análisis Matemático II - Final", "Análisis y Diseño de Algoritmos I - Final", "Ciencias de la Computación II - Final", "Electricidad y Magnetismo - Final", "Introducción a la Arquitectura de Sistemas - Final", 
                             "Algebra Lineal - Final", "Ciencias de la Computación I - Final", "Introducción a la Programación II - Final", "Física General - Final", "Matemática Discreta - Final")

rama_DdC_nodo_7 <- list ("Diseño de Compiladores I")
names(rama_DdC_nodo_7) <- c ("Diseño de Compiladores I")

#--- Agrupacion de los nodos ---

rama_DdC <- list (rama_DdC_nodo_1, rama_DdC_nodo_2, rama_DdC_nodo_3, rama_DdC_nodo_4, 
                  rama_DdC_nodo_5, rama_DdC_nodo_6, rama_DdC_nodo_7)
names(rama_DdC) <- c ("AG I, IPROG I",
                      "CC I, IPROG II, MD",
                      "AyDA I, CC II",
                      "AyDA II",
                      "PROGOO, AG I - F, AM I - F,\n IPROG I - F, Q - F",
                      "LP I, AM II - F, AyDA I - F,\n CC II - F, EyM - F, IAdeS - F,\n AL - F, CC I - F,\n IPROG II -F, \n FG - F, MD - F",
                      "DdC")


#----- Rama TdI: Teoría de la Información -----

rama_TdI_nodo_1 <- list ("Introducción a la Programación I")
names(rama_TdI_nodo_1) <- c ("Introducción a la Programación I")

rama_TdI_nodo_2 <- list ("Introducción a la Programación II", "Análisis Matemático I", "Algebra I")
names(rama_TdI_nodo_2) <- c ("Introducción a la Programación II", "Análisis Matemático I", "Algebra I")

rama_TdI_nodo_3 <- list ("Introducción a la Arquitectura de Sistemas", "Análisis Matemático II", "Algebra Lineal", "Ciencias de la Computación I", "Matemática Discreta")
names(rama_TdI_nodo_3) <- c ("Introducción a la Arquitectura de Sistemas", "Análisis Matemático II", "Algebra Lineal", "Ciencias de la Computación I", "Matemática Discreta")

rama_TdI_nodo_4 <- list ("Comunicación de Datos I", "Probabilidades y Estadística", "Análisis y Diseño de Algoritmos I", 
                         "Algebra I - Final", "Análisis Matemático I - Final", "Introducción a la Programación I - Final", "Química - Final", 
                         "Algebra Lineal - Final", "Ciencias de la Computación I - Final", "Introducción a la Programación II - Final", "Física General - Final", "Matemática Discreta - Final")
names(rama_TdI_nodo_4) <- c ("Comunicación de Datos I", "Probabilidades y Estadística", "Análisis y Diseño de Algoritmos I", 
                             "Algebra I - Final", "Análisis Matemático I - Final", "Introducción a la Programación I - Final", "Química - Final", 
                             "Algebra Lineal - Final", "Ciencias de la Computación I - Final", "Introducción a la Programación II - Final", "Física General - Final", "Matemática Discreta - Final")

rama_TdI_nodo_5 <- list ("Teoría de la Información")
names(rama_TdI_nodo_5) <- c ("Teoría de la Información")

#--- Agrupacion de los nodos ---

rama_TdI <- list (rama_TdI_nodo_1, rama_TdI_nodo_2, rama_TdI_nodo_3, rama_TdI_nodo_4, rama_TdI_nodo_5)
names(rama_TdI) <- c ("IPROG I",
                      "IPROG II, AM I, AG I",
                      "IAdeS, AM II, AL, CC I, MD",
                      "CdD I, PyE, AyDA I, AG - F,\n AM I - F,IPROG I - F,\n Q - F, AL - F,\n CC I - F, IPROG II -F,\n FG - F, MD - F",
                      "TdI")


#----- Rama CdD2: Comunicación de Datos II -----

rama_CdD2_nodo_1 <- list ("Algebra I", "Análisis Matemático I", "Introducción a la Programación I")
names(rama_CdD2_nodo_1) <- c ("Algebra I", "Análisis Matemático I", "Introducción a la Programación I")

rama_CdD2_nodo_2 <- list ("Física General", "Ciencias de la Computación I", "Introducción a la Programación II", "Matemática Discreta")
names(rama_CdD2_nodo_2) <- c ("Física General", "Ciencias de la Computación I", "Introducción a la Programación II", "Matemática Discreta")

rama_CdD2_nodo_3 <- list ("Electricidad y Magnetismo", "Análisis y Diseño de Algoritmos I", "Ciencias de la Computación II", "Análisis Matemático II", "Algebra Lineal")
names(rama_CdD2_nodo_3) <- c ("Electricidad y Magnetismo", "Análisis y Diseño de Algoritmos I", "Ciencias de la Computación II", "Análisis Matemático II", "Algebra Lineal")

rama_CdD2_nodo_4 <- list ("Introducción a la Arquitectura de Sistemas", "Electrónica Digital", "Análisis y Diseño de Algoritmos II", "Probabilidades y Estadística")
names(rama_CdD2_nodo_4) <- c ("Introducción a la Arquitectura de Sistemas", "Electrónica Digital", "Análisis y Diseño de Algoritmos II", "Probabilidades y Estadística")

rama_CdD2_nodo_5 <- list ("Arquitectura de Computadoras I", "Estructuras de Almacenamiento de Datos", 
                         "Algebra I - Final", "Análisis Matemático I - Final", "Introducción a la Programación I - Final", "Química - Final")
names(rama_CdD2_nodo_5) <- c ("Arquitectura de Computadoras I", "Estructuras de Almacenamiento de Datos", 
                              "Algebra I - Final", "Análisis Matemático I - Final", "Introducción a la Programación I - Final", "Química - Final")

rama_CdD2_nodo_6 <- list ("Comunicación de Datos I", "Sistemas Operativos I", 
                          "Algebra Lineal - Final", "Ciencias de la Computación I - Final", "Introducción a la Programación II - Final", "Física General - Final", "Matemática Discreta - Final")
names(rama_CdD2_nodo_6) <- c ("Comunicación de Datos I", "Sistemas Operativos I", 
                              "Algebra Lineal - Final", "Ciencias de la Computación I - Final", "Introducción a la Programación II - Final", "Física General - Final", "Matemática Discreta - Final")

rama_CdD2_nodo_7  <- list ("Comunicación de Datos II")
names(rama_CdD2_nodo_7) <- c ("Comunicación de Datos II")

#--- Agrupacion de los nodos ---

rama_CdD2 <- list (rama_CdD2_nodo_1, rama_CdD2_nodo_2, rama_CdD2_nodo_3, 
                   rama_CdD2_nodo_4, rama_CdD2_nodo_5, rama_CdD2_nodo_6, rama_CdD2_nodo_7)
names(rama_CdD2) <- c ("AG I, AM I, IPROG I",
                       "FG, CC I, IPROG II, MD",
                       "EyM, AyDA I, CC II, AM II, AL",
                       "IAdeS, ED, AyDA II, PyE",
                       "AdC I, EAdD, AG I - F, AM I - F,\nIPROG I - F, Q - F",
                       "CdD I, SO I, AL - F, CC I - F,\n IPROG II - F,FG - F, MD - F", 
                       "CdD II")


#----- Rama ICDI: Introducción al Cálculo Diferenciál e Integral -----

rama_ICDI_nodo_1 <- list ("Análisis Matemático I")
names(rama_ICDI_nodo_1) <- c ("Análisis Matemático I")

rama_ICDI_nodo_2 <- list ("Análisis Matemático II", 
                          "Algebra I - Final", "Análisis Matemático I - Final", "Introducción a la Programación I - Final", "Química - Final", 
                          "Algebra Lineal - Final", "Ciencias de la Computación I - Final", "Introducción a la Programación II - Final", "Física General - Final", "Matemática Discreta - Final")
names(rama_ICDI_nodo_2) <- c ("Análisis Matemático II", 
                              "Algebra I - Final", "Análisis Matemático I - Final", "Introducción a la Programación I - Final", "Química - Final", 
                              "Algebra Lineal - Final", "Ciencias de la Computación I - Final", "Introducción a la Programación II - Final", "Física General - Final", "Matemática Discreta - Final")

rama_ICDI_nodo_3 <- list ("Introducción al Cálculo Diferencial e Integral")
names(rama_ICDI_nodo_3) <- c ("Introducción al Cálculo Diferencial e Integral")

#--- Agrupacion de los nodos ---

rama_ICDI <- list (rama_ICDI_nodo_1, rama_ICDI_nodo_2, rama_ICDI_nodo_3)
names(rama_ICDI) <- c ("AM I",
                       "AM II, AG I - F,\n AM I - F, IPROG I - F,\n Q - F, AL - F,\n CC I - F, IPROG II - F,\n FG - F, MD - F",
                       "ICDI")

#----- Rama AdCyTD: Arquitectura de Computadoras y Técnicas Digitales -----

rama_AdCyTD_nodo_1 <- list ("Análisis Matemático I")
names(rama_AdCyTD_nodo_1) <- c ("Análisis Matemático I")

rama_AdCyTD_nodo_2 <- list ("Física General", "Introducción a la Programación I")
names(rama_AdCyTD_nodo_2) <- c ("Física General", "Introducción a la Programación I")

rama_AdCyTD_nodo_3 <- list ("Electricidad y Magnetismo", "Introducción a la Programación II")
names(rama_AdCyTD_nodo_3) <- c ("Electricidad y Magnetismo", "Introducción a la Programación II")

rama_AdCyTD_nodo_4 <- list ("Electrónica Digital", "Introducción a la Arquitectura de Sistemas")
names(rama_AdCyTD_nodo_4) <- c ("Electrónica Digital", "Introducción a la Arquitectura de Sistemas")

rama_AdCyTD_nodo_5 <- list ("Arquitectura de Computadoras I", 
                            "Algebra I - Final", "Análisis Matemático I - Final", "Introducción a la Programación I - Final", "Química - Final", 
                            "Algebra Lineal - Final", "Ciencias de la Computación I - Final", "Introducción a la Programación II - Final", "Física General - Final", "Matemática Discreta - Final")
names(rama_AdCyTD_nodo_5) <- c ("Arquitectura de Computadoras I", 
                                "Algebra I - Final", "Análisis Matemático I - Final", "Introducción a la Programación I - Final", "Química - Final", 
                                "Algebra Lineal - Final", "Ciencias de la Computación I - Final", "Introducción a la Programación II - Final", "Física General - Final", "Matemática Discreta - Final")


rama_AdCyTD_nodo_6 <- list ("Arquitectura de Computadoras y Técnicas Digitales")
names(rama_AdCyTD_nodo_6) <- c ("Arquitectura de Computadoras y Técnicas Digitales")


#--- Agrupacion de los nodos ---

rama_AdCyTD <- list (rama_AdCyTD_nodo_1, rama_AdCyTD_nodo_2, rama_AdCyTD_nodo_3, 
                     rama_AdCyTD_nodo_4, rama_AdCyTD_nodo_5, rama_AdCyTD_nodo_6)
names(rama_AdCyTD) <- c ("AM I",
                         "FG, IPROG I",
                         "EyM, IPROG II",
                         "ED, IAdeS",
                         "AdC I, AG I - F, AM I - F,\n IPROG I - F, Q - F,\n AL -F, CC I - F,\n IPROG II - F, FG - F, MD - F",
                         "AdCyTD")


#-------------------- Agrupacion de todas las ramas en una lista --------------------

ramas_correlatividades <- list(rama_IdS, rama_FEyPI, rama_LyGA, rama_OE, rama_DdC, rama_TdI, rama_CdD2, rama_ICDI, rama_AdCyTD)

names(ramas_correlatividades) <- c("Ingeniería de Software", "Fundamentos de Economía y Proyectos de Inversión", "Legislación y Gestión Ambiental", "Organización Empresarial", 
                                   "Diseño de Compiladores I", "Teoría de la Información", "Comunicación de Datos II", "Introducción al Cálculo Diferencial e Integral", "Arquitectura de Computadoras y Técnicas Digitales")

