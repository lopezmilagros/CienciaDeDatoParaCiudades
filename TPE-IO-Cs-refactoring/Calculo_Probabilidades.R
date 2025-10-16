#-------------------- Funciones para calcular la probabilidad de aprobar cada uno de los nodos que componen las ramas --------------------

#----- Generar matriz de probabilidades -----


# (0.5, 0.1, 0.6, 0.9)

# 0.5     0.5    0     0      0
# 0       0.9    0.1   0      0 
# 0       0      0.4   0.6    0
# 0       0       0    0.1    0.9
# 0        0      0     0     1


generar_matriz_probabilidades <- function (probabilidades_aprobacion) ## TO_DO revisar esta funcion
{
    matriz <- matrix(0, length (probabilidades_aprobacion) + 1, length (probabilidades_aprobacion) + 1)
    
    for(i in 1 : length(probabilidades_aprobacion))
    {
        matriz[i,i + 1] <- probabilidades_aprobacion[[i]]
        matriz[i,i] <- ( 1 - probabilidades_aprobacion[[i]] )
    }
    
    matriz[length (probabilidades_aprobacion) + 1,length (probabilidades_aprobacion) + 1] <- 1
    
    matriz
}

#----- Funcion para calcular los aprobados por cada nodo de la rama -----

calcular_probabilidades_por_rama_de_aprobar <- function (rama, cohorte, anios_retraso_considerados)
{
    probabilidades_por_rama_de_aprobar <- c() #declaramos el arreglo donde se guardaran las probabilidades de aprobar
    
    for (i in 1 : length (rama))  # por nodo de la rama
    {
        
        #--- Se obtiene el total de alumnos que cursaron dicho conjunto de materias ---
        total_alumnos_nodo <- cohorte %>%  # El operador pipeline %>% es útil para concatenar múltiples dplyr operaciones. 
            group_by(LegajoT) %>%
            filter(  (nombre %in% rama [[i]]) & (meses_transcurridos_para_regularizar <= meses_requeridos_para_regularizar + 12 * anios_retraso_considerados)) %>%
            distinct(nombre) %>%
            filter (n() == length(rama [[i]])) %>%
            distinct(LegajoT) %>%
            nrow()
        
        #--- Se obtienen los que aprobaron dicho conjunto de materias ---
        aprobados_nodo <- cohorte %>%
            group_by(LegajoT) %>%
            filter((nombre %in% rama[[i]]) & (resultado == "A" | resultado == "P") & (meses_transcurridos_para_regularizar <= meses_requeridos_para_regularizar + 12 * anios_retraso_considerados)) %>%
            distinct(nombre) %>%
            filter (n() == length(rama [[i]])) %>%
            distinct(LegajoT) %>%
            nrow()
        
        if (total_alumnos_nodo == 0)
        {
            probabilidades_por_rama_de_aprobar[i] <- 0  # Si no hay alumnos que cursaron la materia, la probabilidad de aprobar es 0
            # TO_DO: Por que no deberia ser -1, o 1?
        }
        else
        {
            probabilidades_por_rama_de_aprobar[i] <- aprobados_nodo / total_alumnos_nodo
        }
    }
    probabilidades_por_rama_de_aprobar
}

#----- Funcion para calcular las matrices de transicion de un mismo anio -----

calcular_matrices_probabilidades <- function (lista_ramas, cohorte, anios_retraso_considerados)
{
    matrices_probabilidades_cohorte <- list()
    
    for(i in 1:length(lista_ramas))
    {
        probabilidades_por_rama_de_aprobar <- calcular_probabilidades_por_rama_de_aprobar(lista_ramas[[i]], cohorte, anios_retraso_considerados)
        matriz <- generar_matriz_probabilidades(probabilidades_por_rama_de_aprobar)
        matrices_probabilidades_cohorte[[i]] <- matriz
        rownames(matrices_probabilidades_cohorte[[i]]) <- c(sprintf("nodo_%d", 1:length(lista_ramas[[i]])), "nodo_final")
        colnames(matrices_probabilidades_cohorte[[i]]) <- c(sprintf("nodo_%d", 1:length(lista_ramas[[i]])), "nodo_final")
    }
    
    names(matrices_probabilidades_cohorte) <- names(lista_ramas)
    matrices_probabilidades_cohorte
}

#----- Calculo de las matrices de cada anio -----
matrices_probabilidades_cohorte_2011 <- calcular_matrices_probabilidades(ramas_correlatividades, cohorte_2011, 0)
matrices_probabilidades_cohorte_2012 <- calcular_matrices_probabilidades(ramas_correlatividades, cohorte_2012, 0)
matrices_probabilidades_cohorte_2013 <- calcular_matrices_probabilidades(ramas_correlatividades, cohorte_2013, 0)
matrices_probabilidades_cohorte_2014 <- calcular_matrices_probabilidades(ramas_correlatividades, cohorte_2014, 0)
matrices_probabilidades_cohorte_2015 <- calcular_matrices_probabilidades(ramas_correlatividades, cohorte_2015, 0)
## Agregamos nuestra nueva cohorte
#matrices_probabilidades_cohorte_2011 <- calcular_matrices_probabilidades(ramas_correlatividades, cohorte_2011, 0)


#----- Funcion para hacer un promedio de aprobados de cada una de las cohortes -----

obtener_promedio_probabilidades <- function(lista_matrices, cantidad_ramas)
{
    promedio_matrices <- list()
    
    for(k in 1:cantidad_ramas)
    {
        promedio_matrices[[k]] <- matrix(0, nrow(lista_matrices[[k]]), ncol(lista_matrices[[k]]))
    }
    
    cont = 0
    for(i in 1:(length(lista_matrices)/cantidad_ramas))
    {
        for(j in 1:cantidad_ramas)
        {
            cont <- cont + 1
            promedio_matrices[[j]] <- promedio_matrices[[j]] + lista_matrices[[cont]]
        }
    }
    
    for(l in 1:cantidad_ramas)
    {
        promedio_matrices[[l]] <- promedio_matrices[[l]] / (length(lista_matrices)/cantidad_ramas)
    }
    
    names(promedio_matrices) <- names(lista_matrices[1:9])
    promedio_matrices
}

lista_matrices <- c(matrices_probabilidades_cohorte_2012, matrices_probabilidades_cohorte_2013, matrices_probabilidades_cohorte_2014, matrices_probabilidades_cohorte_2015)
promedio_probabilidades_cohortes <- obtener_promedio_probabilidades(lista_matrices, length(ramas_correlatividades))

#-------------------- Realizando las cadenas de Markov --------------------

#----- Funcion para crear las cadenas de Markov -----

obtener_cadenas <- function (matrices)
{
    cadenas_markov <- list()
    
    for(i in 1:length(matrices))
    {
        cadenas_markov[[i]] <- new("markovchain", transitionMatrix = matrices[[i]], states = colnames(matrices[[i]]), name = names(matrices[i]))
    }
    
    cadenas_markov
}

#----- Funcion para graficar las cadenas de Markov -----

graficar_cadenas <- function (cadenas, lista_ramas, anio, plot , color_vertex)
{
    
    for(i in 1:length(cadenas))
    {
        rownames(cadenas[[i]]) <- c(names(lista_ramas[[i]]), "nodo_final")
        colnames(cadenas[[i]]) <- c(names(lista_ramas[[i]]), "nodo_final")
        plotmat(cadenas[[i]],
                main = paste(names (cadenas[i]), anio, sep  = " - cohorte "), 
                lwd = 1, 
                box.lwd = 2,
                cex.txt = 0.5,
                box.size = 0.08,
                box.cex = 0.9, 
                box.type = "square",
                shadow.size = 0, 
                box.prop = 0.5,
                box.col = color_vertex,
                arr.length=.2,
                arr.width=.2,
                arr.type = "T",
                mx = -0.02,
                my = -0.009, 
                self.cex = .15,
                self.shifty = .02,
                self.shiftx = c(-0.09, -0.09, 0.094, -0.09, 0.094, 0.094, 0.094, 0.094),
                curve = 0.15,
                dtext = 0.2)
        
        if(plot){
            text_legend <- c("Algebra I - AG I, Análisis Matemático I - AM I", "Introducción a la Programación I - IPROG I, Química - Q",
                             "Algebra Lineal - AL, Ciencias de la Computación I - CC I", "Introducción a la Programación II - IPROG II, Física General - FG", "Matemática Discreta - MD, Análisis Matemático II - AM II",
                             "Análisis y Diseño de Algoritmos I - AyDA I, Ciencias de la Computación II - CC II", "Electricidad y Magnetismo - EyM, Introducción a la Arquitectura de Sistemas - IAdeS",
                             "Análisis y Diseño de Algoritmos II - AyDA II, Comunicación de Datos I - CdD I", "Electrónica Digital - ED, Inglés - I", "Probabilidades y Estadística - PyE, Arquitectura de Computadoras I - AdC I",
                             "Estructuras de Almacenamiento de Datos - EAdD", "Metodologías de Desarrollo de Software I - MDdeS I", "Programación Orientada a Objetos - PROGOO I, Bases de Datos I - BdD I",
                             "Investigación Operativa I - IO I, Lenguajes de Programación I - LdP I", "Programación Exploratoria - PE, Sistemas Operativos I - SO I",
                             "Arquitectura de Computadoras y Técnicas Digitales - AdCyTD", "Comunicación de Datos II - CdD II", "Introducción al Cálculo Diferencial e Integral - ICDI, Teoría de la Información - TdI",
                             "Diseño de Compiladores I - DdC I, Diseño de Sistemas de Software - DSdeS",
                             "Ingeniería de Software - IdS, Fundamentos de Economía y Proyectos de Inversión - FeyPI",
                             "Legislación y Gestión Ambiental - LyGA, Organización Empresarial - OE")
            legend("center",
                   legend = text_legend,
                   cex=0.55,
                   y.intersp = 0.27,
                   text.width = strwidth(text_legend)[4]/4,
                   lty)
        }
    }
}

cadenas_markov_2011 <- obtener_cadenas(matrices_probabilidades_cohorte_2011)
cadenas_markov_2012 <- obtener_cadenas(matrices_probabilidades_cohorte_2012)
cadenas_markov_2013 <- obtener_cadenas(matrices_probabilidades_cohorte_2013)
cadenas_markov_2014 <- obtener_cadenas(matrices_probabilidades_cohorte_2014)
cadenas_markov_2015 <- obtener_cadenas(matrices_probabilidades_cohorte_2015)
## Agregamos la cade de markov de la cohorte 2011



#----- Graficando las cadenas de Markov -----

##Doc Material, Cadenas de markov representando cada rama de cada cohorte
cadenas_markov_promedio <- obtener_cadenas(promedio_probabilidades_cohortes)

graficar_cadenas(matrices_probabilidades_cohorte_2011, ramas_correlatividades, "2011", FALSE , "light blue")
graficar_cadenas(matrices_probabilidades_cohorte_2012, ramas_correlatividades, "2012", FALSE , "light blue")
graficar_cadenas(matrices_probabilidades_cohorte_2013, ramas_correlatividades, "2013", FALSE , "pink")
graficar_cadenas(matrices_probabilidades_cohorte_2014, ramas_correlatividades, "2014", FALSE , "light yellow")
graficar_cadenas(matrices_probabilidades_cohorte_2015, ramas_correlatividades, "2015", FALSE , "gray")
graficar_cadenas(promedio_probabilidades_cohortes, ramas_correlatividades, "promedio", FALSE , "light green")

#-------------------- Realizando un relevamiento sobre la cantidad de alumnos que finalizan todas las materias a tiempo por cohorte --------------------

#----- Funcion para obtener los alumnos de una cohorte que aprueban el ultimo nodo de una rama en particular -----

alumnos_aprobados_ultimo_nodo <- function (rama, dataset, anios_retraso_considerados)
{
    
    aprobados <- 
        dataset %>%
        group_by(LegajoT) %>%
        filter((nombre %in% rama[[length (rama)]]) & (resultado == "A" | resultado == "P") & (meses_transcurridos_para_regularizar <= meses_requeridos_para_regularizar + 12 * anios_retraso_considerados)) %>%
        distinct(nombre) %>%
        filter (n() == length(rama [[length (rama)]])) %>%
        distinct(LegajoT) 
    
    aprobados
}

#----- Funcion para obtener los legajos de los alumnos de una cohorte que aprueban cada una de las ramas de la carrera a tiempo -----

obtener_legajos_alumnos_terminan_cada_rama <- function (lista_ramas, dataset_anio, anios_retraso_considerados)
{
    alumnos_terminan_cada_rama <- list()
    
    for(i in 1:length(lista_ramas))
    {
        alumnos_terminan_cada_rama[[i]] <- alumnos_aprobados_ultimo_nodo(lista_ramas[[i]], dataset_anio, anios_retraso_considerados)
    }
    
    names(alumnos_terminan_cada_rama) <- names(lista_ramas[1:9])
    alumnos_terminan_cada_rama
}

#----- Funcion para obtener los legajos de los alumnos de una cohorte que aprueban todas las ramas de la carrera a tiempo -----

obtener_legajos_alumnos_terminan <- function (alumnos_terminan_anio)
{
    alumnos_terminan <- alumnos_terminan_anio[[1]]
    for(i in 2:length(alumnos_terminan_anio))
    {
        alumnos_terminan <- 
            alumnos_terminan %>% 
            inner_join(alumnos_terminan_anio[[i]], by = c("LegajoT"))
    }
    
    alumnos_terminan
}

#----- Funcion para determinar la probabilidad de que alumnos de una misma cohorte finalicen juntos todas las ramas de la carrera -----

obtener_probabilidad_de_terminar <- function(alumnos_terminan_anio, dataset_anio)
{
    aprobados <- 
        alumnos_terminan_anio %>% 
        nrow()
    
    total <- 
        dataset_anio %>% 
        distinct(LegajoT) %>%
        nrow()
    
    if (total == 0)
    {
        prob_aprobar <- 0
    }
    else
    {
        prob_aprobar <- aprobados/total
    }
    
    prob_aprobar
}

graficar_torta <- function (alumnos_terminan_anio, dataset_anio, anio_retraso, anio_cohorte)
{
    aprobados <- 
        alumnos_terminan_anio %>% 
        nrow()
    
    total <- 
        dataset_anio %>% 
        distinct(LegajoT) %>%
        nrow()
    
    if (total == 0)
    {
        porcentaje <- 0
    }
    else
    {
        porcentaje <- (aprobados / total) * 100
    }
    
    etiquetas <- c(paste ("terminaron con",paste (anio_retraso, " años de retraso", sep = " "), sep = " "), "no terminaron")
    etiquetas <- paste (etiquetas, porcentaje, sep = "\n")
    etiquetas <- paste (etiquetas, "%", sep = "")
    pie (c(aprobados, total), labels = etiquetas, col = c("green", "red"),  main = paste (paste ("Estudiantes que finalizaron vs Estudiantes que no finalizaron\n - Cohorte", anio_cohorte, sep = " "), anio_retraso, sep = " . Años de retraso: "))
}


# alumnos 2012
##Doc material, Alumnos que terminan cada rama en la cohorte 2012
alumnos_terminan_cada_rama_2012 <- obtener_legajos_alumnos_terminan_cada_rama(ramas_correlatividades, cohorte_2012, 0)
alumnos_terminan_cada_rama_2012
alumnos_terminan_2012 <- obtener_legajos_alumnos_terminan(alumnos_terminan_cada_rama_2012)

graficar_torta (alumnos_terminan_2012, cohorte_2012, 0, "2012")


##Doc material, Alumnos que terminan la carrera de la cohorte 2012
alumnos_terminan_2012
obtener_probabilidad_de_terminar(alumnos_terminan_2012, cohorte_2012)

# alumnos 2013

alumnos_terminan_cada_rama_2013 <- obtener_legajos_alumnos_terminan_cada_rama(ramas_correlatividades, cohorte_2013, 0)
alumnos_terminan_2013 <- obtener_legajos_alumnos_terminan(alumnos_terminan_cada_rama_2013)

graficar_torta (alumnos_terminan_2013, cohorte_2013, 0, "2013")

alumnos_terminan_2013
obtener_probabilidad_de_terminar(alumnos_terminan_2013, cohorte_2013)

# alumnos 2014

alumnos_terminan_cada_rama_2014 <- obtener_legajos_alumnos_terminan_cada_rama(ramas_correlatividades, cohorte_2014, 0)
alumnos_terminan_2014 <- obtener_legajos_alumnos_terminan(alumnos_terminan_cada_rama_2014)

graficar_torta (alumnos_terminan_2014, cohorte_2014, 0, "2014")

alumnos_terminan_2014
obtener_probabilidad_de_terminar(alumnos_terminan_2014, cohorte_2014)

# alumnos 2015

alumnos_terminan_cada_rama_2015 <- obtener_legajos_alumnos_terminan_cada_rama(ramas_correlatividades, cohorte_2015, 0)
alumnos_terminan_2015 <- obtener_legajos_alumnos_terminan(alumnos_terminan_cada_rama_2015)

graficar_torta (alumnos_terminan_2015, cohorte_2015, 0, "2015")

alumnos_terminan_2015
obtener_probabilidad_de_terminar(alumnos_terminan_2015, cohorte_2015)

##replicamos el proceso para la cohorte 2011
alumnos_terminan_cada_rama_2011 <- obtener_legajos_alumnos_terminan_cada_rama(ramas_correlatividades, cohorte_2011, 0)
alumnos_terminan_2011 <- obtener_legajos_alumnos_terminan(alumnos_terminan_cada_rama_2011)

graficar_torta (alumnos_terminan_2011, cohorte_2011, 0, "2011")

alumnos_terminan_2011
obtener_probabilidad_de_terminar(alumnos_terminan_2011, cohorte_2011)




#-------------------- Analisis de cada materia en particular --------------------

#----- Funcion para obtener cuantos aprueban, promocionan, reprueban, y están ausentes o abandonaron por materia -----


obtener_resultados_cursadas_por_materia <- function(dataset_anio, anios_retraso_considerados)
{
    resultados <- list()
    
    for(i in 1:length(nombre_materias_obligatorias))
    {
        resultados[[i]] <- 
            dataset_anio %>% 
            group_by(resultado) %>% 
            filter((nombre == nombre_materias_obligatorias[[i]]) & (meses_transcurridos_para_regularizar <= meses_requeridos_para_regularizar + 12 * anios_retraso_considerados)) %>% 
            distinct(LegajoT) %>% 
            count(resultado)
    }
    names(resultados) <- names(nombre_materias_obligatorias)
    
    resultados
}

#----- Funcion para graficar cuantos aprueban, promocionan, reprueban, y están ausentes o abandonaron una materia en particular -----

graficar_resultado_cursada <- function(resultados, anio, materia)
{
    df <- data.frame(x = resultados[materia])
    names(df) <- c("resultado", "n")
    ggplot(data = df, aes(x = resultado, y = n)) + geom_bar(stat = "identity", color = "blue", fill = "white") + scale_y_continuous(limits = c(0, 100)) + ggtitle (paste ("Resultados", paste (anio, materia, sep = " - "), sep = " "))
}

#----- Funcion para obtener la probabilidad acorde a un/varios resultado/s (aprobar, reprobar, promocionar, ausente o abandono) por materia -----

calcular_probabilidades_por_materia <- function (cohorte, anios_retraso_considerados, resultados_considerados)
{
    probabilidades_por_materia <- c()
    
    for (i in 1 : length (nombre_materias_obligatorias))
    {
        #--- Se obtiene el total de alumnos que cursaron la materia ---
        
        total <-
            cohorte %>%
            group_by(resultado) %>%
            filter((nombre == nombre_materias_obligatorias[[i]]) & (meses_transcurridos_para_regularizar <= meses_requeridos_para_regularizar + 12 * anios_retraso_considerados)) %>%
            distinct(LegajoT) %>%
            nrow()
        
        #--- Se obtienen los que cumplen el/los resultado/s considerado/s ---
        
        cumplen_resultado <- 
            cohorte %>%
            group_by(resultado) %>%
            filter((nombre == nombre_materias_obligatorias[[i]]) & resultado %in% resultados_considerados & (meses_transcurridos_para_regularizar <= meses_requeridos_para_regularizar + 12 * anios_retraso_considerados)) %>%
            distinct(LegajoT) %>%
            nrow()
        
        if (total == 0)
        {
            probabilidades_por_materia[i] <- 0
        }
        else
        {
            probabilidades_por_materia[i] <- cumplen_resultado / total
        }
        
    }
    names(probabilidades_por_materia) <- names(nombre_materias_obligatorias)
    
    probabilidades_por_materia
}


## Doc Material (cantidad de alumnos que aprueban, recursan o dejan las materias en cierta cohorte, con el retraso de años)

resultados_2012 <- obtener_resultados_cursadas_por_materia(cohorte_2012, 0)
resultados_2013 <- obtener_resultados_cursadas_por_materia(cohorte_2013, 0)
resultados_2014 <- obtener_resultados_cursadas_por_materia(cohorte_2014, 0)
resultados_2015 <- obtener_resultados_cursadas_por_materia(cohorte_2015, 0)
#agregamos resultados de la cohorte 2011
# resultados_2011 <- obtener_resultados_cursadas_por_materia(cohorte_2011, 0)

graficar_resultado_cursada(resultados_2012, "2012", "Algebra I")

#DOC MATERIAL (DESGRANAMIENTO, SE LLAMA A LA FUNCION CON R y U)
probabilidades_por_materia_2012 <- calcular_probabilidades_por_materia(cohorte_2012, 0, c("R","U"))
probabilidades_por_materia_2012

probabilidades_por_materia_2013 <- calcular_probabilidades_por_materia(cohorte_2013, 0, c("R", "U"))
probabilidades_por_materia_2013

probabilidades_por_materia_2014 <- calcular_probabilidades_por_materia(cohorte_2014, 0, c("R"))
probabilidades_por_materia_2014

probabilidades_por_materia_2015 <- calcular_probabilidades_por_materia(cohorte_2015, 0, c("R", "U"))
probabilidades_por_materia_2015

##agregamos la cohorte 2011
probabilidades_por_materia_2011 <- calcular_probabilidades_por_materia(cohorte_2011, 0, c("R", "U"))
probabilidades_por_materia_2011

lista_matrices <-
    c(
        matrices_probabilidades_cohorte_2012,
        matrices_probabilidades_cohorte_2013,
        matrices_probabilidades_cohorte_2014,
        matrices_probabilidades_cohorte_2015
    )
promedio_probabilidades_cohortes <-
    obtener_promedio_probabilidades(lista_matrices, length(ramas_correlatividades))

#-------------------- Realizando las cadenas de Markov --------------------#

#----- Funcion para crear las cadenas de Markov -----

obtener_cadenas_markov <- function (matrices)
{
    cadenas_markov <- list()
    
    for (i in 1:length(matrices))
    {
        cadenas_markov[[i]] <-
            new(
                "markovchain",
                transitionMatrix = matrices[[i]],
                states = colnames(matrices[[i]]),
                name = names(matrices[i])
            )
    }
    
    cadenas_markov
}

#----- Funcion para graficar las cadenas de Markov -----

graficar_cadenas <-
    function (cadenas, lista_ramas, anio, color_vertex)
    {
        for (i in 1:length(cadenas))
        {
            rownames(cadenas[[i]]) <- c(names(lista_ramas[[i]]), "nodo_final")
            colnames(cadenas[[i]]) <- c(names(lista_ramas[[i]]), "nodo_final")
            plotmat(
                cadenas[[i]],
                main = paste(names (cadenas[i]), anio, sep  = " - cohorte "),
                lwd = 1,
                box.lwd = 2,
                cex.txt = 0.5,
                box.size = 0.08,
                box.cex = 0.9,
                box.type = "square",
                shadow.size = 0,
                box.prop = 0.5,
                box.col = color_vertex,
                arr.length = .2,
                arr.width = .2,
                arr.type = "T",
                mx = -0.02,
                my = -0.009,
                self.cex = .15,
                self.shifty = .02,
                self.shiftx = c(-0.09,-0.09, 0.094,-0.09, 0.094, 0.094, 0.094, 0.094),
                curve = 0.15,
                dtext = 0.2
            )
            
            text_legend <-
                c(
                    "Algebra I - AG I, Análisis Matemático I - AM I",
                    "Introducción a la Programación I - IPROG I, Química - Q",
                    "Algebra Lineal - AL, Ciencias de la Computación I - CC I",
                    "Introducción a la Programación II - IPROG II, Física General - FG",
                    "Matemática Discreta - MD, Análisis Matemático II - AM II",
                    "Análisis y Diseño de Algoritmos I - AyDA I, Ciencias de la Computación II - CC II",
                    "Electricidad y Magnetismo - EyM, Introducción a la Arquitectura de Sistemas - IAdeS",
                    "Análisis y Diseño de Algoritmos II - AyDA II, Comunicación de Datos I - CdD I",
                    "Electrónica Digital - ED, Inglés - I",
                    "Probabilidades y Estadística - PyE, Arquitectura de Computadoras I - AdC I",
                    "Estructuras de Almacenamiento de Datos - EAdD",
                    "Metodologías de Desarrollo de Software I - MDdeS I",
                    "Programación Orientada a Objetos - PROGOO I, Bases de Datos I - BdD I",
                    "Investigación Operativa I - IO I, Lenguajes de Programación I - LdP I",
                    "Programación Exploratoria - PE, Sistemas Operativos I - SO I",
                    "Arquitectura de Computadoras y Técnicas Digitales - AdCyTD",
                    "Comunicación de Datos II - CdD II",
                    "Introducción al Cálculo Diferencial e Integral - ICDI, Teoría de la Información - TdI",
                    "Diseño de Compiladores I - DdC I, Diseño de Sistemas de Software - DSdeS",
                    "Ingeniería de Software - IdS, Fundamentos de Economía y Proyectos de Inversión - FeyPI",
                    "Legislación y Gestión Ambiental - LyGA, Organización Empresarial - OE"
                )
            legend(
                "center",
                legend = text_legend,
                cex = 0.55,
                y.intersp = 0.27,
                text.width = strwidth(text_legend)[4] / 4,
                lty
            )
        }
    }

## Agregamos la cade de markov de la cohorte 2011
cadenas_markov_2011 <-
    obtener_cadenas_markov(matrices_probabilidades_cohorte_2011)
cadenas_markov_2012 <-
    obtener_cadenas_markov(matrices_probabilidades_cohorte_2012)
cadenas_markov_2013 <-
    obtener_cadenas_markov(matrices_probabilidades_cohorte_2013)
cadenas_markov_2014 <-
    obtener_cadenas_markov(matrices_probabilidades_cohorte_2014)
cadenas_markov_2015 <-
    obtener_cadenas_markov(matrices_probabilidades_cohorte_2015)


#----- Graficando las cadenas de Markov -----

cadenas_markov_promedio <-
    obtener_cadenas_markov(promedio_probabilidades_cohortes)

graficar_cadenas(matrices_probabilidades_cohorte_2012,
                 ramas_correlatividades,
                 "2012",
                 "light blue")
graficar_cadenas(matrices_probabilidades_cohorte_2013,
                 ramas_correlatividades,
                 "2013",
                 "pink")
graficar_cadenas(
    matrices_probabilidades_cohorte_2014,
    ramas_correlatividades,
    "2014",
    "light yellow"
)
graficar_cadenas(matrices_probabilidades_cohorte_2015,
                 ramas_correlatividades,
                 "2015",
                 "gray")
graficar_cadenas(
    promedio_probabilidades_cohortes,
    ramas_correlatividades,
    "promedio",
    "light green"
)

#-------------------- Realizando un relevamiento sobre la cantidad de alumnos que finalizan todas las materias a tiempo por cohorte --------------------

#----- Funcion para obtener los alumnos de una cohorte que aprueban el ultimo nodo de una rama en particular -----

alumnos_aprobados_ultimo_nodo <-
    function (rama,
              dataset,
              anios_retraso_considerados)
    {
        aprobados <-
            dataset %>%
            group_by(legajoT) %>%
            filter((materia %in% rama[[length (rama)]]) &
                       (resultado == "A" |
                            resultado == "P") &
                       (
                           meses_transcurridos_para_regularizar <= meses_requeridos_para_regularizar + 12 * anios_retraso_considerados
                       )
            ) %>%
            distinct(materia) %>%
            filter (n() == length(rama [[length (rama)]])) %>%
            distinct(legajoT)
        
        aprobados
    }

#----- Funcion para obtener los legajos de los alumnos de una cohorte que aprueban cada una de las ramas de la carrera a tiempo -----

obtener_legajos_alumnos_terminan_cada_rama <-
    function (lista_ramas,
              dataset_anio,
              anios_retraso_considerados)
    {
        alumnos_terminan_cada_rama <- list()
        
        for (i in 1:length(lista_ramas))
        {
            alumnos_terminan_cada_rama[[i]] <-
                alumnos_aprobados_ultimo_nodo(lista_ramas[[i]],
                                              dataset_anio,
                                              anios_retraso_considerados)
        }
        
        names(alumnos_terminan_cada_rama) <- names(lista_ramas[1:9])
        alumnos_terminan_cada_rama
    }

#----- Funcion para obtener los legajos de los alumnos de una cohorte que aprueban todas las ramas de la carrera a tiempo -----

obtener_legajos_alumnos_terminan <- function (alumnos_terminan_anio)
{
    alumnos_terminan <- alumnos_terminan_anio[[1]]
    for (i in 2:length(alumnos_terminan_anio))
    {
        alumnos_terminan <-
            alumnos_terminan %>%
            inner_join(alumnos_terminan_anio[[i]], by = c("legajoT"))
    }
    
    alumnos_terminan
}

#----- Funcion para determinar la probabilidad de que alumnos de una misma cohorte finalicen juntos todas las ramas de la carrera -----

obtener_probabilidad_de_terminar <-
    function(alumnos_terminan_anio, dataset_anio)
    {
        aprobados <-
            alumnos_terminan_anio %>%
            nrow()
        
        total <-
            dataset_anio %>%
            distinct(legajoT) %>%
            nrow()
        
        if (total == 0)
        {
            prob_aprobar <- 0
        }
        else
        {
            prob_aprobar <- aprobados / total
        }
        
        prob_aprobar
    }

graficar_torta <-
    function (alumnos_terminan_anio,
              dataset_anio,
              anio_retraso,
              anio_cohorte)
    {
        aprobados <-
            alumnos_terminan_anio %>%
            nrow()
        
        total <-
            dataset_anio %>%
            distinct(legajoT) %>%
            nrow()
        
        if (total == 0)
        {
            porcentaje <- 0
        }
        else
        {
            porcentaje <- (aprobados / total) * 100
        }
        
        etiquetas <-
            c(paste (
                "terminaron con",
                paste (anio_retraso, " años de retraso", sep = " "),
                sep = " "
            ), "no terminaron")
        etiquetas <- paste (etiquetas, porcentaje, sep = "\n")
        etiquetas <- paste (etiquetas, "%", sep = "")
        pie (
            c(aprobados, total),
            labels = etiquetas,
            col = c("green", "red"),
            main = paste (
                paste (
                    "Estudiantes que finalizaron vs Estudiantes que no finalizaron\n - Cohorte",
                    anio_cohorte,
                    sep = " "
                ),
                anio_retraso,
                sep = " . Años de retraso: "
            )
        )
    }


##replicamos el proceso para la cohorte 2011
alumnos_terminan_cada_rama_2011 <-
    obtener_legajos_alumnos_terminan_cada_rama(ramas_correlatividades, cohorte_2011, 0)
alumnos_terminan_2011 <-
    obtener_legajos_alumnos_terminan(alumnos_terminan_cada_rama_2011)

graficar_torta (alumnos_terminan_2011, cohorte_2011, 0, "2011")

alumnos_terminan_2011
obtener_probabilidad_de_terminar(alumnos_terminan_2011, cohorte_2011)

# alumnos 2012

alumnos_terminan_cada_rama_2012 <-
    obtener_legajos_alumnos_terminan_cada_rama(ramas_correlatividades, cohorte_2012, 0)
alumnos_terminan_2012 <-
    obtener_legajos_alumnos_terminan(alumnos_terminan_cada_rama_2012)

graficar_torta (alumnos_terminan_2012, cohorte_2012, 0, "2012")

alumnos_terminan_2012
obtener_probabilidad_de_terminar(alumnos_terminan_2012, cohorte_2012)

# alumnos 2013

alumnos_terminan_cada_rama_2013 <-
    obtener_legajos_alumnos_terminan_cada_rama(ramas_correlatividades, cohorte_2013, 0)
alumnos_terminan_2013 <-
    obtener_legajos_alumnos_terminan(alumnos_terminan_cada_rama_2013)

graficar_torta (alumnos_terminan_2013, cohorte_2013, 0, "2013")

alumnos_terminan_2013
obtener_probabilidad_de_terminar(alumnos_terminan_2013, cohorte_2013)

# alumnos 2014

alumnos_terminan_cada_rama_2014 <-
    obtener_legajos_alumnos_terminan_cada_rama(ramas_correlatividades, cohorte_2014, 0)
alumnos_terminan_2014 <-
    obtener_legajos_alumnos_terminan(alumnos_terminan_cada_rama_2014)

graficar_torta (alumnos_terminan_2014, cohorte_2014, 0, "2014")

alumnos_terminan_2014
obtener_probabilidad_de_terminar(alumnos_terminan_2014, cohorte_2014)

# alumnos 2015

alumnos_terminan_cada_rama_2015 <-
    obtener_legajos_alumnos_terminan_cada_rama(ramas_correlatividades, cohorte_2015, 0)
alumnos_terminan_2015 <-
    obtener_legajos_alumnos_terminan(alumnos_terminan_cada_rama_2015)

graficar_torta (alumnos_terminan_2015, cohorte_2015, 0, "2015")

alumnos_terminan_2015
obtener_probabilidad_de_terminar(alumnos_terminan_2015, cohorte_2015)





#-------------------- Analisis de cada materia en particular --------------------

#----- Funcion para obtener cuantos aprueban, promocionan, reprueban, y están ausentes o abandonaron por materia -----

obtener_resultados_cursadas_por_materia <-
    function(dataset_anio,
             anios_retraso_considerados)
    {
        resultados <- list()
        
        for (i in 1:length(nombre_materias_obligatorias))
        {
            resultados[[i]] <-
                dataset_anio %>%
                group_by(resultado) %>%
                filter((materia == nombre_materias_obligatorias[[i]]) &
                           (
                               meses_transcurridos_para_regularizar <= meses_requeridos_para_regularizar + 12 * anios_retraso_considerados
                           )
                ) %>%
                distinct(legajoT) %>%
                count(resultado)
        }
        names(resultados) <- names(nombre_materias_obligatorias)
        
        resultados
    }

#----- Funcion para graficar cuantos aprueban, promocionan, reprueban, y están ausentes o abandonaron una materia en particular -----

graficar_resultado_cursada <- function(resultados, anio, materia)
{
    df <- data.frame(x = resultados[materia])
    names(df) <- c("resultado", "n")
    ggplot(data = df, aes(x = resultado, y = n)) + geom_bar(stat = "identity",
                                                            color = "blue",
                                                            fill = "white") + scale_y_continuous(limits = c(0, 100)) + ggtitle (paste ("Resultados", paste (anio, materia, sep = " - "), sep = " "))
}

#----- Funcion para obtener la probabilidad acorde a un/varios resultado/s (aprobar, reprobar, promocionar, ausente o abandono) por materia -----

calcular_probabilidades_por_materia <-
    function (cohorte,
              anios_retraso_considerados,
              resultados_considerados)
    {
        probabilidades_por_materia <- c()
        
        for (i in 1:length (nombre_materias_obligatorias))
        {
            #--- Se obtiene el total de alumnos que cursaron la materia ---
            
            total <-
                cohorte %>%
                group_by(resultado) %>%
                filter((materia == nombre_materias_obligatorias[[i]]) &
                           (
                               meses_transcurridos_para_regularizar <= meses_requeridos_para_regularizar + 12 * anios_retraso_considerados
                           )
                ) %>%
                distinct(legajoT) %>%
                nrow()
            
            #--- Se obtienen los que cumplen el/los resultado/s considerado/s ---
            
            cumplen_resultado <-
                cohorte %>%
                group_by(resultado) %>%
                filter((materia == nombre_materias_obligatorias[[i]]) &
                           resultado %in% resultados_considerados &
                           (
                               meses_transcurridos_para_regularizar <= meses_requeridos_para_regularizar + 12 * anios_retraso_considerados
                           )
                ) %>%
                distinct(legajoT) %>%
                nrow()
            
            if (total == 0)
            {
                probabilidades_por_materia[i] <- 0
            }
            else
            {
                probabilidades_por_materia[i] <- cumplen_resultado / total
            }
            
        }
        names(probabilidades_por_materia) <-
            names(nombre_materias_obligatorias)
        
        probabilidades_por_materia
    }

#agregamos resultados de la cohorte 2011
resultados_2011 <-
    obtener_resultados_cursadas_por_materia(cohorte_2011, 0)
resultados_2012 <-
    obtener_resultados_cursadas_por_materia(cohorte_2012, 0)
resultados_2013 <-
    obtener_resultados_cursadas_por_materia(cohorte_2013, 0)
resultados_2014 <-
    obtener_resultados_cursadas_por_materia(cohorte_2014, 0)
resultados_2015 <-
    obtener_resultados_cursadas_por_materia(cohorte_2015, 0)

# graficar_resultado_cursada(resultados_2011, "2011", "Algebra I")
# graficar_resultado_cursada(resultados_2012, "2012", "Algebra I")
# graficar_resultado_cursada(resultados_2013, "2013", "Algebra I")


##agregamos la cohorte 2011
probabilidades_por_materia_2011 <-
    calcular_probabilidades_por_materia(cohorte_2011, 0, c("R"))
probabilidades_por_materia_2011

probabilidades_por_materia_2012 <-
    calcular_probabilidades_por_materia(cohorte_2012, 0, c("R"))
probabilidades_por_materia_2012

probabilidades_por_materia_2013 <-
    calcular_probabilidades_por_materia(cohorte_2013, 0, c("R"))
probabilidades_por_materia_2013

probabilidades_por_materia_2014 <-
    calcular_probabilidades_por_materia(cohorte_2014, 0, c("R"))
probabilidades_por_materia_2014

probabilidades_por_materia_2015 <-
    calcular_probabilidades_por_materia(cohorte_2015, 0, c("R"))
probabilidades_por_materia_2015
