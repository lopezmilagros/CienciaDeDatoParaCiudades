## ============================================================
## CIENCIA DE DATOS PARA CIUDADES
## TP especial - Movilidad y tiempos de viaje
## Dataset: "Viajes origen destino optativa Exactas.xlsx"
## Lenguaje: R
## ============================================================

# 1) Paquetes necesarios --------------------------------------
library(readxl)   # leer Excel
library(dplyr)    # manipular datos
library(janitor)  # limpiar nombres de columnas
library(ggplot2)  # gráficos (opcional)

# 2) Carga y limpieza del dataset -----------------------------
ruta_archivo <- ("Viajes origen destino optativa Exactas.xlsx")

viajes <- read_excel(ruta_archivo) %>%
  clean_names()
# columnas principales después de clean_names():
# identificacion
# zona_residencia
# zona_origen
# zona_destino
# hora_inicio
# hora_fin
# tiempo_de_duracion_del_viaje_en_minutos
# motivo_del_viaje
# movil
# dia
# tipo_de_dia
# lugar_semantico_de_origen
# lugar_semantico_de_destino


# 3) TABLA PRINCIPAL DEL TP:
#    Tiempo promedio de viaje por motivo ----------------------

tabla_tiempo_promedio_motivo <- viajes %>%
  group_by(motivo_del_viaje) %>%
  summarise(
    tiempo_promedio_min = mean(tiempo_de_duracion_del_viaje_en_minutos,
                               na.rm = TRUE)
  ) %>%
  ungroup() %>%
  mutate(
    tiempo_promedio_min = round(tiempo_promedio_min, 6)
  ) %>%
  arrange(desc(tiempo_promedio_min))

tabla_tiempo_promedio_motivo
# -> Esta tabla debería verse como la de tu captura:
# motivo_del_viaje | tiempo_promedio_min (ordenada de mayor a menor)


# 4) OTRAS MÉTRICAS QUE PIDEN EN LAS PAUTAS -------------------
# (las dejás como tablas complementarias en el informe)

## 4.1 Cantidad de viajes por tipo de día
viajes_por_tipo_dia <- viajes %>%
  count(tipo_de_dia, name = "cantidad_viajes") %>%
  arrange(desc(cantidad_viajes))

viajes_por_tipo_dia

## 4.2 Cantidad de viajes por motivo
viajes_por_motivo <- viajes %>%
  count(motivo_del_viaje, name = "cantidad_viajes") %>%
  arrange(desc(cantidad_viajes))

viajes_por_motivo

## 4.3 Viajes laborales (en línea con el paper Trabajo/Ocio)
viajes_laborales <- viajes %>%
  filter(motivo_del_viaje %in% c("Al Trabajo", "Por Trabajo"))

# cantidad de viajes laborales por tipo de día
viajes_laborales_por_tipo <- viajes_laborales %>%
  count(tipo_de_dia, name = "cantidad_viajes_laborales")

viajes_laborales_por_tipo

# tiempo promedio de viaje para motivos laborales
tiempo_promedio_laboral <- viajes_laborales %>%
  summarise(
    tiempo_promedio_laboral_min =
      mean(tiempo_de_duracion_del_viaje_en_minutos, na.rm = TRUE)
  )

tiempo_promedio_laboral

## 4.4 Tiempo promedio por motivo y tipo de día
tiempo_promedio_motivo_tipo_dia <- viajes %>%
  group_by(tipo_de_dia, motivo_del_viaje) %>%
  summarise(
    tiempo_promedio_min = mean(tiempo_de_duracion_del_viaje_en_minutos,
                               na.rm = TRUE),
    .groups = "drop"
  ) %>%
  arrange(tipo_de_dia, desc(tiempo_promedio_min))

tiempo_promedio_motivo_tipo_dia


# 5) Gráfico ejemplo para el informe --------------------------
ggplot(tabla_tiempo_promedio_motivo,
       aes(x = reorder(motivo_del_viaje, tiempo_promedio_min),
           y = tiempo_promedio_min)) +
  geom_col() +
  coord_flip() +
  labs(
    x = "Motivo del viaje",
    y = "Tiempo promedio de viaje (minutos)",
    title = "Tiempo promedio de viaje por motivo"
  ) +
  theme_minimal()

## ============================================================
## Notas para el informe:
## - La tabla 'tabla_tiempo_promedio_motivo' es la que replica
##   exactamente el ejemplo de la consigna.
## - Con estas tablas podés discutir, usando el paper, cómo
##   ciertas actividades (trabajo, estudio, ocio, etc.) implican
##   distintos "esfuerzos de viaje" y cómo eso se relaciona con
##   la calidad de vida urbana.
## ============================================================



