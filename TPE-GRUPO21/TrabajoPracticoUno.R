## ============================================================
## CIENCIA DE DATOS PARA CIUDADES
## TP especial - Movilidad y tiempos de viaje
## Dataset: "Viajes origen destino optativa Exactas.xlsx"
## Lenguaje: R (Corregido y Optimizado)
## ============================================================

# 1) Paquetes necesarios --------------------------------------
library(readxl)   # leer Excel
library(dplyr)    # manipular datos
library(janitor)  # limpiar nombres de columnas
library(ggplot2)  # gráficos (opcional)
library(tidyr)    # Requerido para el heatmap (se movió al inicio)

# 2) Carga y limpieza del dataset -----------------------------
ruta_archivo <- ("Viajes origen destino optativa Exactas.xlsx")

viajes <- read_excel(ruta_archivo) %>%
  clean_names()

# 3) TABLAS DE TIEMPO PROMEDIO Y TTR ---------------------------

# 3.1) Tiempo promedio de viaje por persona y motivo (Individual)
tiempo_promedio_persona_motivo <- viajes %>%
  group_by(identificacion, motivo_del_viaje) %>%
  summarise(
    promedio_persona_actividad = mean(tiempo_de_duracion_del_viaje_en_minutos,
                                      na.rm = TRUE),
    .groups = "drop"
  )

# 3.2) Tiempo promedio de viaje general por motivo (Global)
tiempo_promedio_general_motivo <- viajes %>%
  group_by(motivo_del_viaje) %>%
  summarise(
    tiempo_promedio_min = mean(tiempo_de_duracion_del_viaje_en_minutos,
                               na.rm = TRUE),
    .groups = "drop" # Usar .groups = "drop" en vez de ungroup()
  ) %>%
  mutate(
    tiempo_promedio_min = round(tiempo_promedio_min, 2)
  ) %>%
  arrange(desc(tiempo_promedio_min))

# 3.3) Cálculo de TTR simple
tiempo_promedio_total <- mean(viajes$tiempo_de_duracion_del_viaje_en_minutos,
                               na.rm = TRUE)

ttr_por_motivo <- tiempo_promedio_general_motivo %>%
  mutate(
    TTR = tiempo_promedio_min / tiempo_promedio_total,
    TTR = round(TTR, 3)
  ) %>%
  arrange(desc(TTR))

# Mostrar las tablas principales
tiempo_promedio_persona_motivo
tiempo_promedio_general_motivo
ttr_por_motivo


# 4) OTRAS MÉTRICAS QUE PIDEN EN LAS PAUTAS -------------------

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

## 4.3 Viajes laborales 
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


# 5) Gráficos --------------------------

# 5.1 Gráfico de barras (box plot original) - USANDO LA TABLA GLOBAL
# Uso 'tiempo_promedio_general_motivo' ya que tiene la columna 'tiempo_promedio_min'
ggplot(tiempo_promedio_general_motivo,
       aes(x = reorder(motivo_del_viaje, tiempo_promedio_min),
           y = tiempo_promedio_min)) +
  geom_col() +
  coord_flip() +
  labs(
    x = "Motivo del viaje",
    y = "Tiempo promedio de viaje (minutos)",
    title = "Tiempo promedio de viaje por motivo (Global)"
  ) +
  theme_minimal()

# 5.2 Gráfico de barras de cantidad de viajes
# Uso 'viajes_por_motivo' que ya fue creado en 4.2
ggplot(viajes_por_motivo,
       aes(x = reorder(motivo_del_viaje, cantidad_viajes), y = cantidad_viajes)) + # CORREGIDO: Usar 'cantidad_viajes'
  geom_col(fill = "steelblue") +
  coord_flip() +
  labs(
    title = "Cantidad de viajes por motivo",
    x = "Motivo del viaje",
    y = "Cantidad de viajes"
  ) +
  theme_minimal()

# 5.3 Histograma de la distribución general
ggplot(viajes, aes(x = tiempo_de_duracion_del_viaje_en_minutos)) +
  geom_histogram(bins = 30, fill = "skyblue", color = "black") +
  labs(
    title = "Distribución general de tiempos de viaje",
    x = "Tiempo de viaje (min)",
    y = "Frecuencia"
  ) +
  theme_minimal()

# 5.4 Box plot por tipo de dia
ggplot(viajes,
       aes(x = tipo_de_dia,
           y = tiempo_de_duracion_del_viaje_en_minutos,
           fill = tipo_de_dia)) +
  geom_boxplot(show.legend = FALSE) +
  labs(
    title = "Tiempos de viaje según tipo de día",
    x = "Tipo de día",
    y = "Duración del viaje (min)"
  ) +
  theme_minimal()

# 5.5 Heatmap: tiempo promedio por motivo y tipo de dia
# Usamos 'tiempo_promedio_motivo_tipo_dia' que ya fue creada
ggplot(tiempo_promedio_motivo_tipo_dia, # CORREGIDO: Usar la tabla ya existente
       aes(x = motivo_del_viaje, y = tipo_de_dia, fill = tiempo_promedio_min)) +
  geom_tile(color = "white") +
  scale_fill_gradient(low = "lightblue", high = "darkblue") +
  coord_flip() +
  labs(
    title = "Mapa de calor del tiempo promedio por motivo y tipo de día",
    x = "Motivo",
    y = "Tipo de día",
    fill = "Promedio (min)"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# 5.6 Scatterplot: hora de inicio vs tiempo de viaje
ggplot(viajes,
       aes(x = hora_inicio, y = tiempo_de_duracion_del_viaje_en_minutos)) +
  geom_point(alpha = 0.2, color = "steelblue") +
  labs(
    title = "Relación entre hora de inicio y duración del viaje",
    x = "Hora de inicio",
    y = "Duración (min)"
  ) +
  theme_minimal()