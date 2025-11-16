## ============================================================
## CIENCIA DE DATOS PARA CIUDADES
## TP especial – Visualizaciones creativas (TP2)
## Dataset 1: "Viajes origen destino optativa Exactas.xlsx"
## Dataset 2: Personas
## Lenguaje: R
## ============================================================

# 1) Paquetes necesarios -------------------------------------------
library(readxl)
library(readr)
library(dplyr)
library(janitor)
library(ggplot2)
library(stringr)
library(tidyr)

# 2) Cargar datasets -------------------------------------------------

## 2.1 Dataset de viajes
viajes <- read_excel("Viajes origen destino optativa Exactas.xlsx") %>%
  clean_names()

## 2.2 Dataset de personas
personas <- read_csv("Base de datos Personas Exactas.csv") %>%
  clean_names()

# Ver estructura
glimpse(viajes)
glimpse(personas)

# 3) Unir datasets por Identificación -------------------------------

viajes_personas <- viajes %>%
  left_join(personas, by = "identificacion")

# Revisar unión
summary(viajes_personas)

# ============================================================
# 4) Crear variables necesarias para el análisis ---------------------
# ============================================================

# 4.1 Crear un rango etario (ejemplo)
viajes_personas <- viajes_personas %>%
  mutate(
    rango_etario = case_when(
      edad >= 15 & edad <= 24 ~ "15-24",
      edad >= 25 & edad <= 39 ~ "25-39",
      edad >= 40 & edad <= 59 ~ "40-59",
      edad >= 60 ~ "60+",
      TRUE ~ NA_character_
    )
  )

# 4.2 Chequear ocupación, zona, etc.
table(viajes_personas$ocupacion)
table(viajes_personas$zona_residencia)

# ============================================================
# 5) MÉTRICA PRINCIPAL TP2:
#    "Promedio de actividades por al menos 3 variables"
# ============================================================

# En este contexto, "actividades" = "viajes realizados" (actividad registrada)

promedio_actividades <- viajes_personas %>%
  group_by(zona_residencia, rango_etario, ocupacion) %>%
  summarise(
    cantidad_actividades = n(),
    tiempo_promedio_viaje = mean(tiempo_de_duracion_del_viaje_en_minutos, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  arrange(desc(cantidad_actividades))

promedio_actividades

# ============================================================
# 6) VISUALIZACIONES CREATIVAS (mínimo 3 variables)
# ============================================================

# ▼ Gráfico 1: mapa de calor (heatmap) actividad promedio
ggplot(promedio_actividades,
       aes(x = rango_etario,
           y = zona_residencia,
           fill = cantidad_actividades)) +
  geom_tile(color = "white") +
  scale_fill_gradient(low = "#a1c4fd", high = "#0d47a1") +
  labs(
    title = "Promedio de actividades por Zona + Rango Etario + Ocupación",
    subtitle = "Cada celda representa cantidad de viajes promedio",
    x = "Rango Etario",
    y = "Zona de Residencia",
    fill = "Cant. Actividades"
  ) +
  facet_wrap(~ ocupacion) +
  theme_minimal()

# ▼ Gráfico 2: barras apiladas por ocupación
ggplot(promedio_actividades,
       aes(x = zona_residencia,
           y = cantidad_actividades,
           fill = rango_etario)) +
  geom_col(position = "stack") +
  facet_wrap(~ ocupacion) +
  labs(
    title = "Cantidad de actividades por zona, edad y ocupación",
    x = "Zona de Residencia",
    y = "Cantidad de Actividades"
  ) +
  theme_minimal()

# ▼ Gráfico 3: tiempo promedio por actividad según 3 variables
ggplot(promedio_actividades,
       aes(x = ocupacion,
           y = tiempo_promedio_viaje,
           color = rango_etario,
           size = cantidad_actividades)) +
  geom_point(alpha = 0.7) +
  facet_wrap(~ zona_residencia) +
  labs(
    title = "Relación entre tiempo promedio de viaje, edad y ocupación por zona",
    x = "Ocupación",
    y = "Tiempo promedio de viaje (min)"
  ) +
  theme_minimal()

# ============================================================
# 7) TABLA FINAL RESUMEN PARA INFORME
# ============================================================

write.csv(promedio_actividades,
          "Promedio_actividades_por_zona_edad_ocupacion.csv",
          row.names = FALSE)

# ============================================================
# FIN DEL SCRIPT
# ============================================================
