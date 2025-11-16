## ============================================================
## CIENCIA DE DATOS PARA CIUDADES – TP3
## Escenarios y Emociones
## Dataset: "Tercer Practico Escenarios.xlsx"
## Lenguaje: R
## ============================================================

# 1) Paquetes --------------------------------------------------------------
library(readxl)
library(dplyr)
library(janitor)
library(stringr)
library(tidyr)
library(ggplot2)
library(fmsb)

# 2) Carga del dataset -----------------------------------------------------
escenarios <- read_excel("Tercer Practico Escenarios.xlsx") %>%
  clean_names()

# 2.1 Mantener solo valores numéricos reales en a_gusto --------------------
escenarios <- escenarios %>%
  mutate(a_gusto_num = suppressWarnings(as.numeric(a_gusto))) %>%
  filter(!is.na(a_gusto_num)) %>%
  select(-a_gusto, a_gusto = a_gusto_num)

# 3) Clasificación de escenarios ------------------------------------------

escenarios <- escenarios %>%
  mutate(

    escenario = case_when(

      ## Actividades de deber
      etiqueta_actividad_resumida %in% c("trámites", "trabajo", "salud", "estudio") ~ "Deber",

      ## Ocio/Recreación en el hogar
      lugar_de_actividad == "hogar" &
        etiqueta_actividad_resumida %in% c("entretenimiento", "dedicación personal") ~ "Ocio en el Hogar",

      ## Socialización
      etiqueta_interaccion %in% c("familia", "amigos") ~ "Socialización",

      ## Relaciones laborales
      etiqueta_interaccion == "Con compañeros de trabajo" ~ "Relaciones Laborales",

      TRUE ~ "Otro"
    )
  )

# 4) Lista de emociones -----------------------------------------------------

emociones <- c("preocupacion", "prisa", "depresion",
               "tension", "calma", "disfrute")

# 5) Calcular ponderación por tiempo ---------------------------------------

escenarios <- escenarios %>%
  mutate(across(all_of(emociones),
                ~ .x * total_en_minutos_de_la_actividad,
                .names = "pond_{col}"))

# 6) Agregar por persona y escenario ---------------------------------------

resultados <- escenarios %>%
  group_by(identificacion, escenario) %>%

  summarise(
    ## Tiempo total por escenario
    tiempo_total = sum(total_en_minutos_de_la_actividad, na.rm = TRUE),

    ## Ponderaciones totales por emoción
    across(starts_with("pond_"), ~ sum(.x, na.rm = TRUE)),


    .groups = "drop"
  ) %>%

  ## Convertir ponderaciones totales en promedios ponderados
  mutate(across(starts_with("pond_"),
                ~ ifelse(tiempo_total > 0, .x / tiempo_total, NA),
                .names = "prom_{col}"))

# 7) Manejar "A gusto" (solo socialización y relaciones laborales) ---------

a_gusto_resumen <- escenarios %>%
  filter(escenario %in% c("Socialización", "Relaciones Laborales")) %>%
  group_by(identificacion, escenario) %>%
  summarise(
    a_gusto_prom_tiempo =
      sum(a_gusto * total_en_minutos_de_la_actividad, na.rm = TRUE) /
      sum(total_en_minutos_de_la_actividad, na.rm = TRUE),
    .groups = "drop"
  )

# 8) Unir todas las métricas ----------------------------------------------

tabla_final <- resultados %>%
  left_join(a_gusto_resumen,
            by = c("identificacion", "escenario"))

# 9) Guardar resultado ------------------------------------------------------

write.csv(tabla_final,
          "TP3_Promedios_Emociones_por_Persona_y_Escenario.csv",
          row.names = FALSE)

# 10) Gráficos -------------------------------------------------------------

## Boxplot de disfrute
ggplot(tabla_final %>% filter(escenario != "Otro"),
       aes(x = escenario, y = prom_pond_disfrute, color = escenario)) +
  geom_boxplot() +
  theme_minimal() +
  labs(
    title = "Promedio ponderado de Disfrute por Escenario",
    x = "Escenario",
    y = "Disfrute ponderado"
  )

## Heatmap emocional
heatmap_data <- tabla_final %>%
  select(identificacion, escenario, starts_with("prom_pond_")) %>%
  pivot_longer(cols = starts_with("prom_"), 
               names_to = "emocion", 
               values_to = "valor") %>%
  mutate(emocion = str_remove(emocion, "prom_pond_"))

ggplot(heatmap_data, aes(x = emocion, y = escenario, fill = valor)) +
  geom_tile(color = "white") +
  scale_fill_gradient(low = "#a7d8f0", high = "#034f84") +
  theme_minimal() +
  labs(
    title = "Mapa de calor del promedio emocional por escenario",
    x = "Emoción",
    y = "Escenario"
  ) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

## Violin plot
ggplot(heatmap_data, aes(x = escenario, y = valor, fill = escenario)) +
  geom_violin(alpha = 0.7, trim = FALSE) +
  facet_wrap(~emocion, scales = "free_y") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45)) +
  labs(
    title = "Distribución de emociones ponderadas por escenario",
    x = "Escenario",
    y = "Intensidad ponderada"
  )

## Radar Chart -------------------------------------------------------------
radar_data <- tabla_final %>%
  group_by(escenario) %>%
  summarise(across(starts_with("prom_pond_"), mean, na.rm = TRUE))

colnames(radar_data) <- str_replace(colnames(radar_data), "prom_pond_", "")

max_val <- apply(radar_data[,-1], 2, max)
min_val <- apply(radar_data[,-1], 2, min)

radar_plot <- rbind(max_val, min_val, radar_data[,-1]) %>%
  as.data.frame()

rownames(radar_plot) <- c("max","min", radar_data$escenario)


radarchart(radar_plot,
           pcol = rainbow(nrow(radar_data)),
           pfcol = scales::alpha(rainbow(nrow(radar_data)), .3),
           plwd = 3,
           cglcol = "grey",
           title = "Perfil emocional por escenario")

## Boxplot de A gusto ------------------------------------------------------

a_gusto_plot <- escenarios %>%
  filter(escenario %in% c("Socialización", "Relaciones Laborales")) %>%
  mutate(a_gusto_pond = a_gusto * total_en_minutos_de_la_actividad)

ggplot(a_gusto_plot, aes(x = escenario, y = a_gusto_pond, fill = escenario)) +
  geom_boxplot() +
  theme_minimal() +
  labs(
    title = "'A gusto' ponderado por tiempo",
    x = "Escenario",
    y = "A gusto ponderado"
  )

## ============================================================
## FIN DEL SCRIPT
## ============================================================
