
# analysis.R
# Script to compute average travel times per person/activity and Travel-Time Ratio (TTR)
# Assumptions:
# - Activity duration is approximated as the time between arrival (HoraFin) of a trip
#   and the next departure (HoraInicio) for the same person on the same day.
# - Times are in HH:MM (time of day). If next trip is on a different day or missing, activity duration = NA.
# - TTR = travel_time / (travel_time + activity_duration) computed only where activity_duration > 0.
#
# Usage: set working directory to where 'TrabajoPractico1_Viajes.xlsx' is located and run this script in R.
# Requires packages: readxl, dplyr, lubridate, ggplot2, tidyr, knitr, rmarkdown

library(readxl)
library(dplyr)
library(lubridate)
library(ggplot2)
library(tidyr)
library(knitr)
library(rmarkdown)

# Load data
df <- read_excel("TrabajoPractico1_Viajes.xlsx")

# Clean column names for convenience
names(df) <- make.names(names(df))

# Convert times to POSIXct using an arbitrary date (we only need differences within a day)
# If HoraInicio or HoraFin are already times, lubridate will handle them
df <- df %>%
  mutate(HoraInicio = as.POSIXct(HoraInicio, format="%H:%M:%S", tz="UTC"),
         HoraFin = as.POSIXct(HoraFin, format="%H:%M:%S", tz="UTC"),
         travel_minutes = as.numeric(`Tiempo.de.duración.del.viaje.en.minutos`))

# If conversion produced NA because times are stored as time objects, try alternative coercion
if(all(is.na(df$HoraInicio)) && any(!is.na(df$HoraFin))) {
  # try parsing as hms strings
  df$HoraInicio <- parse_date_time(as.character(df$HoraInicio), orders = c("HMS","HM"))
  df$HoraFin <- parse_date_time(as.character(df$HoraFin), orders = c("HMS","HM"))
}

# Order trips by person and day/time to compute next trip times
df <- df %>% arrange(Identificación, Día, HoraInicio)

# Compute activity duration as time between arrival (HoraFin) and next departure for same person & day
df <- df %>%
  group_by(Identificación, Día) %>%
  mutate(next_HoraInicio = lead(HoraInicio),
         activity_duration_mins = as.numeric(difftime(next_HoraInicio, HoraFin, units="mins"))) %>%
  ungroup()

# Some activity durations may be negative (if data out-of-order); set negatives to NA
df <- df %>% mutate(activity_duration_mins = ifelse(activity_duration_mins <= 0, NA, activity_duration_mins))

# Compute TTR where possible
df <- df %>% mutate(TTR = ifelse(!is.na(activity_duration_mins) & (activity_duration_mins + travel_minutes) > 0,
                                 travel_minutes / (travel_minutes + activity_duration_mins), NA))

# Summary statistics overall and by Motivo del Viaje
overall_summary <- df %>% summarise(
  n_trips = n(),
  mean_travel_min = mean(travel_minutes, na.rm=TRUE),
  median_travel_min = median(travel_minutes, na.rm=TRUE),
  mean_activity_min = mean(activity_duration_mins, na.rm=TRUE),
  mean_TTR = mean(TTR, na.rm=TRUE)
)

by_motive <- df %>% group_by(Motivo.del.Viaje) %>% summarise(
  trips = n(),
  mean_travel = mean(travel_minutes, na.rm=TRUE),
  median_travel = median(travel_minutes, na.rm=TRUE),
  mean_activity = mean(activity_duration_mins, na.rm=TRUE),
  mean_TTR = mean(TTR, na.rm=TRUE)
) %>% arrange(desc(trips))

# Per-person average travel time by motive
person_motive <- df %>% group_by(Identificación, Motivo.del.Viaje) %>% summarise(
  trips = n(),
  avg_travel = mean(travel_minutes, na.rm=TRUE),
  avg_activity = mean(activity_duration_mins, na.rm=TRUE),
  avg_TTR = mean(TTR, na.rm=TRUE)
) %>% ungroup()

# Save summaries
write.csv(overall_summary, "overall_summary.csv", row.names=FALSE)
write.csv(by_motive, "by_motive_summary.csv", row.names=FALSE)
write.csv(person_motive, "person_motive_summary.csv", row.names=FALSE)

# Visualizations
# 1. Distribution of travel times
p1 <- ggplot(df, aes(x=travel_minutes)) + geom_histogram(bins=50) + ggtitle("Distribución de tiempos de viaje (minutos)")
ggsave("hist_travel_time.png", p1, width=8, height=4)

# 2. Boxplot of travel time by motive (top 8 motives)
top_motives <- by_motive$Motivo.del.Viaje[1:8]
p2 <- df %>% filter(Motivo.del.Viaje %in% top_motives) %>%
  ggplot(aes(x=reorder(Motivo.del.Viaje, travel_minutes, FUN=median), y=travel_minutes)) +
  geom_boxplot() + coord_flip() + ggtitle("Tiempos de viaje por motivo (top 8)")
ggsave("box_travel_by_motive.png", p2, width=8, height=6)

# 3. TTR distribution
p3 <- ggplot(df %>% filter(!is.na(TTR)), aes(x=TTR)) + geom_histogram(bins=50) + ggtitle("Distribución de TTR")
ggsave("hist_TTR.png", p3, width=8, height=4)

# 4. Mean TTR by Motive
p4 <- by_motive %>% filter(!is.na(mean_TTR)) %>%
  ggplot(aes(x=reorder(Motivo.del.Viaje, mean_TTR), y=mean_TTR)) + geom_col() + coord_flip() + ggtitle("Mean TTR por motivo")
ggsave("mean_TTR_by_motive.png", p4, width=8, height=6)

# Optionally render an RMarkdown report (report.Rmd should be in the same folder)
if(file.exists("report.Rmd")) {
  render("report.Rmd", output_file = "Informe_Viajes_report.html")
}

cat("Script finished. Files produced:\n")
cat(" overall_summary.csv, by_motive_summary.csv, person_motive_summary.csv\n")
cat(" hist_travel_time.png, box_travel_by_motive.png, hist_TTR.png, mean_TTR_by_motive.png\n")
