# Load libraries
library(tidyverse)

# Setting
setwd("C:/Users/tiger/OneDrive/桌面/IJC445 Data Visualisation/Project/Data")


# SECTION 1: DATA LOADING AND PREPARATION

# Read data
greenwich <- read_csv("Greenwich - Westhorne Avenue PM2.5&NO2.csv", show_col_types = FALSE)
harlington <- read_csv("London Harlington PM2.5&NO2.csv", show_col_types = FALSE)
westminster <- read_csv("London Westminster PM2.5&NO2.csv", show_col_types = FALSE)
elizabeth <- read_csv("Westminster - Elizabeth Bridge PM2.5&NO2.csv", show_col_types = FALSE)

# Add station identifiers
greenwich$station <- "Greenwich"
harlington$station <- "Harlington"
westminster$station <- "Westminster"
elizabeth$station <- "Elizabeth Bridge"

# Combine all datasets
all_data <- bind_rows(greenwich, harlington, westminster, elizabeth)

# Parse datetime and extract hour
all_data$datetime <- as.POSIXct(all_data$datetimeUtc, format = "%Y-%m-%d %H:%M:%S")
all_data$hour <- as.numeric(format(all_data$datetime, "%H"))

# Remove invalid data
all_data <- all_data[!is.na(all_data$value) & all_data$value > 0, ]

# Define rush hour periods
all_data <- all_data %>%
  mutate(period = ifelse(hour %in% c(7:9, 17:19), "Rush Hour", "Off-Peak"))


# SECTION 2: THEME SETTINGS


my_theme <- theme_minimal(base_size = 11) +
  theme(legend.position = "top")

# FIGURE 1: Hourly Variation of PM2.5 and NO2

hourly <- all_data %>%
  group_by(hour, parameter) %>%
  summarise(mean_value = mean(value, na.rm = TRUE), .groups = "drop")

fig1 <- ggplot(hourly, aes(x = hour, y = mean_value, color = parameter)) +
  geom_hline(yintercept = c(10, 5), linetype = "dashed", color = "gray60", linewidth = 0.5) +
  geom_vline(xintercept = c(7, 9, 17, 19), linetype = "dotted", color = "gray80", linewidth = 0.4) +
  geom_line(linewidth = 1) +
  geom_point(size = 2) +
  annotate("text", x = 23, y = 10.5, label = "WHO NO2 guideline (10 µg/m³)",
           color = "gray30", size = 3, hjust = 1) +
  annotate("text", x = 23, y = 5.5, label = "WHO PM2.5 guideline (5 µg/m³)",
           color = "gray30", size = 3, hjust = 1) +
  scale_color_manual(
    values = c("no2" = "steelblue", "pm25" = "darkorange"),
    labels = c("NO2", "PM2.5"),
    name   = "Pollutant"
  ) +
  scale_x_continuous(
    breaks = c(0, 6, 12, 18, 24),
    labels = c("00:00", "06:00", "12:00", "18:00", "24:00"),
    expand = c(0.02, 0.02)
  ) +
  scale_y_continuous(limits = c(0, 40), expand = c(0, 0)) +
  labs(
    title    = " Figure1: Hourly Variation of PM2.5 and NO2 ",
    subtitle = "Average pollution levels across four London stations (March 2025)",
    x = "Time of Day",
    y = "Concentration (µg/m³)",
    caption = "Dashed = WHO annual mean (NO2 10 µg/m³; PM2.5 5 µg/m³). Dotted = Rush hours (7–9, 17–19)."
  ) +
  my_theme

print(fig1)
ggsave("Figure1_final.png", fig1, width = 11, height = 6.5, dpi = 300)


# FIGURE 2: Mean Pollution Levels by Station

station_summary <- all_data %>%
  group_by(station, parameter) %>%
  summarise(mean_value = mean(value, na.rm = TRUE), .groups = 'drop')

fig2 <- ggplot(station_summary, aes(x = station, y = mean_value, fill = parameter)) +
  geom_col(position = position_dodge(0.8), width = 0.7) +
  scale_fill_manual(
    values = c("no2" = "steelblue", "pm25" = "darkorange"),
    labels = c("NO2", "PM2.5"),
    name = "Pollutant"
  ) +
  scale_x_discrete(
    labels = c(
      "Elizabeth Bridge" = "Elizabeth\nBridge",
      "Greenwich"        = "Greenwich",
      "Harlington"       = "Harlington",
      "Westminster"      = "Westminster"
    )
  ) +
  scale_y_continuous(expand = c(0, 0), limits = c(0, max(station_summary$mean_value) * 1.1)) +
  labs(
    title = " Figure2: Mean Pollution Levels by Station ",
    subtitle = "Average PM2.5 and NO2 levels recorded at four monitoring sites",
    x = NULL,
    y = "Concentration (µg/m³)"
  ) +
  my_theme
print(fig2)
ggsave("Figure2_final.png", fig2, width = 11, height = 6.5, dpi = 300)

# FIGURE 3: 24-Hour Variation of PM2.5 by Station

pm25_only <- all_data %>% filter(parameter == "pm25")

pm25_hourly <- pm25_only %>%
  group_by(hour, station) %>%
  summarise(mean_pm25 = mean(value, na.rm = TRUE), .groups = "drop")

fig3 <- ggplot(pm25_hourly, aes(x = hour, y = mean_pm25, color = station, group = station)) +
  geom_line(linewidth = 1.2) +
  geom_point(size = 2) +
  coord_polar(start = -pi/2) +
  scale_color_manual(
    values = c(
      "Elizabeth Bridge" = "black",
      "Greenwich"        = "darkorange",
      "Harlington"       = "steelblue",
      "Westminster"      = "darkgreen"
    ),
    name = "Station"
  ) +
  scale_x_continuous(
    breaks = seq(0, 23, 3),
    labels = c("00", "03", "06", "09", "12", "15", "18", "21"),
    limits = c(0, 24)
  ) +
  scale_y_continuous(breaks = c(0, 10, 20, 30), limits = c(0, 35), expand = c(0, 0)) +
  labs(
    title = " Figure3: 24-Hour Variation of PM2.5 by Station ",
    subtitle = "Polar plot showing hourly changes; radius represents average level (µg/m³)",
    x = "Hour of Day",
    y = "PM2.5 (µg/m³)"
  ) +
  my_theme +
  theme(legend.position = "right", axis.text.y = element_text(size = 7))

print(fig3)
ggsave("Figure3_final.png", fig3, width = 10, height = 8, dpi = 300)

# FIGURE 4: Relationship Between PM2.5 and NO2

dual <- all_data %>%
  group_by(station, hour, parameter) %>%
  summarise(value = mean(value, na.rm = TRUE), .groups = 'drop') %>%
  pivot_wider(names_from = parameter, values_from = value) %>%
  filter(!is.na(pm25) & !is.na(no2)) %>%
  mutate(period = ifelse(hour %in% c(7:9, 17:19), "Rush Hour", "Off-Peak"))

fig4 <- ggplot(dual, aes(x = no2, y = pm25, color = station, shape = period)) +
  geom_point(alpha = 0.6, size = 3) +
  geom_hline(yintercept = 5, linetype = "dashed", color = "gray50", linewidth = 0.5, alpha = 0.5) +
  geom_vline(xintercept = 10, linetype = "dashed", color = "gray50", linewidth = 0.5, alpha = 0.5) +
  scale_color_viridis_d(option = "D", begin = 0, end = 1, name = "Station") +
  scale_shape_manual(
    values = c("Rush Hour" = 16, "Off-Peak" = 1),
    labels = c("Rush Hour" = "Rush Hour (7–9, 17–19)", "Off-Peak" = "Off-Peak"),
    name = "Period"
  ) +
  labs(
    title = " Figure4: Relationship Between PM2.5 and NO2 ",
    subtitle = "Rush-hour (solid) vs off-peak (hollow); dashed = WHO annual mean",
    x = "NO2 (µg/m³)",
    y = "PM2.5 (µg/m³)"
  ) +
  my_theme +
  theme(legend.position = "right")

print(fig4)
ggsave("Figure4_final.png", fig4, width = 12, height = 6.5, dpi = 300)

# SIMPLE STATISTICAL SUMMARY

all_data %>% group_by(parameter) %>%
  summarise(Count = n(), Mean = round(mean(value), 2), SD = round(sd(value), 2))

all_data %>% group_by(parameter, period) %>%
  summarise(Mean = round(mean(value), 2), .groups = 'drop')

cor(dual$pm25, dual$no2)

