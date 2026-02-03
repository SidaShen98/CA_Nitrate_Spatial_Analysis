library(tidyverse)
library(sf)
library(tigris)
library(patchwork)

options(tigris_use_cache = TRUE)

pollution_raw <- read_csv("data/narrowresult.csv")
station_data <- read_csv("data/station.csv")

station_coords <- station_data %>% 
  select(MonitoringLocationIdentifier, LatitudeMeasure, LongitudeMeasure) %>%
  distinct(MonitoringLocationIdentifier, .keep_all = TRUE)

nitrate_sf <- pollution_raw %>%
  filter(str_detect(tolower(CharacteristicName), "nitrate")) %>%
  mutate(nitrate_value = as.numeric(ResultMeasureValue)) %>%
  filter(!is.na(nitrate_value), 
         `ResultMeasure/MeasureUnitCode` == "mg/l as N") %>%
  select(MonitoringLocationIdentifier, nitrate_value) %>%
  left_join(station_coords, by = "MonitoringLocationIdentifier") %>%
  filter(!is.na(LatitudeMeasure) & !is.na(LongitudeMeasure)) %>%
  st_as_sf(coords = c("LongitudeMeasure", "LatitudeMeasure"), crs = 4326)

ca_counties <- counties(state = "CA", cb = TRUE)

station_summary <- nitrate_sf %>%
  st_drop_geometry() %>% 
  group_by(MonitoringLocationIdentifier) %>%
  summarise(
    nitrate_median = median(nitrate_value, na.rm = TRUE),
    sample_count = n()
  ) %>%
  left_join(station_coords, by = "MonitoringLocationIdentifier") %>%
  st_as_sf(coords = c("LongitudeMeasure", "LatitudeMeasure"), crs = 4326)

ca_counties <- st_transform(ca_counties, crs = st_crs(station_summary))
county_joined <- st_join(station_summary, ca_counties)

county_nitrate_final <- county_joined %>%
  st_drop_geometry() %>%
  group_by(NAME) %>% 
  summarise(
    county_median_nitrate = median(nitrate_median, na.rm = TRUE),
    station_count = n()
  ) %>%
  left_join(ca_counties, ., by = "NAME") %>%
  st_as_sf()

p1 <- ggplot(nitrate_sf, aes(x = nitrate_value)) + 
  geom_histogram(bins = 50, fill = "#4C72B0", color = "white") +
  scale_x_log10() + theme_minimal() +
  labs(title = "A: Statistical Distribution", x = "Nitrate (mg/L)", y = "Count")

p2 <- ggplot() +
  geom_sf(data = ca_counties, fill = "#f9f9f9", color = "grey80", size = 0.1) +
  geom_sf(data = nitrate_sf, aes(color = nitrate_value), alpha = 0.4, size = 0.8) +
  scale_color_viridis_c(trans = "log10", option = "viridis", name = "mg/L") +
  theme_minimal() + labs(title = "B: Sample Points Pattern")

p3 <- ggplot(county_nitrate_final) +
  geom_sf(aes(fill = county_median_nitrate), color = "white", size = 0.1) +
  scale_fill_viridis_c(trans = "log10", option = "viridis", name = "Median mg/L", na.value = "grey95") +
  theme_minimal() + labs(title = "C: County-Level Aggregated Vulnerability")

final_viz <- (p1 | p2) / p3 + 
  plot_layout(heights = c(1, 1.2)) + 
  plot_annotation(title = "Multiscale Analysis of Nitrate Pollution in California")

print(final_viz)

