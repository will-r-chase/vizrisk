library(tidyverse)
library(geosphere)
library(geojsonio)

##landers 
landers <- read_csv("landers.csv")
landers_mainshock <- landers %>% filter(mag == 7.3)
landers_rupture_length <- 80 * 2 * 1000
landers_mainshock_time <- landers_mainshock$time

landers_dist <- 
  landers %>%
  mutate(mainshock_lon = landers_mainshock$longitude, mainshock_lat = landers_mainshock$latitude) %>%
  group_by(id) %>%
  mutate(dist = distHaversine(p1 = c(mainshock_lon, mainshock_lat), p2 = c(longitude, latitude))) %>%
  ungroup()

landers_triggered <- 
  landers_dist %>%
  filter(time > landers_mainshock_time) %>%
  filter(dist > landers_rupture_length) %>%
  arrange(time) %>%
  mutate(id_overall = row_number()) %>%
  mutate(day = lubridate::floor_date(time, unit = "day")) %>%
  group_by(day) %>%
  mutate(id_day = row_number()) %>%
  ungroup() 

landers_before <- 
  landers_dist %>%
  filter(time < landers_mainshock_time) %>%
  filter(dist > landers_rupture_length) %>%
  arrange(time) %>%
  mutate(id_overall = row_number()) %>%
  mutate(day = lubridate::floor_date(time, unit = "day")) %>%
  group_by(day) %>%
  mutate(id_day = row_number()) %>%
  ungroup() 

landers_aftershocks <- 
  landers_dist %>%
  filter(time > landers_mainshock_time) %>%
  filter(dist < landers_rupture_length) %>%
  arrange(time) %>%
  mutate(id_overall = row_number()) %>%
  mutate(day = lubridate::floor_date(time, unit = "day")) %>%
  group_by(day) %>%
  mutate(id_day = row_number()) %>%
  ungroup() 


#sumatra
sumatra <- read_csv("sumatra.csv")
sumatra_mainshock <- sumatra %>% filter(mag == 8.6)
sumatra_rupture_length <- 500 * 2 * 1000
sumatra_mainshock_time <- sumatra_mainshock$time

sumatra_dist <- 
  sumatra %>%
  mutate(mainshock_lon = sumatra_mainshock$longitude, mainshock_lat = sumatra_mainshock$latitude) %>%
  group_by(id) %>%
  mutate(dist = distHaversine(p1 = c(mainshock_lon, mainshock_lat), p2 = c(longitude, latitude))) %>%
  ungroup()

sumatra_triggered <- 
  sumatra_dist %>%
  filter(time > sumatra_mainshock_time) %>%
  filter(dist > sumatra_rupture_length) %>%
  arrange(time) %>%
  mutate(id_overall = row_number()) %>%
  mutate(day = lubridate::floor_date(time, unit = "day")) %>%
  group_by(day) %>%
  mutate(id_day = row_number()) %>%
  ungroup() 

sumatra_before <- 
  sumatra_dist %>%
  filter(time < sumatra_mainshock_time) %>%
  filter(dist > sumatra_rupture_length) %>%
  arrange(time) %>%
  mutate(id_overall = row_number()) %>%
  mutate(day = lubridate::floor_date(time, unit = "day")) %>%
  group_by(day) %>%
  mutate(id_day = row_number()) %>%
  ungroup() 

sumatra_aftershocks <- 
  sumatra_dist %>%
  filter(time > sumatra_mainshock_time) %>%
  filter(dist < sumatra_rupture_length) %>%
  arrange(time) %>%
  mutate(id_overall = row_number()) %>%
  mutate(day = lubridate::floor_date(time, unit = "day")) %>%
  group_by(day) %>%
  mutate(id_day = row_number()) %>%
  ungroup() 


#write data as geojson
landers_before %>%
  select(time, latitude, longitude, mag, id_overall, id_day, day) %>%
  geojson_write(., lat = "latitude", lon = "longitude", file = "landers_before.geojson")
landers_triggered %>%
  select(time, latitude, longitude, mag, id_overall, id_day, day) %>%
  geojson_write(., lat = "latitude", lon = "longitude", file = "landers_triggered.geojson")

sumatra_before %>%
  select(time, latitude, longitude, mag, id_overall, id_day, day) %>%
  geojson_write(., lat = "latitude", lon = "longitude", file = "sumatra_before.geojson")
sumatra_triggered %>%
  select(time, latitude, longitude, mag, id_overall, id_day, day) %>%
  geojson_write(., lat = "latitude", lon = "longitude", file = "sumatra_triggered.geojson")
  

