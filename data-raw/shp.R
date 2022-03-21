library(tidyverse)
library(sf)
library(fs)

world_map_raw <- rnaturalearth::ne_countries(scale = "small", type = "countries", returnclass = "sf")

sf_world <- world_map_raw %>% 
  sf::st_transform(crs = 4326) %>% 
  select(country = name_long, iso_a3, iso_a2, pop_est) %>% 
  filter(stringr::str_detect(country, "Antarctic", negate = TRUE)) %>% 
  mutate(continent = countrycode::countrycode(iso_a3, origin = "iso3c", destination = "continent"),
         region = countrycode::countrycode(iso_a3, origin = "iso3c", destination = "region")) %>% 
  cbind(sf::st_coordinates(sf::st_centroid(., of_largest_polygon = TRUE))) %>% 
  rename(lon = X, lat = Y) %>% 
  select(continent, region, everything())

saveRDS(sf_world, here::here("data", "sf_world.rds"))
