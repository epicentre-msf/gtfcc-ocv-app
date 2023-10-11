library(tidyverse)
library(sf)
library(fs)

world_map_raw <- rnaturalearth::ne_countries(scale = "small", type = "countries", returnclass = "sf")

sf_world <- world_map_raw %>% 
  sf::st_transform(crs = "+proj=longlat +datum=WGS84") %>% 
  select(country = name_long, iso_a3, iso_a2, pop_est) %>% 
  filter(stringr::str_detect(country, "Antarctic", negate = TRUE)) %>% 
  mutate(continent = countrycode::countrycode(iso_a3, origin = "iso3c", destination = "continent"),
         region = countrycode::countrycode(iso_a3, origin = "iso3c", destination = "region")) %>% 
  cbind(sf::st_coordinates(sf::st_centroid(., of_largest_polygon = TRUE))) %>% 
  rename(lon = X, lat = Y) %>% 
  select(continent, region, everything())

saveRDS(sf_world, here::here("data", "sf_world.rds"))

#shp for CMR 

get_admin_level <- function(obt_gb_path, country, level) {
  require("sf")
  sp_dir <- fs::path(obt_gb_path, country)
  # get latest version directory
  latest <- max(fs::dir_ls(sp_dir, regexp = glue::glue("{country}__"), type = "directory"))
  shp_path <- fs::path(latest, "sf", paste(country, tolower(level), sep = "_"), ext = "rds")
  readr::read_rds(shp_path)
}

obt_gb_path <- "~/MSF/OutbreakTools - GeoBase"
adm1 <- get_admin_level(obt_gb_path, "CMR", "ADM1")
adm2 <- get_admin_level(obt_gb_path, "CMR", "ADM2")
adm3 <- get_admin_level(obt_gb_path, "CMR", "ADM3")


saveRDS(adm1, here::here("data", "cmr_adm1.rds"))
saveRDS(adm2, here::here("data", "cmr_adm2.rds"))
saveRDS(adm3, here::here("data", "cmr_adm3.rds"))
