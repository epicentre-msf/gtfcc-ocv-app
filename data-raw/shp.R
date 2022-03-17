library(tidyverse)
library(sf)
library(fs)

path_shps <- "~/MSF/GRP-EPI-COVID-19 - NCoVEpi/data/shapefiles/NGA/geopode/"

sf_adm2 <- read_rds(path(path_shps, "NGA_adm2.rds")) 
sf_adm3 <- read_rds(path(path_shps, "NGA_adm3.rds"))

saveRDS(sf_adm2, here::here("data", "sf_adm2.rds"))
saveRDS(sf_adm3, here::here("data", "sf_adm3.rds"))
