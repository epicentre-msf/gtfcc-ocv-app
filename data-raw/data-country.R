library(tidyverse)
library(sf)
library(geoarrow)
library(arrow)
library(fs)
source(path("data-raw", "utils-import.R"))

paths <- set_paths()

files_country <- path(
  paths$proj,
  "data-clean",
  "export4dashboard",
  "country"
) |> dir_ls(glob = "*.xlsx")

df_country_profile <- 
  map(files_country, import_country_data) |> 
  list_rbind() |> 
  clean_geo(paths)

app_data <- read_rds(here::here("data", "app_data.rds"))

app_data$df_country_profile <- df_country_profile

write_rds(app_data, path("data", "app_data", ext = "rds"))

# build the geobase
countries <- unique(app_data$df_country_profile$ref_adm0_name)

geo_path <- "~/epicentre/outbreak-tools-geoapp/data/geoparquet/"

lvls <- purrr::set_names(paste0("adm", 1:3))

geo_data <- purrr::map(lvls, make_geo_layer, geo_path, countries)

write_rds(geo_data, path("data", "geo_data", ext = "rds"))
