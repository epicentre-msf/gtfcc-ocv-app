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
  files_country |> 
  purrr::set_names() |> 
  map(import_country_data) |> 
  list_rbind(names_to = "file") |> 
  filter(
    !is.na(pcode_adm1_t_target_area),
    !is.na(t_d1_date_round_start) | !is.na(t_d2_date_round_start)
  ) |> 
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

# export for Grace
app_data$df_country_profile |> 
  filter(t_r_iso3 == "MWI") |> 
  qxl::qxl(
    file = path(paths$proj, "data-clean", "sub-national", "gtfcc-ocv-mwi", ext = "xlsx")
  )

  app_data$df_country_profile |> 
    filter(t_r_iso3 == "YEM")

  df_country_profile |> 
    select(contains("date"))

  df_country_profile |> 
    filter(
      # is.na(pcode_adm1_t_target_area) | 
        (is.na(t_d1_date_round_start) & is.na(t_d2_date_round_start))
    ) |> 
    mutate(file = fs::path_file(file)) |> 
    distinct(file) |> 
    gt::gt()

kbl_count(df_country_profile, t_r_country)


