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

df_obt_exports <- 
  files_country |> 
  purrr::set_names() |> 
  map(import_country_data) |> 
  list_rbind(names_to = "file") |> 
  filter(
    !is.na(pcode_adm1_t_target_area),
    !is.na(t_d1_date_round_start) | !is.na(t_d2_date_round_start)
  ) |> 
  clean_geo(paths) |> 
  mutate(source = "OBT", .before = 1)


# retrospective data
path_retro <- path(paths$proj, "data-clean", "database", "multicountry_June_2024.rds")
df_retro_raw <- read_rds(path_retro)
setdiff(names(df_retro_raw), names(df_obt_exports))

ignore_ids <- df_obt_exports |> semi_join(df_retro_raw, by = "t_r_id") |> distinct(t_r_id) 

df_retro <- df_retro_raw |> 
  drop_na(vaccination_round, t_dose_adm) |> 
  filter(t_r_id != "2017-G03-D01") |> 
  anti_join(ignore_ids) |> 
  mutate(across(contains("date"), as_date)) |> 
  pivot_wider(
    id_cols = c(
      r_id,
      t_r_country,
      adm1_t_target_area,
      adm2_t_target_area,
      adm3_t_target_area,
      pcode_adm1_t_target_area, 
      pcode_adm2_t_target_area, 
      pcode_adm3_t_target_area,
      t_campaign_type,
      t_campaign_strategy,
      t_target_area_population,
      t_target_population,
      t_target_type,
      t_target_type_details
    ),
    names_from = vaccination_round,
    values_from = c(t_date_round_start, t_date_round_end, t_dose_adm, t_cov_adm),
    names_glue = "d{vaccination_round}_{.value}"
  ) |> 
  mutate(t_r_id = paste0(r_id, "-D01"), .after = r_id) |> 
  mutate(adm4_t_target_area = NA_character_, .after  = adm3_t_target_area) |> 
  rename_with(.cols = starts_with("d1"), \(x) str_replace(x, "d1_t", "t_d1")) |> 
  rename_with(.cols = starts_with("d2"), \(x) str_replace(x, "d2_t", "t_d2")) |> 
  relocate(starts_with("t_d"), .after = t_target_population) |> 
  mutate(
    across(starts_with("adm"), as.character),
    across(contains("pcode"), as.character),
    across(contains("nrow"), as.numeric),
    across(contains("population"), as.numeric),
    across(contains("dose"), as.numeric),
    across(contains("cov"), as.numeric),
    across(contains("drop"), as.numeric),
    across(contains("date"), as_date),
    adm1_t_target_area = stringr::str_remove(adm1_t_target_area, "^[A-Z]{3}(?=\\s)")
  ) |> 
  clean_geo(paths) |> 
  mutate(source = "retrospective", .before = 1)


df_country_profile <- bind_rows(df_obt_exports, df_retro |> select(-r_id))

# save data
app_data <- read_rds(here::here("data", "app_data.rds"))

app_data$df_country_profile <- df_country_profile

write_rds(app_data, path("data", "app_data", ext = "rds"))

# build the geobase
countries <- unique(app_data$df_country_profile$ref_adm0_name)
current_countries <- unique(read_rds(path("data", "geo_data", ext = "rds"))$adm1$sf$adm0_iso3)
setdiff(countries, current_countries)

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
  qxl::qxl(
    file = path(paths$proj, "data-clean", "sub-national", "gtfcc-ocv-all", ext = "xlsx")
  )

app_data$df_country_profile |> 
  saveRDS(
    file = path(paths$proj, "data-clean", "sub-national", "gtfcc-ocv-all", ext = "rds")
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


