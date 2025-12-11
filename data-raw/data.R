library(tidyverse)
library(fs)
library(readxl)
source(here::here("R", "utils_data.R"))

path_sharepoint <- "~/MSF/EpiDS - GTFCC-OCV/data-clean/export4dashboard/"
files_gtfcc <- dir_ls(path_sharepoint, regexp = "gtfcc_ocv_data_dashoard__.*.xlsx")
files_dt <- file_info(files_gtfcc)$birth_time
path_data <- files_gtfcc[files_dt == max(files_dt)]
# path_data <- "~/MSF/EpiDS - GTFCC-OCV/data-clean/export4dashboard/gtfcc_ocv_data_dashoard__vb-1003__20251027-1100.xlsx"
path_file(path_data)
# (path_data <- max(dir_ls(path_sharepoint, regexp = "gtfcc_ocv_data_dashoard__.*.xlsx")))

(date_updated <- as_date(str_extract(path_data, "\\d{8}")))
# date_updated <- fs::file_info(path_data)$birth_time %>% lubridate::as_date()
write_rds(date_updated, here::here("data", "date_updated.rds"))

data_sheets <- c("Request", "Shipment", "Campaign and round")

dat <- data_sheets %>%
  purrr::set_names(snakecase::to_snake_case(.)) %>%
  map(~ qxl::qread(path_data, .x, na = c("", "NA", "Nap", "NAp")))
# map(~ mutate(.x, across(contains("date"), lubridate::as_date)))

dat$request <- dat$request %>%
  mutate(
    r_country = recode(r_country, "Zanzibar" = "Tanzania"),
    iso_a3 = countrycode::countrycode(r_country, origin = "country.name", destination = "iso3c"),
    .after = r_country
  ) %>%
  filter(!is.na(iso_a3)) %>%
  mutate(
    r_mechanism_type = case_when(
      r_mechanism %in% c("ICG", "Loan") ~ "Reactive",
      r_mechanism %in% c("GTFCC") ~ "Preventive"
    ),
    .after = r_mechanism_abb
  ) %>%
  mutate(
    quarter = lubridate::quarter(r_date_receipt, with_year = TRUE) %>% str_replace("\\.", "-Q")
  ) %>%
  left_join(
    dat$shipment %>%
      group_by(s_r_demand_id) %>%
      summarise(s_dose_ship = sum(s_dose_ship, na.rm = TRUE), .groups = "drop"),
    by = c("r_demand_id" = "s_r_demand_id")
  )

df_vaccine <- dat$shipment %>%
  distinct(s_r_demand_id, s_vaccine) %>%
  group_by(s_r_demand_id) %>%
  summarise(s_vaccine = paste(s_vaccine, collapse = ", "), .groups = "drop")

df_info <- dat$request %>%
  distinct(
    r_demand_id,
    r_who_region,
    r_country,
    r_status,
    r_context,
    r_mechanism,
    r_mechanism_type,
    r_agency
  ) %>%
  left_join(df_vaccine, by = c("r_demand_id" = "s_r_demand_id"))

df_timeline <- get_timevis_df(dat$request, dat$shipment, dat$campaign_and_round)

df_timevis <- df_info %>%
  left_join(df_timeline, by = "r_demand_id", multiple = "all") %>%
  drop_na(start) %>%
  mutate(
    type = "point",
    title = glue::glue("ID: {r_demand_id} | Date: {format(start, '%d %b %y')}")
  ) %>%
  rename(group = r_country)

df_delay <- df_info %>%
  left_join(get_delay_df(df_timeline), by = "r_demand_id", multiple = "all") %>%
  drop_na(date)

# df_event_max <- df_delay %>%
#   separate(event, c("event", "index"), sep = "_") %>%
#   group_by(event) %>%
#   summarise(max = max(index, na.rm = TRUE), .groups = "drop")

# max_shipments <- df_event_max %>% filter(event == "shipment") %>% pull(max) %>% as.numeric()
# max_rounds <- df_event_max %>% filter(event == "round") %>% pull(max) %>% as.numeric()

# Country profile data ----------------------------------------------------

# path_country_data <- max(fs::dir_ls("~/MSF/EpiDS - GTFCC-OCV/data-clean/rds/", regexp = "data_target_areas_.*.rds"))

# df_country_profile <- read_rds(path_country_data)

app_data <- c(
  dat,
  tibble::lst(
    df_info,
    df_timevis,
    df_delay
    # df_country_profile
  )
)

data_dir <- here::here("data")
if (!dir_exists(data_dir)) {
  dir_create(data_dir)
}
data_path <- path(data_dir, "app_data", ext = "rds")

# add country data from current data file to new data
old_dat <- read_rds(data_path)
write_rds(old_dat, path(data_dir, "app_data_backup", ext = "rds"))
app_data$df_country_profile <- old_dat$df_country_profile

write_rds(app_data, data_path)

if (FALSE) {
  library(summarytools)
  walk2(
    dat,
    names(dat),
    ~ {
      df_sum <- dfSummary(.x)
      view(df_sum, method = "browser", file = path("data-raw", .y, ext = "html"))
    }
  )
}
